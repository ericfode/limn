#!/usr/bin/env python3
"""
Iterated Learning Experiment — Cultural Transmission Pressure

Tests whether compositionality emerges through iterated learning:
a chain of agents where each generation learns from limited examples
produced by the previous generation.

Protocol:
1. Generation 0: Standard Lewis game training (20k steps) → protocol P0
2. For generation g = 1..N:
   a. Use sender_{g-1} to produce messages for ALL objects
   b. Sample a SUBSET (bottleneck_fraction) of object→message pairs
   c. Train sender_g to reproduce these messages (supervised, not RL)
   d. Train receiver_g alongside sender_g (standard Lewis game)
   e. Measure topsim, accuracy, disentanglement

The transmission bottleneck is the key pressure:
- Generation g+1 sees only a fraction of the protocol
- With fewer examples, the easiest protocol to learn is compositional
  (fewer parameters needed to encode the rule)
- Holistic codes require memorizing each object→message mapping separately

Based on: Ren et al. 2020 "Compositional Languages Emerge in a Neural
Iterated Learning Model"

— Lex
"""

import json
import math
import os
import random
from pathlib import Path

import numpy as np
from scipy import stats

os.environ.setdefault("CUDA_PATH", "/usr/lib/wsl/lib/libcuda.so.1")

from tinygrad import Tensor, Device, dtypes
from tinygrad.nn.state import get_parameters, safe_save, get_state_dict
from tinygrad.nn.optim import Adam
from tinygrad.engine.jit import TinyJit
import tinygrad.nn as nn

# ─── Device Selection ────────────────────────────────────────────────────────

if os.path.exists("/usr/lib/wsl/lib/libcuda.so.1"):
    try:
        Device.DEFAULT = "CUDA"
        Tensor.randn(2, 2).numpy()
    except Exception:
        Device.DEFAULT = "CPU"
else:
    Device.DEFAULT = "CPU"

print(f"Device: {Device.DEFAULT}")

# ─── Configuration ───────────────────────────────────────────────────────────

N_ATTRIBUTES = 4
N_VALUES = 8
VOCAB_SIZE = 32
MSG_LEN = 4
HIDDEN_DIM = 256
N_DISTRACTORS = 3
N_CANDIDATES = 1 + N_DISTRACTORS

BATCH_SIZE = 512
SEED = 42

# Iterated learning parameters
N_GENERATIONS = 10          # Number of generations
GEN0_STEPS = 20_000         # Training steps for generation 0 (full Lewis game)
IMIT_STEPS = 5_000          # Imitation steps per generation (supervised from teacher)
COMM_STEPS = 5_000          # Communication steps per generation (Lewis game with new receiver)
BOTTLENECK_FRACTION = 0.25  # Fraction of objects seen by each new generation
LR = 1e-3
TEMP_INIT = 2.0
TEMP_MIN = 0.5
TEMP_ANNEAL_STEPS = 5_000

EVAL_EVERY = 2000
OUTPUT_DIR = Path(__file__).resolve().parent
RESULTS_FILE = OUTPUT_DIR / "iterated_learning_results.json"

# ─── Object Space ────────────────────────────────────────────────────────────

from itertools import product

ALL_OBJECTS_NP = np.array(list(product(range(N_VALUES), repeat=N_ATTRIBUTES)), dtype=np.int32)
N_OBJECTS = len(ALL_OBJECTS_NP)


def objects_to_onehot_np(objects_np):
    batch = objects_np.shape[0]
    onehot = np.zeros((batch, N_ATTRIBUTES * N_VALUES), dtype=np.float32)
    for i in range(N_ATTRIBUTES):
        onehot[np.arange(batch), i * N_VALUES + objects_np[:, i]] = 1.0
    return onehot


ALL_ONEHOT_NP = objects_to_onehot_np(ALL_OBJECTS_NP)


def sample_batch_np(batch_size, n_distractors, pool_indices=None):
    """Sample a batch. If pool_indices given, sample targets only from pool."""
    if pool_indices is None:
        pool_indices = np.arange(N_OBJECTS)
    target_pool_idx = np.random.choice(len(pool_indices), size=batch_size)
    target_idx = pool_indices[target_pool_idx]
    # Distractors from full object space
    distractor_idx = np.random.randint(0, N_OBJECTS, size=(batch_size, n_distractors))
    indices = np.concatenate([target_idx[:, None], distractor_idx], axis=1)
    targets_oh = ALL_ONEHOT_NP[indices[:, 0]]
    candidates_oh = ALL_ONEHOT_NP[indices.flatten()].reshape(
        batch_size, 1 + n_distractors, N_ATTRIBUTES * N_VALUES)
    return targets_oh, candidates_oh


# ─── Gumbel-Softmax ─────────────────────────────────────────────────────────

def gumbel_softmax(logits, temperature, noise=None, hard=True):
    if noise is None:
        u = Tensor.rand(*logits.shape)
    else:
        u = noise
    g = -(-(u + 1e-20).log() + 1e-20).log()
    y = ((logits + g) / temperature).softmax(axis=-1)
    if hard:
        idx = y.argmax(axis=-1)
        y_hard = idx.one_hot(logits.shape[-1]).cast(y.dtype)
        y = (y_hard - y).detach() + y
    return y


# ─── Agents ──────────────────────────────────────────────────────────────────

class Sender:
    def __init__(self, vocab_size=VOCAB_SIZE):
        input_dim = N_ATTRIBUTES * N_VALUES
        self.vocab_size = vocab_size
        self.enc1 = nn.Linear(input_dim, HIDDEN_DIM)
        self.enc2 = nn.Linear(HIDDEN_DIM, HIDDEN_DIM)
        self.heads = [nn.Linear(HIDDEN_DIM, vocab_size) for _ in range(MSG_LEN)]

    def __call__(self, x, temperature, noise_packed=None):
        h = self.enc2(self.enc1(x).relu()).relu()
        softs, hards = [], []
        for i, head in enumerate(self.heads):
            logits = head(h)
            n = noise_packed[i] if noise_packed is not None else None
            gs = gumbel_softmax(logits, temperature, noise=n, hard=True)
            softs.append(gs)
            hards.append(gs.argmax(axis=-1))
        msg_soft = Tensor.stack(*softs, dim=1)
        msg_hard = Tensor.stack(*hards, dim=1)
        return msg_soft, msg_hard

    def get_logits(self, x):
        """Get raw logits for each position (for imitation loss)."""
        h = self.enc2(self.enc1(x).relu()).relu()
        return [head(h) for head in self.heads]


class Receiver:
    def __init__(self, vocab_size=VOCAB_SIZE):
        msg_dim = MSG_LEN * vocab_size
        obj_dim = N_ATTRIBUTES * N_VALUES
        self.msg_enc1 = nn.Linear(msg_dim, HIDDEN_DIM)
        self.msg_enc2 = nn.Linear(HIDDEN_DIM, HIDDEN_DIM)
        self.obj_enc1 = nn.Linear(obj_dim, HIDDEN_DIM)
        self.obj_enc2 = nn.Linear(HIDDEN_DIM, HIDDEN_DIM)

    def __call__(self, msg_soft, candidates):
        batch = msg_soft.shape[0]
        n_cand = candidates.shape[1]
        msg_flat = msg_soft.reshape(batch, -1)
        msg_emb = self.msg_enc2(self.msg_enc1(msg_flat).relu()).relu()
        cand_flat = candidates.reshape(batch * n_cand, -1)
        cand_emb = self.obj_enc2(self.obj_enc1(cand_flat).relu()).relu()
        cand_emb = cand_emb.reshape(batch, n_cand, HIDDEN_DIM)
        scores = (cand_emb * msg_emb.unsqueeze(1)).sum(axis=-1)
        return scores


# ─── Analysis ────────────────────────────────────────────────────────────────

def get_all_messages(sender, temperature=0.1, batch_size=512):
    all_messages = []
    for i in range(0, N_OBJECTS, batch_size):
        end = min(i + batch_size, N_OBJECTS)
        oh = Tensor(ALL_ONEHOT_NP[i:end])
        _, msg_hard = sender(oh, temperature)
        all_messages.append(msg_hard.numpy())
    return np.concatenate(all_messages, axis=0)


def compute_topographic_similarity(sender, n_samples=2000, temperature=0.1):
    all_msgs = get_all_messages(sender, temperature)
    idx = np.random.choice(N_OBJECTS, size=min(n_samples, N_OBJECTS), replace=False)
    sample_objects = ALL_OBJECTS_NP[idx]
    sample_msgs = all_msgs[idx]
    n_pairs = min(50000, len(idx) * (len(idx) - 1) // 2)
    pairs = [random.sample(range(len(idx)), 2) for _ in range(n_pairs)]
    input_dists, msg_dists = [], []
    for i, j in pairs:
        input_dists.append((sample_objects[i] != sample_objects[j]).sum())
        msg_dists.append((sample_msgs[i] != sample_msgs[j]).sum())
    rho, _ = stats.spearmanr(input_dists, msg_dists)
    return rho if not math.isnan(rho) else 0.0


def compute_message_entropy(sender, temperature=0.1):
    all_msgs = get_all_messages(sender, temperature)
    msg_tuples = [tuple(m) for m in all_msgs]
    counts = {}
    for m in msg_tuples:
        counts[m] = counts.get(m, 0) + 1
    total = len(msg_tuples)
    return sum(-c/total * math.log2(c/total) for c in counts.values())


# ─── Temperature ─────────────────────────────────────────────────────────────

def get_temperature(step, anneal_steps=TEMP_ANNEAL_STEPS):
    if step >= anneal_steps:
        return TEMP_MIN
    return TEMP_INIT * (TEMP_MIN / TEMP_INIT) ** (step / anneal_steps)


# ─── Generation 0: Standard Lewis Game ───────────────────────────────────────

def train_generation_0():
    """Train initial agents with standard Lewis game."""
    print("\n" + "=" * 60)
    print("GENERATION 0: Standard Lewis Game")
    print("=" * 60)

    np.random.seed(SEED)
    random.seed(SEED)
    Tensor.manual_seed(SEED)

    sender = Sender()
    receiver = Receiver()
    params = get_parameters(sender) + get_parameters(receiver)
    opt = Adam(params, lr=LR)
    labels = Tensor.zeros(BATCH_SIZE, dtype=dtypes.int)

    @TinyJit
    def train_step(tgt_oh, cand_oh, noise_packed, temp_t):
        msg_soft, _ = sender(tgt_oh, temp_t, noise_packed=noise_packed)
        scores = receiver(msg_soft, cand_oh)
        loss = scores.sparse_categorical_crossentropy(labels)
        loss.backward()
        opt.step()
        opt.zero_grad()
        return loss.realize(), (scores.argmax(axis=-1) == 0).mean().realize()

    Tensor.training = True
    for step in range(GEN0_STEPS):
        temperature = get_temperature(step)
        tgt_oh_np, cand_oh_np = sample_batch_np(BATCH_SIZE, N_DISTRACTORS)
        noise_packed = Tensor.rand(MSG_LEN, BATCH_SIZE, VOCAB_SIZE)
        temp_t = Tensor([temperature])
        loss, acc = train_step(Tensor(tgt_oh_np), Tensor(cand_oh_np), noise_packed, temp_t)

        if step % EVAL_EVERY == 0 or step == GEN0_STEPS - 1:
            Tensor.training = False
            topsim = compute_topographic_similarity(sender)
            print(f"  Gen 0 Step {step:>6d} | Loss {loss.numpy():.4f} | "
                  f"Acc {acc.numpy():.4f} | TopSim {topsim:.4f}")
            Tensor.training = True

    Tensor.training = False
    final_topsim = compute_topographic_similarity(sender)
    final_acc = acc.numpy()
    print(f"  Gen 0 done — TopSim {final_topsim:.4f}, Acc {float(final_acc):.4f}")

    return sender, receiver, final_topsim, float(final_acc)


# ─── Generation N: Iterated Learning ────────────────────────────────────────

def train_generation_n(teacher_sender, gen_num):
    """Train a new generation using iterated learning (two-phase).

    Phase 1 (Imitation): New sender learns to reproduce teacher's messages
    for a LIMITED subset of objects (the transmission bottleneck).
    Phase 2 (Communication): New sender+receiver play Lewis game together
    using JIT for speed.

    The bottleneck in Phase 1 is what forces compositionality: with only 25%
    of object-message pairs, the easiest protocol to learn is compositional.
    """
    print(f"\n{'─' * 60}")
    print(f"GENERATION {gen_num}")
    print(f"{'─' * 60}")

    # Get teacher's complete protocol
    Tensor.training = False
    teacher_messages = get_all_messages(teacher_sender, temperature=0.1)  # (4096, 4)

    # Transmission bottleneck: student sees only a fraction
    n_exposed = int(N_OBJECTS * BOTTLENECK_FRACTION)
    exposed_indices = np.random.choice(N_OBJECTS, size=n_exposed, replace=False)
    exposed_objects_oh = ALL_ONEHOT_NP[exposed_indices]  # (n_exposed, 32)
    exposed_messages = teacher_messages[exposed_indices]   # (n_exposed, 4)

    print(f"  Bottleneck: {n_exposed}/{N_OBJECTS} objects ({BOTTLENECK_FRACTION:.0%})")

    # ── Phase 1: Imitation (sender only, JIT'd for speed) ──
    student_sender = Sender()
    sender_params = get_parameters(student_sender)
    sender_opt = Adam(sender_params, lr=LR)

    @TinyJit
    def imit_step(imit_objects, tgt_0, tgt_1, tgt_2, tgt_3):
        logits_list = student_sender.get_logits(imit_objects)
        targets = [tgt_0, tgt_1, tgt_2, tgt_3]
        losses = [logits_list[p].sparse_categorical_crossentropy(targets[p])
                  for p in range(MSG_LEN)]
        loss = Tensor.stack(*losses).mean()
        loss.backward()
        sender_opt.step()
        sender_opt.zero_grad()
        return loss.realize()

    Tensor.training = True
    for step in range(IMIT_STEPS):
        imit_idx = np.random.choice(n_exposed, size=BATCH_SIZE, replace=True)
        imit_objects = Tensor(exposed_objects_oh[imit_idx])
        imit_targets = exposed_messages[imit_idx]
        tgt_per_pos = [Tensor(imit_targets[:, p].astype(np.int32)) for p in range(MSG_LEN)]
        imit_loss = imit_step(imit_objects, *tgt_per_pos)

        if step % EVAL_EVERY == 0 or step == IMIT_STEPS - 1:
            Tensor.training = False
            topsim = compute_topographic_similarity(student_sender)
            print(f"  Gen {gen_num} Imit Step {step:>6d} | "
                  f"Loss {imit_loss.numpy():.4f} | TopSim {topsim:.4f}")
            Tensor.training = True

    # ── Phase 2: Communication (sender+receiver, JIT'd) ──
    student_receiver = Receiver()
    all_params = get_parameters(student_sender) + get_parameters(student_receiver)
    comm_opt = Adam(all_params, lr=LR)
    labels = Tensor.zeros(BATCH_SIZE, dtype=dtypes.int)

    @TinyJit
    def comm_step(tgt_oh, cand_oh, noise_packed, temp_t):
        msg_soft, _ = student_sender(tgt_oh, temp_t, noise_packed=noise_packed)
        scores = student_receiver(msg_soft, cand_oh)
        loss = scores.sparse_categorical_crossentropy(labels)
        loss.backward()
        comm_opt.step()
        comm_opt.zero_grad()
        return loss.realize(), (scores.argmax(axis=-1) == 0).mean().realize()

    Tensor.training = True
    for step in range(COMM_STEPS):
        temperature = get_temperature(step)
        tgt_oh_np, cand_oh_np = sample_batch_np(BATCH_SIZE, N_DISTRACTORS)
        noise_packed = Tensor.rand(MSG_LEN, BATCH_SIZE, VOCAB_SIZE)
        temp_t = Tensor([temperature])
        loss, acc = comm_step(Tensor(tgt_oh_np), Tensor(cand_oh_np), noise_packed, temp_t)

        if step % EVAL_EVERY == 0 or step == COMM_STEPS - 1:
            Tensor.training = False
            topsim = compute_topographic_similarity(student_sender)
            print(f"  Gen {gen_num} Comm Step {step:>6d} | "
                  f"Loss {loss.numpy():.4f} | Acc {acc.numpy():.4f} | TopSim {topsim:.4f}")
            Tensor.training = True

    Tensor.training = False
    final_topsim = compute_topographic_similarity(student_sender)
    tgt_oh_np, cand_oh_np = sample_batch_np(BATCH_SIZE, N_DISTRACTORS)
    noise_packed = Tensor.rand(MSG_LEN, BATCH_SIZE, VOCAB_SIZE)
    msg_soft, _ = student_sender(Tensor(tgt_oh_np), 0.1, noise_packed=noise_packed)
    scores = student_receiver(msg_soft, Tensor(cand_oh_np))
    final_acc = (scores.argmax(axis=-1) == 0).mean().numpy()

    print(f"  Gen {gen_num} done — TopSim {final_topsim:.4f}, Acc {float(final_acc):.4f}")

    return student_sender, student_receiver, final_topsim, float(final_acc)


# ─── Main ────────────────────────────────────────────────────────────────────

def run_experiment():
    print("=" * 60)
    print("ITERATED LEARNING EXPERIMENT")
    print(f"Generations: {N_GENERATIONS}, Bottleneck: {BOTTLENECK_FRACTION:.0%}")
    print(f"Per gen: {IMIT_STEPS} imitation + {COMM_STEPS} communication steps")
    print("=" * 60)

    results = {
        "config": {
            "n_generations": N_GENERATIONS,
            "gen0_steps": GEN0_STEPS,
            "imit_steps": IMIT_STEPS,
            "comm_steps": COMM_STEPS,
            "bottleneck_fraction": BOTTLENECK_FRACTION,
            "seed": SEED,
            "device": Device.DEFAULT,
        },
        "generations": [],
    }

    # Generation 0
    sender, receiver, topsim, acc = train_generation_0()
    results["generations"].append({
        "generation": 0,
        "topsim": topsim,
        "accuracy": acc,
    })

    # Iterated generations
    for gen in range(1, N_GENERATIONS + 1):
        sender, receiver, topsim, acc = train_generation_n(sender, gen)
        results["generations"].append({
            "generation": gen,
            "topsim": topsim,
            "accuracy": acc,
        })

        # Save intermediate results
        with open(RESULTS_FILE, "w") as f:
            json.dump(results, f, indent=2)

    # Summary
    print(f"\n{'=' * 60}")
    print("SUMMARY — Compositionality Across Generations")
    print(f"{'=' * 60}")
    print(f"\n  {'Gen':>4s}  {'TopSim':>8s}  {'Acc':>8s}  {'Delta':>8s}")
    print(f"  {'─' * 32}")

    prev_ts = None
    for g in results["generations"]:
        ts = g["topsim"]
        delta = f"{ts - prev_ts:+.4f}" if prev_ts is not None else "—"
        print(f"  {g['generation']:>4d}  {ts:>8.4f}  {g['accuracy']:>8.4f}  {delta:>8s}")
        prev_ts = ts

    gen0_ts = results["generations"][0]["topsim"]
    final_ts = results["generations"][-1]["topsim"]
    improvement = final_ts - gen0_ts

    print(f"\n  Total improvement: {improvement:+.4f} ({gen0_ts:.4f} → {final_ts:.4f})")

    if final_ts > 0.55:
        print(f"  STRONG compositionality induced by iterated learning!")
        results["verdict"] = "STRONG_COMPOSITIONALITY"
    elif improvement > 0.10:
        print(f"  MODERATE improvement — iterated learning helps but doesn't fully solve")
        results["verdict"] = "MODERATE_IMPROVEMENT"
    elif improvement > 0.03:
        print(f"  WEAK improvement — iterated learning has a small effect")
        results["verdict"] = "WEAK_IMPROVEMENT"
    else:
        print(f"  NO significant improvement from iterated learning")
        results["verdict"] = "NO_IMPROVEMENT"

    with open(RESULTS_FILE, "w") as f:
        json.dump(results, f, indent=2)
    print(f"\n  Results saved to: {RESULTS_FILE}")


if __name__ == "__main__":
    run_experiment()
