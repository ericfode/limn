#!/usr/bin/env python3
"""
Emergent Communication vs Limn — Lewis Signaling Game (tinygrad)

Two neural agents (sender/receiver) must develop a communication protocol
to identify objects with compositional attributes. After convergence, we
analyze whether the emerged protocol develops compositionality resembling
Limn's operator structure.

Hypothesis (H15): Machine communication protocols converge to compositional
structure when the input space is compositionally structured. Limn's design
is not arbitrary — it's an attractor in the space of efficient encodings.

Setup:
- Objects: 4 attributes × 8 values = 4096 possible objects
- Sender: object → message (sequence of discrete tokens via Gumbel-Softmax)
- Receiver: message + candidates → selection (dot-product scoring)
- Training: Gumbel-Softmax straight-through, cross-entropy loss
- Analysis: topographic similarity, positional disentanglement, entropy

References:
- Lazaridou et al. (2017) "Multi-Agent Cooperation and the Emergence of
  (Natural) Language"
- Chaabouni et al. (2020) "Compositionality and Generalization in Emergent
  Languages"
- Ren et al. (2020) "Compositional Languages Emerge in a Neural Iterated
  Learning Model"

— Lex
"""

import json
import math
import os
import random
from itertools import product
from pathlib import Path

import numpy as np
from scipy import stats

os.environ.setdefault("CUDA_PATH", "/usr/lib/wsl/lib/libcuda.so.1")

from tinygrad import Tensor, Device, dtypes
from tinygrad.nn.state import get_parameters, safe_save, get_state_dict
from tinygrad.nn.optim import Adam
from tinygrad.engine.jit import TinyJit
import tinygrad.nn as nn

# Labels tensor (constant, created once)
LABELS = None  # initialized after device selection

# ─── Configuration ───────────────────────────────────────────────────────────

N_ATTRIBUTES = 4       # color, shape, size, texture
N_VALUES = 8           # values per attribute
VOCAB_SIZE = 32        # message token vocabulary
MSG_LEN = 4            # message length (one slot per attribute, conceptually)
HIDDEN_DIM = 256       # hidden layer size
N_DISTRACTORS = 3      # receiver chooses from target + N distractors
N_CANDIDATES = 1 + N_DISTRACTORS

BATCH_SIZE = 512
N_STEPS = 20_000
LR = 1e-3
TEMP_INIT = 2.0
TEMP_MIN = 0.5
TEMP_ANNEAL_STEPS = 10_000

SEED = 42
EVAL_EVERY = 500

OUTPUT_DIR = Path(__file__).resolve().parent
RESULTS_FILE = OUTPUT_DIR / "lewis_game_results.json"

# ─── Device Selection ────────────────────────────────────────────────────────

if os.path.exists("/usr/lib/wsl/lib/libcuda.so.1"):
    try:
        Device.DEFAULT = "CUDA"
        Tensor.randn(2, 2).numpy()  # smoke test
    except Exception:
        Device.DEFAULT = "CPU"
else:
    Device.DEFAULT = "CPU"

print(f"Device: {Device.DEFAULT}")


# ─── Object Space ────────────────────────────────────────────────────────────

def make_all_objects_np():
    """Generate all objects as numpy array of attribute indices."""
    return np.array(list(product(range(N_VALUES), repeat=N_ATTRIBUTES)), dtype=np.int32)

ALL_OBJECTS_NP = make_all_objects_np()  # (4096, 4)
N_OBJECTS = len(ALL_OBJECTS_NP)


def objects_to_onehot_np(objects_np):
    """(batch, 4) int → (batch, 32) float one-hot."""
    batch = objects_np.shape[0]
    onehot = np.zeros((batch, N_ATTRIBUTES * N_VALUES), dtype=np.float32)
    for i in range(N_ATTRIBUTES):
        onehot[np.arange(batch), i * N_VALUES + objects_np[:, i]] = 1.0
    return onehot


# Pre-compute all one-hots
ALL_ONEHOT_NP = objects_to_onehot_np(ALL_OBJECTS_NP)  # (4096, 32)


def sample_batch_np(batch_size, n_distractors):
    """Sample (target_onehot, candidates_onehot) as numpy arrays.
    Target is always at candidate position 0."""
    indices = np.random.randint(0, N_OBJECTS, size=(batch_size, 1 + n_distractors))
    targets_oh = ALL_ONEHOT_NP[indices[:, 0]]       # (batch, 32)
    candidates_oh = ALL_ONEHOT_NP[indices.flatten()].reshape(
        batch_size, 1 + n_distractors, N_ATTRIBUTES * N_VALUES)  # (batch, 4, 32)
    target_attrs = ALL_OBJECTS_NP[indices[:, 0]]     # (batch, 4)
    return targets_oh, candidates_oh, target_attrs


# ─── Gumbel-Softmax ─────────────────────────────────────────────────────────

def gumbel_softmax(logits, temperature, noise=None, hard=True):
    """Gumbel-Softmax with optional straight-through.
    If noise is provided (uniform samples), use it instead of generating new."""
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
    """Maps an object (one-hot) to a sequence of discrete message tokens."""
    def __init__(self):
        input_dim = N_ATTRIBUTES * N_VALUES  # 32
        self.enc1 = nn.Linear(input_dim, HIDDEN_DIM)
        self.enc2 = nn.Linear(HIDDEN_DIM, HIDDEN_DIM)
        self.heads = [nn.Linear(HIDDEN_DIM, VOCAB_SIZE) for _ in range(MSG_LEN)]

    def __call__(self, x, temperature, noise_packed=None):
        """
        Args:
            x: (batch, 32) one-hot objects
            temperature: float or Tensor
            noise_packed: optional (MSG_LEN, batch, VOCAB_SIZE) uniform noise tensor
        Returns:
            msg_soft: (batch, MSG_LEN, VOCAB_SIZE) soft distributions
            msg_hard: (batch, MSG_LEN) discrete token indices
        """
        h = self.enc2(self.enc1(x).relu()).relu()
        softs = []
        hards = []
        for i, head in enumerate(self.heads):
            logits = head(h)
            n = noise_packed[i] if noise_packed is not None else None
            gs = gumbel_softmax(logits, temperature, noise=n, hard=True)
            softs.append(gs)
            hards.append(gs.argmax(axis=-1))

        msg_soft = Tensor.stack(*softs, dim=1)
        msg_hard = Tensor.stack(*hards, dim=1)
        return msg_soft, msg_hard


class Receiver:
    """Takes a message and candidate objects, scores each candidate."""
    def __init__(self):
        msg_dim = MSG_LEN * VOCAB_SIZE
        obj_dim = N_ATTRIBUTES * N_VALUES

        self.msg_enc1 = nn.Linear(msg_dim, HIDDEN_DIM)
        self.msg_enc2 = nn.Linear(HIDDEN_DIM, HIDDEN_DIM)
        self.obj_enc1 = nn.Linear(obj_dim, HIDDEN_DIM)
        self.obj_enc2 = nn.Linear(HIDDEN_DIM, HIDDEN_DIM)

    def __call__(self, msg_soft, candidates):
        """
        Args:
            msg_soft: (batch, MSG_LEN, VOCAB_SIZE)
            candidates: (batch, n_candidates, 32) one-hot objects
        Returns:
            scores: (batch, n_candidates)
        """
        batch = msg_soft.shape[0]
        n_cand = candidates.shape[1]

        # Encode message
        msg_flat = msg_soft.reshape(batch, -1)  # (batch, MSG_LEN * VOCAB_SIZE)
        msg_emb = self.msg_enc2(self.msg_enc1(msg_flat).relu()).relu()  # (batch, HIDDEN)

        # Encode candidates
        cand_flat = candidates.reshape(batch * n_cand, -1)  # (batch*n_cand, 32)
        cand_emb = self.obj_enc2(self.obj_enc1(cand_flat).relu()).relu()
        cand_emb = cand_emb.reshape(batch, n_cand, HIDDEN_DIM)  # (batch, n_cand, HIDDEN)

        # Dot-product scoring
        scores = (cand_emb * msg_emb.unsqueeze(1)).sum(axis=-1)  # (batch, n_cand)
        return scores


# ─── Training ────────────────────────────────────────────────────────────────

def get_temperature(step):
    if step >= TEMP_ANNEAL_STEPS:
        return TEMP_MIN
    return TEMP_INIT * (TEMP_MIN / TEMP_INIT) ** (step / TEMP_ANNEAL_STEPS)


def train():
    """Run the full training loop with TinyJit for speed."""
    global LABELS
    np.random.seed(SEED)
    random.seed(SEED)
    Tensor.manual_seed(SEED)

    sender = Sender()
    receiver = Receiver()
    params = get_parameters(sender) + get_parameters(receiver)
    opt = Adam(params, lr=LR)
    LABELS = Tensor.zeros(BATCH_SIZE, dtype=dtypes.int)

    # JIT-compiled training step — all random data passed as arguments
    @TinyJit
    def train_step(tgt_oh, cand_oh, noise_packed, temp_t):
        msg_soft, _ = sender(tgt_oh, temp_t, noise_packed=noise_packed)
        scores = receiver(msg_soft, cand_oh)
        loss = scores.sparse_categorical_crossentropy(LABELS)
        loss.backward()
        opt.step()
        opt.zero_grad()
        return loss.realize(), (scores.argmax(axis=-1) == 0).mean().realize()

    history = {"step": [], "loss": [], "accuracy": [], "temperature": [],
               "msg_entropy": [], "topsim": []}

    print(f"Training Lewis signaling game")
    print(f"Objects: {N_ATTRIBUTES} attrs × {N_VALUES} values = {N_OBJECTS}")
    print(f"Messages: {MSG_LEN} tokens × {VOCAB_SIZE} vocab")
    print(f"Distractors: {N_DISTRACTORS} (chance = {1/N_CANDIDATES:.2%})")
    print(f"Steps: {N_STEPS}, Batch: {BATCH_SIZE}")
    print("─" * 60)

    Tensor.training = True

    for step in range(N_STEPS):
        temperature = get_temperature(step)

        # All random data generated OUTSIDE JIT
        tgt_oh_np, cand_oh_np, _ = sample_batch_np(BATCH_SIZE, N_DISTRACTORS)
        tgt_oh = Tensor(tgt_oh_np)
        cand_oh = Tensor(cand_oh_np)
        noise_packed = Tensor.rand(MSG_LEN, BATCH_SIZE, VOCAB_SIZE)
        temp_t = Tensor([temperature])

        loss, acc = train_step(tgt_oh, cand_oh, noise_packed, temp_t)

        # Logging (infrequent, not JIT'd)
        if step % EVAL_EVERY == 0 or step == N_STEPS - 1:
            Tensor.training = False
            loss_val = loss.numpy()
            acc_val = acc.numpy()

            entropy = compute_message_entropy(sender)
            topsim = compute_topographic_similarity(sender, n_samples=1000)

            history["step"].append(step)
            history["loss"].append(float(loss_val))
            history["accuracy"].append(float(acc_val))
            history["temperature"].append(temperature)
            history["msg_entropy"].append(entropy)
            history["topsim"].append(topsim)

            print(f"Step {step:>6d} | Loss {loss_val:.4f} | Acc {acc_val:.4f} | "
                  f"Temp {temperature:.3f} | H(msg) {entropy:.2f} | TopSim {topsim:.4f}")
            Tensor.training = True

    return sender, receiver, history


# ─── Analysis Metrics ────────────────────────────────────────────────────────

def get_all_messages(sender, temperature=0.1, batch_size=512):
    """Get discrete messages for all objects."""
    all_messages = []
    for i in range(0, N_OBJECTS, batch_size):
        end = min(i + batch_size, N_OBJECTS)
        oh = Tensor(ALL_ONEHOT_NP[i:end])
        _, msg_hard = sender(oh, temperature)
        all_messages.append(msg_hard.numpy())
    return np.concatenate(all_messages, axis=0)  # (4096, MSG_LEN)


def compute_message_entropy(sender, temperature=0.1):
    """Entropy of the message distribution over all objects."""
    all_msgs = get_all_messages(sender, temperature)
    msg_tuples = [tuple(m) for m in all_msgs]
    counts = {}
    for m in msg_tuples:
        counts[m] = counts.get(m, 0) + 1
    total = len(msg_tuples)
    entropy = sum(-c/total * math.log2(c/total) for c in counts.values())
    return entropy


def compute_topographic_similarity(sender, n_samples=2000, temperature=0.1):
    """Spearman correlation between input distances and message distances."""
    all_msgs = get_all_messages(sender, temperature)

    idx = np.random.choice(N_OBJECTS, size=n_samples, replace=False)
    sample_objects = ALL_OBJECTS_NP[idx]
    sample_msgs = all_msgs[idx]

    n_pairs = min(50000, n_samples * (n_samples - 1) // 2)
    pairs = [random.sample(range(n_samples), 2) for _ in range(n_pairs)]

    input_dists = []
    msg_dists = []
    for i, j in pairs:
        d_in = (sample_objects[i] != sample_objects[j]).sum()
        d_msg = (sample_msgs[i] != sample_msgs[j]).sum()
        input_dists.append(d_in)
        msg_dists.append(d_msg)

    rho, _ = stats.spearmanr(input_dists, msg_dists)
    return rho if not math.isnan(rho) else 0.0


def compute_positional_disentanglement(sender, temperature=0.1):
    """MI between each message position and each attribute."""
    all_msgs = get_all_messages(sender, temperature)

    mi_matrix = np.zeros((MSG_LEN, N_ATTRIBUTES))
    for p in range(MSG_LEN):
        for a in range(N_ATTRIBUTES):
            mi_matrix[p, a] = _mutual_information(all_msgs[:, p], ALL_OBJECTS_NP[:, a])

    # Disentanglement score
    scores = []
    for p in range(MSG_LEN):
        row = mi_matrix[p]
        if row.sum() == 0:
            scores.append(0.0)
            continue
        probs = row / row.sum()
        h = -sum(p_ * math.log2(p_) for p_ in probs if p_ > 0)
        scores.append(1.0 - h / math.log2(N_ATTRIBUTES))

    return mi_matrix, np.mean(scores)


def _mutual_information(x, y):
    """MI between two discrete arrays."""
    joint = {}
    for xi, yi in zip(x, y):
        key = (int(xi), int(yi))
        joint[key] = joint.get(key, 0) + 1
    total = len(x)
    px, py = {}, {}
    for (xi, yi), c in joint.items():
        px[xi] = px.get(xi, 0) + c
        py[yi] = py.get(yi, 0) + c
    mi = 0.0
    for (xi, yi), c in joint.items():
        p_xy = c / total
        p_x = px[xi] / total
        p_y = py[yi] / total
        if p_xy > 0 and p_x > 0 and p_y > 0:
            mi += p_xy * math.log2(p_xy / (p_x * p_y))
    return mi


def compute_info_density(sender, temperature=0.1):
    """Information density of the emerged protocol."""
    entropy = compute_message_entropy(sender, temperature)
    needed_bits = math.log2(N_VALUES ** N_ATTRIBUTES)
    max_capacity = MSG_LEN * math.log2(VOCAB_SIZE)
    return {
        "message_entropy_bits": entropy,
        "needed_bits": needed_bits,
        "max_capacity_bits": max_capacity,
        "efficiency": entropy / needed_bits,
        "utilization": entropy / max_capacity,
    }


# ─── Limn Comparison ────────────────────────────────────────────────────────

def compare_to_limn(mi_matrix, topsim, info_density):
    """Compare emerged protocol to Limn's design principles."""
    return {
        "compositionality": {
            "topsim": topsim,
            "interpretation": (
                "HIGH (>0.4): Protocol is compositional — similar objects get similar messages. "
                "Parallels Limn's compositional operator design."
                if topsim > 0.4 else
                "LOW (<0.4): Protocol is holistic — messages are arbitrary codes. "
                "Suggests Limn's compositionality is a design choice, not inevitable."
            ),
        },
        "disentanglement": {
            "mi_matrix": mi_matrix.tolist(),
            "interpretation": (
                "Rows = message positions, columns = attributes. "
                "Limn analogy: @ projects one dimension, * blends two, ^ scales. "
                "Diagonal MI = Limn-like disentangled encoding."
            ),
        },
        "information_density": {
            "emerged_efficiency": info_density["efficiency"],
            "emerged_bits_per_token": info_density["message_entropy_bits"] / MSG_LEN,
            "limn_comparison": (
                f"Emerged: {info_density['message_entropy_bits']:.1f} bits in "
                f"{MSG_LEN} tokens = {info_density['message_entropy_bits']/MSG_LEN:.2f} bits/token. "
                f"Limn: 14.9 compressed bits/semantic unit vs English 30.8. "
                f"Efficiency: {info_density['efficiency']:.1%}."
            ),
        },
    }


# ─── Main ────────────────────────────────────────────────────────────────────

def run_experiment():
    print("=" * 60)
    print("EXPERIMENT: Emergent Communication vs Limn")
    print("Lewis Signaling Game — Compositional Convergence Test")
    print("=" * 60)
    print()

    sender, receiver, history = train()

    print()
    print("─" * 60)
    print("ANALYSIS")
    print("─" * 60)

    Tensor.training = False

    final_acc = history["accuracy"][-1]
    final_topsim = history["topsim"][-1]
    print(f"\nFinal accuracy: {final_acc:.4f} (chance: {1/N_CANDIDATES:.4f})")
    print(f"Final topsim: {final_topsim:.4f}")

    mi_matrix, disent_score = compute_positional_disentanglement(sender)
    print(f"\nPositional disentanglement: {disent_score:.4f}")
    print("MI matrix (rows=positions, cols=attributes):")
    for p in range(MSG_LEN):
        row = " ".join(f"{mi_matrix[p, a]:.3f}" for a in range(N_ATTRIBUTES))
        print(f"  Position {p}: [{row}]")

    info_density = compute_info_density(sender)
    print(f"\nInformation density:")
    print(f"  Message entropy: {info_density['message_entropy_bits']:.2f} bits")
    print(f"  Needed: {info_density['needed_bits']:.2f} bits")
    print(f"  Max capacity: {info_density['max_capacity_bits']:.2f} bits")
    print(f"  Efficiency: {info_density['efficiency']:.2%}")
    print(f"  Utilization: {info_density['utilization']:.2%}")

    comparison = compare_to_limn(mi_matrix, final_topsim, info_density)
    print(f"\n{'─' * 60}")
    print("LIMN COMPARISON")
    print(f"{'─' * 60}")
    for key, val in comparison.items():
        print(f"\n{key.upper()}:")
        for k, v in val.items():
            if k != "mi_matrix":
                print(f"  {k}: {v}")

    results = {
        "config": {
            "n_attributes": N_ATTRIBUTES, "n_values": N_VALUES,
            "vocab_size": VOCAB_SIZE, "msg_len": MSG_LEN,
            "hidden_dim": HIDDEN_DIM, "n_distractors": N_DISTRACTORS,
            "batch_size": BATCH_SIZE, "n_steps": N_STEPS, "seed": SEED,
            "device": Device.DEFAULT,
        },
        "final_metrics": {
            "accuracy": final_acc,
            "topographic_similarity": final_topsim,
            "disentanglement_score": float(disent_score),
            "mi_matrix": mi_matrix.tolist(),
            "info_density": info_density,
        },
        "training_history": history,
        "limn_comparison": comparison,
    }

    with open(RESULTS_FILE, "w") as f:
        json.dump(results, f, indent=2, default=str)
    print(f"\nResults saved to {RESULTS_FILE}")

    # Save models
    safe_save(get_state_dict(sender), str(OUTPUT_DIR / "sender.safetensors"))
    safe_save(get_state_dict(receiver), str(OUTPUT_DIR / "receiver.safetensors"))
    print(f"Models saved to {OUTPUT_DIR}")

    return results


if __name__ == "__main__":
    results = run_experiment()
