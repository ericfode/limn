#!/usr/bin/env python3
"""
Lewis Game Variants — Compositionality Pressure Experiments (tinygrad)

Bead: hq-2uoa8
Depends on: hq-jbm2m (baseline, topsim≈0.27, holistic)

5 variants test which pressures induce compositionality:
  A: GENERALIZATION — 80/20 train/test split on objects
  B: BOTTLENECK    — Reduce vocab from 32→8 (matching attribute values)
  C: COMBINED      — Both A+B (strongest compositionality expected)
  D: ENTROPY REG   — Message entropy bonus prevents codebook collapse
  E: GROKKING      — 200k steps, AdamW w/ weight_decay=0.01, track phase transition

Compare topsim, disentanglement, and info density across all variants vs baseline.

— Lex
"""

import json
import math
import os
import random
import sys
import time
from itertools import product
from pathlib import Path

import numpy as np
from scipy import stats

os.environ.setdefault("CUDA_PATH", "/usr/lib/wsl/lib/libcuda.so.1")

from tinygrad import Tensor, Device
from tinygrad.nn.state import get_parameters
from tinygrad.nn.optim import Adam, AdamW
import tinygrad.nn as nn

# ─── Shared infrastructure from baseline ────────────────────────────────────

N_ATTRIBUTES = 4
N_VALUES = 8
HIDDEN_DIM = 256
N_DISTRACTORS = 3
N_CANDIDATES = 1 + N_DISTRACTORS
MSG_LEN = 4
BATCH_SIZE = 512
SEED = 42

ALL_OBJECTS_NP = np.array(list(product(range(N_VALUES), repeat=N_ATTRIBUTES)), dtype=np.int32)
N_OBJECTS = len(ALL_OBJECTS_NP)


def objects_to_onehot_np(objects_np):
    batch = objects_np.shape[0]
    onehot = np.zeros((batch, N_ATTRIBUTES * N_VALUES), dtype=np.float32)
    for i in range(N_ATTRIBUTES):
        onehot[np.arange(batch), i * N_VALUES + objects_np[:, i]] = 1.0
    return onehot


ALL_ONEHOT_NP = objects_to_onehot_np(ALL_OBJECTS_NP)


def gumbel_softmax(logits, temperature, hard=True):
    u = Tensor.rand(*logits.shape)
    g = -(-(u + 1e-20).log() + 1e-20).log()
    y = ((logits + g) / temperature).softmax(axis=-1)
    if hard:
        idx = y.argmax(axis=-1)
        y_hard = idx.one_hot(logits.shape[-1]).cast(y.dtype)
        y = (y_hard - y).detach() + y
    return y


class Sender:
    def __init__(self, vocab_size=32):
        input_dim = N_ATTRIBUTES * N_VALUES
        self.vocab_size = vocab_size
        self.enc1 = nn.Linear(input_dim, HIDDEN_DIM)
        self.enc2 = nn.Linear(HIDDEN_DIM, HIDDEN_DIM)
        self.heads = [nn.Linear(HIDDEN_DIM, vocab_size) for _ in range(MSG_LEN)]

    def __call__(self, x, temperature):
        h = self.enc2(self.enc1(x).relu()).relu()
        softs, hards = [], []
        for head in self.heads:
            logits = head(h)
            gs = gumbel_softmax(logits, temperature, hard=True)
            softs.append(gs)
            hards.append(gs.argmax(axis=-1))
        msg_soft = Tensor.stack(*softs, dim=1)
        msg_hard = Tensor.stack(*hards, dim=1)
        return msg_soft, msg_hard


class Receiver:
    def __init__(self, vocab_size=32):
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


# ─── Analysis functions ─────────────────────────────────────────────────────

def get_all_messages(sender, temperature=0.1, batch_size=512):
    all_messages = []
    for i in range(0, N_OBJECTS, batch_size):
        end = min(i + batch_size, N_OBJECTS)
        oh = Tensor(ALL_ONEHOT_NP[i:end])
        _, msg_hard = sender(oh, temperature)
        all_messages.append(msg_hard.numpy())
    return np.concatenate(all_messages, axis=0)


def compute_message_entropy(sender, temperature=0.1):
    all_msgs = get_all_messages(sender, temperature)
    msg_tuples = [tuple(m) for m in all_msgs]
    counts = {}
    for m in msg_tuples:
        counts[m] = counts.get(m, 0) + 1
    total = len(msg_tuples)
    return sum(-c/total * math.log2(c/total) for c in counts.values())


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


def _mutual_information(x, y):
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


def compute_positional_disentanglement(sender, temperature=0.1):
    all_msgs = get_all_messages(sender, temperature)
    mi_matrix = np.zeros((MSG_LEN, N_ATTRIBUTES))
    for p in range(MSG_LEN):
        for a in range(N_ATTRIBUTES):
            mi_matrix[p, a] = _mutual_information(all_msgs[:, p], ALL_OBJECTS_NP[:, a])
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


def compute_info_density(sender, temperature=0.1, vocab_size=32):
    entropy = compute_message_entropy(sender, temperature)
    needed_bits = math.log2(N_VALUES ** N_ATTRIBUTES)
    max_capacity = MSG_LEN * math.log2(vocab_size)
    return {
        "message_entropy_bits": entropy,
        "needed_bits": needed_bits,
        "max_capacity_bits": max_capacity,
        "efficiency": entropy / needed_bits if needed_bits > 0 else 0,
        "utilization": entropy / max_capacity if max_capacity > 0 else 0,
    }


# ─── Device Selection ───────────────────────────────────────────────────────

if os.path.exists("/usr/lib/wsl/lib/libcuda.so.1"):
    try:
        Device.DEFAULT = "CUDA"
        Tensor.randn(2, 2).numpy()
    except Exception:
        Device.DEFAULT = "CPU"
else:
    Device.DEFAULT = "CPU"

print(f"Device: {Device.DEFAULT}")


# ─── Sampling helpers ───────────────────────────────────────────────────────

def sample_batch_from_pool(pool_indices, batch_size, n_distractors):
    """Sample batch from a restricted pool of object indices."""
    chosen = np.random.choice(pool_indices, size=(batch_size, 1 + n_distractors))
    targets_oh = ALL_ONEHOT_NP[chosen[:, 0]]
    candidates_oh = ALL_ONEHOT_NP[chosen.flatten()].reshape(
        batch_size, 1 + n_distractors, N_ATTRIBUTES * N_VALUES)
    target_attrs = ALL_OBJECTS_NP[chosen[:, 0]]
    return targets_oh, candidates_oh, target_attrs


def sample_batch_all(batch_size, n_distractors):
    """Sample from all objects (baseline behavior)."""
    return sample_batch_from_pool(np.arange(N_OBJECTS), batch_size, n_distractors)


def get_temperature(step, anneal_steps=10_000, temp_init=2.0, temp_min=0.5):
    if step >= anneal_steps:
        return temp_min
    return temp_init * (temp_min / temp_init) ** (step / anneal_steps)


# ─── Variant runners ────────────────────────────────────────────────────────

def run_variant_a(n_steps=20_000, lr=1e-3):
    """GENERALIZATION — 80/20 train/test split."""
    print("\n" + "=" * 60)
    print("VARIANT A: GENERALIZATION (80/20 split)")
    print("=" * 60)

    np.random.seed(SEED); random.seed(SEED); Tensor.manual_seed(SEED)

    # Split objects: 80% train, 20% test
    perm = np.random.permutation(N_OBJECTS)
    split = int(0.8 * N_OBJECTS)
    train_idx = perm[:split]
    test_idx = perm[split:]
    print(f"Train: {len(train_idx)}, Test: {len(test_idx)}")

    sender = Sender(vocab_size=32)
    receiver = Receiver(vocab_size=32)
    params = get_parameters(sender) + get_parameters(receiver)
    opt = Adam(params, lr=lr)

    history = {"step": [], "loss": [], "train_acc": [], "test_acc": [],
               "topsim": [], "temperature": []}

    Tensor.training = True
    for step in range(n_steps):
        temperature = get_temperature(step)
        tgt_oh_np, cand_oh_np, _ = sample_batch_from_pool(train_idx, BATCH_SIZE, N_DISTRACTORS)
        tgt_oh = Tensor(tgt_oh_np)
        cand_oh = Tensor(cand_oh_np)
        msg_soft, _ = sender(tgt_oh, temperature)
        scores = receiver(msg_soft, cand_oh)
        labels = Tensor.zeros(BATCH_SIZE).cast("int")
        loss = scores.sparse_categorical_crossentropy(labels)
        loss.backward()
        opt.step()
        opt.zero_grad()

        if step % 500 == 0 or step == n_steps - 1:
            Tensor.training = False
            loss_val = loss.numpy()
            train_acc = (scores.argmax(axis=-1).numpy() == 0).mean()

            # Test accuracy on held-out objects
            tgt_test, cand_test, _ = sample_batch_from_pool(test_idx, min(BATCH_SIZE, len(test_idx)), N_DISTRACTORS)
            msg_test, _ = sender(Tensor(tgt_test), 0.1)
            scores_test = receiver(msg_test, Tensor(cand_test))
            test_acc = (scores_test.argmax(axis=-1).numpy() == 0).mean()

            topsim = compute_topographic_similarity(sender, n_samples=1000)

            history["step"].append(step)
            history["loss"].append(float(loss_val))
            history["train_acc"].append(float(train_acc))
            history["test_acc"].append(float(test_acc))
            history["topsim"].append(topsim)
            history["temperature"].append(temperature)

            print(f"Step {step:>6d} | Loss {loss_val:.4f} | TrainAcc {train_acc:.4f} | "
                  f"TestAcc {test_acc:.4f} | TopSim {topsim:.4f}")
            Tensor.training = True

    Tensor.training = False
    mi_matrix, disent = compute_positional_disentanglement(sender)
    info_d = compute_info_density(sender, vocab_size=32)
    return {"variant": "A_GENERALIZATION", "history": history,
            "final_topsim": history["topsim"][-1],
            "final_test_acc": history["test_acc"][-1],
            "disentanglement": float(disent),
            "info_density": info_d, "mi_matrix": mi_matrix.tolist()}


def run_variant_b(n_steps=20_000, lr=1e-3):
    """BOTTLENECK — Vocab 8 (matching attribute values)."""
    print("\n" + "=" * 60)
    print("VARIANT B: BOTTLENECK (vocab=8)")
    print("=" * 60)

    vocab_size = 8
    np.random.seed(SEED); random.seed(SEED); Tensor.manual_seed(SEED)

    sender = Sender(vocab_size=vocab_size)
    receiver = Receiver(vocab_size=vocab_size)
    params = get_parameters(sender) + get_parameters(receiver)
    opt = Adam(params, lr=lr)

    history = {"step": [], "loss": [], "accuracy": [], "topsim": [], "temperature": []}

    Tensor.training = True
    for step in range(n_steps):
        temperature = get_temperature(step)
        tgt_oh_np, cand_oh_np, _ = sample_batch_all(BATCH_SIZE, N_DISTRACTORS)
        tgt_oh = Tensor(tgt_oh_np)
        cand_oh = Tensor(cand_oh_np)
        msg_soft, _ = sender(tgt_oh, temperature)
        scores = receiver(msg_soft, cand_oh)
        labels = Tensor.zeros(BATCH_SIZE).cast("int")
        loss = scores.sparse_categorical_crossentropy(labels)
        loss.backward()
        opt.step()
        opt.zero_grad()

        if step % 500 == 0 or step == n_steps - 1:
            Tensor.training = False
            loss_val = loss.numpy()
            acc = (scores.argmax(axis=-1).numpy() == 0).mean()
            topsim = compute_topographic_similarity(sender, n_samples=1000)
            history["step"].append(step)
            history["loss"].append(float(loss_val))
            history["accuracy"].append(float(acc))
            history["topsim"].append(topsim)
            history["temperature"].append(temperature)
            print(f"Step {step:>6d} | Loss {loss_val:.4f} | Acc {acc:.4f} | TopSim {topsim:.4f}")
            Tensor.training = True

    Tensor.training = False
    mi_matrix, disent = compute_positional_disentanglement(sender)
    info_d = compute_info_density(sender, vocab_size=vocab_size)
    return {"variant": "B_BOTTLENECK", "history": history,
            "final_topsim": history["topsim"][-1],
            "final_accuracy": history["accuracy"][-1],
            "disentanglement": float(disent),
            "info_density": info_d, "mi_matrix": mi_matrix.tolist()}


def run_variant_c(n_steps=20_000, lr=1e-3):
    """COMBINED — Generalization + Bottleneck."""
    print("\n" + "=" * 60)
    print("VARIANT C: COMBINED (80/20 split + vocab=8)")
    print("=" * 60)

    vocab_size = 8
    np.random.seed(SEED); random.seed(SEED); Tensor.manual_seed(SEED)

    perm = np.random.permutation(N_OBJECTS)
    split = int(0.8 * N_OBJECTS)
    train_idx = perm[:split]
    test_idx = perm[split:]
    print(f"Train: {len(train_idx)}, Test: {len(test_idx)}")

    sender = Sender(vocab_size=vocab_size)
    receiver = Receiver(vocab_size=vocab_size)
    params = get_parameters(sender) + get_parameters(receiver)
    opt = Adam(params, lr=lr)

    history = {"step": [], "loss": [], "train_acc": [], "test_acc": [],
               "topsim": [], "temperature": []}

    Tensor.training = True
    for step in range(n_steps):
        temperature = get_temperature(step)
        tgt_oh_np, cand_oh_np, _ = sample_batch_from_pool(train_idx, BATCH_SIZE, N_DISTRACTORS)
        tgt_oh = Tensor(tgt_oh_np)
        cand_oh = Tensor(cand_oh_np)
        msg_soft, _ = sender(tgt_oh, temperature)
        scores = receiver(msg_soft, cand_oh)
        labels = Tensor.zeros(BATCH_SIZE).cast("int")
        loss = scores.sparse_categorical_crossentropy(labels)
        loss.backward()
        opt.step()
        opt.zero_grad()

        if step % 500 == 0 or step == n_steps - 1:
            Tensor.training = False
            loss_val = loss.numpy()
            train_acc = (scores.argmax(axis=-1).numpy() == 0).mean()

            tgt_test, cand_test, _ = sample_batch_from_pool(test_idx, min(BATCH_SIZE, len(test_idx)), N_DISTRACTORS)
            msg_test, _ = sender(Tensor(tgt_test), 0.1)
            scores_test = receiver(msg_test, Tensor(cand_test))
            test_acc = (scores_test.argmax(axis=-1).numpy() == 0).mean()

            topsim = compute_topographic_similarity(sender, n_samples=1000)
            history["step"].append(step)
            history["loss"].append(float(loss_val))
            history["train_acc"].append(float(train_acc))
            history["test_acc"].append(float(test_acc))
            history["topsim"].append(topsim)
            history["temperature"].append(temperature)
            print(f"Step {step:>6d} | Loss {loss_val:.4f} | TrainAcc {train_acc:.4f} | "
                  f"TestAcc {test_acc:.4f} | TopSim {topsim:.4f}")
            Tensor.training = True

    Tensor.training = False
    mi_matrix, disent = compute_positional_disentanglement(sender)
    info_d = compute_info_density(sender, vocab_size=vocab_size)
    return {"variant": "C_COMBINED", "history": history,
            "final_topsim": history["topsim"][-1],
            "final_test_acc": history["test_acc"][-1],
            "disentanglement": float(disent),
            "info_density": info_d, "mi_matrix": mi_matrix.tolist()}


def run_variant_d(n_steps=20_000, lr=1e-3, entropy_weight=0.5):
    """ENTROPY REGULARIZATION — Message entropy bonus."""
    print("\n" + "=" * 60)
    print(f"VARIANT D: ENTROPY REGULARIZATION (weight={entropy_weight})")
    print("=" * 60)

    np.random.seed(SEED); random.seed(SEED); Tensor.manual_seed(SEED)

    sender = Sender(vocab_size=32)
    receiver = Receiver(vocab_size=32)
    params = get_parameters(sender) + get_parameters(receiver)
    opt = Adam(params, lr=lr)

    history = {"step": [], "loss": [], "accuracy": [], "topsim": [],
               "msg_entropy": [], "temperature": []}

    Tensor.training = True
    for step in range(n_steps):
        temperature = get_temperature(step)
        tgt_oh_np, cand_oh_np, _ = sample_batch_all(BATCH_SIZE, N_DISTRACTORS)
        tgt_oh = Tensor(tgt_oh_np)
        cand_oh = Tensor(cand_oh_np)
        msg_soft, _ = sender(tgt_oh, temperature)
        scores = receiver(msg_soft, cand_oh)
        labels = Tensor.zeros(BATCH_SIZE).cast("int")
        task_loss = scores.sparse_categorical_crossentropy(labels)

        # Entropy bonus: encourage diverse token usage per position
        # msg_soft: (batch, MSG_LEN, VOCAB_SIZE)
        # Average distribution per position across batch
        avg_dist = msg_soft.mean(axis=0)  # (MSG_LEN, VOCAB_SIZE)
        # Entropy of average distribution (higher = more diverse)
        position_entropy = -(avg_dist * (avg_dist + 1e-10).log()).sum(axis=-1)  # (MSG_LEN,)
        entropy_bonus = position_entropy.mean()

        # Minimize task_loss, maximize entropy (subtract entropy = minimize negative entropy)
        loss = task_loss - entropy_weight * entropy_bonus
        loss.backward()
        opt.step()
        opt.zero_grad()

        if step % 500 == 0 or step == n_steps - 1:
            Tensor.training = False
            loss_val = task_loss.numpy()
            acc = (scores.argmax(axis=-1).numpy() == 0).mean()
            topsim = compute_topographic_similarity(sender, n_samples=1000)
            msg_ent = compute_message_entropy(sender)
            history["step"].append(step)
            history["loss"].append(float(loss_val))
            history["accuracy"].append(float(acc))
            history["topsim"].append(topsim)
            history["msg_entropy"].append(msg_ent)
            history["temperature"].append(temperature)
            print(f"Step {step:>6d} | Loss {loss_val:.4f} | Acc {acc:.4f} | "
                  f"TopSim {topsim:.4f} | H(msg) {msg_ent:.2f}")
            Tensor.training = True

    Tensor.training = False
    mi_matrix, disent = compute_positional_disentanglement(sender)
    info_d = compute_info_density(sender, vocab_size=32)
    return {"variant": "D_ENTROPY_REG", "history": history,
            "final_topsim": history["topsim"][-1],
            "final_accuracy": history["accuracy"][-1],
            "disentanglement": float(disent),
            "info_density": info_d, "mi_matrix": mi_matrix.tolist()}


def run_variant_e(n_steps=200_000, lr=1e-3, weight_decay=0.01):
    """GROKKING — Long training with weight decay. Track phase transition."""
    print("\n" + "=" * 60)
    print(f"VARIANT E: GROKKING ({n_steps} steps, AdamW wd={weight_decay})")
    print("=" * 60)
    print("Looking for holistic→compositional phase transition...")

    np.random.seed(SEED); random.seed(SEED); Tensor.manual_seed(SEED)

    sender = Sender(vocab_size=32)
    receiver = Receiver(vocab_size=32)
    params = get_parameters(sender) + get_parameters(receiver)
    opt = AdamW(params, lr=lr, wd=weight_decay)

    # Track every 1k steps for fine-grained topsim curve
    eval_every = 1_000
    history = {"step": [], "loss": [], "accuracy": [], "topsim": [],
               "weight_norm": [], "temperature": []}

    Tensor.training = True
    for step in range(n_steps):
        temperature = get_temperature(step, anneal_steps=20_000)
        tgt_oh_np, cand_oh_np, _ = sample_batch_all(BATCH_SIZE, N_DISTRACTORS)
        tgt_oh = Tensor(tgt_oh_np)
        cand_oh = Tensor(cand_oh_np)
        msg_soft, _ = sender(tgt_oh, temperature)
        scores = receiver(msg_soft, cand_oh)
        labels = Tensor.zeros(BATCH_SIZE).cast("int")
        loss = scores.sparse_categorical_crossentropy(labels)
        loss.backward()
        opt.step()
        opt.zero_grad()

        if step % eval_every == 0 or step == n_steps - 1:
            Tensor.training = False
            loss_val = loss.numpy()
            acc = (scores.argmax(axis=-1).numpy() == 0).mean()
            topsim = compute_topographic_similarity(sender, n_samples=1000)

            # Track total weight norm (grokking signature: norm decreases at transition)
            total_norm = sum(float((p * p).sum().numpy()) for p in params)
            total_norm = math.sqrt(total_norm)

            history["step"].append(step)
            history["loss"].append(float(loss_val))
            history["accuracy"].append(float(acc))
            history["topsim"].append(topsim)
            history["weight_norm"].append(total_norm)
            history["temperature"].append(temperature)

            # Detect phase transition: topsim jump > 0.15 between consecutive evals
            if len(history["topsim"]) >= 2:
                delta = history["topsim"][-1] - history["topsim"][-2]
                marker = " *** PHASE TRANSITION?" if delta > 0.15 else ""
            else:
                marker = ""

            print(f"Step {step:>7d} | Loss {loss_val:.4f} | Acc {acc:.4f} | "
                  f"TopSim {topsim:.4f} | ||W|| {total_norm:.1f}{marker}")
            Tensor.training = True

    Tensor.training = False
    mi_matrix, disent = compute_positional_disentanglement(sender)
    info_d = compute_info_density(sender, vocab_size=32)

    # Find if grokking occurred: topsim jumped from <0.35 to >0.55 at some point
    topsim_arr = np.array(history["topsim"])
    grokked = bool(topsim_arr.max() > 0.55 and topsim_arr[:5].mean() < 0.35)

    return {"variant": "E_GROKKING", "history": history,
            "final_topsim": history["topsim"][-1],
            "final_accuracy": history["accuracy"][-1],
            "disentanglement": float(disent),
            "info_density": info_d, "mi_matrix": mi_matrix.tolist(),
            "grokked": grokked,
            "max_topsim": float(topsim_arr.max()),
            "topsim_at_20k": float(topsim_arr[min(20, len(topsim_arr)-1)])}


# ─── Main ───────────────────────────────────────────────────────────────────

OUTPUT_DIR = Path(__file__).resolve().parent
RESULTS_FILE = OUTPUT_DIR / "variant_results.json"


def run_all():
    print("=" * 60)
    print("LEWIS GAME VARIANTS — Compositionality Pressure Experiments")
    print(f"Baseline reference: topsim≈0.27 (holistic)")
    print("=" * 60)

    all_results = {}
    t0 = time.time()

    # Run A-D first (20k steps each), then E (200k steps)
    for name, fn in [("A", run_variant_a), ("B", run_variant_b),
                     ("C", run_variant_c), ("D", run_variant_d)]:
        t1 = time.time()
        result = fn()
        elapsed = time.time() - t1
        result["elapsed_seconds"] = elapsed
        all_results[name] = result
        print(f"\nVariant {name} done in {elapsed:.1f}s — topsim={result['final_topsim']:.4f}")

        # Save intermediate results
        with open(RESULTS_FILE, "w") as f:
            json.dump(all_results, f, indent=2, default=str)

    # Variant E last (long run)
    t1 = time.time()
    result = run_variant_e()
    elapsed = time.time() - t1
    result["elapsed_seconds"] = elapsed
    all_results["E"] = result
    print(f"\nVariant E done in {elapsed:.1f}s — topsim={result['final_topsim']:.4f}")
    if result["grokked"]:
        print("*** GROKKING DETECTED — compositionality emerged as phase transition!")

    total = time.time() - t0

    # ─── Summary table ───────────────────────────────────────────────────
    print("\n" + "=" * 60)
    print("SUMMARY: Compositionality Pressure Results")
    print("=" * 60)
    print(f"{'Variant':<25} {'TopSim':>8} {'Disent':>8} {'Eff%':>8} {'Acc':>8}")
    print("─" * 60)
    print(f"{'Baseline (reference)':<25} {'0.27':>8} {'0.06':>8} {'—':>8} {'0.996':>8}")
    for k in ["A", "B", "C", "D", "E"]:
        r = all_results[k]
        acc_key = "final_test_acc" if "final_test_acc" in r else "final_accuracy"
        acc = r.get(acc_key, 0)
        eff = r["info_density"]["efficiency"]
        print(f"{r['variant']:<25} {r['final_topsim']:>8.4f} {r['disentanglement']:>8.4f} "
              f"{eff:>7.1%} {acc:>8.4f}")
    print("─" * 60)
    print(f"Total time: {total:.0f}s")

    # Save final results
    all_results["_meta"] = {
        "baseline_topsim": 0.27,
        "total_seconds": total,
        "device": Device.DEFAULT,
        "seed": SEED,
    }
    with open(RESULTS_FILE, "w") as f:
        json.dump(all_results, f, indent=2, default=str)
    print(f"\nResults saved to {RESULTS_FILE}")

    return all_results


if __name__ == "__main__":
    run_all()
