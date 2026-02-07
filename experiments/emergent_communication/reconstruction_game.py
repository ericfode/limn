#!/usr/bin/env python3
"""
Reconstruction Game — Discrimination vs Reconstruction Objective

Ben Zion et al. (2024, NeurIPS) proved that:
- Discrimination objectives can be solved with semantically INCONSISTENT protocols
- Reconstruction objectives enforce semantic CONSISTENCY (same message = same meaning)
- Reconstruction also encourages spatial meaningfulness (similar messages = similar meanings)

This experiment compares:
  Condition A: Standard discrimination (Lewis game, our baseline)
  Condition B: Reconstruction (receiver must output all 4 attribute values from message)
  Condition C: Combined (discrimination + reconstruction auxiliary loss)

H20 showed discrimination with random distractors needs only 1 symbol.
Reconstruction inherently requires ALL attributes → forces full encoding.

Reference: Ben Zion, Carmeli, Paradise & Belinkov (2024) "Semantics and
Spatiality of Emergent Communication" (NeurIPS 2024, arXiv:2411.10173)

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
from tinygrad.nn.state import get_parameters
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
BATCH_SIZE = 512
SEED = 42
TRAIN_STEPS = 20_000
LR = 1e-3
TEMP_INIT = 2.0
TEMP_MIN = 0.5
TEMP_ANNEAL_STEPS = 5_000
EVAL_EVERY = 2000

OUTPUT_DIR = Path(__file__).resolve().parent
RESULTS_FILE = OUTPUT_DIR / "reconstruction_results.json"


# ─── Object Space ────────────────────────────────────────────────────────────

ALL_OBJECTS_NP = np.array(list(product(range(N_VALUES), repeat=N_ATTRIBUTES)), dtype=np.int32)
N_OBJECTS = len(ALL_OBJECTS_NP)


def objects_to_onehot_np(objects_np):
    batch = objects_np.shape[0]
    onehot = np.zeros((batch, N_ATTRIBUTES * N_VALUES), dtype=np.float32)
    for i in range(N_ATTRIBUTES):
        onehot[np.arange(batch), i * N_VALUES + objects_np[:, i]] = 1.0
    return onehot


ALL_ONEHOT_NP = objects_to_onehot_np(ALL_OBJECTS_NP)


def sample_batch_np(batch_size, n_distractors):
    target_idx = np.random.randint(0, N_OBJECTS, size=batch_size)
    distractor_idx = np.random.randint(0, N_OBJECTS, size=(batch_size, n_distractors))
    indices = np.concatenate([target_idx[:, None], distractor_idx], axis=1)
    targets_oh = ALL_ONEHOT_NP[indices[:, 0]]
    targets_raw = ALL_OBJECTS_NP[indices[:, 0]]
    candidates_oh = ALL_ONEHOT_NP[indices.flatten()].reshape(
        batch_size, 1 + n_distractors, N_ATTRIBUTES * N_VALUES)
    return targets_oh, targets_raw, candidates_oh


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
    def __init__(self):
        input_dim = N_ATTRIBUTES * N_VALUES
        self.enc1 = nn.Linear(input_dim, HIDDEN_DIM)
        self.enc2 = nn.Linear(HIDDEN_DIM, HIDDEN_DIM)
        self.heads = [nn.Linear(HIDDEN_DIM, VOCAB_SIZE) for _ in range(MSG_LEN)]

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


class DiscriminationReceiver:
    """Standard Lewis game receiver — picks among candidates."""
    def __init__(self):
        msg_dim = MSG_LEN * VOCAB_SIZE
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


class ReconstructionReceiver:
    """Reconstruction receiver — outputs attribute values from message."""
    def __init__(self):
        msg_dim = MSG_LEN * VOCAB_SIZE
        self.enc1 = nn.Linear(msg_dim, HIDDEN_DIM)
        self.enc2 = nn.Linear(HIDDEN_DIM, HIDDEN_DIM)
        # One head per attribute, each predicting N_VALUES classes
        self.heads = [nn.Linear(HIDDEN_DIM, N_VALUES) for _ in range(N_ATTRIBUTES)]

    def __call__(self, msg_soft):
        msg_flat = msg_soft.reshape(msg_soft.shape[0], -1)
        h = self.enc2(self.enc1(msg_flat).relu()).relu()
        # Returns list of logits, one per attribute
        return [head(h) for head in self.heads]


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


def compute_positional_disentanglement(sender, temperature=0.1):
    all_msgs = get_all_messages(sender, temperature)
    entropies = []
    for pos in range(MSG_LEN):
        mis = []
        for attr in range(N_ATTRIBUTES):
            joint_counts = np.zeros((VOCAB_SIZE, N_VALUES), dtype=np.float64)
            for obj, msg in zip(ALL_OBJECTS_NP, all_msgs):
                joint_counts[msg[pos], obj[attr]] += 1
            joint = joint_counts / joint_counts.sum()
            margin_msg = joint.sum(axis=1, keepdims=True)
            margin_attr = joint.sum(axis=0, keepdims=True)
            mask = joint > 0
            mi = np.sum(joint[mask] * np.log2(joint[mask] / (margin_msg * margin_attr)[mask]))
            mis.append(mi)
        mis = np.array(mis)
        total_mi = mis.sum()
        if total_mi > 0:
            entropies.append(mis.max() / total_mi)
        else:
            entropies.append(0.0)
    return float(np.mean(entropies))


def get_temperature(step):
    if step >= TEMP_ANNEAL_STEPS:
        return TEMP_MIN
    return TEMP_INIT * (TEMP_MIN / TEMP_INIT) ** (step / TEMP_ANNEAL_STEPS)


# ─── Training Conditions ─────────────────────────────────────────────────────

def train_discrimination(name="A_discrimination"):
    """Standard Lewis game (baseline)."""
    print(f"\n{'=' * 60}")
    print(f"CONDITION {name}: Standard discrimination")
    print(f"{'=' * 60}")

    np.random.seed(SEED)
    random.seed(SEED)
    Tensor.manual_seed(SEED)

    sender = Sender()
    receiver = DiscriminationReceiver()
    params = get_parameters(sender) + get_parameters(receiver)
    opt = Adam(params, lr=LR)
    labels = Tensor.zeros(BATCH_SIZE, dtype=dtypes.int)

    @TinyJit
    def train_step(tgt_oh, cand_oh, noise, temp_t):
        msg_soft, _ = sender(tgt_oh, temp_t, noise_packed=noise)
        scores = receiver(msg_soft, cand_oh)
        loss = scores.sparse_categorical_crossentropy(labels)
        loss.backward()
        opt.step()
        opt.zero_grad()
        return loss.realize(), (scores.argmax(axis=-1) == 0).mean().realize()

    Tensor.training = True
    for step in range(TRAIN_STEPS):
        temperature = get_temperature(step)
        tgt_oh_np, _, cand_oh_np = sample_batch_np(BATCH_SIZE, N_DISTRACTORS)
        noise = Tensor.rand(MSG_LEN, BATCH_SIZE, VOCAB_SIZE)
        temp_t = Tensor([temperature])
        loss, acc = train_step(Tensor(tgt_oh_np), Tensor(cand_oh_np), noise, temp_t)

        if step % EVAL_EVERY == 0 or step == TRAIN_STEPS - 1:
            Tensor.training = False
            topsim = compute_topographic_similarity(sender)
            print(f"  Step {step:>6d} | Loss {loss.numpy():.4f} | "
                  f"Acc {acc.numpy():.4f} | TopSim {topsim:.4f}")
            Tensor.training = True

    Tensor.training = False
    final_topsim = compute_topographic_similarity(sender)
    final_disent = compute_positional_disentanglement(sender)
    print(f"\n  {name} done — TopSim {final_topsim:.4f}, Disent {final_disent:.4f}")

    return {
        "condition": name,
        "topsim": final_topsim,
        "disentanglement": final_disent,
        "accuracy": float(acc.numpy()),
    }


def train_reconstruction(name="B_reconstruction"):
    """Reconstruction game — receiver outputs attribute values."""
    print(f"\n{'=' * 60}")
    print(f"CONDITION {name}: Reconstruction")
    print(f"{'=' * 60}")

    np.random.seed(SEED)
    random.seed(SEED)
    Tensor.manual_seed(SEED)

    sender = Sender()
    receiver = ReconstructionReceiver()
    params = get_parameters(sender) + get_parameters(receiver)
    opt = Adam(params, lr=LR)

    @TinyJit
    def train_step(tgt_oh, attr_targets, noise, temp_t):
        msg_soft, _ = sender(tgt_oh, temp_t, noise_packed=noise)
        attr_logits = receiver(msg_soft)
        losses = []
        for a in range(N_ATTRIBUTES):
            losses.append(attr_logits[a].sparse_categorical_crossentropy(attr_targets[a]))
        loss = Tensor.stack(*losses).mean()
        # Accuracy: all attributes correct
        preds = Tensor.stack(*[logits.argmax(axis=-1) for logits in attr_logits], dim=1)
        acc = (preds == Tensor.stack(*[attr_targets[a] for a in range(N_ATTRIBUTES)], dim=1)).all(axis=1).mean()
        loss.backward()
        opt.step()
        opt.zero_grad()
        return loss.realize(), acc.realize()

    Tensor.training = True
    for step in range(TRAIN_STEPS):
        temperature = get_temperature(step)
        tgt_oh_np, tgt_raw_np, _ = sample_batch_np(BATCH_SIZE, N_DISTRACTORS)
        noise = Tensor.rand(MSG_LEN, BATCH_SIZE, VOCAB_SIZE)
        temp_t = Tensor([temperature])
        # Attribute targets: list of tensors, one per attribute
        attr_targets = [Tensor(tgt_raw_np[:, a].astype(np.int32)) for a in range(N_ATTRIBUTES)]
        loss, acc = train_step(Tensor(tgt_oh_np), attr_targets, noise, temp_t)

        if step % EVAL_EVERY == 0 or step == TRAIN_STEPS - 1:
            Tensor.training = False
            topsim = compute_topographic_similarity(sender)
            print(f"  Step {step:>6d} | Loss {loss.numpy():.4f} | "
                  f"Acc {acc.numpy():.4f} | TopSim {topsim:.4f}")
            Tensor.training = True

    Tensor.training = False
    final_topsim = compute_topographic_similarity(sender)
    final_disent = compute_positional_disentanglement(sender)
    print(f"\n  {name} done — TopSim {final_topsim:.4f}, Disent {final_disent:.4f}")

    return {
        "condition": name,
        "topsim": final_topsim,
        "disentanglement": final_disent,
        "accuracy": float(acc.numpy()),
    }


def train_combined(name="C_combined", recon_weight=0.5):
    """Combined: discrimination + reconstruction auxiliary loss."""
    print(f"\n{'=' * 60}")
    print(f"CONDITION {name}: Combined (recon_weight={recon_weight})")
    print(f"{'=' * 60}")

    np.random.seed(SEED)
    random.seed(SEED)
    Tensor.manual_seed(SEED)

    sender = Sender()
    disc_receiver = DiscriminationReceiver()
    recon_receiver = ReconstructionReceiver()
    params = (get_parameters(sender) + get_parameters(disc_receiver) +
              get_parameters(recon_receiver))
    opt = Adam(params, lr=LR)
    labels = Tensor.zeros(BATCH_SIZE, dtype=dtypes.int)

    @TinyJit
    def train_step(tgt_oh, cand_oh, attr_targets, noise, temp_t):
        msg_soft, _ = sender(tgt_oh, temp_t, noise_packed=noise)

        # Discrimination loss
        scores = disc_receiver(msg_soft, cand_oh)
        disc_loss = scores.sparse_categorical_crossentropy(labels)
        disc_acc = (scores.argmax(axis=-1) == 0).mean()

        # Reconstruction loss
        attr_logits = recon_receiver(msg_soft)
        recon_losses = []
        for a in range(N_ATTRIBUTES):
            recon_losses.append(attr_logits[a].sparse_categorical_crossentropy(attr_targets[a]))
        recon_loss = Tensor.stack(*recon_losses).mean()

        # Combined
        total_loss = (1 - recon_weight) * disc_loss + recon_weight * recon_loss
        total_loss.backward()
        opt.step()
        opt.zero_grad()
        return total_loss.realize(), disc_acc.realize()

    Tensor.training = True
    for step in range(TRAIN_STEPS):
        temperature = get_temperature(step)
        tgt_oh_np, tgt_raw_np, cand_oh_np = sample_batch_np(BATCH_SIZE, N_DISTRACTORS)
        noise = Tensor.rand(MSG_LEN, BATCH_SIZE, VOCAB_SIZE)
        temp_t = Tensor([temperature])
        attr_targets = [Tensor(tgt_raw_np[:, a].astype(np.int32)) for a in range(N_ATTRIBUTES)]
        loss, acc = train_step(
            Tensor(tgt_oh_np), Tensor(cand_oh_np), attr_targets, noise, temp_t)

        if step % EVAL_EVERY == 0 or step == TRAIN_STEPS - 1:
            Tensor.training = False
            topsim = compute_topographic_similarity(sender)
            print(f"  Step {step:>6d} | Loss {loss.numpy():.4f} | "
                  f"Acc {acc.numpy():.4f} | TopSim {topsim:.4f}")
            Tensor.training = True

    Tensor.training = False
    final_topsim = compute_topographic_similarity(sender)
    final_disent = compute_positional_disentanglement(sender)
    print(f"\n  {name} done — TopSim {final_topsim:.4f}, Disent {final_disent:.4f}")

    return {
        "condition": name,
        "topsim": final_topsim,
        "disentanglement": final_disent,
        "accuracy": float(acc.numpy()),
    }


# ─── Main ────────────────────────────────────────────────────────────────────

def run_experiment():
    print("=" * 60)
    print("RECONSTRUCTION GAME EXPERIMENT")
    print(f"Steps: {TRAIN_STEPS}, Batch: {BATCH_SIZE}")
    print("=" * 60)

    results = {
        "config": {
            "train_steps": TRAIN_STEPS,
            "batch_size": BATCH_SIZE,
            "seed": SEED,
            "device": Device.DEFAULT,
        },
        "conditions": [],
    }

    for train_fn in [train_discrimination, train_reconstruction, train_combined]:
        result = train_fn()
        results["conditions"].append(result)
        with open(RESULTS_FILE, "w") as f:
            json.dump(results, f, indent=2)

    # Summary
    print(f"\n{'=' * 60}")
    print("SUMMARY — Discrimination vs Reconstruction vs Combined")
    print(f"{'=' * 60}")
    print(f"\n  {'Condition':<20s}  {'TopSim':>8s}  {'Disent':>8s}  {'Acc':>6s}")
    print(f"  {'─' * 46}")

    baseline_ts = None
    for c in results["conditions"]:
        ts = c["topsim"]
        delta = f" ({ts - baseline_ts:+.4f})" if baseline_ts is not None else ""
        if baseline_ts is None:
            baseline_ts = ts
        print(f"  {c['condition']:<20s}  {ts:>8.4f}{delta:>10s}  "
              f"{c['disentanglement']:>8.4f}  {c['accuracy']:>6.4f}")

    # Verdict
    ts_values = [c["topsim"] for c in results["conditions"]]
    best_idx = int(np.argmax(ts_values))
    best = results["conditions"][best_idx]

    if best["condition"] != "A_discrimination" and best["topsim"] > baseline_ts + 0.05:
        results["verdict"] = f"RECONSTRUCTION_HELPS: {best['condition']} improved topsim by {best['topsim'] - baseline_ts:+.4f}"
    elif best["topsim"] > baseline_ts + 0.02:
        results["verdict"] = f"WEAK_EFFECT: {best['condition']} slightly improved topsim"
    else:
        results["verdict"] = "NO_EFFECT: Reconstruction did not improve compositionality"

    print(f"\n  VERDICT: {results['verdict']}")

    with open(RESULTS_FILE, "w") as f:
        json.dump(results, f, indent=2)
    print(f"\n  Results saved to: {RESULTS_FILE}")


if __name__ == "__main__":
    run_experiment()
