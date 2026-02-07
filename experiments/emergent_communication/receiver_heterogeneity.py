#!/usr/bin/env python3
"""
Receiver Heterogeneity Experiment — Diverse Listeners as Compositionality Pressure

Tests whether broadcasting to multiple receivers with DIFFERENT interests forces
compositional encoding. Lee (2024) found this is one of the strongest compositionality
pressures: when different listeners need different attributes of the same object,
holistic codes fail because they can't serve all listeners simultaneously.

Protocol:
  Condition A (baseline): 1 receiver, standard Lewis game
  Condition B: 2 specialized receivers — attrs [0,1] and [2,3]
  Condition C: 4 specialized receivers — each cares about 1 attribute
  Condition D: 2 receivers, SAME interests (control — should NOT help)

For specialized receivers, distractors only differ in the receiver's relevant
attributes. This means:
  - Receiver caring about attrs [0,1]: candidates share attrs [2,3], differ in [0,1]
  - The sender must encode ALL attributes because different receivers need them

Reference: Lee 2024 — "One-to-Many Communication and Compositionality" (EMNLP 2024)

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
RESULTS_FILE = OUTPUT_DIR / "receiver_heterogeneity_results.json"


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


def sample_standard_batch(batch_size, n_distractors):
    """Standard Lewis game sampling — distractors are random objects."""
    target_idx = np.random.randint(0, N_OBJECTS, size=batch_size)
    distractor_idx = np.random.randint(0, N_OBJECTS, size=(batch_size, n_distractors))
    indices = np.concatenate([target_idx[:, None], distractor_idx], axis=1)
    targets_oh = ALL_ONEHOT_NP[indices[:, 0]]
    candidates_oh = ALL_ONEHOT_NP[indices.flatten()].reshape(
        batch_size, 1 + n_distractors, N_ATTRIBUTES * N_VALUES)
    return targets_oh, candidates_oh


def sample_specialized_batch(batch_size, n_distractors, relevant_attrs):
    """Sample batch where distractors only differ in relevant attributes.

    For a receiver caring about attrs [0,1], distractors share the same
    values for attrs [2,3] but have different values for attrs [0,1].
    This forces the receiver to use the relevant attributes for discrimination.
    """
    target_idx = np.random.randint(0, N_OBJECTS, size=batch_size)
    target_objs = ALL_OBJECTS_NP[target_idx]
    targets_oh = ALL_ONEHOT_NP[target_idx]

    irrelevant_attrs = [a for a in range(N_ATTRIBUTES) if a not in relevant_attrs]

    # Build distractors that share irrelevant attributes with target
    candidates_list = [targets_oh]
    for _ in range(n_distractors):
        dist_objs = target_objs.copy()
        for attr in relevant_attrs:
            # Randomize relevant attributes (ensuring at least one differs)
            dist_objs[:, attr] = np.random.randint(0, N_VALUES, size=batch_size)
        # If distractor is identical to target, re-randomize one relevant attr
        identical = np.all(dist_objs == target_objs, axis=1)
        if np.any(identical):
            fix_attr = relevant_attrs[0]
            dist_objs[identical, fix_attr] = (target_objs[identical, fix_attr] + 1 +
                np.random.randint(0, N_VALUES - 1, size=identical.sum())) % N_VALUES
        candidates_list.append(objects_to_onehot_np(dist_objs))

    candidates_oh = np.stack(candidates_list, axis=1)
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


class Receiver:
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
    """Measure whether each message position corresponds to one attribute."""
    all_msgs = get_all_messages(sender, temperature)
    entropies = []
    for pos in range(MSG_LEN):
        mis = []
        for attr in range(N_ATTRIBUTES):
            # Mutual information between message position and attribute
            joint_counts = np.zeros((VOCAB_SIZE, N_VALUES), dtype=np.float64)
            for obj, msg in zip(ALL_OBJECTS_NP, all_msgs):
                joint_counts[msg[pos], obj[attr]] += 1
            joint = joint_counts / joint_counts.sum()
            margin_msg = joint.sum(axis=1, keepdims=True)
            margin_attr = joint.sum(axis=0, keepdims=True)
            # MI = sum p(x,y) log(p(x,y) / (p(x)p(y)))
            mask = joint > 0
            mi = np.sum(joint[mask] * np.log2(joint[mask] / (margin_msg * margin_attr)[mask]))
            mis.append(mi)
        # Disentanglement: how much does the best attribute dominate?
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

RECEIVER_CONFIGS = {
    "A_baseline": {
        "description": "Standard Lewis game — 1 receiver, random distractors",
        "receivers": [{"relevant_attrs": [0, 1, 2, 3]}],  # all attributes
    },
    "B_disjoint_2": {
        "description": "2 receivers with disjoint interests — attrs [0,1] and [2,3]",
        "receivers": [
            {"relevant_attrs": [0, 1]},
            {"relevant_attrs": [2, 3]},
        ],
    },
    "C_individual_4": {
        "description": "4 receivers — each cares about 1 attribute",
        "receivers": [
            {"relevant_attrs": [0]},
            {"relevant_attrs": [1]},
            {"relevant_attrs": [2]},
            {"relevant_attrs": [3]},
        ],
    },
    "D_same_2": {
        "description": "2 receivers with SAME interests (control) — both [0,1,2,3]",
        "receivers": [
            {"relevant_attrs": [0, 1, 2, 3]},
            {"relevant_attrs": [0, 1, 2, 3]},
        ],
    },
}


def train_condition(condition_name, config):
    """Train one experimental condition."""
    print(f"\n{'=' * 60}")
    print(f"CONDITION {condition_name}: {config['description']}")
    print(f"{'=' * 60}")

    np.random.seed(SEED)
    random.seed(SEED)
    Tensor.manual_seed(SEED)

    sender = Sender()
    receivers = [Receiver() for _ in config["receivers"]]
    all_params = get_parameters(sender)
    for r in receivers:
        all_params += get_parameters(r)
    opt = Adam(all_params, lr=LR)
    labels = Tensor.zeros(BATCH_SIZE, dtype=dtypes.int)

    n_receivers = len(config["receivers"])
    is_standard = all(
        sorted(r["relevant_attrs"]) == list(range(N_ATTRIBUTES))
        for r in config["receivers"]
    )

    # Can only JIT if all receivers use same batch structure (standard distractors)
    # Specialized distractors have different batch contents per receiver
    if is_standard:
        @TinyJit
        def train_step(tgt_oh, cand_oh, noise_packed, temp_t):
            msg_soft, _ = sender(tgt_oh, temp_t, noise_packed=noise_packed)
            total_loss = Tensor(0.0)
            total_acc = Tensor(0.0)
            for recv in receivers:
                scores = recv(msg_soft, cand_oh)
                total_loss = total_loss + scores.sparse_categorical_crossentropy(labels)
                total_acc = total_acc + (scores.argmax(axis=-1) == 0).mean()
            avg_loss = total_loss / n_receivers
            avg_acc = total_acc / n_receivers
            avg_loss.backward()
            opt.step()
            opt.zero_grad()
            return avg_loss.realize(), avg_acc.realize()

        Tensor.training = True
        for step in range(TRAIN_STEPS):
            temperature = get_temperature(step)
            tgt_oh_np, cand_oh_np = sample_standard_batch(BATCH_SIZE, N_DISTRACTORS)
            noise = Tensor.rand(MSG_LEN, BATCH_SIZE, VOCAB_SIZE)
            temp_t = Tensor([temperature])
            loss, acc = train_step(Tensor(tgt_oh_np), Tensor(cand_oh_np), noise, temp_t)

            if step % EVAL_EVERY == 0 or step == TRAIN_STEPS - 1:
                Tensor.training = False
                topsim = compute_topographic_similarity(sender)
                print(f"  Step {step:>6d} | Loss {loss.numpy():.4f} | "
                      f"Acc {acc.numpy():.4f} | TopSim {topsim:.4f}")
                Tensor.training = True
    else:
        # Non-JIT path for specialized receivers (different batches per receiver)
        Tensor.training = True
        for step in range(TRAIN_STEPS):
            temperature = get_temperature(step)
            # All receivers see the SAME sender message for the SAME target
            tgt_oh_np, _ = sample_standard_batch(BATCH_SIZE, N_DISTRACTORS)
            tgt_oh = Tensor(tgt_oh_np)
            noise = Tensor.rand(MSG_LEN, BATCH_SIZE, VOCAB_SIZE)
            temp_t = Tensor([temperature])
            msg_soft, _ = sender(tgt_oh, temp_t, noise_packed=noise)

            losses = []
            accs = []
            for recv_cfg, recv in zip(config["receivers"], receivers):
                rel_attrs = recv_cfg["relevant_attrs"]
                if sorted(rel_attrs) == list(range(N_ATTRIBUTES)):
                    # Standard receiver — random distractors
                    _, cand_oh_np = sample_standard_batch(BATCH_SIZE, N_DISTRACTORS)
                else:
                    # Specialized receiver — distractors only differ in relevant attrs
                    _, cand_oh_np = sample_specialized_batch(
                        BATCH_SIZE, N_DISTRACTORS, rel_attrs)
                    # Target is candidate 0 — replace it with the actual target
                    cand_oh_np[:, 0] = tgt_oh_np

                scores = recv(msg_soft, Tensor(cand_oh_np))
                losses.append(scores.sparse_categorical_crossentropy(labels))
                accs.append((scores.argmax(axis=-1) == 0).mean())

            avg_loss = Tensor.stack(*losses).mean()
            avg_acc = Tensor.stack(*accs).mean()
            avg_loss.backward()
            opt.step()
            opt.zero_grad()

            if step % EVAL_EVERY == 0 or step == TRAIN_STEPS - 1:
                Tensor.training = False
                topsim = compute_topographic_similarity(sender)
                per_recv_acc = [float(a.numpy()) for a in accs]
                print(f"  Step {step:>6d} | Loss {avg_loss.numpy():.4f} | "
                      f"Acc {avg_acc.numpy():.4f} | TopSim {topsim:.4f} | "
                      f"Per-recv: {per_recv_acc}")
                Tensor.training = True

    # Final evaluation
    Tensor.training = False
    final_topsim = compute_topographic_similarity(sender)
    final_disent = compute_positional_disentanglement(sender)

    # Per-receiver accuracy
    per_recv_acc = []
    tgt_oh_np, _ = sample_standard_batch(BATCH_SIZE, N_DISTRACTORS)
    msg_soft, _ = sender(Tensor(tgt_oh_np), 0.1)
    for recv_cfg, recv in zip(config["receivers"], receivers):
        if sorted(recv_cfg["relevant_attrs"]) == list(range(N_ATTRIBUTES)):
            _, cand_oh_np = sample_standard_batch(BATCH_SIZE, N_DISTRACTORS)
        else:
            _, cand_oh_np = sample_specialized_batch(
                BATCH_SIZE, N_DISTRACTORS, recv_cfg["relevant_attrs"])
            cand_oh_np[:, 0] = tgt_oh_np
        scores = recv(msg_soft, Tensor(cand_oh_np))
        per_recv_acc.append(float((scores.argmax(axis=-1) == 0).mean().numpy()))

    final_acc = float(np.mean(per_recv_acc))

    print(f"\n  {condition_name} done — TopSim {final_topsim:.4f}, "
          f"Disent {final_disent:.4f}, Acc {final_acc:.4f}")
    print(f"  Per-receiver accuracy: {per_recv_acc}")

    return {
        "condition": condition_name,
        "description": config["description"],
        "n_receivers": n_receivers,
        "topsim": final_topsim,
        "disentanglement": final_disent,
        "accuracy": final_acc,
        "per_receiver_accuracy": per_recv_acc,
    }


# ─── Main ────────────────────────────────────────────────────────────────────

def run_experiment():
    print("=" * 60)
    print("RECEIVER HETEROGENEITY EXPERIMENT")
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

    for name, config in RECEIVER_CONFIGS.items():
        result = train_condition(name, config)
        results["conditions"].append(result)

        # Save intermediate results
        with open(RESULTS_FILE, "w") as f:
            json.dump(results, f, indent=2)

    # Summary
    print(f"\n{'=' * 60}")
    print("SUMMARY — Receiver Heterogeneity vs Compositionality")
    print(f"{'=' * 60}")
    print(f"\n  {'Condition':<20s}  {'N_recv':>6s}  {'TopSim':>8s}  {'Disent':>8s}  {'Acc':>6s}")
    print(f"  {'─' * 52}")

    baseline_ts = None
    for c in results["conditions"]:
        ts = c["topsim"]
        delta = f" ({ts - baseline_ts:+.4f})" if baseline_ts is not None else ""
        if baseline_ts is None:
            baseline_ts = ts
        print(f"  {c['condition']:<20s}  {c['n_receivers']:>6d}  {ts:>8.4f}{delta:>10s}  "
              f"{c['disentanglement']:>8.4f}  {c['accuracy']:>6.4f}")

    # Verdict
    ts_values = [c["topsim"] for c in results["conditions"]]
    best_idx = int(np.argmax(ts_values))
    best = results["conditions"][best_idx]

    if best["condition"] != "A_baseline" and best["topsim"] > baseline_ts + 0.05:
        results["verdict"] = f"HETEROGENEITY_HELPS: {best['condition']} improved topsim by {best['topsim'] - baseline_ts:+.4f}"
        print(f"\n  HETEROGENEITY HELPS: {best['condition']} wins ({best['topsim'] - baseline_ts:+.4f})")
    elif best["topsim"] > baseline_ts + 0.02:
        results["verdict"] = f"WEAK_EFFECT: {best['condition']} slightly improved topsim by {best['topsim'] - baseline_ts:+.4f}"
        print(f"\n  WEAK EFFECT: {best['condition']} slightly helps ({best['topsim'] - baseline_ts:+.4f})")
    else:
        results["verdict"] = "NO_EFFECT: Receiver heterogeneity did not improve compositionality"
        print(f"\n  NO EFFECT: Heterogeneity didn't help compositionality")

    with open(RESULTS_FILE, "w") as f:
        json.dump(results, f, indent=2)
    print(f"\n  Results saved to: {RESULTS_FILE}")


if __name__ == "__main__":
    run_experiment()
