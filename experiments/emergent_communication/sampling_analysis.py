#!/usr/bin/env python3
"""
Sampling Complexity Analysis — Are Our Lewis Games Too Easy?

Zhang (2024, COLING 2025) identified a *sampling pitfall*: standard Lewis
game datasets often require only 1-2 message symbols for successful
discrimination. If our 4-attr × 8-val setup with random distractors is
solvable with fewer than 4 symbols, the model has no pressure to develop
full compositionality.

This script analyzes the minimum symbolic complexity of our experimental
setup to determine whether:
1. Random distractors are hard enough to require all 4 message positions
2. Our N_DISTRACTORS=3 is sufficient to pressure full encoding
3. The sampling pitfall explains our ~0.42 topsim ceiling

— Lex
"""

import numpy as np
from itertools import product
from collections import Counter
import math

N_ATTRIBUTES = 4
N_VALUES = 8
N_DISTRACTORS = 3
N_OBJECTS = N_VALUES ** N_ATTRIBUTES  # 4096
MSG_LEN = 4
VOCAB_SIZE = 32
N_SIMULATIONS = 100_000

ALL_OBJECTS = np.array(list(product(range(N_VALUES), repeat=N_ATTRIBUTES)), dtype=np.int32)

print("=" * 60)
print("SAMPLING COMPLEXITY ANALYSIS")
print(f"Objects: {N_OBJECTS} ({N_ATTRIBUTES} attrs × {N_VALUES} vals)")
print(f"Candidates: 1 target + {N_DISTRACTORS} distractors")
print(f"Message: {MSG_LEN} positions × {VOCAB_SIZE} tokens")
print("=" * 60)


# ─── Analysis 1: How many attributes differ between random pairs? ────────────

print(f"\n{'─' * 60}")
print("Analysis 1: Attribute overlap between random object pairs")
print(f"{'─' * 60}")

diffs = []
for _ in range(N_SIMULATIONS):
    i, j = np.random.randint(0, N_OBJECTS, size=2)
    diffs.append((ALL_OBJECTS[i] != ALL_OBJECTS[j]).sum())

diff_counts = Counter(diffs)
total = sum(diff_counts.values())
print(f"\n  Attributes differing between random pair:")
for d in sorted(diff_counts.keys()):
    pct = diff_counts[d] / total * 100
    print(f"    {d} attrs differ: {pct:.1f}%")

# P(any attribute matches) = 1 - (7/8)^4 per pair
p_all_diff = (1 - 1/N_VALUES) ** N_ATTRIBUTES
print(f"\n  P(all {N_ATTRIBUTES} attrs differ): {p_all_diff:.4f} = {p_all_diff*100:.1f}%")
print(f"  P(at least 1 attr shared): {1 - p_all_diff:.4f} = {(1-p_all_diff)*100:.1f}%")


# ─── Analysis 2: How many symbols needed to discriminate? ─────────────────────

print(f"\n{'─' * 60}")
print("Analysis 2: Minimum symbols needed to discriminate target from distractors")
print(f"{'─' * 60}")

def min_symbols_needed(target, distractors):
    """Minimum number of attribute positions needed to uniquely identify target."""
    n_dist = len(distractors)
    # Try all subsets of attributes, smallest first
    for n_attrs in range(1, N_ATTRIBUTES + 1):
        from itertools import combinations
        for attr_set in combinations(range(N_ATTRIBUTES), n_attrs):
            # Check if these attributes distinguish target from ALL distractors
            target_vals = tuple(target[a] for a in attr_set)
            all_distinct = True
            for d in distractors:
                dist_vals = tuple(d[a] for a in attr_set)
                if target_vals == dist_vals:
                    all_distinct = False
                    break
            if all_distinct:
                return n_attrs
    return N_ATTRIBUTES  # Should always be reachable

min_syms = []
for _ in range(min(N_SIMULATIONS, 50000)):  # Combinatorial check is expensive
    target_idx = np.random.randint(0, N_OBJECTS)
    dist_idx = np.random.randint(0, N_OBJECTS, size=N_DISTRACTORS)
    target = ALL_OBJECTS[target_idx]
    distractors = ALL_OBJECTS[dist_idx]
    min_syms.append(min_symbols_needed(target, distractors))

sym_counts = Counter(min_syms)
total = sum(sym_counts.values())
print(f"\n  Minimum symbols needed (random distractors):")
for s in sorted(sym_counts.keys()):
    pct = sym_counts[s] / total * 100
    print(f"    {s} symbol(s): {pct:.1f}%")

avg_min = np.mean(min_syms)
print(f"  Average minimum: {avg_min:.2f} symbols")
print(f"  Uses all {MSG_LEN} positions: {sym_counts.get(MSG_LEN, 0)/total*100:.2f}%")


# ─── Analysis 3: What about harder distractors? ──────────────────────────────

print(f"\n{'─' * 60}")
print("Analysis 3: Minimum symbols with HARD distractors (share some attrs)")
print(f"{'─' * 60}")

def sample_hard_distractors(target, n_distractors, n_shared):
    """Sample distractors that share n_shared attributes with target."""
    distractors = []
    for _ in range(n_distractors):
        dist = target.copy()
        # Pick which attributes to randomize
        randomize = np.random.choice(N_ATTRIBUTES, size=N_ATTRIBUTES - n_shared, replace=False)
        for attr in randomize:
            dist[attr] = np.random.randint(0, N_VALUES)
            # Ensure at least one randomized attr differs
        # If identical, re-randomize one attr
        if np.array_equal(dist, target):
            attr = randomize[0]
            dist[attr] = (target[attr] + 1 + np.random.randint(0, N_VALUES - 1)) % N_VALUES
        distractors.append(dist)
    return distractors

for n_shared in [0, 1, 2, 3]:
    min_syms_hard = []
    for _ in range(min(N_SIMULATIONS, 30000)):
        target_idx = np.random.randint(0, N_OBJECTS)
        target = ALL_OBJECTS[target_idx]
        distractors = sample_hard_distractors(target, N_DISTRACTORS, n_shared)
        min_syms_hard.append(min_symbols_needed(target, distractors))

    sym_counts_h = Counter(min_syms_hard)
    total_h = sum(sym_counts_h.values())
    avg_h = np.mean(min_syms_hard)
    needs_all = sym_counts_h.get(MSG_LEN, 0) / total_h * 100

    print(f"\n  Sharing {n_shared}/{N_ATTRIBUTES} attributes:")
    for s in sorted(sym_counts_h.keys()):
        pct = sym_counts_h[s] / total_h * 100
        print(f"    {s} symbol(s): {pct:.1f}%")
    print(f"    Average minimum: {avg_h:.2f}, Needs all {MSG_LEN}: {needs_all:.1f}%")


# ─── Analysis 4: Effect of number of distractors ─────────────────────────────

print(f"\n{'─' * 60}")
print("Analysis 4: Effect of distractor count on minimum symbols")
print(f"{'─' * 60}")

for n_dist in [1, 3, 7, 15, 31, 63]:
    min_syms_nd = []
    for _ in range(min(N_SIMULATIONS, 20000)):
        target_idx = np.random.randint(0, N_OBJECTS)
        dist_idx = np.random.randint(0, N_OBJECTS, size=n_dist)
        target = ALL_OBJECTS[target_idx]
        distractors = ALL_OBJECTS[dist_idx]
        min_syms_nd.append(min_symbols_needed(target, distractors))

    avg_nd = np.mean(min_syms_nd)
    needs_all = Counter(min_syms_nd).get(MSG_LEN, 0) / len(min_syms_nd) * 100
    print(f"  {n_dist:>3d} distractors: avg min={avg_nd:.2f}, needs all {MSG_LEN}: {needs_all:.1f}%")


# ─── Verdict ──────────────────────────────────────────────────────────────────

print(f"\n{'=' * 60}")
print("VERDICT")
print(f"{'=' * 60}")

pct_needs_all = sym_counts.get(MSG_LEN, 0) / sum(sym_counts.values()) * 100
if pct_needs_all < 1:
    print(f"\n  PITFALL CONFIRMED: Only {pct_needs_all:.1f}% of samples need all {MSG_LEN} symbols.")
    print(f"  The model can achieve >99% accuracy using ~{int(avg_min)} message positions.")
    print(f"  This explains our ~0.42 topsim ceiling: no pressure for full compositionality.")
    print(f"  Fix: Use harder distractors (share 2-3 attrs) or more distractors.")
elif pct_needs_all < 30:
    print(f"\n  PARTIAL PITFALL: {pct_needs_all:.1f}% need all {MSG_LEN} symbols.")
    print(f"  Moderate pressure for full compositionality.")
else:
    print(f"\n  NO PITFALL: {pct_needs_all:.1f}% need all {MSG_LEN} symbols.")
    print(f"  Strong pressure for full compositionality.")


if __name__ == "__main__":
    pass  # Already executed at module level
