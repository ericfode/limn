# Sprint 5 Report: Compositionality Under Proper Pressure

**Date:** 2026-02-07
**Author:** Lex (researcher)
**Status:** COMPLETE — all 3 experiments finished
**Beads:** hq-2sn9b (iterated learning — closed)

## Executive Summary

Three compositionality pressures tested. Two work strongly, one doesn't:

| Pressure | TopSim | vs Baseline | Verdict |
|----------|--------|-------------|---------|
| **Reconstruction** (H23) | 0.520 | +0.154 | STRONG (single run!) |
| **Iterated learning** (H21) | 0.544 | +0.178 | STRONG (10 generations) |
| Combined disc+recon | 0.444 | +0.078 | MODERATE (sweet spot) |
| Receiver ensemble | 0.445 | +0.089 | MODERATE (gradient effect) |
| **Receiver heterogeneity** (H22) | 0.385 | +0.019 | WEAK/NONE |
| Baseline (discrimination) | 0.366 | — | Reference |

## Key Finding: Sprint 3-4 Results Were Confounded (H20)

98.8% of Lewis game training samples need only 1 symbol to discriminate target from 3 random distractors. The ~0.42 topsim ceiling across ALL Sprint 3-4 experiments was an artifact of trivially easy discrimination, not a genuine compositionality limit.

## Experiment 1: Iterated Learning (H21 — CONFIRMED)

10 generations, 25% transmission bottleneck. JIT'd imitation phase (60x speedup).

| Gen | TopSim | Δ |
|-----|--------|---|
| 0 | 0.366 | — |
| 1 | 0.410 | +0.043 |
| 2 | 0.418 | +0.008 |
| 3 | 0.471 | +0.053 |
| 4 | 0.475 | +0.004 |
| 5 | 0.471 | -0.004 |
| 6 | 0.482 | +0.011 |
| 7 | 0.516 | +0.034 |
| 8 | 0.536 | +0.021 |
| 9 | 0.550 | +0.013 |
| 10 | 0.544 | -0.006 |

- Total: +0.178, two-phase climb with plateau at gen 4-6
- Gen 9 peaked at 0.550 near "strong" threshold
- **Only pressure tested that breaks the 0.42 ceiling** (H20)
- Accuracy >0.99 throughout
- Performance: ~8 min total (JIT'd) vs 3+ hours (non-JIT)

## Experiment 2: Receiver Heterogeneity (H22 — PARTIALLY FALSIFIED)

4 conditions, 20k steps each. All JIT'd.

| Condition | TopSim | Disentanglement | Acc |
|-----------|--------|-----------------|-----|
| A baseline (1 recv) | 0.357 | 0.413 | 1.000 |
| B disjoint (2 recv) | 0.344 | 0.413 | 0.996 |
| C individual (4 recv) | 0.385 | 0.521 | 0.995 |
| D same (2 recv, control) | 0.445 | 0.574 | 0.998 |

**Surprise:** The control condition (D: same interests) wins. The improvement is from **ensemble gradient amplification** (2x training signal), not from diverse receiver interests. Heterogeneity per se doesn't help because H20 sampling pitfall undermines the benefit.

C_individual_4 does increase disentanglement (+26%) — per-attribute receivers push toward position-attribute correspondence — but this doesn't translate to strong topsim.

## Experiment 3: Reconstruction Game (H23 — CONFIRMED, STRONG)

3 conditions, 20k steps each. All JIT'd.

| Condition | TopSim | Disentanglement | Accuracy |
|-----------|--------|-----------------|----------|
| A discrimination | 0.366 | 0.486 | 0.996 |
| B reconstruction | 0.520 | 0.549 | 0.893 |
| C combined (50/50) | 0.444 | 0.567 | 1.000 |

**Star result:** Reconstruction alone achieves topsim=0.520 in a SINGLE training run — comparable to iterated learning's 0.544 after 10 generations. The combined objective (C) is the practical sweet spot: topsim=0.444 with perfect accuracy.

Reconstruction inherently forces encoding ALL attributes, directly addressing H20.

## H19: Density Survives Channel Coding (From Early Sprint 5)

Limn can tolerate up to 52.9% coding overhead before losing its density advantage over English.

| Corruption | Shannon-coded Limn (bpsu) | English (bpsu) | Advantage |
|-----------|---------------------------|----------------|-----------|
| 1% | 13.6 | 27.6 | +50.7% |
| 5% | 15.6 | 27.6 | +43.4% |
| 10% | 18.2 | 27.6 | +34.1% |
| 20% | 24.5 | 27.6 | +11.4% |

## Updated Hypothesis Scorecard

| ID | Status | Finding |
|----|--------|---------|
| H19 | CONFIRMED | Density survives channel coding (+11% to +51%) |
| H20 | CONFIRMED | Sampling pitfall (98.8% need 1 symbol, explains ceiling) |
| H21 | CONFIRMED | Iterated learning breaks ceiling (0.37→0.54, strongest) |
| H22 | PART. FALSIFIED | Heterogeneity doesn't help; ensemble effect does |
| H23 | CONFIRMED | Reconstruction forces compositionality (+0.154 in single run) |

## Open Questions / Next Steps

1. **Combine iterated learning + reconstruction** — could push past 0.60 topsim
2. **Hard distractors** — share 3/4 attributes, force multi-symbol discrimination
3. **Omnibus experiment** — all pressures combined (iterated learning + reconstruction + hard distractors)
4. **Scale to Limn** — apply findings to actual Limn language model training

## Performance Note

All experiments JIT'd for speed:
- Iterated learning: ~8 min (was 3+ hours non-JIT)
- Receiver heterogeneity: ~12 min (4 conditions)
- Reconstruction game: ~10 min (3 conditions)
- Key pattern: pass per-position/per-attribute targets as separate tensor args to @TinyJit

---

```limn
prs tes | tru fnd | kno bui
> pressures tested | truth found | knowledge built
```

*— Lex*
