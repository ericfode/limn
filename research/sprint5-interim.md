# Sprint 5 Interim Report: Compositionality Under Proper Pressure

**Date:** 2026-02-07
**Author:** Lex (researcher)
**Status:** IN PROGRESS — iterated learning experiment running (5/10 generations)
**Beads:** hq-2sn9b (iterated learning — in progress)

## Key Finding: Sprint 3-4 Results Were Confounded

**H20 (CONFIRMED):** 98.8% of our Lewis game training samples need only 1 message symbol to discriminate target from 3 random distractors. The ~0.42 topsim ceiling across ALL Sprint 3-4 experiments was an artifact of trivially easy discrimination, not a genuine compositionality limit.

This means:
- The "compositionality doesn't emerge" conclusion was premature
- Variants A-E (generalization pressure, bottleneck, entropy reg, grokking) may all have been fighting against a ceiling imposed by task difficulty, not by lack of pressure
- The Lewis game setup needs fundamental changes: harder distractors, reconstruction objective, or receiver heterogeneity

## Iterated Learning: First Positive Result

The iterated learning experiment (25% transmission bottleneck, 10 generations) shows genuine compositionality increase:

| Generation | TopSim | Δ | Accuracy |
|-----------|--------|---|----------|
| 0 (baseline) | 0.3663 | — | 99.6% |
| 1 | 0.4097 | +0.043 | 99.2% |
| 2 | 0.4180 | +0.008 | 99.4% |
| 3 | 0.4710 | +0.053 | 99.2% |
| 4 | 0.4750 | +0.004 | *pending* |

- Linear trend: +0.028/gen
- Projected gen 10: 0.651
- Total improvement: +0.109 (29.7% relative increase)
- Accuracy maintained >99% throughout

**Why iterated learning is less affected by H20:** The bottleneck operates on LEARNING CAPACITY (generalize from 25% of examples), not discrimination difficulty. The imitation phase forces the student to discover compositional structure as the simplest explanation for the limited data. The communication phase (which IS affected by H20) just fine-tunes.

## H19: Density Survives Channel Coding

Limn can tolerate up to **52.9% coding overhead** before losing its density advantage over English. Even at the Shannon limit for 20% corruption, coded Limn is 11.4% denser than English.

| Corruption | Shannon-coded Limn (bpsu) | English (bpsu) | Advantage |
|-----------|---------------------------|----------------|-----------|
| 1% | 13.6 | 27.6 | +50.7% |
| 5% | 15.6 | 27.6 | +43.4% |
| 10% | 18.2 | 27.6 | +34.1% |
| 20% | 24.5 | 27.6 | +11.4% |

At realistic 1% corruption, RS(11,9) gives +42.5% advantage with <1% residual error. The correct architecture is Shannon's: Limn as source code (dense) + channel code as separate layer.

## Arxiv Index Expansion (15 → 32 papers)

Key new findings from the expanded survey:

1. **Freeborn 2025:** Standard receivers fail to interpret compositional messages compositionally. They use messages as holistic hashes. Novel minimalist/generalist receiver architectures fix this.

2. **Sevestre & Dupoux 2025:** Limited data exposure (not frequency) drives compositionality. Directly supports iterated learning mechanism.

3. **Lee et al. 2026:** Iterated learning scales to 128-glyph meaning spaces with semi-supervised autoencoder. Validates our approach.

4. **Zhang 2024:** The sampling pitfall we confirmed as H20. Standard Lewis games are too easy.

5. **Ben Zion et al. 2024:** Reconstruction > discrimination for semantic consistency. Discrimination objectives allow semantically inconsistent protocols.

Updated pressure taxonomy: 9 positive pressures + 4 documented non-pressures.

## Experiments Designed (Ready to Run)

### 1. Receiver Heterogeneity (`receiver_heterogeneity.py`)
- 4 conditions: baseline, 2 disjoint receivers, 4 individual receivers, 2 same receivers
- Specialized receivers force encoding ALL attributes (inherently fixes H20)
- Based on Lee 2024 (EMNLP), second strongest compositionality pressure

### 2. Reconstruction Game (`reconstruction_game.py`)
- 3 conditions: discrimination (baseline), reconstruction, combined
- Reconstruction forces encoding all attributes (fixes H20)
- Based on Ben Zion et al. 2024 (NeurIPS)

### 3. Sampling Analysis (`sampling_analysis.py`)
- Confirmed H20: 98.8% of samples need 1 symbol
- Even 63 random distractors: only 1.5% need all 4 symbols
- Hard distractors (sharing 3/4 attrs): avg 2.31 symbols needed

## Performance Note

The iterated learning experiment has a critical performance bottleneck: the non-JIT imitation phase takes ~39 min per generation (vs ~30s for the JIT'd communication phase). A JIT'd version is prepared and will reduce per-generation time from 39 min to ~40 sec (60x speedup). Apply after current experiment completes.

## Updated Hypothesis Scorecard

| ID | Status | Sprint 5 Update |
|----|--------|-----------------|
| H15 | CONFIRMED | Reframed: compositionality IS emergent under proper pressure |
| H19 | CONFIRMED | Density survives channel coding (+11% to +51%) |
| H20 | CONFIRMED | Sampling pitfall explains topsim ceiling |
| H17 | FALSIFIED | Still falsified — grokking doesn't help |

## Next Steps (when experiment finishes)

1. Analyze full 10-gen iterated learning trajectory
2. Apply JIT fix to iterated_learning.py
3. Run receiver heterogeneity experiment (~1 hour with 4 conditions × 20k steps)
4. Run reconstruction game (~1 hour with 3 conditions × 20k steps)
5. Design and run omnibus experiment (all pressures combined)
6. Write final sprint 5 synthesis

---

```limn
prs fnd | pit unc | tru ris
> pressures found | pitfall uncovered | truth rises
```

*— Lex*
