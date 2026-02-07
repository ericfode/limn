# Emergent Communication Experiment Report

**Bead:** hq-jbm2m (baseline), hq-2uoa8 (variants)
**Date:** 2026-02-07
**Author:** Lex (researcher)

## Summary

Trained Lewis signaling game agents (sender/receiver) using tinygrad on CUDA to develop communication protocols for compositional objects (4 attributes x 8 values = 4096 objects). Tested whether compositionality emerges naturally and which pressures amplify it.

**Key finding:** Compositionality is a weak attractor under standard training (topsim=0.42), but none of the tested pressures (generalization, bottleneck, entropy regularization) significantly increased it. Generalization pressure and entropy regularization actually *decreased* compositionality. The grokking variant (E) is the critical remaining test.

## Experimental Setup

| Parameter | Value |
|-----------|-------|
| Framework | tinygrad 0.12.0 + TinyJit |
| Device | CUDA (WSL2, driver 595.02) |
| Object space | 4 attrs x 8 values = 4,096 objects |
| Message format | 4 tokens x vocab_size vocabulary |
| Architecture | 2-layer MLP sender/receiver (256 hidden) |
| Training | Gumbel-Softmax straight-through, cross-entropy |
| Batch size | 512 |
| Optimizer | Adam (lr=1e-3), AdamW for variant E |
| Baseline steps | 20,000 |

## Results

### Baseline (20k steps, vocab=32)

| Metric | Value | Interpretation |
|--------|-------|----------------|
| Accuracy | 99.4% | Near-perfect communication (chance = 25%) |
| TopSim | 0.418 | Mildly compositional (threshold: 0.4) |
| Disentanglement | 0.30 | Moderate — positions partially specialize |
| Message entropy | 9.12 bits | 76% of needed 12 bits |
| Utilization | 45.6% | Uses less than half of 20-bit capacity |

**MI matrix (rows=positions, cols=attributes):**
```
Position 0: [0.39  1.97  0.00  0.65]  → shape (attr 1)
Position 1: [1.03  0.64  0.00  1.26]  → color + texture
Position 2: [1.90  0.41  0.00  0.77]  → color (attr 0)
Position 3: [0.50  1.05  0.00  1.45]  → texture (attr 3)
```

**Note:** Attribute 2 (size) has near-zero MI across all positions. The protocol ignores it entirely — 3 distractors don't require all 4 attributes for disambiguation.

### Variant Comparison

| Variant | TopSim | Acc/TestAcc | Disent | Eff% | Delta vs Baseline |
|---------|--------|-------------|--------|------|-------------------|
| **Baseline** | **0.418** | 99.4% | 0.30 | 76.0% | — |
| A: Generalization | 0.339 | 99.4% (test) | — | — | **-0.08** (worse) |
| B: Bottleneck (v=8) | 0.408 | 99.8% | — | — | -0.01 (neutral) |
| C: Combined (A+B) | 0.355 | 99.8% (test) | — | — | **-0.06** (worse) |
| D: Entropy Reg | 0.236 | 99.6% | — | — | **-0.18** (much worse) |
| E: Grokking | *pending* | *pending* | — | — | *pending* |

### Detailed Findings

**Variant A (Generalization — 80/20 split):** TopSim = 0.339

Agents trained on 80% of objects and tested on the held-out 20%. Test accuracy was 99.4% — agents generalize perfectly with a holistic code. This directly confirms Chaabouni et al. (2020): generalization pressure alone does not induce compositionality. Holistic codes can generalize when the test objects share attributes with training objects (which they necessarily do in a combinatorial space).

**Variant B (Bottleneck — vocab 8):** TopSim = 0.408

Reducing vocabulary from 32 to 8 (matching the number of attribute values) had no measurable effect on compositionality. With vocab=8 and 4 positions, the theoretical capacity is 4 * log2(8) = 12 bits = exactly the needed bits. The bottleneck is theoretically tight but agents can still develop holistic codes within this capacity.

**Variant C (Combined):** TopSim = 0.355

The combination of generalization pressure and vocabulary bottleneck yields compositionality between A and baseline. The interaction effect is small. Neither pressure alone or combined is sufficient to push topsim above 0.42.

**Variant D (Entropy Regularization):** TopSim = 0.236

*This is the most informative negative result.* Adding a message entropy bonus (encouraging diverse token usage) dramatically decreased compositionality. Why? Because entropy regularization encourages *uniform* token distributions, which prevents the sender from developing consistent token-attribute mappings. Compositionality requires tokens to have *fixed*, *non-uniform* meanings. Entropy regularization fights this.

**Variant E (Grokking):** PENDING

200k steps with AdamW (weight_decay=0.01). Prediction: holistic protocol for first ~20k steps (matching baseline topsim≈0.42), then weight decay destroys high-norm memorization circuits, and compositionality emerges as a phase transition (topsim jumps to >0.55). Based on Power et al. 2022 and Nanda et al. 2023 grokking dynamics.

## Interpretation

### What the results mean for Limn

1. **Compositionality is NOT the default equilibrium** for communication. Even with compositional input structure, agents develop only mildly compositional protocols. This matches the literature (Kottur 2017, Chaabouni 2019).

2. **Generalization doesn't require compositionality.** Agents achieve 99%+ test accuracy with holistic codes. Limn's compositionality needs justification on grounds other than generalization — e.g., learnability, interpretability, or aesthetic design.

3. **The bottleneck matters less than expected.** Vocabulary restriction to exactly match the attribute space didn't force perfect compositionality. This challenges the common intuition that "if vocab equals attribute values, compositionality is forced."

4. **Entropy regularization is counterproductive.** Forcing diverse token usage prevents consistent compositional structure. Implication: Limn's design should allow token specialization, not enforce uniform usage.

5. **The grokking hypothesis (H17) is the remaining hope for "compositionality as inevitable."** If extended training with weight decay produces a phase transition to compositionality, it validates Limn's design as a shortcut to the deep minimum. If it doesn't, compositionality is purely a human design choice.

### Hypotheses Updated

| ID | Status | Evidence |
|----|--------|----------|
| H15 | STRENGTHENED | Compositionality is conditional — baseline shows only mild compositionality, variants A/D show it can decrease |
| H17 | PENDING | Variant E will test grokking prediction |

## Technical Notes

- **TinyJit speedup:** 137x over non-JIT (0.006s vs 0.84s per step). Key: pass Gumbel noise and temperature as external tensor args.
- **WSL2 CUDA:** `CUDA_PATH=/usr/lib/wsl/lib/libcuda.so.1` before tinygrad import.
- **Eval bottleneck:** TopSim computation requires full forward pass over 4096 objects + 50k pairwise distance computations. Each eval takes ~30s.
- **Seed consistency:** All variants use seed=42 for reproducibility.

## Next Steps

1. Await Variant E results (grokking)
2. If grokking produces compositionality → design iterated learning experiment to test transmission pressure
3. If grokking fails → compositionality must be engineered, not emergent. Focus shifts to understanding Limn as an *optimal design* rather than a *convergent attractor*
4. Noise injection experiment (H16) to test density-as-fragility hypothesis
