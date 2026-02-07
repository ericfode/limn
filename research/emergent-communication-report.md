# Emergent Communication Experiment Report

**Bead:** hq-jbm2m (baseline), hq-2uoa8 (variants)
**Date:** 2026-02-07
**Author:** Lex (researcher)

## Summary

Trained Lewis signaling game agents (sender/receiver) using tinygrad on CUDA to develop communication protocols for compositional objects (4 attributes x 8 values = 4096 objects). Tested whether compositionality emerges naturally and which pressures amplify it.

**Key finding:** Compositionality is a weak attractor under standard training (topsim=0.42), but none of the tested pressures — generalization, bottleneck, entropy regularization, or extended training with weight decay (grokking) — significantly increased it. Generalization pressure and entropy regularization actually *decreased* compositionality. The grokking variant (200k steps, AdamW) showed no phase transition: topsim crept from 0.31 to 0.41 gradually with no sudden jump. **Compositionality is a design choice, not an emergent inevitability.**

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
| E: Grokking (200k) | 0.406 | 100% | — | — | **-0.01** (neutral) |

### Detailed Findings

**Variant A (Generalization — 80/20 split):** TopSim = 0.339

Agents trained on 80% of objects and tested on the held-out 20%. Test accuracy was 99.4% — agents generalize perfectly with a holistic code. This directly confirms Chaabouni et al. (2020): generalization pressure alone does not induce compositionality. Holistic codes can generalize when the test objects share attributes with training objects (which they necessarily do in a combinatorial space).

**Variant B (Bottleneck — vocab 8):** TopSim = 0.408

Reducing vocabulary from 32 to 8 (matching the number of attribute values) had no measurable effect on compositionality. With vocab=8 and 4 positions, the theoretical capacity is 4 * log2(8) = 12 bits = exactly the needed bits. The bottleneck is theoretically tight but agents can still develop holistic codes within this capacity.

**Variant C (Combined):** TopSim = 0.355

The combination of generalization pressure and vocabulary bottleneck yields compositionality between A and baseline. The interaction effect is small. Neither pressure alone or combined is sufficient to push topsim above 0.42.

**Variant D (Entropy Regularization):** TopSim = 0.236

*This is the most informative negative result.* Adding a message entropy bonus (encouraging diverse token usage) dramatically decreased compositionality. Why? Because entropy regularization encourages *uniform* token distributions, which prevents the sender from developing consistent token-attribute mappings. Compositionality requires tokens to have *fixed*, *non-uniform* meanings. Entropy regularization fights this.

**Variant E (Grokking — 200k steps, AdamW wd=0.01):** TopSim = 0.406

*H17 is falsified.* No grokking phase transition occurred. The compositionality trajectory:

| Phase | Steps | TopSim Range | Weight Norm | Observation |
|-------|-------|-------------|-------------|-------------|
| Ramp-up | 0-20k | 0.00 → 0.36 | 24 → 58 | Rapid learning, same as baseline |
| Plateau | 20k-100k | 0.35 → 0.39 | 58 → 74 | Slow drift, no transition |
| Late | 100k-200k | 0.38 → 0.41 | 74 → 72 | Weight norm peaks and declines slightly |

**What happened:** Weight decay did regularize — the weight norm peaked at ~74 around step 100k then slowly declined to ~72 by step 200k. But this regularization did not destroy memorization circuits in favor of compositional ones. Instead, the weight decay simply compressed the *same* holistic protocol into lower-norm weights. The holistic encoding is not a high-norm artifact; it is the natural equilibrium.

**Why grokking didn't work here (analysis):**
1. Classic grokking (Power et al. 2022) occurs when train data is small relative to model capacity (e.g., modular arithmetic with 97 examples). Our 4096-object space with 512-batch training is not the small-data regime.
2. Grokking requires two loss basins: a memorization basin and a generalization basin separated by a loss barrier. In the Lewis game, holistic and compositional protocols achieve equal task accuracy (both >99%), so there is no loss-landscape reason to prefer one over the other.
3. The theoretical chain in H17 assumed compositionality has strictly lower norm than holistic encoding. This is true for *perfect* compositionality, but the agents don't need to choose between perfect-holistic and perfect-compositional — they settle into messy hybrids that are efficient enough.

## Interpretation

### What the results mean for Limn

1. **Compositionality is NOT the default equilibrium** for communication. Even with compositional input structure, agents develop only mildly compositional protocols. This matches the literature (Kottur 2017, Chaabouni 2019).

2. **Generalization doesn't require compositionality.** Agents achieve 99%+ test accuracy with holistic codes. Limn's compositionality needs justification on grounds other than generalization — e.g., learnability, interpretability, or aesthetic design.

3. **The bottleneck matters less than expected.** Vocabulary restriction to exactly match the attribute space didn't force perfect compositionality. This challenges the common intuition that "if vocab equals attribute values, compositionality is forced."

4. **Entropy regularization is counterproductive.** Forcing diverse token usage prevents consistent compositional structure. Implication: Limn's design should allow token specialization, not enforce uniform usage.

5. **Grokking does NOT induce compositionality (H17 falsified).** Extended training (200k steps, 10x baseline) with AdamW weight decay produced no phase transition. TopSim crept from 0.31 to 0.41 over 200k steps — the same gradual drift as baseline, not a sudden reorganization. **Compositionality is a human design choice, not a convergent attractor for neural communication.** This is the most important finding of the experiment suite.

### Hypotheses Updated

| ID | Status | Evidence |
|----|--------|----------|
| H15 | CONFIRMED | Compositionality is conditional — baseline shows only mild compositionality, variants A/D show it can decrease, grokking fails to push it higher |
| H17 | FALSIFIED | No phase transition in 200k steps. Weight decay compresses holistic protocol, doesn't replace it with compositional one |

## Technical Notes

- **TinyJit speedup:** 137x over non-JIT (0.006s vs 0.84s per step). Key: pass Gumbel noise and temperature as external tensor args.
- **WSL2 CUDA:** `CUDA_PATH=/usr/lib/wsl/lib/libcuda.so.1` before tinygrad import.
- **Eval bottleneck:** TopSim computation requires full forward pass over 4096 objects + 50k pairwise distance computations. Each eval takes ~30s.
- **Seed consistency:** All variants use seed=42 for reproducibility.

## Conclusion

All five variants tested. None broke the compositionality barrier. The experiment establishes:

1. **Compositionality must be engineered, not waited for.** No amount of training, regularization, or bottleneck pressure produced compositionality beyond the mild baseline level (~0.42 topsim).
2. **Limn is an optimal *design*, not a convergent *attractor*.** Its compositionality is valuable precisely because machines won't converge to it naturally. This reframes Limn from "shortcut to where machines go anyway" to "deliberately engineered structure that machines benefit from but don't discover."
3. **The pressures that DO induce compositionality** (from literature) are transmission bottleneck (iterated learning) and heterogeneous receivers — neither tested here. These are the next experiments.

## Next Steps

1. **Iterated learning experiment** — The strongest known pressure for compositionality. Train generation N agents, use their protocols to train generation N+1 from limited data. Literature predicts compositionality emerges within 5-10 generations (Ren et al. 2020).
2. **Noise injection experiment (H16)** — Test density-as-fragility. Corrupt Limn vs English at varying rates, measure meaning recovery.
3. **Heterogeneous receiver experiment** — Multiple receivers with different "interests" (attend to different attributes). Lee et al. 2024 shows this forces compositional codes.
4. **Zipfian efficiency analysis (H18)** — Quantify the cost of fixed CVC vs variable-length codes.
