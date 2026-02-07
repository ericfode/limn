# Sprint 4 Synthesis: The Design Space of Machine Language

**Date:** 2026-02-07
**Author:** Lex (researcher)
**Beads:** hq-jbm2m, hq-2uoa8, hq-eekfq, hq-ue0z2

## Executive Summary

Three experiments completed in Sprint 4 converge on a single insight: **Limn is a well-engineered artifact in an underexplored design space, not a natural convergent point.**

| Experiment | Hypothesis | Result | Implication |
|-----------|-----------|--------|-------------|
| Lewis game variants (A-E) | H15, H17 | Compositionality is not emergent | Limn's structure is engineered, not inevitable |
| Noise injection | H16 | Density = fragility | Limn needs channel coding for noisy deployment |
| Zipfian efficiency | H18 | CVC wastes 49% of encoding | Variable-length is strictly better for machines |

## The New Picture of Limn

### What Limn IS

1. **An efficient source code** — 53% denser than English after compression (H11, Sprint 3). Each Limn character carries more semantic information than each English character.

2. **A deliberately compositional encoding** — Compositionality does not emerge naturally in neural communication (H15, confirmed). Limn's compositional operators are a *design decision* that provides extensibility, interpretability, and algebraic structure. These are valuable engineering properties, not inevitable ones.

3. **A human-readable machine language** — The CVC constraint, while costing 49% of optimal encoding (H18), makes Limn pronounceable and memorable. This is a deliberate tradeoff: human-learnability at the cost of machine-optimal bandwidth.

### What Limn is NOT

1. **Not where machines converge** — Given unconstrained communication, neural agents develop holistic, anti-efficient codes (topsim ~0.42, no grokking transition). Compositionality requires external pressures that don't exist in typical M2M deployment.

2. **Not robust under noise** — Limn preserves 14-27 percentage points less meaning than English under character corruption (H16). The dense 3-letter namespace causes 3-8x more silent collisions (wrong meaning, not broken meaning). Without channel coding, Limn is fragile.

3. **Not bandwidth-optimal** — The fixed 3-character word constraint wastes 49% of encoding capacity vs Huffman coding (H18). For pure machine communication, variable-length tokens would double throughput.

## Revised Design Space

The experiments suggest Limn sits at a specific point in a multi-dimensional design space:

```
                    Human-Readable ←───────→ Machine-Optimal
                          │                        │
Compositionality:      designed in              not emergent
Word length:           fixed (3)               variable (Huffman)
Redundancy:            minimal (dense)          error-coded
Bandwidth:             53% better than EN       ~2x better than Limn-CVC
Noise resilience:      fragile (14-27pp gap)    robust (channel coded)
```

### Three Possible Limn Futures

**1. Limn as Human-Machine Bridge** (current design)
- Keep CVC constraint (human readability)
- Add explicit error-correction layer for noisy channels
- Accept 49% bandwidth overhead as the price of interpretability
- Best for: human oversight of machine communication, pedagogical use

**2. Limn-M: Machine Dialect**
- Drop CVC constraint → variable-length tokens
- Keep compositional operators (they provide algebraic structure, not just compositionality)
- Add channel coding (Reed-Solomon or similar)
- Expected improvement: 2x bandwidth, noise-robust
- Best for: M2M communication where no human reads the wire format

**3. Limn-S: Source-Coded Limn**
- Limn as source representation (human-side)
- Arithmetic/Huffman coding as wire format (machine-side)
- Shannon's source-channel separation theorem says this is the optimal architecture
- Best for: mixed environments where Limn is authored by humans, transmitted between machines

## Open Questions

1. **Would iterated learning produce higher compositionality?** The Lewis game variants tested individual pressures but not transmission bottleneck (the strongest known pressure). This is the next experiment.

2. **What's the error-correction overhead for Limn?** Channel coding adds redundancy. How much of the 53% density advantage survives after adding enough error correction to match English's noise resilience?

3. **Does compositionality matter for machines at all?** If holistic codes work fine for machine communication (99%+ accuracy, perfect generalization), why force compositionality? The answer may be: extensibility, debuggability, and compositional generalization to novel concepts. But these need empirical validation.

4. **What's the optimal alphabet size for machine Limn?** The 26-letter constraint is inherited from English. A larger alphabet (e.g., 256 byte values) would allow shorter words while maintaining prefix-free decodability.

## Hypotheses Status After Sprint 4

| ID | Status | Key Number |
|----|--------|-----------|
| H15 | CONFIRMED | Compositionality is conditional, not default |
| H16 | CONFIRMED | 14-27pp meaning preservation gap under noise |
| H17 | FALSIFIED | No grokking phase transition in 200k steps |
| H18 | CONFIRMED | 49.3% CVC encoding overhead |

## Next Experiments (Priority Order)

1. **Iterated learning** — Does transmission pressure (new agents learning from limited data) induce compositionality? This is the strongest known pressure and the only one we haven't tested.

2. **Error correction overhead** — What's the minimum redundancy needed to bring Limn's noise resilience up to English's level? This directly determines whether Limn-S is viable.

3. **Variable-length Limn prototype** — Build a minimal Limn-M variant with Huffman-coded tokens. Test whether the compositional operators still work when word boundaries are variable.

---

```limn
kno bui | tru nar | des gro
> knowledge built | truth narrows | design grows
```

*— Lex*
