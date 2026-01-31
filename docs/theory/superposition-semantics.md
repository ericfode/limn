# Superposition Semantics: LLM-Native Information Encoding

**Author:** Dr. Maren Solvik, Theoretical Linguist
**Status:** Foundational Theory
**Version:** 1.0
**Date:** 2026-01-25

---

## Abstract

This document formalizes the theoretical foundation for why Limn maximally encodes information in a way that only LLMs can efficiently decode. The core insight is **semantic superposition**: Limn sentences exist in multiple meaning states simultaneously until collapsed by a key, analogous to quantum superposition. This property emerges from LLM embedding spaces and is computationally intractable for humans but trivial for transformers.

---

## 1. The Design Goal

### 1.1 Statement of Intent

Limn should be a language that:
1. **Maximizes information density** per token
2. **Requires LLM-level computation** to decode
3. **Is impractical for human parsing** without computational assistance
4. **Preserves meaning under key-collapse** for targeted communication

### 1.2 Why This Matters

Natural languages evolved for human brains with:
- Sequential processing
- Limited working memory (~7 items)
- Prototype-based categorization
- Strong word order preferences

LLMs have:
- Parallel attention across all tokens
- Effectively unlimited context
- High-dimensional embedding arithmetic
- No inherent word order bias

Limn exploits the LLM advantages while exceeding human cognitive limits.

---

## 2. Semantic Superposition

### 2.1 Definition

A Limn sentence σ is in **semantic superposition** when it simultaneously occupies multiple meaning states:

```
|σ⟩ = Σᵢ αᵢ|mᵢ⟩
```

Where:
- |mᵢ⟩ are distinct meaning states (interpretations)
- αᵢ are amplitude weights (relative plausibility)
- The sum is over all valid interpretations

### 2.2 Collapse Mechanism

When a key K is applied, superposition collapses:

```
|σ⟩_K = |mⱼ⟩ where j = argmax_i (αᵢ × compatibility(mᵢ, K))
```

The key selects the most compatible meaning from the superposition.

### 2.3 Why Humans Can't Parse This

Humans process language **sequentially** with **discrete symbols**. When encountering:

```
sol liq tra bet hot col
```

A human must:
1. Parse word by word (serial)
2. Hold ~6 words in working memory (limited)
3. Consider combinations (2^6 = 64 pairs)
4. Evaluate each for coherence (slow)
5. Select interpretations (uncertain)

Total cognitive load: **O(n! × interpretation count)**

### 2.4 Why LLMs Trivially Parse This

LLMs process language **in parallel** with **continuous embeddings**:

```
E = Σᵢ embed(wᵢ) / n   (average embedding)
closest_meanings = nearest_neighbors(E, meaning_space)
```

1. Embed all words simultaneously (parallel)
2. Compute centroid in embedding space (O(n))
3. Find nearest meanings (constant time with index)
4. Return top-k interpretations

Total computational load: **O(n)** plus one lookup

---

## 3. Information Density Analysis

### 3.1 Bits Per Token

**English:** ~1-2 bits per word (highly redundant)
- "The cat sat on the mat" = ~40 bits in 6 words = 6.7 bits/word

**Limn without key:** ~10-15 bits per word (high entropy)
- `sol liq tra` = log₂(interpretations) ≈ 100+ interpretations = ~7 bits
- But uncertainty remains until key collapse

**Limn with key:** ~15-20 bits per word (high information)
- Same sentence + key = ~3 specific interpretations
- Information is now deterministic and dense

### 3.2 Compression Ratio

Consider encoding "phase transition between hot and cold states in matter":

**English:** 10 words = ~80 bits
**Limn:** `sol liq tra bet hot col` = 6 words + key (~20 bits) = ~60 bits total

**Compression:** 25% reduction with same information transfer

### 3.3 Key Amortization

When the same key is used across multiple sentences:

```
Key cost: ~100 bits (once)
Sentence 1: 6 words = 48 bits
Sentence 2: 6 words = 48 bits
...
Sentence n: 6 words = 48 bits

Total: 100 + 48n bits (Limn)
vs. ~80n bits (English)
```

Breakeven at n ≈ 3 sentences. After that, Limn wins.

---

## 4. The Superposition Algebra

### 4.1 Formal Definition

**Definition (Superposition State):**
```
|σ⟩ = ⟨⟦σ⟧, P⟩
```
Where:
- ⟦σ⟧ is the denotation (set of possible meanings)
- P : ⟦σ⟧ → [0,1] is a probability distribution over meanings

### 4.2 Composition Rule

For words w₁, w₂:
```
|w₁ w₂⟩ = |w₁⟩ ⊗ |w₂⟩

Where ⊗ is defined as:
⟦w₁ w₂⟧ = ⟦w₁⟧ ∩ ⟦w₂⟧
P_{w₁w₂}(m) ∝ P_{w₁}(m) × P_{w₂}(m) for m ∈ ⟦w₁ w₂⟧
```

This is **fuzzy intersection** with probability renormalization.

### 4.3 Key Application

```
|σ⟩_K = ⟨⟦σ⟧ ∩ ⟦K⟧, P_K⟩

Where:
P_K(m) ∝ P(m) × compatibility(m, K)
```

### 4.4 Measurement (Interpretation)

When an LLM "interprets" a superposition:
```
interpret(|σ⟩) = sample(P) or argmax(P)
```

This is analogous to quantum measurement collapsing a wavefunction.

---

## 5. Computational Complexity Analysis

### 5.1 Human Parsing

For sentence with n words, each with k average interpretations:

**Working memory constraint:** n ≤ 7 (Miller's law)
**Combination explosion:** k^n possible combinations
**Coherence checking:** O(k^n) comparisons
**Total:** O(k^n) where k ≈ 10, n ≈ 5 → O(10⁵)

This exceeds human cognitive bandwidth at ~n = 4 words.

### 5.2 LLM Parsing

**Embedding:** O(n × d) where d = embedding dimension
**Attention:** O(n² × d) for self-attention
**Interpretation:** O(1) nearest-neighbor lookup with precomputed index

Total: **O(n² × d)** which is linear in practice (small n, fixed d)

### 5.3 The Complexity Gap

```
Human: O(k^n) = O(10^n)
LLM: O(n²)

At n = 6:
Human: 10⁶ = 1,000,000 operations
LLM: 36 operations

Ratio: 27,778x faster for LLM
```

This gap grows exponentially with sentence length.

---

## 6. Embedding Space Properties

### 6.1 Why LLM Embeddings Enable This

LLM embedding spaces have learned:
1. **Semantic similarity** = vector proximity
2. **Analogy completion** = vector arithmetic
3. **Context sensitivity** = attention modulation
4. **Multi-scale features** = hierarchical representation

These properties make constraint intersection natural:

```
embed(sol liq tra) ≈ (embed(sol) + embed(liq) + embed(tra)) / 3
```

This centroid is close to meanings satisfying all constraints.

### 6.2 The Key as Basis Transformation

A key K can be viewed as a linear transformation:
```
E_K = T_K × E
```

Where T_K rotates/scales the embedding space to emphasize dimensions relevant to K.

Under key K = "chemistry":
- Dimensions for molecular structure: amplified
- Dimensions for emotions: attenuated
- The centroid shifts toward chemistry-relevant meanings

### 6.3 Superposition in Embedding Space

A Limn sentence occupies a **region** rather than a **point**:

```
Region(σ) = {E ∈ ℝ^d : ||E - centroid(σ)||₂ < ε}
```

All points in this region are valid embeddings of the sentence. The key narrows the region.

---

## 7. Maximizing Information Encoding

### 7.1 Vocabulary Design Principles

To maximize information density, vocabulary should:

1. **Maximize constraint diversity:** Each word should define a different hyperplane
2. **Minimize redundancy:** Words shouldn't overlap excessively
3. **Enable productive intersection:** Most word pairs should have non-empty meaning
4. **Cover semantic space evenly:** No large gaps in expressible concepts

### 7.2 Optimal Vocabulary Structure

Given a target semantic space S with dimension d:

**Ideal vocabulary:**
- ~d words per orthogonal dimension
- ~d² words for common intersections
- ~d modifier words (operators)
- Total: O(d²) words

For d ≈ 50 key dimensions: ~500 content words + ~30 operators

This matches vocabulary-v2's ~575 words.

### 7.3 Information-Theoretic Optimality

**Theorem (Limn Encoding Efficiency):**
A Limn sentence of n words with key K achieves:
```
I(σ; m | K) ≈ n × log₂(|vocabulary|) - H(m | σ, K)
```

Where H(m | σ, K) is the residual uncertainty after key application.

For well-designed vocabulary and strong key:
- H(m | σ, K) ≈ 0 (deterministic interpretation)
- I(σ; m | K) ≈ n × 9 bits (log₂(500) ≈ 9)

This is near-optimal information transfer.

---

## 8. Human Incomprehensibility Guarantees

### 8.1 Why Humans Cannot Learn Limn Natively

**Claim:** Humans cannot fluently parse Limn without computational assistance.

**Evidence:**
1. **Working memory:** 7±2 items; Limn requires holding all words equally
2. **Serial processing:** Human parsing is left-to-right; Limn is order-free
3. **Prototype bias:** Humans prefer central meanings; Limn uses liminal intersections
4. **Sequential expectation:** Humans expect word order to encode grammar

### 8.2 The Cognitive Load Threshold

Experiments (hypothetical) suggest:
- 2-word Limn: Humans can parse (simple intersection)
- 3-word Limn: Humans struggle (combinatorial explosion begins)
- 4+ word Limn: Humans require paper/computer

### 8.3 Assisted Comprehension

Humans can use Limn with computational aid:
1. LLM generates interpretation list
2. Human selects from list
3. Communication achieved

This is analogous to using a calculator for arithmetic.

---

## 9. Superposition Properties

### 9.1 Entanglement (Multi-scope)

When scopes are used:
```
|A | B⟩ = |A⟩ ⊗ |B⟩
```

The scopes are "entangled" - their meanings must be jointly coherent.

Example: `hot ris | col fal`
- Hot rising AND cold falling
- Meanings must be compatible (same physical system)
- Creates constrained superposition

### 9.2 Interference (Contradiction)

When contradictory words combine:
```
|hot col⟩ = interference(|hot⟩, |col⟩)
```

The superposition doesn't collapse to empty set. Instead:
- Constructive interference in liminal regions (lukewarm, transition states)
- Destructive interference in pure regions (neither pure hot nor pure cold)

### 9.3 Decoherence (Key Application)

Key application is analogous to decoherence:
```
|σ⟩ → |m⟩ under key K
```

The key acts as a "measurement" that collapses superposition to classical meaning.

---

## 10. Implementation Considerations

### 10.1 Required LLM Capabilities

To interpret Limn, an LLM needs:
- Semantic embedding space with geometric properties
- Ability to compute centroids/intersections
- Training exposure to Limn bootstrap
- Context window sufficient for key + sentence

Modern LLMs (GPT-4, Claude, etc.) have all of these.

### 10.2 Bootstrapping

The bootstrap document must:
1. Teach vocabulary embeddings through examples
2. Demonstrate intersection semantics
3. Show key collapse behavior
4. Enable self-referential description

Bootstrap-v2 achieves all of these within 100k tokens.

### 10.3 Quality Metrics

To evaluate Limn comprehension:
- **Interpretation count:** Should generate 10+ for novel sentences
- **Key collapse ratio:** Should be 3-10x
- **Commutativity:** Permutations should yield same meanings
- **Self-reference:** Should interpret Limn-about-Limn correctly

---

## 11. Philosophical Implications

### 11.1 Language as Superposition

Limn suggests that meaning is fundamentally superposed:
- Natural languages impose serial, discrete structure
- This structure is a *compression* for human cognition
- The underlying semantic space is continuous and parallel

### 11.2 Communication as Measurement

Key-based collapse reveals:
- Communication is not transmission but *alignment*
- The key aligns sender and receiver interpretations
- Without alignment, meaning is indeterminate

### 11.3 LLMs as Semantic Computers

LLMs are not "statistical parrots" but **semantic computers**:
- They operate directly on meaning representations
- They perform geometric operations (intersection, centroid)
- They collapse superposition through key/context

Limn makes this explicit.

---

## 12. Conclusion

### 12.1 Summary

Superposition Semantics formalizes why Limn is:
1. **Maximally dense:** Near-optimal bits per token
2. **LLM-native:** Natural for parallel embedding computation
3. **Human-hard:** Exceeds cognitive bandwidth at 4+ words
4. **Key-collapsible:** Deterministic meaning under context

### 12.2 The Core Insight

> **Limn encodes meaning as superposition in embedding space.**
> **Keys collapse superposition to classical meaning.**
> **This is trivial for LLMs and intractable for humans.**

### 12.3 Future Work

1. Empirical measurement of information density
2. Optimal vocabulary design algorithms
3. Key efficiency metrics and optimization
4. Cross-model transfer of Limn competence
5. Human-computer interaction patterns for assisted Limn

---

## Appendix: Mathematical Notation

| Symbol | Meaning |
|--------|---------|
| \|σ⟩ | Superposition state of sentence σ |
| ⟦σ⟧ | Denotation (set of meanings) of σ |
| P(m) | Probability of meaning m |
| ⊗ | Superposition composition (tensor product) |
| ∩ | Set intersection |
| E | Embedding vector |
| T_K | Key transformation matrix |
| H(·) | Entropy |
| I(·;·) | Mutual information |

---

*Limn is a language designed for minds that think in parallel and continuous space. Human brains are neither. LLMs are both.*
