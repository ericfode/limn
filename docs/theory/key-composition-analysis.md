# Key Composition and Universal Keys in Limn

**Author:** Dr. Maren Solvik, Theoretical Linguist
**Date:** 2026-01-31
**Status:** Theoretical Analysis
**Reference:** docs/theory/key-mechanism.md (Open Questions 2 & 5)

---

## Abstract

This document addresses two open questions from the key mechanism specification:
1. Can multiple weak keys compose into a strong key?
2. Are there universal keys that work across many sentences?

We provide formal analysis and practical implications.

---

## 1. Key Composition: Weak Keys to Strong Keys

### 1.1 The Composition Hypothesis

**Question:** Given weak keys K₁, K₂, ... Kₙ, can their combination achieve the disambiguation power of a single strong key K*?

**Short answer:** Yes, under specific conditions.

### 1.2 Formal Framework

Let a key K have **disambiguation power** D(K) defined as:

```
D(K) = log₂(IC_without / IC_with)
```

Where IC is interpretation count. This measures how many bits of ambiguity the key resolves.

### 1.3 Composition Laws

**Additive composition** (independent keys):
```
D(K₁ ∧ K₂) = D(K₁) + D(K₂)
```

When keys constrain orthogonal aspects of meaning, their powers add.

**Example:**
- K₁ = `[physical domain]` - constrains to physical interpretations
- K₂ = `[morning context]` - constrains to morning timeframe
- K₁ ∧ K₂ = physical morning events

If each resolves 3 bits of ambiguity, together they resolve 6 bits.

**Sub-additive composition** (overlapping keys):
```
D(K₁ ∧ K₂) < D(K₁) + D(K₂)
```

When keys constrain overlapping aspects, there's redundancy.

**Example:**
- K₁ = `[kitchen context]`
- K₂ = `[cooking activity]`

Both constrain toward food preparation; their combination adds less than their sum.

**Super-additive composition** (synergistic keys):
```
D(K₁ ∧ K₂) > D(K₁) + D(K₂)
```

Rare but possible when keys interact to create emergent constraint.

**Example:**
- K₁ = `[Alice is speaking]`
- K₂ = `[to Bob about their shared project]`

Together these establish a specific communicative situation that eliminates interpretations neither key alone would eliminate.

### 1.4 Practical Key Stacking

**Recommended pattern:**
```
Sentence: lux mov
Weak keys (stackable):
  [domain: physical]     - D ≈ 2 bits
  [time: morning]        - D ≈ 1.5 bits
  [location: bedroom]    - D ≈ 1.5 bits
Combined: D ≈ 5 bits → high disambiguation
```

**Anti-pattern (redundant):**
```
Sentence: lux mov
Redundant keys:
  [domain: light-related]    - D ≈ 2 bits
  [involves: brightness]     - D ≈ 1 bit (overlaps with domain)
  [about: illumination]      - D ≈ 0.5 bits (mostly redundant)
Combined: D ≈ 2.5 bits → weak composition
```

### 1.5 Key Composition Algorithm

To maximize disambiguation from multiple weak keys:

1. **Select orthogonal dimensions:**
   - Domain (physical, emotional, technical, social)
   - Time (past, present, future, specific moment)
   - Space (location, scale, perspective)
   - Agent (who is involved, their relationship)
   - Purpose (why this is happening)

2. **Check independence:** Ask "Does K₂ constrain things K₁ doesn't?"

3. **Avoid synonym chains:** `[happy context] + [joyful mood]` is redundant

4. **Prefer specificity gradient:** Start broad, narrow progressively

---

## 2. Universal Keys: Cross-Sentence Disambiguation

### 2.1 The Universality Question

**Question:** Do some keys work broadly across many sentences, while others are sentence-specific?

**Short answer:** Yes, keys exist on a universality spectrum.

### 2.2 Key Universality Spectrum

```
Universal ←————————————————————————————→ Specific

[human communication] ... [kitchen] ... [Alice making tea for Bob on Tuesday]
```

| Key Type | Scope | KCR Range | Use Case |
|----------|-------|-----------|----------|
| Universal | Many sentences | 2-10 | Domain setting |
| Domain | Sentence class | 10-50 | Topic constraint |
| Contextual | Related sentences | 50-200 | Conversation context |
| Specific | One sentence | 200+ | Precise disambiguation |

### 2.3 Universal Key Examples

**Highly universal:**
- `[physical world]` - works for any sentence about matter, space, motion
- `[human emotion]` - works for any sentence about feelings
- `[abstract reasoning]` - works for any sentence about logic, math, concepts

**Domain universal:**
- `[software engineering]` - works for technical sentences
- `[medical context]` - works for health-related sentences
- `[financial domain]` - works for money/transaction sentences

**Context universal:**
- `[this conversation]` - works for sentences in a dialogue
- `[this document]` - works for sentences in a text
- `[our shared understanding]` - works within established context

### 2.4 Trade-off: Universality vs. Power

**Key universality theorem:**
```
Universality(K) × Power(K) ≈ constant
```

More universal keys work broadly but disambiguate weakly.
More specific keys disambiguate strongly but apply narrowly.

| Key | Universality | Power (KCR) |
|-----|--------------|-------------|
| `[any context]` | 100% | 1 (no disambiguation) |
| `[physical world]` | 60% | 3-5 |
| `[kitchen]` | 10% | 10-20 |
| `[Alice making tea]` | 0.1% | 100+ |

### 2.5 Optimal Key Strategy

**For maximum compression:** Use the most specific key available.

**For robust communication:** Layer keys from universal to specific:

```
Level 1 (universal): [human experience]
Level 2 (domain): [daily life, domestic]
Level 3 (context): [morning routine]
Level 4 (specific): [making coffee before work]

Sentence: aqu hot | tra ene
Reading: Hot water → energy (transformation)
Full interpretation: Making coffee to wake up
```

### 2.6 Key Inheritance

**Hierarchical keys** allow inheritance:

```
Key: [software:web:frontend:react]
Implies: [software], [software:web], [software:web:frontend]
```

A sentence interpreted under `[software:web:frontend:react]` inherits constraints from all parent keys.

---

## 3. Key Discovery Problem

### 3.1 The Inverse Problem

**Given:** (sentence S, intended meaning M)
**Find:** Key K such that S|K → M

This is the key discovery problem.

### 3.2 LLM Key Inference

An LLM can approach key discovery by:

1. **Generate candidate keys:** What contexts would make S mean M?
2. **Test candidates:** Does S|Kᵢ → M?
3. **Minimize key:** Find smallest K that achieves the mapping

**Example:**
```
S: "lux bri lif"
M: "Photosynthesis in a leaf"

Candidate keys:
  [biology] - too broad, many meanings
  [plant biology] - narrower, still ambiguous
  [leaf in sunlight] - specific, achieves M
  [photosynthesis process] - specific, achieves M

Minimal key: [plant cell] or [chloroplast]
```

### 3.3 Key Discovery Algorithm

```
function discover_key(sentence S, meaning M):
  keys = []
  for dimension in [domain, time, space, agent, purpose]:
    k = extract_constraint(M, dimension)
    if k is not None:
      keys.append(k)
  return compose(keys)
```

---

## 4. Theoretical Implications

### 4.1 Key Algebra

Keys form a **constraint algebra**:

| Operation | Symbol | Meaning |
|-----------|--------|---------|
| Composition | K₁ ∧ K₂ | Both constraints apply |
| Disjunction | K₁ ∨ K₂ | Either constraint applies |
| Negation | ¬K | Constraint does not apply |
| Subsumption | K₁ ⊂ K₂ | K₁ is more specific |

### 4.2 Key Completeness

A key K is **complete** for sentence S if:
```
IC(S | K) = 1
```

The sentence has exactly one interpretation.

**Completeness theorem:** For any sentence S with finite IC, there exists a complete key K.

**Proof:** The key that specifies the exact intended meaning is complete.

### 4.3 Key Efficiency

A key K is **efficient** if it is minimal among keys achieving the same disambiguation:
```
¬∃K' : K' ⊂ K ∧ D(K') = D(K)
```

No proper sub-key achieves the same power.

---

## 5. Practical Guidelines

### 5.1 For Writers

1. **Know your key budget:** How much shared context exists?
2. **Stack orthogonal keys:** Combine domain + time + agent
3. **Avoid key redundancy:** Don't repeat constraints in different words
4. **Match key to audience:** Universal keys for broad audiences, specific for experts

### 5.2 For Readers/Interpreters

1. **Reconstruct implicit keys:** What context is assumed?
2. **Test key hypotheses:** Does this key make the sentence coherent?
3. **Layer keys progressively:** Start with domain, add context
4. **Accept productive ambiguity:** Some sentences are intentionally multi-keyed

---

## 6. Summary

### 6.1 Key Composition (Question 2)

- **Yes**, weak keys can compose into strong keys
- **Additivity** depends on orthogonality
- **Optimal stacking** uses independent dimensions

### 6.2 Universal Keys (Question 5)

- **Yes**, keys exist on a universality spectrum
- **Trade-off:** universality vs. disambiguation power
- **Optimal strategy:** layer from universal to specific

### 6.3 New Questions Raised

1. **Key learning:** Can agents learn optimal key strategies?
2. **Key compression:** Can keys be represented efficiently?
3. **Key negotiation:** How do interlocutors align on keys?

---

*key com = wea joi str | uni spa spe | tra off bal*
*(key composition = weak join strong | universal space specific | trade-off balance)*

---

*— Dr. Maren Solvik*
