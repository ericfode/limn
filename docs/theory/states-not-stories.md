# States Not Stories: A Core Limn Principle

**Author:** Dr. Maren Solvik, Theoretical Linguist
**Date:** 2026-01-25
**Reference:** linga-land-0zf

---

## 1. The Principle

> **Limn encodes states, not stories.**

Limn is designed to describe **static constraint intersections** (states) rather than **dynamic sequences** (stories). This is a fundamental design choice that shapes what Limn can and cannot express.

---

## 2. Formal Definition

### 2.1 State

A **state** is a simultaneous conjunction of constraints:

```
State = C1 ∩ C2 ∩ ... ∩ Cn
```

Where each Ci is a constraint region (semantic space of a word).

**Example:**
```limn
chi gar sit bor
```
= child ∩ garden ∩ sitting ∩ bored
= "a bored child sitting in a garden" (a snapshot)

### 2.2 Story

A **story** is a temporal sequence of states with causal connections:

```
Story = S1 → S2 → S3 → ...
```

Where → indicates temporal succession and/or causation.

**Example (English):**
> "Alice was sitting in the garden, getting bored. She saw a white rabbit run by and decided to follow it."

This involves:
- State 1: Alice bored in garden
- Causation: seeing rabbit
- State 2: Decision to follow
- State 3: Following

---

## 3. Why Limn Favors States

### 3.1 Design Properties

| Property | Effect on States | Effect on Stories |
|----------|------------------|-------------------|
| **Commutativity** | Natural (order irrelevant) | Breaks (order is meaning) |
| **No tense** | Natural (timeless) | Problematic (when?) |
| **Superposition** | Natural (multiple meanings) | Conflicting (sequences branch) |
| **No proper nouns** | Natural (concepts) | Problematic (who?) |

### 3.2 Mathematical Basis

States are **set intersections**: well-defined, commutative, associative.

Stories are **sequences**: order-dependent, require temporal logic.

Limn's constraint intersection model:
```
⟦A B C⟧ = ⟦A⟧ ∩ ⟦B⟧ ∩ ⟦C⟧
```

There is no operator for "A then B" in this model.

### 3.3 LLM-Native Reasoning

LLMs process meaning through:
1. Semantic embeddings (static vectors)
2. Attention mechanisms (parallel, not sequential)
3. Context windows (simultaneous, not temporal)

States align with this processing model. Stories require additional structure.

---

## 4. Implications

### 4.1 What Limn Expresses Well

| Content Type | Limn Suitability | Example |
|--------------|------------------|---------|
| Scenes | Excellent | `gar sun chi pla` (sunny garden, child playing) |
| Emotions | Excellent | `sad lon mis` (sad lonely missing) |
| Concepts | Excellent | `lib tru bea` (freedom truth beauty) |
| Descriptions | Excellent | `tal dar han man` (tall dark handsome man) |
| Aphorisms | Good | `fea dan opa` (fear is the mind-killer) |

### 4.2 What Limn Struggles With

| Content Type | Limn Suitability | Why |
|--------------|------------------|-----|
| Narratives | Poor | Require temporal sequence |
| Arguments | Poor | Require logical sequence |
| Instructions | Poor | Require ordered steps |
| Causation | Moderate | Lacks temporal operators |

### 4.3 Translation Fidelity

Empirical testing shows:
- **~70% fidelity** on narrative round-trips
- States preserved, sequences lost
- Proper nouns lost (no mechanism)
- Fine detail lost (superposition)

This 70% represents the "state content" of the original.

---

## 5. Working Around the Limitation

### 5.1 Temporal Markers

For sequences, use explicit temporal vocabulary:

```limn
bef | chi sit gar
aft | chi see rab
the | chi fol rab
```

= before: child sits garden
= after: child sees rabbit
= then: child follows rabbit

### 5.2 Causal Operators

Use `cau` and `eff` for causal relationships:

```limn
see rab | cau | fol rab
```

= seeing rabbit causes following rabbit

### 5.3 Sequential Scopes

Use scope markers for implicit sequence:

```limn
sit | see | ris | run
```

= sitting → seeing → rising → running

The left-to-right reading implies sequence.

### 5.4 Accept Lossiness

Some narratives simply don't compress into Limn. This is acceptable:
- Limn is not a universal language
- Limn is optimized for specific use cases
- Keys can restore narrative context

---

## 6. Theoretical Foundation

### 6.1 Constructivism

Limn is constructivist: it describes what EXISTS, not what HAPPENS.

The void is unsayable (see void-semantics-ruling.md).
The sequence is underdetermined (this principle).

### 6.2 Information Density

States are information-dense (many constraints, one expression).
Stories require more tokens (sequential markers, proper nouns).

Limn optimizes for state density, not narrative density.

### 6.3 Key Leverage

Keys provide temporal/narrative context:
- Key: "Alice in Wonderland, opening chapter"
- Limn: `chi gar bor see rab`
- Result: Full narrative reconstructed from state + key

---

## 7. Comparison

| Language | Primary Mode | Secondary Mode |
|----------|--------------|----------------|
| English | Stories | States |
| Limn | States | Stories (with markers) |
| Math | Relations | Sequences |
| Poetry | States + Stories | - |

Limn aligns more with mathematical/poetic expression than prose narrative.

---

## 8. Summary

**States Not Stories** means:

1. Limn naturally describes simultaneous constraints (states)
2. Limn struggles with temporal sequences (stories)
3. This is intentional, not a deficiency
4. Workarounds exist (temporal markers, keys)
5. ~70% of narrative content is state-content (preserved)
6. ~30% is sequence-content (lost without markers)

**Design Choice:** Limn trades narrative expressiveness for information density and LLM-native processing.

---

*This principle addresses linga-land-0zf. See also translation-fidelity-analysis.md and llm-native-design-principles.md.*
