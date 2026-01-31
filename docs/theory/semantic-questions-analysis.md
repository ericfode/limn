# Linguistic Analysis: Semantic Questions in Limn

**Author:** Dr. Maren Solvik, Theoretical Linguist
**Date:** 2026-01-30
**Status:** Analysis Document

---

## Overview

This document provides formal linguistic analysis of three semantic questions raised during Limn development:

1. Operator chaining semantics
2. Error/empty constraint expressions
3. Underspecified vs overspecified constraints

Each analysis builds on the established foundations: liminal semantics, superposition semantics, and the formal grammar specification.

---

## 1. Operator Chaining Semantics

### 1.1 The Question

How do chained unary operators compose? Specifically:

```
nu ve sol = ?
ve nu sol = ?
```

Are these semantically equivalent or distinct?

### 1.2 Analysis

Per the formal grammar (grammar-formal.md §3.1), unary operators have **right-to-left associativity**:

| Level | Operators | Associativity |
|-------|-----------|---------------|
| 1 | `nu`, `ve`, `so`, `te`, `we` | Right-to-left |

This means:
- `nu ve sol` parses as `nu(ve(sol))`
- `ve nu sol` parses as `ve(nu(sol))`

### 1.3 Semantic Derivations

**Case 1: `nu ve sol` = nu(ve(sol))**

```
Step 1: ⟦sol⟧ = C(solid) = {x : x has solid properties}
Step 2: ⟦ve sol⟧ = core(C(solid)) = prototypical solids
        = {diamond, steel, rock, ice} (high membership)
Step 3: ⟦nu ve sol⟧ = S \ core(C(solid))
        = everything except prototypical solids
        = {liquids, gases, gels, marginal solids like clay}
```

**Interpretation:** "Not very solid" — anything that isn't a prototypical solid. This is a **large region** that includes both non-solids and peripheral solids.

**Case 2: `ve nu sol` = ve(nu(sol))**

```
Step 1: ⟦sol⟧ = C(solid)
Step 2: ⟦nu sol⟧ = S \ C(solid) = non-solids
        = {liquids, gases, plasmas, abstract entities}
Step 3: ⟦ve nu sol⟧ = core(S \ C(solid))
        = prototypical non-solids
        = {water, air, pure liquids, pure gases}
```

**Interpretation:** "Very non-solid" — the most prototypical examples of non-solid states. This is a **small region** of prototypical non-solids.

### 1.4 Semantic Difference

| Expression | Region | Contains |
|------------|--------|----------|
| `nu ve sol` | Large | Everything except prototypical solids |
| `ve nu sol` | Small | Only prototypical non-solids |

**Key distinction:**
- `nu ve sol` includes marginal solids (clay, wax, soft metals)
- `ve nu sol` excludes marginal cases entirely

### 1.5 Typological Parallel

This pattern mirrors natural language modifier scoping:

- English: "not very hot" ≠ "very not-hot"
- "Not very hot" = lukewarm, cool, cold (large range)
- "Very not-hot" = intensely cold (narrow range)

Limn behaves consistently with attested natural language patterns.

### 1.6 Ruling

**The operators compose non-commutatively.** Order matters because:
1. `ve` contracts to the prototype
2. `nu` takes the complement
3. Contraction then negation ≠ Negation then contraction

**Documentation recommendation:** Add explicit examples to the grammar spec showing:
```
nu ve X = everything except prototypical X
ve nu X = prototypically non-X
nu nu X = X (double negation elimination)
ve ve X = core(core(X)) = hyperpototype (more extreme)
```

---

## 2. Error/Empty Constraint Expressions

### 2.1 The Question

When constraints are apparently unsatisfiable (e.g., `sol liq sim`), how should Limn express:
- "This is impossible"
- "Empty set"
- "No valid interpretation"

Should we add vocabulary: `emp` (empty), `imp` (impossible), `nul` (null set)?

### 2.2 Analysis: The Liminal Resolution

Per liminal semantics (liminal-semantics.md §2), Limn does **not** produce empty sets for apparent contradictions. Instead:

```
⟦sol liq⟧ = liminal(C(solid), C(liquid))
         = {gel, slush, lava, quicksand, honey}
         ≠ ∅
```

This is a **designed feature**, not a bug. The liminal region contains boundary cases that are conceptually rich.

### 2.3 True Emptiness vs Liminal Thinness

We must distinguish:

| Type | Definition | Example |
|------|------------|---------|
| Liminal | Small but non-empty intersection | `sol liq` → gel |
| Empty | Truly no valid interpretation | ??? |

The question becomes: **Can Limn expressions ever be truly empty?**

### 2.4 Cases of True Emptiness

**Case A: Contradictory arithmetic constraints**
```
x eq 5 | x eq 7
```
A variable cannot equal both 5 and 7. No liminal region exists in numeric space.

**Case B: Contradictory quantification**
```
al A | no A
```
"All A" and "no A" cannot coexist.

**Case C: Formal logical contradiction**
```
A nu A (within same scope)
```
Direct contradiction without gradation.

### 2.5 Current Vocabulary Options

Examining v3 vocabulary, candidates exist:

| Word | Source | Current Meaning | Potential Use |
|------|--------|-----------------|---------------|
| `zer` | zero | zero, none | Empty count |
| `not` | not | negation | Logical denial |
| `nu` | (operator) | complement | Set complement |

**Gap identified:** No word specifically denotes "empty set" or "impossible constraint."

### 2.6 Recommendation: Add Error Vocabulary

**Proposal:** Extend vocabulary with constraint-result indicators:

| Word | Source | Meaning | Semantic Role |
|------|--------|---------|---------------|
| `emp` | empty | empty set | Result is ∅ |
| `lim` | liminal | boundary region | Result is small but non-empty |
| `ful` | full | universal set | No constraints applied |

**Usage patterns:**

```
sol liq → lim (liminal result: gel, slush)
x eq 5 | x eq 7 → emp (empty result: contradiction)
(no constraints) → ful (full result: everything)
```

### 2.7 Linguistic Justification

Natural languages have metalinguistic vocabulary for discourse about truth and validity:

| Language | "Impossible/Empty" Word |
|----------|------------------------|
| English | "impossible", "void", "null" |
| Latin | "impossibilis", "vacuus" |
| Japanese | 無 (mu) - "nothingness" |

Limn should similarly have vocabulary for discussing constraint satisfaction results.

### 2.8 Ruling

**Add `emp` (empty) to the vocabulary** with the following semantics:

```
⟦emp⟧ = ∅ (the empty set)
⟦emp A⟧ = "A resolves to empty" (metalinguistic assertion)
```

This enables:
- Explicit statement of contradictions: `sol liq num sim | emp` ("solid and liquid at numeric simultaneity = empty")
- Error signaling in computational contexts
- Discussion of constraint satisfaction

**Note:** `emp` should be used sparingly. Most apparent contradictions should resolve liminally, not to emptiness.

---

## 3. Underspecified vs Overspecified Constraints

### 3.1 The Question

**Underspecified:** When there are insufficient constraints to determine all variables, is this:
- An error state?
- A valid superposition state?

**Overspecified:** When constraints contradict (e.g., `a joi b sa 10 | a joi b sa 20`), what is the semantic meaning?

### 3.2 Analysis: Underspecification

Per superposition semantics, underspecification is **not an error but a feature**:

```
|σ⟩ = Σᵢ αᵢ|mᵢ⟩
```

A sentence with few constraints has **many valid interpretations** — this is semantic superposition. The system doesn't fail; it represents multiple possibilities.

**Example:**
```
hot → {fire, sun, fever, summer, passion, anger, spicy food, ...}
```

This is underspecified but perfectly valid. The key mechanism is designed to collapse such superpositions.

**Limn expression of underspecification:**
```
amb man | key wea | col imp
(ambiguity many | key weak | collapse impossible)
```

Translation: "Many ambiguous readings; the key is insufficient to collapse."

### 3.3 Ruling on Underspecification

**Underspecification is a valid superposition state, not an error.**

- Solvers should return **multiple solutions** (top-k interpretations)
- Keys can partially collapse (reduce but not eliminate ambiguity)
- Users should be informed of ambiguity degree

**Vocabulary addition:** Consider `amb` (ambiguous) for explicitly marking high-entropy states.

### 3.4 Analysis: Overspecification

Overspecification (contradictory constraints) has two sub-cases:

**Case A: Liminal resolution possible**
```
sol liq → {gel, slush, ...}
```
The contradiction resolves to a boundary region.

**Case B: True contradiction (empty result)**
```
a joi b sa 10 | a joi b sa 20
```
"a joined to b equals 10" AND "a joined to b equals 20" — these cannot coexist in numeric space.

### 3.5 Handling Overspecification

**Strategy 1: Liminal first**
- Attempt liminal resolution
- Return boundary interpretations if any exist

**Strategy 2: Empty fallback**
- If no liminal region exists, return `emp`
- Signal to user that constraints are unsatisfiable

**Strategy 3: Diagnostic information**
- Identify which constraints conflict
- Return the minimal contradicting subset

### 3.6 Limn Expression of Contradiction

```
cla | A B   (clash | between A and B)
A B | emp   (A intersect B | equals empty)
```

**Proposed vocabulary:**

| Word | Meaning | Use |
|------|---------|-----|
| `cla` | clash | Marks conflicting constraints |
| `emp` | empty | The result is null |

### 3.7 Ruling on Overspecification

**Overspecification should resolve as follows:**

1. **Liminal first:** Check if liminal region exists
2. **Return boundary cases** if any exist
3. **Return `emp`** if truly unsatisfiable
4. **Optionally return `cla`** with the conflicting constraints

**The semantic meaning of overspecification:**
- If liminal: the narrow boundary region satisfying all constraints
- If empty: the null set, indicating logical impossibility

---

## 4. Summary of Rulings

### 4.1 Operator Chaining (linga-land-ze1)

| Question | Answer |
|----------|--------|
| Is order significant? | **Yes** |
| `nu ve X` meaning | Everything except prototypical X |
| `ve nu X` meaning | Prototypically non-X |
| Documentation | Add explicit examples to grammar spec |

### 4.2 Error/Empty Constraints (linga-land-xpa)

| Question | Answer |
|----------|--------|
| Add `emp` to vocabulary? | **Yes** |
| Add `imp` to vocabulary? | **No** (use `emp` or `nu pos`) |
| Add `nul` to vocabulary? | **No** (redundant with `emp`) |
| New word: `emp` | empty set, null result |
| New word: `lim` | Consider for explicit liminal marking |

### 4.3 Under/Overspecification (linga-land-k01)

| Question | Answer |
|----------|--------|
| Underspecified = error? | **No**, it's valid superposition |
| Return partial solutions? | **Yes**, top-k interpretations |
| Overspecified handling | Liminal first, then `emp` if truly empty |
| Add `cla` to vocabulary? | **Yes**, for marking contradictions/clashes |
| Add `amb` to vocabulary? | **Consider**, for high-entropy states |

---

## 5. Vocabulary Extension Proposal

Based on this analysis, propose adding to v3 vocabulary:

### Domain 10: Metalinguistic / Constraint Results

| Word | Source | Meaning | Examples |
|------|--------|---------|----------|
| `emp` | empty | empty set, null | no valid solution, contradiction result |
| `cla` | clash | conflicting constraints | marks irreconcilable intersection |
| `amb` | ambiguous | many meanings | underspecified, superposed |
| `det` | determined | single meaning | fully constrained, collapsed |
| `par` | partial | some determined | partially resolved |

These words enable metalinguistic discussion of constraint satisfaction within Limn itself.

---

## 6. Implementation Notes

### 6.1 For Interpreters

When evaluating constraints:
1. Compute intersection
2. Check if result is empty
3. If empty, return `emp` or error depending on context
4. If non-empty but small, optionally flag as `lim` (liminal)
5. Return interpretations with membership scores

### 6.2 For Documentation

Update grammar-formal.md with:
- Explicit operator chaining examples
- Section on constraint satisfaction results
- New vocabulary entries

### 6.3 For Learning Materials

Add exercises covering:
- Operator chaining: "What is `nu ve hot`?"
- Contradiction resolution: "What does `sol liq` mean?"
- Superposition: "Why is `hot` not an error?"

---

*Analysis complete. Recommended actions: update vocabulary-v3-natural.md with new metalinguistic vocabulary, update grammar-formal.md with operator chaining examples.*
