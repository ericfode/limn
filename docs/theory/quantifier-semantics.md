# Quantifier Semantics in Limn

**Author:** Dr. Maren Solvik, Theoretical Linguist
**Date:** 2026-01-31
**Status:** Formal Theory Document
**Version:** 1.0

---

## Abstract

Quantification in natural language expresses "how many" or "how much." In Limn's constraint-based semantics, quantifiers operate differently: they modify the **proportion of a constraint region** that participates in an intersection. This document formalizes quantifier semantics for Limn.

---

## 1. The Problem of Quantification

### 1.1 Natural Language Quantifiers

English quantifiers bind variables over domains:
- "All humans are mortal" → ∀x(Human(x) → Mortal(x))
- "Some birds fly" → ∃x(Bird(x) ∧ Fly(x))

This presupposes:
1. Individual entities
2. Predication over individuals
3. Variable binding

### 1.2 Limn's Challenge

Limn has:
- **Constraint regions**, not individuals
- **Intersection**, not predication
- **No variables** to bind

How do quantifiers work in this framework?

---

## 2. Limn Quantifier Vocabulary

### 2.1 Core Quantifiers (from v3-natural)

| Word | Source | Meaning | Traditional Logic |
|------|--------|---------|-------------------|
| `all` | all | totality | ∀ (universal) |
| `non` | none | absence | ¬∃ (no existence) |
| `som` | some | partial | ∃ (existential) |
| `man` | many | large quantity | (no direct equivalent) |
| `few` | few | small quantity | (no direct equivalent) |
| `mos` | most | majority | (no direct equivalent) |
| `one` | one | singularity | ∃! (unique existence) |
| `eac` | each | distributive | ∀ (with distributivity) |

### 2.2 Proportional Quantifiers

| Word | Proportion | Example |
|------|------------|---------|
| `non` | 0% | no members |
| `few` | ~10-30% | minority |
| `som` | ~30-70% | some portion |
| `mos` | ~70-90% | majority |
| `all` | 100% | totality |

---

## 3. Quantifier Semantics: Proportion Operators

### 3.1 The Core Insight

In Limn, a quantifier Q applied to a constraint C modifies how C participates in intersection:

```
⟦Q C⟧ = proportion(Q) × ⟦C⟧
```

Where `proportion(Q)` defines what subset of the constraint region is "active."

### 3.2 Formal Definition

Let ⟦C⟧ be a constraint region in semantic space S.

**Full quantifier:**
```
⟦all C⟧ = ⟦C⟧
```
The entire region participates.

**Existential quantifier:**
```
⟦som C⟧ = {x ∈ ⟦C⟧ : x is contextually selected}
```
A non-empty subset participates.

**Universal negation:**
```
⟦non C⟧ = ∅ (when intersected with C)
         = S \ ⟦C⟧ (as standalone region)
```

**Proportional quantifiers:**
```
⟦few C⟧ = minority_subset(⟦C⟧)
⟦mos C⟧ = majority_subset(⟦C⟧)
⟦man C⟧ = large_subset(⟦C⟧)
```

### 3.3 Intersection Behavior

When quantified constraints intersect:

```
⟦Q₁ C₁ Q₂ C₂⟧ = ⟦Q₁ C₁⟧ ∩ ⟦Q₂ C₂⟧
```

**Example:**
```limn
som hum all mor
```
"Some humans, all mortal" = the intersection of some-humans with all-mortality = some mortal humans

---

## 4. Quantifier Scope

### 4.1 The Scope Problem

In "Every student read some book," scope matters:
- ∀s∃b (each student read possibly different books)
- ∃b∀s (one book that all students read)

### 4.2 Limn's Solution: Explicit Scope

Use the pipe operator `|` to mark scope boundaries:

```limn
eac stu | som boo rea
```
"Each student | some book read" = for each student, there's some book

```limn
som boo | eac stu rea
```
"Some book | each student read" = there's some book that each student read

### 4.3 Default Scope

Without explicit scope marking, Limn uses **surface scope** (left-to-right):

```limn
eac stu som boo rea
```
Default: each student scopes over some book.

---

## 5. Quantifier Interactions

### 5.1 Monotonicity

Quantifiers have **monotonicity** properties:

| Quantifier | Left Monotone | Right Monotone |
|------------|---------------|----------------|
| `all` | ↓ downward | ↑ upward |
| `som` | ↑ upward | ↑ upward |
| `non` | ↓ downward | ↓ downward |

**Inference patterns:**
```
all avi fli         # All birds fly
all avi mig         # ✓ All birds migrate (if flying → migrating)

som avi fli         # Some birds fly
som ani fli         # ✓ Some animals fly (birds ⊂ animals)
```

### 5.2 Conservativity

Natural language quantifiers are conservative:
- "Most birds fly" = "Most birds are birds that fly"

Limn preserves this:
```limn
mos avi fli = mos avi (avi fli)
```

### 5.3 Quantifier Negation

| Expression | Equivalent | Meaning |
|------------|------------|---------|
| `nu all C` | `som nu C` | Not all C = some not-C |
| `nu som C` | `all nu C` | Not some C = all not-C = none C |
| `nu non C` | `som C` | Not none C = some C |

**De Morgan for quantifiers:**
```
nu (all C) ≡ som (nu C)
nu (som C) ≡ non C
```

---

## 6. Distributivity and Collectivity

### 6.1 The Distinction

"The students lifted the piano"
- Collective: together, as a group
- Distributive: each lifted separately

### 6.2 Limn Markers

| Word | Reading | Example |
|------|---------|---------|
| `eac` | Distributive | `eac stu lif pia` = each student individually |
| `tog` | Collective | `tog stu lif pia` = students together |
| `all` | Ambiguous | `all stu lif pia` = either reading |

### 6.3 Formal Semantics

```
⟦eac C P⟧ = ∀x∈⟦C⟧: x ∈ ⟦P⟧  (distributive)
⟦tog C P⟧ = ⟦C⟧ ∈ ⟦P⟧         (collective, C as group)
```

---

## 7. Generalized Quantifiers

### 7.1 Beyond All/Some

Natural languages have complex quantifiers:
- "More than half"
- "At least three"
- "Between 5 and 10"

### 7.2 Limn Numeric Quantifiers

Combine quantity words with comparison operators:

| Expression | Meaning |
|------------|---------|
| `mor hal C` | More than half of C |
| `les hal C` | Less than half of C |
| `exa tri C` | Exactly three C |
| `min fiv C` | At least five C |
| `max ten C` | At most ten C |
| `bet fiv ten C` | Between 5 and 10 C |

### 7.3 Formal Definition

```
⟦mor hal C⟧ = {x : |{x} ∩ ⟦C⟧| > 0.5 × |⟦C⟧|}
⟦exa n C⟧ = {x : |{x} ∩ ⟦C⟧| = n}
```

---

## 8. Quantification and Keys

### 8.1 Key Restriction

Keys can restrict the domain of quantification:

```
Key: "Students in this class"
Sentence: all stu pas

Without key: All students (universal) passed
With key: All students in this class passed
```

The key narrows the quantifier's domain.

### 8.2 Contextual Quantifiers

Some quantifiers are inherently context-dependent:

| Quantifier | Depends On |
|------------|------------|
| `man` (many) | What counts as "many" in context |
| `few` (few) | What counts as "few" in context |
| `eno` (enough) | What threshold is "enough" |

Keys provide the threshold:
```
Key: "For a party, many = 20+"
Sentence: man gue arr

Reading: Many (20+) guests arrived
```

---

## 9. Existential Import

### 9.1 Classical Problem

"All unicorns have horns" - true or false if no unicorns exist?

- Aristotelian: Presupposes existence → neither true nor false
- Modern logic: Vacuously true (∀x(U(x)→H(x)) true when no U)

### 9.2 Limn's Position

Limn takes the **constraint region** view:

```limn
all uni hor
```

Defines the region: things that are both universal-unicorn-members and horned.

- If ⟦uni⟧ = ∅, then ⟦all uni hor⟧ = ∅
- The sentence isn't "true" or "false" - it defines an empty region

**Limn avoids the existential import problem** by not making truth claims about existence.

### 9.3 Explicit Existence

To assert existence, use explicit markers:

```limn
exi uni         # Unicorns exist (existence claim)
exi uni hor     # Horned unicorns exist
```

Where `exi` (exist) is an existence predicate.

---

## 10. Summary: Quantification as Proportion

### 10.1 The Limn Model

| Aspect | Natural Language | Limn |
|--------|------------------|------|
| What quantifiers do | Bind variables over domains | Modify constraint participation |
| Scope | Implicit, ambiguous | Explicit via `|` or default |
| Existence | Presupposed or vacuous | Separate assertion (`exi`) |
| Proportion | Binary (all/some) or vague | Explicit proportion words |

### 10.2 Key Innovations

1. **Quantifiers as proportion modifiers:** Not variable binders, but region selectors
2. **Explicit scope marking:** Pipe operator disambiguates
3. **Graded quantification:** `few < som < man < mos < all` as scale
4. **No existential import:** Constraint regions don't presuppose entities

### 10.3 The Quantifier Formula

```
⟦Q C⟧ = select(proportion(Q), ⟦C⟧)

Where:
- proportion(Q) ∈ [0, 1]
- select() returns that proportion of the region
- Intersection proceeds normally with selected region
```

---

## 11. Open Questions

### 11.1 For Future Work

1. **Quantifier adverbs:** How do `alw` (always), `nev` (never), `oft` (often) relate?
2. **Mass vs. count:** Does Limn distinguish "all water" from "all cups"?
3. **Plural logic:** How do plurals interact with quantification?
4. **Second-order quantification:** Can Limn quantify over properties?

### 11.2 Implementation Notes

For Limn-PL (programming extension):
- Quantifiers may need explicit iteration constructs
- Collection types should support quantified queries
- Solver engines need quantifier elimination strategies

---

## References

- `docs/spec/vocabulary-v3-natural.md` - Quantifier vocabulary
- `docs/spec/grammar-formal.md` - Operator precedence
- `docs/theory/superposition-semantics.md` - Constraint regions
- `crew/linguist/analysis/scope-delimiter-semantics.md` - Pipe operator

---

*qua = pro sel | all = tot | som = par | non = abs*
*(quantifier = proportion select | all = total | some = partial | none = absent)*

---

*— Dr. Maren Solvik*
