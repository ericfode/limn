# Operator Interaction Analysis in Limn

**Author:** Dr. Maren Solvik, Theoretical Linguist
**Date:** 2026-01-31
**Status:** Formal Theory Document
**Version:** 1.0

---

## Abstract

Limn's grammar defines multiple operator classes: unary modifiers, quantifiers, references, comparators, and the sequence operator. This document analyzes how these operators interact when combined, identifying legal compositions, semantic effects, and edge cases.

---

## 1. Operator Taxonomy

### 1.1 Operator Classes

| Class | Operators | Type Signature | Scope |
|-------|-----------|----------------|-------|
| **Unary Modifiers** | nu, ve, so, te, we | Region → Region | Next term |
| **Quantifiers** | al, ex, on | Region → Region | Next term |
| **References** | yo, an, sa | Region → Ref(Region) | Next term |
| **Comparators** | mi, ma, eq | Num × Num → Bool | Infix |
| **Sequence** | → | Region × Region → Sequence | Infix |
| **Scope** | \| | Region × Region → Pair | Infix |

### 1.2 Precedence Hierarchy

From highest (binds tightest) to lowest:
1. Unary modifiers (nu, ve, so, te, we) - right-to-left
2. Quantifiers (al, ex, on) - right-to-left
3. References (yo, an, sa) - right-to-left
4. Intersection (implicit space) - commutative
5. Sequence (→) - left-to-right
6. Scope (\|) - left-to-right

---

## 2. Unary Modifier Interactions

### 2.1 Modifier Stacking

Unary modifiers compose right-to-left:

| Expression | Parse | Semantic Effect |
|------------|-------|-----------------|
| `nu ve X` | nu(ve(X)) | Complement of core(X) |
| `ve nu X` | ve(nu(X)) | Core of complement(X) |
| `nu nu X` | nu(nu(X)) | X (double negation) |
| `ve ve X` | ve(ve(X)) | Core of core(X) - ultra-prototype |
| `so so X` | so(so(X)) | Expand of expand(X) - ultra-periphery |
| `ve so X` | ve(so(X)) | Core of expanded X |
| `so ve X` | so(ve(X)) | Expand of core X |

### 2.2 Semantic Commutativity Analysis

**Non-commutative pairs** (order matters):

```
nu ve X ≠ ve nu X
  nu ve X = S \ core(X) = everything except prototypical X
  ve nu X = core(S \ X) = prototypical non-X

  Example: nu ve bri = not very bright (dim, dark, or liminal)
           ve nu bri = very not-bright (prototypically dark)
```

**Commutative pairs** (order doesn't matter semantically but differs in region size):

```
ve so X = so ve X?
  ve(so(X)) = core(expand(X)) ≈ X (back to baseline)
  so(ve(X)) = expand(core(X)) ≈ X (back to baseline)
  These may collapse to approximately X, but via different paths
```

### 2.3 Idempotency and Fixed Points

| Pattern | Result | Note |
|---------|--------|------|
| `nu nu X` | X | Involution |
| `ve ve X` | core(core(X)) | Shrinking |
| `so so X` | expand(expand(X)) | Growing |
| `te te X` | te(X) | Question about question ≈ question |
| `we we X` | we(X) | Command to command ≈ command |

### 2.4 Question and Imperative Interactions

The operators `te` (question) and `we` (imperative) are pragmatic, not purely semantic:

| Expression | Parse | Interpretation |
|------------|-------|----------------|
| `te nu X` | te(nu(X)) | "Is it not-X?" |
| `nu te X` | nu(te(X)) | "Not questioning X" = asserting X |
| `te we X` | te(we(X)) | "Asking about command" = requesting |
| `we te X` | we(te(X)) | "Commanding a question" = demand answer |
| `te te X` | te(te(X)) | Meta-question about X |

---

## 3. Quantifier Interactions

### 3.1 Quantifier Stacking

Stacking quantifiers creates nested scope:

| Expression | Parse | Meaning |
|------------|-------|---------|
| `al al X` | al(al(X)) | "All of all X" = al X (redundant) |
| `ex al X` | ex(al(X)) | "Some of all X" = ex X |
| `al ex X` | al(ex(X)) | "All of some X" = some X exists for all |
| `on ex X` | on(ex(X)) | "Exactly one of some X" |

### 3.2 Quantifier + Unary Modifier

The order determines what is quantified vs. what is modified:

| Expression | Parse | Meaning |
|------------|-------|---------|
| `al nu X` | al(nu(X)) | "All non-X" |
| `nu al X` | nu(al(X)) | "Not all X" = "some not-X" |
| `al ve X` | al(ve(X)) | "All very-X" = all prototypical X |
| `ve al X` | ve(al(X)) | "Very all X" = intensified totality (odd) |
| `ex nu X` | ex(nu(X)) | "Some non-X" = "not none X" |
| `nu ex X` | nu(ex(X)) | "Not some X" = "no X" |

### 3.3 De Morgan Laws for Quantifiers

Classical quantifier duality holds:

```
nu (al X) ≡ ex (nu X)    # Not all = some not
nu (ex X) ≡ al (nu X)    # Not some = all not = none
nu (on X) ≡ (nu on) X    # Not exactly one = zero or more than one
```

### 3.4 Quantifier + Intensifier Interactions

| Expression | Equivalent | Semantic |
|------------|------------|----------|
| `al ve X` | - | All of the very-X (smaller subset, all of it) |
| `ve al X` | - | Very-all-X (unusual; intensifying totality) |
| `mos ve X` | - | Most of the very-X |
| `ve mos X` | - | Very-most-X (unusual semantics) |

**Analysis:** Quantifiers naturally take scope over modifiers (`al ve X`), while the reverse (`ve al X`) is pragmatically odd in natural language. Limn allows both syntactically.

---

## 4. Reference Interactions

### 4.1 Reference + Unary Modifier

| Expression | Parse | Meaning |
|------------|-------|---------|
| `yo nu X` | yo(nu(X)) | "This non-X" |
| `nu yo X` | nu(yo(X)) | "Not this X" = "other X" or "non-proximal X" |
| `yo ve X` | yo(ve(X)) | "This very-X" = proximal prototype |
| `ve yo X` | ve(yo(X)) | "Very this-X" (unusual) |

### 4.2 Reference + Quantifier

| Expression | Parse | Meaning |
|------------|-------|---------|
| `al yo X` | al(yo(X)) | "All of this X" |
| `yo al X` | yo(al(X)) | "This all-X" (referring to totality) |
| `ex an X` | ex(an(X)) | "Some of that X" |
| `an ex X` | an(ex(X)) | "That some-X" (referring to a portion) |

### 4.3 Reference Stacking

| Expression | Parse | Interpretation |
|------------|-------|----------------|
| `yo yo X` | yo(yo(X)) | Emphatic proximal - "this very X" |
| `an an X` | an(an(X)) | Emphatic distal - "that distant X" |
| `yo an X` | yo(an(X)) | "This that-X" = referential paradox |
| `an yo X` | an(yo(X)) | "That this-X" = referential paradox |
| `sa yo X` | sa(yo(X)) | "Same as this X" |
| `yo sa X` | yo(sa(X)) | "This same-X" |

---

## 5. Sequence Operator Interactions

### 5.1 Sequence with Modifiers

The sequence operator (→) breaks commutativity intentionally:

| Expression | Parse | Meaning |
|------------|-------|---------|
| `A → nu B` | A → nu(B) | A leads to not-B |
| `nu A → B` | nu(A) → B | Not-A leads to B |
| `nu (A → B)` | nu(A → B) | Negation of causality |
| `ve A → B` | ve(A) → B | Very-A leads to B |
| `A → ve B` | A → ve(B) | A leads to very-B |

### 5.2 Sequence with Quantifiers

| Expression | Parse | Meaning |
|------------|-------|---------|
| `al A → B` | al(A) → B | "All A leads to B" |
| `A → al B` | A → al(B) | "A leads to all B" |
| `ex A → ex B` | ex(A) → ex(B) | "Some A leads to some B" |
| `al (A → B)` | al(A → B) | "For all: A leads to B" (universal causation) |

### 5.3 Sequence Chains

| Expression | Parse | Associativity |
|------------|-------|---------------|
| `A → B → C` | (A → B) → C | Left-to-right |
| `A → (B → C)` | A → (B → C) | Explicit grouping |

**Semantic difference:**
- `(A → B) → C`: The (A-to-B process) leads to C
- `A → (B → C)`: A leads to (B-to-C process)

---

## 6. Scope Operator Interactions

### 6.1 Scope with Modifiers

| Expression | Parse | Meaning |
|------------|-------|---------|
| `nu A \| B` | nu(A) \| B | Topic: not-A, Comment: B |
| `A \| nu B` | A \| nu(B) | Topic: A, Comment: not-B |
| `nu (A \| B)` | nu(A \| B) | Negation of topic-comment structure |

### 6.2 Scope with Quantifiers

This is where explicit scope marking resolves ambiguity:

| Expression | Parse | Meaning |
|------------|-------|---------|
| `al A \| ex B` | al(A) \| ex(B) | Topic: all-A, Comment: some-B |
| `al (A \| B)` | al(A \| B) | For all: (A is topic, B is comment) |
| `(al A) \| B` | (al A) \| B | Topic: all-A, Comment: B (standard) |

---

## 7. Complex Interaction Patterns

### 7.1 Triple Stacking

| Expression | Parse | Analysis |
|------------|-------|----------|
| `nu ve so X` | nu(ve(so(X))) | Not very-somewhat-X |
| `al ex on X` | al(ex(on(X))) | All of some of exactly-one-X |
| `yo nu ve X` | yo(nu(ve(X))) | This not-very-X |
| `te we nu X` | te(we(nu(X))) | Asking about commanding not-X |

### 7.2 Mixed Class Interactions

| Expression | Parse | Type Check | Valid? |
|------------|-------|------------|--------|
| `nu al ve X` | nu(al(ve(X))) | Region → Region → Region → Region | Yes |
| `yo nu ex X` | yo(nu(ex(X))) | Region → Region → Region → Ref(Region) | Yes |
| `mi ve X` | mi(ve(X)) | - | Invalid: mi needs two numeric args |
| `al mi 5` | al(mi(5)) | - | Invalid: type mismatch |

---

## 8. Semantic Puzzles

### 8.1 The ve-nu Asymmetry

The pair `ve nu X` and `nu ve X` exhibit asymmetric semantics:

```
ve nu X = core of (not-X) = prototypical non-X
nu ve X = not (core of X) = anything except prototypical X
```

**Region size comparison:**
- `⟦ve nu X⟧` is small (core of complement)
- `⟦nu ve X⟧` is large (complement of core)

**Example with `bri` (bright):**
- `ve nu bri` = very dark (prototypical darkness)
- `nu ve bri` = not very bright (includes dim, liminal, and dark)

### 8.2 The Quantifier Collapse

When quantifiers are applied to modifiers:

```
al ve X = all of (very-X) = just the prototypes
ve al X = very (all-X) = ?
```

The second form `ve al X` is semantically odd because "very all" doesn't naturally intensify. Possible interpretations:
1. Emphatic totality ("absolutely all")
2. Core of the totality set (same as `al X`)
3. Marked as pragmatically anomalous

**Recommendation:** Allow syntactically but flag as stylistically unusual.

### 8.3 Double Negation in Complex Expressions

```
nu nu ve X = ve X (double negation eliminates)
nu ve nu X = ?
  = nu(ve(nu(X)))
  = nu(core(S \ X))
  = S \ core(S \ X)
  = everything except prototypical non-X
  = X plus liminal zone
```

This creates a larger region than X alone - the "generous X" interpretation.

---

## 9. Operator Interaction Laws

### 9.1 Proven Equivalences

| Law | Expression | Equivalent |
|-----|------------|------------|
| Double negation | `nu nu X` | `X` |
| Quantifier duality | `nu al X` | `ex nu X` |
| Quantifier duality | `nu ex X` | `al nu X` |
| De Morgan | `nu (A B)` | `(nu A) \| (nu B)` |
| Intensifier absorption | `ve ve X` | `ve X` (approximately) |
| Weakener absorption | `so so X` | `so X` (approximately) |

### 9.2 Non-Equivalences (Order Matters)

| Expression 1 | Expression 2 | Relationship |
|--------------|--------------|--------------|
| `nu ve X` | `ve nu X` | Different regions |
| `al ve X` | `ve al X` | Different scope |
| `yo nu X` | `nu yo X` | Different reference |
| `A → B` | `B → A` | Different causality |
| `A \| B` | `B \| A` | Different topic/comment |

---

## 10. Implementation Guidelines

### 10.1 Parser Requirements

1. Respect precedence: unary > quantifier > reference > intersection > sequence > scope
2. Right-to-left associativity for prefix operators
3. Left-to-right associativity for infix operators

### 10.2 Type Checking

Operators have type signatures that must be respected:

```
nu, ve, so, te, we : Region → Region
al, ex, on : Region → Region
yo, an, sa : Region → Ref(Region)
mi, ma, eq : Num × Num → Bool
→ : Region × Region → Sequence
| : Region × Region → Pair
```

### 10.3 Semantic Evaluation Order

For expression `op₁ op₂ ... opₙ X`:

1. Evaluate X to get ⟦X⟧
2. Apply opₙ: ⟦opₙ X⟧
3. Apply opₙ₋₁: ⟦opₙ₋₁ (opₙ X)⟧
4. Continue until op₁

---

## 11. Summary

### 11.1 Key Findings

1. **Order matters** for most operator combinations, especially:
   - `nu` vs. other modifiers
   - Quantifiers with modifiers
   - Any operator with sequence (→)

2. **Type constraints** prevent invalid combinations:
   - Comparators require numeric arguments
   - References produce Ref types

3. **Semantic puzzles** arise with:
   - Modifier stacking (`ve nu` vs. `nu ve`)
   - Quantifier scope over modifiers
   - Double negation in complex expressions

### 11.2 Recommendations for Writers

1. Use explicit grouping `(...)` when operator interaction is ambiguous
2. Prefer natural scope order: `al ve X` over `ve al X`
3. Be aware that `nu ve X` ≠ `ve nu X` semantically
4. Use scope marker `|` to disambiguate quantifier scope

---

## References

- `docs/spec/grammar-formal.md` - Operator precedence specification
- `docs/theory/quantifier-semantics.md` - Quantifier theory
- `docs/theory/liminal-semantics.md` - Negation and boundary semantics
- `docs/spec/vocabulary-v3-natural.md` - Operator vocabulary

---

*ope int = for sem | ord mat | typ gua*
*(operator interaction = formal semantics | order matters | type guards)*

---

*— Dr. Maren Solvik*
