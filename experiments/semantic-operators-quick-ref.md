# Semantic Operators: Quick Reference

**Proposed Semantic Operations for Limn**

---

## The Three Operations

### 1. `without` - Semantic Subtraction

**Concept:** Remove a semantic component from a concept

```limn
kin without man          > royal essence
lov without sel          > unconditional love
int without err          > pure reasoning
```

**Why:**
- Aligns with Limn's hybrid philosophy (semantics use words)
- No ambiguity
- Natural English equivalent
- Already proven in natural language ("love without fear")

**Alternatives considered:**
- `÷` - too mathematical
- `\` - too overloaded in programming

---

### 2. `±` - Superposition

**Concept:** Two concepts in quantum superposition (unresolved both/and)

```limn
yes ± no                 > unresolved yes-and-no
hop ± des                > hope and despair simultaneously
lit ± met                > literal and metaphorical meaning
```

**Why:**
- Strong quantum mechanics association
- Mathematical precision (both values at once)
- Efficient symbol
- Directly maps to epistemic uncertainty

**Alternatives considered:**
- `⊗` - conflicts with context product operator
- `≈` - could be confused with mathematical approximation

---

### 3. `given` - Conditional

**Concept:** A concept in the context of another (conditional dependency)

```limn
lov given tru            > love conditioned on trust
jus given law            > justice as legal concept
par given con            > parenting as expression of concern
```

**Why:**
- Exact English equivalent
- Perfect semantic clarity
- Aligns with Limn's hybrid philosophy
- No overloading

**Alternatives considered:**
- `/` - too overloaded (paths, division, set theory)
- `:` - too overloaded (type theory, lists, time)

---

## Usage Patterns

### Individual Use

```limn
kin without man          > just the regal aspect
hop ± des               > both hope and despair
lov given tru           > love dependent on trust
```

### Combined (All Three)

```limn
(lov without sel) ± (pas without pre) given tru

Reading: Love without selfishness is superposed with
         passion without prejudice, both conditioned on truth
```

### With Existing Operators

```limn
~ [und (lov without sel) ± hop given sup] @ psy
> Delegate understanding of (selfless love superposed with
> hope conditioned on support) in psychology context

∎ [eve] → tru given evi → dec
> Ground event, then conditional truth, then decide

∿was [(hop ± des) without pre] ⊕ ∿now [cou given mor]
> Past superposed hope/despair with prejudice removed,
> merged with current courage conditioned on morality
```

---

## When to Use Each

### `without` - Use when:
- Removing an unwanted quality or aspect
- Refining a concept to its essence
- Creating semantic purity
- Examples: "bravery without cowardice", "knowledge without doubt"

### `±` - Use when:
- Concepts coexist unresolved
- Quantum/epistemic uncertainty matters
- Both alternatives are simultaneously valid
- Examples: "yes and no", "hope and fear", "literal and metaphorical"

### `given` - Use when:
- Concepts depend on context
- Conditional logic matters
- Meaning varies by situation
- Examples: "truth given evidence", "meaning given culture"

---

## Composition Rules

**Precedence (highest to lowest):**
1. `without`, `±`, `given` (semantic operations) - left-to-right
2. Other operators (`~`, `@`, `→`, etc.)

**Valid combinations:**
- `X without Y → Z` ✓ (subtract then sequence)
- `~ [X without Y]` ✓ (delegate subtraction)
- `(X ± Y) @ ctx` ✓ (superpose then focus)
- `X given Y → Z` ✓ (condition then sequence)

**Invalid combinations:**
- Semantic operators with wrong types (caught by parser)

---

## Examples by Domain

### Psychology
```limn
lov without fea ± pas without con given mor
> Love without fear, superposed with passion without control,
> conditioned on morality
```

### Knowledge Representation
```limn
fac given eve → con without pre
> Facts given evidence, leading to conclusion without prejudice
```

### Narrative
```limn
∿was [hop without que] ⊕ ∿now [tru given tim]
> Past hope-without-questioning merged with present
> truth-conditioned-on-time
```

### Philosophy
```limn
exs ± non_exs given per
> Existence superposed with non-existence,
> conditioned on perspective
```

---

## Implementation Status

**Phase 1: Adoption**
- [x] Tested 3 alternatives for each operation
- [x] Selected optimal candidates
- [x] Validated composition with existing operators
- [ ] Dr. Solvik approval

**Phase 2: Integration**
- [ ] Update operators specification
- [ ] Add to vocabulary database
- [ ] Parser implementation
- [ ] Type checking rules

**Phase 3: Documentation**
- [ ] User guide
- [ ] Developer guide
- [ ] 20+ usage examples
- [ ] Tutorial

---

## Design Principles

These operators preserve Limn's core philosophy:

**Hybrid Architecture:**
- **Symbols** (execution): `~`, `∎`, `∿`, `@`, `→`, `⊕`, `⊗`, `⊂`
- **Words** (semantics): `without`, `given`
- **Math** (precise): `±`

**This preserves:**
- Clarity (words are unambiguous)
- Efficiency (symbols where needed)
- Composability (work with all operators)
- Psychological grounding (map to human concepts)

---

## Quick Syntax

```
X without Y     # X with Y-aspect removed
X ± Y          # X and Y unresolved
X given Y      # X dependent on Y

Compose:
(X without Y) ± (Z given W)
A → (B without C)
~ [D given E] @ ctx
```

---

## For Developers

**Lexer tokens:**
```
WITHOUT  : "without"
PLUS_MINUS : "±" or "+/-"
GIVEN    : "given"
```

**Parser rules:**
```
semantic_expr : term "without" term
              | term "±" term
              | term "given" term
```

**Type system:**
```
without : Term × Term → Term
± : Term × Term → Term
given : Term × Term → Term
```

---

*Quick reference for Limn semantic operators*
*Status: Ready for specification approval*
