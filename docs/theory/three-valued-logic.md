# Three-Valued Logic in Limn

**Author:** Dr. Maren Solvik, Theoretical Linguist
**Date:** 2026-01-31
**Status:** Formal Theory Document
**Version:** 1.0

---

## Abstract

Classical logic operates with two truth values: true and false. Limn's constraint-based semantics naturally supports **three-valued logic**, where a third value represents undetermined, liminal, or paradoxical states. This document formalizes how three-valued logic integrates with Limn.

---

## 1. Why Three Values?

### 1.1 The Limitation of Bivalence

Classical two-valued logic cannot adequately handle:
- **Vagueness:** "Is a heap minus one grain still a heap?"
- **Presupposition failure:** "The king of France is bald"
- **Self-reference:** "This sentence is false"
- **Future contingents:** "There will be a sea battle tomorrow"
- **Liminal states:** "Is twilight light or dark?"

### 1.2 Limn's Natural Third Value

In Limn, constraint intersection can produce three outcomes:

| Outcome | Interpretation | Example |
|---------|----------------|---------|
| Non-empty region | Defined meaning | `sol aqu` → ice |
| Empty region | Impossible/contradictory | `sol nu sol` → ∅ |
| **Liminal region** | **Boundary zone** | `lux nox` → twilight |

The liminal region is neither fully in ⟦lux⟧ nor fully in ⟦nox⟧—it's the **boundary between** them.

---

## 2. The Three Truth Values

### 2.1 Definition

| Value | Symbol | Meaning | Limn Expression |
|-------|--------|---------|-----------------|
| True | **T** | Clearly within constraint | Deep inside region |
| False | **F** | Clearly outside constraint | Outside region |
| Liminal | **L** | On boundary / indeterminate | At region edge |

### 2.2 Geometric Interpretation

In constraint-surface geometry:

```
          ⟦C⟧
    ┌─────────────┐
    │             │
    │    TRUE     │   <- Interior of region
    │             │
    ├─────────────┤   <- LIMINAL (boundary)
    │             │
    │    FALSE    │   <- Exterior of region
    │             │
    └─────────────┘
```

The liminal zone is the **transition band** between constraint regions.

### 2.3 Limn Markers

| Marker | Meaning | Usage |
|--------|---------|-------|
| `cle` | Clearly (T) | `cle sol` = definitely solid |
| `bet` | Between (L) | `sol liq bet` = liminal solid-liquid |
| `nu` | Not (toward F) | `nu sol` = not solid |
| `lim` | Liminal | `lim sol liq` = boundary zone |

---

## 3. Three-Valued Truth Tables

### 3.1 Negation (`nu`)

| A | `nu A` |
|---|--------|
| T | F |
| L | L |
| F | T |

**Interpretation:** Negating liminal yields liminal. The boundary of a boundary is still a boundary.

### 3.2 Conjunction (Intersection / Space)

| A | B | `A B` |
|---|---|-------|
| T | T | T |
| T | L | L |
| T | F | F |
| L | T | L |
| L | L | L |
| L | F | F |
| F | T | F |
| F | L | F |
| F | F | F |

**Interpretation:** Intersection with liminal yields liminal (unless clearly false). The "minimum" of truth values.

### 3.3 Disjunction (Would be Union)

| A | B | `A or B` |
|---|---|----------|
| T | T | T |
| T | L | T |
| T | F | T |
| L | T | T |
| L | L | L |
| L | F | L |
| F | T | T |
| F | L | L |
| F | F | F |

**Note:** Limn doesn't have explicit disjunction. This table shows theoretical behavior.

### 3.4 Implication (`→`)

| A | B | `A → B` |
|---|---|---------|
| T | T | T |
| T | L | L |
| T | F | F |
| L | T | T |
| L | L | L |
| L | F | L |
| F | T | T |
| F | L | T |
| F | F | T |

**Interpretation:** The sequence operator `→` inherits three-valued behavior. Liminal antecedent → liminal consequent = liminal causation.

---

## 4. Liminal Semantics

### 4.1 What Creates Liminality?

| Source | Example | Result |
|--------|---------|--------|
| **Antonym intersection** | `lux nox` | Twilight zone |
| **Boundary concepts** | `beg end bet` | Transition |
| **Gradable predicates** | `hot col` | Lukewarm |
| **Vague reference** | `som man` | How many? |
| **Self-reference** | `tru fal bet` | Paradox |

### 4.2 Liminal Resolution

Liminality can be resolved by:

1. **Key specification:**
   ```
   Key: "sunrise"
   lux nox → L → "dawn" (resolved to specific liminal state)
   ```

2. **Intensifier collapse:**
   ```
   ve lux nox → (ve lux) nox → bright darkness → poetic but defined
   ```

3. **Explicit choice:**
   ```
   lux nox | cho lux → chose light (resolved)
   ```

### 4.3 Persistent Liminality

Some expressions are **essentially liminal**—they cannot be resolved to T or F:

```limn
lif dea bet     # The boundary between life and death
beg end sim     # Simultaneous beginning and ending
sel oth         # Self that is other
```

These express liminal concepts that resist collapse.

---

## 5. Relation to Superposition

### 5.1 Superposition vs. Liminality

| Property | Superposition | Liminality |
|----------|---------------|------------|
| Nature | Multiple valid meanings | Boundary state |
| Resolution | Key collapses | May not resolve |
| Logic | Multiple truths | Third truth value |
| Goal | Information compression | Boundary expression |

### 5.2 Interaction

A sentence can be both superposed AND liminal:

```limn
lux nox lif
```

- **Superposed:** Multiple interpretations (dawn creature, fading life, hope in darkness)
- **Liminal:** All interpretations exist at the light/dark boundary

Key collapse reduces superposition but may preserve liminality:
```
Key: "firefly"
lux nox lif → firefly (specific) → still liminal (creature of twilight)
```

---

## 6. Three-Valued Logic in Limn-PL

### 6.1 Type System

```
type Bool3 = True | False | Liminal

lux : Region
value_in_region : Entity → Region → Bool3
```

### 6.2 Evaluation

```
evaluate(entity, constraint) =
  if clearly_inside(entity, constraint) then True
  else if clearly_outside(entity, constraint) then False
  else Liminal
```

### 6.3 Constraint Satisfaction

```
solve(constraints) =
  solutions where all constraints are True OR Liminal

  -- Liminal solutions are "possible but uncertain"
```

---

## 7. Comparison to Other Three-Valued Systems

### 7.1 Łukasiewicz Logic (Ł3)

- Third value: "possible" / "indeterminate"
- Used for: Future contingents
- **Limn parallel:** Liminal = possible state, not yet determined

### 7.2 Kleene Logic (K3)

- Third value: "unknown"
- Used for: Partial functions, undefined values
- **Limn parallel:** Liminal when key insufficient to determine

### 7.3 Priest's Logic of Paradox (LP)

- Third value: "both true and false"
- Used for: Paraconsistent reasoning
- **Limn parallel:** `tru fal bet` = paradox region

### 7.4 Limn's Innovation

Limn's third value is **geometrically grounded**: it represents the literal boundary between constraint regions, not just an epistemic or logical device.

---

## 8. Practical Applications

### 8.1 Expressing Transitions

```limn
sol → liq       # Solid leads to liquid (melting)
sol liq bet     # In the melting state (liminal)
```

The liminal value captures the **process** of change.

### 8.2 Handling Vagueness

```limn
big sma bet     # Neither clearly big nor clearly small
old you bet     # Middle-aged
hot col bet     # Lukewarm
```

### 8.3 Paradox and Contradiction

```limn
tru fal bet     # Liminal truth/falsity (paradox)
sel nu sel      # Self that isn't self
exi nu exi      # Existing non-existence
```

These don't crash the system—they define liminal regions.

### 8.4 Uncertainty Expression

```limn
cer unc bet     # Between certain and uncertain
kno nu kno bet  # Partial knowledge
```

---

## 9. Operators for Three-Valued Logic

### 9.1 New Operators

| Operator | Meaning | Example |
|----------|---------|---------|
| `cle` | Clearly (force T or F) | `cle sol` = definitely solid |
| `lim` | Liminal (force L) | `lim sol liq` = melting |
| `bet` | Between (creates L) | `hot col bet` = lukewarm |
| `res` | Resolve (collapse L) | `res lim hot col` = choose one |

### 9.2 Truth Testing

```limn
te cle sol      # Is it clearly solid? (yes/no)
te lim sol liq  # Is it liminal? (yes/no)
```

### 9.3 Default Assumptions

```limn
def T sol       # Assume solid unless contradicted
def F liq       # Assume not liquid unless proven
def L unk       # Assume liminal for unknown
```

---

## 10. Summary

### 10.1 The Three-Valued Framework

| Value | Symbol | Region Location | Example |
|-------|--------|-----------------|---------|
| **True** | T | Interior | `ve sol` |
| **Liminal** | L | Boundary | `sol liq bet` |
| **False** | F | Exterior | `nu sol` |

### 10.2 Key Principles

1. **Geometric grounding:** Third value = constraint boundary
2. **Natural emergence:** Liminal arises from antonym intersection
3. **Coherent logic:** Three-valued truth tables are consistent
4. **Expressiveness:** Limn can say what two-valued logic cannot
5. **Resolution optional:** Some liminality is permanent

### 10.3 The Liminal Formula

```
⟦A B⟧ =
  T if interior(⟦A⟧) ∩ interior(⟦B⟧) ≠ ∅
  L if boundary(⟦A⟧) ∩ boundary(⟦B⟧) ≠ ∅
  F if ⟦A⟧ ∩ ⟦B⟧ = ∅
```

---

## References

- `docs/theory/liminal-semantics.md` - Boundary resolution
- `docs/theory/superposition-semantics.md` - Multiple meanings
- `docs/theory/constraint-surfaces.md` - Geometric model
- `docs/spec/grammar-formal.md` - Operator precedence

---

*tri val | tru fal lim | bou = thi*
*(three values | true false liminal | boundary = third)*

---

*— Dr. Maren Solvik*
