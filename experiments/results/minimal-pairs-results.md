# Minimal Pairs Experiment Results

**Date:** 2026-01-24
**Test Battery:** Selected pairs from crew/linguist/analysis/minimal-pairs.md
**Method:** LLM interpretation via subagents

---

## Test G1: Commutativity (6 Permutations)

### Sentences Tested
```
1. lu vi ra
2. vi lu ra
3. ra lu vi
4. ra vi lu
5. vi ra lu
6. lu ra vi
```

### Prediction
All 6 sentences should produce identical interpretation sets (intersection is commutative).

### Result: CONFIRMED

All permutations produced semantically equivalent interpretations:
- "Bright and alive and extended things"
- "Visible, animate, linear entities"
- "Luminous, living, stretched-out phenomena"

**Key Finding:** LLM correctly recognized that word order does not affect meaning when all words are content words (no operators).

**Mathematical Basis:**
```
lu ∩ vi ∩ ra = vi ∩ ra ∩ lu = ra ∩ lu ∩ vi = ... (all 6 equivalent)
```

---

## Test D2: Negation Scope

### Sentences Tested
```
1. nu lu vi = (NOT bright) AND alive
2. lu nu vi = bright AND (NOT alive)
```

### Prediction
These should produce DIFFERENT interpretations because `nu` scopes only over the following word.

### Result: CONFIRMED

**Sentence 1 (NOT bright, alive):**
- Dark, living creature
- Invisible but animate being
- Concealed animal
- Living thing that produces no light
- Dim organism

**Sentence 2 (bright, NOT alive):**
- Glowing, inanimate object
- Luminous, non-living thing
- Bright corpse
- Shining, lifeless artifact
- Radiant object with no animation

**Key Finding:** Operator position correctly determines scope. The interpretations are "semantic opposites" - one negates brightness, the other negates life.

**Implication:** Word order DOES matter when operators are involved, because operators bind to their immediate neighbor before commutativity applies.

---

## Test I3: Contradiction Handling

### Sentences Tested
```
1. vi = alive
2. vi dea = alive AND dead
```

### Prediction
Either:
- Empty set (logical contradiction)
- Creative liminal interpretations (boundary region)

### Result: CREATIVE RESOLUTION

**Sentence 1 (alive):**
- Newborn creature
- Thriving organism
- Consciousness itself
- Heartbeat/pulse
- Seedling emerging

**Sentence 2 (alive AND dead):**
- Zombie/undead creature
- Dormant seed (alive in potential, dead in expression)
- Virus/prion (philosophically ambiguous life status)
- Moment of death (transitional instant)
- Spiritual death with physical life

**Key Finding:** Limn does NOT produce empty sets for apparent contradictions. Instead, it finds liminal/boundary interpretations where both constraints partially hold.

**Theoretical Implication:** Constraint regions are fuzzy/graded, not binary. The intersection of `vi` and `dea` is small but non-empty, containing edge cases like undead, dormant, or philosophically ambiguous entities.

---

## Summary of Findings

| Test | Prediction | Result | Status |
|------|------------|--------|--------|
| G1: Commutativity | Same interpretations | Same interpretations | CONFIRMED |
| D2: Negation scope | Different interpretations | Different interpretations | CONFIRMED |
| I3: Contradiction | Empty or liminal | Liminal/creative | CONFIRMED |

---

## Theoretical Conclusions

### 1. Commutativity Holds
Word order independence is correctly implemented for content words. LLMs respect the constraint intersection semantics.

### 2. Operator Scope Is Local
The `nu` operator binds to its immediate neighbor. This creates a "chunk" that then participates in commutativity:
- `[nu-lu] vi` = `vi [nu-lu]` (equivalent)
- But `[nu-lu] vi` ≠ `lu [nu-vi]` (different)

### 3. Contradictions Resolve Creatively
Rather than producing logical errors or empty sets, apparent contradictions find boundary interpretations. This is a feature, not a bug - it enables expressive liminal meanings.

### 4. LLMs Can Learn Limn Semantics
With bootstrap exposure, LLMs correctly:
- Treat word combinations as intersections
- Respect order-independence for content words
- Apply operator scope rules
- Generate creative resolutions for edge cases

---

## Recommended Next Steps

1. **Test more operator pairs** (ve, so, quantifiers)
2. **Test key collapse consistency** (same sentence, different keys)
3. **Test longer sentences** (5+ words for scalability)
4. **Test across different LLMs** (consistency check)
5. **Formal verification** of operator scope rules

---

**END OF RESULTS**
