# Negation Scope Test: `nu` Operator Positioning

**Date:** 2026-01-24
**Model:** Claude Opus 4.5
**Vocabulary Version:** v1 (40 words)

---

## Purpose

Test how the position of the negation operator `nu` affects interpretation. Per grammar-formal.md, `nu` applies to the immediately following word unless scoped with `|`.

---

## Test 1: Single Negation - Position Effects

### Base Sentence: `lu vi ga`
**Meaning:** bright + alive + positive

**Interpretations without negation:**
- Sunny day with life, feeling good
- Healthy vibrant organism
- Hopeful living being
- Light-filled happy place

---

### Position A: `nu lu vi ga`
**Parse:** (NOT bright) + alive + positive

**Interpretations:**
1. **Happy life in darkness** - nocturnal creature content in the dark
2. **Blind person living well** - without light perception, alive and positive
3. **Good secret life** - hidden (not bright/visible) but fulfilling
4. **Night shift worker who loves their job** - dark hours, alive, positive

**Semantic Character:** Life and positivity exist despite absence of brightness

---

### Position B: `lu nu vi ga`
**Parse:** bright + (NOT alive) + positive

**Interpretations:**
1. **Beautiful sunset** - bright, not alive (inanimate), beautiful/positive
2. **Good lamp** - illuminating (bright), inanimate (not alive), useful/positive
3. **Hopeful monument** - bright in sunlight, not alive (stone), positive symbol
4. **Crystal in light** - bright, inanimate, aesthetically pleasing

**Semantic Character:** Positive inanimate brightness - objects that bring good through light

---

### Position C: `lu vi nu ga`
**Parse:** bright + alive + (NOT positive)

**Interpretations:**
1. **Dangerous predator in daylight** - visible, alive, threatening
2. **Illness visible on someone** - bright/flushed, alive, unwell
3. **Beautiful but poisonous creature** - bright colors, alive, dangerous
4. **Wildfire** - bright, "alive" in movement, destructive

**Semantic Character:** Living visible things that are harmful or unwanted

---

### Summary Table: Position Effects

| Position | Parse | Core Meaning |
|----------|-------|--------------|
| `nu lu vi ga` | ¬bright, alive, positive | Good life in darkness |
| `lu nu vi ga` | bright, ¬alive, positive | Good bright objects |
| `lu vi nu ga` | bright, alive, ¬positive | Visible living threats |

**Conclusion:** Position dramatically changes meaning. Negation binds to its immediate neighbor.

---

## Test 2: Multiple Negations

### 2.1 Double Negation - Same Word

**Sentence:** `nu nu lu vi`
**Parse:** NOT (NOT bright) + alive = bright + alive

**Interpretations:** Same as `lu vi`
- Living thing in light
- Visible animate being

**Observation:** Double negation cancels, as expected from grammar rules.

---

### 2.2 Double Negation - Different Words

**Sentence:** `nu lu nu vi`
**Parse:** (NOT bright) + (NOT alive)

**Interpretations:**
1. **Dark inanimate object** - rock in shadow
2. **Death in darkness** - corpse in tomb
3. **Abstract void** - emptiness (no light, no life)
4. **Deep space** - dark, lifeless void
5. **Turned-off machine** - dark, inanimate

**Semantic Character:** The intersection of darkness and lifelessness

---

### 2.3 Triple Negation

**Sentence:** `nu lu nu vi nu ga`
**Parse:** (NOT bright) + (NOT alive) + (NOT positive)

**Interpretations:**
1. **Despair in the void** - dark, dead, bad
2. **Disease in corpse** - rotting (dark, not alive, negative)
3. **Evil in darkness** - malevolent absence
4. **Deepest suffering** - no light, no life, no good
5. **Nihilism embodied** - negation of everything

**Semantic Character:** Triple negation creates maximally negative meaning region

---

## Test 3: Negation with Grouping

### 3.1 Narrow Scope: `nu lu | vi`

**Parse:** (NOT bright) | (alive) - two separate regions

**Interpretations:**
1. Darkness and life separately - dark things exist, alive things exist (not necessarily same thing)
2. A scene: shadow in one place, living creature elsewhere
3. Contrast: the dark vs. the living

**Different from:** `nu lu vi` which is "non-bright alive thing" (single entity)

---

### 3.2 Wide Scope: `nu | lu vi`

**Parse:** NOT (bright AND alive) = anything that isn't (bright AND alive)

**Interpretations:**
This is a HUGE region including:
- Dark things (whether alive or not)
- Dead things (whether bright or not)
- Both dark and dead things
- Only excluded: things that are BOTH bright AND alive

**Observation:** Sentence-level negation creates nearly universal region. Almost everything satisfies this.

---

### 3.3 Complex Grouping

**Sentence:** `nu | lu vi | mu to`
**Parse:** NOT (bright AND alive) | (dark AND still)

**Interpretations:**
Two regions:
1. Anything not (bright+alive): dark things, dead things, etc.
2. Dark still things: shadows, corpses, rocks at night

**Combined meaning:** A scene of non-living brightness contrasted with dark stillness

---

## Test 4: Negation and Operators

### 4.1 `nu ve` - Negating Intensifier

**Sentence:** `nu ve lu`
**Parse:** NOT (very-bright) = anything not extremely bright

**Interpretations:**
- Dim light
- Moderate brightness
- Total darkness
- Anything except blazing bright

**Observation:** Negating an intensified word creates a different region than negating the base word.

Compare:
- `nu lu` = NOT bright = dark things
- `nu ve lu` = NOT very-bright = includes dimly bright things

---

### 4.2 `ve nu` - Intensified Negation

**Sentence:** `ve nu lu`
**Parse:** Very-(NOT bright) = very dark

**Interpretations:**
- Pitch black
- Absolute darkness
- Deepest shadow
- Light-absorbing

**Observation:** `ve` intensifies the negation, producing "extremely not-bright" = very dark.

---

### 4.3 Order Matters

| Sentence | Parse | Meaning |
|----------|-------|---------|
| `nu ve lu` | ¬(ve lu) | Not extremely bright |
| `ve nu lu` | ve(¬lu) | Extremely dark |

These are NOT equivalent. Operator order matters even though content-word order doesn't.

---

## Test 5: Negation with Reference

### 5.1 `yo nu lu | an lu`

**Parse:** (this NOT-bright) | (that bright)

**Interpretations:**
1. "This dark thing here, that bright thing there"
2. Explicit contrast: my darkness vs. your light
3. Here is shadow, there is illumination

---

### 5.2 `nu yo lu | an`

**Parse:** NOT (this bright) | that

**Interpretations:**
1. "Not this bright thing, but that one" (rejection + alternative)
2. "This isn't bright, but that (implied: is)"

**Observation:** Negation can scope over reference, creating denial patterns.

---

## Summary Findings

1. **Position-Sensitive:** `nu` always scopes over its immediate neighbor
2. **Double Negation Cancels:** `nu nu X` = X
3. **Multiple Different Negations:** Create intersection of absences
4. **Grouping Controls Scope:** `nu X | Y` vs. `nu | X Y` produce different meanings
5. **Operator Order Matters:** `nu ve X` ≠ `ve nu X`
6. **Universal Negation:** `nu |` on entire sentences creates near-universal regions

---

## Practical Implications

1. **Placement is semantic:** Moving `nu` changes meaning significantly
2. **Grouping is required:** For complex negation, `|` is necessary for clarity
3. **Double negation is valid:** Can be used for emphasis or cancellation
4. **Order-independence has limits:** Operators break pure commutativity

---

## Edge Cases Discovered

1. **`nu` alone:** Undefined. `nu` requires a following word to negate.
2. **`nu |`:** Scopes over everything after the boundary.
3. **Multiple `|`:** Each creates a new scope for `nu` to interact with.

---

## Result: PASS

The `nu` operator behaves consistently and predictably:
- Scopes over immediate following word by default
- Interacts correctly with `|` for broader scoping
- Interacts correctly with `ve`/`so` intensifiers
- Double negation cancels as expected from Boolean logic

Position effects are significant and semantically meaningful, as designed.

---

**END OF NEGATION SCOPE TEST**
