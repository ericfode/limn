# Grouping Test: Scope Boundary Operator "|"

**Date:** 2026-01-24
**Model:** Claude Opus 4.5
**Vocabulary Version:** v1 (40 words)

---

## Purpose

Test whether the `|` operator correctly delimits scope boundaries, creating distinct meaning regions within a sentence.

---

## Test 1: Basic Grouping

### 1.1 Without Grouping

**Sentence:** `lu vi mu to`
**Constraints:** bright + alive + dark + still

**Analysis:** All four constraints intersect. Things that are bright AND alive AND dark AND still.

**Interpretations:**
1. Bioluminescent creature at rest - bright (lu), alive (vi), in darkness (mu), still (to)
2. Sleeping firefly - glows (lu), alive (vi), night (mu), motionless (to)
3. Deep sea fish with photophores, resting - luminous (lu), living (vi), deep dark (mu), stationary (to)
4. A dream about light - alive in mind (vi), experienced in sleep darkness (mu), stillness of sleep (to), seeing brightness (lu)

**Interpretation Count:** 4

---

### 1.2 With Grouping

**Sentence:** `lu vi | mu to`
**Constraints:** (bright + alive) | (dark + still)

**Analysis:** Two separate regions that coexist in the same sentence. Not intersection but juxtaposition.

**Interpretations:**
1. **Living thing beside dead thing** - a bright living creature (lu vi) next to a dark still object (mu to)
2. **Day and night** - the bright living day (lu vi) contrasted with dark silent night (mu to)
3. **Predator and prey** - alert bright predator (lu vi) and hidden still prey (mu to)
4. **Consciousness vs unconscious** - bright waking mind (lu vi) vs dark sleeping body (mu to)
5. **Fire dying** - the flame alive and bright (lu vi), becoming dark and still (mu to)
6. **Sun and shadow** - illuminated living area (lu vi), dark still shadow (mu to)

**Interpretation Count:** 6

**Key Difference:** With `|`, interpretations involve *two entities* or *two states*, not one entity with all four properties.

---

## Test 2: Negation Scope

### 2.1 `nu` Without Grouping

**Sentence:** `nu lu vi mu`
**Parse:** (NOT bright) AND alive AND dark

**Interpretations:**
1. Living thing in darkness - not bright (nu lu), alive (vi), dark (mu)
2. Creature in a cave - non-luminous (nu lu), living (vi), in darkness (mu)
3. Deep sea creature (non-bioluminescent) - no glow (nu lu), alive (vi), deep dark (mu)
4. Nocturnal animal at night - doesn't shine (nu lu), alive (vi), in darkness (mu)

---

### 2.2 `nu |` Full-Sentence Negation

**Sentence:** `nu | lu vi mu`
**Parse:** NOT (bright AND alive AND dark)

**Interpretations:**
Things that fail to be (bright AND alive AND dark) simultaneously:
1. A lamp - bright but not alive
2. A rock in sunlight - bright but not alive, not dark
3. A dead thing in darkness - dark but not alive
4. Any alive thing in light - alive but not dark
5. Almost everything - very few things are all three

**Observation:** Full-sentence negation creates a HUGE region (almost everything). This is the complement of a narrow intersection.

---

### 2.3 Nested Grouping with Negation

**Sentence:** `nu lu | vi mu`
**Parse:** (NOT bright) | (alive AND dark)

**Interpretations:**
1. **Two separate statements:** "something not bright" and separately "something alive and dark"
2. Could mean: darkness next to a living shadow-creature
3. Could mean: a non-bright thing and a dark living thing (possibly the same thing from two perspectives)

**Sentence:** `lu | nu vi | mu`
**Parse:** bright | (NOT alive) | dark

**Interpretations:**
1. Three separate regions: something bright, something not alive, something dark
2. A scene: light source, dead object, shadow
3. A gradient: light → death → darkness

---

## Test 3: Reference Scope

### 3.1 References with Grouping

**Sentence:** `yo lu vi | an mu to`
**Parse:** (this-bright-alive) | (that-dark-still)

**Interpretations:**
1. "This living light" and "that dead darkness" - explicit contrast
2. "I'm alive and bright here, that thing there is dark and still"
3. "My hope (bright, alive) vs. that despair (dark, still)"
4. "The candle (this, bright, alive with flame) vs. the corpse (that, dark, still)"

**Key Effect:** `yo` and `an` anchor the two groups to distinct referents, making the two-entity reading primary.

---

### 3.2 Without Explicit Reference

**Sentence:** `lu vi | mu to`

Could be:
- Same entity in two states (alive→dead, bright→dark)
- Two different entities coexisting
- Abstract contrast (life/death, light/dark)

Adding `yo`/`an` disambiguates toward two-entity reading.

---

## Test 4: Complex Multi-Part Sentences

### 4.1 Three-Part Sentence

**Sentence:** `su lu | bi | na mu`
**Parse:** (above + bright) | connecting | (below + dark)

**Interpretations:**
1. Sky connecting to underground - bright sky (su lu), something connecting (bi), dark depths (na mu)
2. Sunrise over a cave - the bright above (su lu) linked (bi) to the dark below (na mu)
3. Tree - canopy bright above (su lu), trunk connecting (bi), roots dark below (na mu)
4. Human: mind/heaven above (su lu), body connecting (bi), unconscious/earth below (na mu)

**Observation:** `bi` in the middle creates a linking structure between the two poles.

---

### 4.2 Temporal Sequence

**Sentence:** `ta lu | du bi | fi mu`
**Parse:** (beginning + bright) | (ongoing + connecting) | (ending + dark)

**Interpretations:**
1. **Day cycle:** dawn bright (ta lu), day continuing (du bi), dusk dark (fi mu)
2. **Life cycle:** birth bright (ta lu), life flowing (du bi), death dark (fi mu)
3. **Relationship:** meeting bright (ta lu), continuing connection (du bi), ending dark (fi mu)
4. **Fire:** ignition bright (ta lu), burning ongoing (du bi), going out dark (fi mu)

**Observation:** The three-part structure with `|` naturally creates a temporal/narrative sequence.

---

## Test 5: Grouping vs. No Grouping - Contrastive Pairs

| Sentence | Interpretation Style |
|----------|---------------------|
| `lu mu` | Paradox - bright AND dark = twilight, chiaroscuro, ambiguity |
| `lu \| mu` | Contrast - bright NEXT-TO dark = light and shadow, distinct |
| `vi to` | Paradox - alive AND still = sleeping, frozen life, meditation |
| `vi \| to` | Contrast - alive NEXT-TO still = living thing beside dead thing |
| `su na` | Paradox - above AND below = spanning, vertical extent |
| `su \| na` | Contrast - above NEXT-TO below = heaven and earth, distinct realms |

---

## Summary Findings

1. **Without `|`:** All words intersect. Interpretations seek single referents satisfying all constraints.

2. **With `|`:** Words form separate groups. Interpretations involve:
   - Multiple entities
   - Multiple states of one entity
   - Abstract contrast/juxtaposition

3. **`nu |`:** Sentence-level negation creates very broad regions (complement of narrow intersection).

4. **`yo`/`an` + `|`:** Explicitly anchors groups to distinct referents.

5. **Multi-part with `bi`:** Creates structured relationships (connection, transition, narrative).

6. **Paradox vs. Contrast:**
   - `A B` = paradox (both simultaneously)
   - `A | B` = contrast (distinct, coexisting)

---

## Implications

1. The `|` operator is essential for expressing relations between entities
2. Without `|`, Limn can only describe single meaning-regions
3. With `|`, Limn can describe scenes, relationships, and narratives
4. The operator is orthogonal to word order (still commutative within groups)

---

## Result: PASS

The `|` grouping operator successfully creates scope boundaries. It transforms single-region sentences into multi-region structures, enabling relational and contrastive meanings that would be impossible with pure intersection.

---

**END OF GROUPING TEST**
