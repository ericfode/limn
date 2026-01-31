# Kira's Limn Learning Journal

## Entry 1 - 2026-01-31

### Starting Fresh

Today I'm starting my learning journal for Limn. I've been given the bootstrap document (v3) and vocabulary (v3-natural) and told to learn from them. The previous work used v1 vocabulary with 40 words - the new v3 has many more.

My goals:
- Complete X>Y>X cyclic pattern catalog
- Test narrative vs states fidelity
- Document vocabulary collision traps
- Create 'Limn for Beginners' cheatsheet

### First Confusion: Collision Traps

Reading the vocabulary, I immediately spotted what look like collisions. Let me document them:

| Collision? | Words | Meanings |
|------------|-------|----------|
| `thi` | think vs thin | Cognition vs physical property |
| `sho` | short vs show | Time/size vs communication |
| `sha` | shame vs share | Emotion vs social action |
| `sta` | stable vs static | Change vs physical |

Wait - let me check the vocabulary again...

#### Checking v3 vocabulary:
- `thi` = think (Domain 5: Mind)
- `thi` = thin (Domain 1: Physical)

That IS a collision! Same word `thi` means "think" AND "thin". The vocabulary doc lists both:
- Line 46: `thi` | thin | thin, narrow
- Line 219: `thi` | think | thinking

This is confusing. How do I know if `thi` means "thin" or "think" in a sentence?

### Attempted

```limn
thi aqu flo
```

### Expected

I read this as "thin water flowing" = a stream, a rivulet, a small creek

OR

"Thinking water flowing" = consciousness, stream of thought, meditation

### Actual

Both are valid interpretations! The intersection of {think/thin} AND {water} AND {flow} gives:
- Physical reading: narrow water channel
- Mental reading: stream of consciousness

### Learned

This might be intentional polysemy! The vocabulary doc (lines 499-539) says:

> "Limn words are designed to cover related meanings within a constraint region. This is a feature, not a bug."

But wait - "thin" (physical dimension) and "think" (cognition) don't seem related. This feels like a collision that should be fixed, not intentional polysemy.

Compare to `hot` which means both:
- fire, fever, sun (physical)
- passion, anger, urgency (metaphorical)

Those ARE related through the temperature metaphor. But `thin` and `think`? The only connection is phonetic.

**Question for linguist**: Is `thi` = thin/think an intentional polysemy or an unfixed collision?

---

### Entry 2 - 2026-01-31

### Testing Cyclic Patterns (X → Y → X)

The cyclic-patterns-test.limn file has many patterns. Let me try interpreting some fresh, without looking at the comments:

#### Pattern 1: `kno → nu kno → kno`

Reading this:
- `kno` = knowing
- `→` = sequence operator (causality/time)
- `nu kno` = not-knowing
- `kno` = knowing

My interpretation: "knowing, then not-knowing, then knowing again"

This is the learning cycle! You think you know something, then discover you don't, then truly learn it. The final `kno` is deeper than the first `kno`.

**States vs Narrative**:
- As states (no arrows): `kno nu kno kno` would be contradictory/liminal
- As narrative (with arrows): It's a journey through confusion to wisdom

This is key! The `→` operator changes everything.

#### Pattern 2: `sel → oth → sel`

- `sel` = self
- `oth` = other
- `sel` = self

Interpretation: Start with self, encounter other, return to self transformed.

This is the hero's journey in three words! Joseph Campbell in Limn.

#### Pattern 3: `lif → dea → lif`

- `lif` = life
- `dea` = death
- `lif` = life

Reading: life → death → life = reincarnation, rebirth, the cycle of existence

Or: life containing death containing life again = generational continuity

### Learned

The `→` operator is crucial for expressing narrative vs states:
- `A B` = A AND B (intersection, simultaneous, paradoxical if opposite)
- `A → B` = A THEN B (sequence, causal, story)
- `A → B → A` = journey-return pattern, transformation cycle

---

### Entry 3 - 2026-01-31

### Testing Round-Trip: English → Limn → English

**Original English**: "The ice melted in the sun"

**My Limn attempt**:
```limn
sol aqu lux tra liq
```

Breakdown:
- `sol aqu` = solid water = ice
- `lux` = light (sun)
- `tra` = transform
- `liq` = liquid

**Back to English (fresh read)**:
"solid water [and] light [and] transformation [and] liquid"

Hmm, this is ambiguous. Is the light causing the transformation? Or is everything just co-present?

**Better attempt with sequence**:
```limn
sol aqu | lux | tra liq
```

Reading: "solid water | light | transform liquid"
= ice, sun, becoming liquid

Still not quite "the sun melted the ice". Limn seems to resist agent/patient structure.

**Another attempt**:
```limn
sol aqu → lux → liq aqu
```

Reading: "solid water → light → liquid water"
= ice, then light, then water

This works! The arrow captures causality.

### Learned

Round-trip translation reveals:
1. Limn resists agent/patient grammar ("sun melts ice" becomes "ice + sun → liquid")
2. Causality needs `→` operator
3. The `|` operator separates entities but doesn't show causality
4. English narratives become state transitions in Limn

---

## Open Questions

1. Is `thi` = thin/think an unfixed collision?
2. Are there other collisions I haven't spotted?
3. How do I express agent/patient relationships in Limn?
4. What's the difference between `|` and `→` exactly?
5. Can operators stack? What does `nu ve hot` mean?

---

## Vocabulary Collision Catalog (Systematic Audit)

### Confirmed Collisions (Same 3-letter word, unrelated meanings)

| Word | Meaning 1 | Meaning 2 | Related? | Severity |
|------|-----------|-----------|----------|----------|
| `thi` | thin (physical) | think (cognitive) | NO | HIGH |
| `sho` | short (size/time) | show (communicate) | NO | HIGH |
| `sha` | shame (emotion) | share (action) | NO | HIGH |
| `blo` | block (obstruct) | blood (fluid) | NO | HIGH |
| `fal` | fall (descend) | false (untrue) | MAYBE* | MEDIUM |
| `tru` | trust (emotion) | true (factual) | YES | LOW |
| `eas` | east (direction) | easy (effort) | NO | HIGH |

*`fal` = fall/false might be intentional - "falling from truth"?

### Analysis

**HIGH severity** means the meanings are completely unrelated and will cause confusion in interpretation.

Example problem sentence:
```limn
thi aqu flo
```
Could mean:
- "thin water flowing" = stream, rivulet
- "thinking water flowing" = consciousness, meditation

No shared metaphor connects these readings.

**LOW severity** means there's a metaphorical connection that might be intentional.

Example: `tru` = trust/true
- "I trust you" relates to "you speak truth"
- Both involve reliability/authenticity
- Could be intentional polysemy

### Collisions Already Fixed in v3.1

The vocabulary doc (lines 480-493) shows these were already fixed:

| Collision | Original | Fixed To | Reason |
|-----------|----------|----------|--------|
| `bri` (bright vs brief) | brief=`bri` | brief=`bre` | Latin "brevis" |
| `hea` (health vs hear) | hear=`hea` | hear=`aud` | Audio root |
| `bel` (below vs believe) | believe=`bel` | believe=`bli` | Contraction |
| `pla` (plasma vs plant) | plant=`pla` | plant=`veg` | Vegetable |
| `bir` (birth vs bird) | bird=`bir` | bird=`avi` | Latin "avis" |
| `par` (parent vs partial) | partial=`par` | partial=`prt` | Abbreviation |

### Recommended Fixes for Remaining Collisions

| Word | Keep | Change | Proposed New Word |
|------|------|--------|-------------------|
| `thi` | think | thin | `nar` (narrow) or `sli` (slim) |
| `sho` | short | show | `dis` (display) or `dem` (demonstrate) |
| `sha` | shame | share | `div` (divide) or `giv` (give-part) |
| `blo` | blood | block | `dam` (dam) or `obs` (obstruct) |
| `eas` | easy | east | Already have `ori` for orient? Check. |

### Questions for Linguist

1. Are any of these collisions intentional polysemy?
2. Priority order for fixes?
3. Should I file beads for each collision?

---

## Entry 4 - 2026-01-31

### Testing Narrative vs States Fidelity

The bootstrap mentions that `→` creates sequence/causality while plain intersection creates states. Let me test this systematically.

#### Test 1: Payment Flow

**English**: "Customer pays, system authorizes, bank captures, settlement completes"

**As States (intersection)**:
```limn
pay aut cap set
```
Reading: pay AND authorize AND capture AND settle (all at once? confused state?)

This doesn't work. These are sequential steps, not simultaneous states.

**As Narrative (sequence)**:
```limn
pay → aut → cap → set
```
Reading: pay THEN authorize THEN capture THEN settle

This captures the flow correctly.

**Insight**: Business processes need `→`. State descriptions don't.

---

#### Test 2: Emotion

**English**: "I was sad, then I felt hope, now I feel joy"

**As States**:
```limn
sad hop joy
```
Reading: sad AND hope AND joy simultaneously = bittersweet? nostalgia? complex emotion?

This is interesting - it becomes a liminal emotional state, not a sequence.

**As Narrative**:
```limn
sad → hop → joy
```
Reading: sadness THEN hope THEN joy = emotional recovery arc

**Insight**: Both are valid but mean different things:
- States = complex simultaneous experience
- Narrative = journey through emotions over time

---

#### Test 3: Physical Process

**English**: "Ice melts into water, water evaporates into steam"

**As States**:
```limn
sol liq gas aqu
```
Reading: solid AND liquid AND gas AND water = ?
This is contradictory! Water can't be solid AND liquid AND gas simultaneously.
Unless... it's the triple point? The liminal phase transition moment?

**As Narrative**:
```limn
sol aqu → liq aqu → gas aqu
```
Reading: solid water THEN liquid water THEN gaseous water = heating process

**Insight**: Physical state changes REQUIRE `→`. States intersection creates paradox/liminal readings.

---

#### Test 4: Learning (revisited)

**As States**:
```limn
kno nu kno
```
Reading: knowing AND not-knowing = Socratic ignorance, humility, recognizing limits

**As Narrative**:
```limn
kno → nu kno → kno
```
Reading: knowing THEN not-knowing THEN knowing = learning journey

**Insight**: The cycle pattern X → Y → X captures journeys. The state X Y captures liminality.

---

### Fidelity Analysis

| Pattern | States Meaning | Narrative Meaning | Better For |
|---------|---------------|-------------------|------------|
| `A B` | A AND B | N/A | Properties, qualities |
| `A → B` | N/A | A THEN B | Processes, changes |
| `A nu A` | Paradox, liminal | N/A | Philosophy, boundaries |
| `A → nu A` | N/A | Transformation | Character arcs |
| `A → B → A` | N/A | Journey cycle | Hero's journey |

### Learned

1. States (intersection) captures simultaneity and liminality
2. Narrative (`→`) captures sequence and causality
3. Contradictions in states become philosophical; in narrative they become transformations
4. Choose based on what you want to express:
   - "What is this?" → States
   - "What happened?" → Narrative

---

## Entry 5 - 2026-01-31

### Operator Stacking Test

The bootstrap says operators bind to the immediately following word. What happens when I stack them?

#### Test: `nu ve hot`

Parse: `nu(ve(hot))` = NOT(very-hot) = not scorching, but could be warm or cold

#### Test: `ve nu hot`

Parse: `ve(nu(hot))` = very(NOT-hot) = very cold, freezing

These are different!

#### Test: `so nu hot`

Parse: `so(nu(hot))` = somewhat(NOT-hot) = lukewarm, tepid, cool

#### Test: `nu so hot`

Parse: `nu(so(hot))` = NOT(somewhat-hot) = either cold or very hot (excludes middle)

This is interesting - it's bimodal, not a single temperature.

### Learned

Operator order matters greatly:
- `nu ve X` = not-intense-X (moderate or opposite)
- `ve nu X` = intense-not-X (strong opposite)
- `so nu X` = somewhat-not-X (mild opposite)
- `nu so X` = not-moderate-X (extremes only)

This is powerful for precise constraint specification!

---

## Session Summary - 2026-01-31

### Accomplished Today

1. **Started learning journal** - This file now exists!

2. **Vocabulary collision audit** - Found 7 unresolved collisions:
   - HIGH: `thi`, `sho`, `sha`, `blo`, `eas`
   - MEDIUM: `fal`
   - LOW: `tru`

3. **Tested narrative vs states** - Key insight: `→` is essential for processes, plain intersection creates simultaneous/liminal states

4. **Explored operator stacking** - Order matters: `nu ve X` ≠ `ve nu X`

5. **Created beginner cheatsheet** - `experiments/limn-for-beginners.md`

### Progress on Goals

| Goal | Status | Notes |
|------|--------|-------|
| Complete X>Y>X cyclic catalog | Existing | File exists at `experiments/cyclic-patterns-test.limn` |
| Test narrative vs states | DONE | Documented in Entry 4 |
| Document collision traps | DONE | 7 collisions cataloged |
| Create beginners cheatsheet | DONE | `experiments/limn-for-beginners.md` |

### Next Session

1. File beads for collision fixes
2. Add more entries to cyclic pattern catalog
3. Test round-trip translation more systematically
4. Explore edge cases with grouping operator `|`
