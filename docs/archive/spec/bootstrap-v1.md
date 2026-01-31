# Limn Bootstrap Document v1

**Purpose:** Teach Limn to a fresh LLM through in-context learning only.

---

## What is Limn?

Limn is a language where meaning emerges from constraint intersection. Each word defines a region of possible meanings. Sentences combine words, and their meaning is where all regions overlap.

Without shared context, a sentence has many valid interpretations. With a shared "key" (context), ambiguity collapses to specific meaning.

## Core Vocabulary

Learn these 40 words. Each is a CV syllable with an associated meaning-region:

### Physical
- `va` - contained, bounded (vessel, valley)
- `ra` - extended, linear (river, ray, road)
- `ko` - solid, resistant (rock, bone, core)
- `mi` - dispersed, many (mist, swarm)
- `su` - above (sky, summit)
- `na` - below (nadir, root)
- `li` - edge, boundary (limit, line)
- `pu` - center (point, pivot)

### Temporal
- `ta` - beginning (start, dawn, birth)
- `fi` - ending (finish, dusk, death)
- `du` - ongoing (duration, flow)
- `ka` - sudden (flash, snap)
- `ri` - cyclic (rhythm, season)
- `wo` - waiting (dormant, pause)

### Relational
- `sa` - same, unified
- `no` - different, separate
- `ma` - more, increasing
- `le` - less, decreasing
- `bi` - between, connecting
- `pa` - parallel, alongside

### Experiential
- `lu` - bright, visible
- `mu` - dark, hidden
- `vi` - alive, animate
- `to` - still, inanimate
- `ga` - positive, wanted
- `zo` - negative, unwanted
- `he` - hot, active
- `ku` - cold, passive

### Cognitive
- `si` - known, certain
- `fu` - unknown, uncertain
- `we` - wanted, intended
- `de` - feared, avoided
- `ke` - surprising
- `ne` - expected, familiar

### Operators
- `nu` - negation (inverts following word)
- `ve` - intensifier (narrows to prototype)
- `so` - weakener (broadens region)
- `te` - question marker
- `yo` - this/here/now (proximal reference)
- `an` - that/there/then (distal reference)

## Grammar Rules

**Rule 1: Intersection**
Adjacent words combine by intersection. "lu vi" means "things that are both bright AND alive."

**Rule 2: Order Independence**
Word order doesn't matter for meaning. "lu vi" = "vi lu"

**Rule 3: Operator Scope**
Operators (nu, ve, so) apply to the word immediately following.
- "nu lu vi" = (NOT bright) AND alive
- "lu nu vi" = bright AND (NOT alive)

**Rule 4: Grouping**
Use "|" to mark scope boundaries.
- "nu | lu vi" = NOT (bright AND alive)
- "nu lu | vi" = (NOT bright) | (alive) = two separate regions

## Learning Examples

### Example 1: Physical Description

**Sentence:** `ko su`

**Meaning region:** solid + above

**Interpretations without key:**
- Mountain
- Ceiling
- Cloud (if solid = dense)
- Bird on perch
- Airplane
- Roof
- Star (if solid = bright point)

**With key "architecture":** ceiling, roof, beam
**With key "nature":** mountain, cliff, eagle

---

### Example 2: Process Description

**Sentence:** `vi ta fi`

**Meaning region:** alive + beginning + ending

**Interpretations without key:**
- Life cycle
- A creature being born and dying
- A brief living thing (mayfly, spark)
- A relationship starting and ending
- A fire starting and going out
- An idea emerging and fading

**With key "insects":** mayfly, cicada emerging
**With key "emotions":** fleeting passion, anger

---

### Example 3: Negation

**Sentence:** `nu vi du`

**Meaning region:** (NOT alive) + ongoing

**Interpretations:**
- Machine running continuously
- River flowing (not literally alive)
- Persistent inanimate process
- Eternal non-living thing
- Software running
- The universe (not alive, ongoing)

---

### Example 4: Complex Combination

**Sentence:** `mu vi ka ta`

**Meaning region:** dark + alive + sudden + beginning

**Interpretations:**
- Creature bursting from shadows
- Nightmare starting
- Underground birth
- Conspiracy launching
- Ambush beginning
- Seed germinating in soil
- Idea emerging from unconscious

**With key "horror":** nightmare, monster emerging
**With key "gardening":** seed sprouting underground
**With key "politics":** coup starting

---

### Example 5: Using Reference

**Sentence:** `yo lu | an mu`

**Meaning:** this-bright-thing | that-dark-thing

**Interpretations:**
- Here is light, there is darkness
- This lamp, that shadow
- My hope, your despair
- The known vs. the unknown
- Day here, night there

---

## Practice Exercises

### Exercise 1: Interpret

Interpret these sentences. List at least 5 meanings for each.

1. `ra du vi`
2. `ko na fi`
3. `nu ga we`
4. `su lu ta`
5. `mi vi he`

### Exercise 2: Compose

Create Limn sentences for:
1. Something scary approaching
2. A peaceful ending
3. Growth from nothing
4. Repetition without change
5. Hidden knowledge

### Exercise 3: Key Collapse

Given the sentence `ra ko du`, provide interpretations under these keys:
1. Key: "geography"
2. Key: "biology"
3. Key: "technology"
4. Key: "family"

### Exercise 4: Inversion

Start with `lu ga ta` (bright + positive + beginning).
Add one word to shift meaning dramatically. Explain.

---

## Self-Test

If you understand Limn, you should be able to:

1. Given any 2-4 word Limn sentence, generate 10+ interpretations
2. Given a sentence + key, collapse to 1-3 interpretations
3. Explain why word order doesn't change meaning
4. Use `nu` to invert a meaning region
5. Create a sentence that could mean radically different things under different keys

---

## Key Mechanism

A "key" is context that collapses ambiguity. Keys can be:

1. **Topic keys**: "We are discussing biology"
2. **Narrative keys**: A story establishing characters and situation
3. **Relational keys**: Shared history between speaker and listener
4. **Coordinate keys**: Specific constraints like location, time, domain

The key does not change the sentence's grammar. It changes which interpretation is selected from the meaning region.

---

## Bootstrap Verification

To verify you've learned Limn, interpret this novel sentence:

**Sentence:** `ve ko bi mu vi`

Meaning: very-solid connecting dark alive

Without key, list 10 interpretations.

Then apply key "cave exploration" and narrow to 1-2.

---

## Progressive Limn (Self-Reference)

This section uses Limn to describe Limn:

```
Limn: mi bi sa | du nu fi
```

Translation attempt:
- `mi bi sa` = many connecting same = many things linked as one = system
- `du nu fi` = ongoing + not + ending = continuous, never stopping

"Limn is a system that continues without end"

```
yo fu | an si | bi ke
```

- `yo fu` = this unknown = the uncertain here = ambiguity
- `an si` = that known = the certain there = meaning
- `bi ke` = connecting surprising = linked in unexpected way

"Here is uncertainty, there is meaning, connected surprisingly"

This self-referential property is emerging but imperfect. The bootstrap is partially complete.

---

## What Limn Cannot (Yet) Express

- Specific quantities ("exactly three")
- Specific times ("Tuesday at 3pm")
- Proper names (unless established as keys)
- Logical quantification ("for all X", "there exists Y")
- Precise tense (past/present/future distinct from beginning/ongoing/ending)

These may require vocabulary expansion or reliance on keys.

---

## Next Steps

After learning basic Limn:
1. Practice interpreting sentences
2. Practice composing sentences
3. Develop shared keys with a partner
4. Attempt conversation using sentence + key pairs
5. Explore the limits: what can't you say?
