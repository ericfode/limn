# Limn Vocabulary v1

## Design Philosophy

The vocabulary must:
1. **Maximize constraint diversity** - words should "point" in different semantic directions
2. **Be learnable** - connect to intuitive concepts
3. **Be minimal** - fewer words = must combine for specificity
4. **Support inversion** - some words should flip meaning regions

## Core Vocabulary (40 words)

### Category 1: Spatial/Physical Primitives (8 words)

| Word | Constraint Region | Intuitive Anchor |
|------|-------------------|------------------|
| `va` | bounded, contained | vessel, valley, void |
| `ra` | extended, linear | ray, river, road |
| `ko` | solid, resistant | core, rock, bone |
| `mi` | dispersed, many | mist, swarm, scatter |
| `su` | above, over | sky, summit, superior |
| `na` | below, under | nadir, beneath, root |
| `li` | edge, boundary | limit, line, lip |
| `pu` | center, origin | point, pivot, pulse |

### Category 2: Temporal/Process Primitives (6 words)

| Word | Constraint Region | Intuitive Anchor |
|------|-------------------|------------------|
| `ta` | beginning, emergence | start, birth, dawn |
| `fi` | ending, cessation | finish, death, dusk |
| `du` | ongoing, continuous | duration, flow, persist |
| `ka` | sudden, instant | crack, flash, snap |
| `ri` | cyclic, returning | rhythm, orbit, season |
| `wo` | waiting, potential | dormant, seed, pause |

### Category 3: Relational Primitives (6 words)

| Word | Constraint Region | Intuitive Anchor |
|------|-------------------|------------------|
| `sa` | same, unified | single, self, together |
| `no` | different, separate | other, apart, distinct |
| `ma` | more, increasing | magnitude, grow, add |
| `le` | less, decreasing | little, shrink, subtract |
| `bi` | between, connecting | bridge, link, medium |
| `pa` | parallel, alongside | pair, companion, beside |

### Category 4: Experiential/Qualia Primitives (8 words)

| Word | Constraint Region | Intuitive Anchor |
|------|-------------------|------------------|
| `lu` | bright, visible | light, clear, known |
| `mu` | dark, hidden | mystery, shadow, unknown |
| `vi` | alive, animate | vital, moving, aware |
| `to` | still, inanimate | stone, object, thing |
| `ga` | positive, wanted | good, gain, toward |
| `zo` | negative, unwanted | bad, loss, away |
| `he` | hot, active | heat, energy, fast |
| `ku` | cold, passive | cool, calm, slow |

### Category 5: Cognitive Primitives (6 words)

| Word | Constraint Region | Intuitive Anchor |
|------|-------------------|------------------|
| `si` | known, certain | sure, see, fact |
| `fu` | unknown, uncertain | fog, guess, question |
| `we` | wanted, intended | will, wish, goal |
| `de` | feared, avoided | danger, dread, threat |
| `ke` | surprising, unexpected | strange, new, alert |
| `ne` | expected, familiar | normal, home, safe |

### Category 6: Modifiers/Operators (6 words)

| Word | Constraint Region | Intuitive Anchor |
|------|-------------------|------------------|
| `nu` | NEGATION - inverts | not, un-, anti- |
| `ve` | INTENSIFIER - narrows | very, pure, essence |
| `so` | WEAKENER - broadens | somewhat, partial, maybe |
| `te` | QUESTION - opens | what?, which?, how? |
| `yo` | SELF-REFERENCE | this, here, now |
| `an` | OTHER-REFERENCE | that, there, then |

## Phonological Constraints

- All words are CV (consonant-vowel) pairs
- Consonants: p, t, k, b, d, g, m, n, l, r, s, f, v, w, h, y, z
- Vowels: a, e, i, o, u
- No consonant clusters
- Stress is not phonemic (all syllables equal)
- Words are clearly segmentable in speech

## Combination Rules

### Basic Combination (Intersection)
Words combine by intersection of their constraint regions:

```
lu vi = bright + alive = visible living thing
       Interpretations: firefly, illuminated animal, consciousness, star (if alive?)

ko du = solid + ongoing = persistent solid thing
       Interpretations: mountain, bone, tradition, institution
```

### Negation Operator
`nu` inverts the constraint region:

```
lu = bright
nu lu = not-bright = dark (but differently constrained than mu)

vi = alive
nu vi = not-alive = dead, inanimate, or never-alive?
```

### Intensification
`ve` narrows to the prototype center:

```
lu = bright (many things)
ve lu = very-bright (fewer things: sun, explosion, enlightenment)
```

### Reference Operators
`yo` and `an` establish discourse reference:

```
yo lu = this bright thing (pointing, present)
an lu = that bright thing (absent, remembered, mentioned)
```

## Example Sentences

### Sentence 1: `ra vi du`
**Components:** linear + alive + ongoing
**Intersection:** Living things that are extended and persist
**Interpretations (no key):**
- A river (alive metaphorically, flows continuously)
- A snake (literally linear, alive, moves continuously)
- A family lineage (line of descent, living tradition)
- A neural pathway (extended, living tissue, ongoing signals)
- DNA (linear molecule, carries life, persists across generations)
- A migration route (path used by living things over time)
- Traffic (line of vehicles, "alive" with movement)

**With key "biology":** Probably DNA or neural pathway
**With key "geography":** Probably river or migration route
**With key "family":** Probably lineage

### Sentence 2: `ko su fi`
**Components:** solid + above + ending
**Intersection:** Solid things above that end
**Interpretations:**
- Falling rock
- Setting sun (solid appearance, above, ends the day)
- Dying star
- Crumbling ceiling
- Avalanche
- Meteor
- Failed ambition (metaphor: solid goal above, ending)

### Sentence 3: `nu ga we`
**Components:** not + positive + wanted
**Intersection:** Things that are not positive that are wanted
**Interpretations:**
- Guilty pleasure
- Necessary evil
- Bitter medicine
- Painful growth
- Challenging workout
- Difficult conversation
- Revenge

### Sentence 4: `mu vi ka ta`
**Components:** dark + alive + sudden + beginning
**Intersection:** Hidden living things that suddenly begin
**Interpretations:**
- Nightmare starting
- Creature emerging from shadows
- Idea forming in the unconscious
- Seed germinating underground
- Conspiracy launching
- Ambush
- Birth in a dark place

## Inversion Demonstration

Base sentence: `lu ra ga` (bright + linear + positive)
- Sunny path, hopeful journey, enlightened direction

Add `nu`: `lu ra ga nu`
- If `nu` applies to whole: NOT(bright linear positive) = dark, scattered, negative
- Interpretations: shadow spreading, chaos, despair

Add `fi` instead: `lu ra ga fi`
- Bright linear positive + ending
- The end of a good journey, sunset on a path, conclusion of enlightenment
- Could be positive (completion) or negative (loss)

Add `mu`: `lu ra ga mu`
- Bright + linear + positive + dark
- Contradiction? Or: partially illuminated path, hope mixed with uncertainty
- Twilight journey, ambivalent progress

## Information Density Analysis

### Without key:
`ra vi du` has ~7-10 interpretations spanning biology, geography, metaphor
Entropy ≈ log2(10) ≈ 3.3 bits
3 words → ~1.1 bits/word (low density due to ambiguity)

### With key "genetics":
`ra vi du` collapses to 1-2 interpretations (DNA, gene expression)
Information conveyed ≈ log2(number of things in genetics) ≈ 15+ bits
3 words + key → ~5 bits/word (higher density)

### With very specific key:
If key specifies "we are discussing the discovery of DNA structure"
`ra vi du` = "the double helix" (essentially 1 interpretation)
Information ≈ specific reference ≈ 20+ bits
Effective density: very high, but key carries most of the load

## Open Design Questions

1. **Should `nu` scope narrowly or broadly?** In `nu lu vi`, does it mean (not-bright) AND alive, or NOT(bright AND alive)?

2. **How to handle time reference?** `ta` and `fi` mark beginning/end, but what about "yesterday" or "in five minutes"?

3. **How to indicate actors vs. actions vs. objects?** Current vocabulary doesn't distinguish noun/verb/adjective.

4. **Should there be grammatical particles?** Like topic markers, case markers, or focus particles?

5. **What about quantity?** `mi` implies many, but how to say "exactly three"?

## Vocabulary Expansion Strategy

The 40-word core is intentional minimal. Expansion should:
- Add words only when combinations cannot express a needed concept
- Prefer combining existing words over adding new ones
- New words should have maximally orthogonal constraint directions
- Test information density before/after to ensure value
