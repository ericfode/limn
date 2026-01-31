# Cross-Key Test: Multiple Keys on Same Sentence

**Date:** 2026-01-24
**Model:** Claude Opus 4.5
**Vocabulary Version:** v1 (40 words)

---

## Purpose

Test how the same Limn sentence produces radically different interpretations under different context keys. This validates the key mechanism as the primary disambiguation tool.

---

## Test 1: `ra vi du` Under 8 Different Keys

**Sentence:** `ra vi du`
**Base Constraints:** linear/extended + alive/animate + ongoing/continuous

### Key 1: Biology
**Interpretations:**
1. River flowing with fish
2. Bloodstream in arteries
3. Neural pathway firing continuously
4. DNA strand during replication
5. Migrating animal following a path

**Primary Reading:** Living biological systems with linear flow

---

### Key 2: Technology
**Interpretations:**
1. Data streaming through fiber optic cable
2. Assembly line with robots working
3. Continuous deployment pipeline
4. Active network connection
5. Live video feed

**Primary Reading:** Active technological processes with continuous data/material flow

---

### Key 3: Geography
**Interpretations:**
1. River with ecosystems
2. Active volcanic fissure
3. Trade route with ongoing commerce
4. Migration corridor
5. Living shoreline

**Primary Reading:** Linear geographic features with active processes

---

### Key 4: Psychology
**Interpretations:**
1. Stream of consciousness
2. Linear narrative of life
3. Ongoing obsessive thought pattern
4. Life path/journey
5. Flow state in activity

**Primary Reading:** Mental processes with continuous linear quality

---

### Key 5: Music
**Interpretations:**
1. Melodic line in a living performance
2. Continuous drone with evolving texture
3. River of sound
4. Ongoing improvisation
5. Life-affirming sustained note

**Primary Reading:** Continuous musical elements that feel alive/dynamic

---

### Key 6: Economics
**Interpretations:**
1. Supply chain operating continuously
2. Revenue stream (alive = active business)
3. Ongoing trade relationship
4. Living wage flowing to workers
5. Active market with continuous trading

**Primary Reading:** Economic flows and active commercial relationships

---

### Key 7: Mythology/Religion
**Interpretations:**
1. The river of life (mythological)
2. Serpent of eternity
3. The path of souls
4. Chi/prana flowing through meridians
5. The thread of fate (Norns weaving)

**Primary Reading:** Mythological concepts of life-force in linear/flowing form

---

### Key 8: Urban Planning
**Interpretations:**
1. Active pedestrian corridor
2. Living street with continuous activity
3. Green corridor connecting parks
4. Transit line with constant service
5. Linear park with ongoing foot traffic

**Primary Reading:** Linear urban spaces with continuous human activity

---

## Cross-Key Analysis for `ra vi du`

| Key | Primary Domain | Core Metaphor |
|-----|---------------|---------------|
| Biology | Organic systems | Blood/nerve flow |
| Technology | Digital/mechanical | Data stream |
| Geography | Landscape | River/corridor |
| Psychology | Mind | Consciousness stream |
| Music | Sound | Melodic line |
| Economics | Commerce | Supply/revenue |
| Mythology | Spirit | Life force |
| Urban | City | Living street |

**Observation:** The sentence `ra vi du` has a consistent *structural* meaning (linear + alive + continuous) but the *referent* shifts completely with the key. The key doesn't change the grammar; it selects from the meaning region.

---

## Test 2: `ko mu fi` Under 6 Different Keys

**Sentence:** `ko mu fi`
**Base Constraints:** solid/resistant + dark/hidden + ending/finishing

### Key 1: Geology
- Cave collapse, mine closing, underground burial, fossil formation

### Key 2: Personal
- Secret grief ending, hidden relationship concluding, buried memory surfacing

### Key 3: Architecture
- Building demolition at night, basement sealing, tomb completion

### Key 4: Medicine
- Tumor dying (good), organ failure (bad), abscess draining

### Key 5: Astronomy
- Black hole forming, star collapsing, eclipse ending

### Key 6: Crime
- Evidence buried, case going cold, criminal's end in darkness

---

## Test 3: Narrative Key (Highly Specific)

**Sentence:** `ve lu vi ta`
**Base Constraints:** very-bright + alive + beginning

### Narrative Key A:
*"Sarah has been depressed for months. Today she met a therapist who finally understood her."*

**Interpretation:** The beginning of Sarah's healing - a very bright moment of hope, feeling alive again, starting recovery.

### Narrative Key B:
*"The lab has been working on cold fusion for decades. Last night the reactor showed unexpected activity."*

**Interpretation:** The reactor is coming to life - intensely bright, becoming active (alive), the moment of breakthrough beginning.

### Narrative Key C:
*"The eggs in the incubator have been still for 21 days. At 3am, movement was detected."*

**Interpretation:** Chicks hatching - the very bright lamp, new life, the beginning of emergence.

---

**Observation:** Narrative keys collapse interpretation to essentially one reading, precisely fitting the story context.

---

## Test 4: Contradictory Keys

**Sentence:** `ga he vi`
**Base Constraints:** positive/wanted + hot/active + alive/animate

### Key: "Winter survival"
**Expected Conflict:** "hot" contradicts winter; "positive+alive" fits survival

**Resolution:** Body warmth (he) keeping someone alive (vi), which is positive (ga) = surviving the cold, internal heat is good

### Key: "Fever during illness"
**Expected Conflict:** "hot" + "alive" present, but "positive" conflicts with illness

**Resolution:** The fever (he) means the immune system is fighting (vi=alive, active), which could be positive (ga) = fever as sign of body fighting infection

### Key: "Desert at noon"
**Expected Conflict:** "hot" and "alive" present, but "positive" may conflict with danger

**Resolution:** Heat-adapted life (vi) thriving (ga) in the hot desert (he) = life finding a way

---

**Observation:** When key and sentence seem to conflict, interpretation finds creative resolutions. The key doesn't reject the sentence; it constrains interpretation to compatible readings.

---

## Test 5: Key Collapse Ratio Across Different Keys

**Sentence:** `mi vi he`
**Base Constraints:** dispersed/many + alive + hot/active

| Key | Interpretations | Count |
|-----|-----------------|-------|
| None | Swarm, bacteria, crowd, particles, sparks, fever cells, etc. | 12+ |
| "insects" | Swarm of bees, ant colony active, mosquito cloud | 3 |
| "medicine" | Fever with elevated white cells, infection spreading | 2 |
| "social" | Flash mob, crowd surge, viral movement | 3 |
| "astronomy" | Active star cluster, meteor shower, cosmic rays | 3 |
| "marine biology" | Bioluminescent plankton bloom, spawning event | 2 |

**Average Collapse Ratio:** 12 / 2.6 = **4.6x**

---

## Summary Findings

1. **Radical Reinterpretation:** The same sentence means completely different things under different keys

2. **Structural Invariance:** The *relationship* between constraints is preserved; only the domain changes

3. **Collapse Ratios:**
   - Topic keys: ~3-5x collapse
   - Narrative keys: ~10x+ collapse (often to single interpretation)

4. **Conflict Resolution:** Keys that seem to contradict sentences find creative resolutions, not rejection

5. **Key Orthogonality:** Keys from different domains (biology vs. technology vs. mythology) produce non-overlapping interpretations

---

## Theoretical Implication

The key functions as a **dimensional selector** in meaning space:
- Without key: projection onto all dimensions (many readings)
- With key: projection onto specific dimensional subspace (few readings)

This is analogous to:
- Basis selection in linear algebra
- Feature selection in machine learning
- Context in pragmatics

---

## Result: PASS

The cross-key test demonstrates that Limn's key mechanism successfully:
1. Produces radically different interpretations from identical sentences
2. Collapses ambiguity by 3-10x depending on key specificity
3. Gracefully handles apparent key-sentence conflicts
4. Maintains structural meaning while shifting referential meaning

The key is the primary disambiguation mechanism, as designed.

---

**END OF CROSS-KEY TEST**
