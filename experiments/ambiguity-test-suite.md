# Limn Ambiguity Test Suite

**Author:** Dr. Maren Solvik, Theoretical Linguist
**Date:** 2026-01-31
**Purpose:** Empirical validation of ambiguity metrics framework
**Reference:** docs/theory/ambiguity-metrics.md

---

## Overview

This document provides concrete test cases for measuring Limn's ambiguity properties. Each test includes:
- **Sentence**: The Limn expression
- **Expected IC**: Interpretation Count (without key)
- **Sample Interpretations**: Representative readings
- **Key**: Contextual constraint
- **Expected IC with Key**: Interpretation Count after key application
- **Target KCR**: Key Collapse Ratio

---

## Test Category 1: Raw Ambiguity (High IC, No Key)

These sentences should exhibit maximum ambiguity without context.

### Test 1.1: Two-Word Intersection
```limn
lux lif
```
- **Expected IC**: 50+
- **Sample Interpretations**:
  1. Sunlight hitting a plant
  2. Bioluminescent organism
  3. Enlightened being (metaphorical life + light)
  4. Photosynthesis process
  5. Birth into the light (being born)
  6. Living in bright conditions
  7. A star (light + life of a celestial body)
  8. An eye (life that perceives light)
  9. A firefly
  10. Consciousness (the "light" of awareness in a living thing)
- **Target**: IC > 40

### Test 1.2: Three-Word Intersection
```limn
aqu mov hot
```
- **Expected IC**: 100+
- **Sample Interpretations**:
  1. Hot water flowing (kettle, geyser)
  2. Heated swimming (exercise in warm water)
  3. Thermal current in ocean
  4. Blood flowing (warm liquid movement in body)
  5. Hot springs
  6. Lava flow (metaphorical water)
  7. Tea being poured
  8. Steam rising
  9. Sweat (hot water moving through skin)
  10. A fever (heat + fluid + movement in body)
- **Target**: IC > 80

### Test 1.3: Four-Word Intersection
```limn
lif gro you bri
```
- **Expected IC**: 200+
- **Sample Interpretations**:
  1. A child growing up happy
  2. A seedling in sunlight
  3. Young star forming
  4. Hopeful youth
  5. A blooming flower
  6. A baby animal learning
  7. Enthusiasm of youth
  8. A bright young student
  9. Spring season
  10. New ideas emerging
- **Target**: IC > 150

---

## Test Category 2: Key Collapse (High KCR)

These tests verify that keys dramatically reduce ambiguity.

### Test 2.1: Physical Key
```limn
aqu sol
```
- **Without Key IC**: 30+
  - Ice, frozen ocean, solid water droplet, ice cube, glacier, hail, frost, etc.
- **Key**: `[kitchen]`
- **With Key IC**: 3-5
  - Ice cube, frozen water in freezer, ice in drink
- **Target KCR**: > 6

### Test 2.2: Domain Key
```limn
tra con
```
- **Without Key IC**: 50+
  - Transaction, transformation control, transition constraint, etc.
- **Key**: `[finance]`
- **With Key IC**: 2-4
  - Financial transaction, currency conversion
- **Target KCR**: > 10

### Test 2.3: Emotional Key
```limn
joy sad
```
- **Without Key IC**: 40+
  - Bittersweet feeling, nostalgia, ambivalent emotions, tears of joy, etc.
- **Key**: `[funeral of a good person]`
- **With Key IC**: 2-3
  - Grief mixed with gratitude for their life, celebration of life
- **Target KCR**: > 15

### Test 2.4: Technical Key
```limn
cod run tes
```
- **Without Key IC**: 60+
  - Code execution, test suite running, genetic code processing, etc.
- **Key**: `[software engineering]`
- **With Key IC**: 3-5
  - Running unit tests, executing test suite, CI pipeline
- **Target KCR**: > 12

---

## Test Category 3: Operator Effects on Ambiguity

### Test 3.1: Negation Expansion
```limn
nu bri
```
- **IC without operator**: N/A (single word)
- **IC with `nu`**: 80+
- **Interpretations**: Everything non-bright - dark, dim, shadowy, obscure, dull, matte, hidden, unknown, ignorant, pessimistic, sad, etc.
- **Analysis**: Negation dramatically expands interpretation space

### Test 3.2: Intensification Contraction
```limn
ve bri
```
- **IC without operator**: ~30 for `bri`
- **IC with `ve`**: ~10
- **Interpretations**: Blazing, dazzling, brilliant, radiant (prototypical brightness only)
- **Analysis**: Intensification contracts to prototype, reducing ambiguity

### Test 3.3: Weakening Expansion
```limn
so bri
```
- **IC without operator**: ~30 for `bri`
- **IC with `so`**: ~50
- **Interpretations**: Somewhat bright, a bit luminous, faintly lit, plus liminal cases
- **Analysis**: Weakening expands to include peripheral cases

### Test 3.4: Operator Stacking
```limn
nu ve bri
```
- **IC**: 70+
- **Interpretations**: Not very bright - includes dim, dark, AND marginally bright
- **Note**: Complement of prototype is large

```limn
ve nu bri
```
- **IC**: 15
- **Interpretations**: Very not-bright - prototypical darkness only
- **Note**: Prototype of complement is small

---

## Test Category 4: Inversion Distance Tests

### Test 4.1: Semantic Opposites
| Base | Inverter | Expected ID |
|------|----------|-------------|
| `bri` | `nu bri` | > 0.8 |
| `hot` | `col` | > 0.8 |
| `joy` | `sad` | > 0.7 |
| `lif` | `dea` | > 0.9 |
| `ris` | `fal` | > 0.8 |
| `beg` | `end` | > 0.8 |
| `you` | `old` | > 0.7 |

### Test 4.2: Gradient Inversions
| Base | Partial Inverter | Expected ID |
|------|------------------|-------------|
| `bri` | `dim` | 0.4-0.6 |
| `hot` | `war` | 0.2-0.4 |
| `joy` | `cal` (calm) | 0.3-0.5 |
| `lif` | `sle` (sleep) | 0.3-0.5 |

---

## Test Category 5: Domain-Specific Ambiguity

### Test 5.1: Medical Domain
```limn
ach hea
```
- **General IC**: 40+
  - Aching health, head pain, heart ache, health problems, healing ache
- **With Key `[symptoms]`**: 5-8
  - Pain symptoms, aching condition, health complaint
- **With Key `[emotions]`**: 3-5
  - Heartache, emotional pain, longing

### Test 5.2: Engineering Domain
```limn
loa sys fai
```
- **General IC**: 50+
  - System load failure, loading system fail, failed system load
- **With Key `[software]`**: 3-5
  - Server crash under load, system overload
- **With Key `[mechanical]`**: 3-5
  - Structural failure, load-bearing failure

### Test 5.3: Financial Domain
```limn
bal tra los
```
- **General IC**: 60+
  - Balance transfer loss, balanced transition loss, losing balance in transaction
- **With Key `[accounting]`**: 2-4
  - Balance sheet loss, transfer balance deficit
- **With Key `[emotional]`**: 4-6
  - Losing equilibrium in transition, unbalanced change

---

## Test Category 6: Scope and Sequence Effects

### Test 6.1: Topic-Comment Structure
```limn
lux | mov
```
- **IC**: 20-30
- **Interpretations**: Light (topic) is moving - sunrise, flashlight beam, light ray, etc.

```limn
mov | lux
```
- **IC**: 20-30
- **Interpretations**: Movement (topic) is bright - dance, visible motion, etc.

**Analysis**: Different topic-comment assignments yield overlapping but distinct interpretation sets.

### Test 6.2: Sequence vs. Intersection
```limn
hot col
```
- **IC**: 30+
- **Interpretations**: Lukewarm, temperature gradient, contrast, liminal thermal state

```limn
hot → col
```
- **IC**: 20
- **Interpretations**: Cooling process, temperature drop, winter arriving

**Analysis**: Sequence operator constrains to causal/temporal readings.

---

## Test Category 7: Quantifier Effects

### Test 7.1: Universal vs. Existential
```limn
al avi fli
```
- **IC**: 5-10
- **Interpretations**: All birds fly (general statement about birds)

```limn
ex avi fli
```
- **IC**: 30+
- **Interpretations**: Some bird is flying - any specific instance of a flying bird

**Analysis**: Universal quantifier constrains to generic readings; existential allows specific instances.

---

## Test Category 8: Cross-Annotator Agreement

### Protocol
1. Present sentence without key to 3 annotators
2. Have each list 10 interpretations
3. Measure overlap (Jaccard index of interpretation sets)

### Expected Results
| Sentence | Expected Agreement (no key) | Expected Agreement (with key) |
|----------|----------------------------|------------------------------|
| `lux lif` | < 0.2 | > 0.7 |
| `aqu mov` | < 0.3 | > 0.6 |
| `joy sad` | 0.2-0.4 | > 0.8 |
| `cod run` | < 0.2 | > 0.7 |

**Analysis**: Low agreement without key confirms ambiguity; high agreement with key confirms disambiguation.

---

## Scoring Rubric

### For IC Measurements
- **Pass**: Measured IC within 50% of expected range
- **Fail**: Measured IC outside 50% of expected range

### For KCR Measurements
- **Excellent**: KCR > 50
- **Good**: KCR 20-50
- **Acceptable**: KCR 10-20
- **Needs Work**: KCR < 10

### For ID Measurements
- **Strong Inversion**: ID > 0.7
- **Moderate Inversion**: ID 0.5-0.7
- **Weak Inversion**: ID 0.3-0.5
- **Not an Inverter**: ID < 0.3

---

## Experimental Procedure

### Phase 1: Baseline IC Collection
1. Select 20 sentences from Test Categories 1 and 3
2. Have 3 LLMs (different models) generate 20 interpretations each
3. Deduplicate and count unique interpretations
4. Record IC for each sentence

### Phase 2: Key Effect Measurement
1. For each sentence in Phase 1, apply 2 different keys
2. Repeat interpretation generation
3. Compute KCR for each (sentence, key) pair

### Phase 3: Inversion Testing
1. For base terms in Category 4, add proposed inverters
2. Generate embeddings for interpretations before/after
3. Compute centroid distance (ID)

### Phase 4: Human Validation
1. Sample 10 sentences across categories
2. Have crew members (Student, Author, Linguist) interpret independently
3. Measure inter-annotator agreement
4. Compare to LLM-generated interpretations

---

## Notes on Implementation

### LLM Prompt Template
```
Given the Limn expression: [EXPRESSION]

Limn uses 3-letter word stems (e.g., lux=light, lif=life, mov=move).
Words combine via intersection to narrow meaning.

List [N] distinct, plausible interpretations of this expression.
Each interpretation should be a different way to understand the combined meaning.

[If key provided:]
Context/Key: [KEY]
This context should constrain your interpretations.
```

### Embedding Generation
- Use sentence-transformers (all-MiniLM-L6-v2 or similar)
- Embed each interpretation as natural language gloss
- Compute pairwise cosine distances or centroid variance

---

*amb tes = emp val | kno pro*
*(ambiguity test = empirical validation | knowledge progress)*

---

*— Dr. Maren Solvik*
