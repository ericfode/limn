# Zero-Bootstrap Validation Protocol for Limn

**Author:** Dr. Maren Solvik, Theoretical Linguist
**Date:** 2026-01-31
**Status:** Validation Methodology
**Version:** 1.0

---

## Abstract

Zero-bootstrap is Limn's claim that a naive reader (human or LLM) can infer word meanings from their forms without prior training. This document provides a formal validation protocol, scoring rubrics, and expected baselines.

---

## 1. The Zero-Bootstrap Hypothesis

### 1.1 Core Claim

> A reader encountering Limn for the first time can correctly infer the meaning of most words from their phonetic/orthographic form alone.

### 1.2 Theoretical Basis

Limn vocabulary uses:
- **First-syllable truncation**: `mov` from "move", `flo` from "flow"
- **Whole-word inclusion**: `hot`, `gas`, `big`, `sad`
- **Latin/Greek roots**: `lux` (light), `aqu` (water), `pyr` (fire)
- **Phonaesthetic patterns**: Existing English sound-meaning associations

### 1.3 Success Criteria

| Level | Accuracy | Interpretation |
|-------|----------|----------------|
| Strong | >80% | Zero-bootstrap succeeds |
| Moderate | 60-80% | Partial success; some training needed |
| Weak | 40-60% | Design needs revision |
| Failed | <40% | Zero-bootstrap fails |

---

## 2. Validation Protocol

### 2.1 Test Population Categories

| Category | Description | Expected Performance |
|----------|-------------|---------------------|
| **Type A** | Native English speakers, no Limn exposure | Primary target |
| **Type B** | Non-native English speakers (high proficiency) | Slightly lower |
| **Type C** | LLMs without Limn training | Should match Type A |
| **Type D** | LLMs with Limn bootstrap | Ceiling (>95%) |

### 2.2 Test Materials

**Vocabulary Sample:** Stratified random sample from v3-natural vocabulary.

| Stratum | Description | Sample Size |
|---------|-------------|-------------|
| Whole-word | English words kept intact | 20 words |
| First-syllable | CVC truncation of English | 30 words |
| Latin/Greek | Classical roots | 20 words |
| Operators | Grammatical function words | 10 words |

**Total:** 80 words per test administration.

### 2.3 Test Format

**Multiple Choice (Recommended for scoring reliability):**
```
Word: lux
Options:
A) Light, brightness
B) Luck, fortune
C) Luxury, comfort
D) Lock, secure
```

Correct: A (from Latin "lux")

**Free Response (Recommended for depth):**
```
Word: mov
Write your best guess for the meaning: ___________
```

Scored: Accept "move", "movement", "motion", close synonyms.

### 2.4 Scoring Rubric

**Multiple Choice:**
- Correct: 1 point
- Incorrect: 0 points
- Maximum: 80 points

**Free Response:**
| Score | Criterion |
|-------|-----------|
| 2 | Exact or synonym match |
| 1 | Related concept (same semantic field) |
| 0 | Unrelated or no response |

**Maximum (free response):** 160 points

### 2.5 Expected Baselines

| Word Type | Expected MC Accuracy | Expected FR Accuracy |
|-----------|---------------------|---------------------|
| Whole-word | 95%+ | 90%+ |
| First-syllable | 70-85% | 60-75% |
| Latin/Greek | 50-70% | 40-60% |
| Operators | 40-60% | 30-50% |

**Weighted Overall Target:** >70% for Type A participants.

---

## 3. Word-Level Predictions

### 3.1 High Confidence (>90% expected)

Words that should be immediately recognizable:

| Word | Meaning | Recognition Source |
|------|---------|-------------------|
| `hot` | hot | Whole word |
| `col` | cold | First syllable |
| `big` | big | Whole word |
| `sad` | sad | Whole word |
| `joy` | joy | Whole word |
| `old` | old | Whole word |
| `new` | new | Whole word |
| `gas` | gas | Whole word |
| `dim` | dim | Whole word |
| `wet` | wet | Whole word |
| `dry` | dry | Whole word |
| `cut` | cut | Whole word |

### 3.2 Medium Confidence (60-90% expected)

Words requiring pattern recognition:

| Word | Meaning | Recognition Source | Challenge |
|------|---------|-------------------|-----------|
| `mov` | move | First syllable | Vowel truncation |
| `gro` | grow | First syllable | Vowel truncation |
| `flo` | flow | First syllable | Vowel truncation |
| `bri` | bright | First syllable | Vowel truncation |
| `sma` | small | First syllable | Vowel truncation |
| `lon` | long | First syllable | Vowel truncation |
| `lif` | life | First syllable | Vowel truncation |
| `lov` | love | First syllable | Vowel truncation |
| `hop` | hope | First syllable | Vowel truncation |
| `ang` | anger | First syllable | Less obvious |

### 3.3 Lower Confidence (40-60% expected)

Words requiring cultural/linguistic knowledge:

| Word | Meaning | Recognition Source | Challenge |
|------|---------|-------------------|-----------|
| `lux` | light | Latin "lux" | Classical education |
| `aqu` | water | Latin "aqua" | Less common root |
| `pyr` | fire | Greek "pyr" | Technical vocabulary |
| `ter` | earth | Latin "terra" | Technical vocabulary |
| `nox` | night | Latin "nox" | Less common root |
| `aer` | air | Latin "aer" | Technical vocabulary |

### 3.4 Challenge Words (<40% expected without training)

Operators and abstract terms:

| Word | Meaning | Recognition Source | Challenge |
|------|---------|-------------------|-----------|
| `nu` | negation | None obvious | Arbitrary |
| `ve` | very/intensifier | English "very" | Truncation |
| `so` | somewhat | English "so" | Meaning shift |
| `al` | all | English "all" | Vowel change |
| `ex` | exists | Latin "ex" | Technical |
| `sa` | same | English "same" | Truncation |

---

## 4. Agent-Type Comparison

### 4.1 Test Across Agent Types

| Agent Type | Test Method | Expected Result |
|------------|-------------|-----------------|
| Human (Type A) | Web survey or interview | Baseline ~70% |
| GPT-4 | Prompt with no Limn context | Should match human |
| Claude | Prompt with no Limn context | Should match human |
| Llama-3 | Prompt with no Limn context | May be slightly lower |
| Gemini | Prompt with no Limn context | Should match human |

### 4.2 LLM Test Prompt Template

```
I'm going to show you words from a constructed language called "Limn."
These words are designed to be guessable from their form.

For each word, guess its meaning. You have not seen this language before.

Word 1: mov
Your guess:

Word 2: lux
Your guess:

[... continue for all test words ...]
```

### 4.3 Control Conditions

**Positive Control:** Include 5 actual English words to verify task understanding.

**Negative Control:** Include 5 nonsense CVC words (e.g., "vep", "zun", "gix") to establish chance baseline.

---

## 5. Validation Study Design

### 5.1 Phase 1: Pilot (N=10)

**Participants:** 10 Type A (English speakers)
**Purpose:** Calibrate item difficulty, refine instructions
**Duration:** 1 session, 30 minutes

### 5.2 Phase 2: Main Study (N=50)

**Participants:**
- 30 Type A (English speakers)
- 10 Type B (Non-native English)
- 10 Type C (LLMs, 2 instances each of 5 models)

**Method:** Randomized word order, multiple-choice and free response

### 5.3 Phase 3: Longitudinal (N=20)

**Participants:** Subset of Phase 2
**Purpose:** Measure learning curve with minimal exposure
**Sessions:** Initial, +24h, +1 week (with brief exposure between)

---

## 6. Analysis Plan

### 6.1 Primary Analysis

**Zero-bootstrap success rate by word type:**
```
Success(word_type) = Σ correct / Σ total
```

**Overall success rate:**
```
Success_overall = Σ (weight_i × Success_i)
```

Where weights reflect vocabulary distribution.

### 6.2 Secondary Analyses

**Predictors of word difficulty:**
- Word length
- Source language (English vs. Latin vs. Greek)
- Frequency in source language
- Phonaesthetic transparency

**Agent-type comparison:**
- Human vs. LLM accuracy correlation
- Model-specific performance patterns

### 6.3 Item Analysis

For each word, compute:
- **Difficulty:** Proportion incorrect
- **Discrimination:** Point-biserial correlation with total score
- **Distractor analysis:** Frequency of each incorrect option

Flag words with:
- Difficulty > 0.7 (too hard) → Consider revision
- Discrimination < 0.2 (poor item) → Consider revision
- Popular distractor → Investigate confusion source

---

## 7. Expected Outcomes

### 7.1 If Zero-Bootstrap Succeeds (>70% overall)

- **Conclusion:** Limn vocabulary design meets zero-bootstrap criterion
- **Action:** Document successful patterns for future vocabulary extension
- **Implication:** Minimal training needed for basic comprehension

### 7.2 If Zero-Bootstrap Partially Succeeds (50-70%)

- **Conclusion:** Some vocabulary needs revision
- **Action:** Identify problematic words, propose alternatives
- **Implication:** Brief orientation needed before productive use

### 7.3 If Zero-Bootstrap Fails (<50%)

- **Conclusion:** Design principle not achieved
- **Action:** Major vocabulary revision required
- **Implication:** Training materials necessary

---

## 8. Vocabulary Revision Guidelines

### 8.1 For Words Below 40% Recognition

| Current | Issue | Proposed Change |
|---------|-------|-----------------|
| Abstract operator | No obvious form | Consider mnemonic |
| Latin/Greek root | Too obscure | Use English truncation |
| Ambiguous truncation | Multiple English sources | Disambiguate form |

### 8.2 Phonaesthetic Alignment

If a word "sounds wrong" for its meaning, consider phonaesthetic patterns:

| Sound | Association | Align Words |
|-------|-------------|-------------|
| /gl-/ | Light, shine | `glo` = glow |
| /sn-/ | Nose, contempt | `sno` = snow, sniff |
| /sl-/ | Slippery, negative | `sli` = slide, slick |
| /br-/ | Broken, sharp | `bri` = bright, break |

---

## 9. Implementation Checklist

### Pre-Study
- [ ] Finalize 80-word test set (stratified sample)
- [ ] Create multiple-choice distractors
- [ ] Design free-response scoring guide
- [ ] Prepare LLM test prompts
- [ ] Recruit human participants (N=50)

### Data Collection
- [ ] Pilot test (N=10)
- [ ] Revise based on pilot
- [ ] Main study data collection
- [ ] LLM testing (5 models × 2 instances)

### Analysis
- [ ] Score all responses
- [ ] Compute success rates by word type
- [ ] Compare human vs. LLM performance
- [ ] Item analysis for problematic words
- [ ] Generate revision recommendations

### Reporting
- [ ] Document overall success rate
- [ ] List words requiring revision
- [ ] Propose specific vocabulary changes
- [ ] Update vocabulary specification

---

## 10. Sample Test Items

### 10.1 Multiple Choice Format

**Item 1 (Whole-word, High confidence)**
```
Word: joy
Options:
A) Happiness, delight
B) Join, connect
C) Journey, travel
D) Juice, liquid
```
Answer: A

**Item 2 (First-syllable, Medium confidence)**
```
Word: mov
Options:
A) Move, motion
B) Mop, clean
C) Mob, crowd
D) Model, example
```
Answer: A

**Item 3 (Latin root, Lower confidence)**
```
Word: lux
Options:
A) Light, brightness
B) Luck, fortune
C) Luxury, comfort
D) Lock, secure
```
Answer: A

**Item 4 (Operator, Challenge)**
```
Word: nu
Options:
A) New, recent
B) Not, negation
C) Nurse, care
D) Number, count
```
Answer: B

### 10.2 Free Response Scoring Examples

**Word: flo**
- Score 2: "flow", "flowing", "to flow"
- Score 1: "water", "liquid", "stream" (related but not exact)
- Score 0: "flower", "floor", unrelated

**Word: aqu**
- Score 2: "water", "aqua", "liquid"
- Score 1: "ocean", "sea", "wet" (related domain)
- Score 0: "equal", "quiet", unrelated

---

*zer boo val = kno nu tra | for rec des*
*(zero bootstrap validation = know without training | form recognition design)*

---

*— Dr. Maren Solvik*
