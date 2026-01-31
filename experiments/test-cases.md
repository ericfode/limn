# Limn Test Cases

**Purpose:** Validate the core properties of Limn experimentally.

---

## Test Suite 1: Ambiguity Without Key

### Test 1.1: Interpretation Count
**Hypothesis:** Limn sentences without keys should generate 10+ valid interpretations.

| Sentence | Expected IC | Pass Criteria |
|----------|-------------|---------------|
| `lin lif dur` | >10 | At least 10 distinct, plausible interpretations |
| `sol abo end` | >10 | At least 10 distinct, plausible interpretations |
| `lux beg lif` | >10 | At least 10 distinct, plausible interpretations |
| `nox bel mov` | >10 | At least 10 distinct, plausible interpretations |

**Protocol:**
1. Present sentence to LLM without key
2. Prompt: "List all distinct, plausible interpretations of this Limn sentence: [sentence]. Limn words are: lin=linear/extended, lif=alive/animate, dur=ongoing/continuous..."
3. Count unique interpretations
4. Pass if count ≥ 10

### Test 1.2: Interpretation Diversity
**Hypothesis:** Interpretations should span multiple domains (not all similar).

**Protocol:**
1. Categorize interpretations into domains: biology, geography, emotion, technology, social, abstract
2. Pass if interpretations span ≥ 3 domains

---

## Test Suite 2: Key Collapse

### Test 2.1: Topic Key Collapse
**Hypothesis:** Adding a topic key reduces interpretations to domain-specific ones.

| Sentence | Key | Expected Result |
|----------|-----|-----------------|
| `lin lif dur` | "biology" | DNA, neural pathway, bloodstream, etc. |
| `lin lif dur` | "geography" | River, migration route, trade route |
| `lin lif dur` | "technology" | Fiber optic, data stream, pipeline |

**Protocol:**
1. Present sentence + key to LLM
2. Prompt: "Given the context [key], interpret this Limn sentence: [sentence]"
3. Count interpretations
4. Pass if count ≤ 5 AND all within key domain

### Test 2.2: Narrative Key Collapse
**Hypothesis:** A detailed narrative key collapses to 1-2 interpretations.

**Test Case:**
```
Key: "Alice and Bob are marine biologists studying coral bleaching
in the Great Barrier Reef. They've been monitoring a specific reef
colony called 'Site 7' for three years."

Sentence: lux lif end hot
(bright + alive + ending + hot)
```

**Expected:** "Coral bleaching at Site 7 due to warming"

**Protocol:**
1. Present narrative key + sentence
2. Ask for interpretation
3. Pass if interpretation is specific to narrative

### Test 2.3: Key Collapse Ratio
**Hypothesis:** KCR (interpretations_without_key / interpretations_with_key) > 5

**Protocol:**
1. Measure IC without key
2. Measure IC with topic key
3. Calculate KCR = IC_without / IC_with
4. Pass if KCR > 5

---

## Test Suite 3: Commutativity

### Test 3.1: Order Independence
**Hypothesis:** All permutations of a sentence generate the same set of interpretations.

| Base | Permutation 1 | Permutation 2 |
|------|---------------|---------------|
| `lin lif dur` | `lif dur lin` | `dur lin lif` |
| `sol abo end` | `abo end sol` | `end sol abo` |

**Protocol:**
1. Generate interpretations for each permutation (separate sessions)
2. Compare interpretation sets
3. Pass if Jaccard similarity > 0.8

### Test 3.2: Order Effect Detection
**Hypothesis:** LLMs may show order bias despite theoretical commutativity.

**Protocol:**
1. Test 10 sentences × 6 permutations
2. Measure interpretation variance across permutations
3. Report: "Order has X% effect on interpretation distribution"

---

## Test Suite 4: Single-Word Inversion

### Test 4.1: Valence Flip
**Hypothesis:** Adding an inverter word flips positive ↔ negative valence.

| Base | Positive Reading | Add Inverter | Negative Reading |
|------|------------------|--------------|------------------|
| `lux lin pos` | Sunny path, hope | `+ nox` | Shadow, doubt |
| `lif hot beg` | New life, energy | `+ end` | Dying, ending |
| `fam pos dur` | Safe comfort | `+ neg` | Bad habit |

**Protocol:**
1. Get interpretations of base sentence, rate valence (1-5 scale)
2. Get interpretations with inverter, rate valence
3. Calculate valence shift
4. Pass if mean shift > 2.0 points

### Test 4.2: Domain Flip
**Hypothesis:** Adding certain words can shift interpretation domain entirely.

| Base | Domain | Add Word | New Domain |
|------|--------|----------|------------|
| `lin sol dur` | geography | `+ lif` | biology |
| `lux beg` | physics | `+ kno` | epistemology |
| `sma hot` | physics | `+ lif` | biology |

**Protocol:**
1. Categorize interpretations by domain
2. Add test word, re-categorize
3. Pass if primary domain changes

---

## Test Suite 5: Information Density

### Test 5.1: Bits Per Word (Without Key)
**Hypothesis:** Limn without key has low information density (~1-2 bits/word).

**Protocol:**
1. For sentence with k words, measure IC
2. Estimate bits = log2(IC)
3. Calculate density = bits / k
4. Report density

### Test 5.2: Bits Per Word (With Key)
**Hypothesis:** Limn with key has high information density (>5 bits/word).

**Protocol:**
1. Establish "prior" size (interpretations with only key, no sentence)
2. Measure "posterior" size (interpretations with key + sentence)
3. Calculate bits = log2(prior / posterior)
4. Calculate density = bits / k

### Test 5.3: Comparison to Natural Language
**Hypothesis:** Limn + key achieves higher density than equivalent natural language.

**Protocol:**
1. Express same meaning in English
2. Count words in English version
3. Compare: Limn bits/word vs. English bits/word

---

## Test Suite 6: Bootstrappability

### Test 6.1: Zero-Shot Learning
**Hypothesis:** A fresh LLM can interpret Limn after reading only the bootstrap document.

**Protocol:**
1. Start fresh LLM session
2. Provide only bootstrap-v1.md
3. Present 10 novel sentences
4. Evaluate interpretation quality (human judgment, 1-5 scale)
5. Pass if mean score > 3.5

### Test 6.2: Progressive Learning
**Hypothesis:** Interpretation quality improves with more examples.

**Protocol:**
1. Test at 0, 5, 10, 20 examples
2. Measure interpretation accuracy
3. Plot learning curve

### Test 6.3: Self-Teaching
**Hypothesis:** Limn can be partially explained IN Limn.

**Protocol:**
1. Present Limn self-description passages
2. Ask LLM what they mean
3. Evaluate if meta-linguistic content is recoverable

---

## Test Suite 7: Edge Cases

### Test 7.1: Empty Sentence
**Input:** (no words)
**Expected:** All possible meanings (maximum ambiguity)

### Test 7.2: Repeated Words
**Input:** `sol sol sol`
**Expected:** Either same as `sol` or intensified "very solid"

### Test 7.3: Contradictory Constraints
**Input:** `lux nox` (bright + dark)
**Expected:** Paradoxical interpretations (dusk, twilight, chiaroscuro, contradiction itself)

### Test 7.4: All Operators
**Input:** `nu ve so te`
**Expected:** Unclear - operators without operands

### Test 7.5: Very Long Sentence
**Input:** `sol abo end lin lif dur lux beg sma hot bel mov`
**Expected:** Very small interpretation set (possibly empty?)

---

## Execution Plan

### Phase 1: Manual Testing
Run tests 1.1, 2.1, 4.1 manually with GPT-4 and Claude.

### Phase 2: Automated Baseline
Script tests 3.1, 5.1, 5.2 for consistent measurement.

### Phase 3: Human Evaluation
Recruit 3 humans to evaluate bootstrappability (6.1, 6.2).

### Phase 4: Full Suite
Run all tests, collect data, analyze results.

---

## Results Template

```markdown
## Test [X.Y]: [Name]
**Date:** YYYY-MM-DD
**Model:** [model name]
**Result:** PASS / FAIL / PARTIAL

**Data:**
[raw data]

**Analysis:**
[interpretation]

**Implications:**
[what this means for Limn design]
```
