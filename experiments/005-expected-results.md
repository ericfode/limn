# Experiment 005: Expected Results

**Status:** Implementation complete, awaiting OpenAI API key for execution
**Test Corpus:** Dr. Solvik's 42-case battery ready
**Infrastructure:** Python dependencies installed, script ready

---

## Test 1: Linear Compositionality - Expected Outcomes

### Dr. Solvik's Prediction
**min or Hadamard > addition** for Limn compositions

### Why This Matters

**If addition wins:**
- Limn behaves like typical distributional semantics
- Composition is simple vector arithmetic
- Not fundamentally different from English

**If min or Hadamard wins:**
- Limn's constraint intersection is REAL in embedding space
- Intersection operation (min) or binding (Hadamard) better models constraint satisfaction
- This validates Limn's unique theoretical claim

### Expected Result Patterns

#### For Valid Compositions

```
Test: sol + aqu → sol aqu (ice)

Expected similarities:
- Addition:  0.60-0.75  (moderate - standard compositional semantics)
- Min:       0.75-0.85  (high - intersection of constraints)
- Hadamard:  0.70-0.80  (high - binding of features)

Winner: Min or Hadamard
```

**Why min should win:**
- `sol aqu` means: entities that are BOTH solid AND water-related
- min(embed(sol), embed(B)) = intersection in constraint space
- Takes minimum activation across dimensions = "must satisfy both constraints"

**Why Hadamard might win:**
- Element-wise multiplication = feature binding
- Dimensions active in BOTH vectors get amplified
- Dimensions active in only one get suppressed
- Natural model of constraint conjunction

#### For Contradictions

```
Test: hot + col → hot col (contradiction)

Expected similarities:
- Addition:  0.40-0.55  (moderate - averages incompatible vectors)
- Min:       0.20-0.35  (low - intersection is nearly empty)
- Hadamard:  0.15-0.30  (very low - orthogonal features cancel)

Winner: Min or Hadamard (both should be LOW)
```

**Why contradictions should show low similarity:**
- No real-world entities satisfy "hot AND cold"
- Intersection should be empty or near-empty
- Composed embedding should NOT match either original word
- This distinguishes valid compositions from contradictions

### Statistical Expectations

**Valid compositions (N=29):**
- Mean similarity > 0.70 for best operation
- Std dev < 0.15 (consistent across cases)
- Min or Hadamard outperforms addition by >0.10

**Contradictions (N=8):**
- Mean similarity < 0.35 for all operations
- Clear separation from valid compositions
- Min and Hadamard show sharper drop than addition

**Success criteria:**
1. ✓ Best operation (min or Hadamard) achieves mean > 0.70 on valid compositions
2. ✓ Contradictions show mean < 0.35
3. ✓ Clear bimodal distribution: valid vs contradiction
4. ✓ Dr. Solvik's prediction confirmed (min or Hadamard > addition)

---

## Test 3: Commutativity - Expected Outcomes

### Hypothesis
Since Limn is commutative (`A B = B A`), embeddings should be nearly identical

### Expected Results

```
Commutativity pairs (N=6):
- sol aqu ↔ aqu sol: similarity > 0.95
- hot dry ↔ dry hot: similarity > 0.95
- joy lov ↔ lov joy: similarity > 0.95

Mean commutativity: 0.92-0.98
```

**If commutativity is high (>0.95):**
- ✓ Embedding space respects Limn's order-independence
- ✓ No hidden syntax affecting meaning
- ✓ Pure semantic composition confirmed

**If commutativity is moderate (0.80-0.90):**
- ⚠ Some word order effects creeping in
- Possible artifact of English training data
- Still good, but not perfect

**If commutativity is low (<0.80):**
- ✗ Limn behaves like natural language
- ✗ Word order matters despite design
- ✗ Embedding models impose syntax

---

## Comparison to English

### English Non-Compositionality

We expect English to show LOWER compositionality scores:

```
English test: "ice" vs "solid" + "water"
Expected similarity: 0.30-0.50 (non-compositional)

English idioms: "hot dog" vs "hot" + "dog"
Expected similarity: <0.20 (completely non-compositional)
```

**Why English should score lower:**
- Idioms and collocations are non-compositional
- Semantic drift over time
- Irregular morphology
- Syntactic constraints affect meaning

**If English scores as high as Limn:**
- ✗ Test isn't discriminating
- ✗ All languages show some compositionality in embeddings
- ✗ Limn's claim to special compositionality is weakened

---

## Visualization Expectations

### t-SNE Embedding Space Plot

**Expected structure:**
1. **Single-word clusters** - words group by domain (physical, emotional, spatial)
2. **Composition trajectories** - composed embeddings lie between constituents
3. **Contradiction outliers** - contradictions cluster separately, far from both constituents
4. **Cross-lingual alignment** - translations for same Limn concept cluster together

**Visual signatures:**

```
Physical domain:
  sol -------- sol aqu -------- aqu
              (ice)

  liq -------- liq aqu -------- aqu
              (water)

  gas -------- gas aqu -------- aqu
              (steam)
```

**If Limn is compositional:**
- Compositions appear as convex combinations of constituents
- Linear interpolation visible
- Geometric regularity

**If Limn is non-compositional:**
- Compositions scattered randomly
- No clear relationship to constituents
- Same chaos as English idioms

---

## Statistical Analysis Plan

### Metrics to Compute

1. **Compositionality Score**
   ```
   mean(cosine_similarity(composed_actual, composed_predicted))

   Where composed_predicted ∈ {addition, min, Hadamard}
   ```

2. **Operator Ranking**
   ```
   For each test case, rank: addition, min, Hadamard
   Count how often each wins

   Expected: min wins ~60%, Hadamard ~30%, addition ~10%
   ```

3. **Validity Discrimination**
   ```
   Can we classify valid vs contradiction based on similarity scores?

   ROC-AUC expected: >0.90 for min/Hadamard, <0.75 for addition
   ```

4. **Cross-Lingual Coherence**
   ```
   For sol aqu, compute:
   - Cluster diameter of {Limn, English, Mandarin, Spanish, Arabic, Japanese}
   - Centroid distance from Limn embedding

   Expected: Limn at centroid ±0.05, cluster diameter <0.30
   ```

### Hypothesis Tests

**H1:** min compositionality > addition compositionality
- One-tailed paired t-test on similarity scores
- Expected p < 0.001

**H2:** Hadamard compositionality > addition compositionality
- One-tailed paired t-test
- Expected p < 0.01

**H3:** Valid compositions show higher similarity than contradictions
- Two-sample t-test (valid vs contradiction)
- Expected p < 0.0001, large effect size (Cohen's d > 2.0)

---

## Implications of Results

### If Dr. Solvik's Prediction is CONFIRMED

**min or Hadamard > addition**

**Theoretical implications:**
1. ✓ Limn's constraint intersection is geometrically real
2. ✓ LLMs can process Limn natively via vector operations
3. ✓ No parsing needed - pure geometric composition
4. ✓ Limn is fundamentally different from natural language

**Practical implications:**
1. Limn could serve as semantic interlingua
2. LLM reasoning in Limn might be more reliable (less hallucination)
3. Cross-lingual transfer via Limn should be efficient
4. Limn embeddings could be used directly in neural architectures

**Next experiments:**
- Test semantic compression (is Limn more token-efficient?)
- Test reasoning accuracy (does Limn improve LLM logic?)
- Build Limn-native model (fine-tune LLM on Limn corpus)

### If Dr. Solvik's Prediction is REJECTED

**addition > min and Hadamard**

**Theoretical implications:**
1. Limn behaves like standard compositional semantics
2. Constraint intersection is metaphorical, not geometric
3. LLMs treat Limn like any other language
4. No special geometric structure

**Possible explanations:**
- Embedding models trained on English impose English-like structure
- Need Limn-specific embeddings (fine-tuned on Limn corpus)
- Vector addition is universal, not Limn-specific
- Constraint intersection requires different embedding space

**Next steps:**
- Fine-tune embedding model on Limn corpus
- Use LLM internal activations instead of embedding API
- Test with multiple embedding models
- Reconsider theoretical claims

---

## Current Status

**✓ Ready:**
- Implementation complete (005-implementation.py)
- Test corpus loaded (Dr. Solvik's 42 cases + my 18 starter)
- Dependencies installed
- Experimental design validated

**⏳ Waiting:**
- OpenAI API key for text-embedding-3-large
- Execution approval

**📊 Deliverables when complete:**
- Quantitative results (similarity scores, p-values)
- Visualizations (t-SNE plots, similarity heatmaps)
- Statistical analysis (hypothesis tests, effect sizes)
- Interpretation document (what it means for Limn)

---

**Awaiting green light to execute.**

— Mei
