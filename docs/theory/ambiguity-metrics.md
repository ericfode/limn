# Ambiguity Metrics for Limn

## The Measurement Problem

Ambiguity in Limn is theoretically defined as the volume of the meaning region. But:
- We can't directly observe semantic space
- Volume computation is intractable
- We need empirical, testable metrics

This document proposes practical ambiguity measures.

## 1. Interpretation Count (IC)

**Definition:** The number of distinct, valid interpretations a sentence admits.

```
IC(σ) = |{meaning m : m is a valid interpretation of σ}|
```

### Operationalization
Ask an LLM (or human panel) to enumerate interpretations:

```
Prompt: "List all distinct, plausible meanings of: [sentence]"
```

Count unique interpretations after deduplication.

### Properties
- **Pros**: Intuitive, directly measurable
- **Cons**: Subjective (what counts as "distinct"?), doesn't scale continuously

### Calibration Targets
| Sentence Type | Target IC |
|---------------|-----------|
| Highly ambiguous | 50-200+ |
| Moderately constrained | 10-50 |
| With key | 1-3 |

## 2. Interpretation Entropy (IE)

**Definition:** Shannon entropy over interpretation distribution.

If interpretations {m₁, m₂, ..., m_n} have probabilities {p₁, p₂, ..., p_n}:

```
IE(σ) = -Σ pᵢ log₂(pᵢ)
```

### Operationalization
1. Generate many interpretations via sampling (temperature > 0)
2. Cluster similar interpretations
3. Estimate probabilities from cluster frequencies
4. Compute entropy

### Properties
- **Pros**: Information-theoretic, accounts for probability distribution
- **Cons**: Requires probability estimation, sensitive to clustering

### Interpretation
- IE = 0: One interpretation with p=1 (no ambiguity)
- IE = log₂(n): Uniform over n interpretations (maximum ambiguity for n options)

## 3. Embedding Variance (EV)

**Definition:** Variance of interpretation embeddings in semantic space.

```
EV(σ) = Var({embed(mᵢ) : mᵢ ∈ interpretations(σ)})
```

### Operationalization
1. Generate k interpretations as natural language glosses
2. Embed each gloss using sentence embedder
3. Compute variance (trace of covariance matrix) or average pairwise distance

### Properties
- **Pros**: Continuous, geometric, aligns with theory
- **Cons**: Depends on embedding model, may conflate semantic vs. syntactic variance

## 4. Key Collapse Ratio (KCR)

**Definition:** How much a key reduces ambiguity.

```
KCR(σ, K) = A(σ) / A_K(σ)
```

where A is any ambiguity metric above.

### Interpretation
- KCR = 1: Key has no effect
- KCR = 100: Key reduces ambiguity 100x
- KCR → ∞: Key collapses to single interpretation

### Target
A good Limn sentence + key should achieve KCR > 50.

## 5. Inversion Distance (ID)

**Definition:** Semantic distance between interpretations before/after adding inverter word.

```
ID(σ, w_inv) = d(centroid(M(σ)), centroid(M(σ ∪ {w_inv})))
```

### Operationalization
1. Generate interpretations of σ, compute centroid embedding
2. Generate interpretations of σ + w_inv, compute centroid
3. Measure cosine distance or Euclidean distance

### Target
A good inverter achieves ID > 0.7 (on normalized scale).

## 6. Practical Test Suite

### Test 1: Raw Ambiguity
```
Given: Limn sentence σ (no key)
Measure: IC(σ), IE(σ), EV(σ)
Pass if: IC > 20, IE > 4 bits
```

### Test 2: Key Collapse
```
Given: Limn sentence σ, key K
Measure: KCR(σ, K)
Pass if: KCR > 50
```

### Test 3: Single-Word Inversion
```
Given: Limn sentence σ, inverter w
Measure: ID(σ, w)
Pass if: ID > 0.5
```

### Test 4: Information Density
```
Given: Limn sentence σ with k words, key K
Measure: I(σ|K) / k
Pass if: D_K > 10 bits/word (vs ~5-7 for natural language)
```

## 7. Experimental Protocol

### Phase 1: Baseline
1. Write 20 Limn sentences of varying length
2. For each, measure IC without key (human annotation)
3. Establish baseline ambiguity distribution

### Phase 2: Key Testing
1. For each sentence, design 3 different keys
2. Measure IC with each key
3. Compute KCR for each (sentence, key) pair

### Phase 3: Inversion Testing
1. Identify 10 candidate inverter words
2. For each sentence, test adding each inverter
3. Measure ID, identify effective inverters

### Phase 4: Cross-Validation
1. Have crew members (Student, Linguist, Author) independently interpret
2. Measure inter-annotator agreement
3. Ambiguity should cause LOW agreement without key, HIGH with key

## 8. Statistical Considerations

### Sample Size
- IC estimation: minimum 3 annotators, 5+ samples per sentence
- IE estimation: minimum 50 sampled interpretations per sentence
- EV estimation: minimum 20 embedded interpretations

### Significance Testing
- Compare Limn IC to natural language baselines
- Use Wilcoxon signed-rank for paired comparisons
- Bootstrap confidence intervals for KCR

### Confounds
- Annotator fatigue (randomize order)
- Priming effects (separate sessions for with/without key)
- Model bias (test multiple LLMs)
