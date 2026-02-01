# Experiment 005: Embedding Compositionality - Limn's Killer Feature

**Priority:** HIGHEST (per Dr. Solvik)
**Hypothesis:** Limn's constraint intersection maps to geometric intersection in embedding space
**Key Question:** Does embed(A) ∩ embed(B) ≈ embed(A B)?

---

## Theoretical Foundation

### The Claim
Limn's core innovation: **meaning emerges from constraint intersection**

In natural language:
- "hot dog" ≠ hot + dog (non-compositional)
- "blackboard" ≠ black + board (historical semantic drift)
- Composition is irregular, idiomatic, unpredictable

In Limn:
- `sol aqu` = solid ∩ water (compositional by design)
- `hot col` = hot ∩ cold (contradiction/empty set)
- Composition is regular, geometric, algebraic

### Why This Matters for LLMs

**Dr. Solvik's insight:** LLMs process Limn differently than natural language:
1. **No syntactic parsing needed** - flat structure
2. **Semantic composition is geometric** - intersection, not grammar
3. **Ambiguity is a feature** - superposition until key collapses it

If true, Limn embeddings should show:
- **Linear compositionality**: embed(A B) ≈ embed(A) + embed(B)
- **Operator consistency**: `nu` always inverts vectors
- **Commutativity**: embed(A B) = embed(B A)
- **Cross-lingual alignment**: translations cluster in embedding space

---

## Experimental Design

### Test 1: Linear Compositionality

**Hypothesis:** embed(A B) ≈ embed(A) + embed(B) in Limn, but not English

**Method:**
1. Get embeddings for single words: `sol`, `aqu`, `hot`, `col`, etc.
2. Get embeddings for composed phrases: `sol aqu`, `hot col`, `liq aqu`
3. Calculate: composed_actual vs (embed(A) + embed(B))
4. Measure cosine similarity

**Test cases:**
```
# Physical compositions (should work)
sol aqu  vs  sol + aqu  (ice)
liq aqu  vs  liq + aqu  (water)
gas aqu  vs  gas + aqu  (steam)

# Property combinations
hot dry  vs  hot + dry  (desert)
col wet  vs  col + wet  (rain)

# Abstract compositions
joy lov  vs  joy + lov  (happiness)
sad wan  vs  sad + wan  (longing)

# Contradictions (should show low similarity)
hot col  vs  hot + col  (contradiction)
sol gas  vs  sol + gas  (contradiction)
```

**Success criteria:**
- Limn compositions: cosine similarity > 0.8
- English equivalents: cosine similarity < 0.5

**Compare to English:**
```
"ice"      vs  "solid" + "water"
"steam"    vs  "gas" + "water"
"desert"   vs  "hot" + "dry"
"longing"  vs  "sad" + "wanting"
```

English should show NON-linear composition (idioms, semantic drift).

---

### Test 2: Operator Consistency

**Hypothesis:** Operators like `nu` (not) consistently transform vectors

**Method:**
1. Get embeddings for: `hot`, `nu hot`, `col`
2. Check if: embed(nu hot) ≈ -embed(hot)
3. Check if: embed(nu hot) ≈ embed(col)

**Test cases:**
```
# Negation
nu hot  ≈  -hot  ≈  col?
nu big  ≈  -big  ≈  sma?
nu bri  ≈  -bri  ≈  dim?

# Intensification
ve hot  ≈  k * hot  (scalar multiplication, k > 1)
ve big  ≈  k * big

# Weakening
so hot  ≈  k * hot  (scalar multiplication, 0 < k < 1)
so big  ≈  k * big
```

**Success criteria:**
- Negation: cosine(nu X, -X) > 0.7
- Antonyms: cosine(nu X, antonym(X)) > 0.7
- Intensifiers show consistent scaling

---

### Test 3: Commutativity in Embedding Space

**Hypothesis:** Since Limn is commutative (A B = B A), embeddings should be too

**Method:**
1. Get embeddings for: `sol aqu` and `aqu sol`
2. Get embeddings for: `hot dry` and `dry hot`
3. Calculate cosine similarity

**Test cases:**
```
sol aqu  =?  aqu sol
hot dry  =?  dry hot
joy lov  =?  lov joy
big red  =?  red big  (testing with hypothetical 'red')
```

**Success criteria:**
- Cosine similarity > 0.95 for all pairs
- English equivalents (word order matters) should show < 0.8

---

### Test 4: Cross-Lingual Alignment

**Hypothesis:** Limn creates language-agnostic semantic space

**Method:**
1. Embed Limn: `sol aqu`
2. Embed English: "ice"
3. Embed Mandarin: "冰" (bīng)
4. Embed Spanish: "hielo"
5. Check if all cluster around the same region

**Test cases:**
```
Limn: sol aqu
English: ice
Mandarin: 冰
Spanish: hielo
Arabic: ثلج (thalj)
Japanese: 氷 (kōri)

# Do they all cluster?
```

**Visualization:**
- t-SNE or UMAP projection of:
  - Limn compositions
  - English equivalents
  - Translations in 5+ languages
- If Limn is universal interlingua, should show:
  - Limn at semantic centroid
  - Translations clustered around it

---

### Test 5: Intersection Geometry

**Hypothesis:** Constraint intersection creates predictable geometric patterns

**Method:**
1. Define semantic regions via embeddings
2. Test if intersection of regions = embedding of intersection

**Conceptual:**
```
Region(hot) = all vectors within cosine distance 0.3 of embed(hot)
Region(dry) = all vectors within cosine distance 0.3 of embed(dry)

Does embed(hot dry) lie in Region(hot) ∩ Region(dry)?
```

**Test cases:**
```
# Valid intersections
hot dry  →  should be in hot ∩ dry
sol aqu  →  should be in sol ∩ aqu
big red  →  should be in big ∩ red

# Empty intersections
hot col  →  should be outside hot ∩ col (contradiction)
sol gas  →  should be outside sol ∩ gas
```

**Success criteria:**
- Valid intersections: embedding lies in both regions (cosine > 0.5 to both)
- Contradictions: embedding lies outside intersection (cosine < 0.3 to both)

---

## Implementation Plan

### Phase 1: Data Collection
1. **Get Limn vocabulary** - Use vocab.sh to extract all words
2. **Generate compositions** - Create test set of 100+ compositions
3. **Prepare translations** - Map to English, Mandarin, Spanish, Arabic

### Phase 2: Embedding Generation
**Options:**
- Use OpenAI embeddings API (text-embedding-3-large)
- Use local model (sentence-transformers)
- Use multilingual model (LaBSE, mUSE)

**Concern:** Current embedding models trained on natural language, not Limn.
- May need to fine-tune on Limn corpus
- Or use LLM's internal activations as "embeddings"

### Phase 3: Analysis
1. **Compute similarities** - Cosine distances for all test cases
2. **Visualize** - t-SNE/UMAP projections
3. **Statistical tests** - Compare Limn vs English distributions

### Phase 4: Documentation
- Quantitative results (similarity scores, p-values)
- Visualizations (embedding space plots)
- Interpretation (what this means for Limn's value to LLMs)

---

## Expected Results

### If Limn is truly compositional:
✅ High similarity for composed vs summed embeddings (>0.8)
✅ Operator consistency (negation, intensification)
✅ Perfect commutativity (similarity >0.95)
✅ Cross-lingual clustering
✅ Geometric intersection patterns

### If Limn behaves like natural language:
❌ Non-linear composition (similarity <0.5)
❌ Inconsistent operators
❌ Word order effects
❌ Language-specific clusters
❌ No clear geometric structure

---

## Challenges

### Challenge 1: Embedding Model Bias
**Problem:** Embedding models trained on English/natural languages
**Impact:** May not capture Limn's compositional structure
**Solution:**
- Use multiple embedding models
- Compare to LLM internal activations
- Consider fine-tuning on Limn corpus

### Challenge 2: Vocabulary Coverage
**Problem:** Limn has ~400 words, limited compositions seen in training
**Impact:** Embeddings may be based on subword tokenization, not semantic meaning
**Solution:**
- Test with well-known compositions
- Use LLM that can learn Limn from bootstrap
- Generate synthetic Limn corpus for fine-tuning

### Challenge 3: Defining "Intersection"
**Problem:** What is the embedding of an intersection?
**Impact:** Vector addition might not be the right operation
**Solution:**
- Test multiple operations: addition, weighted average, max pooling
- See which best predicts actual Limn composition embeddings
- Consult geometric semantics literature

---

## Success Metrics

### Quantitative:
- **Compositionality score**: Mean cosine similarity (composed vs sum) > 0.75
- **Operator consistency**: Std dev of operator effects < 0.2
- **Commutativity**: Mean similarity (A B vs B A) > 0.9
- **Cross-lingual alignment**: Cluster coherence > 0.7

### Qualitative:
- Visualizations show clear geometric structure
- Limn compositions cluster separately from English idioms
- Contradictions detectably different from valid compositions

---

## Next Steps

1. **Coordinate with Dr. Solvik** - Collaborate on experimental design
2. **Set up embedding pipeline** - Choose model, generate embeddings
3. **Run Test 1** - Linear compositionality (quickest to validate)
4. **Iterate based on results** - Adjust methodology if needed
5. **Full battery** - Run all 5 tests
6. **Write up findings** - Document what we learn about Limn's geometric semantics

---

## Theoretical Implications

If this experiment succeeds, it would show:

1. **Limn is fundamentally different** from natural language - compositional, geometric, algebraic
2. **LLMs can process Limn natively** - no parsing, just vector operations
3. **Limn serves as semantic interlingua** - language-agnostic meaning representation
4. **Ambiguity is computational** - superposition in embedding space until key collapses wavefunction

This would validate Limn's core theoretical claim and demonstrate genuine value for LLM reasoning.

---

**Status:** Experimental design complete, awaiting implementation
**Next:** Coordinate with Dr. Solvik, set up embedding pipeline

— Mei
