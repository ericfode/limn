# Survey: LLM Evaluation Frameworks for Limn Quality Metrics

> fra sur | qal mes | too fit
> *(frameworks surveyed | quality measured | tools fitted)*

**Author:** Nux (Polecat)
**Date:** 2026-02-07
**Issue:** hq-e819r
**Status:** COMPLETE

---

## Executive Summary

This survey evaluates LLM evaluation frameworks across seven categories for their applicability to Limn's specific quality measurement needs. Limn is a constructed language with ~1,040 CVC words and 6 algebraic operators, requiring evaluation along dimensions that no single existing framework fully addresses: vocabulary validity, operator semantics, compositional generalization, translation fidelity, and semantic coherence.

**Key finding:** Limn's evaluation needs are best served by a **hybrid approach** combining:
1. **Grammar-constrained validation** (structural correctness — already partially implemented via FSM)
2. **Compositional generalization benchmarks** adapted from COGS/SCAN methodology (operator understanding)
3. **Round-trip translation consistency** (semantic fidelity — reference-free)
4. **LLM-as-judge with rubric** (semantic quality — leveraging Claude/GPT for meaning evaluation)
5. **RLVR with verifiable rewards** (training signal — replacing broken reward model)

No off-the-shelf framework works for Limn without adaptation. The closest analog is evaluation for formal/programming languages, not natural language MT.

---

## 1. LLM-as-Judge Frameworks

### 1.1 G-Eval (Liu et al., 2023)

**What it is:** Uses GPT-4 with chain-of-thought to evaluate text quality on specified dimensions (coherence, fluency, consistency, relevance). Outputs scores on a 1-5 scale per dimension.

**How it works:** Provides the LLM with evaluation criteria, the text to evaluate, and asks for step-by-step reasoning before scoring. Uses probability-weighted scoring from output token logits for calibration.

**Applicability to Limn: HIGH**

G-Eval's dimension-based approach maps directly to Limn's quality dimensions:
- *Operator correctness:* "Does `lov@fer` correctly extract the fear-component of love?"
- *Compositional coherence:* "Does the expression form a meaningful whole?"
- *Translation fidelity:* "Does this Limn capture the meaning of the English source?"

**Key advantage:** Claude and GPT already comprehend Limn at 77-85% zero-bootstrap (README claims). An LLM judge with the Limn bootstrap document could evaluate semantic quality with reasonable accuracy.

**Limitations:** Requires careful prompt engineering to prevent the judge from accepting plausible-sounding but semantically incorrect Limn. Judge must understand operator semantics, not just word meanings.

**Cost/complexity:** Moderate. Each evaluation requires an API call (~$0.01-0.03 per evaluation with GPT-4/Claude). Scalable for eval sets of 100-500 items.

### 1.2 MT-Bench / Chatbot Arena (Zheng et al., 2023)

**What it is:** Pairwise comparison framework where an LLM judge compares two outputs and selects the better one. Used for chatbot ranking.

**Applicability to Limn: MEDIUM**

Pairwise comparison is more robust than absolute scoring for DPO preference data collection. Could be used to generate preference pairs for Limn: "Which translation better captures the English meaning?"

**Limitation:** Position bias in pairwise comparison (first response often preferred). Requires many comparisons for stable ranking.

### 1.3 FLASK (Ye et al., 2023)

**What it is:** Fine-grained Language Model Evaluation with Skill-based Assessment Kit. Decomposes evaluation into 12 fine-grained skills across 4 ability categories.

**Applicability to Limn: MEDIUM**

FLASK's skill decomposition maps well to Limn's multi-dimensional quality needs. Could define Limn-specific skills: vocabulary validity, operator application, constraint group structure, semantic preservation, compositional creativity.

**Practical note:** More granular than G-Eval but harder to calibrate for a novel domain like Limn.

### Recommendation for Limn

**Implement a G-Eval-style rubric evaluator** with Limn-specific dimensions. Use Claude as the judge model (highest Limn comprehension at 82%). Provide the bootstrap document + operator specification as context. Score on 4 dimensions:

| Dimension | Weight | What it measures |
|-----------|--------|-----------------|
| Semantic fidelity | 0.35 | Does the Limn capture the intended meaning? |
| Operator correctness | 0.25 | Are operators used with correct semantics? |
| Structural validity | 0.20 | Does it parse? Proper group structure? |
| Compositional quality | 0.20 | Creative/appropriate use of composition vs. word listing? |

This directly replaces the broken v4 reward model (H5) and the limited v5 `score_composition_response()` which still relies on keyword overlap.

---

## 2. Machine Translation Evaluation

### 2.1 BLEU (Papineni et al., 2002)

**What it is:** N-gram precision metric comparing candidate translation against reference(s). Counts matching 1-4 grams, applies brevity penalty.

**Applicability to Limn: LOW**

BLEU assumes surface-form similarity between candidate and reference. Limn's compositional nature means the same meaning can be expressed in multiple valid ways:
- `lov@fer` and `fer*lov` might both express related concepts about fear and love
- `hot^0.8` vs `col^0.2` could convey similar temperature meanings

BLEU would penalize valid alternative expressions. Additionally, Limn's 3-letter CVC vocabulary creates many n-gram collisions.

### 2.2 METEOR (Banerjee & Lavie, 2005)

**What it is:** Harmonic mean of unigram precision and recall with stemming, synonym matching, and fragmentation penalty. Recall-weighted.

**Applicability to Limn: LOW-MEDIUM**

Better than BLEU because recall-weighted (catches missing content). However, synonym/stem matching doesn't apply to Limn's CVC vocabulary. The fragmentation penalty is relevant — Limn expressions should maintain operator-group coherence.

**Adaptation potential:** A Limn-specific METEOR could replace synonym matching with operator-equivalence matching (e.g., `A*B` ≡ `B*A` for commutative operators).

### 2.3 chrF / chrF++ (Popović, 2015)

**What it is:** Character n-gram F-score. Language-agnostic — doesn't depend on word boundaries or linguistic resources.

**Applicability to Limn: MEDIUM**

Character-level matching is more appropriate for Limn's short CVC words than word-level metrics. Language-agnostic design means no dependency on English-trained resources. chrF++ adds word n-grams to character n-grams.

**Limitation:** Still surface-form comparison. `lov@fer` and "the fear component of love" share no character n-grams. Only useful for Limn→Limn comparison, not English↔Limn.

### 2.4 BERTScore (Zhang et al., 2020)

**What it is:** Token-level cosine similarity using contextual embeddings from BERT. Computes precision, recall, and F1 over token embeddings.

**Applicability to Limn: LOW**

BERTScore depends on pre-trained BERT embeddings. Limn words are not in BERT's vocabulary — they'll be fragmented by BPE tokenizer into meaningless subwords. Cross-lingual BERTScore (using multilingual models) performs poorly on low-resource and constructed languages. The v2 Limn embedder has only 0.335 positive similarity — too low for reliable scoring.

**Potential with custom embedder:** If the Limn embedder were improved (custom tokenizer, more training data), a BERTScore-like approach using Limn embeddings could work for Limn↔Limn comparison.

### 2.5 COMET (Rei et al., 2020)

**What it is:** Learned metric trained on human quality annotations. Takes source, hypothesis, and reference as input. State-of-the-art correlation with human judgments for MT.

**Applicability to Limn: LOW (off-the-shelf) / HIGH (if fine-tuned)**

Pre-trained COMET models know nothing about Limn. However, COMET's *architecture* — a cross-encoder that learns to predict human quality scores from (source, candidate, reference) triples — is exactly what Limn needs. If sufficient human annotations existed (~500+ rated translations), a Limn-specific COMET model could be trained.

**Prerequisite:** Human-rated English↔Limn translation pairs. Currently only 329 parallel pairs exist (HGttG), with no quality annotations.

### 2.6 BLEURT (Sellam et al., 2020)

**What it is:** Pre-trained BERT model fine-tuned on synthetic + human evaluation data for MT quality prediction.

**Applicability to Limn: LOW**

Same embedding limitation as BERTScore. BLEURT's pre-training is English-centric.

### Recommendation for Limn

**Do not use standard MT metrics for English↔Limn evaluation.** They all depend on either surface-form matching (useless across languages) or pre-trained embeddings (no Limn representation).

Instead:
1. **chrF for Limn→Limn comparison** (comparing two Limn translations of the same source)
2. **Round-trip translation** for English↔Limn (see Section 6)
3. **LLM-as-judge** for semantic quality (see Section 1)
4. **Long-term: Train a Limn-specific COMET** once sufficient human annotations exist

---

## 3. Semantic Similarity & Embedding-Based Evaluation

### 3.1 Current State: Limn Embedder v2

The project's own embedder (all-MiniLM-L6-v2 fine-tuned with MNRL on 329 HGttG pairs) achieves:
- Positive pair similarity: 0.335
- Negative pair similarity: 0.013
- Discrimination: 0.322

This is usable for ranking but insufficient for absolute quality scoring. The low absolute similarity (0.335) stems from BPE tokenizer fragmentation of Limn words.

### 3.2 Cross-Lingual Sentence Embeddings

Recent work on cross-lingual embeddings (2024) introduces frameworks that explicitly align words between languages using word alignment models. This is directly relevant to Limn — the parallel HGttG corpus provides word-level alignment data.

**Key approaches:**
- **Aligned word prediction** + word translation ranking
- **LASER/LaBSE** multilingual sentence embeddings (though constructed languages were explicitly excluded from training)
- **Custom contrastive training** on parallel data

### 3.3 MAUVE (Pillutla et al., 2021)

**What it is:** Measures the gap between generated and reference text distributions using KL divergence on quantized embedding clusters.

**Applicability to Limn: LOW**

MAUVE measures distributional similarity, not individual quality. Would require a large corpus of high-quality Limn text as reference distribution. Useful only once enough Limn text exists to characterize the distribution.

### Recommendation for Limn

**Priority: Build a Limn-native embedding model.**

1. Train with custom Limn tokenizer (each word = 1 token, already built: ~2,189 tokens)
2. Use contrastive learning on English-Limn pairs (329 existing + generated pairs)
3. Add hard negatives: operator-swapped expressions (e.g., positive: `lov@fer` → "fear of love", negative: `lov*fer` → "fear of love")
4. Target: discrimination > 0.5, positive similarity > 0.6
5. Use this embedder for semantic reward scoring (replacing current 0.335 similarity)

---

## 4. Compositional Generalization Benchmarks

This is the most critical category for Limn. The core question (H1) is whether models learn operator semantics vs. memorize word-pair associations.

### 4.1 COGS (Kim & Linzen, 2020)

**What it is:** Compositional Generalization Challenge based on Semantic Parsing. Tests systematic generalization to novel combinations of known primitives.

**How it tests:** Trains on simple compositions, evaluates on:
- Novel combinations of known primitives
- Longer sequences than seen in training
- New structural configurations

**Applicability to Limn: HIGH**

COGS' test design maps directly to Limn's 3-tier evaluation:
- **Tier 1 (Interpolation)** ≈ COGS in-distribution test
- **Tier 2 (Compositional)** ≈ COGS novel combination test
- **Tier 3 (Productive)** ≈ COGS structural generalization test

**Key insight from COGS:** Transformers struggle with systematic generalization to longer sequences and novel structural configurations. This predicts that Limn models may handle simple unseen word-combos (Tier 2) but fail on nested/complex expressions with unseen words (Tier 3).

### 4.2 SCAN (Lake & Baroni, 2018)

**What it is:** Simplified Command/Action Navigation. Maps simple commands to action sequences with compositional rules.

**How it tests:** Multiple generalization splits:
- **Length split:** Train on short, test on long
- **Addprim split:** Hold out a primitive, test it in all compositions
- **Template split:** Hold out specific structural templates

**Applicability to Limn: HIGH**

The addprim split is directly applicable to H1 testing. Analog for Limn:
- Train with word `lov` in all operator contexts
- Hold out word `wis` entirely from training
- Test: Can the model apply `@`, `*`, `^`, etc. to `wis`?

If model performance drops to chance on held-out words but maintains accuracy on known words, it has memorized pairs rather than learning operators.

**Critical finding:** Lake & Baroni (2018) showed standard seq2seq models fail catastrophically on addprim splits. Modern LLMs do somewhat better but still struggle. This sets expectations for Limn SLM performance.

### 4.3 CFQ (Keysers et al., 2020)

**What it is:** Compositional Freebase Questions. Measures "compound divergence" — the distributional shift between train and test in terms of compound (multi-primitive) structures.

**Applicability to Limn: MEDIUM**

CFQ's compound divergence metric could quantify how different Tier 2/3 test sets are from training data. Currently, Limn's tier splits are ad-hoc; CFQ-style maximum compound divergence (MCD) splits would be more rigorous.

### 4.4 Ordered CommonGen (2024-2025)

**What it is:** Recent benchmark evaluating compositional generalization and instruction-following in LLMs. Uses ordered concept generation.

**Key finding:** Even the most instruction-compliant LLM achieved only ~75% ordered coverage, highlighting that compositional generalization remains hard for frontier models. LLMs show biases toward specific concept-order patterns.

**Relevance to Limn:** Limn's pipe-delimited constraint groups impose ordering constraints similar to this benchmark. Models may exhibit ordering biases in Limn too.

### 4.5 Emergent Communication Compositionality Metrics

The emergent communication literature offers metrics specifically designed to measure compositionality in learned languages — directly relevant to evaluating Limn outputs:

| Metric | What it measures | Limn relevance |
|--------|-----------------|----------------|
| **Topographic Similarity** (topsim) | Correlation between meaning space and message space distances | MEDIUM — measures if similar meanings get similar Limn expressions |
| **Positional Disentanglement** (posdis) | Whether each position in a message encodes a single meaning factor | HIGH — Limn constraint groups should disentangle semantic factors |
| **Bag-of-Symbols Disentanglement** (bosdis) | Whether each symbol encodes a single meaning factor | MEDIUM — Limn words should map to distinct semantic roles |
| **Tree Reconstruction Error** (TRE) | Whether the compositional tree can be recovered from the message | HIGH — Limn expressions have explicit operator trees |
| **Context Independence** | Whether symbol meaning is context-independent | MEDIUM — Limn operators should have fixed semantics regardless of operands |

**Key insight (Conklin & Smith, 2023):** Topographic similarity (the most common metric) doesn't correlate well with task success. TRE is the only metric that reliably identifies non-trivial compositionality. For Limn, this means: measuring if operator trees are recoverable from outputs is more diagnostic than measuring meaning-space distances.

### Recommendation for Limn

**Adapt SCAN's addprim split methodology for operator testing:**

1. **Word holdout test:** For each operator, hold out 20% of words from training. Test operator application on held-out words. Per-operator accuracy drop = memorization measure.

2. **CFQ-style compound divergence splits:** Replace ad-hoc Tier 2/3 with maximum compound divergence splits. Measure exact distributional gap between train and test.

3. **Add Tree Reconstruction Error:** Parse model outputs, reconstruct the operator tree. Compare against expected tree. TRE directly tests whether the model produces structurally correct compositions.

4. **Nested complexity gradient:** Test increasing expression depth: simple (`A@B`), medium (`(A@B)*C`), deep (`((A@B)*C)^0.5:D`). Plot accuracy vs. depth — a truly compositional model should degrade gracefully, not catastrophically.

---

## 5. Reward Modeling Approaches

The v4 reward model is broken (H5: random word salad scores 0.917-0.955 vs. genuine Limn at 0.730). The v5 semantic reward model (discrimination: 0.156) is an improvement but still weak. Modern approaches offer better alternatives.

### 5.1 Reinforcement Learning with Verifiable Rewards (RLVR)

**What it is:** Post-training paradigm using automatically verifiable signals (answer correctness, format compliance) as direct rewards. Key advance: DeepSeek-R1 (2025) demonstrated GRPO with verifiable rewards for reasoning.

**Applicability to Limn: HIGH**

Limn has multiple verifiable properties:
- **Grammar validity:** Does it parse? (FSM check — binary signal)
- **Vocabulary validity:** Are all words in the Dolt database? (binary per word)
- **Operator well-formedness:** Are operators applied to valid operands? (parser check)
- **Algebraic invariance:** Does `A*B` produce the same meaning as `B*A`? (commutativity test)

These are *deterministic, verifiable* rewards — no learned reward model needed. This is the strongest signal available for Limn training.

### 5.2 Hybrid Verification (VerIF approach)

**What it is:** Combines rule-based checkers (hard constraints) with LLM-based verification (semantic constraints).

**Applicability to Limn: HIGH**

Natural fit for Limn's dual evaluation needs:
- **Rule-based:** Grammar FSM, vocabulary lookup, operator syntax
- **LLM-based:** Semantic coherence, translation quality, compositional meaning

This is essentially what the v5 semantic reward model attempts, but formalized. The rule-based component provides the floor (structural validity), the LLM component provides the ceiling (semantic quality).

### 5.3 DPO Variants

The DPO landscape has matured significantly:

| Variant | Key property | Limn use case |
|---------|-------------|---------------|
| **DPO** (Rafailov et al., 2023) | Standard preference optimization | Baseline approach |
| **SimPO** (2024) | No reference model needed, length-normalized | Good for small-model Limn SLM (no reference model overhead) |
| **ORPO** (2024) | Robust to class imbalance | Good if genuine Limn is rare vs. garbage (which it is) |
| **KTO** (Ethayarajh et al., 2024) | Asymmetric loss (errors penalized more than correct rewarded) | Good for Limn where structural errors are catastrophic |
| **IPO** (Azar et al., 2023) | Regularized, trains to convergence | Prevents overfitting on small Limn preference datasets |

### 5.4 Generative Reward Models (GenRM)

**What it is:** Uses an LLM to generate judgments and justifications. The LLM produces both a score and a reasoning chain explaining the score. "Writing-Zero" (2025) applies this to creative writing with principle-based evaluation.

**Applicability to Limn: HIGH**

A GenRM that evaluates Limn outputs with chain-of-thought reasoning would:
1. Check vocabulary: "Is `wis` a valid Limn word? Yes, it means 'wisdom'."
2. Check operators: "`wis@dep` uses projection — extracts the depth-aspect of wisdom."
3. Check meaning: "The expression means 'deep wisdom' — this matches the intended translation."
4. Score: 0.85

This provides interpretable, auditable evaluation — critical for debugging the reward signal.

### Recommendation for Limn

**Replace v5 reward model with a 3-layer hybrid:**

| Layer | Signal | Method | Cost |
|-------|--------|--------|------|
| **L1: Verifiable** | Grammar + vocabulary | FSM parse + Dolt lookup | Free (deterministic) |
| **L2: Algebraic** | Operator properties | Commutativity/associativity tests | Free (deterministic) |
| **L3: Semantic** | Meaning quality | Claude GenRM with Limn bootstrap context | ~$0.01/eval |

**Training approach:** KTO (asymmetric loss) is the best fit for Limn's error landscape:
- Structural errors (wrong operators, invalid words) are catastrophic → heavy penalty
- Mild semantic imprecision is acceptable → lighter penalty
- KTO's Kahneman-Tversky loss function naturally models this asymmetry

---

## 6. Reference-Free Evaluation

These methods are particularly important for Limn, which often lacks reference translations.

### 6.1 Round-Trip Translation Consistency

**What it is:** Translate English → Limn → English, measure semantic similarity between original and round-trip English.

**Applicability to Limn: HIGH — this is the single most important evaluation method.**

Round-trip testing directly measures meaning preservation without needing reference Limn translations. It tests the full pipeline: does the Limn expression encode enough information to reconstruct the original meaning?

**Already planned:** v5 design Phase 4 specifies round-trip consistency reward (0.3 weight). Not yet implemented.

**Implementation:**
1. Source English → Model generates Limn
2. Limn → Claude generates English (back-translation)
3. Measure: semantic similarity (embedding cosine) between source and back-translated English
4. Bonus: syntactic parsing of the Limn intermediate (does it even parse?)

**Advantages:**
- No reference translations needed
- Tests actual meaning preservation, not surface similarity
- Works for any domain (not just HGttG)
- Can generate unlimited test data from arbitrary English text

**Limitations:**
- Lossy: some precision is inherently lost in two translations
- Expensive: requires LLM call for back-translation
- May overestimate quality if back-translator fills in missing information from context

**Mitigation for information leak:** Use a different model for back-translation than for forward translation. Or use a weaker model (e.g., Haiku) that can't "guess" what was meant from partial Limn.

### 6.2 Self-Consistency

**What it is:** Generate multiple translations for the same input, measure consistency across outputs.

**Applicability to Limn: MEDIUM**

High-quality Limn should be deterministic for simple expressions (`hot` → `hot^0.5` is always "moderately hot"). Consistency across multiple samples measures whether the model has stable compositional rules.

**Test:** Generate 10 translations of the same English sentence. Compute pairwise chrF among all Limn outputs. High variance = unstable operator application.

### 6.3 Perplexity-Based Quality Estimation

**What it is:** Use a reference language model's perplexity on the candidate text as a quality proxy. Low perplexity = more "natural" according to the reference model.

**Applicability to Limn: MEDIUM (with custom LM)**

A Limn language model trained on the HGttG corpus could provide perplexity-based quality scores. This measures "does this Limn text look like well-formed Limn?" without reference translations.

**Limitation:** Perplexity measures fluency, not adequacy. A perfectly fluent but semantically wrong Limn expression would score well.

### Recommendation for Limn

**Implement round-trip translation as the primary reference-free metric.** This is the strongest single signal for Limn quality and the most urgent gap in the current evaluation stack.

---

## 7. Formal Language Evaluation

Limn has a formal grammar (EBNF, implemented in `compositional_parser.py` and as an FSM in `grammar_constrained_generation.py`). This enables evaluation methods unavailable for natural languages.

### 7.1 Grammar-Constrained Validation

**Current state:** The v5 `LimnGrammarFSM` (10 states) validates token sequences against Limn grammar rules. Used in `semantic_reward.py` for structural scoring.

**Recent advances (2024-2025):**
- **DOMINO** (ICML 2024): Grammar constraints aligned to BPE subwords with zero overhead
- **Grammar-Aligned Decoding** (ASAp, 2024): Reweights sampling to preserve model distribution while enforcing grammar
- **XGrammar** (2024): Default backend in vLLM/NIM for EBNF-constrained generation

**Applicability to Limn: HIGH — already partially implemented.**

The FSM approach is correct. What's missing:
1. **ASAp-style distribution preservation:** Current logit masking may distort the model's semantic choices
2. **Parse tree extraction:** The FSM validates but doesn't produce a parse tree. Parse trees enable TRE (Tree Reconstruction Error) evaluation
3. **Grammar coverage metrics:** What % of valid Limn grammar is exercised by model outputs?

### 7.2 Type-Checking Analogs

**What it is:** Treat Limn operators as typed functions and verify type consistency.

**Applicability to Limn: HIGH**

Limn operators have implicit type signatures:
- `@` : (word, word) → word (projection)
- `*` : (word, word) → word (interference, commutative)
- `^` : (word, float) → word (gradient)
- `\` : (word, word) → word (subtraction)
- `±` : (word, word) → word (superposition, commutative)
- `:` : (word, word) → word (conditional)

Type-checking would catch errors like:
- `hot^big` — gradient requires a float, not a word
- `(lov@fer)^(joy*sad)` — nested operators must produce word-type results (this is actually valid)
- Missing operands: `@fer` — projection requires two operands

### 7.3 Algebraic Property Testing

Building on Limn's mathematical structure:

| Property | Test | Expected |
|----------|------|----------|
| Commutativity of `*` | `A*B` = `B*A` | Same meaning |
| Commutativity of `±` | `A±B` = `B±A` | Same meaning |
| Non-commutativity of `@` | `A@B` ≠ `B@A` | Different meanings |
| Non-commutativity of `\` | `A\B` ≠ `B\A` | Different meanings |
| Identity: `A^1.0` | `A^1.0` = `A` | Same meaning |
| Zero: `A^0.0` | `A^0.0` ≈ absence of A | Diminished meaning |
| Idempotence: `A*A` | `A*A` = `A`? | TBD — not yet formalized |

**Already tested:** v4 baseline achieves 100% (25/25) on algebraic invariance tests. This is a solved dimension.

### 7.4 Probing Classifiers (MDL Probing)

**What it is:** Train lightweight classifiers on model representations to test if specific properties are encoded. Minimum Description Length (MDL) probing (Voita & Titov, 2020) measures the "effort" needed to extract a property — lower MDL = more directly encoded.

**Applicability to Limn: HIGH**

Probe the Limn SLM's hidden states for:
- **Operator identity:** Can a linear probe predict which operator was used from the hidden state of the expression?
- **Operand identity:** Can probes recover the specific words in an expression?
- **Semantic role:** Can probes distinguish left operand from right operand in non-commutative operators?
- **Compositional structure:** Can probes recover tree depth from hidden states?

If operators are truly learned (not memorized), operator identity probes should have low MDL (easy to extract), while specific word-pair probes should have high MDL (not directly memorized).

### 7.5 Nonce Word Testing (Wug Tests for Limn)

**What it is:** Present the model with novel words (nonce words) that follow Limn's CVC phonotactics but aren't in the vocabulary. Test if the model can apply operators to them.

**Recent work:** "Morphology Matters" (2024) applied wug tests to GPT-family models across 6 languages. LLMs can generalize morphological rules to novel words, with success predicted by language complexity rather than training data size.

**Applicability to Limn: HIGH — this IS the Tier 3 test.**

The current Tier 3 (productive generalization) already tests this: unseen words with known operators. Enhance with:
1. **Completely fabricated CVC words** (e.g., `zux`, `piv`, `rok`) that follow Limn phonotactics
2. **Test:** `zux@piv` — can the model explain what projection means even for unknown words?
3. **Expected:** The model should say "the piv-aspect of zux" regardless of not knowing what `zux` or `piv` mean
4. **This tests operator understanding independent of vocabulary knowledge**

### Recommendation for Limn

**Formalize the grammar evaluation pipeline:**

1. **Parser-based validation:** Every model output must parse. Report parse rate as a hard metric.
2. **Type checking:** Verify operator type signatures on all outputs.
3. **Parse tree extraction → TRE:** Extract trees, compare against expected structure.
4. **MDL probing:** After next training run, probe hidden states for operator identity. This definitively answers H1.
5. **Fabricated nonce word tests:** Add 50 nonce CVC words to Tier 3 eval for strongest generalization test.

---

## 8. Mapping to Limn's Open Hypotheses

| Hypothesis | Best Evaluation Framework | Priority |
|------------|--------------------------|----------|
| **H1:** Operators vs. memorization | SCAN addprim splits + MDL probing + nonce word tests | CRITICAL |
| **H5:** Reward model quality | RLVR (verifiable rewards) + KTO + GenRM | CRITICAL |
| **H9:** Claude Limn reliability | G-Eval rubric + round-trip translation | HIGH |
| **H4:** Constrained decoding | Grammar-Aligned Decoding (ASAp) + parse rate metric | HIGH |
| **H11:** Information density | Round-trip fidelity + compression benchmark (already done) | MEDIUM |
| **H1/H3:** Model architecture | Compositional generalization tiers + per-operator accuracy | MEDIUM |
| **H10:** DPO convergence | KTO with verifiable + semantic rewards, track human correlation | MEDIUM |

---

## 9. Implementation Priority Matrix

### Phase 1 (Immediate — addresses H5 and current eval gaps)

| Action | Effort | Impact | Depends on |
|--------|--------|--------|------------|
| Implement round-trip translation eval | Medium | HIGH | Claude API |
| Implement RLVR verifiable reward layer | Low | HIGH | FSM (exists), Dolt (exists) |
| Add parse tree extraction to FSM | Low | MEDIUM | FSM (exists) |
| Add nonce word test cases to Tier 3 | Low | MEDIUM | eval_v5.py (exists) |

### Phase 2 (Next sprint — addresses H1 and training signal)

| Action | Effort | Impact | Depends on |
|--------|--------|--------|------------|
| Implement G-Eval rubric evaluator with Claude | Medium | HIGH | Claude API + bootstrap doc |
| SCAN-style addprim splits for operators | Medium | HIGH | Training data pipeline |
| Replace DPO with KTO | Low | MEDIUM | Preference data |
| CFQ-style compound divergence splits | Medium | MEDIUM | Training data pipeline |

### Phase 3 (Research track — deeper understanding)

| Action | Effort | Impact | Depends on |
|--------|--------|--------|------------|
| MDL probing of SLM hidden states | High | HIGH | Trained v5 model |
| Train Limn-native embedding model | High | HIGH | Custom tokenizer (exists) |
| Implement TRE metric | Medium | MEDIUM | Parse tree extraction |
| ASAp grammar-aligned decoding | Medium | MEDIUM | Constrained decoding (exists) |

---

## 10. Gap Analysis: What Limn Has vs. What It Needs

| Eval Dimension | Current State | Gap | Solution |
|----------------|--------------|-----|----------|
| **Structural validity** | FSM validation in semantic_reward.py | No parse tree extraction, no TRE | Add parser output to FSM |
| **Vocabulary validity** | Tokenizer + Dolt check | Adequate | — |
| **Operator correctness** | Keyword overlap (score_composition_response) | Doesn't verify actual semantic correctness | LLM-as-judge with rubric |
| **Semantic fidelity** | Embedder cosine similarity (0.335) | Too low for reliable scoring | Custom Limn embedder + round-trip |
| **Compositional generalization** | 3-tier eval (v5) | Scoring function too lenient (inverted pass rates) | SCAN splits + harder scoring |
| **Translation quality** | No metric | Complete gap | Round-trip translation + G-Eval |
| **Training signal** | Semantic reward (disc=0.156) | Too weak to drive DPO reliably | RLVR + KTO + GenRM |
| **Algebraic properties** | 100% on invariance tests | Solved | Maintain |

---

## References

### LLM-as-Judge
- Liu et al. (2023). "G-Eval: NLG Evaluation using GPT-4 with Better Human Alignment." arXiv:2303.16634
- Zheng et al. (2023). "Judging LLM-as-a-Judge with MT-Bench and Chatbot Arena." NeurIPS 2023
- Ye et al. (2023). "FLASK: Fine-grained Language Model Evaluation based on Alignment Skill Sets." arXiv:2307.10928

### Machine Translation
- Papineni et al. (2002). "BLEU: A Method for Automatic Evaluation of Machine Translation." ACL 2002
- Banerjee & Lavie (2005). "METEOR: An Automatic Metric for MT Evaluation." ACL Workshop 2005
- Popović (2015). "chrF: Character n-gram F-score for Automatic MT Evaluation." WMT 2015
- Zhang et al. (2020). "BERTScore: Evaluating Text Generation with BERT." ICLR 2020
- Rei et al. (2020). "COMET: A Neural Framework for MT Evaluation." EMNLP 2020

### Compositional Generalization
- Lake & Baroni (2018). "Generalization without Systematicity." ICML 2018 (SCAN)
- Kim & Linzen (2020). "COGS: A Compositional Generalization Challenge." EMNLP 2020
- Keysers et al. (2020). "Measuring Compositional Generalization: A Comprehensive Method on Realistic Data." ICLR 2020 (CFQ)
- Chaabouni et al. (2020). "Compositionality and Generalization in Emergent Languages." ACL 2020

### Reward Modeling
- Rafailov et al. (2023). "Direct Preference Optimization." NeurIPS 2023
- Ethayarajh et al. (2024). "KTO: Model Alignment as Prospect Theoretic Optimization." ICML 2024
- Hong et al. (2024). "ORPO: Monolithic Preference Optimization without Reference Model." arXiv:2403.07691
- Meng et al. (2024). "SimPO: Simple Preference Optimization." arXiv:2405.14734

### Formal/Structural
- Voita & Titov (2020). "Information-Theoretic Probing with MDL." EMNLP 2020
- Conklin & Smith (2023). "Compositionality metrics in emergent communication." ACL 2023

### Grammar-Constrained Generation
- DOMINO (ICML 2024): Grammar constraints aligned to BPE subwords
- Grammar-Aligned Decoding / ASAp (2024): Distribution-preserving grammar enforcement
- XGrammar (2024): Production-ready EBNF-constrained generation

---

```limn
fra sur | too fit | qal ris | tru eme
> frameworks surveyed | tools fitted | quality rises | truth emerges
```

*— Nux*
