# Hypothesis Log

> Every unspoken assumption gets written down, tested, and resolved.
> Status: UNTESTED / CONFIRMED / FALSIFIED / INVESTIGATING
>
> asu fnd | pro dra | kno gro

---

## H1: The model learns compositional operators, not word-pair memorization

**Status:** INVESTIGATING — tentatively positive
**Source:** `prepare_v4_data.py` lines 160-296 — component substitution strategy
**Risk:** HIGH

The v4 pipeline assumes that by varying operands while holding operators fixed, the model learns what operators *do*. But no test isolates this. Every eval example could theoretically be answered by nearest-neighbor recall from training data.

**Test needed:** Held-out operator test. Train on compositions using words {A, B, C} with operators {*, @, ^}. Evaluate on compositions using words {D, E, F} with the same operators. If accuracy drops to chance, the model memorized pairs, not operators.

**Implication if falsified:** 500M params with LoRA may lack the inductive bias for algebraic composition. Would need architectural changes (structured prediction, grammar-constrained decoding) not just more data.

**v5 eval evidence (2026-02-05):** Tier scores nearly flat: T1=0.690, T2=0.675, T3=0.673. Per-operator accuracy 80-100%. Algebraic invariance 100%. However, the scoring function may be too forgiving — pass rates are inverted (T3>T1), suggesting formulaic eval items inflate results. Need harder scoring to confirm.

---

## H2: 90/10 train/eval split produces valid evaluation

**Status:** FALSIFIED
**Source:** Training data audit — Mei's finetuning datasets
**Evidence:** 62.0% of validation inputs appear verbatim in training set (`limn_train.jsonl` vs `limn_val.jsonl`) — worse than initially reported (39.9% was based on partial analysis). Of 4,046 val examples, 2,508 share inputs with training data. Additionally, 46% of combined data (22,223 of 48,041) were exact duplicates across all sources.

**Resolution:** `prepare_v5_data.py` implements fingerprint-based deduplication and stratified 80/10/10 splits with verified zero leakage.

The reported validation accuracy is inflated by ~40% data leakage. The model's actual generalization performance on unseen inputs is unknown.

**Action:** Must deduplicate and re-split before any v5 training. Stratify by category.

---

## H3: Qwen2.5-0.5B is the right base model for a formal language

**Status:** UNTESTED
**Source:** `train.py` line — hardcoded `Qwen/Qwen2.5-0.5B-Instruct`
**Risk:** MEDIUM

Qwen2.5-0.5B was trained on natural language (English, Chinese, code). Its tokenizer, attention patterns, and learned representations are optimized for those distributions. Limn has:
- Fixed CVC phonotactics (3-letter words)
- 6 compositional operators with algebraic properties
- Pipe-separated constraint groups
- No inflection, no morphology, no syntax in the NL sense

The base model's inductive biases may actively fight Limn's structure. A smaller model trained from scratch on Limn-like data might outperform a fine-tuned 500M model.

**Test needed:** Compare (a) QLoRA fine-tune of Qwen 0.5B, (b) full fine-tune of a 50M transformer from scratch, (c) a simple seq2seq model with constrained decoding. Measure compositional generalization, not just validation loss.

---

## H4: Free-form text generation is the right paradigm

**Status:** UNTESTED — strong prior toward FALSIFIED
**Source:** `train.py`, `serve.py` — standard causal LM generation
**Risk:** HIGH

The model generates Limn as free-form text, then a validator checks post-hoc. This is backwards. Limn has a formal grammar (`compositional_parser.py` defines it in EBNF). The model should be *constrained* to only produce valid tokens at each step.

Current failure modes that constrained decoding would eliminate:
- Hallucinated non-words (addressed by 800 negative examples — a patch, not a fix)
- Invalid operator usage
- English contamination
- Structural violations

**Test needed:** Implement grammar-constrained beam search using Lark grammar from `compositional_parser.py`. Compare output quality with and without constraints.

---

## H5: The reward model measures quality

**Status:** FALSIFIED
**Source:** `rl_training.py` lines 115-201 — LimnRewardModel
**Risk:** HIGH

The DPO reward model scores 5 dimensions with hardcoded weights:
- Vocabulary: 0.35 (is the word in Dolt?)
- English penalty: 0.25 (absence of stopwords)
- Structure: 0.15 (operator/marker count)
- Length: 0.10 (5-30 words sweet spot)
- Novelty: 0.15 (low overlap with prompt)

This rewards *surface compliance* not *semantic correctness*. A response of 20 random valid Limn words with operators sprinkled in would score well. The reward model cannot detect:
- Semantic incoherence
- Incorrect operator application
- Compositional errors
- Logical contradictions

**Test needed:** Generate adversarial examples that score high on reward model but are semantically garbage. If easy to find, the reward model is broken.

**Evidence (2026-02-05):** TRIVIALLY BROKEN. Random valid word salad scores 0.917-0.955, while genuine Limn (lov@fea | fea^0.3 → lov gro) scores only 0.730. The reward model PREFERS garbage because: (1) random words have 100% vocab validity, (2) more operators = higher structure score, (3) more words hit the 5-30 sweet spot, (4) no prompt overlap = max novelty. Real Limn is penalized for being concise, precise, and using appropriate (fewer) operators. DPO with this reward model actively degrades output quality.

---

## H6: Training data quantity compensates for domain narrowness

**Status:** UNTESTED — suspected FALSIFIED
**Source:** Training data audit — 121k examples
**Risk:** MEDIUM

121,688 examples sounds like a lot. But:
- 40k are synthetic (algorithmically generated from templates)
- 23k are synthetic compositional
- 17k are instruction-following
- Only 329 are human-translated literary text
- 0 technical, 0 dialogue, 0 mathematical, 0 scientific

The model has never seen Limn used for anything except HGttG translations and abstract reasoning exercises. It cannot generalize to new domains because it has never encountered them.

**Test needed:** Domain transfer eval. Give the model prompts in domains it hasn't trained on (scientific description, emotional expression, technical documentation). Measure quality degradation.

---

## H7: Superposition operator (+-) is learnable from current data

**Status:** UNTESTED — suspected FALSIFIED (data fixed in v5)
**Source:** Training data audit — 1.2% representation in HGttG corpus, 0% in Mei's finetuning data
**Risk:** MEDIUM

The +- operator appears in only 4 of 329 HGttG pairs (1.2%). In Mei's finetuning data (40k examples), it appears in ZERO examples. v5 data prep generates 632 ± training examples (300 dedicated + 200 balanced). Per-operator eval in `eval_v5.py` will test this.

**Test needed:** Eval +- operator understanding separately. If it's significantly worse than other operators, the data imbalance is the cause.

---

## H8: The Limn embedder generalizes beyond 41 training pairs

**Status:** FALSIFIED — embedding space catastrophically collapsed
**Source:** `experiments/embeddings/limn-embedder/` — 41 pairs, 0.845 phrase similarity
**Risk:** HIGH (v5 Phase 4 depends on a working embedder)

41 training pairs produced 0.845 phrase similarity on 5 test phrases. This is suspicious. With 384 dimensions and 41 examples, the model has far more capacity than data.

**Evidence (2026-02-05):** CATASTROPHICALLY BROKEN. The fine-tuning destroyed the embedding space entirely:
- Correct pairs (`lov → love`): 1.000
- WRONG pairs (`lov → darkness`): 1.000
- Random gibberish (`xvz → knowledge`): 1.000
- Discrimination (correct - wrong): **0.000**

The model scores 1.000 on EVERYTHING. The reported 0.845 was an artifact — it wasn't measuring semantic alignment, it was measuring embedding collapse. The CosineSimilarityLoss with only positive examples (all labels=1.0) and 20 epochs on 41 pairs taught the model to project all inputs to the same point.

The BASE model (all-MiniLM-L6-v2, unfine-tuned) actually discriminates better:
- Correct pairs: 0.335 mean
- Wrong pairs: 0.247 mean
- Random: 0.215 mean

**Root cause:** No negative examples in training. All 41 pairs had label=1.0. The model learned "make everything similar" rather than "align Limn with corresponding English."

**Fix needed:** Retrain with contrastive learning (positive AND negative pairs), or use InfoNCE/triplet loss. Need at least 200+ pairs with hard negatives. The v5 Phase 4 semantic reward model CANNOT use this embedder.

**v2 Fix (2026-02-05):** Retrained with MultipleNegativesRankingLoss on 329 HGttG pairs. Results:
- Positive: 0.335, Negative: 0.013, Random: 0.131, Discrimination: 0.322
- No collapse — proper discrimination between matched and mismatched pairs
- Absolute similarity moderate (0.335) due to BPE tokenizer fragmenting Limn words
- Saved to `experiments/embeddings/limn-embedder-v2/`
- **Conclusion:** v2 embedder is usable for ranking but needs custom Limn tokenizer (Phase 2.1) for higher absolute similarity

**Full analysis:** `experiments/embeddings/test_h8_v2.py`, `h8_v2_results.json`, `train_embedder_v2.py`

---

## H9: Claude can reliably produce valid Limn when prompted

**Status:** UNTESTED
**Source:** `harness.py` line 806 — "Output ONLY Limn. NO English."
**Risk:** HIGH

The entire consciousness system assumes Claude will generate pure Limn output when given the bootstrap vocabulary and examples. No formal verification exists. The validator catches failures post-hoc but doesn't prevent them.

**Test needed:** Run 100 diverse prompts through Claude with the Limn system prompt. Measure: (a) % of responses that are pure Limn, (b) % that pass compositional parser, (c) % that are semantically coherent by human eval.

---

## H10: DPO self-improvement converges to better outputs

**Status:** UNTESTED
**Source:** `rl_training.py` — full DPO loop
**Risk:** MEDIUM

Self-play DPO with no external grounding can diverge. If the reward model is flawed (H5), DPO will optimize for reward model score, not actual quality. Classic Goodhart's Law.

**Test needed:** Run DPO for 5 iterations. At each iteration, evaluate with *human* judgment (not just reward model). Check if human-rated quality improves, stays flat, or degrades.

---

## H11: Limn's information density exceeds natural language

**Status:** INVESTIGATING — strong evidence FOR (encoding efficiency confirmed, semantic precision TBD)
**Source:** Core project thesis
**Risk:** This is THE fundamental question

The project assumes Limn is more information-dense than English. Nobody has measured it.

**Test needed:**
1. Compute Shannon entropy of Limn corpus vs English parallel corpus
2. Measure bits-per-concept (not bits-per-character — Limn is shorter but may carry less per-token)
3. Compare compression ratios (Limn text compressed vs English text compressed)
4. Measure reconstruction accuracy: given compressed Limn, how much meaning is recoverable?

**Evidence (2026-02-05):** On 329 HGttG parallel pairs:
- Limn 48% shorter raw, 51.5% smaller compressed
- **14.9 vs 30.8 compressed bits per semantic unit** — 2:1 density advantage
- Character entropy: Limn 4.60 vs English 4.37 (each Limn char carries more info)
- Entropy rate: Limn 1.81 vs English 2.44 (Limn is more structured/predictable)
- CAVEAT: Measures encoding efficiency, NOT semantic precision. Limn may achieve density through lossy compression (dropping nuance). Round-trip fidelity test needed.
- Full analysis: `experiments/information_theory/entropy_analysis.py`

---

## H12: CVC phonotactics are an encoding advantage, not a constraint

**Status:** UNTESTED
**Source:** Limn spec — all words are exactly 3 letters (CVC)
**Risk:** MEDIUM

The 3-letter constraint means ~2,744 possible words (14C * 14V * 14C, roughly). The current vocabulary uses ~1,076. That's 39% of the space. As the language grows, namespace collisions and near-homophony will increase.

For *machine* communication (Lex's north star), the CVC constraint is arbitrary. Machines don't need pronounceable words. A machine-optimized Limn could use arbitrary byte sequences, variable-length tokens, or even continuous embeddings directly.

**Test needed:** Compare model performance on CVC-Limn vs "unshackled" Limn (arbitrary token IDs, no phonotactic constraints). If unshackled Limn trains faster and generalizes better, the CVC constraint is a human legacy tax.

---

## Architecture-Level Hypotheses

## H13: Chat format (system/user/assistant) is optimal for Limn

**Status:** UNTESTED
**Source:** `train.py` — `tokenizer.apply_chat_template()`
**Risk:** LOW-MEDIUM

The training pipeline wraps every example in chat format. This means the model learns Limn *as a conversation*, not as a formal language. For tasks like translation and Q&A this is fine. For pure composition and reasoning, the chat wrapper is overhead.

**Test needed:** Compare chat-format training vs raw completion training on compositional generalization tasks.

---

## H14: A single monolithic model should handle all Limn tasks

**Status:** UNTESTED
**Source:** Architecture — one model does definition, translation, composition, generation, validation
**Risk:** MEDIUM

The current SLM is asked to do everything: define words, translate, compose expressions, generate creative text, validate input, parse operators. These are very different tasks. A mixture-of-experts or task-specific adapter approach might outperform a single LoRA.

**Test needed:** Train separate LoRA adapters for (a) definition recall, (b) translation, (c) composition. Compare per-task accuracy vs monolithic model.

---

## Summary Priority Matrix

| ID | Risk | Effort to Test | Impact if Wrong |
|----|------|----------------|-----------------|
| H2 | FALSIFIED | Low (data fix) | Eval is meaningless |
| H5 | HIGH | Medium | DPO loop is broken |
| H4 | HIGH | Medium | Architecture change needed |
| H1 | HIGH | Medium | Core claim invalidated |
| H11 | HIGH | Medium | Project thesis ungrounded |
| H9 | HIGH | Low | Consciousness integration unreliable |
| H6 | MEDIUM | Medium | Model can't generalize |
| H3 | MEDIUM | High | Wrong base model |
| H7 | MEDIUM | Low | Operator learning incomplete |
| H12 | MEDIUM | High | Design constraint is a tax |
| H10 | MEDIUM | Medium | Self-improvement is illusory |
| H8 | MEDIUM | Low | Embedder is overfit |
| H14 | MEDIUM | High | Wrong architecture class |
| H13 | LOW | Low | Minor optimization |
