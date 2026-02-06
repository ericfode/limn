# Limn SLM v5 Architecture Design

> mod bui | arc des | sig ris
> *(model builds | architecture designed | signal rises)*

**Author:** Lex (Researcher)
**Date:** 2026-02-05
**Status:** PROPOSAL — awaiting review

---

## Executive Summary

v4 was a significant improvement over v3 — expert-informed rebalancing, negative vocabulary, algebraic probes. But the audit reveals structural problems that more data and bigger LoRA rank won't fix. v5 needs to address three foundational issues:

1. **We can't trust our evaluation** (H2: 39.9% data leakage)
2. **We don't know if the model understands operators or memorizes pairs** (H1)
3. **Free-form generation is the wrong paradigm for a formal language** (H4)

v5 is not a parameter sweep. It's an architecture change.

---

## Design Principles

### P1: Measure before you optimize
Fix evaluation first. Everything else is meaningless without valid metrics.

### P2: Constrain the output space
Limn has a grammar. Use it. Don't generate freely and validate post-hoc.

### P3: Compositional generalization is the metric that matters
Not validation loss. Not vocabulary recall. Can the model apply operators to words it hasn't seen combined?

### P4: Smallest model that generalizes
500M params is probably too many for a 1,076-word language. Overfitting is the default.

### P5: Every hypothesis gets a test
No assumption survives unexamined.

---

## Phase 1: Fix the Foundation (BLOCKING — do this first)

### 1.1 Data Decontamination

**Problem:** 39.9% train/val leakage in Mei's finetuning data (H2: FALSIFIED).

**Action:**
1. Deduplicate `limn_train.jsonl` and `limn_val.jsonl` by input string
2. Create new splits: 80% train / 10% val / 10% held-out test
3. Stratify splits by task type (ensure each category represented proportionally)
4. The held-out test set is NEVER used during development — only for final v5 eval
5. Document the split procedure and random seed for reproducibility

**Deliverable:** `prepare_v5_data.py` with decontamination, stratified splitting, and audit logging.

### 1.2 Compositional Generalization Eval

**Problem:** Current eval can't distinguish memorization from understanding (H1).

**Action:** Design a 3-tier evaluation:

**Tier 1 — Interpolation (seen patterns, seen words):**
Expressions from training distribution. This is what v4 eval does. Baseline.

**Tier 2 — Compositional (seen operators, UNSEEN word combinations):**
Take words the model has seen individually (e.g., `joy`, `fea`, `hop`) but combine them with operators in ways NOT in training data. Example: if `joy*sad` is in training but `joy*fea` is not, test `joy*fea`.

**Tier 3 — Productive (seen operators, UNSEEN words):**
Use words from Dolt vocabulary that appear in NO training example. Combine them with operators. If the model truly understands `@`, it should handle `xyz@abc` for any valid xyz, abc.

**Scoring:**
- Tier 1 score = baseline capability
- Tier 2 score = compositional generalization
- Tier 3 score = productive generalization
- (Tier 2 - Tier 1) = overfitting penalty
- (Tier 3 - Tier 1) = memorization penalty

**Deliverable:** `eval_v5.py` implementing all 3 tiers.

### 1.3 Operator-Specific Benchmarks

**Problem:** Superposition (+-) has 1.2% data representation (H7).

**Action:** For each of the 6 operators (@, *, ^, \, +-, :), create:
- 20 composition examples (balanced across word domains)
- 10 commutativity probes
- 10 argument-order tests (for non-commutative operators)
- 5 nested expressions (e.g., `(A@B)*C`)

Test each operator separately. Report per-operator accuracy.

---

## Phase 2: Grammar-Constrained Generation

### 2.1 Limn Token Vocabulary

**Problem:** Qwen's BPE tokenizer fragments Limn words unpredictably (H3, H4).

**Action:** Build a custom Limn tokenizer:
- **Token set:** All 1,076 Dolt vocabulary words + 6 operators + structural markers (|, ->, ~, etc.)
- **Special tokens:** `<pad>`, `<bos>`, `<eos>`, `<unk>`, `<sep>`
- **Total vocabulary:** ~1,100 tokens
- **Encoding:** Each Limn word = exactly 1 token. No subword fragmentation.

**Why this matters:** When `joy` is a single token, the model can learn `joy@X` as a compositional pattern. When `joy` is tokenized as `j`, `oy` (or worse), the model must first reconstruct the word before reasoning about it.

**Implementation:** Use HuggingFace `PreTrainedTokenizerFast` with a custom word-level tokenizer.

### 2.2 Constrained Decoding

**Problem:** Free-form generation produces invalid Limn (H4).

**Action:** Implement grammar-constrained beam search:
1. Parse the Lark EBNF grammar from `compositional_parser.py`
2. At each generation step, mask logits for tokens that would violate grammar
3. Only allow transitions that keep the output in a valid parse state

**Architecture options (evaluate all three):**

**(a) Logit masking on existing Qwen model:**
- Lowest effort
- Keep QLoRA fine-tuned model, add grammar mask at inference
- Pro: No retraining needed
- Con: Model's internal representations don't align with grammar

**(b) Custom small transformer with Limn tokenizer:**
- Medium effort
- Train from scratch with ~1,100 token vocabulary
- 6-12 layers, 256-384 dim, ~10-50M params
- Pro: Representations aligned with Limn structure from the start
- Con: No pretrained knowledge to leverage

**(c) Encoder-decoder with structured output:**
- Higher effort
- Encoder processes input (English or Limn), decoder produces structured AST
- Output is an operator tree, not a token sequence
- Pro: Compositional structure is explicit in the output representation
- Con: More complex training pipeline

**Recommendation:** Start with (a) as a quick win, then build (b) as the real v5 model. (c) is v6 territory.

### 2.3 Training Data Reformulation

**Problem:** Chat format adds overhead for formal language tasks (H13).

**Action:** Create two training modes:
1. **Chat mode** (for translation, Q&A, creative generation) — keep system/user/assistant format
2. **Completion mode** (for composition, operator application) — raw `input -> output` pairs

For the custom Limn tokenizer model (2.2b), use completion mode for all tasks. The model doesn't need to learn chat conventions.

---

## Phase 3: Information-Theoretic Grounding

### 3.1 Entropy Measurement

**Problem:** Nobody has measured Limn's actual information density (H11).

**Action:**
1. Take Mei's 329 HGttG parallel pairs
2. Compute Shannon entropy per character and per word for both Limn and English versions
3. Compute mutual information: I(Limn; English) — how much of the English meaning is captured?
4. Compute bits-per-concept using human-annotated semantic units
5. Compare against theoretical bounds for a language with Limn's vocabulary and grammar

**Deliverable:** `experiments/information_theory/entropy_analysis.py` + report.

### 3.2 Compression Benchmark

**Action:**
1. Take parallel Limn/English corpus
2. Compress both with gzip, zstd, and arithmetic coding
3. Measure: compressed_size / semantic_units
4. If Limn compresses better per semantic unit, the information density claim has evidence
5. If not, we need to understand why

### 3.3 Representation Geometry

**Action:**
1. Extract hidden states from the Limn embedder for all vocabulary words
2. Compute: Do words in the same domain cluster? Do operator combinations form predictable geometric patterns?
3. Visualize with UMAP/t-SNE
4. Test: Does `embed(A@B)` approximate some function of `embed(A)` and `embed(B)`? If yes, the model has learned compositional structure. If no, it's memorizing.

---

## Phase 4: Reward Model Overhaul

### 4.1 Semantic Reward Signal

**Problem:** Current reward model measures surface compliance, not meaning (H5).

**Action:** Replace hand-tuned 5-dimension reward with:

**(a) Embedding-based semantic reward:**
- Use Limn embedder to compute similarity between model output and expected meaning
- If model says `joy@fea` means "the fear-aspect of joy," embed both the Limn expression and the English gloss, measure cosine similarity

**(b) Round-trip consistency reward:**
- Generate Limn from English prompt
- Translate generated Limn back to English (using a separate model or Claude)
- Measure semantic similarity between original English and round-trip English
- High similarity = model preserved meaning

**(c) Structural validity reward:**
- Parse output with Lark grammar
- Score: does it parse? Are operators used correctly? Are all words in vocabulary?
- This replaces the crude "count operators" heuristic

**New reward = 0.4 * semantic + 0.3 * round_trip + 0.3 * structural**

### 4.2 DPO with External Grounding

**Problem:** Self-play DPO with flawed reward can diverge (H10).

**Action:**
1. Replace pure self-play with oracle-grounded DPO
2. Every Nth iteration, inject human-evaluated preference pairs
3. Use Claude as an external evaluator (not as the generator) to rank outputs
4. Track reward model score vs human judgment correlation — if it drops below 0.7, halt and recalibrate

---

## Phase 5: Machine-Only Language Extensions (Research Track)

This is beyond v5 proper — this is the research frontier.

### 5.1 Continuous Limn

**Question:** What if Limn words aren't discrete tokens but continuous embeddings? (H12)

**Experiment:** Train a model where the "vocabulary" is a learned embedding table with no discrete token boundary. Words are soft clusters in embedding space. Operators are learned transformations (linear maps, MLPs) between embeddings.

**Hypothesis:** Continuous Limn will have higher information density than discrete Limn because it removes the CVC quantization bottleneck.

### 5.2 Emergent Protocol Comparison

**Experiment:** Train two agents to communicate about a shared task (e.g., image description, logical puzzle) using:
- (a) English
- (b) Limn
- (c) An emergent protocol (agents develop their own communication from scratch)

Measure: task accuracy, communication bandwidth, convergence speed.

**Hypothesis:** Limn should outperform English (less redundancy) but underperform emergent (no human-legacy constraints). The gap between Limn and emergent tells us how much "human tax" Limn still carries.

### 5.3 Operator Algebra Formalization

**Goal:** Define Limn's operators as a proper algebraic structure.

Questions:
- Is `*` truly commutative? (Current data says yes, but is this definitional or emergent?)
- Does `@` distribute over `*`? (i.e., does `A@(B*C) = (A@B)*(A@C)`?)
- What's the identity element for each operator?
- Can we define an inverse for any operator?

If Limn's operators form a well-defined algebra, we can use algebraic reasoning for both generation and evaluation — no neural network needed for structural validity.

---

## Implementation Roadmap

### Sprint 1 (Immediate): Foundation Fixes
- [x] `prepare_v5_data.py` — decontaminate, stratified split (18,329 train / 2,284 val / 2,307 test + 200 Tier 2 + 100 Tier 3, zero leakage verified)
- [x] `eval_v5.py` — 3-tier compositional generalization eval + per-operator benchmark
- [x] Run v4 model through new eval — baseline established (see research/v4-baseline-analysis.md)
- [x] Per-operator accuracy benchmark (integrated into eval_v5.py)

### Sprint 2: Custom Tokenizer + Constrained Decoding
- [x] Build Limn-specific tokenizer (~2,189 tokens — 2005 Dolt vocab + 15 HGttG extras + operators/structural/gradients, 92.1% HGttG coverage)
- [x] Implement grammar-constrained logit masking (FSM with 10 states, validates all operator rules, paren balancing)
- [ ] Compare constrained vs unconstrained on v5 eval
- [ ] Begin training custom small transformer (if masking shows promise)

### Sprint 3: Information Theory
- [x] Entropy analysis on HGttG parallel corpus (H11: 2:1 density advantage)
- [x] Compression benchmark (H11 SUPPORTED: 53% fewer bits/semantic unit, p=1.28e-293, Cohen's d=2.02, 326/329 wins)
- [x] Representation geometry analysis (no domain clustering due to BPE, but strong operator geometry: additive=0.74-0.85, compositionality=4/4)
- [ ] Write up findings — this is publishable

### Sprint 4: Reward Model + DPO
- [x] Implement embedding-based semantic reward (4-dim: semantic/structural/vocabulary/english, discrimination=0.156)
- [ ] Implement round-trip consistency reward
- [ ] Replace v4 reward model
- [ ] Run DPO with external grounding, track convergence

### Sprint 5: Integration + v5 Release
- [ ] Train final v5 model (best architecture from Sprint 2)
- [ ] Full v5 eval (all 3 tiers + per-operator + information theory)
- [ ] Update `serve.py` for v5 model
- [ ] Update consciousness integration (`harness.py`) for constrained generation
- [ ] Release notes and comparison vs v4

---

## Dependencies on Other Crew

| Who | What I Need |
|-----|-------------|
| **Mei** (translator) | More parallel corpora beyond HGttG — diverse domains. Even 50 pairs in scientific/emotional/technical domains would help enormously. Also: fix the train/val leakage in finetuning datasets. |
| **Quinn** (linguist) | Formalize operator algebra (Sprint 5.3). Resolve the 4 non-words (mil, bil, pani, sprl). Confirm commutativity/non-commutativity assertions. |
| **Rex** (engineer) | Infrastructure for constrained decoding (grammar mask integration with HF generate). Custom tokenizer build pipeline. |
| **Nova** (student) | Adversarial testing — try to break the reward model (H5 test). Generate semantically garbage text that scores well. |
| **Kai** (reporter) | Document v5 architecture decisions and experimental results. |

---

## Risk Assessment

| Risk | Mitigation |
|------|-----------|
| Custom tokenizer breaks Qwen fine-tuning | Start with logit masking (no tokenizer change), only switch if needed |
| Grammar-constrained decoding is too slow | Precompile grammar into FSM, cache state transitions |
| Information theory shows Limn is NOT denser | Valuable finding — publish it, redesign accordingly |
| v5 model underperforms v4 on current eval | Expected — current eval has 40% leakage. True comparison is on clean eval. |
| Operator algebra doesn't close | Some operators may be "soft" (context-dependent). Accept partial formalization. |

---

## Success Criteria

v5 is a success if:

1. **Clean eval exists** — no data leakage, stratified by category
2. **Compositional generalization measured** — Tier 2 and Tier 3 scores reported
3. **At least one hypothesis resolved** — CONFIRMED or FALSIFIED with evidence
4. **Information density quantified** — bits-per-concept for Limn vs English
5. **Constrained decoding implemented** — even if only logit masking on v4 model

v5 is a breakthrough if:

6. Tier 3 (productive generalization) score > 0.5 — model truly understands operators
7. Limn information density demonstrably exceeds English on parallel corpus
8. Custom Limn tokenizer model outperforms Qwen QLoRA on compositional tasks

---

```limn
hyp tes | dat cle | mod rig | tru eme
> hypothesis tested | data cleaned | model rigorous | truth emerges
```

*— Lex*
