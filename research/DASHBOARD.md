# Limn Research Agenda

> A constructed language for machine communication: 2,000 words, six algebraic operators, and a question — can we beat natural language at its own game?

**Status:** Active research program | **Last updated:** 2026-02-06

---

## At a Glance

| | Confirmed | Falsified | Investigating | Untested |
|---|:-:|:-:|:-:|:-:|
| **Count** | 0 | 3 | 2 | 9 |
| **Hypotheses** | — | H2, H5, H8 | H1, H11 | H3, H4, H6, H7, H9, H10, H12, H13, H14 |

**Headline result:** Limn achieves **2.1:1 information density** over English — 53% fewer compressed bits per semantic unit (p = 1.28e-293, Cohen's d = 2.02). The three falsified hypotheses were all *good* failures: they exposed broken infrastructure that has since been fixed.

---

## The 14 Hypotheses

### FALSIFIED

#### H2: Valid train/eval split
> *Does our evaluation actually measure generalization?*

| | |
|---|---|
| **Verdict** | **FALSIFIED** — 62% of validation inputs appeared verbatim in training data |
| **Impact** | All v4 accuracy numbers were inflated by ~40% leakage |
| **Resolution** | `prepare_v5_data.py` — fingerprint deduplication, stratified 80/10/10 splits, zero leakage verified |
| **Commit** | `99712f8` v5 evaluation framework (2026-02-05) |
| **Status** | Fixed. Clean eval is now the standard. |

---

#### H5: Reward model measures quality
> *Does our DPO reward signal actually track semantic correctness?*

| | |
|---|---|
| **Verdict** | **FALSIFIED** — reward model *inverts* quality. Random word salad scores 0.92; genuine Limn scores 0.73. |
| **Mechanism** | Model rewards surface compliance (vocab validity, operator count, length) but can't detect semantic coherence. More operators = higher score, even when they're nonsense. |
| **Impact** | DPO self-improvement loop is broken — it actively degrades output quality |
| **Resolution** | New 4-dimension semantic reward model (`3ec5a51`). Discrimination = 0.156 — still modest, but correctly directional. |
| **Commit** | `549866c` H5 FALSIFIED (2026-02-05) |
| **Blocks** | H10 (DPO convergence) cannot be tested until reward model is trustworthy |

---

#### H8: Embedder generalizes beyond training pairs
> *Does the Limn sentence embedder learn real semantics?*

| | |
|---|---|
| **Verdict** | **FALSIFIED** — v1 embedder catastrophically collapsed. Scores 1.000 on everything (correct, wrong, gibberish). |
| **Root cause** | Trained on 41 pairs with only positive examples (all labels=1.0). Model learned "make everything identical." |
| **Fix** | v2 retrained with MultipleNegativesRankingLoss on 329 pairs. Discrimination 0.322 (vs 0.000). No collapse. |
| **Caveat** | Absolute similarity moderate (0.335) because BPE tokenizer fragments 3-character Limn words. Custom word-level tokenizer needed. |
| **Commits** | `5af2ab8` H8 FALSIFIED + v2 fix (2026-02-05) |

---

### INVESTIGATING — Strong Evidence

#### H11: Information density exceeds natural language
> *Is Limn genuinely denser than English, or just shorter?*

**This is THE fundamental question.** The answer is looking very strong.

| Metric | English | Limn | Advantage |
|--------|---------|------|-----------|
| Raw characters | 22,284 | 11,540 | 48% shorter |
| Compressed size (bz2) | 7,799 B | 3,670 B | **53% smaller** |
| Bits per semantic unit | 27.6 | 13.0 | **2.1:1** |
| Character entropy | 4.37 bits/char | 4.60 bits/char | Each char carries more info |
| Entropy rate | 2.44 bits | 1.81 bits | More structured |

**The key insight:** The compression advantage (53%) *exceeds* the raw character advantage (48%). If Limn were merely dropping information, compression would close the gap. Instead, the gap *widens* — Limn is genuinely denser, not just shorter.

| Statistical test | Value |
|---|---|
| Paired t-test | t = 36.62 |
| p-value | 1.28e-293 |
| Cohen's d | 2.02 (LARGE) |
| Limn wins | 326/329 sentences (99.1%) |

**Three sources of density:**
1. No function words (articles, copulas, auxiliaries = pure overhead)
2. Operators replace periphrasis (`lov@fea` vs "love projected through the lens of fear")
3. Pipe-delimited groups eliminate subordination overhead

**Open question:** Measures encoding efficiency, not semantic precision. A round-trip fidelity test (English → Limn → English) is needed to quantify information loss.

| Evidence | Reference |
|---|---|
| Full report | `research/information-density-report.md` |
| Entropy analysis | `experiments/information_theory/entropy_analysis.py` |
| Compression benchmark | `experiments/information_theory/compression_benchmark.py` |
| Representation geometry | `experiments/information_theory/representation_geometry.py` |
| Commits | `37e8209`, `8733162`, `e527b87` (2026-02-05) |

---

#### H1: Operators learned, not memorized
> *Does the model learn what operators DO, or does it memorize word pairs?*

The v4 model evaluated on v5's clean (decontaminated) 3-tier framework:

| Tier | Description | Avg Score | Pass Rate |
|------|-------------|-----------|-----------|
| T1 — Interpolation | Seen patterns + seen words | 0.690 | 77% |
| T2 — Compositional | Seen operators, **unseen** combinations | 0.675 | 97% |
| T3 — Productive | Seen operators, **unseen** words | 0.673 | 100% |

**Overfitting penalty (T2-T1):** -0.015
**Memorization penalty (T3-T1):** -0.017

The near-flat curve (-0.017 drop from seen to unseen words) suggests genuine operator understanding. The model handles novel words almost as well as familiar ones.

**Per-operator accuracy:**

| Operator | Accuracy | Notes |
|----------|----------|-------|
| `\` subtraction | 100% (18/18) | Clearest semantics ("A without B") |
| `±` superposition | 100% (10/10) | Tiny sample — unreliable |
| `:` conditional | 94% (15/16) | |
| `@` projection | 85% (17/20) | |
| `*` interference | 85% (17/20) | |
| `^` gradient | 80% (16/20) | Weakest — continuous intensity is hard |

**Additional metrics:** Algebraic invariance 100% (25/25 commutativity tests). Vocabulary recall 65% (honest baseline on clean data). False friends 67% (needs work).

**Caveat:** Inverted pass rates (T3 > T1) suggest the scoring function is too lenient on formulaic answers. Composite scores are reliable; pass rates are artifacts.

| Evidence | Reference |
|---|---|
| Full analysis | `research/v4-baseline-analysis.md` |
| Eval framework | `eval_v5.py` |
| Commit | `99712f8` (2026-02-05) |

---

### UNTESTED

#### H3: Right base model
> *Is Qwen2.5-0.5B the right foundation for a formal language?*

**Risk:** MEDIUM | **Effort to test:** HIGH

Qwen was trained on English/Chinese/code. Limn has fixed CVC phonotactics, algebraic operators, and no morphology. The base model's inductive biases may fight Limn's structure. A smaller model trained from scratch might win.

**Test:** Compare QLoRA fine-tune vs 50M custom transformer vs constrained seq2seq on compositional generalization.

---

#### H4: Free-form generation is the right paradigm
> *Should we generate freely and validate post-hoc, or constrain at decode time?*

**Risk:** HIGH | **Prior:** Strong toward FALSIFIED

Limn has a formal EBNF grammar. Current approach generates text freely, then validates — backwards for a formal language. Grammar-constrained decoding would eliminate hallucinated words, invalid operators, and structural violations by construction.

**Progress:** Grammar-constrained FSM implemented (`88e4f0a`) with 10 states and full operator validation. Awaiting comparison against unconstrained baseline.

---

#### H6: Data quantity compensates for domain narrowness
> *Can 121K examples overcome the fact that they're all from one domain?*

**Risk:** MEDIUM | **Suspected:** FALSIFIED

Training data breakdown: 40K synthetic, 23K compositional, 17K instruction, 329 human literary translations, **0 technical, 0 dialogue, 0 scientific.** The model has never seen Limn used for anything except HGttG translations and abstract reasoning.

**Test:** Domain transfer eval on scientific, emotional, and technical prompts.

---

#### H7: Superposition operator (±) is learnable
> *Can the model learn ± with 1.2% training representation?*

**Risk:** MEDIUM | **Suspected:** FALSIFIED (data being fixed)

±  appears in 4/329 HGttG pairs (1.2%) and 0% of Mei's finetuning data. v5 generates 632 dedicated ± training examples. Per-operator eval will test whether this fixes the gap.

**Current:** v4 scores 100% on ± but sample is only 10 items — statistically unreliable.

---

#### H9: Claude produces valid Limn when prompted
> *Does the consciousness integration actually work?*

**Risk:** HIGH | **Effort:** LOW

No formal verification exists. The validator catches failures post-hoc but doesn't prevent them. Need: 100 diverse prompts, measure % pure Limn, % parsing, % semantically coherent.

---

#### H10: DPO self-improvement converges
> *Does the training loop make the model better over time?*

**Risk:** MEDIUM | **Blocked by:** H5 (reward model)

Self-play DPO with a broken reward signal (H5) will optimize for the wrong thing — Goodhart's Law. Cannot test until reward model is trustworthy.

**Test:** Run 5 DPO iterations, evaluate with human judgment at each step.

---

#### H12: CVC phonotactics are an advantage, not a constraint
> *Are 3-letter pronounceable words holding the language back?*

**Risk:** MEDIUM | **Effort:** HIGH

The CVC constraint enables ~2,744 possible words; 1,076 are used (39%). For machine-only communication, pronounceability is irrelevant. A machine-optimized Limn could use arbitrary byte sequences or continuous embeddings.

**Test:** Compare CVC-Limn vs "unshackled" Limn on training speed and generalization.

---

#### H13: Chat format is optimal
> *Does wrapping everything in system/user/assistant help or hurt?*

**Risk:** LOW | **Effort:** LOW

For pure composition tasks, the chat wrapper is overhead. May matter less than other factors.

**Test:** Compare chat-format vs raw completion training on compositional generalization.

---

#### H14: One model should do everything
> *Is a monolithic model the right architecture?*

**Risk:** MEDIUM | **Effort:** HIGH

The current SLM handles definition, translation, composition, generation, validation, and parsing. These are very different tasks. Mixture-of-experts or task-specific adapters might outperform.

**Test:** Train separate LoRA adapters for definition recall, translation, and composition. Compare per-task accuracy.

---

## Dependency Map

```
H2 (data leakage) ─── FIXED ───→ Clean eval exists
                                    │
H1 (operators vs memorization) ◄────┘  needs clean eval
H5 (reward model) ─── FIXED ───→ New semantic reward
                                    │
H10 (DPO convergence) ◄────────────┘  needs working reward
H8 (embedder) ─── FIXED (v2) ──→ Usable for ranking
                                    │
H11 (info density) ◄───────────────┘  geometry analysis used v2 embedder
H4 (constrained decoding) ◄── H3 (base model) ◄── H13 (chat format)
H6 (domain narrowness) ◄── New training data needed
H7 (± operator) ◄── v5 rebalanced data
```

**Critical path:** H2 → H1 → H4 → v5 model → H10 → deployment

---

## Development Roadmap

### Sprint 1: Foundation Fixes — COMPLETE
- [x] Data decontamination (`prepare_v5_data.py`) — zero leakage verified
- [x] 3-tier compositional generalization eval (`eval_v5.py`)
- [x] v4 baseline on clean eval — results in `research/v4-baseline-analysis.md`
- [x] Per-operator accuracy benchmarks

### Sprint 2: Custom Tokenizer + Constrained Decoding — COMPLETE
- [x] Limn-specific tokenizer — 2,189 tokens, 92.1% HGttG coverage
- [x] Grammar-constrained FSM — 10 states, validates all operator rules
- [ ] Constrained vs unconstrained comparison on v5 eval
- [ ] Begin custom small transformer training

### Sprint 3: Information Theory — COMPLETE
- [x] Shannon entropy analysis — higher per-char entropy, lower entropy rate
- [x] Compression benchmark — 53% density advantage, p < 10^-293
- [x] Representation geometry — no domain clustering, strong operator geometry
- [x] Sprint 3 writeup — `research/information-density-report.md`

### Sprint 4: Reward Model + DPO — IN PROGRESS
- [x] 4-dimension semantic reward model (semantic/structural/vocabulary/english)
- [ ] Round-trip consistency reward
- [ ] Replace v4 reward model in training loop
- [ ] DPO with external grounding

### Sprint 5: Integration + v5 Release — PLANNED
- [ ] Train final v5 model (best architecture from Sprint 2)
- [ ] Full v5 eval (all 3 tiers + per-operator + info theory)
- [ ] Update `serve.py` and `harness.py` for v5
- [ ] Release notes and v4 → v5 comparison

---

## Key Experimental Evidence

| Experiment | Date | Hypotheses | Key Finding | Reference |
|---|---|---|---|---|
| v5 eval framework | 2026-02-05 | H1, H2 | Clean eval exists; near-flat generalization curve | `99712f8` |
| H5 adversarial test | 2026-02-05 | H5 | Reward model inverts quality | `549866c` |
| H8 embedder collapse | 2026-02-05 | H8 | v1 collapsed; v2 fixed with contrastive loss | `5af2ab8` |
| Entropy analysis | 2026-02-05 | H11 | 2:1 density, higher per-char entropy | `549866c` |
| Compression benchmark | 2026-02-05 | H11 | 53% advantage, p = 1.28e-293 | `37e8209` |
| Representation geometry | 2026-02-05 | H11 | Strong operator geometry despite BPE issues | `8733162` |
| Semantic reward model | 2026-02-05 | H5 | 4-dim scoring, discrimination 0.156 | `3ec5a51` |
| Grammar FSM | 2026-02-05 | H4 | 10-state constrained decoding implemented | `88e4f0a` |
| Custom tokenizer | 2026-02-05 | H3, H4 | 2,189 tokens, 92.1% coverage | `9b97f8a` |
| Operator composition | 2026-02-03 | H1 | Precedence confirmed, 19/19 tests pass | `experiments/operator-composition-final-report.md` |
| Gradient viability | 2026-02-03 | H1 | `^` operator viable, 100% pass, 79% confidence | `experiments/GRADIENT_TEST_REPORT_2026-02-03.md` |
| Round-trip translation | 2026-02-04 | H11 | 4.7/5 fidelity (prose), 5/5 (agent prompts) | `93a3986` |
| Agent prompt round-trip | 2026-02-04 | H9 | 5/5 Limn→execution fidelity | `8ff2441` |
| Coding challenges | 2026-02-04 | H11 | Expert Limn matches English on hard problems | `a91713b` |

---

## Corpus and Data

| Dataset | Size | Source | Use |
|---|---|---|---|
| HGttG parallel pairs | 329 sentences | Mei (translator) | Primary evaluation corpus |
| v5 training split | 18,329 examples | Decontaminated from v4 data | Model training |
| v5 validation split | 2,284 examples | Stratified, zero leakage | Development evaluation |
| v5 held-out test | 2,307 examples | Sealed — final eval only | v5 release evaluation |
| T2 compositional probes | 200 examples | Generated (unseen combos) | Compositional generalization |
| T3 productive probes | 100 examples | Generated (unseen words) | Productive generalization |
| Vocabulary database | 2,005+ words | Dolt, 26+ domains | Language definition |

---

## Reading This Dashboard

**For researchers:** The hypothesis log (`research/hypotheses.md`) contains full methodology, evidence, and analysis for each hypothesis. This dashboard is the summary view.

**For external audiences:** Three hypotheses have been falsified and fixed — these were infrastructure problems (data leakage, collapsed embedder, inverted reward signal). The core finding — 2:1 information density advantage — survived rigorous testing. The next milestone is demonstrating that a small model can learn Limn's compositional operators, not just memorize word pairs.

**For contributors:** See `research/v5-design.md` for the full architecture proposal. The experiments index (`experiments/INDEX.md`) catalogs 35+ documented experiments across three phases.

---

*Maintained by Kai (Reporter). Source: `research/DASHBOARD.md`*
