# Falsifiable Claims: Limn Prompt Compression

> `cla tes | fal can | tru eme`
> *(claims tested | falsification possible | truth emerges)*

**Date:** 2026-02-04
**Researcher:** Kira (limn/crew/student)
**Status:** EXPERIMENT DESIGN (not yet executed)
**Predecessor:** `README.md` (3 anecdotal tests, N=3)

---

## Why This Document Exists

The initial experiment (README.md) showed promising results: 3.5-4.2x compression,
100% task completion, Limn agents matching or exceeding English agents. But N=3 is
anecdotal. "It worked three times" is not evidence — it's a story.

This document defines **what we actually claim**, **how to test each claim**, and
**what results would kill each claim**. If we can't specify failure criteria, we
don't have a hypothesis — we have wishful thinking.

---

## The Claims

### Claim 1: Compression Ratio

**Statement:** Limn v4 prompts achieve ≥2.5x word-count compression relative to
functionally equivalent English prompts, on average across diverse task categories.

**Why 2.5x (not 3.5x):** The initial experiments showed 3.2-4.2x, but that was
cherry-picked by a practitioner (me) who chose prompts amenable to compression.
The conservative bound accounts for prompts that compress poorly (e.g., those
heavy on proper nouns, specific numbers, or domain jargon Limn lacks).

**Measurement:**
- Count: words in English prompt vs. words in Limn prompt (excluding vocab key)
- Tool: `wc -w` on the prompt text, stripping markdown formatting
- Per-prompt ratio: English words ÷ Limn words

**Failure criterion:** Mean compression ratio < 2.5x across ≥20 prompt pairs.

**What would also weaken the claim:**
- Standard deviation > 1.5x (too inconsistent to be useful)
- Any task category averaging < 1.5x (category-specific failure)

---

### Claim 2: Net Token Savings (Including Overhead)

**Statement:** Limn prompts plus a task-specific vocabulary key consume fewer
tokens than the equivalent English prompt, with net savings ≥15% on single use
and ≥50% on repeated use (5+ prompts sharing a key).

**Why this matters:** Compression ratio alone is misleading if the vocabulary key
overhead erases the savings. This claim accounts for the full cost.

**Measurement:**
- Token count: Use `tiktoken` (cl100k_base) or Anthropic's tokenizer
- Single-use cost: tokens(Limn prompt) + tokens(vocab key)
- Repeated-use cost: tokens(Limn prompt) + tokens(vocab key) ÷ N prompts
- Baseline: tokens(English prompt)

**Failure criterion:**
- Single-use: Mean net savings < 15% across ≥20 prompt pairs
- Repeated-use (N=5): Mean net savings < 50%

**What would also weaken the claim:**
- Any single-use prompt where Limn + key uses MORE tokens than English
  (net negative) in >25% of cases

---

### Claim 3: Task Completion Parity

**Statement:** Agents given Limn-compressed prompts complete the intended task
at a rate statistically indistinguishable from agents given English prompts.

**Null hypothesis (H₀):** Limn-prompted agents have a lower task completion rate
than English-prompted agents.

**Alternative hypothesis (H₁):** Limn-prompted agents complete tasks at an equal
or higher rate.

**Measurement:**
- Binary scoring per prompt: 1 = task completed, 0 = task not completed
- "Completed" = output satisfies all explicitly stated requirements
- Scored by: automated rubric where possible, blind human evaluation otherwise
- Each prompt tested on both English and Limn versions (paired design)

**Scoring rubric (per prompt):**
1. **Structural compliance** — Does the output match the requested format? (0/1)
2. **Content coverage** — Does it address all stated requirements? (0/1)
3. **Correctness** — Are factual/logical claims accurate? (0/1)
4. Task completion = all three criteria met

**Statistical test:** McNemar's test for paired binary outcomes, α = 0.05.

**Failure criterion:** Statistically significant difference (p < 0.05) showing
Limn completion rate < English completion rate, OR Limn completion drops below
80% absolute (regardless of statistical significance).

---

### Claim 4: Output Quality Equivalence

**Statement:** When both agents complete the task, Limn-prompted output quality
is not meaningfully worse than English-prompted output quality.

**Measurement:** Blind pairwise comparison.
- Evaluator sees Output A and Output B (randomized order, no label)
- Evaluator rates: A is better / B is better / equivalent
- Minimum 2 evaluators per prompt pair (human or strong LLM-as-judge)

**Automated proxy metrics (supplementary, not primary):**
- Structural similarity: Do both outputs have similar structure?
- Length ratio: Limn output length ÷ English output length (expect 0.7-1.3)
- For code tasks: Does the code compile/run? Same test pass rate?
- For search tasks: Same files/items found?

**Failure criterion:** Evaluators prefer English output over Limn output in >60%
of completed pairs (i.e., Limn is rated worse most of the time).

**What would also weaken the claim:**
- Limn outputs are systematically shorter (indicating lost information)
- Limn agents produce more hallucinated content

---

### Claim 5: Operator Decoding Accuracy

**Statement:** LLM agents correctly decode v4 compositional operators (`@`, `*`,
`^`, `:`, `\`, `±`) ≥85% of the time when provided with a brief operator key
(one line per operator).

**Measurement:** Operator-specific test battery.
- For each operator, create 5 expressions using that operator
- Agent receives expression + operator key + "explain what this means"
- Score: Does the explanation match the intended semantics? (0/1 per expression)
- 6 operators × 5 expressions = 30 test items

**Scoring criteria per operator:**
| Operator | Correct if agent describes it as... |
|----------|--------------------------------------|
| `@` | extracting/projecting B-component of A |
| `*` | blending/combining/interfering A and B |
| `^` | intensity/degree/gradient of the concept |
| `\` | A without/minus B component |
| `±` | both A and B simultaneously/unresolved |
| `:` | A in context of / given / conditioned on B |

**Failure criterion:** Overall accuracy < 85% (fewer than 26/30 correct), OR
any single operator < 60% (fewer than 3/5 correct).

---

### Claim 6: Vocabulary Key Sufficiency

**Statement:** A task-specific vocabulary key (≤30 Limn-English word pairs +
operator definitions) is sufficient for an agent to execute Limn instructions,
without needing the full bootstrap document (~770 lines).

**Measurement:**
- Same 20 Limn prompts tested with two conditions:
  - **Key-only:** Limn prompt + vocabulary key (≤30 pairs + operators)
  - **Full-bootstrap:** Limn prompt + complete bootstrap-v4-compositional.md
- Compare task completion rates between conditions

**Failure criterion:** Key-only completion rate is >15 percentage points lower
than full-bootstrap completion rate (e.g., full-bootstrap gets 90%, key-only
gets <75%).

**What this tells us:**
- If key-only ≈ full-bootstrap: The key IS the compression; bootstrap is
  unnecessary overhead for task execution
- If key-only << full-bootstrap: Agents need deeper Limn understanding;
  compression savings are overstated because they depend on full context

---

## Experimental Design

### Prompt Corpus

**20 prompt pairs** across 5 task categories, 4 prompts each:

| Category | Description | Example tasks |
|----------|-------------|---------------|
| **Retrieval** | Find, list, catalog information | Search files, list functions, find patterns |
| **Code** | Review, debug, generate, refactor | Code review, write function, fix bug, refactor |
| **Analysis** | Compare, evaluate, reason about | Compare approaches, evaluate tradeoffs, critique |
| **Generation** | Create text, docs, creative content | Write docs, poem, summary, explanation |
| **Reasoning** | Multi-step logic, planning | Design plan, solve constraint, sequence steps |

**Prompt construction rules:**
1. English prompt written first (natural, not artificially verbose)
2. Limn prompt written independently (not word-for-word translation)
3. Both validated by a second reviewer for functional equivalence
4. Limn vocabulary validated via `vocab.sh check` for every word
5. Prompts span a range of complexity (simple → multi-step)

### Controls

**Held constant across all tests:**
- Model: Same model for both English and Limn agents (haiku for cost, sonnet for validation)
- Temperature: 0 (deterministic)
- System prompt: None (only the task prompt differs)
- Max tokens: Same limit for both
- Tools available: Same tool set for both

**Varied systematically:**
- Task category (5 categories)
- Task complexity (simple, moderate, complex)
- Limn operator usage (none, single, multi-operator)

### Execution Protocol

For each of the 20 prompt pairs:

```
1. Generate English prompt
2. Generate Limn prompt + vocabulary key
3. Validate Limn vocabulary (vocab.sh check for each word)
4. Run Agent-English (record: output, token counts, completion time)
5. Run Agent-Limn with key-only (record: same metrics)
6. Run Agent-Limn with full-bootstrap (record: same metrics)
7. Score task completion (rubric)
8. Score output quality (blind comparison)
9. Count tokens (actual, not estimated)
10. Record operator usage and decoding accuracy
```

Steps 4-6 run as independent sub-agents via Task tool (no shared context).

### Sample Size Justification

**N=20 prompt pairs** is the minimum for:
- McNemar's test to detect a 25% difference in completion rates (power ≈ 0.80)
- Paired t-test on compression ratios with expected effect size d ≈ 1.5
- Per-category analysis with 4 prompts each (exploratory, not confirmatory)

**Limitations of N=20:**
- Cannot detect small differences (<15%) in completion rates
- Per-category conclusions are exploratory only
- No correction for multiple comparisons across claims

**Ideal future study:** N=50+ with 10 per category, enabling confirmatory
per-category analysis and Bonferroni correction.

---

## What Would Kill Each Claim (Summary)

| Claim | Dead if... |
|-------|------------|
| 1. Compression ratio | Mean < 2.5x across 20 prompts |
| 2. Net token savings | Single-use savings < 15% mean |
| 3. Task completion | p < 0.05 showing Limn worse, OR Limn < 80% absolute |
| 4. Output quality | English preferred in >60% of blind comparisons |
| 5. Operator decoding | Overall < 85% accuracy, OR any operator < 60% |
| 6. Key sufficiency | Key-only completion >15pp below full-bootstrap |

---

## What Would Strengthen Each Claim

| Claim | Strong if... |
|-------|--------------|
| 1. Compression ratio | Mean ≥ 3x with SD < 1.0 |
| 2. Net token savings | Single-use savings ≥ 25%, repeated ≥ 60% |
| 3. Task completion | Limn ≥ 95% with no significant difference |
| 4. Output quality | Limn preferred or tied in ≥ 80% of comparisons |
| 5. Operator decoding | ≥ 93% accuracy (28/30), all operators ≥ 80% |
| 6. Key sufficiency | Key-only within 5pp of full-bootstrap |

---

## Confounds and Threats to Validity

### Known confounds

1. **Prompt author bias:** I write both English and Limn prompts. I might
   unconsciously make the English verbose or the Limn artificially clean.
   *Mitigation:* Second reviewer checks functional equivalence.

2. **Task selection bias:** I might choose tasks where Limn excels.
   *Mitigation:* Pre-register the 20 prompts before running any tests.
   Include tasks where Limn is expected to struggle (proper nouns, numbers,
   domain-specific jargon).

3. **Model familiarity:** The model may have seen Limn-like constructs in
   training data, inflating apparent comprehension.
   *Mitigation:* Test on multiple model families if possible. Note that this
   is actually the *mechanism* — Limn is designed to leverage LLM priors.

4. **Vocabulary key leakage:** The vocab key gives the agent hints about the
   task structure even before reading the Limn.
   *Mitigation:* Measure whether key-only (no Limn prompt, just the key)
   enables task completion. If so, the key is doing the work, not Limn.

5. **Evaluator bias in quality scoring:** Human evaluators might favor more
   verbose (English-like) outputs.
   *Mitigation:* Blind evaluation. Randomize which output is A vs B.

### What this experiment CANNOT prove

- That Limn is better than English for all tasks
- That Limn compression generalizes to all models
- That token savings translate to cost savings (depends on pricing)
- That Limn is learnable by humans (this tests LLM comprehension only)
- That v4 operators add value beyond simple word compression

### What this experiment CAN establish

- Whether Limn compression is *real* (not just cherry-picked)
- Whether the overhead (vocab key) erases the compression gains
- Whether task quality is preserved under compression
- Whether v4 operators are reliably decoded
- Whether the full bootstrap is necessary or a small key suffices

---

## Execution Checklist

```
Phase 1: Prompt Construction
[ ] Write 20 English prompts (4 per category)
[ ] Write 20 Limn prompts with vocabulary keys
[ ] Validate all Limn vocabulary (vocab.sh check)
[ ] Review functional equivalence (second reviewer)
[ ] Pre-register prompt corpus (commit before running tests)

Phase 2: Execution
[ ] Run 20 Agent-English tests
[ ] Run 20 Agent-Limn (key-only) tests
[ ] Run 20 Agent-Limn (full-bootstrap) tests
[ ] Run 30 operator decoding tests (Claim 5)
[ ] Record all outputs, token counts, and metadata

Phase 3: Scoring
[ ] Score task completion (rubric, 3 criteria each)
[ ] Conduct blind quality comparisons (2 evaluators)
[ ] Calculate compression ratios
[ ] Calculate token savings (actual token counts)
[ ] Score operator decoding accuracy

Phase 4: Analysis
[ ] Compute descriptive statistics per claim
[ ] Run McNemar's test (Claim 3)
[ ] Run paired t-test (Claims 1, 2)
[ ] Compile blind comparison results (Claim 4)
[ ] Tabulate operator accuracy (Claim 5)
[ ] Compare key-only vs full-bootstrap (Claim 6)
[ ] Write results report with pass/fail per claim

Phase 5: Reporting
[ ] Document results in RESULTS.md
[ ] Flag any claims that failed
[ ] Identify unexpected findings
[ ] Propose follow-up experiments
```

---

## Relationship to Prior Work

| Aspect | README.md (prior) | This design |
|--------|-------------------|-------------|
| Sample size | N=3 | N=20 (minimum) |
| Categories | 3 (search, code, creative) | 5 (+ analysis, reasoning) |
| Scoring | Subjective comparison | Rubric + blind evaluation |
| Token counting | Estimated (~1.3 tok/word) | Actual (tokenizer) |
| Statistics | None | McNemar's, paired t-test |
| Failure criteria | None defined | Explicit per claim |
| Operator testing | Incidental (3 operators in 1 test) | Systematic (all 6, 30 items) |
| Key vs bootstrap | Not tested | Explicit comparison |
| Pre-registration | No | Yes (commit before execution) |

---

## Notes on Scope

This experiment tests **Limn as an LLM instruction language** — whether compressed
prompts work for directing agent behavior. It does NOT test:

- Limn as a communication language between agents
- Limn as a human-readable language
- Limn's theoretical expressiveness claims (30,000+ expressions)
- Whether Limn is "better" than English in any absolute sense

The narrower the claim, the more falsifiable it is. We're testing one thing:
**can you compress English agent prompts into Limn without losing task performance?**

---

```limn
cla spe | tes rig | fal hon
> claims specific | testing rigorous | falsification honest
```

*— Kira, 2026-02-04*
