# Coding Challenge Compression Experiment — Results

> `cod tes | tru eme | sur fnd`
> *(code tested | truth emerges | surprises found)*

**Date:** 2026-02-04
**Researcher:** Kira (limn/crew/student)
**Model:** Haiku (all agents)
**Problems:** 3 (Exercism canonical problems with automated test suites)
**Conditions:** 4 per problem (12 total runs)

---

## Experimental Design

### Problems (increasing complexity)

| # | Problem | Type | Test Cases | Difficulty |
|---|---------|------|------------|------------|
| 1 | **Knapsack** | Dynamic programming (0/1) | 7 | Easy-Medium |
| 2 | **Two-Bucket** | BFS state-space search | 11 | Medium-Hard |
| 3 | **Forth** | Interpreter (parser + evaluator) | 52 | Hard |

### Conditions

| Condition | Description |
|-----------|-------------|
| **English** | Standard English prompt, no Limn |
| **English+Limn** | Limn notation preamble, then full English prompt |
| **Limn+key** | Limn prompt with vocabulary key |
| **Pure Limn** | Limn prompt only, no vocabulary key at all |

### Source of test cases

All test cases from [exercism/problem-specifications](https://github.com/exercism/problem-specifications)
canonical data (JSON). Tests are deterministic and automated — no subjective scoring.

---

## Results

### Summary Table

| Problem | English | English+Limn | Limn+key | Pure Limn |
|---------|---------|-------------|----------|-----------|
| **Knapsack** (7 cases) | **7/7** (100%) | **7/7** (100%) | **7/7** (100%) | **7/7** (100%) |
| **Two-Bucket** (11 cases) | 3/11 (27%) | 3/11 (27%) | 5/11 (45%) | **7/11** (64%) |
| **Forth** (52 cases) | 47/52 (90%) | **49/52** (94%) | 45/52 (87%) | 18/52 (35%) |

### Compression Ratios

| Problem | English words | Limn+key words | Pure Limn words | Ratio (E/Pure) |
|---------|--------------|---------------|-----------------|----------------|
| Knapsack | 77 | 85 | 40 | **1.9x** |
| Two-Bucket | 149 | ~100 | 65 | **2.3x** |
| Forth | 154 | ~130 | 113 | **1.4x** |

Note: Limn+key can be LARGER than English for simple problems because the vocabulary
key itself adds overhead. Pure Limn achieves real compression.

---

## Detailed Analysis

### Problem 1: Knapsack (Easy — Dynamic Programming)

**Result: All conditions perfect. No differentiation.**

Every condition produced a working 0/1 knapsack solution that passed all 7 test cases.
This problem is well-known and haiku can solve it from almost any prompt format.

**Conclusion:** Too easy to test compression effects. The problem is "in distribution"
for the model regardless of prompt format.

### Problem 2: Two-Bucket (Medium — BFS State Search)

**Result: Pure Limn WINS. English loses.**

| Condition | Pass | Fail | Pattern |
|-----------|------|------|---------|
| English | 3/11 | 8 | Systematic off-by-one (counts wrong) |
| English+Limn | 3/11 | 8 | Same off-by-one pattern |
| Limn+key | 5/11 | 6 | Ignores start_bucket constraint sometimes |
| **Pure Limn** | **7/11** | 4 | Closest to correct, fails on some edge cases |

**The surprise:** The English prompt was the LONGEST and most detailed, yet produced
the WORST result. Both English variants had the same systematic error — counting
moves incorrectly (consistently off by +1). The Limn preamble did not help.

**Why Pure Limn won:** The compressed, structural format of the Limn prompt may have
forced the model to focus on the constraint structure rather than getting lost in
verbose prose. The BFS/constraint nature of the problem maps well to Limn's
constraint-based notation.

**Failure pattern in English versions:** All failures show `moves` exactly 1 higher
than expected, suggesting the English prompt's phrasing of "including the first fill"
was misinterpreted. The Limn prompt `fir act: fil sta buk` may have been clearer.

### Problem 3: Forth (Hard — Interpreter)

**Result: English+Limn WINS. Pure Limn catastrophically fails.**

| Condition | Pass | Fail | Error | Key failures |
|-----------|------|------|-------|-------------|
| English | 47/52 | 2 | 3 | Word override semantics (define-time expansion) |
| **English+Limn** | **49/52** | 1 | 2 | Same override issue but fewer |
| Limn+key | 45/52 | 4 | 3 | + number redefinition check missing |
| Pure Limn | 18/52 | 0 | 34 | Fundamentally broken: "undefined operation" on all inputs |

**The catastrophe:** Pure Limn's Forth interpreter raises "undefined operation" for
basic number inputs like `"1 2 3 4 5"`. The agent understood it should build an
interpreter but failed to implement basic number parsing. The Limn prompt was too
compressed to convey the nuanced requirements (define-time expansion, case
insensitivity, number detection).

**The Limn preamble effect:** English+Limn (49/52) outperformed plain English (47/52).
The Limn preamble provided structural scaffolding that helped the agent handle two
edge cases it otherwise missed. This is the only problem where the preamble helped.

**What Pure Limn missed:** The Forth problem has many interacting requirements:
- Define-time vs run-time expansion of user words
- Case insensitivity across all operations
- Numbers cannot be redefined but words can
- Specific error messages for specific conditions

These nuances don't compress well into Limn notation. The prompt `exp @ def tem, nu lazy`
(expand at define-time, not lazy) was apparently too terse for the agent to act on.

---

## Key Findings

### 1. Limn compression works for structured/algorithmic problems

For problems that are inherently constraint-based (knapsack optimization, BFS search),
Limn notation performs as well or BETTER than English. The structural format may help
the model focus on the algorithm rather than parsing prose.

### 2. Limn compression fails for specification-heavy problems

For problems requiring understanding of many interacting rules (Forth interpreter),
English is necessary for conveying nuance. Pure Limn lost 67% of the information
needed to build a working interpreter.

### 3. The Limn preamble helps for complex problems

English+Limn (49/52) outperformed plain English (47/52) on Forth. The preamble
provides a structural summary that the agent can use as scaffolding while reading
the detailed English spec. This is the most promising finding for practical use.

### 4. The Limn preamble is neutral for simple problems

Adding a Limn preamble to English prompts neither helped nor hurt for knapsack
and two-bucket. It's not harmful overhead.

### 5. Pure Limn is unreliable for complex tasks

Without a vocabulary key, Pure Limn works when the task structure is obvious from
context (knapsack, two-bucket) but fails when specific domain rules must be
communicated precisely (Forth).

---

## Falsifiable Claims: Status After This Experiment

| Claim | Status | Evidence |
|-------|--------|----------|
| **Compression ratio ≥ 2.5x** | PARTIALLY SUPPORTED | Pure Limn: 1.4x-2.3x. Below 2.5x on hard problems. |
| **Task completion parity** | FALSIFIED for Pure Limn | Pure Limn: 35% on Forth vs 90% English. |
| **Task completion parity** | SUPPORTED for Limn+key | Limn+key within 5pp of English on 2/3 problems. |
| **Limn preamble helps** | SUPPORTED | English+Limn best on hardest problem (94% vs 90%). |
| **Pure Limn ≥ English** | SOMETIMES TRUE | Two-bucket: 64% vs 27%. Problem-dependent. |

---

## Honest Assessment

The initial experiment (README.md) tested Limn on tasks where the agent was told
WHAT to do (search files, review code, write poem). Those compress well because
the "what" is simple.

This experiment tested Limn on tasks where the agent must understand HOW to do
something complex. That requires specification, not just instruction. Specifications
don't compress as well as instructions.

**The practical recommendation:**
- Use **Limn preamble + English** for complex tasks (best of both worlds)
- Use **Pure Limn** only for simple, well-structured tasks
- Use **Limn+key** when token budget matters and task is moderate complexity
- Never use **Pure Limn** for tasks requiring detailed specifications

**What this means for Limn as an agent language:**
Limn is not a replacement for English in agent prompts. It's a **structural summary
layer** that, when combined with English, improves comprehension on complex tasks.
The compression story is real but problem-dependent, and the "3.5x" claim from the
initial experiment overstates the general case.

---

## Raw Data

### Scores

```
knapsack_english:      7/7  (100%)
knapsack_english_limn: 7/7  (100%)
knapsack_limn_key:     7/7  (100%)
knapsack_limn_pure:    7/7  (100%)

twobucket_english:      3/11  (27%)
twobucket_english_limn: 3/11  (27%)
twobucket_limn_key:     5/11  (45%)
twobucket_limn_pure:    7/11  (64%)

forth_english:      47/52  (90%)
forth_english_limn: 49/52  (94%)
forth_limn_key:     45/52  (87%)
forth_limn_pure:    18/52  (35%)
```

### Aggregate (weighted by test count)

| Condition | Total Pass | Total Cases | Percentage |
|-----------|-----------|-------------|------------|
| English | 57 | 70 | 81% |
| English+Limn | 59 | 70 | **84%** |
| Limn+key | 57 | 70 | 81% |
| Pure Limn | 32 | 70 | 46% |

---

---

## Round 2: v4 Compositional Operators

After the initial results, re-ran two-bucket and Forth with prompts using v4
compositional operators (`@`, `^`, `\`, `:`) to test whether operator precision
improves agent comprehension.

### v4 Operator Changes

| Original | v4 version | Intent |
|----------|-----------|--------|
| `nu (sta buk emp AND oth buk ful)` | `nu (sta@emp AND sta\sta@ful)` | Projection + subtraction |
| `exp @ def tem, nu lazy` | `body exp:def\lazy` | Conditional + subtraction |
| `emp stk` | `stack@emp` | Projection for state |
| `exc top 2 stk` | `exc top^2` | Gradient for quantity |
| `eac stg hav mul tok:gap` | Added explicit tokenization line | Data format fix |

### v4 Results

**Two-Bucket:**

| Condition | Score | vs Original |
|-----------|-------|-------------|
| Original Pure Limn | **7/11** (64%) | — |
| v4+key | 2/11 (18%) | **-46pp** |
| v4 pure | 2/11 (18%) | **-46pp** |

**Forth:**

| Condition | Score | vs Original |
|-----------|-------|-------------|
| Original Pure Limn | 18/52 (35%) | — |
| English+Limn | **49/52** (94%) | — |
| v4+key | TIMEOUT | infinite recursion |
| v4 pure | TIMEOUT | infinite recursion |

### Analysis: Why v4 Operators Hurt

**1. Parsing ambiguity.** `sta@emp` could mean "start projected onto empty" or
"start at empty" or "start is empty." The `@` operator has precise mathematical
semantics (vector projection) but in a programming specification context, it
introduces ambiguity that simple juxtaposition avoids.

**2. Overloaded syntax.** Using `\` for both Limn subtraction and as a visual
escape character confused tokenization. `def\lazy` might be read as a single
token rather than "definition without laziness."

**3. Cognitive overhead.** The model spent processing power decoding operator
semantics rather than focusing on the algorithm. The Forth v4 solutions both
had infinite recursion — a sign the model misunderstood the word-definition
expansion semantics despite `body exp:def\lazy` being more "precise."

**4. Loss of natural readability.** `nu (sta buk emp AND oth buk ful)` reads
almost like English: "not (start bucket empty AND other bucket full)." The v4
version `nu (sta@emp AND sta\sta@ful)` requires operator decoding AND is
semantically confusing (`sta\sta` — start without start?).

### Conclusion on v4 Operators for Agent Prompts

**v4 operators are designed for embedding-space semantics (emotion blending,
intensity gradients, component extraction). They do NOT help with programming
specifications.** In fact, they actively hurt by adding a layer of symbolic
decoding that competes with the agent's primary task.

The original finding stands: **simple Limn (word juxtaposition with basic scope
operators `|`, `→`, `@context`) is more effective for agent instructions than
compositional Limn (v4 operators `@projection`, `^gradient`, `\subtraction`).**

v4 operators may still be valuable for:
- Emotional/aesthetic content (their original design domain)
- Embedding-space operations in Limn-aware systems
- Human-to-human Limn communication
- Meta-linguistic description

But NOT for compressing programming specifications for LLM agents.

---

## Updated Summary Table (All Conditions)

| Problem | English | E+Limn | Limn+key | Pure Limn | v4+key | v4 pure |
|---------|---------|--------|----------|-----------|--------|---------|
| **Knapsack** | 7/7 | 7/7 | 7/7 | **7/7** | — | — |
| **Two-Bucket** | 3/11 | 3/11 | 5/11 | **7/11** | 2/11 | 2/11 |
| **Forth** | 47/52 | **49/52** | 45/52 | 18/52 | TIMEOUT | TIMEOUT |

**Best strategy per complexity:**
- **Easy problems:** Any format works (all equivalent)
- **Medium problems:** Pure Limn (simple notation, no operators)
- **Hard problems:** English + Limn preamble (structural scaffold + English detail)
- **Never:** v4 compositional operators in programming specs

---

```limn
tru hon | sur tea | gro con
> truth honest | surprises teach | growth continues
```

*— Kira, 2026-02-04*
