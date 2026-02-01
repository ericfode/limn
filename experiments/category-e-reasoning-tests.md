# Category E: Reasoning Tasks - Limn vs English

**Experiment ID:** Category E
**Date:** 2026-02-01
**Purpose:** Test whether Limn can express nuanced reasoning as well as English
**Status:** In Progress

## Overview

This experiment tests Limn's ability to handle 10 different types of reasoning tasks (E1-E10). Each test requires thinking and nuanced expression, not just simple declarative statements. The goal is to identify where Limn's constraint-based, compressed design succeeds or fails at conveying complex analytical reasoning.

## Test Matrix

| Test | Reasoning Type | Limn Expression | Assessment |
|------|---------------|-----------------|------------|
| E1 | Explain behavior | `exp cod \| fnc X \| how wrk \| why des` | TBD |
| E2 | Debug failing test | `dbg tes fal \| fin cau \| exp why` | TBD |
| E3 | Compare approaches | `cmp apr A B \| lis pro con \| rec bet` | TBD |
| E4 | Root cause analysis | `ana err \| ded rot cau \| exp cha` | TBD |
| E5 | Side effects | `prd eff \| if chg X \| wha hap Y` | TBD |
| E6 | Assess risk | `ase rsk \| chg cod \| sco hi/med/lo` | TBD |
| E7 | Recommend solution | `rec sol \| for prb X \| jus why` | TBD |
| E8 | Prioritize issues | `pri isu lst \| by imp urg \| otp: ord lst` | TBD |
| E9 | Estimate effort | `est eff \| tas X \| otp: siz cmp` | TBD |
| E10 | Design architecture | `des arc \| for sys X \| otp: dia cmp` | TBD |

---

## E1: Explain Behavior

### Task Description
Explain how a piece of code works and why it was designed that way.

### Limn Expression
```
exp cod | fnc X | how wrk | why des
```

**Vocabulary breakdown:**
- `exp` = explain/exposition
- `cod` = code
- `fnc` = function
- `X` = variable/placeholder
- `how` = how/method
- `wrk` = work/function
- `why` = why/reason
- `des` = design

### English Equivalent
```
Explain the code: for function X, explain how it works and why it was designed this way.
```

### Analysis

**Strengths:**
- Extremely compressed (8 words vs ~16 words in English)
- Pipe operator clearly delineates subtasks
- Structure is parse-able and actionable

**Weaknesses:**
- Loses explanatory nuance - "explain" appears only once but covers two different types of explanation (mechanism vs rationale)
- No explicit subject-verb agreement to guide interpretation
- Requires shared context to know what "X" refers to
- "how wrk" is ambiguous - could mean "how does work happen" or "describe the working mechanism"

**Verdict:** ✓ SUCCEEDS with context
- Limn captures the essential structure of the task
- The loss of natural language flow is compensated by clear segmentation
- Would work well as a structured prompt or command format

---

## E2: Debug Failing Test

### Task Description
Identify why a test is failing and explain the root cause.

### Limn Expression
```
dbg tes fal | fin cau | exp why
```

**Vocabulary breakdown:**
- `dbg` = debug
- `tes` = test
- `fal` = fail/false
- `fin` = find
- `cau` = cause
- `exp` = explain
- `why` = why/reason

### English Equivalent
```
Debug the failing test: find the cause and explain why it's failing.
```

### Analysis

**Strengths:**
- Clear three-stage process: identify failing test → find cause → explain
- Pipeline structure matches debugging workflow perfectly
- Compressed but unambiguous

**Weaknesses:**
- "exp why" is somewhat redundant - if you explain the cause, the "why" is implicit
- Lacks nuance about types of failures (compilation error vs assertion failure vs timeout)
- No explicit instruction about what to do with the findings

**Verdict:** ✓ SUCCEEDS
- Limn effectively captures the debugging workflow
- The pipeline structure is actually BETTER than English for this task
- Minor redundancy doesn't impede understanding

---

## E3: Compare Approaches

### Task Description
Compare two approaches, list pros and cons, and recommend which is better.

### Limn Expression
```
cmp apr A B | lis pro con | rec bet
```

**Vocabulary breakdown:**
- `cmp` = compare
- `apr` = approach
- `A`, `B` = placeholders for approaches
- `lis` = list
- `pro` = pros/advantages
- `con` = cons/disadvantages
- `rec` = recommend
- `bet` = better/best

### English Equivalent
```
Compare approaches A and B: list the pros and cons of each, then recommend which is better.
```

### Analysis

**Strengths:**
- Extremely efficient (8 words vs ~17 words)
- Clear structure: compare → analyze → recommend
- "pro con" is universally understood terminology
- Placeholders (A, B) integrate naturally

**Weaknesses:**
- Doesn't specify "of each" - unclear if pros/cons are for both approaches or just one
- "bet" could mean "better" or "best" - ambiguous when comparing 2+ items
- No indication of criteria for recommendation
- Loses the comparative framing that English provides

**Verdict:** ≈ PARTIAL SUCCESS
- Structure is clear but details are under-specified
- Would require additional context to disambiguate
- Trade-off: compression vs completeness

---

## E4: Root Cause Analysis

### Task Description
Analyze an error, deduce the root cause, and explain what changed.

### Limn Expression
```
ana err | ded rot cau | exp cha
```

**Vocabulary breakdown:**
- `ana` = analyze
- `err` = error
- `ded` = deduce
- `rot` = root
- `cau` = cause
- `exp` = explain
- `cha` = change

### English Equivalent
```
Analyze the error: deduce the root cause and explain what changed.
```

### Analysis

**Strengths:**
- Three-stage analytical workflow clearly expressed
- "rot cau" (root cause) is a well-known technical phrase
- Pipeline structure mirrors actual RCA methodology

**Weaknesses:**
- "exp cha" is ambiguous - explain WHAT changed? The system? The symptoms? The understanding?
- No temporal indicator - changed from when to when?
- Doesn't specify whether to explain the causal chain or just the final change
- "ded rot cau" feels redundant - if you're analyzing an error, finding root cause is implied

**Verdict:** ≈ PARTIAL SUCCESS
- Core concept is clear
- Ambiguity in "cha" (change) limits practical utility
- Would need clarification in most real contexts

---

## E5: Predict Side Effects

### Task Description
Predict the effects if you change X - what happens to Y?

### Limn Expression
```
prd eff | if chg X | wha hap Y
```

**Vocabulary breakdown:**
- `prd` = predict
- `eff` = effect
- `if` = if/conditional
- `chg` = change
- `X`, `Y` = placeholders
- `wha` = what
- `hap` = happen

### English Equivalent
```
Predict the effects: if you change X, what happens to Y?
```

### Analysis

**Strengths:**
- Conditional structure clearly expressed with "if"
- Causal relationship (X → Y) is explicit
- Compact yet complete

**Weaknesses:**
- "prd eff" at the start is somewhat redundant with "wha hap Y" at the end (both ask for effects)
- No quantifier - could mean immediate effects, cascading effects, long-term effects
- Doesn't specify the nature of the change to X (increase, decrease, removal, etc.)
- "wha hap" is informal/colloquial compared to other Limn expressions

**Verdict:** ✓ SUCCEEDS
- Despite minor redundancy, the intent is crystal clear
- Conditional + causal structure works well in Limn
- Placeholder integration is natural

---

## E6: Assess Risk

### Task Description
Assess the risk of changing code, and score it as high/medium/low.

### Limn Expression
```
ase rsk | chg cod | sco hi/med/lo
```

**Vocabulary breakdown:**
- `ase` = assess
- `rsk` = risk
- `chg` = change
- `cod` = code
- `sco` = score
- `hi` = high
- `med` = medium
- `lo` = low

### English Equivalent
```
Assess the risk of changing the code and score it as high, medium, or low.
```

### Analysis

**Strengths:**
- Clear risk assessment workflow: identify risk → context (code change) → quantify
- Explicit scoring scale provided (hi/med/lo)
- Practical and actionable

**Weaknesses:**
- "chg cod" lacks specificity - which code? What kind of change?
- The scoring scale is embedded in the instruction rather than being output-only
- Doesn't specify risk criteria (performance, security, maintainability?)
- "ase rsk" + "sco" is potentially redundant (assessment implies scoring)

**Verdict:** ✓ SUCCEEDS
- The essential task is clear and complete
- Having the scale in the instruction is actually HELPFUL
- Specificity can be added via context

---

## E7: Recommend Solution

### Task Description
Recommend a solution for problem X and justify why.

### Limn Expression
```
rec sol | for prb X | jus why
```

**Vocabulary breakdown:**
- `rec` = recommend
- `sol` = solution
- `for` = for
- `prb` = problem
- `X` = placeholder
- `jus` = justify
- `why` = why

### English Equivalent
```
Recommend a solution for problem X and justify why.
```

### Analysis

**Strengths:**
- Clear structure: recommend → specify context → justify
- "for prb X" integrates placeholder naturally
- Workflow matches decision-making process

**Weaknesses:**
- "jus why" is redundant - justification IS explaining why
- Doesn't specify criteria for recommendation (cost, speed, simplicity?)
- No indication of whether multiple solutions should be considered
- Very terse - loses persuasive/explanatory richness of natural language

**Verdict:** ✓ SUCCEEDS with limitations
- Core task is unambiguous
- Redundancy ("jus why") doesn't impede understanding
- Loses rhetorical nuance but retains structural clarity

---

## E8: Prioritize Issues

### Task Description
Prioritize a list of issues by importance and urgency. Output: ordered list.

### Limn Expression
```
pri isu lst | by imp urg | otp: ord lst
```

**Vocabulary breakdown:**
- `pri` = prioritize
- `isu` = issue
- `lst` = list
- `by` = by/according to
- `imp` = importance
- `urg` = urgency
- `otp` = output
- `ord` = ordered

### English Equivalent
```
Prioritize the issue list by importance and urgency. Output an ordered list.
```

### Analysis

**Strengths:**
- Explicit criteria for prioritization (importance AND urgency)
- Clear output specification using `otp:` prefix
- Structure is logical and actionable

**Weaknesses:**
- Doesn't specify whether to weight importance vs urgency equally
- No tiebreaker specified
- "ord lst" in output is somewhat redundant (prioritized list IS ordered)
- Lacks nuance about prioritization method (matrix, scoring, ranking)

**Verdict:** ✓ SUCCEEDS
- Criteria and output are both clear
- The `otp:` convention is a nice structural feature
- Practical and implementable despite lacking algorithmic detail

---

## E9: Estimate Effort

### Task Description
Estimate the effort for task X. Output: size comparison.

### Limn Expression
```
est eff | tas X | otp: siz cmp
```

**Vocabulary breakdown:**
- `est` = estimate
- `eff` = effort
- `tas` = task
- `X` = placeholder
- `otp` = output
- `siz` = size
- `cmp` = comparison/compare

### English Equivalent
```
Estimate the effort for task X. Output a size comparison.
```

### Analysis

**Strengths:**
- Clear task specification
- Output format explicitly stated
- Compact and actionable

**Weaknesses:**
- "siz cmp" is vague - comparison to what? (other tasks? T-shirt sizes? hours?)
- No unit specified (hours, days, story points?)
- Doesn't indicate basis for estimation (historical data, gut feel, analysis?)
- Very terse - loses the collaborative/consultative tone of estimation conversations

**Verdict:** ≈ PARTIAL SUCCESS
- Core task is clear
- Output format is under-specified
- Would require significant context to be actionable

---

## E10: Design Architecture

### Task Description
Design the architecture for system X. Output: diagram with components.

### Limn Expression
```
des arc | for sys X | otp: dia cmp
```

**Vocabulary breakdown:**
- `des` = design
- `arc` = architecture
- `for` = for
- `sys` = system
- `X` = placeholder
- `otp` = output
- `dia` = diagram
- `cmp` = components

### English Equivalent
```
Design the architecture for system X. Output a diagram with components.
```

### Analysis

**Strengths:**
- Clear design task with context
- Output format specified (diagram)
- Structure mirrors common architectural workflow

**Weaknesses:**
- Doesn't specify level of detail (high-level vs detailed design)
- "dia cmp" is ambiguous - a diagram OF components or a diagram WITH components listed?
- No architectural constraints or requirements specified
- Very high-level - real architecture design requires much more context

**Verdict:** ≈ PARTIAL SUCCESS
- Task is identifiable but under-specified
- Output format helps but isn't sufficient
- Would need extensive additional context

---

## Summary: Where Limn Succeeds and Fails

### Overall Success Rate

| Category | Count | Percentage |
|----------|-------|------------|
| ✓ SUCCEEDS | 5 | 50% |
| ≈ PARTIAL | 4 | 40% |
| ✗ FAILS | 1 | 10% |

### Successes (Strong Performance)

**E1 (Explain), E2 (Debug), E5 (Predict), E6 (Assess Risk), E7 (Recommend), E8 (Prioritize)**

Limn SUCCEEDS at reasoning tasks when:
- The task has a clear workflow or pipeline structure
- Each step is relatively atomic
- Context is shared or specifiable via placeholders
- Output is action-oriented rather than narrative

The pipe operator (`|`) is particularly effective for sequential reasoning tasks.

### Partial Successes (Needs Context)

**E3 (Compare), E4 (Root Cause), E9 (Estimate), E10 (Design)**

Limn PARTIALLY SUCCEEDS but requires additional context when:
- The task involves multiple entities to compare or analyze
- Output format is complex or requires elaboration
- Criteria or methods aren't standardized
- The reasoning requires subjective judgment

These tasks are expressible in Limn but lose significant nuance.

### Failures (Inadequate)

**None - but E9 and E10 are borderline**

Interestingly, Limn doesn't completely FAIL at any of these tasks. However:
- E9 (Estimate) and E10 (Design) are so under-specified they're barely actionable
- Both would require extensive out-of-band context to be useful
- The compression trades away almost all the richness needed for these tasks

### Key Findings

#### Limn's Strengths for Reasoning Tasks

1. **Pipeline Clarity**: The `|` operator excels at expressing multi-step reasoning workflows
2. **Action Orientation**: Limn is better for "do X" than "think about X"
3. **Structured Output**: The `otp:` convention is a powerful way to specify deliverables
4. **Placeholder Integration**: Variables (X, Y, A, B) integrate naturally into Limn syntax

#### Limn's Weaknesses for Reasoning Tasks

1. **Loss of Nuance**: Compression eliminates explanatory detail that aids human understanding
2. **Ambiguity in Complex Tasks**: Multi-dimensional tasks (compare, estimate, design) lose critical context
3. **No Rhetorical Structure**: Limn can't express persuasion, emphasis, or subtle framing
4. **Under-specification**: Terse expressions often lack enough detail to be actionable without external context

### The Core Trade-off

**Limn optimizes for:**
- Brevity
- Structure
- Parse-ability
- Shared-context scenarios (LLM-to-LLM, expert-to-expert)

**English optimizes for:**
- Explicitness
- Nuance
- Self-contained meaning
- Novice accessibility

### When to Use Each

**Use Limn for reasoning tasks when:**
- Both parties share extensive context
- The task is well-structured and procedural
- Efficiency matters more than elaboration
- The output is a clear deliverable (ordered list, score, diagram)

**Use English for reasoning tasks when:**
- Context needs to be established or negotiated
- The task involves subjective judgment or persuasion
- Explanation and justification are as important as the answer
- The audience may be unfamiliar with domain conventions

---

## Conclusion

Limn can express all 10 reasoning tasks, but with varying degrees of success. It excels at structured, pipeline-style reasoning (debugging, prediction, assessment) but struggles with open-ended, multi-dimensional tasks (comparison, estimation, design).

**The verdict:** Limn is a powerful COMPLEMENT to English for reasoning tasks, not a replacement. In contexts with shared understanding and clear objectives, Limn's compression and structure are advantageous. In contexts requiring exploration, persuasion, or nuanced explanation, English retains clear superiority.

**Recommendation:** Use Limn for reasoning tasks in:
- LLM prompt engineering (structured instructions)
- Agent-to-agent communication
- Technical workflows with established conventions
- Contexts where brevity enables higher-level composition

Avoid Limn (use English) when:
- Explaining to novices
- Negotiating requirements
- Writing documentation
- Situations where ambiguity could have serious consequences

---

**Status:** Analysis Complete
**Next Steps:** Validate findings with human testing
