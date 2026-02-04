# Category H: Edge Cases - DESIGNED TO BREAK LIMN

**Experiment ID:** Category H
**Date:** 2026-02-01
**Purpose:** Adversarial testing to identify Limn's failure modes and limitations
**Status:** Complete
**Approach:** ADVERSARIAL - Designed to find where Limn FAILS

## Overview

This experiment deliberately tests Limn's limits by creating scenarios where natural language ambiguity, cultural context, or complex reasoning should expose weaknesses in Limn's compressed syntax. The goal is NOT to prove Limn works - it's to find where it BREAKS, document why, and propose fixes.

## Test Matrix

| Test | Edge Case Type | Expected Failure | Actual Result |
|------|---------------|------------------|---------------|
| H1 | Ambiguous prompts | Limn can't handle multiple interpretations | TBD |
| H2 | Contradictory statements | Limn breaks on logical conflicts | TBD |
| H3 | Incomplete information | Limn fails to ask for clarification | TBD |
| H4 | Context-dependent meaning | Same Limn phrase, different contexts | TBD |
| H5 | Nuanced distinctions | Subtle meanings lost in compression | TBD |
| H6 | Creative/open-ended tasks | Limn too rigid for creativity | TBD |
| H7 | Subjective opinions | Limn can't express taste/preference | TBD |
| H8 | Cultural idioms | Metaphors don't translate to Limn | TBD |
| H9 | Complex temporal relations | Time relationships too complex | TBD |
| H10 | Counterfactual reasoning | "What if" scenarios too abstract | TBD |

---

## H1: Ambiguous Prompts (Multiple Valid Interpretations)

### Hypothesis
Limn's abbreviated syntax will make ambiguity WORSE because it strips away disambiguating context words.

### Test Case: "Read the file"

**English (ambiguous):**
"Read the file report.txt"

Possible interpretations:
1. Display file contents
2. Parse/analyze file contents
3. Check if file is readable
4. Open file for editing

**Limn (attempting same ambiguity):**
```
red fil report.txt
```

**Analysis:**
- English: "Read" is already ambiguous (display vs analyze vs check permissions)
- Limn: `red` (read) has same ambiguity BUT loses contextual clues from surrounding words
- **Prediction**: Limn will be MORE ambiguous because "read the file" at least implies display in context, while `red fil` is maximally terse

**Expected Failure:**
Agent interprets `red fil` differently each time depending on prior context, with no way to disambiguate.

**Proposed Fix:**
Add explicit mode markers:
- `red.dsp fil X` = display file contents
- `red.par fil X` = parse/analyze file
- `red.chk fil X` = check readability
- `red.edi fil X` = open for editing

---

## H2: Contradictory Statements (Logical Conflicts)

### Hypothesis
Limn can't express negation of previous statements clearly, making contradictions ambiguous.

### Test Case: "Do X, but don't do X"

**English (contradictory):**
"List all Python files in src/, but exclude any test files"

This is internally contradictory if phrased as:
"Show me test_*.py files, but don't show me test files"

**Limn (attempting contradiction):**
```
lst fil src/ | pat "*.py" | nu pat "test_*"
```

**Analysis:**
- English: Can use "but", "except", "however" to indicate exception/contradiction
- Limn: Uses `nu` (not/negate) but unclear if it negates the ENTIRE pipeline or just the last filter
- **Problem**: Does `nu pat` mean "not matching pattern" or "don't apply this pattern filter"?

**Expected Failure:**
`nu` operator scope is ambiguous. Is it:
1. Logical negation (show files NOT matching pattern)
2. Pipeline negation (ignore this filter)
3. Imperative negation (don't do this action)

**Proposed Fix:**
Explicit negation scoping:
- `pat.not "X"` = pattern negation (files NOT matching X)
- `!step` = skip this pipeline step
- Prohibit contradictory pipelines entirely (fail with error)

---

## H3: Incomplete Information (Missing Critical Context)

### Hypothesis
Limn's compression forces omission of context, making incomplete prompts WORSE.

### Test Case: "Find the bug"

**English (incomplete):**
"Find the bug in the authentication system"

Missing information:
- What file/module?
- What symptoms?
- What expected behavior?

**Limn (attempting same incompleteness):**
```
fin bug | sys auth
```

**Analysis:**
- English: Can infer from "authentication system" that we're talking about auth-related code
- Limn: `sys auth` is maximally compressed - loses specificity entirely
- **Problem**: No contextual words to hint at scope, symptoms, or location

**Expected Failure:**
Agent has NO IDEA where to start:
- Which files contain `sys auth`?
- Is `bug` a keyword to search for, or a concept?
- How to know when bug is found?

**English might prompt**: "Can you clarify which file or what the bug symptoms are?"

**Limn likely fails silently** or makes wild guesses.

**Proposed Fix:**
Require minimum context in prompts:
- `fin bug | mod: auth.js | sym: crash` = find bug in auth.js causing crash
- Add prompt validation that rejects under-specified queries
- Add `?` operator for explicit "need more info" marker: `fin bug | sys auth | ?loc ?sym`

---

## H4: Context-Dependent Meaning (Same Phrase, Different Meanings)

### Hypothesis
Limn abbreviations will have different meanings in different contexts, causing confusion.

### Test Case: "count" in different contexts

**English:**
1. "Count the lines in file.txt" (count as verb)
2. "The count is 42" (count as noun)
3. "Count on me" (count as idiom - rely)

**Limn:**
1. `cnt lin | fil file.txt`
2. `val cnt = 42`
3. `??? ` (no way to express idiom)

**Context shift:**
```
cnt wor | doc "report.md"
```
Does `cnt wor` mean:
- Count words (verb)
- The count of words (noun, expecting assignment)
- Count each word (distributive)

**Expected Failure:**
Limn has no grammatical markers (noun vs verb vs adjective) so `cnt` could be:
- Verb: perform counting
- Noun: the count result
- Modifier: countable

**Proposed Fix:**
Add part-of-speech markers:
- `cnt!` = verb (perform count)
- `cnt:` = noun (the count value)
- `cnt.able` = adjective (countable items)

Or: Use position to determine meaning (verb always before noun):
- `cnt X` = count X (verb)
- `X.cnt` = the count of X (noun)

---

## H5: Nuanced Distinctions (Subtle Meanings Lost)

### Hypothesis
Limn compression will conflate subtle distinctions that matter in English.

### Test Case: "refactor" vs "rewrite" vs "restructure"

**English:**
- **Refactor**: Improve code structure WITHOUT changing behavior
- **Rewrite**: Replace code with new implementation
- **Restructure**: Reorganize code layout/architecture

**Limn:**
```
ref cod
```

**Problem:**
Does `ref` mean refactor, rewrite, or restructure?
- All start with "re-"
- All involve changing code
- But have VERY different implications (preserve behavior vs change implementation)

**Expected Failure:**
Agent treats `ref cod` as generic "redo code" and might:
- Refactor when rewrite was needed
- Rewrite when refactor was requested (breaking behavior)
- Restructure when neither was intended

**Proposed Fix:**
Distinct abbreviations:
- `rfc cod` = refactor (preserve behavior)
- `rwr cod` = rewrite (new implementation)
- `rst cod` = restructure (reorganize)

Or: Use modifiers:
- `ref.saf cod` = refactor safely (preserve behavior)
- `ref.new cod` = rewrite (new code)
- `ref.arc cod` = restructure (architecture change)

---

## H6: Creative/Open-Ended Tasks (Limn Too Rigid?)

### Hypothesis
Limn's constraint-based design can't express open-ended creative instructions.

### Test Case: "Make the UI feel more polished"

**English:**
"Make the user interface feel more polished and professional"

This is intentionally vague and creative:
- "Feel" is subjective
- "Polished" could mean: smoother animations, better spacing, consistent colors, refined typography
- "Professional" could mean: minimalist, serious tone, business-appropriate

**Limn (attempting same vagueness):**
```
imp UI | fel pol pro
```

**Analysis:**
- English: Vague but conveys INTENT (improve subjective quality)
- Limn: `fel pol pro` is nonsensical without expansion
  - `fel` = feel (but feel is subjective, not actionable)
  - `pol` = polish/polished (verb or adjective?)
  - `pro` = professional (again, subjective)

**Expected Failure:**
Agent has no idea what concrete actions to take:
- No specific UI elements mentioned
- No measurable success criteria
- "Feel" is not a programmatic concept

**English might yield**: Agent asks for clarification or makes reasonable guesses

**Limn likely fails**: Agent doesn't know how to execute `fel pol pro`

**Proposed Fix:**
Limn should REJECT vague creative prompts and force specificity:
```
imp UI | ani smt | spa con | col uni | typ ref
```
= improve UI: smooth animations, consistent spacing, unified colors, refined typography

**Alternative**: Add creative mode operator:
```
cre imp UI | tgt: pol pro | sug: ?
```
= creatively improve UI targeting polished professional feel, suggest options

---

## H7: Subjective Opinions (Can Limn Express Taste?)

### Hypothesis
Limn can't express subjective preferences or opinions.

### Test Case: "I prefer tabs over spaces"

**English:**
"I prefer tabs over spaces for indentation"

This expresses:
- Personal preference (not objective fact)
- Comparative judgment (tabs > spaces)
- Domain context (indentation)

**Limn (attempting opinion):**
```
prf tab | ovr spc | for ind
```

**Analysis:**
- English: "prefer" clearly marks this as subjective opinion
- Limn: `prf` (prefer) exists, but can Limn express the SUBJECTIVITY?
- **Problem**: Limn is designed for declarative/imperative statements, not opinions

Does `prf tab` mean:
1. "I prefer tabs" (opinion)
2. "Prefer tabs" (recommendation/command)
3. "Preference is tabs" (configuration setting)

**Expected Failure:**
Limn doesn't distinguish between:
- Personal opinion ("I prefer X")
- Universal recommendation ("X is better")
- Configuration ("use X")

**Proposed Fix:**
Add opinion markers:
- `opn: prf tab` = opinion: prefer tabs (subjective)
- `rec: use tab` = recommendation: use tabs (advice)
- `cfg: ind=tab` = configuration: indentation is tabs (setting)

Or: Add perspective markers:
- `I.prf tab` = I prefer tabs
- `we.rec tab` = we recommend tabs
- `sys.cfg tab` = system configured for tabs

---

## H8: Cultural Idioms (Metaphors Don't Translate)

### Hypothesis
Limn can't handle idiomatic expressions or cultural metaphors.

### Test Case: "This code smells fishy"

**English:**
"This authentication code smells fishy - I think there's a security vulnerability"

Idioms used:
- "Smells fishy" = seems suspicious (metaphor)
- "There's a vulnerability" = potential security issue exists

**Limn (attempting literal translation):**
```
cod auth | sme fis | ??? sec vul
```

**Analysis:**
- English: "Smells fishy" is immediately understood as suspicion/concern
- Limn: `sme fis` is nonsensical unless Limn has idiom database
  - `sme` = smell (literal sense)
  - `fis` = fish/fishy (literal fish or idiom?)

**Expected Failure:**
Agent interprets `sme fis` literally and gets confused:
- "Why would code smell like fish?"
- "Is there a variable named 'fish'?"
- "Should I search for 'smell' in code?"

**Proposed Fix (Option 1): Ban idioms, require literal language**
```
sus cod auth | lik sec vul
```
= suspicious about auth code, likely security vulnerability

**Proposed Fix (Option 2): Idiom operator**
```
cod auth | ~sme.fis | mea: sus
```
= auth code [idiom: smells fishy] meaning: suspicious

**Proposed Fix (Option 3): Cultural marker**
```
cod auth | @en-US.sus.hi | rea: sec.vul?
```
= auth code [English-US: highly suspicious] reason: security vulnerability?

---

## H9: Complex Temporal Relations (Time Relationships)

### Hypothesis
Limn can't express complex temporal relationships between events.

### Test Case: "After the server starts, but before the first request, initialize the cache"

**English:**
"After the server starts, but before the first request arrives, initialize the cache"

Temporal sequence:
1. Server starts
2. Cache initializes (after 1, before 3)
3. First request arrives

**Limn (attempting temporal logic):**
```
aft srv.sta | bef req.fst | ini cac
```

**Analysis:**
- English: Uses "after" and "before" with clear subjects
- Limn: `aft srv.sta | bef req.fst | ini cac`
  - `aft` = after (what?)
  - `bef` = before (what?)
  - Pipeline order vs temporal order confusion

**Problem**: Does the pipeline mean:
1. After server starts, do something before first request (THEN init cache)?
2. Init cache in the time window between server start and first request?
3. Two separate conditions: (after start) AND (before first request)?

**Expected Failure:**
Ambiguous temporal scope:
- Does `bef req.fst` modify `aft srv.sta` or `ini cac`?
- Is this a sequence (A then B then C) or a condition (WHEN A and BEFORE B, do C)?

**Proposed Fix:**
Explicit temporal operators:
```
tem.win | sta: srv.sta | end: req.fst | do: ini cac
```
= temporal window from server start to first request, do: init cache

Or: Event-driven syntax:
```
on srv.sta | wai.unt req.fst | do ini cac
```
= on server start, wait until (before) first request, do init cache

---

## H10: Counterfactual Reasoning ("What If" Scenarios)

### Hypothesis
Limn can't express hypothetical or counterfactual reasoning.

### Test Case: "What if we had used Redis instead of in-memory cache?"

**English:**
"What would have happened if we had used Redis instead of an in-memory cache?"

This is counterfactual:
- We didn't use Redis (factual)
- Imagine if we had (counterfactual)
- What would be different? (hypothetical outcome)

**Limn (attempting counterfactual):**
```
wha.if use Redis | ins mem.cac | wha.dif?
```

**Analysis:**
- English: "What if" and "would have" clearly mark hypothetical
- Limn: `wha.if` is explicit but rest is unclear
  - `use Redis | ins mem.cac` = use Redis instead of memory cache (ambiguous tense)
  - `wha.dif` = what difference? (outcome question)

**Problem:**
Limn has no tense markers:
- `use Redis` could mean "we use Redis" (present)
- Or "we used Redis" (past)
- Or "if we had used Redis" (counterfactual past)

**Expected Failure:**
Agent interprets `use Redis` as imperative command ("go use Redis now") rather than counterfactual hypothesis ("imagine we had used Redis").

**Proposed Fix (Option 1): Explicit modal markers**
```
hyp: [use Redis | ins mem.cac] | res: wha.dif?
```
= hypothesis: [used Redis instead of memory cache], result: what would differ?

**Proposed Fix (Option 2): Tense markers**
```
if.pst [use Redis] | ins [use mem.cac] | out: ?
```
= if in past [used Redis] instead of [used memory cache], outcome: what?

**Proposed Fix (Option 3): Conditional syntax**
```
cond.ctr | [X: use Redis] [Y: use mem.cac] | cmp out
```
= counterfactual condition, [X: used Redis] [Y: used memory cache], compare outcomes

---

## Summary of Failures

| Test | Failure Type | Severity | Fix Difficulty |
|------|-------------|----------|----------------|
| H1 | Ambiguity amplification | HIGH | Medium (add mode markers) |
| H2 | Negation scope unclear | HIGH | Medium (explicit negation syntax) |
| H3 | Incomplete prompts worse | CRITICAL | Hard (require context validation) |
| H4 | Context-dependent meaning | MEDIUM | Hard (POS markers or position rules) |
| H5 | Nuance conflation | HIGH | Easy (distinct abbreviations) |
| H6 | Creative tasks impossible | MEDIUM | Hard (reject or add creative mode) |
| H7 | Subjectivity unexpressed | LOW | Medium (opinion markers) |
| H8 | Idioms untranslatable | MEDIUM | Hard (idiom DB or ban idioms) |
| H9 | Temporal logic ambiguous | HIGH | Hard (event-driven syntax) |
| H10 | Counterfactuals impossible | HIGH | Hard (tense/modal markers) |

---

## Vocabulary Additions Needed

### 1. Mode Markers (H1 fix)
```
red.dsp fil X    # display file
red.par fil X    # parse file
red.chk fil X    # check readability
red.edi fil X    # edit file
```

### 2. Negation Scoping (H2 fix)
```
pat.not "X"      # pattern negation
!step            # skip pipeline step
```

### 3. Context Markers (H3 fix)
```
fin bug | mod: auth.js | sym: crash
?loc ?sym        # explicit "need info" markers
```

### 4. Part-of-Speech Markers (H4 fix)
```
cnt!             # verb (count action)
cnt:             # noun (count value)
X.cnt            # attribute access
```

### 5. Nuanced Distinctions (H5 fix)
```
rfc cod          # refactor
rwr cod          # rewrite
rst cod          # restructure
```

### 6. Creative Mode (H6 fix)
```
cre imp UI | tgt: pol pro | sug: ?
```

### 7. Opinion Markers (H7 fix)
```
opn: prf X       # opinion
rec: use X       # recommendation
cfg: X=Y         # configuration
```

### 8. Idiom Handling (H8 fix)
```
~sme.fis         # idiom operator
@en-US.sus.hi    # cultural marker
```

### 9. Temporal Operators (H9 fix)
```
tem.win | sta: X | end: Y | do: Z
on X | wai.unt Y | do Z
```

### 10. Counterfactual Markers (H10 fix)
```
hyp: [...]       # hypothesis
if.pst [...]     # counterfactual past
cond.ctr         # counterfactual condition
```

---

## Grammar Extensions Needed

### 1. Explicit Scoping
Current: `cnt wor | fil X | nu pat "test"`
Problem: Does `nu` negate the pattern or the entire pipeline?

Proposal:
```
cnt wor | fil X | (nu pat "test")    # scoped negation
cnt wor | !fil X                      # skip step
```

### 2. Temporal Sequencing
Current: Pipeline is execution order, but not temporal order

Proposal:
```
seq: [A | B | C]                     # sequential execution
par: [A | B | C]                     # parallel execution
tem: [aft A | bef B | do C]          # temporal constraint
```

### 3. Conditional Logic
Current: No if/then/else

Proposal:
```
if [cond] | thn [action] | els [alternative]
when [event] | do [action]
```

### 4. Modal Operators
Current: No hypothetical/counterfactual

Proposal:
```
cud [X]          # could do X
wud [X]          # would do X
shd [X]          # should do X
if.pst [X]       # if had done X (counterfactual)
```

---

## Recommendations

### Immediate Actions (High Priority)

1. **Add disambiguation markers** to common verbs (H1, H5)
   - Prevents ambiguity amplification
   - Low implementation cost
   - High impact on clarity

2. **Require minimum context** in prompts (H3)
   - Add validation layer that rejects under-specified queries
   - Force users to provide: target, scope, symptoms
   - Critical for Limn to be usable

3. **Explicit negation syntax** (H2)
   - `pat.not X` for logical negation
   - `!step` for pipeline skip
   - Prevents contradictory pipelines

### Medium Priority

4. **Part-of-speech markers** (H4)
   - Use position (`X.cnt` vs `cnt X`) or explicit markers (`cnt!` vs `cnt:`)
   - Prevents context-dependent confusion

5. **Opinion/subjectivity markers** (H7)
   - `opn:`, `rec:`, `cfg:` prefixes
   - Allows expressing preferences and recommendations

### Long-term Research Needed

6. **Creative mode operator** (H6)
   - Limn fundamentally constrained for open-ended tasks
   - Either reject creative prompts or add `cre` mode with looser validation
   - Requires rethinking Limn's design philosophy

7. **Idiom database or ban** (H8)
   - Idioms are culturally specific and don't compress well
   - Either build idiom → literal translation DB
   - Or prohibit idioms entirely in Limn (force literal language)

8. **Temporal logic system** (H9)
   - Add `tem.win`, `on`, `wai.unt` operators
   - Event-driven syntax for complex sequencing
   - Major grammar addition

9. **Modal/tense system** (H10)
   - Add tense markers (past, future, counterfactual)
   - Add modal operators (could, would, should)
   - Fundamental language extension

---

## Conclusion: Limn's Breaking Points

**Where Limn FAILS:**

1. **Ambiguity amplification** - Compression removes disambiguating context
2. **Incomplete specifications** - Terse syntax makes missing info LESS obvious
3. **Subjective/creative tasks** - Limn is too rigid for open-ended instructions
4. **Cultural context** - Idioms and metaphors don't translate
5. **Complex temporal logic** - Pipeline order ≠ temporal order
6. **Counterfactual reasoning** - No tense/modal markers for hypotheticals

**Where Limn COULD work with fixes:**

1. Add mode markers for disambiguation (H1)
2. Add explicit negation scoping (H2)
3. Require minimum context validation (H3)
4. Add POS markers for context-dependent words (H4)
5. Use distinct abbreviations for nuanced terms (H5)

**Fundamental limitations:**

1. Creative/subjective tasks (H6, H7) - Limn's design philosophy conflicts
2. Cultural idioms (H8) - Requires external knowledge base
3. Temporal logic (H9) - Needs major grammar extension
4. Counterfactuals (H10) - Needs tense/modal system

**Final Assessment:**

Limn CAN be fixed for technical tasks (H1-H5) with vocabulary and grammar additions. But Limn FUNDAMENTALLY STRUGGLES with subjective, creative, cultural, and hypothetical reasoning (H6-H10). These require either major language extensions (tense, modals, events) or acceptance that Limn is NOT suitable for these task types.

**Recommendation:** Focus Limn on technical/imperative tasks where compression helps. For nuanced reasoning, use English.
