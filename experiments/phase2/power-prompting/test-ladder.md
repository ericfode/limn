# Limn Power Prompting Test Ladder

## Goal
Prove Limn prompts can be MORE effective than English.

## Test Protocol
1. Write task in English (baseline)
2. Write equivalent in Limn
3. Run both via fresh agent
4. Compare: tokens, accuracy, clarity
5. Iterate vocabulary if Limn fails

---

## Level 1: Simple Commands

### Test 1.1: File Read
**English:** "Read the file bootstrap-v3-natural.md and tell me how many sections it has"
**Limn:** `red fil docs/spec/bootstrap-v3-natural.md | cnt sec | otp: num`

### Test 1.2: Pattern Find
**English:** "Find all files ending in .pl in the tools directory"
**Limn:** `fin fil tools/ | pat "*.pl" | otp: lst`

### Test 1.3: Word Search
**English:** "Search for the word 'intersection' in the bootstrap document"
**Limn:** `src wor "intersection" | fil docs/spec/bootstrap-v3-natural.md | otp: lst lin`

---

## Level 2: Multi-Step Tasks

### Test 2.1: Read and Analyze
**English:** "Read the grammar-formal.md file, identify the main rules, and list them"
**Limn:** `red fil docs/spec/grammar-formal.md | ana rul | otp: lst rul`

### Test 2.2: Create and Verify
**English:** "Create a new file called test.txt with the content 'hello world', then verify it exists"
**Limn:** `cre fil test.txt | cnt "hello world" | ver exi`

### Test 2.3: Find and Count
**English:** "Find all Limn vocabulary words in domain 11, count them, report the total"
**Limn:** `get wor dom 11 | cnt | otp: num tot`

---

## Level 3: Reasoning Tasks

### Test 3.1: Code Analysis
**English:** "Analyze this code for potential bugs, focusing on error handling, and suggest fixes"
**Limn:** `ana cod | foc: err han | fin bug | sug fix | otp: lst pri`

### Test 3.2: Compare Options
**English:** "Compare these two approaches and recommend which is better, explaining why"
**Limn:** `cmp opc A B | ded bet | exp why | otp: rec`

### Test 3.3: Debug Task
**English:** "This test is failing. Find the root cause and propose a fix"
**Limn:** `tes fal | fin cau | ded rot | sug fix | otp: stp`

---

## Level 4: Complex Workflows

### Test 4.1: Feature Implementation
**English:** "Add a new command 'stats' to the vocab.sh script that shows word count by domain"
**Limn:** `cre fnc "stats" | fil scripts/vocab.sh | otp wor cnt per dom | tes wrk`

### Test 4.2: Refactoring
**English:** "Refactor the json.pl file to separate parsing from formatting, keeping tests passing"
**Limn:** `ref fil tools/mcp-server/json.pl | sep prs fmt | req: tes pas`

---

## Level 5: Meta-Tasks

### Test 5.1: Self-Correction Loop
**English:** "Try to fix this bug. If your fix doesn't work, analyze why and try again, up to 3 times"
**Limn:** `try fix bug | tes | if fal: ana why | rtr | lmt: 3 itr`

### Test 5.2: Adaptive Problem Solving
**English:** "Solve this problem. Start simple, if that fails try more complex approaches"
**Limn:** `sol prb | mod: sim fst | if fal: mod: cpx | itr til suc`

---

## Scoring Rubric

| Metric | Weight | Measure |
|--------|--------|---------|
| Token Efficiency | 30% | Limn tokens / English tokens |
| Task Completion | 40% | Did it work? (0/1) |
| Precision | 20% | Fewer retries needed |
| Clarity | 10% | Agent understood on first try |

## Success Threshold
- Level 1-2: Limn must match English (100% completion)
- Level 3-4: Limn must be ≥90% as effective
- Level 5: Limn must demonstrate advantage (fewer iterations)

---

## Vocabulary Gaps Log

Track words needed but missing:

| Test | Missing Word | Proposed | Status |
|------|-------------|----------|--------|
| 1.1 | count | cnt | ✓ added |
| 1.2 | pattern | pat | check |
| 2.1 | rule | rul | check |
| 3.1 | suggest | sug | check |
| 3.2 | option | opc | check |
| 3.3 | cause | cau | check |
| 4.1 | per | per | check |
| 5.1 | retry | rtr | ✓ exists |
