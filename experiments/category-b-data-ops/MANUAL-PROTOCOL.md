# Manual Testing Protocol
## Category B: Data Operations - Limn vs English

**Note:** This experiment requires manual execution since automated API calls are not available in this environment.

## How to Run the Tests

### Prerequisites
1. Access to Claude (claude.ai or API)
2. Test data files in `test-data/` directory
3. Spreadsheet or table for recording results

### For Each Test (B1-B10):

#### Step 1: Run Limn Version
1. Open new Claude conversation
2. Send this prompt:
```
You are interpreting a Limn language command. Limn is a constructed language optimized for semantic precision.

Command: [INSERT LIMN COMMAND]

Data:
[INSERT TEST DATA]

Execute the command and provide the output as specified.
```

3. Record:
   - Input tokens (from API response or estimate)
   - Output tokens
   - Response text
   - Success (yes/no)
   - Quality (1-5)

#### Step 2: Run English Version
1. Open new Claude conversation (or use same one after separator)
2. Send this prompt:
```
Data:
[INSERT TEST DATA]

Task: [INSERT ENGLISH INSTRUCTION]
```

3. Record same metrics

#### Step 3: Compare
- Token efficiency (Limn vs English input tokens)
- Output quality
- Success rate

## Sample Test Execution

### B1: Parse JSON and Extract Field

**Test Data** (test-data/b1-users.json):
```json
{
  "users": [
    {"id": 1, "name": "Alice", "age": 30},
    {"id": 2, "name": "Bob", "age": 25},
    {"id": 3, "name": "Charlie", "age": 35}
  ],
  "metadata": {
    "version": "1.0",
    "created": "2026-01-15"
  }
}
```

**Limn Prompt:**
```
You are interpreting a Limn language command. Limn is a constructed language optimized for semantic precision.

Command: prs jso fil | get fld "name" | otp: vlu

Data:
{
  "users": [
    {"id": 1, "name": "Alice", "age": 30},
    {"id": 2, "name": "Bob", "age": 25},
    {"id": 3, "name": "Charlie", "age": 35}
  ],
  "metadata": {
    "version": "1.0",
    "created": "2026-01-15"
  }
}

Execute the command and provide the output as specified.
```

**English Prompt:**
```
Data:
{
  "users": [
    {"id": 1, "name": "Alice", "age": 30},
    {"id": 2, "name": "Bob", "age": 25},
    {"id": 3, "name": "Charlie", "age": 35}
  ],
  "metadata": {
    "version": "1.0",
    "created": "2026-01-15"
  }
}

Task: Parse the JSON file and extract the 'name' field from the first user. Output only the value.
```

**Expected Output:** "Alice"

## Quick Execution Template

For rapid testing, copy this template into a spreadsheet:

| Test | Language | Input Tokens | Output Tokens | Success | Quality | Notes |
|------|----------|--------------|---------------|---------|---------|-------|
| B1 | Limn | | | | | |
| B1 | English | | | | | |
| B2 | Limn | | | | | |
| B2 | English | | | | | |
...

## Alternative: Estimate Metrics

If full execution is not feasible, use these estimation methods:

### Input Token Estimation
- Count words in prompt × 1.3 (average tokens per word)
- Limn commands are typically 3-8 words
- English instructions are typically 15-30 words

### Example Estimates for B1:

**Limn Input:**
- Command: "prs jso fil | get fld \"name\" | otp: vlu" ≈ 8 words
- Context text: ≈ 30 words
- Data: ≈ 50 tokens
- Total: ~90 tokens

**English Input:**
- Task: "Parse the JSON file and extract..." ≈ 18 words
- Data: ≈ 50 tokens
- Total: ~75 tokens

## Vocabulary Gap Analysis

As you run tests, note any Limn vocabulary that doesn't exist:

| Test | Missing Word | English Equivalent | Suggested Limn Word |
|------|--------------|-------------------|---------------------|
| B1 | parse | parse/analyze | prs or ana |
| B2 | filter | filter/select | flt or sel |
| ... | | | |

## Completion Criteria

- All 10 tests executed (Limn + English = 20 prompt runs)
- Results table completed
- Vocabulary gaps documented
- Comparison analysis written

## Results Format

See `RESULTS.md` template for how to document findings.
