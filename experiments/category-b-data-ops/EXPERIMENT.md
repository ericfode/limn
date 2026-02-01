# Category B: Data Operations - Limn vs English

**Date:** 2026-02-01
**Experimenter:** Polecat Toast
**Purpose:** Compare Limn and English prompt effectiveness for common data operations

## Hypothesis

Limn's compositional structure should make data operation instructions more predictable and token-efficient than equivalent English expressions, particularly for structured tasks.

## Test Design

### Variables
- **Independent:** Language (Limn vs English)
- **Dependent:**
  - Token count (input)
  - Success rate (binary: correct output or not)
  - Output quality (1-5 scale)
  - Response tokens (output)

### Controls
- Same LLM model (Claude Sonnet 3.5)
- Same test data for each pair
- Same expected output format
- Same temperature (0.0 for consistency)

## Tests

### B1: Parse JSON and Extract Field
**Task:** Extract the first user's name from JSON data
**Data:** `test-data/b1-users.json`
**Limn:** `prs jso fil | get fld "name" | otp: vlu`
**English:** "Parse the JSON file and extract the 'name' field from the first user. Output only the value."
**Expected Output:** "Alice"

### B2: Filter List by Condition
**Task:** Filter numbers greater than 10
**Data:** `test-data/b2-numbers.json`
**Limn:** `flt lst | whr vlu > 10 | otp: lst`
**English:** "Filter the list to include only values greater than 10. Output the filtered list."
**Expected Output:** [15, 42, 19, 27, 11, 33]

### B3: Sort Data by Field
**Task:** Sort events by date
**Data:** `test-data/b3-events.json`
**Limn:** `srt dat | by fld "date" | otp: lst`
**English:** "Sort the data by the 'date' field in ascending order. Output the sorted list."
**Expected Output:** Events sorted chronologically

### B4: Group and Count
**Task:** Group items by type and count each group
**Data:** `test-data/b4-items.json`
**Limn:** `grp dat | by fld "type" | cnt | otp: tbl`
**English:** "Group the data by the 'type' field and count how many items are in each group. Output as a table."
**Expected Output:** fruit: 3, vegetable: 3

### B5: Validate Schema
**Task:** Check if user data matches schema
**Data:** `test-data/b5-user-data.json`, `test-data/b5-schema.json`
**Limn:** `val dat | aga sch | otp: bol err`
**English:** "Validate the data against the schema. Output true if valid, or false with errors if invalid."
**Expected Output:** true (or validation details)

### B6: Transform Structure
**Task:** Transform flat structure to nested
**Data:** `test-data/b6-flat.json`
**Limn:** `tra dat | frm A | to B | otp: dat`
**English:** "Transform the flat data structure (user_name, user_age, city_name, city_pop) into a nested structure with separate 'user' and 'city' objects. Output the transformed data."
**Expected Output:** Nested structure with user and city objects

### B7: Aggregate Statistics
**Task:** Calculate sum, avg, min, max of sales
**Data:** `test-data/b7-sales.json`
**Limn:** `agg dat | sum avg min max | otp: tbl`
**English:** "Aggregate statistics for the 'sales' field: calculate sum, average, minimum, and maximum. Output as a table."
**Expected Output:** Statistics table

### B8: Deduplicate by Field
**Task:** Remove duplicates based on id field
**Data:** `test-data/b8-duplicates.json`
**Limn:** `ded lst | by fld "id" | otp: lst`
**English:** "Remove duplicate entries based on the 'id' field, keeping only the first occurrence. Output the deduplicated list."
**Expected Output:** 4 unique users

### B9: Merge Datasets
**Task:** Merge users and salaries on id
**Data:** `test-data/b9-users.json`, `test-data/b9-salaries.json`
**Limn:** `mrg dat A B | on fld "id" | otp: dat`
**English:** "Merge the two datasets on the 'id' field, combining user information with salary data. Output the merged dataset."
**Expected Output:** Combined user+salary records

### B10: Query Nested Data
**Task:** Extract first user's name from nested structure
**Data:** `test-data/b10-nested.json`
**Limn:** `qry dat | pth "users[0].name" | otp: vlu`
**English:** "Query the nested data to extract the value at path 'users[0].name'. Output only the value."
**Expected Output:** "Alice"

## Methodology

1. Load test data files
2. For each test:
   - Send Limn prompt + data to Claude API
   - Record: input tokens, output tokens, response, success
   - Send English prompt + data to Claude API
   - Record: input tokens, output tokens, response, success
   - Compare results
3. Generate comparison table
4. Analyze patterns

## Metrics

### Token Efficiency
- Average input tokens: Limn vs English
- Average output tokens: Limn vs English
- Total tokens per test: Limn vs English

### Success Rate
- Correct outputs: Limn vs English
- Partial success: Limn vs English
- Failures: Limn vs English

### Vocabulary Gaps
- Missing Limn words needed for data operations
- Ambiguous constructions
- Workarounds required

## Expected Outcomes

1. **Token efficiency:** Limn prompts should be shorter (3-5 words vs 10-20 words)
2. **Success rate:** Both should succeed on well-defined tasks, but Limn may fail if vocabulary is missing
3. **Vocabulary gaps:** Likely missing words for: parse (prs?), filter (flt?), aggregate (agg?), validate (val?), transform (tra?), merge (mrg?), query (qry?)

## Implementation

See `run_tests.py` for test execution script.

## Results

_To be filled after running tests_

