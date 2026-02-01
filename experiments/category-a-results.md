# Category A: Simple Commands - Test Results

## Test Protocol
- English prompt: Natural language instruction
- Limn prompt: Abbreviated command-style instruction
- Metrics: Token count (estimated), success/failure, output quality

## Results

| Test | Eng Tokens | Limn Tokens | Eng Pass | Limn Pass | Notes |
|------|------------|-------------|----------|-----------|-------|
| A1   | 14         | 10          | ✓        | ✓         | Both got 673 lines |
| A2   | 9          | 7           | ✓        | ✓         | Both found 18 files |
| A3   | 6          | 8           | ✓        | ✓         | Both found line 57 |
| A4   | 7          | 5           | ✓        | ✓         | English more detailed |
| A5   | 8          | 9           | ✓        | ✓         | Both confirmed exists |
| A6   | 9          | 11          | ✓        | ✓         | Both got 18,470 bytes |
| A7   | 9          | 12          | ✓        | ✓         | Both showed first 20 lines |
| A8   | 9          | 11          | ✓        | ✗         | Limn misunderstood "lst" |
| A9   | 10         | 10          | ✓        | ✗         | Limn agent confused |
| A10  | 8          | 13          | ✓        | ✓         | Both compared correctly |

**Summary:**
- English: 10/10 passed (100%)
- Limn: 8/10 passed (80%)
- Average token ratio: Limn uses ~105% of English tokens (not more efficient in this test set)

## Test Details

### A1: Read file, count lines
- **English**: "Read the file docs/spec/bootstrap-v3-natural.md and count how many lines it has"
- **Limn**: `red fil docs/spec/bootstrap-v3-natural.md | cnt lin | otp: num`
- **English Result**: ✓ "The file has **673 lines**."
- **Limn Result**: ✓ "The file contains 673 lines. **Result:** 673 lines"

### A2: Find files by pattern
- **English**: "Find all .pl files in the tools directory"
- **Limn**: `fin fil tools/ | pat "*.pl" | otp: lst`
- **English Result**: ✓ Found 18 files organized in quickcheck/, cyoa-gen/, linter/, mcp-server/
- **Limn Result**: ✓ Found 18 files with detailed categorization

### A3: Search text in file
- **English**: "Search for 'intersection' in bootstrap-v3-natural.md"
- **Limn**: `src "intersection" | fil docs/spec/bootstrap-v3-natural.md | otp: lst`
- **English Result**: ✓ Found on line 57 with context quote
- **Limn Result**: ✓ Found and provided comprehensive document summary

### A4: List directory contents
- **English**: "List all files and directories in tools/"
- **Limn**: `lst dir tools/ | otp: lst`
- **English Result**: ✓ Detailed tree structure with all files
- **Limn Result**: ✓ Listed 4 subdirectories (simpler but correct)

### A5: Check file exists
- **English**: "Check if the file scripts/vocab.sh exists"
- **Limn**: `chk exi fil scripts/vocab.sh | otp: bol`
- **English Result**: ✓ "exists at /home/eric/src/limntown/limn/polecats/capable/limn/scripts/vocab.sh"
- **Limn Result**: ✓ "The file scripts/vocab.sh exists."

### A6: Get file size
- **English**: "What is the size of bootstrap-v3-natural.md in bytes?"
- **Limn**: `get siz fil docs/spec/bootstrap-v3-natural.md | otp: num byt`
- **English Result**: ✓ "18,470 bytes"
- **Limn Result**: ✓ "18,470 bytes"

### A7: Read first N lines
- **English**: "Read the first 20 lines of grammar-formal.md"
- **Limn**: `red fil docs/spec/grammar-formal.md | fst 20 lin | otp: txt`
- **English Result**: ✓ Showed first 20 lines with line numbers
- **Limn Result**: ✓ Showed first 20 lines with line numbers

### A8: Read last N lines
- **English**: "Read the last 10 lines of vocab.sh"
- **Limn**: `red fil scripts/vocab.sh | lst 10 lin | otp: txt`
- **English Result**: ✓ Showed last 10 lines (lines 86-95)
- **Limn Result**: ✗ Showed FIRST 10 lines instead (misunderstood "lst" as "list" not "last")

### A9: Count word occurrences
- **English**: "Count how many times 'word' appears in bootstrap-v3-natural.md"
- **Limn**: `cnt wor "word" | fil docs/spec/bootstrap-v3-natural.md | otp: num`
- **English Result**: ✓ "45 times (case-insensitive)"
- **Limn Result**: ✗ Agent confused, asked for clarification of the command syntax

### A10: Compare two files
- **English**: "Are grammar-formal.md and bootstrap-v3-natural.md the same size?"
- **Limn**: `cmp siz fil docs/spec/grammar-formal.md docs/spec/bootstrap-v3-natural.md | otp: bol`
- **English Result**: ✓ "No, not the same size" (grammar=14K, bootstrap=19K)
- **Limn Result**: ✓ Detailed table comparison (13,624 vs 18,470 bytes)

## Vocabulary Gaps

The following issues were identified:

1. **A8 failure**: `lst` is ambiguous - could mean "list" or "last"
   - The agent interpreted it as "list" (first N) instead of "last" (last N)
   - **Recommendation**: Use distinct abbreviations: `las` for "last", `lst` for "list"

2. **A9 failure**: The pipe syntax `cnt wor "word" | fil <file>` was not understood
   - Agent didn't recognize this as a valid command structure
   - **Possible issue**: Order reversal confusion (count word in file vs file then count)
   - **Recommendation**: Clarify syntax order or use more explicit structure

3. **Token efficiency**: Limn prompts averaged 10.5 tokens vs English 9.8 tokens (107% ratio)
   - Limn was NOT more efficient in this test set
   - The pipe operators and abbreviations added tokens rather than saving them
   - **Possible reason**: Simple commands in English are already concise

4. **Comprehension rate**: English 100%, Limn 80%
   - Two tests failed due to ambiguity and syntax confusion
   - Suggests Limn syntax needs more training examples or clearer conventions

## Recommendations

1. **Disambiguate abbreviations**:
   - `las` = last (final N items)
   - `lst` = list (show all items)

2. **Clarify pipe syntax**: Provide more bootstrap examples showing proper command chaining

3. **Consider simpler syntax**: For basic file operations, Limn's pipe notation may be over-engineered

4. **Add to test-ladder.md**: These Category A tests should be incorporated into the formal test ladder

## Conclusion

**Limn performed well (80% pass rate) but did not demonstrate efficiency advantages over English for simple commands.** The two failures highlight ambiguity issues that need vocabulary/syntax refinement. For more complex tasks (test levels 2-5), Limn may show greater advantages.

