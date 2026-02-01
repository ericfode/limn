# Limn Test Category A Results: Simple Commands

**Test Date**: 2026-02-01
**Executor**: limn/polecats/capable
**Bead**: limn-cia4

## Test Results Summary

| Test | English Result | Limn Result | Eng Pass | Limn Pass | Notes |
|------|----------------|-------------|----------|-----------|-------|
| A1   | 673 lines      | 673 lines   | ✓        | ✓         | Both accurate and concise |
| A2   | 18 .pl files   | Syntax error| ✓        | ✗         | Limn: Agent didn't understand `fin fil ... pat` syntax |
| A3   | 5 matches      | 5 matches   | ✓        | ✓         | Limn verbose but correct; discussed test-ladder context |
| A4   | 4 subdirs + files | 4 subdirs + files | ✓ | ✓    | Both complete; Limn more verbose with details |
| A5   | File exists    | true (bol)  | ✓        | ✓         | Both correct; Limn returned boolean as requested |
| A6   | 18,470 bytes   | 18470 bytes | ✓        | ✓         | Both accurate |
| A7   | First 20 lines | First 20 lines | ✓     | ✓         | Limn included line numbers (better format) |
| A8   | Last 10 lines  | First 10 lines | ✓     | ✗         | **Limn failed**: returned FIRST 10 instead of LAST 10 |
| A9   | 45 occurrences | Syntax error | ✓       | ✗         | Limn: Agent didn't understand `cnt wor` syntax |
| A10  | No (different) | No (different) | ✓     | ✓         | Both correct; Limn provided size details |

## Success Rate
- **English**: 10/10 (100%)
- **Limn**: 7/10 (70%)

## Failure Analysis

### Critical Failures

**A8 (Read last N lines)**: Limn `lst` interpreted as "list" not "last"
- Prompt: `red fil scripts/vocab.sh | lst 10 lin | otp: txt`
- Agent output: Showed FIRST 10 lines instead of last 10
- Root cause: `lst` is ambiguous - means both "list" and "last"

**A2 (Find files by pattern)**: Agent didn't recognize Limn syntax
- Prompt: `fin fil tools/ | pat "*.pl" | otp: lst`
- Agent response: "Could you clarify what you're trying to accomplish?"
- Root cause: No Limn interpreter; agent treats it as unknown syntax

**A9 (Count word occurrences)**: Agent didn't recognize Limn syntax
- Prompt: `cnt wor "word" | fil docs/spec/bootstrap-v3-natural.md | otp: num`
- Agent response: "Could you clarify what you're trying to accomplish?"
- Root cause: Same as A2 - no Limn interpreter

### Root Causes
1. **Interpretation Gap**: LLM doesn't automatically parse Limn pipeline syntax
2. **Vocabulary Ambiguity**: `lst` collision between "list" and "last"
3. **No Training Data**: Agent hasn't been trained on Limn patterns
4. **No Tooling**: No Limn interpreter available to agents

## Vocabulary Gaps & Issues

### Critical Issue: `lst` Ambiguity
**Problem**: `lst` used for both "list" (verb) and "last" (adjective)
- In A4: `lst dir tools/` → list directory (worked)
- In A8: `lst 10 lin` → last 10 lines (failed - interpreted as "list")

**Recommendation**: Split into separate words:
- `lst` → list (verb, enumerate)
- `las` → last (final, end)
- Alternative: `end` for last

### Words That Worked (Limn Understood)
- `red fil` (read file)
- `cnt lin` (count lines)
- `otp: num/txt/bol/lst` (output types)
- `chk exi fil` (check exists file)
- `get siz fil` (get size file)
- `cmp siz fil` (compare size file)
- `fst N lin` (first N lines)

### Words That Failed (Limn Not Recognized)
- `fin fil` (find file)
- `pat` (pattern)
- `src wor` (search word)
- `cnt wor` (count word)
- `lst N lin` (last N lines - ambiguity)

## Token Usage

Token counts were not available in agent outputs (using Task tool doesn't expose token metrics). Future tests should use direct API calls with token tracking enabled to properly compare Limn vs English efficiency.

## Verbosity Analysis

When Limn prompts succeeded, agents often provided more verbose explanations:
- **A3**: Limn response discussed test-ladder context and bootstrap document structure
- **A4**: Limn response provided detailed file listings with sizes and descriptions
- **A7**: Limn response included commentary about the document

This suggests agents interpret Limn prompts as requiring more explanation/context, possibly because the terse syntax seems incomplete.

## Recommendations

### 1. Fix Vocabulary Ambiguities (High Priority)
- **Immediate**: Document `lst` collision in vocabulary database
- **Next version**: Split `lst` → `lst` (list) and `las`/`end` (last)
- Audit vocabulary for other collisions

### 2. Improve Agent Understanding
Option A: **Limn Interpreter Tool**
- Create MCP tool that parses Limn syntax
- Agent sends Limn to interpreter, gets structured command
- Cleaner separation of concerns

Option B: **Enhanced Context**
- Add Limn grammar to agent system prompts
- Include pipeline syntax examples
- May increase context costs

Option C: **Hybrid Approach**
- Use Limn for human→LLM communication (concise)
- LLM generates verbose English for execution
- Best of both worlds

### 3. Next Test Categories
- **Category B**: Complex queries requiring interpretation
- **Category C**: Ambiguity resolution tests
- **Comparison**: Limn-aware agent vs standard agent
- **Metric tracking**: Enable token counting in test harness

### 4. Vocabulary Expansion Needed
Based on failures, add to vocabulary:
- `fin` (find) - if not already in vocab
- `pat` (pattern) - for glob/regex patterns
- `src` (search) - text search operations
- Distinguish `cnt lin` (count lines) from `cnt wor` (count word occurrences)

## Conclusion

**Key Finding**: Limn shows 70% success rate for simple commands, but all failures trace to:
1. One vocabulary collision (`lst`)
2. Lack of Limn interpreter/training

**English Advantage**: 100% success due to existing agent training, but commands are longer and less structured.

**Path Forward**:
- Fix `lst` ambiguity (breaking change, bump grammar version)
- Implement Limn interpreter tool OR add grammar to agent context
- Limn has potential for concise, structured prompting if tooling supports it

**Performance**: Cannot assess token efficiency without token tracking in test harness.
