# Limn vs English: Power Prompting Test Results

**Date:** 2026-02-01
**Vocabulary:** 906 words
**Tests Run:** 80 (8 categories × 10 tests each)

## Executive Summary

Limn demonstrates **conditional superiority** over English:
- **Simple commands**: English wins (100% vs 80%)
- **Structured queries**: Limn wins (1.8:1 compression)
- **Complex workflows**: Limn wins (1.6:1 compression)
- **Reasoning tasks**: Comparable (Limn more compact)

**Conclusion**: Limn is NOT a universal replacement for English. It excels at **structured, multi-step tasks** where constraint composition provides clarity. For simple, one-shot commands, English remains superior.

---

## Category Results

### A: Simple Commands
| Metric | English | Limn |
|--------|---------|------|
| Pass Rate | 100% (10/10) | 80% (8/10) |
| Avg Tokens | 9.8 | 10.5 |
| Compression | - | 0.93:1 |

**Winner: English**

**Failures:**
- A8: `lst` ambiguous (list vs last) - **FIXED: added `las`**
- A9: Pipe syntax confused agent

### B: Data Operations
**Status:** Complete (toast)
**Winner: Limn** (structured data manipulation)

### C: Code Analysis
| Test | Compression Ratio |
|------|-------------------|
| C1: Find functions | 1.88:1 |
| C2: Count LOC | 1.86:1 |
| C3: Find TODOs | 1.86:1 |
| C4: List imports | 1.83:1 |

**Winner: Limn** (consistent 1.8x compression)

### D: Code Generation
**Status:** Complete (cheedo)
**Winner: Draw** (both capable, different strengths)

### E: Reasoning Tasks
| Test | English Tokens | Limn Tokens | Compression |
|------|----------------|-------------|-------------|
| E1: Explain code | 16 | 8 | 2.0:1 |
| E2: Debug test | 12 | 7 | 1.7:1 |

**Winner: Draw** (Limn more compact, English clearer)

### F: Complex Workflows
| Test | English Tokens | Limn Tokens | Compression |
|------|----------------|-------------|-------------|
| F1: Feature impl | 27 | 17 | 1.59:1 |
| F2: Multi-file refactor | ~20 | ~12 | 1.67:1 |

**Winner: Limn** (workflow specification strength)

### G: Meta-Tasks
**Status:** Complete (cheedo)
**Winner: Limn** (recursive/iterative specifications)

### H: Edge Cases (Try to Break Limn)
**Status:** Complete (valkyrie)
**Finding:** Limn handles ambiguity through superposition, not failure

---

## Vocabulary Fixes Applied

| Issue | Fix | Status |
|-------|-----|--------|
| lst/last ambiguity | Added `las` (last) | ✓ |
| Missing `abl` | Added (able) | ✓ |
| Missing `suf` | Added (suffer) | ✓ |
| Missing `pur` | Added (purpose) | ✓ |

---

## Key Insights

### 1. Complexity Threshold
Limn's overhead pays off above ~10 English words:
- <10 words: English wins
- 10-20 words: Draw
- >20 words: Limn wins

### 2. Structure Matters
Limn excels when tasks have:
- Clear phases (|)
- Constraint specifications (:)
- Output requirements (otp:)

### 3. Ambiguity Risk
3-letter abbreviations create collision risk. Mitigations:
- Distinct abbreviations for similar concepts
- Context markers (otp:, fil:, etc.)
- Vocabulary validation before use

### 4. Agent Comprehension
Agents understand Limn better when:
- Bootstrap examples are available
- Pipe syntax is consistent
- Output format is explicit

---

## Recommendations

1. **Position Limn for complex tasks**, not simple queries
2. **Expand vocabulary** for common ambiguities
3. **Create Limn→English training examples** for agents
4. **Document pipe syntax formally**
5. **Test with multiple LLM providers**

---

## Final Status

**ALL 8 CATEGORIES COMPLETE** ✓

| Category | Winner | Notes |
|----------|--------|-------|
| A: Simple Commands | English | 100% vs 80% |
| B: Data Operations | Limn | Structured data |
| C: Code Analysis | Limn | 1.8:1 compression |
| D: Code Generation | Draw | Both capable |
| E: Reasoning | Draw | Limn more compact |
| F: Complex Workflows | Limn | 1.6:1 compression |
| G: Meta-Tasks | Limn | Recursive specs |
| H: Edge Cases | Draw | Superposition handles ambiguity |

**Overall: Limn 4, English 1, Draw 3**

## Next Steps

- [x] Complete all 8 categories
- [ ] Merge polecat branches
- [ ] Create Limn prompt cookbook
- [ ] Publish findings to crew

---

*tes don | dat col | kno gro*
*(testing done | data collected | knowledge grows)*
