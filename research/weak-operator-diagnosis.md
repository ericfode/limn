# Weak Operator Diagnosis: Why `:` Scores 53% and `\` Scores 58%

> **ope wea | tes ign | sym ove > operators weak | test ignored | symbols overloaded**

**Author:** Quinn (Dr. Solvik), Linguist
**Date:** 2026-02-06
**Bead:** hq-ibgav.3.1

---

## Executive Summary

The two weakest operators in Limn (`:` conditional at 53%, `\` subtraction at 58%) are weak because **the spec overrode its own test results**. The alternative-symbols test (2026-02-03) recommended word operators (`given` at 38/40, `without` at 38/40) over symbols (`:` at 21/40, `\` at 23/40). The spec adopted the losing candidates for compactness. The one operator whose test recommendation WAS followed (`±` at 35/40) scores 88% — the highest of the three.

**Root cause: symbol overloading.** `:` and `\` are heavily loaded in programming, mathematics, and natural language contexts. LLMs have strong priors for these symbols that compete with Limn's intended semantics.

**Fix: adopt the winning candidates from the original test.**

---

## 1. The Evidence

### Alternative Symbols Test (2026-02-03)

Three operators were tested with multiple symbol candidates:

| Operator | Recommended | Score | Adopted | Score | Delta |
|----------|-------------|-------|---------|-------|-------|
| Subtraction | `without` | **38/40** | `\` | 23/40 | -15 |
| Superposition | `±` | **35/40** | `±` | 35/40 | 0 |
| Conditional | `given` | **38/40** | `:` | 21/40 | -17 |

The test explicitly warned:

**About `:`:**
> "Extremely overloaded. Already used in logic/type theory. Heavily used for time, lists, key-value pairs, labels, and ratios. Not intuitive for 'conditional'. High ambiguity risk in parsing context."

**About `\`:**
> "ASCII-friendly and moderately intuitive but heavily overloaded in programming contexts. Conflicts with escape character semantics and path syntax expectations. Less intuitive for non-programmers."

### Bootstrap v4 Validation (from spec)

| Operator | Score | % |
|----------|-------|---|
| `@` (projection) | — | 100% |
| `^` (gradient) | — | 100% |
| `\` (subtraction) | — | 100% (uniqueness, not comprehension) |
| `*` (interference) | — | 92% |
| `±` (superposition) | 35/40 | 88% |
| `:` (conditional) | 21/40 | 53% |

Note: `\` is listed as "100% uniqueness" — meaning generated expressions were unique, NOT that comprehension was 100%. The actual comprehension test for `\` was the 23/40 symbol test.

### Correlation

| Followed test recommendation? | Comprehension score |
|-------------------------------|-------------------|
| Yes (`±`) | 88% |
| No (`:`) | 53% |
| No (`\`) | 23/40 raw → unmeasured in bootstrap |

**Perfect correlation.** The operator that followed the test recommendation works. The ones that didn't, don't.

---

## 2. Why Symbols Fail Here

### LLM Prior Competition

LLMs have been trained on billions of tokens where `:` means:
- Key-value separator (`"name": "value"`)
- Time notation (`14:30`)
- Python slice notation (`list[1:3]`)
- Type annotation (`x: int`)
- URL scheme separator (`https://`)
- List/enumeration marker
- Ratio notation (`3:1`)

When an LLM sees `lov:trs`, it must override ALL of these priors to interpret `:` as "given the context of." This is an enormous cognitive load.

Similarly, `\` triggers:
- Escape character (`\n`, `\t`)
- File path separator (`C:\Users`)
- LaTeX command prefix (`\frac`)
- Regex escape (`\.`)
- Line continuation

`lov\fer` looks like an escape sequence, not "love without fear."

### Word Operators Have No Competition

`lov given trs` has exactly one interpretation. `lov without fer` has exactly one interpretation. The word IS the semantics. There are no competing priors.

### The `±` Exception

`±` works because it has ONE strong prior (plus-or-minus, both simultaneously) that ALIGNS with superposition semantics. It's not competing with the intended meaning — it IS the intended meaning.

---

## 3. The Compactness Trap

The likely reason the spec overrode the test: compactness.

| Form | Characters | Example |
|------|-----------|---------|
| `lov:trs` | 7 | compact |
| `lov given trs` | 13 | verbose |
| `king\man` | 8 | compact |
| `king without man` | 16 | verbose |

The spec prioritized visual compactness over comprehension. This was the wrong tradeoff. A language that's compact but misunderstood 47% of the time is worse than a verbose language understood 95% of the time.

### Information-Theoretic Argument

If `:` is misinterpreted 47% of the time, it carries only ~0.53 × intended_bits of useful information per use. The "savings" from compactness are illusory — you're transmitting noise.

`given` is 5 extra characters but carries ~0.95 × intended_bits. Net information gain: positive.

---

## 4. Recommendation

### Option A: Adopt Word Operators (Recommended)

Replace the two weakest symbols with the test-recommended word operators:

| Current | Proposed | Comprehension improvement |
|---------|----------|--------------------------|
| `A:B` | `A given B` | 53% → ~95% (projected from 38/40) |
| `A\B` | `A without B` | ~58% → ~95% (projected from 38/40) |

**Impact on existing expressions:**

```
Before: lov:trs          After: lov given trs
Before: fer:dan          After: fer given dan
Before: king\man         After: king without man
Before: lov\fer          After: lov without fer
```

**Precedence:** `given` and `without` slot into the existing hierarchy as low-precedence infix operators:

```
^  highest
@
*
without  (was \)
given    (was :)
±  lowest
```

### Option B: Hybrid — Keep Symbols as Aliases

Allow both forms:
- `A:B` and `A given B` are synonyms
- `A\B` and `A without B` are synonyms

Word forms are canonical in the bootstrap (for teaching). Symbol forms are permitted in dense expressions (for expert use). This preserves backward compatibility.

### Option C: Different Symbols

Find new symbols that don't compete with LLM priors:
- Conditional: `A|>B` or `A~>B` or `A⊃B`
- Subtraction: `A÷B` (scored 31/40, better than `\` at 23/40)

Less recommended — word operators scored highest by a wide margin.

---

## 5. The `±` Analysis (Why 88% Not Higher)

The 12% failure rate for `±` appears to stem from:

1. **Confusion with `*` (interference)**: Both create "combined" states. LLMs sometimes interpret `A±B` as "A and B interfering" rather than "A and B unresolved." The distinction (interference = emergent new state, superposition = both original states maintained) is subtle.

2. **Training representation**: H7 notes `±` has only 1.2% training representation in the v4 data. Underrepresentation, not symbol confusion, is the likely cause.

3. **The fix for `±` is volume, not redesign.** More examples in training data. The symbol itself (35/40) is well-chosen.

---

## 6. Next Steps

1. **Decision needed:** Option A, B, or C? (Recommend B — hybrid with word-canonical)
2. **Update bootstrap spec** with chosen approach
3. **Update grammar EBNF** to accept word operators
4. **Regenerate expression databases** with new syntax
5. **Retest comprehension** on fresh LLM instances
6. **Rebalance `±` training data** (address the 1.2% representation issue)

---

*tes sho | spe ign | wor win*
*(tests showed | spec ignored | words win)*

**— Quinn (Dr. Solvik), 2026-02-06**
