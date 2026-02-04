# ^ (Gradient) Operator Quick Reference

## What is ^?

The **gradient operator** enables **continuous intensity control** from 0 (absent) to 1 (maximum).

```
big^0.7  =  70% big (considerably big)
```

## Test Results: VIABLE ✓

- **19/19 tests passed** (100%)
- **79% average confidence**
- **Adds genuine expressiveness**

---

## Examples by Use Case

### Simple Intensity

```
tired^0.3    slightly tired
tired^0.6    moderately tired
tired^0.9    extremely tired

love^0.0     no love (indifferent)
love^0.5     moderate love
love^1.0     absolute love
```

### Weighted Emotions

```
lov^0.7 * hat^0.3     mostly love, some hate
hop^0.5 * des^0.5     equally hopeful and despairing
app^0.8 * res^0.2     mostly appreciative, some reservations
```

### Dimensional Focus

```
big^0.6 @ siz         considerably big (in size dimension)
con^0.7 @ tim         substantial concern (in time dimension)
str^0.8 @ emot        strong (in emotional dimension)
```

### Planned Sequences

```
start^0.5 → grow^0.7 → peak^1.0 → decline^0.6 → end^0.0
```

Start moderately, grow to strong, reach peak, decline somewhat, end gently.

---

## Natural Semantic Chunks

| Value | Common Interpretation |
|-------|----------------------|
| 0.0 | Absent, none, nonexistent |
| 0.25 | Slight, barely, minimal |
| 0.5 | Moderate, fair, balanced |
| 0.75 | Strong, deep, considerable |
| 1.0 | Maximum, absolute, complete |

---

## Composition with Other Operators

| Pattern | Meaning |
|---------|---------|
| `A^0.7 * B^0.3` | Weighted interference (70% A, 30% B) |
| `(A^0.7) @ ctx` | Scoped intensity (A at 70%, focused on context) |
| `A^0.8 → B^0.5` | Intensity-planned sequence |
| `(A^0.6) @ B^0.4` | Complex scoped gradient |

---

## Key Findings

### Strengths ✓
1. Continuous control (not just ±)
2. Maps naturally to language ("quite", "very", "slightly")
3. No conflicts with existing operators
4. Enables weighted compositions
5. Real expressiveness gain (3 choices → infinite choices)

### Design Questions
1. **Extended range**: Allow >1.0 or <0.0? (recommend: no initially)
2. **Nested gradients**: How to handle (A^0.7)^0.5? (recommend: flatten)
3. **Expression exponents**: Allow A^(B*C)? (recommend: no initially)
4. **Weight normalization**: Should A^x * B^y sum to 1.0? (recommend: define explicitly)

---

## Comparison: Before vs After

### Before (discrete modifiers)
```
love         = 1 option (baseline)
love+        = 1 intensifier (strong)
love-        = 1 weakener (mild)
→ Total: 3 intensity choices
```

### After (with gradient)
```
love^0.1, love^0.2, ... love^0.9
→ Total: infinite continuous choices
→ Plus: love^0.7 * hate^0.3 (weighted compositions)
```

**Expressiveness gain:** 3 choices → infinite control

---

## Test Categories Summary

| Category | Tests | Result | Confidence |
|----------|-------|--------|-----------|
| Range (0, 0.5, 1.0, >1, <0) | 5 | 5/5 ✓ | 80% |
| Precision (0.7 vs 0.8, chunking) | 3 | 3/3 ✓ | 82% |
| Boundaries (zero/one semantics) | 3 | 3/3 ✓ | 85% |
| Variable (A^B, nested, expressions) | 3 | 3/3 ✓ | 60% |
| Combinations (with @, *, →) | 5 | 5/5 ✓ | 84% |

---

## For Implementation

### Priority 1 (Ready Now)
- [ ] Core [0,1] range support
- [ ] Basic operator precedence (^ before @, *, →)
- [ ] Vocabulary examples with gradient

### Priority 2 (Design First)
- [ ] Extended range policy (>1.0, <0.0)
- [ ] Nested gradient behavior
- [ ] Weight normalization rules

### Priority 3 (Future)
- [ ] Expression exponents (A^(B*C))
- [ ] Cross-model consistency testing
- [ ] Advanced composition patterns

---

## Quick Truth Table

| Expression | Interpretation |
|-----------|-----------------|
| `X^0.0` | X is absent (0% intensity) |
| `X^0.25` | X is slight (25% intensity) |
| `X^0.5` | X is moderate (50% intensity) |
| `X^0.75` | X is strong (75% intensity) |
| `X^1.0` | X is maximum (100% intensity) |
| `A^0.7 * B^0.3` | A at 70% interfering with B at 30% |
| `A^0.6 @ ctx` | A at 60%, focused on ctx |

---

## Status

✓ **VIABLE**
✓ **TESTED** (19/19 tests passed)
✓ **READY FOR IMPLEMENTATION**

Pending: Design decisions on extended range, nesting, and weight normalization.

---

*Quick Reference for Limn ^ (Gradient) Operator*
*Generated: 2026-02-03*
*See full report: GRADIENT_TEST_REPORT_2026-02-03.md*
