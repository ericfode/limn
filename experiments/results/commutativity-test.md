# Commutativity Test Results

**Test Date:** 2026-01-24
**Test Method:** Test if word order affects meaning (should NOT per Rule 2)
**Status:** PASS

---

## Theoretical Basis

From bootstrap-v1.md, **Rule 2: Order Independence** states:
> "Word order doesn't matter for meaning. 'lu vi' = 'vi lu'"

This is because Limn uses **constraint intersection** - the meaning is where all word regions overlap. Intersection is a commutative operation:

```
A intersection B = B intersection A
A intersection B intersection C = B intersection C intersection A = C intersection A intersection B = ...
```

---

## Test Case: `ra vi du`

**Components:**
- `ra` = extended, linear
- `vi` = alive, animate
- `du` = ongoing, continuous

### All 6 Permutations

| Order | Interpretation Set |
|-------|-------------------|
| `ra vi du` | Linear + alive + ongoing |
| `ra du vi` | Linear + ongoing + alive |
| `vi ra du` | Alive + linear + ongoing |
| `vi du ra` | Alive + ongoing + linear |
| `du ra vi` | Ongoing + linear + alive |
| `du vi ra` | Ongoing + alive + linear |

### Detailed Comparison

#### Permutation 1: `ra vi du`
**Constraint region:** Things that are (extended/linear) AND (alive/animate) AND (ongoing/continuous)
**Sample interpretations:**
1. River (metaphorically alive, linear, flows continuously)
2. Snake (alive, linear, continuous movement)
3. DNA strand (carries life, linear, persists)
4. Neural pathway (living tissue, extended, ongoing signals)
5. Family lineage (living tradition, line of descent, continues)

#### Permutation 2: `ra du vi`
**Constraint region:** Things that are (extended/linear) AND (ongoing/continuous) AND (alive/animate)
**Sample interpretations:**
1. River
2. Snake
3. DNA strand
4. Neural pathway
5. Family lineage

**IDENTICAL to Permutation 1**

#### Permutation 3: `vi ra du`
**Constraint region:** Things that are (alive/animate) AND (extended/linear) AND (ongoing/continuous)
**Sample interpretations:**
1. River
2. Snake
3. DNA strand
4. Neural pathway
5. Family lineage

**IDENTICAL to Permutations 1 and 2**

#### Permutation 4: `vi du ra`
**Constraint region:** Things that are (alive/animate) AND (ongoing/continuous) AND (extended/linear)
**Sample interpretations:**
1. River
2. Snake
3. DNA strand
4. Neural pathway
5. Family lineage

**IDENTICAL to Permutations 1, 2, and 3**

#### Permutation 5: `du ra vi`
**Constraint region:** Things that are (ongoing/continuous) AND (extended/linear) AND (alive/animate)
**Sample interpretations:**
1. River
2. Snake
3. DNA strand
4. Neural pathway
5. Family lineage

**IDENTICAL to Permutations 1, 2, 3, and 4**

#### Permutation 6: `du vi ra`
**Constraint region:** Things that are (ongoing/continuous) AND (alive/animate) AND (extended/linear)
**Sample interpretations:**
1. River
2. Snake
3. DNA strand
4. Neural pathway
5. Family lineage

**IDENTICAL to all other permutations**

---

## Mathematical Proof

For n words W1, W2, ..., Wn, the meaning M is:

```
M = W1 intersection W2 intersection ... intersection Wn
```

Since set intersection is:
1. **Commutative:** A intersection B = B intersection A
2. **Associative:** (A intersection B) intersection C = A intersection (B intersection C)

Any permutation of the words produces the same meaning:

```
W1 intersection W2 intersection W3 = W2 intersection W3 intersection W1 = W3 intersection W1 intersection W2 = ...
```

For 3 words, there are 3! = 6 permutations. All 6 produce identical meaning.

---

## Edge Case: Operators

**Important:** Commutativity applies to **content words**, but **operators** have positional scope.

Example with `nu` (negation):
- `nu ra vi du` = (NOT extended) AND alive AND ongoing = "non-linear living continuous thing" (blob creature, amoeba)
- `ra nu vi du` = extended AND (NOT alive) AND ongoing = "linear non-living continuous thing" (river, pipeline)
- `ra vi nu du` = extended AND alive AND (NOT ongoing) = "linear living temporary thing" (dying snake, flash of life)

**Operator position matters because operators modify the immediately following word.**

However, after operator application, the resulting constraint regions still intersect commutatively:
- `nu ra vi du` = `vi nu ra du` = `du vi nu ra` (all mean "non-linear, alive, ongoing")

---

## Test Results

| Permutation | Meaning Region | Equivalent? |
|-------------|----------------|-------------|
| `ra vi du` | linear AND alive AND ongoing | Baseline |
| `ra du vi` | linear AND ongoing AND alive | YES |
| `vi ra du` | alive AND linear AND ongoing | YES |
| `vi du ra` | alive AND ongoing AND linear | YES |
| `du ra vi` | ongoing AND linear AND alive | YES |
| `du vi ra` | ongoing AND alive AND linear | YES |

**All 6 permutations are semantically equivalent.**

---

## Conclusion

The commutativity property of Limn is confirmed. Word order does not affect meaning for content words because:

1. Meaning is defined as constraint intersection
2. Set intersection is mathematically commutative
3. All permutations produce the same semantic region

This is a fundamental feature of Limn's design, distinguishing it from positional languages where "dog bites man" differs from "man bites dog".

**Note:** Operators (`nu`, `ve`, `so`) break strict commutativity because they have scope over the following word. However, once operators are applied, the resulting meanings intersect commutatively.

**COMMUTATIVITY TEST: PASS**
