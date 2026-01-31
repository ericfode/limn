# Limn Unit Test Summary

**Test Date:** 2026-01-24
**Tester:** Claude Opus 4.5
**Vocabulary Version:** v1 (40 words)
**Bootstrap Version:** v1

---

## Test Results Overview

| Test | Status | Description |
|------|--------|-------------|
| Bootstrap Test | **PASS** | Successfully learned Limn from bootstrap-v1.md only |
| Ambiguity Test | **PASS** | Generated 10+ interpretations, key collapse ratio 3.33x avg |
| Commutativity Test | **PASS** | All 6 permutations of `ra vi du` are semantically equivalent |
| Inversion Test | **PASS** | Adding `mu` to `lu ra ga` dramatically shifts meaning |
| Scalability Test | **PASS** | Long sentences gracefully degrade from concrete to systemic meanings |
| Grouping Test | **PASS** | `\|` operator correctly delimits scope, enables multi-entity sentences |
| Cross-Key Test | **PASS** | Same sentence produces radically different interpretations under different keys |
| Negation Scope Test | **PASS** | `nu` position significantly affects meaning; interacts correctly with `\|` |

---

## Detailed Results

### 1. Bootstrap Test: PASS

**File:** `experiments/results/bootstrap-test.md`

**Criteria met:**
- [x] Learned 40-word vocabulary from bootstrap document
- [x] Understood 4 grammar rules (intersection, order independence, operator scope, grouping)
- [x] Generated 10+ interpretations for novel sentences
- [x] Successfully collapsed interpretations with context key
- [x] Demonstrated negation operator usage
- [x] Created maximally ambiguous sentences

**Key finding:** The bootstrap document is sufficient to teach Limn to a fresh LLM through in-context learning.

---

### 2. Ambiguity Test: PASS

**File:** `experiments/results/ambiguity-test.md`

**Test sentences:**
- `ra vi du` - 12 interpretations without key, 3 with key "technology"
- `ko su fi` - 11 interpretations without key, 3 with key "technology"
- `mu na wo` - 13 interpretations without key, 4 with key "technology"
- `ve lu ta` - 11 interpretations without key, 4 with key "technology"
- `mi he vi` - 12 interpretations without key, 4 with key "technology"

**Key Collapse Ratios:**
| Sentence | Ratio |
|----------|-------|
| `ra vi du` | 4.00x |
| `ko su fi` | 3.67x |
| `mu na wo` | 3.25x |
| `ve lu ta` | 2.75x |
| `mi he vi` | 3.00x |
| **Average** | **3.33x** |

**Key finding:** Context keys reduce ambiguity by an average factor of 3.33x, confirming the key-based disambiguation mechanism.

---

### 3. Commutativity Test: PASS

**File:** `experiments/results/commutativity-test.md`

**Tested permutations of `ra vi du`:**
- `ra vi du` = `ra du vi` = `vi ra du` = `vi du ra` = `du ra vi` = `du vi ra`

**All 6 permutations produce identical meaning regions.**

**Mathematical basis:** Set intersection is commutative and associative.

**Edge case documented:** Operators (`nu`, `ve`, `so`) have positional scope and break strict word-level commutativity, but operator-modified regions still intersect commutatively.

**Key finding:** Limn is order-independent for content words, distinguishing it from positional languages.

---

### 4. Inversion Test: PASS

**File:** `experiments/results/inversion-test.md`

**Base sentence:** `lu ra ga` (bright + linear + positive)
- Interpretations: sunny path, hopeful journey, golden road, clear conscience
- Valence: Strongly positive (+0.9)

**Modified sentence:** `lu ra ga mu` (bright + linear + positive + dark)
- Interpretations: twilight path, tunnel with light, fading hope, moral ambiguity
- Valence: Slightly positive/neutral (+0.2)

**Metrics:**
- Interpretation overlap: 0 (no direct matches)
- Average semantic distance: 7/10
- Valence shift: -0.7

**Key finding:** Adding `mu` creates semantic tension resolved through intersection, producing ambivalent rather than negated meanings. This demonstrates the inversion property - certain words dramatically alter semantic character without full negation.

---

### 5. Scalability Test: PASS

**File:** `experiments/results/scalability-test.md`

**Tested sentence lengths:** 3, 5, 6, 8, 10, 12 words

**Key findings:**
- 3 words: 10-15 interpretations (highly ambiguous)
- 5 words: 7-10 interpretations (moderately ambiguous)
- 8 words: 3-5 interpretations (narrow)
- 12 words: 1-3 interpretations (systemic/strained)
- Contradictory constraints (su+na, ta+fi) force systemic interpretations
- ~8 words is the practical limit for concrete referent interpretation
- Beyond 8 words, sentences describe systems/processes rather than objects

---

### 6. Grouping Test: PASS

**File:** `experiments/results/grouping-test.md`

**Key findings:**
- Without `|`: All words intersect into single meaning region
- With `|`: Creates separate groups (multiple entities, states, or contrasts)
- `nu |` at sentence start creates near-universal region (complement of narrow intersection)
- `yo`/`an` + `|` anchors groups to distinct referents
- Multi-part with `bi` creates structured relationships
- Paradox (`A B`) vs. Contrast (`A | B`) is a fundamental distinction

---

### 7. Cross-Key Test: PASS

**File:** `experiments/results/cross-key-test.md`

**Key findings:**
- Same sentence (`ra vi du`) produced completely different interpretations under 8 different keys
- Topic keys: ~3-5x collapse ratio
- Narrative keys: ~10x+ collapse (often to single interpretation)
- Keys that seem to contradict sentences find creative resolutions
- Structural meaning preserved; referential meaning shifts with key
- Key functions as dimensional selector in meaning space

---

### 8. Negation Scope Test: PASS

**File:** `experiments/results/negation-scope-test.md`

**Key findings:**
- `nu` position dramatically changes meaning (`nu lu vi ga` ≠ `lu nu vi ga` ≠ `lu vi nu ga`)
- Double negation cancels: `nu nu X` = X
- `nu` interacts correctly with grouping: `nu X | Y` vs `nu | X Y`
- Operator order matters: `nu ve X` ≠ `ve nu X`
- Sentence-level negation (`nu |`) creates near-universal regions
- Operators are the exception to commutativity

---

## Overall Assessment

**All 8 tests passed.**

The Limn language specification v1 demonstrates:

1. **Learnability:** The bootstrap document successfully transfers knowledge
2. **Controlled ambiguity:** High ambiguity without keys, controlled collapse with keys (3-10x ratio)
3. **Mathematical coherence:** Commutativity holds for constraint intersection
4. **Expressive flexibility:** Inversion allows dramatic meaning shifts
5. **Scalability:** Graceful degradation from 3 to 12+ words
6. **Structural expressiveness:** Grouping enables multi-entity and relational sentences
7. **Key power:** Same sentence radically reinterprets under different keys
8. **Precise negation:** Position-sensitive negation with proper operator scoping

**Phase 3 Validation Status:** SUBSTANTIALLY COMPLETE

All core properties specified in the language design have been empirically validated:
- Commutativity of content words
- Key-based disambiguation
- Operator scoping rules
- Scalability behavior
- Grouping semantics

---

## Recommendations for Future Tests

1. ~~**Scalability test:** Test with 5+ word sentences~~ **COMPLETED**
2. ~~**Grouping test:** Test "|" operator for scope boundaries~~ **COMPLETED**
3. ~~**Cross-key test:** Same sentence with multiple different keys~~ **COMPLETED**
4. **Composability test:** Test ability to express arbitrary concepts
5. ~~**Negation scope test:** Test `nu` with different positions~~ **COMPLETED**
6. **Human comprehension test:** Test if humans can learn from bootstrap
7. **Information density test:** Measure bits per word with and without keys
8. **Bidirectional programming test:** Test constraint satisfaction in both directions

---

## Files Generated

```
experiments/results/
  bootstrap-test.md       (Bootstrap learning verification)
  ambiguity-test.md       (Key collapse ratio measurements)
  commutativity-test.md   (Order independence verification)
  inversion-test.md       (Meaning inversion verification)
  scalability-test.md     (Long sentence interpretation)
  grouping-test.md        (Scope boundary operator testing)
  cross-key-test.md       (Multiple keys on same sentence)
  negation-scope-test.md  (Negation operator positioning)
  SUMMARY.md              (This file)
```

---

**END OF TEST SUMMARY**
