# Vocabulary v1 vs v2: Relationship and Design Intent

**Purpose:** Clarify the relationship between the 40-word v1 vocabulary and the 500-word v2 vocabulary.

---

## Summary

| Aspect | v1 (40 words) | v2 (500 words) |
|--------|---------------|----------------|
| Purpose | Minimal proof-of-concept | Comprehensive coverage |
| Pattern | Strict CV (2 letters) | CV or CVC (2-3 letters) |
| Domains | 6 semantic groups | 9 semantic domains |
| Operators | 6 basic | 30 full set |
| Opacity | Opaque (requires bootstrap) | Partially transparent |
| Learnability | Easier | Harder |
| Expressiveness | Limited | High |

---

## Design Intent

### v1: The Learning Core

The 40-word vocabulary in bootstrap-v1.md serves as:

1. **Proof of concept** - Demonstrates Limn's core mechanics with minimal vocabulary
2. **Learning scaffold** - Easy to memorize, provides quick fluency
3. **Opaque test** - Validation showed v1 words require bootstrap to understand
4. **Sufficient for basics** - Can express many concepts through composition

**v1 is "real Limn"** - It's a complete, usable subset.

### v2: Full Expressiveness

The 500-word vocabulary in vocabulary-v2.md serves as:

1. **Complete coverage** - Addresses semantic gaps in v1
2. **Reduced ambiguity** - More specific words = less key-dependence
3. **Production use** - Suitable for complex programs and documents
4. **Extended operators** - Full logical and reference system

**v2 is also "real Limn"** - It's an expanded version for full expressiveness.

---

## Compatibility

### Grammar Rules Apply to Both

All grammar rules from v1 apply to v2:

- Word order independence (commutativity)
- Constraint intersection
- `nu` negation binding
- `|` scope boundaries
- Key-based disambiguation

### Mixing Vocabularies

You can mix v1 and v2 words in the same sentence:

```
# v1 word: lu (bright)
# v2 word: aqu (water)
# v2 word: flo (flowing)

lu aqu flo = bright water flowing = sparkling river, sunlit stream
```

### Mapping v1 to v2

Some v1 words have v2 equivalents or near-equivalents:

| v1 | v1 Meaning | v2 Equivalent | Notes |
|----|------------|---------------|-------|
| `lu` | bright | `bri` | Near-identical |
| `mu` | dark | `dim`, `nox` | v2 has more specific options |
| `vi` | alive | `lif` | Near-identical |
| `ko` | solid | `sol` | Near-identical |
| `su` | above | `abo` | Near-identical |
| `na` | below | `bel` | Near-identical |
| `ta` | beginning | `beg` | Near-identical |
| `fi` | ending | `end` | Near-identical |
| `du` | ongoing | `dur`, `cnt` | v2 has options |
| `sa` | same | `sam` | Near-identical |
| `no` | different | `dif` | Near-identical |
| `ma` | more | `mor` | Near-identical |
| `le` | less | `les` | Near-identical |
| `he` | hot | `hot` | Identical |
| `ku` | cold | `col` | Near-identical |
| `ga` | positive | `goo` | v2 uses "good" |
| `zo` | negative | `bad` | v2 uses "bad" |
| `si` | known | `kno` | Near-identical |
| `fu` | unknown | v2 lacks direct equiv | Use `nu kno` |
| `nu` | negation | `nu`, `not` | v2 has both |

---

## Which to Use?

### Use v1 When:

- Teaching Limn to beginners
- Creating examples for documentation
- Testing core language mechanics
- Need guaranteed opacity (for validation)
- Working with limited context

### Use v2 When:

- Expressing complex concepts
- Writing production code
- Need domain-specific vocabulary
- Reducing ambiguity for clarity
- Accessing extended operators

### Use Both When:

- Mixing is fine! v1 words work with v2 grammar
- Start with v1 core, add v2 specifics as needed
- Use v2 operators with v1 content words

---

## Learning Path

### Recommended Progression

1. **Week 1-2:** Learn v1 (40 words)
   - Master the 6 semantic groups
   - Practice interpretation and composition
   - Understand operators (`nu`, `ve`, `so`)

2. **Week 3-4:** Add v2 operators
   - Learn reference operators (`yo`, `an`, `th`, `a`)
   - Learn quantifiers (`al`, `ex`, `on`)
   - Practice scope (`|`, parentheses)

3. **Week 5+:** Expand to v2 vocabulary
   - Add domain-specific words as needed
   - Focus on domains relevant to your use case
   - Use v1 as "core vocabulary" supplemented by v2

---

## Technical Notes

### Phonological Differences

- **v1:** Strict CV pattern (consonant-vowel, 2 letters)
  - Examples: `lu`, `mu`, `vi`, `ko`

- **v2:** CV or CVC pattern (2-3 letters), with some CCV clusters
  - Examples: `sol`, `liq`, `bri`, `gro`

### Opacity Validation Results

From algorithm-encoding validation:

| Vocabulary | Without Bootstrap | Verdict |
|------------|-------------------|---------|
| v1 (opaque) | CANNOT INTERPRET | Working correctly |
| v2 (transparent) | Often interpretable | May leak information |

**Implication:** For security-sensitive or validation use cases, prefer v1 words.

---

## Conclusion

- **v1 and v2 are both valid Limn**
- **v1 is a learnable subset** suitable for beginners and testing
- **v2 is the full language** suitable for complex expression
- **They are compatible** and can be mixed freely
- **Grammar rules are identical** between versions

The distinction is vocabulary size and specificity, not fundamental language structure.

---

**END OF VOCABULARY RELATIONSHIP DOCUMENT**
