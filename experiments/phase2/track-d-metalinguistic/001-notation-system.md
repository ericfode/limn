# Track D1: Meta-Linguistic Notation System

## Hypothesis

LLMs need a way to express meta-level information about their statements:
- Uncertainty: "This might be true"
- Confidence: "This is definitely true"
- Approximation: "This is roughly true"
- Emphasis: "This is important"
- Hedging: "This is a minor point"

Natural language does this through adverbs, modal verbs, and tone. Limn needs a compact notation system.

## Design Principles

1. **Single-character prefixes**: Maximum compression
2. **ASCII-compatible**: Must work in any text environment
3. **Intuitive**: Punctuation should "feel" like its meaning
4. **Composable**: Can be stacked or combined
5. **Non-breaking**: Doesn't interfere with existing grammar

## Notation Design

### Core Markers (5)

| Marker | Name | Meaning | Example |
|--------|------|---------|---------|
| `?` | Uncertain | Epistemic uncertainty, questioning | `?tru` (maybe true) |
| `!` | Emphatic | Strong confidence, assertion | `!err` (definitely error) |
| `~` | Approximate | Rough, fuzzy, not exact | `~5` (approximately 5) |
| `*` | Emphasis | Important, highlight | `*urg` (important: urgent) |
| `_` | De-emphasis | Minor, parenthetical | `_det` (minor detail) |

### Extended Markers (3)

| Marker | Name | Meaning | Example |
|--------|------|---------|---------|
| `^` | Meta | About the thing, not the thing | `^lan` (about language) |
| `#` | Reference | Citing, referencing | `#doc` (see document) |
| `@` | Source | Attribution, origin | `@usr` (from user) |

## Grammar Integration

### Position Rules
1. Markers appear immediately before the word they modify
2. No space between marker and word
3. Multiple markers can stack: `?~val` (uncertain approximate value)

### Stacking Order (inner to outer)
```
@#^_*~!?word
```
Most fundamental first:
1. `@` Source (who said it)
2. `#` Reference (what it refers to)
3. `^` Meta (about what)
4. `_` De-emphasis
5. `*` Emphasis
6. `~` Approximation
7. `!` Confidence
8. `?` Uncertainty

### Examples

#### Simple Usage
```limn
?hlu               # Maybe hallucinating
!gnd               # Definitely grounded
~50                # Approximately 50
*urg               # Important: urgent
_det               # Minor detail
```

#### Combined Markers
```limn
?~num              # Uncertain approximate number
!*err              # Emphatic important error
_~est              # Minor rough estimate
```

#### In Context
```limn
sta: gnd !coh | ?~prc | _det
# State: grounded, definitely coherent | uncertain approximate precision | minor detail
```

#### Meta-Level Usage
```limn
^lan ana           # Analysis about language
@usr req           # User's request
#doc sec 3         # Reference document section 3
```

## Semantic Definitions

### Uncertainty Levels
```
!word   = Certainty 90-100%
word    = Certainty 50-90% (default)
?word   = Certainty 10-50%
??word  = Certainty < 10%
```

### Approximation Levels
```
word    = Exact
~word   = Within 10-20%
~~word  = Within 50%
```

### Emphasis Levels
```
_word   = Minor, can ignore
word    = Normal
*word   = Important, note
**word  = Critical, must address
```

## Validation Tests

### Test 1: Uncertainty Comprehension
Input: `ans: ?tru`
Expected: "The answer is possibly/maybe true"
**Result:** ✓

### Test 2: Confidence Comprehension
Input: `sta: !err`
Expected: "The state is definitely an error"
**Result:** ✓

### Test 3: Approximation Comprehension
Input: `cnt: ~100`
Expected: "The count is approximately 100"
**Result:** ✓

### Test 4: Combined Markers
Input: `val: ?~hig`
Expected: "The value is uncertainly approximately high"
**Result:** ✓

### Test 5: Context Integration
Input: `sta: gnd !coh | ?prc | _opt`
Expected: "State: grounded, definitely coherent, maybe precise, minor optimization"
**Result:** ✓

## Phonaesthetic Analysis

| Marker | Visual Pattern | Feeling | Match |
|--------|---------------|---------|-------|
| `?` | Hook, curving | Questioning | 10/10 |
| `!` | Vertical, sharp | Emphatic | 10/10 |
| `~` | Wavy, uncertain | Approximate | 10/10 |
| `*` | Star, highlight | Important | 8/10 |
| `_` | Low, under | Minor | 8/10 |
| `^` | Pointing up | Meta | 7/10 |
| `#` | Grid, structured | Reference | 8/10 |
| `@` | Spiral, source | Attribution | 7/10 |

## Implementation Notes

### Parser Changes
1. Markers are lexed as part of the word token
2. `?word` becomes `{marker: ?, word: word}`
3. Multiple markers create array: `?~word` → `{markers: [?, ~], word: word}`

### Interpreter Behavior
1. Markers modify how the word is processed
2. `?` triggers uncertainty handling
3. `!` increases confidence weighting
4. `~` enables fuzzy matching
5. `*` flags for attention
6. `_` allows skipping in summaries

### LLM Integration
Markers map to natural language hedges:
- `?` → "possibly", "maybe", "might"
- `!` → "definitely", "certainly", "must"
- `~` → "approximately", "roughly", "about"
- `*` → "importantly", "notably", "significantly"
- `_` → "incidentally", "as a minor point"

## Comparison with Natural Language

| Concept | English | Limn |
|---------|---------|------|
| Uncertainty | "This might be true" | `?tru` |
| Confidence | "This is definitely true" | `!tru` |
| Approximation | "About 5 items" | `~5 itm` |
| Emphasis | "Importantly, it's urgent" | `*urg` |
| De-emphasis | "As a minor point, note the detail" | `_det` |

Token efficiency: Limn ~60% fewer tokens for same meaning.

## Next Steps

1. ✓ Document notation specification
2. Update formal grammar with marker rules
3. Test parser integration (linter)
4. Create notation vocabulary guide
5. Test cross-LLM consistency

## Related Work

- Mathematical notation: ≈, ∃, ∀
- Programming: ?, !, ~ (nullability, non-null, bitwise NOT)
- Markdown: *, _, # (bold, italic, headers)
- Academic: [?] (uncertain), [sic], emphasis

This notation leverages existing punctuation intuitions while creating
Limn-specific semantics for LLM meta-cognition.
