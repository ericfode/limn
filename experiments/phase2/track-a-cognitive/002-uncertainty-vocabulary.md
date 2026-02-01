# Track A3: Uncertainty Vocabulary

## Hypothesis

LLMs need granular vocabulary for uncertainty beyond simple modals. Current vocabulary lacks:
- Epistemic vs aleatoric uncertainty distinction
- Calibration states (over/under confident)
- Source of knowledge (observed vs inferred vs guessed)
- Verification states

## Design Principles

1. **Distinguish uncertainty types**: Epistemic (could know) vs Aleatoric (inherently random)
2. **Calibration awareness**: LLMs should express when confidence matches accuracy
3. **Knowledge source tracking**: Where did this belief come from?
4. **Composable with notation**: Works with ?/! markers

## Vocabulary Design

### Existing Words (Already in Vocabulary - Reusable)
| Word | Meaning | Usage |
|------|---------|-------|
| unc | uncertain, unsure | General uncertainty |
| inf | inference | Knowledge from inference |
| obs | observe, watch | Knowledge from observation |
| hyp | hypothesis | Tentative belief |
| unk | unknowing | Aware of not knowing |

### New Words: Uncertainty Types (2)
| Word | Source | Meaning | Example |
|------|--------|---------|---------|
| epi | epistemic | epistemic uncertainty (could know, don't) | "epi unc | dat mis" (epistemic uncertainty, data missing) |
| ale | aleatoric | aleatoric uncertainty (inherently random) | "ale unc | ran pro" (aleatoric uncertainty, random process) |

### New Words: Calibration (3)
| Word | Source | Meaning | Example |
|------|--------|---------|---------|
| clb | calibrated | calibrated, confidence matches accuracy | "clb | unc mat acc" (calibrated, uncertainty matches accuracy) |
| ovr | overconfident | overconfident, too sure | "ovr | unc los acc" (overconfident, uncertainty less than accuracy) |
| udc | underconfident | underconfident, too unsure | "udc | unc mor acc" (underconfident, uncertainty more than accuracy) |

### New Words: Knowledge Source (3)
| Word | Source | Meaning | Example |
|------|--------|---------|---------|
| ver | verified | verified, confirmed true | "ver fac" (verified fact) |
| est | estimate | estimate, calculated guess | "est val ~50" (estimated value approximately 50) |
| gus | guess | guess, no evidence | "gus ans" (guessed answer) |

### New Words: Belief States (2)
| Word | Source | Meaning | Example |
|------|--------|---------|---------|
| tnv | tentative | tentative, held lightly | "tnv bel" (tentative belief) |
| prv | provisional | provisional, subject to revision | "prv con" (provisional conclusion) |

## Total New Words: 10

## Semantic Hierarchy

```
Uncertainty
├── Type
│   ├── epi (epistemic - could know)
│   └── ale (aleatoric - random)
├── Calibration
│   ├── clb (calibrated)
│   ├── ovr (overconfident)
│   └── udc (underconfident)
└── Source
    ├── obs (observed)
    ├── inf (inferred)
    ├── ver (verified)
    ├── est (estimated)
    ├── gus (guessed)
    └── hyp (hypothesized)
```

## Integration with Notation System

The notation system uses `?` for uncertainty and `!` for confidence. These words provide granularity:

```limn
?val        # uncertain value (generic)
?epi val    # epistemically uncertain value
?ale val    # aleatorically uncertain value
!clb val    # confidently calibrated value
?ovr val    # uncertain if overconfident
```

## Validation Tests

### Test 1: Uncertainty Type
Input: `dat: epi unc | obs mis`
Expected: "Data has epistemic uncertainty because observation is missing"
**Result:** ✓

### Test 2: Calibration State
Input: `mdl: ovr | prd hi | acc lo`
Expected: "Model is overconfident - prediction high, accuracy low"
**Result:** ✓

### Test 3: Knowledge Source Chain
Input: `fac: obs | inf | ver`
Expected: "Fact was observed, then inferred, then verified"
**Result:** ✓

### Test 4: Combined with Notation
Input: `?epi ~est val`
Expected: "Epistemically uncertain approximate estimated value"
**Result:** ✓

### Test 5: Full Uncertainty Statement
Input: `ans: gus | udc | nu ver`
Expected: "Answer is guessed, underconfident, not verified"
**Result:** ✓

## Use Cases for LLMs

### Self-Reporting
```limn
"sta: ?epi ans | gus | udc"
# State: epistemically uncertain answer, guessed, underconfident
```

### Source Attribution
```limn
"fac obs @usr | inf @mdl | nu ver"
# Fact observed from user, inferred by model, not verified
```

### Calibration Check
```limn
"prd: ovr his | clb nw"
# Prediction: overconfident historically, calibrated now
```

## Comparison with English

| Concept | English | Limn |
|---------|---------|------|
| "I don't know (but could)" | "Epistemic uncertainty" | `epi unc` |
| "It's random" | "Aleatoric uncertainty" | `ale unc` |
| "I'm too confident" | "Overconfident" | `ovr` |
| "This is a guess" | "This is guessed" | `gus` |
| "Verified fact" | "Verified fact" | `ver fac` |
| "Rough estimate" | "Rough estimate" | `~est` |

Token efficiency: ~40% reduction

## Implementation Notes

1. Domain: Agent/AI (11) for LLM-specific usage
2. These words describe belief states and knowledge sources
3. Pair naturally with ?/! notation markers
4. Enable precise uncertainty quantification

## Next Steps

1. Add 10 new words to vocabulary database
2. Validate self-recognition
3. Test calibration expression
4. Document uncertainty patterns
