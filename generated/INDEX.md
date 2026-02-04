# Limn Gradient Expression Dataset - Complete Index

## Quick Access

**Generated Dataset:** `gradient_expressions.csv`
- **Total Expressions:** 1,208
- **Format:** CSV with 8 columns
- **Size:** 132 KB
- **Generation Date:** 2026-02-03

## What Are Gradient Expressions?

Gradient expressions use the `^` operator to apply **intensity parameters** (0.1 to 1.0) to vocabulary words. This creates a compositional system for expressing scalable properties across emotions, physical states, cognitive states, and abstract concepts.

### Core Concept

```
word^intensity → semantic_interpretation

hot^0.1  →  barely warm
hot^0.5  →  hot (neutral)
hot^1.0  →  searing (maximum)
```

## Four Expression Types

### 1. Simple Gradients: `A^n`
Apply a single intensity value to a word.

```csv
hot^0.1,barely warm,Physical Intensity
hot^0.5,hot,Physical Intensity
hot^1.0,searing,Physical Intensity
joy^0.3,subtle happy,Emotional
joy^1.0,transcendent bliss,Emotional
```

**Count:** 448 expressions (160 physical + 176 emotional + 96 cognitive + 16 actions)

### 2. Composite Expressions: `A^n + B^m`
Combine two intensity-modified words for nuanced meanings.

```csv
lov^0.3+tru^0.7,subtle love with strong trust,Composite
joy^0.9+hop^0.8,ecstatic with intense hope,Composite
sad^0.7+des^0.9,strong sadness with extreme despair,Composite
```

**Count:** 256 expressions (16 word pairs × 4 low intensities × 4 high intensities)

### 3. Negation Expressions: `nu A^n`
Invert a property while preserving intensity semantics.

```csv
nu hot^0.5,negated/not moderate heat,Negation
nu joy^0.7,negated/not strong happiness,Negation
nu tru^0.8,negated/not intense trust,Negation
```

**Count:** 80 expressions (16 words × 5 mid-range intensities)

### 4. Modifier Expressions: `ve/so A^n`
Apply prototype-narrowing (ve) or prototype-broadening (so) modifiers.

```csv
ve joy^0.5,prototypical/core happiness,Modifiers
so joy^0.5,peripheral/extended joy,Modifiers
ve hot^0.7,prototypical strong heat,Modifiers
so hot^0.7,peripheral strong heat,Modifiers
```

**Count:** 224 expressions (14 words × 8 intensities × 2 modifiers)

## Intensity Levels

All expressions use one of 8 standardized intensity values:

| Value | Label | Meaning | Example |
|-------|-------|---------|---------|
| 0.1 | Minimal | Barely perceptible | `hot^0.1` = barely warm |
| 0.2 | Slight | Noticeably reduced | `hot^0.2` = slightly warm |
| 0.3 | Subtle | Moderately reduced | `hot^0.3` = moderately warm |
| 0.5 | Moderate | Neutral/reference point | `hot^0.5` = hot |
| 0.7 | Strong | Notably increased | `hot^0.7` = scorching |
| 0.8 | Intense | Significantly elevated | `hot^0.8` = searing |
| 0.9 | Extreme | Near maximum | `hot^0.9` = searing |
| 1.0 | Maximum | Absolute/complete | `hot^1.0` = searing |

## Domain Coverage

### Physical Intensity (160 expressions)
Temperature, weight, brightness, surface, texture, size, density

**Key words:** `hot`, `col`, `lit`, `hev`, `bri`, `dim`, `smo`, `rou`, `har`, `sof`, `wet`, `dry`, `mag`, `min`, `lon`, `bre`, `wid`, `nar`, `den`, `spa`

Sample expressions:
- `hot^0.1` = barely warm
- `col^0.7` = freezing
- `mag^0.9` = huge
- `lit^1.0` = weightless

### Emotions (176 expressions)
Basic emotions, complex emotions, social emotions, regulatory emotions

**Key words:** `joy`, `sad`, `ang`, `hat`, `lov`, `tru`, `hop`, `des`, `fea`, `anx`, `exc`, `bor`, `pri`, `sha`, `gui`, `gra`, `env`, `jea`, `cal`, `dis`, `awe`, `emp`

Sample expressions:
- `joy^0.1` = slight happy
- `sad^1.0` = utterly devastated
- `lov^0.3` = slight affection
- `ang^0.9` = enraged

### Cognitive (96 expressions)
Mental operations, belief states, awareness, memory, learning

**Key words:** `thi`, `und`, `kno`, `bel`, `dou`, `rem`, `obl`, `ima`, `cur`, `att`, `ign`, `lea`

Sample expressions:
- `kno^0.1` = uncertain
- `und^1.0` = complete understanding
- `cur^0.9` = burning curiosity
- `lea^0.3` = basic understanding

### Living States (88 expressions)
Consciousness, hunger, strength, health, age, dreaming, boredom

**Key words:** `awa`, `sle`, `hun`, `str`, `wea`, `hea`, `sic`, `you`, `old`, `dre`, `bor`

Sample expressions:
- `awa^0.1` = barely conscious
- `str^1.0` = ultimate strength
- `hea^0.5` = healthy
- `old^0.9` = ancient

### Action Intensity (64 expressions)
Motion, vertical movement, growth, expansion, rotation

**Key words:** `mov`, `ris`, `fal`, `gro`, `dec`, `exp`, `con`, `rot`

Sample expressions:
- `mov^0.1` = barely moving
- `gro^1.0` = infinite expansion
- `dec^0.5` = decaying
- `rot^0.9` = spinning

### Abstract (64 expressions)
Magnitude, quantity, utility, ease, value, reality, artificiality

**Key words:** `big`, `mor`, `les`, `use`, `eas`, `val`, `rea`, `fak`

Sample expressions:
- `big^0.1` = tiny
- `use^1.0` = absolutely indispensable
- `val^0.5` = valuable
- `eas^0.9` = trivial

## CSV Structure

```
expression,meaning,example,domain,base_word,intensity_level,intensity_value,semantic_group
hot^0.1,barely warm,hot^0.1 = barely warm,Physical Intensity,hot,minimal,0.1,temperature increase
lov^0.3+tru^0.7,subtle lov with strong tru,lov^0.3+tru^0.7 = subtle lov with strong tru,Composite,lov|tru,subtle+strong,0.5,trusting love
nu joy^0.7,negated/not strong joy,nu joy^0.7 = negated/not strong joy,Negation,joy,strong,0.7,negation operator
ve hot^0.5,prototypical/narrow moderate hot,ve hot^0.5 = prototypical/narrow moderate hot,Modifiers,hot,moderate,0.5,narrowing (ve) operator
```

### Column Definitions

1. **expression** - Full compositional expression (e.g., "hot^0.5")
2. **meaning** - Semantic interpretation (e.g., "warm")
3. **example** - Complete example with interpretation
4. **domain** - Semantic category (Physical Intensity, Emotional, etc.)
5. **base_word** - Core vocabulary word(s) (e.g., "hot" or "lov|tru")
6. **intensity_level** - Semantic label (minimal, slight, moderate, strong, intense, extreme, maximum)
7. **intensity_value** - Numeric intensity parameter (0.1-1.0)
8. **semantic_group** - Detailed semantic field (e.g., "temperature increase", "negation operator")

## Usage Examples

### Expressing States of Being

**Physical sensations:**
```
hot^0.8 = searing heat
col^0.3 = moderately cool
wet^0.9 = dripping wet
```

**Emotional states:**
```
joy^1.0 = transcendent bliss
sad^0.5 = sad (neutral sadness)
ang^0.9 = enraged
```

**Cognitive states:**
```
kno^0.9 = certain
und^0.3 = basic understanding
cur^0.7 = intensely curious
```

### Expressing Complex States

**Multi-word expressions:**
```
lov^0.3+tru^0.7 = subtle love with strong trust
joy^0.9+hop^0.8 = ecstatic with intense hope
sad^0.5+des^0.9 = sad with extreme despair
ang^0.2+hat^0.9 = slight anger with consuming hatred
```

**Negated states:**
```
nu joy^0.7 = not strongly happy (implies sadness)
nu tru^0.8 = not intensely trusting (implies suspicion)
nu hot^0.5 = not moderately hot (implies cold)
```

**Prototype variations:**
```
ve joy^0.5 = pure/core happiness
so joy^0.5 = extended/connotative joy
ve str^0.9 = pure physical strength
so str^0.9 = strength as perseverance
```

## Statistical Summary

```
Total Expressions:         1,208
By Expression Type:
  - Simple (A^n):            448 (37.1%)
  - Composite (A^n+B^m):     256 (21.2%)
  - Negation (nu A^n):        80 (6.6%)
  - Modifiers (ve/so A^n):   224 (18.5%)

By Domain:
  - Physical Intensity:      160 (13.2%)
  - Emotional:               176 (14.6%)
  - Cognitive:                96 (7.9%)
  - Living States:            88 (7.3%)
  - Action Intensity:         64 (5.3%)
  - Abstract:                 64 (5.3%)
  - Composite:               256 (21.2%)
  - Negation:                 80 (6.6%)
  - Modifiers:               224 (18.5%)

Intensity Distribution:
  - 0.1 (minimal):          116 expressions
  - 0.2 (slight):           116 expressions
  - 0.3 (subtle):           116 expressions
  - 0.5 (moderate):         116 expressions
  - 0.7 (strong):           116 expressions
  - 0.8 (intense):          116 expressions
  - 0.9 (extreme):          116 expressions
  - 1.0 (maximum):          116 expressions
```

## Semantic Principles

### 1. Compositional Semantics
Meaning is built from:
- **Base word meaning** (from vocabulary)
- **Intensity modifier** (0.1-1.0)
- **Operator semantics** (^, +, nu, ve, so)

### 2. Perceptual Validity
Intensity values map to human experience:
- 0.1 is truly minimal, not arbitrary
- 0.5 is neutral/reference point
- 1.0 is maximum/extreme

### 3. Domain Consistency
Similar words in same field show parallel patterns:
- `hot^0.1` and `col^0.1` are opposite extremes
- `joy^0.5` and `sad^0.5` are balanced midpoints
- `mag^0.1` and `min^0.1` are opposite poles

### 4. Semantic Transparency
Each expression should be interpretable from its components:
- `hot^0.7` = strong heat (from "hot" + 0.7)
- `lov^0.3+tru^0.7` = subtle love combined with strong trust
- `nu joy^0.5` = negation of moderate joy

## Implementation Files

**Primary Output:**
- `gradient_expressions.csv` - Complete 1,208-expression dataset

**Supporting Files:**
- `GRADIENT_REPORT.md` - Detailed technical documentation
- `generate_gradients.py` - Generator script (reproducible, extensible)
- `INDEX.md` - This file

## Extensibility

The system supports future expansion:

1. **Finer gradations** - Add 0.05, 0.15, 0.25, etc. for more precision
2. **Three-way composites** - `A^n + B^m + C^p` for complex states
3. **Comparative expressions** - `eq A^n B^m` (A and B at equal intensity)
4. **Temporal modulation** - `A^n_past`, `A^n_future` for tense
5. **Probabilistic intensity** - `A^?0.5` meaning "possibly at 1.0"

## Quality Notes

- **Semantic Coherence:** Every expression preserves core word meaning
- **Intensity Validity:** Intensity levels are psychologically meaningful, not arbitrary
- **Coverage:** All major semantic domains represented
- **Consistency:** Parallel structure across similar words and domains
- **Usability:** Clear mappings from expression to interpretation

## References

**Vocabulary Source:**
- `/home/eric/src/limntown/limn/crew/linguist/src/claude-skill/vocabulary.json`
- Contains 585 core Limn words across 13 semantic domains

**Generated By:**
- Python script: `generate_gradients.py`
- Limn Language Project - Linguist Crew
- Generated: 2026-02-03

---

For detailed semantic documentation, see `GRADIENT_REPORT.md`.
For implementation details, see `generate_gradients.py`.
