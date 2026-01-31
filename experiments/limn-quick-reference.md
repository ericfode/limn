# Limn Quick Reference Card

*For when you need to look something up fast*

---

## Core Concept

**Words are constraints. Adjacent words intersect. Meaning = overlap.**

```limn
sol aqu = solid ∩ water = ice
```

---

## Operators

### Unary Modifiers (bind right)

| Op | Name | Effect | Example |
|----|------|--------|---------|
| `nu` | not | complement | `nu hot` = cold |
| `ve` | very | intensify to core | `ve hot` = scorching |
| `so` | somewhat | weaken to periphery | `so hot` = warm |

**Stacking:**
- `nu ve X` = NOT(very X) = moderate or opposite
- `ve nu X` = very(NOT X) = strong opposite

### Reference

| Op | Meaning | Use |
|----|---------|-----|
| `yo` | this/here/proximal | `yo wor` = this word |
| `an` | that/there/distal | `an wor` = that word |
| `sa` | same/equals | `x sa 5` = x equals 5 |

### Quantifiers

| Op | Meaning | Example |
|----|---------|---------|
| `al` | all/universal | `al hum` = all humans |
| `ex` | some/existential | `ex hum` = some humans |
| `on` | exactly one | `on tru` = one truth |

### Comparators

| Op | Meaning | Math |
|----|---------|------|
| `mi` | less than | `x mi 5` = x < 5 |
| `ma` | greater than | `x ma 5` = x > 5 |
| `eq` | equal to | `x eq 5` = x = 5 |

### Speech Acts

| Op | Meaning | Example |
|----|---------|---------|
| `te` | question | `te lov` = is there love? |
| `we` | imperative | `we mov` = move! |

### Tones (modify pragmatics)

| Op | Meaning | Effect |
|----|---------|--------|
| `frm` | formal | professional register |
| `cas` | casual | informal register |
| `iro` | ironic | sarcastic tone |
| `sin` | sincere | earnest tone |
| `urj` | urgent | emergency feel |
| `hes` | hesitant | tentative feel |

---

## Structural Operators

### Grouping

| Symbol | Name | Effect |
|--------|------|--------|
| `\|` | scope boundary | separates entities/topics |
| `( )` | parentheses | groups for precedence |
| `→` | sequence/causality | temporal/causal order |

### Examples

```limn
A B           # intersection (simultaneous)
A | B         # separation (coexisting)
A → B         # sequence (causal)
(A B) | C     # grouped intersection, separated from C
```

---

## Precedence (high to low)

1. Unary ops (`nu`, `ve`, `so`, `te`, `we`) - right to left
2. Arithmetic (`*`, `/`)
3. Arithmetic (`+`, `-`)
4. Comparison (`mi`, `ma`, `eq`)
5. Intersection (space) - commutative
5.5. Sequence (`→`) - left to right
6. Scope (`|`) - left to right

---

## Grammar Rules

### Rule 1: Intersection

Adjacent words combine by AND.
```limn
hot aqu = hot AND water
```

### Rule 2: Commutativity

Word order doesn't matter (except operators).
```limn
A B = B A
```

### Rule 3: Operator Binding

Operators bind to immediately following term.
```limn
nu hot col = (NOT hot) AND cold
```

### Rule 4: Sequence Breaks Commutativity

```limn
A B = B A         # true
A → B ≠ B → A     # false
```

### Rule 5: Contradiction = Boundary

```limn
A nu A = boundary(A)    # liminal zone
```

---

## Common Patterns

### States
```limn
hot aqu          # hot water
col aqu sol      # cold solid water = ice
```

### Transformations
```limn
sol → liq        # solid to liquid
A → B → C        # chain
```

### Cycles
```limn
X → Y → X        # journey pattern
```

### Negation
```limn
wri nu sen       # written, not-sent
```

### Questions
```limn
te X             # is there X?
te X Y           # is X related to Y?
```

### Logic
```limn
al X | ex Y | X rel Y    # ∀x ∃y : x relates to y
```

---

## Constraint Programming

### Variables
```limn
whe x            # declare variable x
```

### Constraints
```limn
x joi y sa z     # x + y = z
x ma 0           # x > 0
x mi 10          # x < 10
```

---

## Vocabulary Shortcuts

### Physical
`sol` solid, `liq` liquid, `gas` gas, `hot` hot, `col` cold

### Elements
`aqu` water, `pyr` fire, `ter` earth, `aer` air, `lux` light, `nox` dark

### Size
`big` big, `sma` small, `lon` long, `sho` short, `nar` narrow/thin

### Time
`now` now, `pas` past, `fut` future, `beg` begin, `end` end, `old` old, `you` young

### Actions
`mov` move, `gro` grow, `flo` flow, `ris` rise, `fal` fall

### Mind
`thi` think, `kno` know, `see` see, `fee` feel

### Emotion
`joy` joy, `sad` sad, `lov` love, `fea` fear, `hop` hope, `ang` anger

### Social
`sel` self, `oth` other, `sha` shame, `shr` share

---

## Remember

- **Words = constraints, not labels**
- **Intersection, not sequence** (unless `→`)
- **Ambiguity is a feature** (keys disambiguate)
- **Agent-less by design** (states, not actions)
- **Paradox = boundary** (liminality is valid)

---

*Quick ref by Kira • 2026-01-31*
