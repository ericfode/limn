# Limn Complex Compositional Expressions

## Overview

Generated **2,250 complex compositional expressions** combining multiple Limn operators using the full vocabulary of **1,076 words**.

## Expression Patterns

### Operator Precedence
- `^` (gradient): Highest
- `@` (projection): Middle
- `*` (interference): Lowest
- `given` (conditional): Right-associative
- `without` (subtraction): Negation

### 20 Core Patterns

#### Pattern 1: (A@B)^X (150 expressions)
**Projection with Intensity**
- Extract an aspect of a concept and modify its intensity
- Example: `(lov@fer)^0.8` = intense fear component of love
- Domain: emotion

#### Pattern 2: A*B given C (150 expressions)
**Blend with Condition**
- Combine two concepts when a condition holds
- Example: `joy*sad given tim` = bittersweet when time passes
- Domain: composite

#### Pattern 3: (A without B)^X (150 expressions)
**Negation with Intensity**
- Pure concept excluding another, at specified intensity
- Example: `(hop without fer)^0.9` = intense hope without fear
- Domain: negation

#### Pattern 4: (A±B)@C (150 expressions)
**Superposition Viewed As**
- Simultaneous dual states from a perspective
- Example: `(joy ± sad)@tim` = simultaneous joy and sorrow viewed as time
- Domain: paradox

#### Pattern 5: A@(B*C^X) (150 expressions)
**Projection Through Blend**
- View something through a blended, intensity-modified concept
- Example: `hop@(joy*trust^0.7)` = hope through blend of joy and strong trust
- Domain: filtered

#### Pattern 6: (A@B) without (C@D) (100 expressions)
**Dual Projection Negation**
- Extract one aspect, exclude another projection
- Example: `(lov@fer) without (hat@joy)` = love's fear minus hate's joy
- Domain: complex

#### Pattern 7: A*B without C given D (100 expressions)
**Conditional Negated Blend**
- Blend two things, exclude third, when fourth condition holds
- Example: `joy*hop without ang given trs` = joy-hope blend without anger when trust holds
- Domain: conditional

#### Pattern 8: ((A@B)*C^X) given D (100 expressions)
**Nested Projection Blend**
- Complex nesting: project, blend, intensify, then condition
- Example: `((lov@fer)*joy^0.6) given tim` = blended love's fear with joy (0.6), when time applies
- Domain: nested

#### Pattern 9: (A±B±C)@D (100 expressions)
**Triple Superposition**
- Three simultaneous states viewed as something
- Example: `(lov ± hat ± ind)@por` = love, hate, and indifference simultaneous from perspective of purpose
- Domain: superposition

#### Pattern 10: A^X without B^Y given C (100 expressions)
**Comparative Intensities**
- Two intensity-modified states, one negated, conditional
- Example: `joy^0.9 without sad^0.1 given trs` = intense joy minus slight sadness when trust holds
- Domain: intensity

#### Pattern 11: (A*B)@(C without D)^X (100 expressions)
**Projection Chain**
- Blend projected through negation, all intensity-modified
- Example: `(joy*hop)@(trs without dou)^0.85` = joy-hope blend viewed through trust-without-doubt at 0.85
- Domain: projection

#### Pattern 12: A given (B without C±D) (100 expressions)
**Complex Conditional**
- Condition on complex state (negation AND superposition)
- Example: `lov given (trs without dou ± hop)` = love when trust is absent/hope exists
- Domain: conditional

#### Pattern 13: (A^X)*(B^Y) without C (100 expressions)
**Blended Intensities with Negation**
- Two separately-modified states blended, one component excluded
- Example: `(joy^0.8)*(hop^0.6) without ang` = blend of strong joy with moderate hope, excluding anger
- Domain: blended

#### Pattern 14: A@B*C@D given E^X (100 expressions)
**Dual Projections Conditioned**
- Blend two projections, condition on intensified state
- Example: `lov@fer*joy@tim given trs^0.7` = blend love's fear with joy's time aspect, when trust strong
- Domain: dual

#### Pattern 15: (A±B)*(C±D) given E without F (100 expressions)
**Superposed Blend with Conditions**
- Blend two superposition pairs, conditional with negation
- Domain: superposed

#### Pattern 16: A@B^X*C^Y (100 expressions)
**Multi-Intensity Projection Blend**
- Project through intensified concept, blend with another intensified concept
- Domain: multi-intensity

#### Pattern 17: A without B*C given D^X (100 expressions)
**Negated Blend Conditional**
- Exclude concept from base, blend with another, intensified condition
- Domain: conditional

#### Pattern 18: (A@B)^X without (C@D) (100 expressions)
**Intensity Negation with Duality**
- Intense projection minus another projection
- Domain: intensity

#### Pattern 19: A±B without C*D^X (100 expressions)
**Superposed Negation**
- Superposition with excluded blended component
- Domain: paradox

#### Pattern 20: ((A*B)@C)^X given D without E (100 expressions)
**Deep Nesting**
- Maximum complexity: blend, project, intensify, condition, negate
- Domain: complex

## Domain Distribution

| Domain | Count | Purpose |
|--------|-------|---------|
| conditional | 300 | States contingent on conditions |
| paradox | 250 | Simultaneous contradictory states |
| complex | 200 | Multi-operator combinations |
| intensity | 200 | Graduated magnitude expressions |
| emotion | 150 | Emotional states and blends |
| composite | 150 | Simple blending operations |
| negation | 150 | Excluded or absent states |
| filtered | 150 | Perspective-filtered views |
| nested | 100 | Deeply nested operations |
| superposition | 100 | Quantum-like dual states |
| projection | 100 | Aspect extraction |
| blended | 100 | Intensity-modified blends |
| dual | 100 | Dual projection combinations |
| superposed | 100 | Multi-superposition blends |
| multi-intensity | 100 | Multiple intensity parameters |

## Vocabulary Coverage

- **Total unique words**: 1,076
- **Words used per expression**: 2-6 words
- **Expression combinations**: 2,250 unique expressions
- **Operator coverage**: All 6 operators used in various combinations

## CSV Format

```csv
expression,meaning,example,domain
(lov@fer)^0.8,"Extract fear aspect of love at 0.8 intensity","(lov@fer)^0.8",emotion
joy*sad given tim,"Blend joy and sadness when time applies","joy*sad given tim",composite
hop without fer given trs,"Hope without fear when trust holds","hop without fer given trs",conditional
```

## Use Cases

These expressions can represent:
- **Emotional states**: Complex feelings combining multiple emotions
- **Cognitive states**: Thoughts viewed through different mental lenses
- **Temporal concepts**: States that change or depend on time
- **Social dynamics**: Relationships with various conditions and exclusions
- **Abstract ideas**: Concepts that require multiple operators to express
- **States of being**: Existence conditions with multiple parameters

## Examples by Semantic Domain

### Emotion
- `(lov@fer)^0.8` = intense fear component of love
- `(joy ± sad)@tim` = simultaneous joy and sadness from temporal perspective
- `joy^0.9 without sad^0.1 given trs` = intense joy minus slight sadness when trust holds

### Cognition
- `(mem@tho)^0.7` = memory as aspect of thought, 0.7 intensity
- `ide*bel without dou given kno` = idea blended with belief, excluding doubt, when knowledge holds
- `(dre@tim)^0.85` = dream-like quality at 0.85 intensity

### Time & Change
- `(aft@now)^0.5` = after-quality of now, moderate intensity
- `gro*sto given tim` = growth blended with stasis when time applies
- `(cha without end)^0.9` = intense change without endpoint

### Social
- `(lov@agt)^0.8` = agent-aspect of love at 0.8
- `trs*hop without dou given tim` = trust blended with hope, excluding doubt, with time
- `(rel ± con)@soc` = relation and connection simultaneously from social perspective

## File Location

- **Generated expressions**: `/home/eric/src/limntown/limn/crew/linguist/generated/complex_expressions.csv`
- **Total expressions**: 2,250
- **Total lines** (including header): 2,251

## Metadata

- **Created**: February 3, 2026
- **Generation method**: Systematic pattern-based composition
- **Operators used**: @, *, ^, without, ±, given
- **Vocabulary source**: Limn vocabulary database (1,076 words)
- **Quality threshold**: Each expression requires 2-6 operators for semantic complexity
