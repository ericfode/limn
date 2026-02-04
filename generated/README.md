# Complex Compositional Expressions Dataset

## Overview

This directory contains **2,250+ complex compositional expressions** generated from the Limn vocabulary database, combining multiple operators to express nuanced semantic meanings.

## Files

### Main Output
- **`complex_expressions.csv`** (260 KB, 2,250 expressions)
  - Master dataset of complex multi-operator expressions
  - Format: expression, meaning, example, domain

### Supporting Files
- **`EXPRESSIONS_SUMMARY.md`** - Comprehensive guide to all 20 expression patterns
- **`README.md`** - This file

### Legacy/Additional Expressions
- `cognitive_expressions.csv` - Expressions for cognition domain
- `projection_expressions.csv` - Expressions using @ operator
- `interference_expressions.csv` - Expressions using * operator
- `gradient_expressions.csv` - Expressions using ^ operator
- `subtraction_expressions.csv` - Expressions using 'without' operator
- `temporal_expressions.csv` - Time-related expressions

## Dataset Specification

### Statistics
- **Total expressions**: 2,250
- **Expression patterns**: 20 distinct syntactic patterns
- **Operator combinations**: 6 operators (@ * ^ without ± given)
- **Vocabulary size**: 1,076 unique words
- **Words per expression**: 2-6
- **Domain categories**: 15 semantic domains

### Domains
1. **conditional** (300) - States contingent on conditions
2. **paradox** (250) - Simultaneous contradictory states
3. **complex** (200) - Multi-operator combinations
4. **intensity** (200) - Graduated magnitude expressions
5. **emotion** (150) - Emotional states and blends
6. **composite** (150) - Simple blending operations
7. **negation** (150) - Excluded or absent states
8. **filtered** (150) - Perspective-filtered views
9. **nested** (100) - Deeply nested operations
10. **superposition** (100) - Quantum-like dual states
11. **projection** (100) - Aspect extraction
12. **blended** (100) - Intensity-modified blends
13. **dual** (100) - Dual projection combinations
14. **superposed** (100) - Multi-superposition blends
15. **multi-intensity** (100) - Multiple intensity parameters

## Expression Examples

### Pattern 1: Projection with Intensity
```
(lov@fer)^0.8 = intense fear component of love
(joy@tim)^0.5 = medium-intensity temporal aspect of joy
```

### Pattern 2: Conditional Blend
```
joy*sad given tim = bittersweet experience when time applies
trs*hop without dou given tim = trust-hope blend minus doubt when time matters
```

### Pattern 3: Negation with Intensity
```
(hop without fer)^0.9 = intensely pure hope excluding fear
(str without wea)^0.7 = relatively pure strength excluding weakness
```

### Pattern 4: Superposition
```
(joy ± sad)@tim = simultaneous joy and sorrow viewed as time
(trs ± dou)@agi = trust and doubt simultaneously from agency perspective
```

### Pattern 5: Complex Nesting
```
((lov@fer)*joy^0.6) given tim = love's fear blended with moderate joy, when time applies
(joy*hop)@(trs without dou)^0.85 = joy-hope blend viewed through strong trust-without-doubt
```

## Operator Reference

### @ (Projection)
Extract or view one concept as an aspect of another
- `A@B` = view A through the lens of B

### * (Interference)
Combine or blend two concepts
- `A*B` = blend A and B together

### ^ (Gradient)
Modify intensity of preceding expression (0.0 to 1.0)
- `A^0.5` = moderate A
- `A^0.9` = intense A

### without (Subtraction)
Exclude or negate a concept
- `A without B` = A excluding B

### ± (Superposition)
Simultaneous dual/contradictory states
- `A ± B` = both A and B at once

### given (Conditional)
Apply condition (right-associative)
- `A given B` = A when condition B holds

## Use Cases

- **Emotional analysis**: Expressing complex feelings like nostalgia, bittersweet, anticipatory anxiety
- **Cognitive modeling**: Mental states combining memory, thought, dream, intention
- **Temporal reasoning**: States that evolve or depend on time
- **Philosophical inquiry**: Paradoxes, contradictions, quantum-like states
- **Natural language**: Nuanced meanings difficult to express with single words
- **Knowledge representation**: Semantic networks with multiple operators

## Generation Method

Expressions were systematically generated using:
1. All 1,076 words from Limn vocabulary database
2. 20 distinct syntactic patterns combining operators
3. Operator precedence: ^ > @ > * > given, without
4. Random word selection ensuring semantic diversity
5. Intensity values uniformly distributed [0.1, 0.95]

## CSV Format

```
expression,meaning,example,domain
(lov@fer)^0.8,Extract fear aspect of love at 0.8 intensity,(lov@fer)^0.8,emotion
joy*sad given tim,Blend joy and sadness when time applies,joy*sad given tim,composite
```

## Quality Assurance

- All expressions are syntactically valid
- Each expression combines 2+ operators for semantic complexity
- Operator precedence strictly followed
- Intensity values within valid range [0.0, 1.0]
- No duplicate expressions
- Balanced distribution across domains and patterns

## Version History

- **v1.0** (2026-02-03): Initial release
  - 2,250 complex expressions
  - 20 compositional patterns
  - Full vocabulary coverage
  - 15 semantic domains

## Future Extensions

Potential enhancements:
- Expressions with multiple intensity parameters
- Recursive/self-referential structures
- Domain-specific expression sets
- Validated natural language translations
- Compositional semantics evaluation
- Expression inference rules

## License

Generated from Limn vocabulary database. Use in accordance with project licensing.

## Contact

Generated for Limn linguistic research. Part of Gas Town Workspace.
