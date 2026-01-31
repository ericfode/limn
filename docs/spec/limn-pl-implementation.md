# Limn-PL Implementation Guide

**Status:** Substantially Complete
**Location:** `src/limn_pl_interpreter.py`

---

## Overview

Limn-PL is a constraint-based programming language where:
- Statement order doesn't matter (commutative)
- Programs are sets of constraints that must all be satisfied
- Execution is bidirectional constraint satisfaction
- Input values ("keys") provide the asymmetry that collapses to specific behavior

---

## Implemented Features

### 1. Core Arithmetic

| Limn Word | Operation | Example |
|------------|-----------|---------|
| `joi` | Addition | `a joi b sa c` (a + b = c) |
| `cut` | Subtraction | `a cut b sa c` (a - b = c) |
| `exp` | Multiplication | `a exp b sa c` (a * b = c) |
| `con` | Division | `a con b sa c` (a / b = c) |
| `pow` | Exponentiation | `a pow b sa c` (a ^ b = c) |

All arithmetic operations work **bidirectionally**. Given any two values, the third is computed.

### 2. Relations

| Limn Word | Relation | Example |
|------------|----------|---------|
| `sa` | Equals | `x sa 5` |
| `ma` | Greater than | `x ma 0` |
| `mi` | Less than | `x mi 10` |
| `eq` | Equal comparison | `x eq y` |

### 3. Collections (Lists/Groups)

| Limn Word | Operation | Example |
|------------|-----------|---------|
| `gro` | Group literal | `gro \| 1 \| 2 \| 3` |
| `par` | Index access | `par 0 arr sa x` (x = arr[0]) |
| `who` | Size/length | `who arr sa n` (n = len(arr)) |
| `amo` | Element of | `x amo arr` (x in arr) |
| `ins` | Contains | `x ins arr` (x in arr) |
| `fst` | First element | `fst arr sa x` (x = arr[0]) |
| `nxt` | Tail/rest | `nxt arr sa rest` (rest = arr[1:]) |
| `fin` | Last element | `fin arr sa x` (x = arr[-1]) |

### 4. Boolean/OR Logic

| Limn Word | Operation | Example |
|------------|-----------|---------|
| `nu` | Negation | `nu x sa 5` (x != 5) |
| `pa` | Alternative (OR) | `x sa 1 pa x sa 2` (x=1 OR x=2) |
| `if/the/oth` | Conditional | `if x ma 0 \| the y sa 1 \| oth y sa 0` |

### 5. Functions

| Limn Word | Operation | Example |
|------------|-----------|---------|
| `cau` | Call function | `func cau arg eff result` |
| `eff` | Result variable | Used in function definitions |

Functions are defined as named programs:

```limn
# Define double function
pro dbl |
var | whe x | whe eff |
cns | eff sa x exp 2
```

Calling:
```limn
# Call: y = double(5)
dbl cau 5 eff y
```

---

## Program Structure

```limn
pro [name] |
var | whe [var1] | whe [var2] | ... |
cns | [constraint1] | [constraint2] | ...
```

- `pro`: Program definition
- `var`: Variable section (each variable marked with `whe`)
- `cns`: Constraints section

---

## Using the Interpreter

### Python API

```python
from src.limn_pl_interpreter import run_limn_pl

# Simple program
source = """
pro addition |
var | whe a | whe b | whe c |
cns | a joi b sa c
"""

# Forward computation
result = run_limn_pl(source, {"a": 3, "b": 5})
# result = {'a': 3, 'b': 5, 'c': 8}

# Backward computation
result = run_limn_pl(source, {"a": 3, "c": 10})
# result = {'a': 3, 'c': 10, 'b': 7}
```

### With Functions

```python
double_func = """
pro dbl |
var | whe x | whe eff |
cns | eff sa x exp 2
"""

main_prog = """
pro main |
var | whe a | whe b |
cns |
a sa 5 |
dbl cau a eff b
"""

result = run_limn_pl(main_prog, {}, functions=[double_func])
# result = {'a': 5, 'b': 10}
```

### Key Parsing

Keys can be provided in Limn syntax:

```python
from src.limn_pl_interpreter import parse_key

key = parse_key("yo a sa 3 | yo b sa 5")
# key = {'a': 3, 'b': 5}
```

---

## Test Coverage

The interpreter has 19 passing tests covering:
- Basic arithmetic (forward/backward)
- Multiplication/Division (bidirectional)
- Chained constraints
- Temperature conversion (bidirectional)
- Key parsing
- Equality constraints
- Constraint violation detection
- List operations (size, index, first, tail, last)
- Element-of constraints
- Conditionals
- Alternatives (OR)
- Function calls
- Bidirectional functions

Run tests:
```bash
python -c "import src.limn_pl_interpreter as lp; lp.run_all_tests()"
```

---

## Not Yet Implemented

1. **Quantifiers**: `al` (all), `ex` (exists) - require semantic clarification
2. **Type constraints**: `ve` (type) - static typing not implemented
3. **Group literals in source**: Parsing `gro | 1 | 2 | 3` in program source is limited
4. **Recursion within constraints**: Self-referential constraint sets

---

## Open Questions for Linguist

See bead `limn-land-355` and `docs/spec/limn-pl-v1.md` Section 12:

1. **Recursion asymmetry**: `cau` introduces asymmetry - is this acceptable?
2. **Quantifier scope**: How do `al`/`ex` interact with commutativity?
3. **Negation-as-failure**: Closed-world assumption needed for `nu x amo gro`
4. **Type system**: Is explicit typing needed for Limn-PL?

---

## Architecture

```
src/limn_pl_interpreter.py
├── AST Nodes (Variable, Literal, BinaryOp, Constraint, etc.)
├── LimnPLParser (tokenize, parse)
├── LimnPLSolver (evaluate, solve_constraint, solve)
├── High-Level API (run_limn_pl, parse_key)
└── Tests (19 test functions)
```

The solver uses iterative constraint propagation:
1. Start with key bindings as initial environment
2. Iterate through constraints, solving any that have exactly one unknown
3. Repeat until all variables are bound or no progress can be made
4. Verify all constraints are satisfied

---

**END OF DOCUMENTATION**
