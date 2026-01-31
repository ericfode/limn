# Limn-PL on HVM: Design and Implementation

**Status:** Implemented (Metacircular)
**Target:** HVM (Higher-order Virtual Machine) via Bend language
**Implementation:** `src/limn_hvm/limn_ful.bend`

---

## Overview

HVM's superposition semantics provide a natural foundation for Limn-PL's constraint-based programming model. This document outlines the design and implementation of the metacircular Limn interpreter in Bend.

## Implementation Status

| Feature | Status | Description |
|---------|--------|-------------|
| ope (joi, cut, exp, con) | Working | Arithmetic operations |
| rel (sa, ma, mi, amo) | Working | Relations with nu inversion |
| cnd (if/the/oth) | Working | Conditionals |
| alt (pa) | Working | Alternatives/superposition |
| cau/eff | Working | Function definitions & calls |
| rec | Working | Recursion via cau |
| bwd | Working | Bidirectional constraint solving |
| itr | Working | Iterative constraint propagation |
| gro (who, fst, fin, par, nxt) | Working | Group operations |
| sup | Working | HVM native superpositions |

---

## Key Mappings

### 1. Alternatives (pa) → Superpositions

Limn-PL:
```limn
x sa 1 pa x sa 2 pa x sa 3
```

HVM/Bend:
```bend
x = {1 2 3}
```

HVM's superposition `{a b}` represents "could be a or b", which is exactly what Limn's `pa` expresses.

### 2. Constraint Satisfaction → Parallel Filtering

Limn-PL:
```limn
x ma 0 | x mi 10 | x sa y exp 2
```

HVM/Bend approach:
```bend
# Generate possibilities
x = {0 1 2 3 4 5 6 7 8 9}

# Filter by constraints
x_filtered = filter(x, λv: (and (> v 0) (< v 10)))

# Apply constraint: x = y^2
y = sqrt(x_filtered)
```

Superpositions allow parallel exploration of all valid values.

### 3. Bidirectional Arithmetic → Symbolic Superposition

Limn-PL's bidirectional arithmetic:
```limn
a joi b sa c
```

For forward computation (given a, b → compute c):
```bend
c = (+ a b)
```

For backward computation (given a, c → compute b):
```bend
# Create superposition of all possible b values
b = collapse(filter({0..MAX}, λb: (== (+ a b) c)))
```

### 4. Functions → Named Superposition Transformers

Limn-PL function:
```limn
pro dbl |
var | whe x | whe eff |
cns | eff sa x exp 2
```

HVM/Bend:
```bend
def dbl(x):
  return (* x 2)

# Calling with superposition
dbl({1 2 3})  # returns {2 4 6}
```

---

## Architecture

### Compilation Pipeline

```
Limn-PL Source
      │
      ▼
┌─────────────────┐
│ Limn-PL Parser │
└─────────────────┘
      │
      ▼
┌─────────────────┐
│ Constraint AST  │
└─────────────────┘
      │
      ▼
┌─────────────────┐
│ HVM/Bend Codegen│
└─────────────────┘
      │
      ▼
┌─────────────────┐
│ HVM Runtime     │
│ (GPU/Parallel)  │
└─────────────────┘
```

### Key Advantages

1. **True Parallelism**: HVM automatically parallelizes superposition operations
2. **Lazy Evaluation**: Only compute paths that are needed
3. **Optimal Reduction**: Beta-optimal evaluation for higher-order functions
4. **GPU Execution**: `bend run-cu` for massive parallelism

---

## Example Translation

### Limn-PL: Temperature Conversion

```limn
pro tem-con |
var | whe C | whe F | whe t1 | whe t2 |
cns |
t1 sa C exp 9 |
t2 sa t1 con 5 |
F sa t2 joi 32
```

### HVM/Bend Translation

```bend
# Forward: C → F
def celsius_to_fahrenheit(C):
  t1 = (* C 9)
  t2 = (/ t1 5)
  F = (+ t2 32)
  return F

# Backward: F → C (using superposition search)
def fahrenheit_to_celsius(F):
  # Search for C that satisfies constraints
  C = collapse(filter({-100..200}, λc:
    let t1 = (* c 9)
    let t2 = (/ t1 5)
    let f = (+ t2 32)
    (== f F)
  ))
  return C

# Main with superposition input
def main():
  # Test forward
  return celsius_to_fahrenheit(100)  # → 212

  # Test backward with superposition
  # return fahrenheit_to_celsius({32 212 98.6})  # → {0 100 37}
```

---

## Challenges and Solutions

### Challenge 1: Duplication Interference

HVM has a restriction: duplicated variables shouldn't duplicate other variables that duplicate.

**Solution**: Linearize constraints where possible, or use explicit duplication points.

### Challenge 2: Infinite Domains

Superpositions must be finite in HVM.

**Solution**:
- Use bounded numeric domains
- Lazy generation with early termination
- Symbolic computation for algebraic cases

### Challenge 3: Non-invertible Operations

Not all operations are bidirectional (e.g., `pow` is hard to invert).

**Solution**:
- Use search for non-invertible operations
- Numeric approximation for real-valued inverses

---

## Implementation Roadmap

### Phase 1: Core Arithmetic - COMPLETE
- [x] Implement `joi`, `cut`, `exp`, `con` as Bend functions
- [x] Test bidirectional with superposition search
- [x] Forward solving (a ope b sa c, solve c)
- [x] Backward solving (a ope ? sa c, solve ?)

### Phase 2: Lists - COMPLETE
- [x] Implement `par`, `who`, `fst`, `nxt`, `fin`
- [x] Superposition over list elements
- [x] Implement `amo` (among) relation

### Phase 3: Constraints - COMPLETE
- [x] AST-based constraint representation
- [x] Iterative constraint propagation
- [x] Relational constraints (sa, ma, mi, amo)
- [x] Negation (nu/inv) support

### Phase 4: Functions - COMPLETE
- [x] Function definitions (cau/eff)
- [x] Function calls (Cal nodes)
- [x] Recursive functions (factorial demo)

### Phase 5: Integration - IN PROGRESS
- [ ] Limn-PL parser outputting Bend
- [ ] REPL with GPU execution option
- [ ] Native Limn vocabulary (no English comments)

---

## Resources

- [HVM Repository](https://github.com/HigherOrderCO/HVM)
- [Bend Language](https://github.com/HigherOrderCO/Bend)
- [Dups and Sups Documentation](https://github.com/HigherOrderCO/Bend/blob/main/docs/dups-and-sups.md)
- [Program Search with Superpositions](https://gist.github.com/VictorTaelin/d5c318348aaee7033eb3d18b0b0ace34)

---

**END OF DESIGN**
