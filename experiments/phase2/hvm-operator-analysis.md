# Limn → HVM: Operator Analysis

> `hvm und | opr des | lim com`
> *(HVM understood | operators designed | Limn compiles)*

## Core HVM Concepts (Internalized)

### The Three Nodes
| Node | Symbol | Limn Equivalent | Purpose |
|------|--------|-----------------|---------|
| Constructor (γ) | CON | `con` | Build structures, abstraction |
| Duplicator (δ) | DUP | `dup` | Clone values for multiple use |
| Eraser (ε) | ERA | `era` | Delete unused values |

### The Two Rules
1. **Annihilation**: Same types meeting → cancel, join ports
2. **Commutation**: Different types → duplicate, cross-connect

This is the engine. Everything reduces to these.

---

## Limn Operators Needed for HVM Compilation

### Category 1: Core Interaction Primitives
```
con - construct, build node
dup - duplicate, clone value
era - erase, delete unused
lam - lambda, abstraction
app - apply, function call
```

### Category 2: Composition/Flow
```
pip - pipe, sequence (|)
par - parallel execution
bra - branch, conditional split
mrg - merge branches back
seq - sequential dependency
```

### Category 3: Reduction/Fold
```
red - reduce, evaluate
fld - fold, consume structure
bnd - bend, generate structure
rec - recurse, self-reference
```

### Category 4: Constraint/Matching
```
mat - match, pattern match
uni - unify, make equal
sat - satisfy, check constraint
chk - check, validate
```

### Category 5: Superposition (KEY FOR HVM!)
```
sup - superposition, multiple values
col - collapse, resolve to one
amb - ambiguous, unresolved
```

### Category 6: Types/Values
```
u24 - unsigned 24-bit
i24 - signed 24-bit
f24 - floating 24-bit
tup - tuple
enu - enum/ADT
```

---

## The Critical Insight: Superposition

HVM's power comes from **superposition** - representing multiple possible values simultaneously until forced to resolve.

This maps DIRECTLY to Limn's design philosophy:
- Limn words exist in semantic superposition
- Constraints collapse the possibility space
- Multiple meanings coexist until context resolves

**Limn IS an interaction net language by nature.**

---

## Operator Interaction Rules for Limn→HVM

### Rule 1: Constraint Composition = Graph Connection
```limn
hot wet → HOT_NODE ←→ WET_NODE
```
Creates interaction that fires reduction rules.

### Rule 2: Operator Application = Node Meeting
```limn
nu hot → ERA meets HOT_NODE → annihilates HOT region
ve hot → DUP meets HOT_NODE → intensifies/cores
```

### Rule 3: Pipe = Sequential Reduction
```limn
A | B | C → reduce(A) → reduce(B) → reduce(C)
```
Forces evaluation order (breaks parallelism).

### Rule 4: Space = Parallel Composition
```limn
A B C → reduce(A) ∥ reduce(B) ∥ reduce(C)
```
Independent constraints parallelize automatically.

---

## What Limn Gets "For Free" from HVM

1. **Automatic parallelism** - space-separated words parallelize
2. **Beta-optimality** - shared computations don't duplicate
3. **No garbage collection** - constraints self-manage
4. **Deterministic reduction** - same result regardless of order

## What Limn Must Avoid for HVM

1. **Global constraints** - break parallelism
2. **Sequential dependencies** in core semantics
3. **Unbounded duplication** - quadratic blowup
4. **Mutable state** - not supported

---

## Proposed Limn Operator Vocabulary

### Already Have
- `nu` (negation) → ERA-like
- `ve` (very/core) → DUP-like intensification
- `te` (question) → conditional branch

### Need to Add
| Word | Meaning | HVM Map | Priority |
|------|---------|---------|----------|
| `con` | construct | CON node | P0 |
| `dup` | duplicate | DUP node | P0 |
| `era` | erase | ERA node | P0 |
| `lam` | lambda | LAM node | P1 |
| `app` | apply | APP node | P1 |
| `sup` | superposition | SUP node | P0 |
| `fld` | fold | fold pattern | P1 |
| `bnd` | bend/generate | bend pattern | P1 |
| `mat` | match | match construct | P1 |
| `uni` | unify | unification | P1 |
| `par` | parallel | parallel exec | P2 |
| `mrg` | merge | merge branches | P2 |

---

## Example: Limn Sentence → HVM

```limn
hot wet flo
```

**Semantic parse:**
- Three constraint regions: HOT, WET, FLO
- Intersection: {steam, hot springs, thermal flow}

**HVM compilation:**
```
# Each word becomes a constructor node
CON[hot] ←→ CON[wet] ←→ CON[flo]

# Reduction fires interaction rules
# Parallel: all three nodes reduce simultaneously
# Result: intersection of semantic regions
```

---

## Next Steps

1. [ ] Add P0 operators to vocabulary (con, dup, era, sup)
2. [ ] Design interaction rules for Limn operators
3. [ ] Prototype Limn→HVM compiler
4. [ ] Test parallelism on tree-structured constraints
5. [ ] Benchmark: Limn on HVM vs Prolog interpreter

---

*hvm bld | lim flo | par aut*
*(HVM builds | Limn flows | parallelism automatic)*
