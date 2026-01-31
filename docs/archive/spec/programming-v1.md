# Limn Programming Language v1: Concrete Syntax

**Status:** Draft Specification
**Prerequisite:** programming-v0.md (theory)
**Goal:** Define concrete syntax for constraint-based, order-independent programs

---

## 1. Design Principles

### 1.1 Core Axioms

1. **Order Independence:** Statement order has no semantic effect
2. **Input as Key:** Input provides asymmetry; program provides structure
3. **Constraints, Not Commands:** Statements describe what must be true
4. **Bidirectionality:** Same program can compute in multiple directions

### 1.2 Syntactic Goals

- Readable by humans familiar with Limn vocabulary
- Parseable by conventional tools
- Clear scope boundaries
- Minimal keywords beyond Limn vocabulary

---

## 2. Basic Syntax

### 2.1 Program Structure

```
program <name>:
  variables: <var-list>
  constraints:
    <constraint-1>
    <constraint-2>
    ...
```

**Example:**
```
program addition:
  variables: a b c
  constraints:
    a plus b equals c
```

### 2.2 Variables

Variables are untyped by default. Types are inferred from constraints.

```
variables: x y z
variables: input output
variables: source target transform
```

### 2.3 Constraints

Constraints use Limn-style vocabulary extended with programming relations.

**Equality:**
```
x equals 5
y equals x plus 3
```

**Relations:**
```
x greater_than y
a element_of collection
p implies q
```

**Negation:**
```
nu x equals y          # x does not equal y
nu a element_of b      # a is not in b
```

---

## 3. Built-in Relations

### 3.1 Arithmetic

| Relation | Meaning | Bidirectional? |
|----------|---------|----------------|
| `a plus b equals c` | a + b = c | Yes: any 2 known → 3rd computed |
| `a minus b equals c` | a - b = c | Yes |
| `a times b equals c` | a × b = c | Yes (for non-zero) |
| `a divided_by b equals c` | a ÷ b = c | Yes (for non-zero b) |
| `a modulo b equals c` | a mod b = c | Partial |
| `a power b equals c` | a^b = c | Partial (roots, logs) |

### 3.2 Comparison

| Relation | Meaning |
|----------|---------|
| `a equals b` | a = b |
| `a greater_than b` | a > b |
| `a less_than b` | a < b |
| `a at_least b` | a ≥ b |
| `a at_most b` | a ≤ b |
| `nu a equals b` | a ≠ b |

### 3.3 Collections

| Relation | Meaning |
|----------|---------|
| `x element_of c` | x ∈ c |
| `a subset_of b` | a ⊆ b |
| `size_of c equals n` | |c| = n |
| `c contains x` | x ∈ c (alternate) |
| `a union b equals c` | a ∪ b = c |
| `a intersect b equals c` | a ∩ b = c |

### 3.4 Logic

| Relation | Meaning |
|----------|---------|
| `p and q` | p ∧ q |
| `p or q` | p ∨ q |
| `nu p` | ¬p |
| `p implies q` | p → q |
| `p equivalent q` | p ↔ q |

### 3.5 Existence

| Relation | Meaning |
|----------|---------|
| `exists x such_that P` | ∃x: P(x) |
| `forall x P` | ∀x: P(x) |
| `unique x such_that P` | ∃!x: P(x) |

---

## 4. Control Structures (As Constraints)

### 4.1 Conditional (If-Then)

```
# If x > 0 then y = x, else y = 0
condition: x greater_than 0
  then: y equals x
  else: y equals 0
```

**Constraint form:**
```
(x greater_than 0 and y equals x) or (x at_most 0 and y equals 0)
```

### 4.2 Branching (Case Analysis)

```
case x:
  when 1: result equals "one"
  when 2: result equals "two"
  otherwise: result equals "other"
```

### 4.3 Iteration (As Recursion/Fixed Point)

```
# Factorial: fact(n) = n * fact(n-1), fact(0) = 1
program factorial:
  variables: n result
  constraints:
    (n equals 0 and result equals 1) or
    (n greater_than 0 and
     exists m f such_that:
       m equals n minus 1
       factorial(m) equals f
       result equals n times f)
```

**Note:** Iteration becomes recursive constraint definition. Order of base case vs recursive case doesn't matter - both are constraints that must be satisfied.

---

## 5. Program Execution

### 5.1 Input Specification

Input is provided as a set of variable bindings:

```
run addition with:
  a = 3
  b = 5
# Output: c = 8

run addition with:
  a = 3
  c = 10
# Output: b = 7 (running "backwards")
```

### 5.2 Query Modes

**Find all:** Return all satisfying assignments
```
find all: x
where: x element_of [1,2,3,4,5]
       x times x less_than 20
# Output: x ∈ {1, 2, 3, 4}
```

**Find one:** Return any satisfying assignment
```
find one: solution
where: ...constraints...
```

**Check:** Return true/false for satisfiability
```
check: x equals 5 and x equals 6
# Output: false (unsatisfiable)
```

### 5.3 Execution Model

1. Parse program into constraint set P
2. Parse input into constraint set I
3. Compute M(P) ∩ C(I)
4. Return:
   - The unique point (if singular)
   - All points (if multiple, for `find all`)
   - Any point (if multiple, for `find one`)
   - Error (if empty intersection)

---

## 6. Examples

### 6.1 Two-Sum Problem

```
program two_sum:
  variables: nums target i j
  constraints:
    i index_of nums
    j index_of nums
    nu i equals j
    (value_at i nums) plus (value_at j nums) equals target
```

**Usage:**
```
run two_sum with:
  nums = [1, 2, 3, 4, 5]
  target = 7
# Output: (i=1, j=4) or (i=2, j=3) etc.
```

### 6.2 Filter

```
program filter:
  variables: source predicate result
  constraints:
    forall item:
      (item element_of source and predicate(item)) implies item element_of result
      (item element_of source and nu predicate(item)) implies nu item element_of result
      nu item element_of source implies nu item element_of result
```

### 6.3 Map

```
program map:
  variables: source transform target
  constraints:
    size_of source equals size_of target
    forall i:
      i index_of source implies
        (value_at i target) equals transform(value_at i source)
```

**Bidirectional usage:**
```
# Forward
run map with:
  source = [1, 2, 3]
  transform = double
# Output: target = [2, 4, 6]

# Backward
run map with:
  target = [10, 20, 30]
  transform = double
# Output: source = [5, 10, 15]
```

### 6.4 Sort

```
program sort:
  variables: input output
  constraints:
    permutation input output
    ordered output
```

**Constraint explanation:**
- `permutation input output`: output contains same elements as input
- `ordered output`: output[i] ≤ output[i+1] for all i

No algorithm specified. The constraint solver finds a satisfying permutation.

### 6.5 Family Relations (Prolog-style)

```
program family:
  variables: parent child grandparent
  constraints:
    # grandparent(G, C) iff exists P: parent(G, P) and parent(P, C)
    exists intermediate such_that:
      parent(grandparent, intermediate)
      parent(intermediate, child)

# Usage 1: Find grandchildren
run family with:
  grandparent = "Alice"
  database = {parent("Alice", "Bob"), parent("Bob", "Carol")}
# Output: child = "Carol"

# Usage 2: Find grandparents
run family with:
  child = "Carol"
  database = {parent("Alice", "Bob"), parent("Bob", "Carol")}
# Output: grandparent = "Alice"
```

---

## 7. Type System

### 7.1 Inferred Types

Types are inferred from constraints:

```
program example:
  variables: x y
  constraints:
    x plus y equals 10    # x, y inferred as Numeric
    x greater_than 0      # x inferred as Ordered Numeric
```

### 7.2 Explicit Type Annotations

```
program typed_example:
  variables:
    x: Integer
    name: String
    items: List[Integer]
  constraints:
    ...
```

### 7.3 Type Constraints

Types are constraints too:
```
x is_type Integer
# Equivalent to: x element_of Integers
```

---

## 8. Modules and Composition

### 8.1 Importing Programs

```
import arithmetic
import collections

program main:
  uses: arithmetic.addition, collections.filter
  ...
```

### 8.2 Program Composition

Programs compose by constraint union:

```
program combined:
  include: program_a
  include: program_b
  constraints:
    # Additional constraints that bridge program_a and program_b
    output_of_a equals input_of_b
```

### 8.3 Namespace

Variables in included programs are prefixed:
```
include: addition as add1
include: addition as add2
constraints:
  add1.c equals add2.a  # Output of first addition feeds second
```

---

## 9. Error Handling

### 9.1 Unsatisfiable Constraints

```
program impossible:
  constraints:
    x equals 5
    x equals 6
# Error: No satisfying assignment exists
```

### 9.2 Underconstrained (Multiple Solutions)

```
program ambiguous:
  variables: x y
  constraints:
    x plus y equals 10
# Multiple solutions: needs more input constraints
```

**Options:**
- Return all solutions
- Return any solution
- Fail and request more constraints

### 9.3 Partial Functions

```
# Division by zero
run with: a=5, b=0
constraints: a divided_by b equals c
# Error: No solution for b=0
```

---

## 10. Relationship to Limn Natural Language

### 10.1 Vocabulary Mapping

| Programming | Limn | Meaning |
|-------------|-------|---------|
| `equals` | `sa` | same, unified |
| `greater_than` | `ma` | more, increasing |
| `less_than` | `le` | less, decreasing |
| `element_of` | `bi` | between, connecting |
| `nu` (negation) | `nu` | negation |
| `and` | (intersection) | default composition |
| `or` | `pa` | parallel, alternative |

### 10.2 Program as Key

A program can serve as a key for natural language interpretation:

```
# The program IS the key
program river_ecology:
  variables: flow life cycle
  constraints:
    flow is_type continuous
    life element_of flow
    cycle repeats

# Limn sentence interpreted under this program:
# "ra vi ri" (linear + alive + cyclic)
# Under river_ecology key → "The cyclical life in the river's flow"
```

---

## 11. Implementation Notes

### 11.1 Execution Strategies

1. **Constraint Propagation:** Incrementally reduce domains
2. **SMT Solving:** For arithmetic and logical constraints
3. **Unification:** For symbolic constraints (Prolog-style)
4. **Search:** For combinatorial constraints

### 11.2 Optimization

- Lazy evaluation: Only compute what's asked
- Incremental solving: Reuse work when input changes
- Parallelization: Independent constraints can be checked concurrently

### 11.3 Compilation

Programs can be compiled to:
- SMT-LIB format (for SMT solvers)
- Prolog clauses (for logic programming)
- Datalog rules (for database queries)
- Custom constraint solver bytecode

---

## 12. Future Directions

### 12.1 Temporal Constraints

```
# Time enters through input, not program
program temporal:
  variables: event1 event2 time_of
  constraints:
    time_of(event1) less_than time_of(event2)
    # "event1 before event2" from data, not syntax
```

### 12.2 Probabilistic Constraints

```
# Soft constraints with weights
program uncertain:
  constraints:
    x equals 5 with_probability 0.8
    x equals 6 with_probability 0.2
```

### 12.3 Interactive Constraints

```
# User provides constraints incrementally
interactive_mode:
  > x plus y equals 10
  # Many solutions
  > x greater_than 3
  # Fewer solutions
  > y is_type Integer
  # Even fewer
  > x equals 7
  # Unique: x=7, y=3
```

---

## Appendix A: Complete Grammar (EBNF)

```ebnf
program     = "program" name ":" vars constraints
vars        = "variables:" var_list
var_list    = var ("," var)*
var         = identifier (":" type)?
constraints = "constraints:" constraint+
constraint  = relation | negation | quantified | conditional

relation    = term rel_op term
rel_op      = "equals" | "greater_than" | "less_than" | ...
negation    = "nu" constraint
quantified  = ("forall" | "exists") var constraint
conditional = "(" constraint "and" constraint ")" |
              "(" constraint "or" constraint ")"

term        = var | literal | term arith_op term | function_call
arith_op    = "plus" | "minus" | "times" | "divided_by"
function_call = name "(" term_list ")"
term_list   = term ("," term)*

literal     = number | string | list | set
```

---

## Appendix B: Comparison to Existing Languages

| Feature | Limn | Prolog | SQL | Haskell |
|---------|-------|--------|-----|---------|
| Order-independent statements | ✓ | Partial | ✓ | No |
| Bidirectional computation | ✓ | ✓ | No | No |
| Input determines direction | ✓ | ✓ (modes) | Partial | No |
| No argument positions | ✓ | No | No | No |
| Constraint-based | ✓ | ✓ | ✓ | No |
| Type inference | ✓ | No | Partial | ✓ |

---

**END OF SPECIFICATION v1**
