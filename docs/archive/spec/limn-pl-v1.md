# Limn-PL: A Programming Language Written in Limn

**Status:** Active Specification
**Author:** Ralph Wiggum Loop Agent
**Purpose:** Define a programming language using only Limn vocabulary and constraint semantics

---

## 1. Design Philosophy

### 1.1 Core Principle

Limn-PL is not a language *about* programming - it IS programming expressed as constraint satisfaction in Limn's semantic space. Every programming concept maps to Limn vocabulary words that define constraint regions.

### 1.2 Key Innovation

**Programs are sentences.** A Limn-PL program is a sequence of Limn sentences where:
- Variables are `whe` (where/unknown) marked terms
- Constraints are ordinary Limn sentences
- Execution is constraint satisfaction
- Input is a key that collapses ambiguity

### 1.3 Properties Inherited from Limn

| Property | Manifestation |
|----------|---------------|
| Order-independence | Constraint order doesn't matter |
| Constraint intersection | All constraints must be satisfied |
| Bidirectionality | Compute any variable from others |
| Key-collapse | Input values narrow solution space |

---

## 2. Vocabulary Mapping

### 2.1 Structural Words (From Domain 9: Operators)

| Limn | Programming Meaning | Type |
|-------|---------------------|------|
| `whe` | Unknown/variable | Variable marker |
| `sa` | Same/equals | Equality constraint |
| `nu` | Negation/NOT | Boolean negation |
| `ve` | Intensifier (type) | Type constraint |
| `yo` | This/here (reference) | Variable binding |
| `an` | That/there (reference) | Reference to previous |
| `eq` | Equal (degree) | Equality comparison |
| `ma` | More/greater | Greater-than |
| `mi` | Less/smaller | Less-than |
| `al` | All/universal | Universal quantifier |
| `ex` | Exists/some | Existential quantifier |
| `on` | One/single | Singleton |

### 2.2 Operation Words (From Domain 8: Abstract + Domain 1: Physical)

| Limn | Programming Meaning | Category |
|-------|---------------------|----------|
| `joi` | Join/add/plus | Arithmetic |
| `cut` | Cut/subtract/minus | Arithmetic |
| `exp` | Expand/multiply/times | Arithmetic |
| `con` | Contract/divide/by | Arithmetic |
| `mor` | More (increment) | Arithmetic |
| `les` | Less (decrement) | Arithmetic |
| `pow` | Power/exponent | Arithmetic |
| `roo` | Root | Arithmetic |

### 2.3 Data Type Words

| Limn | Programming Type | From Domain |
|-------|------------------|-------------|
| `one` | Integer/number | Abstract (8) |
| `flo` | Float/decimal | Physical (1) - flow |
| `wor` | Word/string | Communication (6) |
| `tru` | True/boolean | Abstract (8) |
| `fal` | False/boolean | Abstract (8) |
| `gro` | Group/list/array | Social (7) |
| `net` | Network/map/dict | Physical (1) |
| `hol` | Hole/null/none | Physical (1) |

### 2.4 Control Words

| Limn | Programming Meaning | Category |
|-------|---------------------|----------|
| `if` | Conditional | Logic (8.2) |
| `the` | Then/consequence | Logic (8.2) |
| `oth` | Otherwise/else | Discourse (13) |
| `cyc` | Cycle/loop/recurse | Time (3) |
| `beg` | Begin/start | Time (3) |
| `end` | End/stop/return | Time (3) |
| `cau` | Cause/call | Time (3) |
| `eff` | Effect/result | Time (3) |

### 2.5 Collection Words

| Limn | Programming Meaning | Category |
|-------|---------------------|----------|
| `ins` | Inside/contains/in | Space (2) |
| `amo` | Among/element-of | Space (2) |
| `fst` | First/head | Discourse (13) |
| `nxt` | Next/tail | Discourse (13) |
| `fin` | Final/last | Discourse (13) |
| `par` | Part/index | Abstract (8) |
| `who` | Whole/size/length | Abstract (8) |

---

## 3. Syntax Specification

### 3.1 Program Structure

A Limn-PL program is a Limn sentence with this structure:

```limn
pro [name] | var [variables] | cns [constraints]
```

Where:
- `pro` = program (from `pro` progress/advance)
- `var` = variables (from vocabulary word `variable`)
- `cns` = constraints (from `cns` constraint)

### 3.2 Variable Declaration

Variables are marked with `whe` (where/unknown):

```limn
var | whe a | whe b | whe c
```

This declares three unknowns: a, b, c

### 3.3 Constraint Statements

Constraints use Limn vocabulary to express relationships:

```limn
# Addition: a + b = c
a joi b sa c

# Multiplication: x * y = z
x exp y sa z

# Comparison: a > b
a ma b

# Equality: x = 5
x sa 5

# Negation: x != y
nu x sa y
```

### 3.4 Complete Program Example

```limn
# Addition Program
pro joi-pro |
var | whe a | whe b | whe c |
cns | a joi b sa c
```

---

## 4. Execution Model

### 4.1 Input as Key

Input values are provided as a key that binds variables:

```limn
# Key (input)
yo a sa 3 | yo b sa 5

# Running the addition program with this key
# Solver finds: c sa 8
```

### 4.2 Bidirectional Computation

Same program, different key:

```limn
# Key specifying a and c
yo a sa 3 | yo c sa 10

# Solver computes: b sa 7 (10 - 3)
```

### 4.3 Constraint Satisfaction Algorithm

The execution model:

```limn
# Pseudocode in Limn style
pro sol |  # solver
var | whe pro | whe key | whe eff |  # program, key, effect/result
cns |
  # Start with key bindings
  key ins eff |
  # All program constraints must be satisfied
  al c | c amo pro.cns | c tru ins eff
```

---

## 5. Data Types

### 5.1 Numbers

```limn
# Integer literal
x sa 42

# Float literal
y sa 3.14 flo

# Type constraint
x ve one  # x must be integer type
```

### 5.2 Booleans

```limn
# True
p sa tru

# False
q sa fal

# Logical AND (intersection)
p q sa r  # r = p AND q

# Logical OR (using pa - parallel)
p pa q sa r  # r = p OR q

# Logical NOT
nu p sa q  # q = NOT p
```

### 5.3 Strings

```limn
# String literal
s sa "hel wor"  # "hello world" in Limn

# Concatenation (joining)
s1 joi s2 sa s3
```

### 5.4 Collections (Lists/Arrays)

```limn
# List literal
gro | 1 | 2 | 3 | 4 | 5 | sa arr

# Element access
par 0 arr sa x  # x = arr[0]

# Contains
x amo arr  # x is element of arr

# Length
who arr sa n  # n = length of arr
```

---

## 6. Control Flow (As Constraints)

### 6.1 Conditional

Conditionals are expressed as logical constraints:

```limn
# If x > 0 then y = x else y = 0
(x ma 0 | y sa x) pa (x mi 1 | y sa 0)
```

Expanded form:
```limn
# Condition and consequences are parallel alternatives
if | x ma 0 |
  the | y sa x |
  oth | y sa 0
```

### 6.2 Pattern Matching (Case Analysis)

```limn
# case x of: 1 -> "one", 2 -> "two", _ -> "other"
(x sa 1 | eff sa "one") pa
(x sa 2 | eff sa "two") pa
(nu x sa 1 | nu x sa 2 | eff sa "oth")
```

### 6.3 Iteration (As Recursion/Fixed Point)

Limn-PL has no loops. Iteration is expressed as recursive constraints:

```limn
# Factorial: fact(n) = n * fact(n-1), fact(0) = 1
pro fac |
var | whe n | whe eff |
cns |
  # Base case
  (n sa 0 | eff sa 1) pa
  # Recursive case
  (n ma 0 |
   ex m | ex f |
   m sa n cut 1 |
   fac cau m eff f |  # recursive call
   eff sa n exp f)
```

---

## 7. Functions/Procedures (As Named Constraint Sets)

### 7.1 Function Definition

Functions are named constraint sets:

```limn
# Define: double(x) = x * 2
pro dbl |
var | whe x | whe eff |
cns | eff sa x exp 2
```

### 7.2 Function Application

Functions are applied by adding their constraints with bound parameters:

```limn
# Call double(5)
dbl cau 5 eff y
# Solver finds: y sa 10
```

### 7.3 Higher-Order Functions

Functions can take functions as arguments:

```limn
# map: apply function to each element
pro map |
var | whe gro-in | whe fun | whe gro-out |
cns |
  who gro-in sa who gro-out |  # same length
  al i |
    i par gro-in |
    fun cau (par i gro-in) eff (par i gro-out)
```

---

## 8. Examples

### 8.1 Temperature Conversion

```limn
# Celsius to Fahrenheit: F = C * 9/5 + 32
pro tem-con |
var | whe C | whe F | whe t1 | whe t2 |
cns |
  t1 sa C exp 9 |      # t1 = C * 9
  t2 sa t1 con 5 |     # t2 = t1 / 5
  F sa t2 joi 32       # F = t2 + 32
```

Usage (bidirectional):
```limn
# Celsius to Fahrenheit
yo C sa 100  # Key: C = 100
# Solver: F = 212

# Fahrenheit to Celsius
yo F sa 32   # Key: F = 32
# Solver: C = 0
```

### 8.2 Fibonacci

```limn
pro fib |
var | whe n | whe eff |
cns |
  # Base cases
  (n sa 0 | eff sa 0) pa
  (n sa 1 | eff sa 1) pa
  # Recursive case
  (n ma 1 |
   ex n1 | ex n2 | ex f1 | ex f2 |
   n1 sa n cut 1 |
   n2 sa n cut 2 |
   fib cau n1 eff f1 |
   fib cau n2 eff f2 |
   eff sa f1 joi f2)
```

### 8.3 List Filter

```limn
# Filter: keep elements satisfying predicate
pro fil |
var | whe gro-in | whe pred | whe gro-out |
cns |
  al x |
    (x amo gro-in | pred cau x eff tru) | x amo gro-out |
    (x amo gro-in | pred cau x eff fal) | nu x amo gro-out |
    (nu x amo gro-in) | nu x amo gro-out
```

### 8.4 Sorting (Declarative)

```limn
# Sort: output is ordered permutation of input
pro sor |
var | whe gro-in | whe gro-out |
cns |
  # Same elements (permutation)
  al x | x amo gro-in eq x amo gro-out |
  # Ordered
  al i |
    (i joi 1) par gro-out |
    (par i gro-out) mi (par (i joi 1) gro-out) pa
    (par i gro-out) sa (par (i joi 1) gro-out)
```

---

## 9. Type System

### 9.1 Type Constraints

Types are expressed using `ve` (intensifier = type narrowing):

```limn
x ve one    # x is an integer
y ve flo    # y is a float
s ve wor    # s is a string
g ve gro    # g is a group/list
```

### 9.2 Type Inference

Types are inferred from constraints:

```limn
x joi y sa z    # x, y, z inferred as numeric
x amo arr       # x inferred as element type of arr
```

### 9.3 Generic Types

```limn
# Generic identity function
pro id |
var | whe a | whe eff |
cns | eff sa a
# Works for any type
```

---

## 10. Semantics in Limn

### 10.1 The Metacircular Definition

The interpreter itself can be expressed in Limn-PL:

```limn
# The eval function
pro eva |
var | whe ter | whe env | whe val |
cns |
  # Variable evaluation
  (ter ve whe | loo env ter eff val) pa
  # Literal evaluation
  (ter ve lit | val sa ter) pa
  # Compound evaluation
  (ter ve com |
   eva cau ter.lef env eff lv |
   eva cau ter.rig env eff rv |
   app ter.op lv rv eff val)
```

### 10.2 Constraint Satisfaction

```limn
# The satisfy function
pro sat |
var | whe cns | whe env | whe eff |
cns |
  eva cau cns.lef env eff lv |
  eva cau cns.rig env eff rv |
  che cns.rel lv rv eff eff
```

### 10.3 The Full Interpreter

```limn
# Run a program
pro run |
var | whe pro | whe inp | whe out |
cns |
  inp ins out |  # input is subset of output
  al c | c amo pro.cns | sat cau c out eff tru
```

---

## 11. Comparison to programming-v1.md

| Feature | programming-v1.md | limn-pl-v1.md |
|---------|-------------------|----------------|
| Syntax | English-like keywords | Pure Limn vocabulary |
| Structure | `program:` blocks | Limn sentences with `|` |
| Variables | `variables:` section | `whe` marker |
| Constraints | `constraints:` section | Inline with vocabulary |
| Operators | `plus`, `minus`, etc. | `joi`, `cut`, etc. |
| Key mechanism | Separate input | Key = Limn sentence |
| Bootstrappable | Requires parser | IS Limn |

---

## 12. Open Questions for Linguist

### 12.1 Recursion Expressibility

**Question:** Can recursion be fully expressed without explicit function call syntax?

Current approach uses `cau` (cause) for function invocation:
```limn
fac cau n eff r  # factorial(n) = r
```

This introduces an asymmetry - `cau` behaves differently from other constraint words.

**Potential Issue:** May need a formal mechanism for recursive constraint definitions.

### 12.2 Quantifier Scope

**Question:** How do `al` (all) and `ex` (exists) interact with Limn's commutativity?

```limn
al x | x ma 0 | x amo gro
# vs
x amo gro | al x | x ma 0
```

Should these be equivalent? Classical logic says no (quantifier scope matters).

### 12.3 Negation-as-Failure

**Question:** Does `nu` (negation) support closed-world assumption?

```limn
nu x amo gro  # x is NOT in group
```

This requires knowing all elements of `gro`. Needs clarification on semantics.

---

## 13. Implementation Notes

### 13.1 Execution Strategy

1. **Parse** Limn sentences into constraint AST
2. **Collect** all constraints from program
3. **Apply** key bindings as initial environment
4. **Propagate** constraints iteratively
5. **Search** if propagation insufficient
6. **Return** solution environment

### 13.2 Host Interpreter Interface

Limn-PL programs can be executed by the Python interpreter in `src/linga_interpreter.py` by:
1. Translating Limn vocabulary to Python constraint objects
2. Using the existing solver

### 13.3 Pure Limn Execution

Alternatively, execute via metacircular interpreter if Limn itself has a runtime.

---

## Appendix A: Complete Vocabulary Reference

| Word | Category | Limn-PL Meaning |
|------|----------|------------------|
| `pro` | Structure | Program definition |
| `var` | Structure | Variable section |
| `cns` | Structure | Constraint section |
| `whe` | Variable | Unknown/variable |
| `sa` | Relation | Equals |
| `nu` | Operator | Negation |
| `ma` | Comparison | Greater than |
| `mi` | Comparison | Less than |
| `eq` | Comparison | Equal to |
| `joi` | Arithmetic | Addition |
| `cut` | Arithmetic | Subtraction |
| `exp` | Arithmetic | Multiplication |
| `con` | Arithmetic | Division |
| `pow` | Arithmetic | Exponentiation |
| `one` | Type | Integer |
| `flo` | Type | Float |
| `wor` | Type | String |
| `tru` | Value | True |
| `fal` | Value | False |
| `gro` | Type | List/array |
| `hol` | Value | Null/none |
| `if` | Control | Conditional |
| `the` | Control | Then |
| `oth` | Control | Else |
| `cau` | Function | Call |
| `eff` | Function | Result |
| `amo` | Collection | Element of |
| `ins` | Collection | Contains |
| `par` | Collection | Index/part |
| `who` | Collection | Size |
| `al` | Quantifier | For all |
| `ex` | Quantifier | Exists |
| `pa` | Logic | Or/parallel |
| `ve` | Type | Type constraint |
| `cyc` | Control | Recursion |

---

**END OF SPECIFICATION**
