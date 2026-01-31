# Programming in Limn: Input as Key

**Status:** Exploratory (v0)
**Premise:** Order-independent programming via constraint satisfaction

---

## Abstract

Limn's commutative semantics creates a fundamental limitation: asymmetric relations cannot be encoded in sentences alone. The key mechanism resolves this by providing external context. This document explores a radical implication: **programs are ambiguous constraint sets, and input provides the asymmetry**.

This is not a bug. It is a programming paradigm.

---

## 1. Input as Key

### 1.1 The Core Insight

From the commutative semantics analysis (Solvik):

> "For any sentence with symmetric content, and any asymmetric proposition P expressible under key K, there exists an alternative key K' such that the sentence under K' expresses a distinct asymmetric proposition P'."

In natural language, this is a limitation. In programming, it is a feature.

**The Reframing:**

| Natural Language | Programming |
|------------------|-------------|
| Sentence | Program |
| Key | Input |
| Meaning | Behavior |
| Ambiguity | Polymorphism |

A Limn program specifies *what* must be true. The input specifies *which* instantiation of that truth to compute.

### 1.2 How Input Collapses Ambiguity

Consider the constraint surface model. A program P defines a meaning region M(P) - all behaviors consistent with P's constraints. Without input, M(P) has positive volume: multiple valid behaviors exist.

Input I provides additional constraints:

```
M(P|I) = M(P) ∩ C(I)
```

The intersection collapses to a specific behavior (ideally a point, or a small region of equivalent behaviors).

**Information-theoretic view:**
- Program provides: structural constraints (relations between values)
- Input provides: specific values + direction
- Output is: the unique (or unique-up-to-equivalence) point satisfying both

### 1.3 Analogy to Prolog

Prolog programs are sets of clauses (order-independent at the semantic level):

```prolog
parent(tom, mary).
parent(mary, john).
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
```

The **query** provides direction:
- `?- grandparent(tom, Who).` -> Who = john
- `?- grandparent(Who, john).` -> Who = tom

Same program, different queries, different execution paths. The query is the key.

**Critical observation:** Prolog clauses are symmetric at the collection level (their order doesn't matter semantically). But argument positions within predicates are asymmetric. Limn goes further: even within "predicates," there is no argument order. The input must supply all asymmetry.

### 1.4 Analogy to SQL

SQL queries are constraint specifications:

```sql
SELECT * FROM employees
WHERE department = 'Engineering'
AND salary > 100000
```

The **data** provides instantiation:
- Same query, different database -> different results
- Query specifies shape; data fills content

**The table schema is part of the key.** Column names like `department` and `salary` are not in the query's commutative content - they are named references that the data provides meaning to.

### 1.5 Analogy to Types

In type theory, a type is a constraint on values:

```
filter : (a -> Bool) -> [a] -> [a]
```

The type specifies: "given a predicate and a list, produce a list." It does not specify *how*. Multiple implementations satisfy this type.

**The implementation is a key** that collapses the type to a specific function.

In Limn programming:
- Program = type (constraint specification)
- Input = implementation selector (which satisfying behavior to manifest)

---

## 2. Order-Independent Programs

### 2.1 Statements as Constraints

In imperative programming, statements are instructions:
```
x = 5
y = x + 3
z = y * 2
```

Order matters. `y = x + 3` before `x = 5` is an error.

In Limn programming, statements are constraints:
```
x equals 5
y equals x plus 3
z equals y times 2
```

As a set: {x=5, y=x+3, z=y*2}

**Order is irrelevant.** The constraints define a system of equations. The solution is the unique assignment satisfying all constraints (if one exists).

### 2.2 Program as Intersection

Let each statement S_i define a constraint region C_i in the space of all possible program states.

```
C_1 = { states where x = 5 }
C_2 = { states where y = x + 3 }
C_3 = { states where z = y * 2 }
```

The program's meaning is:

```
M(Program) = C_1 ∩ C_2 ∩ C_3
```

This intersection contains exactly one point: {x=5, y=8, z=16}.

**No execution order was specified.** The constraints simultaneously hold. An interpreter could solve them in any order, or via constraint propagation, or by algebraic substitution.

### 2.3 Execution as Constraint Satisfaction

**Execution is not sequential stepping.** Execution is finding a point in the constraint intersection.

Given:
- Program P (constraint set)
- Input I (additional constraints + values)

Execution means:
1. Compute M(P) ∩ C(I)
2. If empty: runtime error (unsatisfiable)
3. If non-empty: return any point in the intersection (or the unique point if singular)

**Multiple satisfying points = nondeterminism or choice.** The program may specify constraints loose enough that multiple valid outputs exist. The runtime can:
- Return any one (nondeterministic)
- Return all (set-valued semantics)
- Fail if uniqueness was expected

### 2.4 Example: Constraint-Based Arithmetic

Consider a "program" in Limn-like pseudosyntax:

```
{ a b c | a plus b equals c }
```

This constrains three values: a + b = c.

**With input {a=3, b=5}:**
Constraints: {a+b=c, a=3, b=5}
Solution: c=8

**With input {a=3, c=10}:**
Constraints: {a+b=c, a=3, c=10}
Solution: b=7

**With input {b=5, c=12}:**
Constraints: {a+b=c, b=5, c=12}
Solution: a=7

**Same program, different inputs, different "directions" of computation.** The program is symmetric in a, b, c (modulo the arithmetic constraint). The input provides asymmetry: which are knowns, which are unknowns.

---

## 3. Asymmetric Relations via Input

### 3.1 "A before B" from Input Order

Limn cannot express "A happens before B" in the program itself. But:

**Input as a stream:** If input arrives as a sequence [A, B, C, ...], the order is in the input, not the program.

Program:
```
{ event1 event2 | event1 precedes event2 }
```

This constrains: there exist two events, one precedes the other.

Input stream: [login_event, purchase_event]
Binding: event1 = login_event, event2 = purchase_event
Interpretation: login precedes purchase.

**The input's sequential structure provides the "before" relation.** The program merely names slots for that relation to fill.

### 3.2 "X causes Y" from Data Dependencies

Consider:
```
{ x y | change_in x associates_with change_in y }
```

This is symmetric. Does x cause y, or y cause x?

**Input as time-series data:**
```
Input: [(t=1, x=5, y=10), (t=2, x=7, y=14), (t=3, x=6, y=12)]
```

The temporal structure in the data reveals: changes in x precede proportional changes in y. The program didn't specify causation direction; the data did.

**Causal inference from data, not code.** The program specifies what to look for (association between changes). The data's structure reveals direction.

### 3.3 Agent and Patient from Context

The commutative semantics document shows:
```
{ bite dog man }
```
Cannot distinguish BITE(dog, man) from BITE(man, dog).

**Solution via input context:**

Input (as key/context):
```
{ typical_scenario: dogs_bite_things }
```

Program + Input -> BITE(dog, man)

Alternative input:
```
{ news_headline: unusual_event_reported }
```

Program + Input -> BITE(man, dog) (the newsworthy interpretation)

**The program describes the event space. The input selects the point within it.**

### 3.4 Program Specifies WHAT, Input Specifies DIRECTION

| Aspect | Where Encoded |
|--------|---------------|
| What entities exist | Program vocabulary |
| What relations hold | Program constraints |
| Which entity fills which role | Input (key) |
| What values are bound | Input (data) |
| What order events occur | Input (sequence structure) |
| What causes what | Input (temporal/causal structure) |

The program is a **shape**. The input **orients and fills** that shape.

---

## 4. Examples

### 4.1 A Simple Filtering Program

**Natural language specification:**
"Keep items where the predicate holds."

**Limn program (pseudosyntax):**
```
{ item collection result predicate |
  item element_of collection
  predicate holds_for item implies item element_of result
  nu predicate holds_for item implies nu item element_of result }
```

**As constraints:**
- result contains exactly items from collection satisfying predicate

**Input 1:**
```
collection = [1, 2, 3, 4, 5]
predicate = is_even
```

Execution: {is_even holds_for 2, is_even holds_for 4}
Result: [2, 4]

**Input 2:**
```
collection = ["apple", "banana", "apricot"]
predicate = starts_with_a
```

Result: ["apple", "apricot"]

**Same program, polymorphic over collection type and predicate.**

### 4.2 A Simple Transformation Program

**Natural language specification:**
"Apply function to each element."

**Limn program:**
```
{ input_item output_item transform |
  for_each input_item in source:
    output_item equals transform applied_to input_item }
```

Wait - "for_each" implies order. Let's be more Limn:

```
{ pair source target transform |
  pair has first from source
  pair has second in target
  second equals transform of first }
```

**As pure constraint:**
Every (source_element, target_element) pair satisfies: target = transform(source).

**Input 1:**
```
source = [1, 2, 3]
transform = double
```

Result: target = [2, 4, 6]

**Input 2:**
```
target = [10, 20, 30]
transform = double
```

Result: source = [5, 10, 15] (running the transform "backwards")

**Bidirectional computation emerges from constraint satisfaction.** The program doesn't specify "map forward." The input determines direction by what is known vs. unknown.

### 4.3 Different Inputs, Different Behaviors

**A relational program:**
```
{ parent child grandparent |
  parent of parent equals grandparent
  -- i.e., grandparent(G, C) iff exists P: parent(P, C) and parent(G, P) }
```

**Input 1: Query grandchildren**
```
grandparent = "Alice"
database = {parent("Alice", "Bob"), parent("Bob", "Carol"), parent("Bob", "Dave")}
```

Result: child in {Carol, Dave}

**Input 2: Query grandparents**
```
child = "Carol"
database = {parent("Alice", "Bob"), parent("Bob", "Carol")}
```

Result: grandparent = Alice

**Input 3: Query intermediate generation**
```
grandparent = "Alice"
child = "Carol"
database = {parent("Alice", "Bob"), parent("Bob", "Carol")}
```

Result: parent = Bob (the intermediate node)

**The same 3-line program answers three different questions** depending on what the input specifies as known vs. unknown.

---

## 5. Connection to Existing Paradigms

### 5.1 Dataflow Programming

In dataflow, computation is a graph where data flows along edges.

**Connection:**
- Limn constraints define the graph implicitly
- Input provides values at sources
- Execution propagates constraints to sinks

**Difference:**
- Dataflow typically has directed edges (data flows one way)
- Limn constraints are symmetric; direction emerges from input

**Limn is bidirectional dataflow.** The constraint graph can be traversed in any direction the input determines.

### 5.2 Logic Programming (Prolog, Datalog)

Prolog programs are Horn clauses; execution is resolution.

**Connection:**
- Both are declarative (specify what, not how)
- Both support multiple query modes
- Both derive direction from query, not program

**Difference:**
- Prolog has ordered argument positions
- Limn has no argument positions; roles come from input
- Prolog execution depends on clause order (for efficiency, not semantics)
- Limn is more radically order-independent

**Limn is "Prolog without argument positions."** Relations are specified, but the mapping of entities to roles is input-dependent.

### 5.3 Constraint Satisfaction Problems (CSP)

CSP: given variables, domains, and constraints, find assignment satisfying all constraints.

**Connection:**
- Limn programs ARE constraint satisfaction problems
- Execution IS finding satisfying assignments
- Input provides partial assignments (knowns) and domain restrictions

**Difference:**
- CSP typically has fixed variable roles
- Limn allows input to determine which variables are knowns vs. unknowns
- CSP solvers are the implementation strategy for Limn execution

**Limn programs are CSPs where the "question" is part of the input.**

### 5.4 Functional Reactive Programming (FRP)

FRP: behaviors (time-varying values) and events, composed declaratively.

**Connection:**
- Both avoid explicit control flow
- Both specify relationships that hold continuously
- Both can be seen as constraint systems over time

**Difference:**
- FRP has explicit time as a dimension
- Limn uses input sequence as implicit time
- FRP behaviors are functions of time; Limn constraints are atemporal (input provides temporal structure)

**Limn is FRP where time enters through the input stream.**

### 5.5 Relational Programming

Languages like miniKanren treat all functions as relations.

**Connection:**
- Both allow "running programs backwards"
- Both support multiple solutions
- Both are fundamentally about relations, not functions

**Difference:**
- Relational programming still has argument positions
- Limn has no argument positions; even the relation's roles are input-determined

**Limn is radically relational:** not just functions-as-relations, but relations-without-role-assignments.

### 5.6 Summary Table

| Paradigm | Order-Independent Code? | Bidirectional? | Role Assignment |
|----------|------------------------|----------------|-----------------|
| Imperative | No | No | Hard-coded |
| Functional | Partially (pure) | No | In function signature |
| Dataflow | Graph structure yes | Usually no | Edge directions |
| Logic (Prolog) | Clauses yes | Yes (modes) | Argument positions |
| CSP | Yes | N/A (search) | Variable names |
| FRP | Yes | Partially | Signal types |
| Relational | Yes | Yes | Argument positions |
| **Limn** | **Fully** | **Fully** | **Input-determined** |

---

## 6. Implementation Considerations

### 6.1 Execution Strategy

A Limn interpreter must:

1. **Parse** the program into a constraint set
2. **Parse** the input into knowns (bound variables) and the question (what to compute)
3. **Combine** program constraints with input constraints
4. **Solve** the constraint system (using CSP/SMT techniques)
5. **Return** the satisfying assignment(s)

### 6.2 Constraint Propagation

For efficiency, use constraint propagation:
- When input binds a variable, propagate to all constraints mentioning it
- Reduce constraint regions incrementally
- Detect unsatisfiability early

### 6.3 Handling Ambiguity

When M(P) ∩ C(I) contains multiple points:

**Option A: Nondeterministic choice**
Return one satisfying point. Useful for search problems.

**Option B: Set-valued return**
Return all satisfying points. Useful for queries.

**Option C: Require uniqueness**
Fail if not unique. Useful for deterministic computation.

The input could specify which semantics is desired.

### 6.4 Types as Constraints

Type checking is constraint checking:
- Each type is a constraint region
- Well-typed programs have non-empty constraint intersections
- Type inference is constraint solving

Limn types are naturally inferred: the constraints imply types.

---

## 7. Open Questions

### 7.1 Efficiency

Can constraint satisfaction be efficient enough for general programming?
- CSP is NP-hard in general
- But many practical cases are tractable (e.g., linear constraints)
- What program structures guarantee polynomial execution?

### 7.2 Expressiveness

Are there computations that require asymmetry in the program itself?
- Recursion seems to require "base case before recursive case"
- But recursion can be expressed as a constraint: f(n) = ... f(n-1) ...
- Is this always sufficient?

### 7.3 Debugging

How do you debug a program with no control flow?
- Trace which constraints propagated
- Identify which constraint led to unsatisfiability
- Visualize the constraint surface shrinking as input is processed

### 7.4 Modularity

How do you compose order-independent programs?
- Constraint union: P1 ∪ P2 means "satisfy both"
- But namespace collision is a problem
- Need module system for constraint isolation

### 7.5 Side Effects

How do you model I/O in a constraint world?
- I/O is inherently ordered (asymmetric)
- Perhaps: I/O events are in the input stream, not the program
- Program specifies what to output given input; actual sequencing is runtime's job

---

## 8. Philosophical Implications

### 8.1 Programs as Questions

A Limn program is not a sequence of instructions. It is a **question about relationships**.

The input provides the **context** in which the question is asked.
The output is the **answer** in that context.

Different contexts, different answers. Same question.

### 8.2 Meaning Without Direction

In natural language, meaning requires direction (who did what to whom). Limn sentences lack this; keys provide it.

In programming, computation requires direction (what is input, what is output). Limn programs lack this; input provides it.

**Radical claim:** Meaning and computation are the same thing. Both require the collapse of symmetric structure into asymmetric instantiation.

### 8.3 The Key as User

In Limn communication, the key is held by the decoder.

In Limn programming, the input is provided by the user.

**The user is the key.** The user's question, data, and context collapse the program's possibilities into specific behavior.

---

## 9. Conclusion

Limn's inability to encode asymmetric relations is not a limitation for programming - it is a paradigm. By separating **what** (program) from **which direction** (input), we get:

1. **Bidirectional computation:** Same program runs "forwards" or "backwards"
2. **Radical polymorphism:** Same program works on any types satisfying its constraints
3. **Order independence:** No sequence bugs, no race conditions (in the logic)
4. **Queryable programs:** Ask different questions of the same code

The cost is the key dependency: inputs must provide more information than in traditional programming. But this cost buys something traditional programming lacks: a clean separation between structure (program) and instantiation (input).

**Programs are shapes. Inputs are orientations. Outputs are points.**

---

## Appendix A: Comparison with Traditional Paradigms

### A.1 Same Problem, Different Paradigms

**Problem:** Given a list and a target sum, find two elements that sum to target.

**Imperative (Python):**
```python
def two_sum(nums, target):
    for i in range(len(nums)):
        for j in range(i+1, len(nums)):
            if nums[i] + nums[j] == target:
                return (i, j)
    return None
```

Order everywhere: loop order, return early, None for failure.

**Functional (Haskell):**
```haskell
twoSum nums target =
    [(i,j) | i <- [0..n-1], j <- [i+1..n-1], nums!!i + nums!!j == target]
    where n = length nums
```

Less explicit order, but still: i before j, list comprehension order.

**Logic (Prolog):**
```prolog
two_sum(Nums, Target, I, J) :-
    nth0(I, Nums, X),
    nth0(J, Nums, Y),
    I < J,
    X + Y =:= Target.
```

More symmetric: I and J are both "outputs" (unified by query). But I < J is asymmetric.

**Limn (pseudosyntax):**
```
{ nums target i j |
  i index_of nums
  j index_of nums
  i different_from j
  (value_at i nums) plus (value_at j nums) equals target }
```

No ordering between i and j. "different_from" is symmetric. The query determines what is known:
- Input: nums=[1,2,3,4], target=5 -> Find i, j
- Input: nums=[1,2,3,4], i=0, j=3 -> Find target (what do these sum to?)
- Input: target=5, i=0, j=3 -> Find nums where position 0 + position 3 = 5 (underdetermined)

### A.2 The Constraint Surface View

The program defines:
```
C = { (nums, target, i, j) | i,j are valid indices, i != j, nums[i] + nums[j] = target }
```

This is a surface in (nums * target * i * j) space.

Input constrains some coordinates. Execution finds the remaining coordinates.

---

## Appendix B: Toward a Limn Programming Syntax

### B.1 Core Constructs

**Variable introduction:**
```
{ x y z | ... }
```
Introduces variables x, y, z in the constraint scope.

**Constraint:**
```
x relation y
```
Asserts that relation holds between x and y.

**Composition:**
Multiple constraints in a block are intersected (all must hold).

### B.2 Built-in Relations

**Equality:** `equals`
**Arithmetic:** `plus`, `minus`, `times`, `divided_by`
**Comparison:** `greater_than`, `less_than` (or just use equality with arithmetic)
**Collection:** `element_of`, `subset_of`, `size_of`
**Logic:** `implies`, `equivalent_to`

### B.3 Negation

```
nu constraint
```

The complement: all assignments NOT satisfying the constraint.

### B.4 Example Programs

**Identity (any value):**
```
{ x }
```
No constraints. Any x satisfies. Input must bind x.

**Constant:**
```
{ x | x equals 42 }
```
x is fixed regardless of input.

**Addition:**
```
{ a b c | a plus b equals c }
```
Three-way: knows any two, computes third.

**Filtering:**
```
{ item source result predicate |
  item element_of source
  predicate of item implies item element_of result
  nu predicate of item implies nu item element_of result }
```

**Sorting (constraint version):**
```
{ source result |
  permutation source result
  ordered result }
```

Specifies: result is an ordered permutation of source. Execution finds it.

---

*This document is exploratory. Syntax and semantics are provisional. The core insight - input as key for asymmetry - is the contribution.*
