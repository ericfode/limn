# Metacircular Limn Interpreter

**Status:** Specification
**Purpose:** A Limn interpreter written in Limn - proving the language is expressive enough to describe its own semantics.

---

## 1. Overview

A metacircular interpreter is an interpreter for a language written in that same language. This demonstrates:
1. The language is expressive enough to describe computation
2. The semantics are self-consistent
3. The core concepts are minimal and orthogonal

For Limn, this is especially interesting because:
- Interpretation IS constraint satisfaction
- Programs ARE constraint sets
- The interpreter describes how constraints compose and resolve

---

## 2. Core Data Structures

### 2.1 Representing Terms

Terms are the atomic units - variables, literals, and compound expressions.

```
program term_representation:
  variables: term term_type term_value term_name

  constraints:
    # A term is either a variable, literal, or compound
    term has term_type
    term_type element_of {variable, literal, compound}

    # Variables have names
    (term_type equals variable) implies (term has term_name)

    # Literals have values
    (term_type equals literal) implies (term has term_value)

    # Compounds have operator and operands
    (term_type equals compound) implies (term has operator and term has operands)
```

### 2.2 Representing Constraints

A constraint relates terms through a relation.

```
program constraint_representation:
  variables: constraint left_term right_term relation

  constraints:
    constraint has left_term
    constraint has right_term
    constraint has relation

    relation element_of {
      equals, nu_equals,
      greater_than, less_than, at_least, at_most,
      element_of, subset_of,
      implies, equivalent_to
    }
```

### 2.3 Representing Programs

A program is a set of constraints plus variable declarations.

```
program program_representation:
  variables: prog prog_name prog_vars prog_constraints

  constraints:
    prog has prog_name
    prog has prog_vars
    prog has prog_constraints

    # prog_vars is a set of variable names
    forall v: v element_of prog_vars implies v is_type variable_name

    # prog_constraints is a set of constraints
    forall c: c element_of prog_constraints implies c is_type constraint
```

---

## 3. Environment and Bindings

### 3.1 Bindings

A binding maps a variable name to a value.

```
program binding:
  variables: binding var_name bound_value

  constraints:
    binding has var_name
    binding has bound_value
    binding maps var_name to bound_value
```

### 3.2 Environment

An environment is a set of bindings.

```
program environment:
  variables: env bindings

  constraints:
    env has bindings
    forall b: b element_of bindings implies b is_type binding

    # No duplicate bindings for same variable
    forall b1 b2:
      (b1 element_of bindings and b2 element_of bindings and
       b1.var_name equals b2.var_name)
      implies b1 equals b2
```

### 3.3 Environment Lookup

```
program lookup:
  variables: env var_name result found

  constraints:
    # If binding exists, return its value
    (exists b: b element_of env.bindings and b.var_name equals var_name)
    implies (
      found equals true and
      exists b: b element_of env.bindings and
               b.var_name equals var_name and
               result equals b.bound_value
    )

    # If no binding, not found
    (nu exists b: b element_of env.bindings and b.var_name equals var_name)
    implies found equals false
```

### 3.4 Environment Extension

```
program extend_env:
  variables: old_env new_binding new_env

  constraints:
    new_env.bindings equals old_env.bindings union {new_binding}
```

---

## 4. Term Evaluation

### 4.1 Evaluate Variable

```
program eval_variable:
  variables: term env result

  constraints:
    term.term_type equals variable
    lookup(env, term.term_name, result, true)
    # Precondition: variable must be bound
```

### 4.2 Evaluate Literal

```
program eval_literal:
  variables: term env result

  constraints:
    term.term_type equals literal
    result equals term.term_value
    # Literals evaluate to themselves; env unused but present for uniformity
```

### 4.3 Evaluate Compound

```
program eval_compound:
  variables: term env result

  constraints:
    term.term_type equals compound

    # Evaluate operands recursively
    forall i:
      i index_of term.operands implies
      exists eval_result:
        eval_term(term.operands[i], env, eval_result)
        evaluated_operands[i] equals eval_result

    # Apply operator to evaluated operands
    apply_operator(term.operator, evaluated_operands, result)
```

### 4.4 Unified Term Evaluation

```
program eval_term:
  variables: term env result

  constraints:
    # Dispatch based on term type
    (term.term_type equals variable) implies eval_variable(term, env, result)
    (term.term_type equals literal) implies eval_literal(term, env, result)
    (term.term_type equals compound) implies eval_compound(term, env, result)
```

---

## 5. Constraint Satisfaction

### 5.1 Satisfy Single Constraint

```
program satisfy_constraint:
  variables: constraint env satisfied new_env

  constraints:
    # Evaluate both sides
    eval_term(constraint.left_term, env, left_value)
    eval_term(constraint.right_term, env, right_value)

    # Check relation
    (constraint.relation equals equals) implies (
      (left_value equals right_value) implies satisfied equals true
      (nu left_value equals right_value) implies satisfied equals false
    )

    (constraint.relation equals nu_equals) implies (
      (nu left_value equals right_value) implies satisfied equals true
      (left_value equals right_value) implies satisfied equals false
    )

    (constraint.relation equals greater_than) implies (
      (left_value greater_than right_value) implies satisfied equals true
      (nu left_value greater_than right_value) implies satisfied equals false
    )

    # ... similar for other relations

    # Environment unchanged for checking (solving extends it)
    new_env equals env
```

### 5.2 Satisfy Constraint Set (Intersection)

The core of Limn: multiple constraints must ALL be satisfied.

```
program satisfy_all:
  variables: constraints env satisfied final_env

  constraints:
    # All constraints must be satisfied
    satisfied equals true equivalent_to
      forall c: c element_of constraints implies
        exists c_satisfied c_env:
          satisfy_constraint(c, env, c_satisfied, c_env) and
          c_satisfied equals true

    # Final environment is consistent with all constraints
    forall c: c element_of constraints implies
      satisfy_constraint(c, final_env, true, final_env)
```

---

## 6. The Solver

### 6.1 Solve for Unknown

Given partial bindings, find values that satisfy all constraints.

```
program solve:
  variables: prog input_env unknowns solution_env satisfiable

  constraints:
    # unknowns = program variables not in input_env
    forall v:
      v element_of prog.prog_vars implies (
        (exists b: b element_of input_env.bindings and b.var_name equals v)
        or v element_of unknowns
      )

    # solution_env extends input_env with bindings for unknowns
    input_env.bindings subset_of solution_env.bindings

    forall v: v element_of unknowns implies
      exists b: b element_of solution_env.bindings and b.var_name equals v

    # solution_env satisfies all constraints
    satisfiable equals true equivalent_to
      satisfy_all(prog.prog_constraints, solution_env, true, solution_env)
```

### 6.2 Enumerate Solutions

```
program enumerate_solutions:
  variables: prog input_env all_solutions

  constraints:
    forall sol:
      sol element_of all_solutions equivalent_to (
        solve(prog, input_env, _, sol, true)
      )
```

---

## 7. The Metacircular Interpreter

### 7.1 Run Program

```
program run:
  variables: prog input output_env

  constraints:
    # Convert input to environment
    input_to_env(input, input_env)

    # Solve constraints
    solve(prog, input_env, unknowns, output_env, satisfiable)

    # Report satisfiability
    satisfiable implies run_succeeded
    nu satisfiable implies run_failed
```

### 7.2 Full Interpreter

```
program limn_interpreter:
  # Meta-variables (about programs, not in programs)
  variables: source_program parsed_program input_bindings result_bindings

  constraints:
    # Parse source into program structure
    parse(source_program, parsed_program)

    # parsed_program is a valid program
    parsed_program is_type program

    # Run the program
    run(parsed_program, input_bindings, result_bindings)
```

---

## 8. Self-Interpretation Example

### 8.1 The Addition Program

```
# Source program (as data)
source: """
program addition:
  variables: a b c
  constraints:
    a plus b equals c
"""

input: {a: 3, b: 5}
```

### 8.2 Parsing (Conceptual)

```
parsed_program:
  prog_name: "addition"
  prog_vars: {a, b, c}
  prog_constraints: {
    constraint1: {
      left_term: {term_type: compound, operator: plus, operands: [a, b]}
      right_term: {term_type: variable, term_name: c}
      relation: equals
    }
  }
```

### 8.3 Execution Trace

```
# Step 1: Build input environment
input_env = {
  bindings: [
    {var_name: a, bound_value: 3},
    {var_name: b, bound_value: 5}
  ]
}

# Step 2: Identify unknowns
unknowns = {c}

# Step 3: Solve constraints
# Constraint: a plus b equals c
# Evaluate left: eval_compound({plus, [a, b]}, env)
#   eval_variable(a, env) = 3
#   eval_variable(b, env) = 5
#   apply_operator(plus, [3, 5]) = 8
# So left_value = 8
# Constraint becomes: 8 equals c
# Solve for c: c = 8

# Step 4: Result
solution_env = {
  bindings: [
    {var_name: a, bound_value: 3},
    {var_name: b, bound_value: 5},
    {var_name: c, bound_value: 8}
  ]
}
```

### 8.4 Bidirectional Execution

```
# Same program, different input
input: {a: 3, c: 10}

# Unknowns: {b}
# Constraint: a plus b equals c
# Evaluate: 3 plus b equals 10
# Solve: b = 10 - 3 = 7

solution_env.bindings includes {var_name: b, bound_value: 7}
```

---

## 9. Bootstrapping Considerations

### 9.1 The Bootstrap Paradox

To run this metacircular interpreter, we need a Limn interpreter. But if we only have this interpreter (written in Limn), we have a chicken-and-egg problem.

**Resolution:** The metacircular interpreter is a *specification*, not a bootstrap. To execute it:
1. First, implement a host interpreter (in Python, Rust, etc.)
2. The host interpreter can run the metacircular interpreter
3. The metacircular interpreter can then interpret Limn programs
4. This proves the semantics are consistent

### 9.2 Fixed Point

The metacircular interpreter should be a fixed point:
```
host_interpreter(metacircular_interpreter, metacircular_interpreter, input)
=
host_interpreter(metacircular_interpreter, some_program, input)
```

When the metacircular interpreter interprets itself interpreting a program, the result should be the same as directly interpreting that program.

### 9.3 Equivalence Theorem

```
theorem metacircular_equivalence:
  forall prog input:
    host_interp(prog, input) equals
    host_interp(metacircular_interp, {prog: prog, input: input})
```

This theorem states that running a program directly is equivalent to running it through the metacircular interpreter.

---

## 10. Order Independence in the Interpreter

### 10.1 Constraint Order Doesn't Matter

Notice that in `satisfy_all`, we use:
```
forall c: c element_of constraints implies ...
```

The constraints form a *set*. There is no ordering. The solver must find bindings that satisfy ALL constraints simultaneously, regardless of the order they appear in the source.

### 10.2 Statement Order = Constraint Set

When we write:
```
program foo:
  constraints:
    a equals 5
    b equals a plus 3
```

The order of lines is syntactic sugar. Semantically:
```
constraints = { (a equals 5), (b equals a plus 3) }
```

The interpreter treats them as an unordered set.

### 10.3 Solving is Intersection

The solver finds:
```
solution_env ∈ ⋂_{c ∈ constraints} valid_envs(c)
```

Where `valid_envs(c)` is the set of all environments satisfying constraint c.

This is constraint intersection - the same as Limn's natural language semantics!

---

## 11. Reflection Capabilities

### 11.1 Programs as Data

Because programs are constraint sets, and Limn can manipulate sets, Limn can manipulate programs.

```
program program_combiner:
  variables: prog1 prog2 combined

  constraints:
    combined.prog_vars equals prog1.prog_vars union prog2.prog_vars
    combined.prog_constraints equals prog1.prog_constraints union prog2.prog_constraints
```

### 11.2 Constraint Introspection

```
program constraint_counter:
  variables: prog count

  constraints:
    size_of(prog.prog_constraints) equals count
```

### 11.3 Self-Modification (Theoretical)

```
program self_extending:
  variables: self new_constraint extended_self

  constraints:
    extended_self.prog_constraints equals
      self.prog_constraints union {new_constraint}
```

---

## 12. Completeness and Limitations

### 12.1 What This Interpreter Can Do

- Evaluate arithmetic expressions
- Solve constraint systems with unique solutions
- Handle bidirectional computation
- Manage variable bindings and scoping
- Compose programs

### 12.2 What Requires Extensions

- **Infinite domains:** Current spec assumes finite constraint solving
- **Recursion:** Self-referential constraints need fixed-point semantics
- **I/O:** Input/output requires external interaction primitives
- **Negation:** Full negation-as-failure requires careful semantics

### 12.3 Gödel Limitations

Per incompleteness theorems, no sufficiently powerful language can:
- Decide all statements about itself
- Prove its own consistency

The metacircular interpreter inherits these limitations. Some self-referential programs will diverge or be undecidable.

---

## 13. Conclusion

This metacircular interpreter demonstrates that:

1. **Limn is Turing-complete** (can describe arbitrary computation)
2. **Semantics are self-consistent** (can describe its own meaning)
3. **Order-independence is fundamental** (interpreter uses set operations)
4. **Constraint intersection is universal** (both language and meta-language use it)

The interpreter serves as both:
- A formal specification of Limn semantics
- A proof of expressiveness
- A foundation for reflective programming

---

## Appendix: Compact Core

The essential metacircular loop in minimal form:

```
program eval:
  variables: term env value

  constraints:
    (term is_variable) implies lookup(env, term, value)
    (term is_literal) implies (value equals term)
    (term is_compound) implies (
      eval(term.left, env, left_val) and
      eval(term.right, env, right_val) and
      apply(term.op, left_val, right_val, value)
    )

program satisfy:
  variables: constraint env result

  constraints:
    eval(constraint.left, env, lv) and
    eval(constraint.right, env, rv) and
    check(constraint.relation, lv, rv, result)

program interpret:
  variables: program input output

  constraints:
    forall c: c element_of program.constraints implies
      satisfy(c, output, true)
    input subset_of output
```

This ~20 line core captures the essence of Limn interpretation.

---

**END OF METACIRCULAR SPECIFICATION**
