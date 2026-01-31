# Limn-PL Formal Grammar (BNF)

This document provides the formal grammar for the Limn-PL programming language.

---

## Lexical Rules

```
WORD       := [a-z][a-z0-9-]*
NUMBER     := [+-]?[0-9]+(\.[0-9]+)?
STRING     := '"' [^"]* '"'
PIPE       := '|'
```

## Production Rules

```bnf
program    := 'pro' WORD? sections

sections   := section (PIPE section)*

section    := var_section
            | cns_section

var_section  := 'var' (PIPE 'whe' WORD)*

cns_section  := 'cns' (PIPE constraint)*

constraint := simple_constraint
            | alternative
            | conditional

simple_constraint := expression relation expression
                   | func_call
                   | 'nu' constraint

alternative := constraint ('pa' constraint)+

conditional := 'if' constraint (PIPE 'the' constraint)? (PIPE 'oth' constraint)?

expression := atom
            | binary_expr
            | unary_expr
            | collection_expr

binary_expr := atom binary_op atom

binary_op  := 'joi' | 'cut' | 'exp' | 'con' | 'pow'

unary_expr := unary_op atom

unary_op   := 'nu'

collection_expr := 'par' atom atom      (* index access: par i arr *)
                 | 'who' atom           (* size: who arr *)
                 | 'fst' atom           (* first element: fst arr *)
                 | 'nxt' atom           (* tail/rest: nxt arr *)
                 | 'fin' atom           (* last element: fin arr *)
                 | 'gro' (PIPE atom)*   (* group literal *)

relation   := 'sa' | 'ma' | 'mi' | 'eq' | 'amo' | 'ins'

func_call  := WORD 'cau' atom* 'eff' atom

atom       := WORD           (* variable *)
            | NUMBER         (* numeric literal *)
            | STRING         (* string literal *)
            | 'tru'          (* boolean true *)
            | 'fal'          (* boolean false *)
            | 'hol'          (* null/hole *)
```

---

## Operator Precedence

From highest to lowest:

1. **Atoms**: literals, variables, collection expressions
2. **Binary arithmetic**: `joi`, `cut`, `exp`, `con`, `pow` (left-to-right)
3. **Relations**: `sa`, `ma`, `mi`, `eq`, `amo`, `ins`
4. **Negation**: `nu` (right-associative, applies to following constraint)
5. **Alternatives**: `pa` (lowest precedence, left-to-right)

---

## Associativity Rules

| Operator Type | Associativity |
|---------------|---------------|
| Arithmetic    | Left-to-right |
| Relations     | Non-associative (a sa b, not a sa b sa c) |
| `nu` (negation) | Right-to-left (prefix) |
| `pa` (alternatives) | Left-to-right |

---

## Parsing Examples

### Simple Constraint
```
a joi b sa c
```
Parse tree:
```
Constraint
├── left: BinaryOp(joi)
│         ├── left: Var(a)
│         └── right: Var(b)
├── relation: sa
└── right: Var(c)
```

### Negated Constraint
```
nu x sa 5
```
Parse tree:
```
Constraint
├── negated: true
├── left: Var(x)
├── relation: sa
└── right: Lit(5)
```

### Chained Arithmetic
```
a joi b cut c sa d
```
Parse: NOT VALID (binary ops take exactly 2 operands)
Use intermediate variables instead:
```
a joi b sa t1 | t1 cut c sa d
```

### Alternative
```
x sa 1 pa x sa 2 pa x sa 3
```
Parse tree:
```
Alternative
├── branch[0]: Constraint(x sa 1)
├── branch[1]: Constraint(x sa 2)
└── branch[2]: Constraint(x sa 3)
```

### Conditional
```
if x ma 0 | the y sa 1 | oth y sa 0
```
Parse tree:
```
CondBlock
├── condition: Constraint(x ma 0)
├── then_result: Constraint(y sa 1)
└── else_result: Constraint(y sa 0)
```

---

## Semantic Notes

1. **Constraint semantics**: All constraints must be satisfied simultaneously
2. **Order independence**: The order of constraints in `cns` does not affect semantics
3. **Bidirectional**: Arithmetic constraints can be solved for any unknown
4. **Closed-world**: Negation uses closed-world assumption (finite domain)

---

## Grammar Diagram

```
┌─────────────────────────────────────────────────────────────┐
│ PROGRAM                                                      │
├─────────────────────────────────────────────────────────────┤
│  pro [name] | var | whe v1 | whe v2 | cns | c1 | c2 | ...   │
└─────────────────────────────────────────────────────────────┘
                                  │
                                  ▼
┌─────────────────────────────────────────────────────────────┐
│ CONSTRAINT                                                   │
├─────────────────────────────────────────────────────────────┤
│  simple:      expr relation expr                             │
│  negated:     nu constraint                                  │
│  alternative: c1 pa c2 pa c3                                 │
│  conditional: if c | the c | oth c                          │
└─────────────────────────────────────────────────────────────┘
                                  │
                                  ▼
┌─────────────────────────────────────────────────────────────┐
│ EXPRESSION                                                   │
├─────────────────────────────────────────────────────────────┤
│  binary:      a op b (joi, cut, exp, con, pow)              │
│  collection:  par i arr | who arr | fst arr | nxt arr       │
│  atom:        var | number | string | tru | fal | hol       │
└─────────────────────────────────────────────────────────────┘
```

---

**END OF GRAMMAR**
