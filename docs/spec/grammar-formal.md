# Limn Formal Grammar Specification

**Version:** 1.0
**Date:** 2026-01-25
**Author:** Dr. Maren Solvik, Theoretical Linguist
**Status:** Formal Specification

---

## 1. Overview

This document provides a complete, formal specification of Limn grammar suitable for interpreter implementation. It consolidates and formalizes rules from grammar-v1.md and various rulings.

---

## 2. EBNF Grammar

### 2.1 Lexical Structure

```ebnf
(* Character sets *)
letter      = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j"
            | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t"
            | "u" | "v" | "w" | "x" | "y" | "z" ;
consonant   = "b" | "c" | "d" | "f" | "g" | "h" | "j" | "k" | "l" | "m"
            | "n" | "p" | "r" | "s" | "t" | "v" | "w" | "x" | "y" | "z" ;
vowel       = "a" | "e" | "i" | "o" | "u" ;

(* Word patterns — tri-letter is the core format *)
(* Two strata: CVC (pronounceable) and clusters (compact) *)
cv_word     = consonant , vowel ;
tri_word    = letter , letter , letter ;   (* any 3 lowercase letters *)
cvcc_word   = consonant , vowel , consonant , consonant ;
word        = cv_word | tri_word | cvcc_word ;

(* Operators *)
unary_op    = "nu" | "ve" | "so" | "te" | "we" ;
quantifier  = "al" | "ex" | "on" ;
reference   = "yo" | "an" | "sa" ;
comparator  = "mi" | "ma" | "eq" ;

(* Punctuation *)
scope_mark  = "|" ;
group_open  = "(" ;
group_close = ")" ;

(* Arithmetic (if used) *)
arith_op    = "+" | "-" | "*" | "/" | ">" | "<" ;

(* Whitespace *)
ws          = " " | "\t" | "\n" ;
```

### 2.2 Syntactic Structure

```ebnf
(* Top level *)
discourse   = sentence , { "." , sentence } ;
sentence    = scope_expr ;

(* Scope expressions *)
scope_expr  = topic_comment | flat_expr ;
topic_comment = flat_expr , "|" , scope_expr ;
flat_expr   = term , { term } ;

(* Terms *)
term        = modified_term | base_term ;
modified_term = unary_op , term ;
base_term   = grouped | word | reference | quantifier | number ;

(* Grouping *)
grouped     = "(" , scope_expr , ")" ;

(* Numbers (optional) *)
number      = digit , { digit } ;
digit       = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
```

### 2.3 Operator Grammar (with arithmetic)

```ebnf
(* Full expression with arithmetic *)
expression  = comparison_expr ;
comparison_expr = additive_expr , [ comparator , additive_expr ] ;
additive_expr = multiplicative_expr , { ("+" | "-") , multiplicative_expr } ;
multiplicative_expr = unary_expr , { ("*" | "/") , unary_expr } ;
unary_expr  = { unary_op } , primary_expr ;
primary_expr = grouped | word | number ;
```

---

## 3. Precedence and Associativity

### 3.1 Operator Precedence (Highest to Lowest)

| Level | Operators | Description | Associativity |
|-------|-----------|-------------|---------------|
| 1 | `nu`, `ve`, `so`, `te`, `we` | Unary modifiers | Right-to-left |
| 2 | `*`, `/` | Multiplication, division | Left-to-right |
| 3 | `+`, `-` | Addition, subtraction | Left-to-right |
| 4 | `mi`, `ma`, `eq` | Comparison | Left-to-right |
| 5 | (space) | Intersection (implicit) | Commutative |
| 5.5 | `→` | Sequence/causality | Left-to-right |
| 6 | `|` | Scope/topic-comment | Left-to-right |

### 3.2 Examples

```
nu ve bri = nu(ve(bri))                    -- Unary right-to-left
3 + 4 * 2 = 3 + (4 * 2) = 11               -- Arithmetic precedence
x mi 0 + 1 = x mi (0 + 1) = x < 1          -- Comparison lower than arithmetic
lux bri lif = (lux ∩ bri ∩ lif)           -- Intersection commutative
lux bri | lif = topic(lux ∩ bri), comment(lif) -- Scope lowest
```

### 3.3 Operator Chaining (Detailed)

Chained unary operators compose right-to-left. Order matters semantically.

| Expression | Parse | Meaning | Region Size |
|------------|-------|---------|-------------|
| `nu ve sol` | nu(ve(sol)) | not(very-solid) | Large |
| `ve nu sol` | ve(nu(sol)) | very(not-solid) | Small |
| `nu nu sol` | nu(nu(sol)) | not(not-solid) = solid | Same as `sol` |
| `ve ve sol` | ve(ve(sol)) | very(very-solid) | Very small |

**Semantic difference:**
- `nu ve sol` = everything except prototypical solids (includes marginal solids, all non-solids)
- `ve nu sol` = prototypical non-solids only (pure liquids, pure gases)

### 3.4 Sequence Operator (Causality)

The `→` operator expresses temporal sequence or causality, breaking commutativity intentionally.

| Expression | Parse | Meaning |
|------------|-------|---------|
| `A B` | `A ∩ B` | Intersection (simultaneous) |
| `A → B` | `A → B` | A leads to/causes B |
| `A → B → C` | `A → B → C` | Causal chain |

**Key difference:**
- `A B = B A` (intersection is commutative)
- `A → B ≠ B → A` (sequence is not commutative)

**Examples:**
```
pay cha ref     = payment ∩ chargeback ∩ refund (all states present)
pay → cha → ref = payment → chargeback → refund (causal sequence)

hot col         = lukewarm (liminal intersection)
hot → col       = cooling (hot leading to cold)

lif dea         = liminal zone between life/death
lif → dea       = dying (life leading to death)
```

**Use case:** Real-world systems requiring causality tracking (payments, state machines, workflows).

**Note:** Use `→` in Limn documents. For platforms that don't render Unicode arrows (e.g., Moltbook), use `tra` (transform) explicitly or `>` as ASCII fallback.

---

## 4. Formal Semantics

### 4.1 Semantic Domain

Let **S** be the semantic space (a complete lattice or topological space).

Each word **w** denotes a constraint region **C(w) ⊆ S**.

### 4.2 Denotation Function

The denotation function **⟦·⟧** maps syntactic structures to semantic values:

```
⟦w⟧ = C(w)                                         -- Word denotation
⟦A B⟧ = ⟦A⟧ ∩ ⟦B⟧                                  -- Intersection
⟦nu A⟧ = S \ ⟦A⟧                                   -- Complement
⟦ve A⟧ = core(⟦A⟧)                                 -- Contraction to prototype
⟦so A⟧ = expand(⟦A⟧)                               -- Expansion to periphery
⟦(A)⟧ = ⟦A⟧                                        -- Grouping
⟦A | B⟧ = (⟦A⟧, ⟦B⟧)                               -- Topic-comment pair
```

### 4.3 Core and Expand Functions

For intensification and weakening:

```
core(C) = {x ∈ C : prototypicality(x, C) ≥ θ_core}
expand(C) = {x ∈ S : similarity(x, C) ≥ θ_expand}
```

Where:
- **prototypicality(x, C)** measures how central x is to C
- **similarity(x, C)** measures distance to nearest point in C
- **θ_core** and **θ_expand** are thresholds

### 4.4 Reference Semantics

```
⟦yo A⟧ = (proximal, ⟦A⟧)      -- This A (discourse-near)
⟦an A⟧ = (distal, ⟦A⟧)        -- That A (discourse-far)
⟦sa A⟧ = (anaphoric, ⟦A⟧)     -- The aforementioned A
```

### 4.5 Quantifier Semantics

```
⟦al A⟧ = universal(⟦A⟧)       -- For all x in A
⟦ex A⟧ = existential(⟦A⟧)     -- For some x in A
⟦on A⟧ = singular(⟦A⟧)        -- For exactly one x in A
```

### 4.6 Comparison Semantics

For ordered types:

```
⟦x mi y⟧ = {v : v <_L y}      -- Less than (open)
⟦x ma y⟧ = {v : v >_L y}      -- Greater than (open)
⟦x eq y⟧ = {v : v =_L y}      -- Equal to
```

Negation creates closed boundaries:
```
⟦nu (x mi y)⟧ = {v : v ≥_L y} -- Greater than or equal (closed)
```

---

## 5. Well-Formedness Rules

### 5.1 Grammatical Sentences

A sentence is **well-formed** iff:
1. It conforms to the EBNF grammar
2. All words are in the vocabulary
3. Operators have compatible operands

### 5.2 Semantic Validity

A well-formed sentence is **semantically valid** iff:
1. Its denotation is non-empty (⟦σ⟧ ≠ ∅)
2. Or it's intentionally paradoxical (liminal semantics applies)

### 5.3 Vacuous Expressions

Standalone operators are grammatical with vacuous interpretation:
```
⟦nu⟧ = ⟦nu S⟧ = ∅           -- The void
⟦ve⟧ = ⟦ve S⟧ = core(S)     -- Pure essence
⟦te⟧ = ⟦te S⟧ = question(S) -- Universal question
```

---

## 6. Commutativity Rules

### 6.1 Commutative Operations

Intersection of content words is commutative:
```
⟦A B⟧ = ⟦B A⟧  for all content words A, B
```

### 6.2 Non-Commutative Operations

The following break commutativity:

**Unary operators** (operator must precede operand):
```
⟦nu A⟧ ≠ ⟦A nu⟧  (A nu is ill-formed or different)
```

**Scope boundaries** (topic-comment structure):
```
⟦A | B⟧ ≠ ⟦B | A⟧  (different topic/comment assignment)
```

**References** (scope over following):
```
⟦yo A B⟧ ≠ ⟦A yo B⟧  (yo scopes differently)
```

---

## 7. De Morgan Equivalences

Group negation follows De Morgan's laws:

### 7.1 Laws

```
⟦nu (A B)⟧ = S \ (⟦A⟧ ∩ ⟦B⟧) = (S \ ⟦A⟧) ∪ (S \ ⟦B⟧) = ⟦nu A | nu B⟧

⟦nu (A | B)⟧ = ⟦nu A nu B⟧  (if | is disjunction)
```

### 7.2 Practical Forms

| Expression | Equivalent | Result Region |
|------------|------------|---------------|
| `nu (A B)` | `nu A | nu B` | Large (either not-A or not-B) |
| `nu A nu B` | n/a | Small (both not-A and not-B) |
| `nu A | nu B` | `nu (A B)` | Large (same as first) |

---

## 8. Key Integration

### 8.1 Key Function

A **key K** is an additional constraint from shared context:

```
⟦σ | K⟧ = ⟦σ⟧ ∩ C(K)
```

Where C(K) is the constraint region activated by the key.

### 8.2 Key Sources

Keys can come from:
1. **Explicit prefix:** `[domain: X] sentence`
2. **Conversation context:** Previous sentences
3. **Shared knowledge:** External to message
4. **Embedded markers:** Sentences that constrain interpretation

### 8.3 Key Collapse

The **key collapse ratio** measures disambiguation:

```
R(σ, K) = |⟦σ⟧| / |⟦σ | K⟧|
```

Higher R means more effective key.

---

## 9. Special Constructions

### 9.1 Empty Sentence

The empty sentence denotes the universal set:
```
⟦""⟧ = S
```

### 9.2 Repetition

Repetition is semantically null (idempotent):
```
⟦w w⟧ = ⟦w⟧ ∩ ⟦w⟧ = ⟦w⟧
```

### 9.3 Contradiction

Contradictory constraints produce liminal regions:
```
⟦A nu-A⟧ = ⟦A⟧ ∩ (S \ ⟦A⟧) = boundary(⟦A⟧)
```

Under liminal semantics, this is the fuzzy boundary, not empty.

---

## 10. Tone Operators (Extension)

### 10.1 Tone Syntax

```ebnf
tone_op     = "frm" | "cas" | "iro" | "sin" | "urj" | "hes" ;
toned_sent  = { tone_op } , sentence ;
```

### 10.2 Tone Semantics

Tone operators modify pragmatic interpretation, not semantic content:

```
⟦tone σ⟧_semantic = ⟦σ⟧                    -- Same denotation
⟦tone σ⟧_pragmatic = apply_tone(tone, ⟦σ⟧) -- Modified interpretation
```

---

## 11. Type System (Optional)

### 11.1 Semantic Types

| Type | Description | Examples |
|------|-------------|----------|
| `Region` | Subset of S | ⟦lux⟧, ⟦nox⟧ |
| `Pair(R,R)` | Topic-comment | ⟦A | B⟧ |
| `Ref(R)` | Referenced region | ⟦yo A⟧ |
| `Num` | Numeric value | 3, 42 |
| `Bool` | Truth value | result of comparison |

### 11.2 Type Rules

```
w : Region                              -- Words are regions
op : Region → Region                    -- Unary operators
compare : Num × Num → Bool              -- Comparisons
arith : Num × Num → Num                 -- Arithmetic
```

---

## 12. Implementation Notes

### 12.1 Parser Requirements

1. Tokenize by whitespace, recognizing operators and punctuation
2. Build AST according to precedence rules
3. Handle grouping with explicit parentheses
4. Track scope boundaries for topic-comment

### 12.2 Semantic Evaluator Requirements

1. Maintain embedding space representation of S
2. Compute constraint regions for vocabulary
3. Implement intersection, complement, core, expand
4. Handle key conditioning

### 12.3 Edge Cases

| Case | Handling |
|------|----------|
| Empty input | Return S (universal) |
| Unknown word | Error or fallback to embedding |
| Numeric overflow | Standard numeric handling |
| Deep nesting | Recursion limit |
| Circular reference | Detection and error |

---

## 13. Example Derivations

### 13.1 Simple Sentence

```
Input: lux lif nox
Parse: flat_expr → term term term → lux lif nox
AST: Intersection(lux, lif, nox)
Semantics: ⟦lux⟧ ∩ ⟦lif⟧ ∩ ⟦nox⟧
```

### 13.2 With Operator

```
Input: nu ve bri
Parse: term → modified_term → nu term → nu (ve bri)
AST: Nu(Ve(bri))
Semantics: S \ core(⟦bri⟧)
```

### 13.3 Topic-Comment

```
Input: joy sad | now
Parse: scope_expr → flat_expr | scope_expr → (joy sad) | now
AST: TopicComment(Intersection(joy, sad), now)
Semantics: (⟦joy⟧ ∩ ⟦sad⟧, ⟦now⟧)
```

### 13.4 With Arithmetic

```
Input: x mi 3 + 2
Parse: comparison_expr → additive_expr comparator additive_expr
     → x mi (3 + 2)
AST: Compare(mi, x, Add(3, 2))
Semantics: {v : v < 5}
```

---

## 14. Appendix: Reserved Words

### 14.1 Operators

```
nu - negation
ve - intensifier
so - weakener
te - question
we - imperative
```

### 14.2 Quantifiers

```
al - universal (all)
ex - existential (some)
on - singular (exactly one)
```

### 14.3 References

```
yo - proximal (this)
an - distal (that)
sa - anaphoric (same)
```

### 14.4 Comparators

```
mi - less than
ma - greater than
eq - equal to
```

### 14.5 Tone (Extension)

```
frm - formal
cas - casual
iro - ironic
sin - sincere
urj - urgent
hes - hesitant
```

---

## See Also

- **[Limn-PL Grammar](limn-pl-grammar.md)** - Programming language extensions (variables, constraints, functions)
- **[Vocabulary Database](../../data/vocabulary/)** - Complete word list (~2,000 tri-letter words, 26 domains) - query via `vocab.sh`
- **[Vocabulary Management Guide](../guides/VOCAB-MANAGEMENT.md)** - How to add words and manage the database
- **[Theory Documents](../theory/)** - Semantic foundations

---

*This specification provides the formal foundation for **core Limn** (natural constraint language).*
*For programming language extensions, see limn-pl-grammar.md.*
