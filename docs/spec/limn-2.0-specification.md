# Limn 2.0: An AI-Native Language for HVM

**Status:** DESIGN SPECIFICATION (v0.1)
**Date:** 2026-02-07
**Authors:** Rex (Engineer), with recursive debate synthesis
**Epic:** hq-mzxvw
**Supersedes:** LIMN-PL-SPECIFICATION.md, limn-pl-hvm-design.md

---

## Abstract

Limn 2.0 is a programming language designed from first principles for a single
authorship model: **AI generates it, HVM executes it, humans never write it**.

The language preserves Limn's CVC (consonant-vowel-consonant) vocabulary as
surface syntax, giving LLMs semantic signal from pretraining. It replaces the
informal "constraint-intersection" semantics with a formal algebraic foundation:
**graded linear types over a monoidal category**, where Limn's six compositional
operators form a **quantale** (bounded distributive lattice with linear exponential
comonad). Programs are interaction net graphs serialized as typed ASTs. Semantic
ambiguity is handled through **algebraic effects** over HVM's native superposition,
not blocking oracle calls.

---

## 1. Design Principles

These principles were established through recursive debate and are non-negotiable.

| # | Principle | Rationale |
|---|-----------|-----------|
| P1 | **AI-Only Authorship** | Syntax optimized for token prediction, not human reading |
| P2 | **HVM-Native** | Every construct maps to an interaction net node pattern |
| P3 | **Deterministic Core + Effect Boundaries** | Pure computation + explicit typed effects for oracles |
| P4 | **Finite by Construction** | All types are bounded; no infinite regions |
| P5 | **CVC Vocabulary Preserved** | LLMs leverage pretrained semantic associations; 53% density advantage proven |
| P6 | **Graph-First** | Canonical form is interaction net; text is serialization |
| P7 | **Compositional by Type System** | Ill-typed compositions are unrepresentable |

---

## 2. Vocabulary Layer

### 2.1 CVC Retention (Debate 1 Verdict)

Limn 2.0 **keeps the CVC vocabulary** as its surface-level token set. Justification:

1. **Semantic signal**: LLMs trained on English associate "lov" with love, "fea" with
   fear. This bootstraps meaning without explicit definitions.
2. **Proven density**: The Limn tokenizer achieves 53% fewer tokens per example vs BPE
   (benchmark: `tools/limn-slm/benchmark_tokenizers.py`).
3. **Debuggability**: CVC words are human-legible, enabling inspection of AI output.
4. **Capacity**: 1,668 unused CVC slots remain; compositional operators provide
   combinatorial expansion beyond the namespace ceiling.

### 2.2 Token Universe

```
VOCABULARY := {w ∈ CVC³ | w ∈ LimnDict} ∪ OPERATORS ∪ STRUCTURAL
```

Where:
- `CVC³` = consonant-vowel-consonant trigrams (e.g., `lov`, `fea`, `pat`)
- `LimnDict` = the 1,076+ validated Limn words
- `OPERATORS` = `{ @, *, ^, \, ±, :, ~, ∎, ∿, →, | }`
- `STRUCTURAL` = `{ (, ), [, ], {, }, <, > }` (grouping, typing)

### 2.3 Token Semantics

Every CVC word carries a **semantic embedding vector** derived from its
English-language associations. The type system uses these embeddings for:
- Domain compatibility checking (can `lov` compose with `met`?)
- Operator applicability (is `lov@fea` well-typed?)
- Region membership (does `joy` satisfy constraint `emo^0.8`?)

The embeddings are NOT part of the language syntax — they are compile-time
metadata used by the type checker and the LLM generator.

---

## 3. Type System: Graded Linear Types over a Monoidal Category

### 3.1 Overview (Debate 2 Verdict)

The type system is **graded linear** over a **symmetric monoidal closed category**
where the grade algebra is a **quantale** — a complete lattice with an associative
binary operation distributing over arbitrary joins.

This means:
- Every value is used **exactly once** unless explicitly duplicated (linearity)
- Duplication carries a **grade** (cost) from the max-product semiring (resource tracking)
- Types compose via **tensor product** (parallel) and **linear implication** (sequential)
- The six Limn operators have precise categorical interpretations

### 3.2 The Grade Semiring

Grades live in the **max-product semiring** (sometimes called the Viterbi
semiring) over [0,1]:

```
(T, ⊕, ⊗, 0̄, 1̄) where:
  T = [0, 1]
  r ⊕ s = max(r, s)     -- addition is max
  r ⊗ s = r · s         -- multiplication is product
  0̄ = 0                  -- additive identity
  1̄ = 1                  -- multiplicative identity
```

Note: This is NOT the tropical semiring (min-plus or max-plus). Our grade
algebra uses max as addition and real product as multiplication. This choice
gives idempotent addition (max(r,r)=r) and a bounded multiplicative structure,
which maps naturally to intensity/certainty semantics.

Intuition: grades track **certainty** or **intensity**. A grade of 1.0 means
"fully certain/present"; 0.0 means "absent". The `^` (gradient) operator
directly manipulates grades.

For LLM generation, grades are **discretized** to 11 values: {0.0, 0.1, 0.2,
..., 0.9, 1.0}. The continuous space can be recovered in later phases.

### 3.3 Primitive Types

```
Type ::= Word                -- Atomic CVC word type (e.g., Type(lov))
       | Type ⊗ Type         -- Tensor product (parallel composition)
       | Type ⊕ Type         -- Direct sum (superposition/tagged union)
       | Type ⊸ Type         -- Linear implication (function type)
       | !_r Type            -- Graded exponential (r-duplicable resource)
       | Type \ Type         -- Residual (subtraction on lattice)
       | Guarded(Type, Obs)  -- Guarded type (conditional, pending observation)
       | Region(p, r)        -- Region: center p ∈ EmbeddingSpace, radius r
       | Effect(ε, Type)     -- Effectful computation
       | Observable          -- Observable condition (ground truth, temporal, etc.)
       | Unit                -- Trivial type (∅ content)
       | Void                -- Empty type (no inhabitants)
```

**Note on Word types:** Every CVC word `w` has type `Word`. Words are the atoms of
the system. Operators construct compound types from atoms. The embedding vectors
associated with words are **soft compile-time hints** used by the LLM generator
for semantic compatibility — they are NOT hard type constraints and do NOT affect
decidability of type checking.

### 3.4 The Six Operators as Categorical Structure

The six Limn operators form a **quantale** — they are not arbitrary combinators
but the canonical operations of a bounded distributive lattice with a linear
exponential comonad.

| Operator | Symbol | Expression Role | Type Role | HVM Node |
|----------|--------|----------------|-----------|----------|
| Interference | `*` | Parallel composition | Tensor product ⊗ | CON |
| Superposition | `±` | Tagged choice | Direct sum ⊕ | SUP |
| Projection | `@` | Apply/extract | (Application of ⊸) | APP + MAT |
| Gradient | `^` | Intensity modulation | Graded exponential !_r | DUP(weight) |
| Subtraction | `\` | Remove aspect | Residual (lattice) | MAT + ERA |
| Conditional | `:` | Guarded expression | Guarded(A, Obs) | LAM(blocked) |

**Key distinction:** `@` in **expressions** means "apply/extract" (e.g., `lov@fea` =
extract the fea-aspect of lov). `⊸` in **types** means "linear function type" (e.g.,
`Word ⊸ Word`). These are related by: if `f : A ⊸ B` then `f @ x : B` when `x : A`.
The currying isomorphism lives at the **type** level: `(A ⊗ B) ⊸ C ≅ A ⊸ (B ⊸ C)`.

### 3.5 The Algebraic Laws

These laws are enforced by the type checker. Any expression violating them is
**unrepresentable** (not a runtime error — a compile-time rejection).

The type system is **quantitative** (not purely linear): linearity is the default,
but graded exponentials allow controlled duplication. The lattice structure on
types is a **distributive lattice** with residuals, NOT a Boolean algebra.

**Tensor (Interference `*`):**
```
L1.  A * B  ≅  B * A                    -- commutativity
L2.  (A * B) * C  ≅  A * (B * C)       -- associativity
L3.  A * Unit  ≅  A                     -- unit
L4.  A * Void  ≅  Void                  -- annihilation
```

**Sum (Superposition `±`):**

`±` is a **tagged** sum (linear coproduct). The two branches are distinguishable.

```
L5.  A ± B  ≅  B ± A                    -- commutativity (up to tag swap)
L6.  (A ± B) ± C  ≅  A ± (B ± C)       -- associativity (re-tagging)
L7.  A ± Void  ≅  A                     -- unit (void branch unreachable)
```

Note: `A ± A` is NOT isomorphic to `A` — the two branches carry distinct tags
(left injection vs right injection). This respects linearity: the consumer
must handle both cases even if the types coincide.

**Distributivity:**
```
L8.  A * (B ± C)  ≅  (A * B) ± (A * C) -- * distributes over ±
```

**Currying (at type level, using ⊸):**
```
L9.  (A ⊗ B) ⊸ C  ≅  A ⊸ (B ⊸ C)     -- currying isomorphism
L10. Unit ⊸ A  ≅  A                     -- trivial input
```

**Application (Projection `@`):**

`@` is expression-level application. These are typing consequences, not type
isomorphisms:

```
L11. If f : A ⊸ B and x : A, then f @ x : B        -- application
L12. If f : (A ⊗ B) ⊸ C, then curry(f) : A ⊸ (B ⊸ C)  -- currying
```

**Graded Exponential (Gradient `^`):**
```
L13. A^1.0  ≅  A                        -- identity grade
L14. A^0.0  ≅  Unit                     -- zero grade (erasure)
L15. (A^r)^s  ≅  A^(r·s)               -- grade composition
L16. (A * B)^r  ≅  A^r * B^r           -- grade distributes over *
L17. (A ± B)^r  ≅  A^r ± B^r           -- grade distributes over ±
```

**Subtraction/Residual (`\`):**

`\` is the **residual** (left adjoint to `*` in the lattice): `A \ B` is the
largest type C such that `C * B ≤ A`. This is well-defined in a distributive
lattice (Heyting algebra structure on the type lattice).

```
L18. A \ Void  ≅  A                     -- subtract nothing
L19. A \ A  ≤  Void                     -- self-cancellation (subtyping)
L20. (A ± B) \ B  ≤  A                  -- residual of sum (subtyping)
```

Note: L19 and L20 are **subtyping** relationships, not isomorphisms. The
residual may have fewer inhabitants than suggested by the notation. At
runtime, applying `\` to a value that matches the subtracted component
produces an ERA (erasure) node — the computation disappears.

In a Heyting algebra, only one De Morgan law holds:
```
L21. (A \ B) * (A \ C)  ≤  A \ (B ± C) -- residual distributes (one direction)
```

The full De Morgan dual `¬(A * B) = ¬A ± ¬B` does NOT hold in general (that
would require a Boolean algebra). We deliberately stay in the Heyting fragment
for constructive computation.

**Conditional (`:`):**
```
L22. (A : g₁) : g₂  ≅  A : (g₁ * g₂)  -- guard composition
L23. A : ∎(true)  ≅  A                  -- trivially satisfied guard
L24. A : ∎(false)  ≅  Void              -- impossible guard
```

### 3.6 Typing Judgments

The core typing judgment has the form:

```
Γ; Δ ⊢_r e : A ! ε
```

Where:
- `Γ` = unrestricted context (duplicable, grade = ∞)
- `Δ` = linear context (used exactly once)
- `r` = current grade (from max-product semiring)
- `e` = expression
- `A` = type
- `ε` = effect row (oracle effects)

**Core rules:**

```
                 w ∈ LimnDict
WORD:           ────────────────────────
                 · ⊢ w : Word

                 Δ₁ ⊢ e₁ : A    Δ₂ ⊢ e₂ : B
TENSOR:         ────────────────────────────────
                 Δ₁, Δ₂ ⊢ e₁ * e₂ : A ⊗ B

                 Δ ⊢ e₁ : A    Δ ⊢ e₂ : B
SUPERPOSE:      ────────────────────────────────
                 Δ ⊢ e₁ ± e₂ : A ⊕ B
```

Note: SUPERPOSE shares context Δ (additive rule — only one branch executes).

```
                 Δ, x:A ⊢ e : B
LAMBDA:         ────────────────────────
                 Δ ⊢ λx.e : A ⊸ B

                 Δ₁ ⊢ f : A ⊸ B    Δ₂ ⊢ e : A
APP:            ────────────────────────────────────
                 Δ₁, Δ₂ ⊢ f @ e : B
```

The `@` operator at expression level is application (APP rule). At the type level,
`⊸` constructs function types (LAMBDA rule).

```
                 Δ ⊢ e : A    r ∈ {0.0, 0.1, ..., 1.0}
GRADE:          ──────────────────────────────────────────
                 r·Δ ⊢ e^r : !_r A
```

Grade multiplication scales the entire linear context: each variable's usage
count is multiplied by r.

```
                 Δ ⊢ e : A ⊕ B
SUBTRACT:       ────────────────────────────────────
                 Δ ⊢ e \ B : Guarded(A, match_left)
```

SUBTRACT produces a **guarded** type: the result is A only when the input is
in the left branch. If the input is in the B branch, reduction encounters ERA
(erasure). This makes the partiality explicit in the type.

```
                 Δ₁ ⊢ g : Observable    Δ₂ ⊢ e : A
GUARD:          ────────────────────────────────────────
                 Δ₁, Δ₂ ⊢ e : g : Guarded(A, g)
```

GUARD introduces a computation that only reduces when the observation `g` is
satisfied. `Observable` includes ground truth assertions (`∎ expr`), temporal
queries (`∿ was/now/wil expr`), and effect results.

```
                 Δ ⊢ e : A    (op : A → B) ∈ Effect(ε)
EFFECT:         ──────────────────────────────────────────
                 Δ ⊢ perform op(e) : B ! ε

                 Δ ⊢ obs : Limn
GROUND:         ────────────────────────────
                 Δ ⊢ ∎ obs : Observable

                 Δ ⊢ q : Limn    t ∈ {was, now, wil}
TEMPORAL:       ──────────────────────────────────────────
                 Δ ⊢ ∿ t q : Observable ! TemporalAccess

                 Δ ⊢ e : Limn    Δ₂ ⊢ ctx : Limn
ORACLE:         ──────────────────────────────────────────
                 Δ, Δ₂ ⊢ ~ e @ ctx : Limn ! LLMOracle
```

---

## 4. Interaction Net Compilation

### 4.1 Node Types

Every Limn 2.0 construct compiles to one of these interaction net node types:

```
HVM Node    | Limn Construct           | Ports
────────────|──────────────────────────|──────────
CON(tag)    | Interference (A * B)     | [principal, left, right]
SUP(label)  | Superposition (A ± B)    | [principal, left, right]
LAM         | Projection (A @ B)       | [principal, variable, body]
APP         | Application (f applied)  | [principal, function, argument]
DUP(label,w)| Gradient (A ^ r)         | [principal, copy1, copy2] + weight
ERA         | Subtraction (erased)     | [principal]
MAT(arms)   | Pattern match            | [principal, scrutinee, arms...]
REF(name)   | Named definition         | [principal]
ORC(effect) | Oracle call              | [principal, context, continuation]
```

### 4.2 Compilation Rules

**Interference (tensor):**
```
compile(A * B) =
  let nA = compile(A)
  let nB = compile(B)
  CON(tag_fresh()) [principal → up, left → nA, right → nB]
```

**Superposition (sum):**
```
compile(A ± B) =
  let nA = compile(A)
  let nB = compile(B)
  SUP(label_fresh()) [principal → up, left → nA, right → nB]
```

**Projection/Application (`@`):**
```
compile(f @ x) =
  let nF = compile(f)   -- must have type A ⊸ B
  let nX = compile(x)   -- must have type A
  APP [principal → up, function → nF, argument → nX]

-- For the common pattern  word₁ @ word₂  ("extract aspect"):
-- This desugars to application of a built-in projection function.
-- Each word has a set of aspect-extractors in its type signature.
-- E.g., lov @ fea  ≡  (aspect_fea) @ lov
-- where aspect_fea : Word ⊸ Word is a built-in projection.
compile(word₁ @ word₂) =
  let nWord = compile(word₁)
  let nAspect = compile(aspect_extractor(word₂))
  APP [principal → up, function → nAspect, argument → nWord]

-- aspect_extractor(w) compiles to a pattern match:
compile(aspect_extractor(w)) =
  LAM [principal → up, var → x,
       body → MAT [scrutinee → x,
                    arm(w_component) → identity,   -- extract w-component
                    arm(_) → ERA]]                 -- erase non-w components
```

**Gradient (graded duplication):**
```
compile(A ^ r) =
  let nA = compile(A)
  DUP(label_fresh(), r) [principal → nA, copy1 → out1, copy2 → out2]
  -- weight r controls interaction priority in reduction
  -- r=0.0 → ERA (erase), r=1.0 → identity, r∈(0,1) → weighted dup
```

**Subtraction (erasure + match):**
```
compile(A \ B) =
  let nA = compile(A)
  MAT [scrutinee → nA,
       arm(B) → ERA,        -- matching B gets erased
       arm(_) → identity]   -- everything else passes through
```

**Conditional (guarded lambda):**
```
compile(A : B) =
  LAM(blocked=true) [principal → up, guard → compile(B), body → compile(A)]
  -- Only reduces when guard port receives a truthy value
```

**Oracle (algebraic effect):**
```
compile(~ expr @ ctx) =
  ORC(LLMOracle) [principal → up,
                   context → compile(ctx),
                   continuation → compile(expr)]
  -- Reduction: when ORC is the active pair's principal,
  -- HVM suspends this subgraph and invokes the effect handler
```

### 4.3 Reduction Rules

The interaction net reduces by **annihilation** (same-type nodes meeting) and
**commutation** (different-type nodes passing through each other).

Key reduction rules beyond standard lambda calculus:

```
-- Gradient: DUP meets DUP
-- Same label (l1 == l2): annihilation — cross-wire copies
DUP(l, r1) >< DUP(l, r2)  →  annihilate (standard HVM same-label rule)
-- Different labels (l1 ≠ l2): commutation — create new SUP/DUP pair
DUP(l1, r1) >< DUP(l2, r2)  →  commute with weight r1·r2 on new DUP nodes

-- Gradient meets ERA: propagate erasure unconditionally
DUP(l, r) >< ERA  →  ERA on both copies (erasure always wins)

-- SUP meets CON: superposition distributes over construction
SUP(l) >< CON(t)  →  CON(t)[SUP(l), SUP(l)]  (distribute)

-- ORC meets anything: suspend and call handler
ORC(ε) >< X  →  suspend(X), perform(ε, context)
               →  resume(handler_result) grafts replacement subgraph
```

---

## 5. Effect System: Algebraic Effects over Superposition

### 5.1 Overview (Debate 3 Verdict)

The oracle is modeled as an **algebraic effect**, not a blocking pause-and-resume.
This means:
- Oracle calls compose with HVM's native superposition
- Multiple oracle strategies can be plugged in via handlers
- Deterministic testing requires no mock infrastructure — just swap the handler

### 5.2 Effect Types

```
effect LLMOracle {
  -- Resolve semantic ambiguity given context
  resolve : (expr: Limn, context: Context) → Limn

  -- Collapse superposition to single branch
  collapse : (sup: A ⊕ B, criteria: Limn) → A | B

  -- Generate continuation from partial program
  continue : (partial: Limn, type_hint: Type) → Limn
}

effect TemporalAccess {
  -- Retrieve from temporal memory
  recall : (temporal: Was|Now|Will, query: Limn) → Limn

  -- Store to temporal memory
  record : (temporal: Now, value: Limn) → Unit
}

effect GroundTruth {
  -- Assert an observation from reality
  observe : (observation: Limn) → Limn

  -- Query external state
  sense : (query: Limn) → Limn
}
```

### 5.3 Effect Handlers

```
-- Production handler: calls actual LLM
handler ProductionOracle : LLMOracle {
  resolve(expr, ctx) → llm_call(expr, ctx, model="limn-slm")
  collapse(sup, criteria) → llm_rank(branches(sup), criteria)
  continue(partial, hint) → llm_generate(partial, constrained_by=hint)
}

-- Testing handler: deterministic, cached
handler CachedOracle : LLMOracle {
  resolve(expr, ctx) → cache_lookup(hash(expr, ctx)) ?? deterministic_fallback(expr)
  collapse(sup, criteria) → always_left(sup)  -- deterministic branch selection
  continue(partial, hint) → cached_continuation(partial)
}

-- Superposition handler: explore ALL branches (for verification)
handler SuperpositionOracle : LLMOracle {
  resolve(expr, ctx) → sup(candidate₁, candidate₂, ..., candidateₙ)
  collapse(sup, criteria) → sup  -- don't collapse, keep all branches
  continue(partial, hint) → sup(continuation₁, ..., continuationₙ)
}
```

### 5.4 Handler Composition

Effect handlers compose via HVM's native superposition. A program can run with
**multiple handlers simultaneously**, producing a superposition of results:

```limn
-- Run with both production and cached oracles in superposition
result = handle program with {ProductionOracle ± CachedOracle}
-- result : (ProductionResult ⊕ CachedResult)
-- Both branches reduce in parallel on HVM
```

### 5.5 Deadlock Prevention

Oracle handlers MUST NOT invoke oracle effects themselves (no re-entrant
effects). This is enforced by the type system: handler implementations have
effect row `ε = ∅` (pure), meaning they cannot perform effects.

If a handler needs to consult another oracle (e.g., a multi-model pipeline),
this must be modeled as a **single effect** with a composite handler, not as
nested effect calls. This prevents circular dependencies.

### 5.6 Oracle Cost Control in Superpositions

When an ORC node appears inside a SUP node, the cost of oracle calls can
grow exponentially with nesting depth. To control this:

1. **Lazy evaluation**: Oracle calls inside superposition branches are
   evaluated **lazily** — only when a branch is selected (collapsed).
2. **Budget parameter**: Each effect handler has a `max_calls` budget.
   When the budget is exceeded, the handler returns a deterministic fallback.
3. **Deduplication**: If two branches of a superposition require the same
   oracle call (same context hash), the call is made once and shared.

---

## 6. Program Representation: Graph-First AST

### 6.1 Canonical Form

The **canonical form** of a Limn 2.0 program is an interaction net graph. The
text serialization (what the LLM generates) is a **typed AST** that deterministically
compiles to the graph. There is no ambiguity in compilation.

### 6.2 AST Grammar

```
Program  ::= Def+
Def      ::= name '=' Expr                -- type inferred by checker

Expr     ::= Word                          -- CVC atom (type: Word)
           | Expr '*' Expr                  -- interference / tensor (TENSOR)
           | Expr '±' Expr                  -- superposition / tagged sum (SUPERPOSE)
           | Expr '@' Expr                  -- application (APP: f @ x)
           | Expr '^' Grade                 -- gradient / graded dup (GRADE)
           | Expr '\' Expr                  -- subtraction / residual (SUBTRACT)
           | Expr ':' Expr                  -- conditional / guard (GUARD)
           | '~' Expr '@' Expr             -- oracle effect (ORACLE)
           | '∎' Expr                       -- ground truth (GROUND → Observable)
           | '∿' Temporal Expr             -- temporal access (TEMPORAL)
           | '(' Expr ')'                   -- grouping
           | Expr '→' Expr                 -- sequential composition (bind)
           | Expr '|' Expr                 -- parallel composition (= tensor *)
           | name                           -- variable reference (REF)
           | name '=' Expr 'in' Expr        -- local definition (let binding)

Word     ::= [bcdfghjklmnpqrstvwxyz][aeiou][bcdfghjklmnpqrstvwxyz]
Grade    ::= n/10 where n ∈ {0, 1, 2, ..., 10}
Temporal ::= 'was' | 'now' | 'wil'
Type     ::= (see §3.3)
```

**Sequential composition (→):** `A → B` means "compute A, then compute B with
A's result in scope." At the type level, this is monadic bind for effectful
computations: `A ! ε → (A → B ! ε) ≡ B ! ε`. For pure computations, `→` is
just `let x = A in B`.

**Parallel composition (|):** `A | B` is syntactic sugar for `A * B` (tensor).
The pipe notation is preserved from Limn 1.0 for readability.

### 6.3 Example Programs

**Hello World — A simple oracle-driven greeting:**
```limn
gre = ~ (sel * awa | wor * gre * new) @ (con * soc)
-- Typing derivation:
--   sel, awa, wor, gre, new, con, soc : Word  (by WORD rule)
--   sel * awa : Word ⊗ Word                    (by TENSOR)
--   ~ (...) @ (...) : Word ! LLMOracle         (by ORACLE)
-- Compiles to: ORC node with context=social, continuation=greeting-pattern
```

**Interference — Blending two concepts:**
```limn
gel = sol * liq
-- Typing derivation:
--   sol : Word, liq : Word                     (by WORD)
--   sol * liq : Word ⊗ Word                    (by TENSOR)
-- Compiles to: CON node connecting sol-atom and liq-atom
```

**Superposition — Holding multiple possibilities:**
```limn
emo = joy ± fea
-- Typing derivation:
--   joy : Word, fea : Word                     (by WORD, shared context ·)
--   joy ± fea : Word ⊕ Word                    (by SUPERPOSE, additive rule)
-- Compiles to: SUP node — both branches live until collapsed
```

**Projection — Extracting an aspect:**
```limn
fea_lov = (aspect_fea) @ lov
-- Typing derivation:
--   aspect_fea : Word ⊸ Word                   (built-in projection)
--   lov : Word                                  (by WORD)
--   aspect_fea @ lov : Word                     (by APP)
-- Sugar:  lov @ fea  desugars to  (aspect_fea) @ lov
-- Compiles to: APP(aspect_fea_LAM, lov-atom)
```

**Gradient — Intensity modulation:**
```limn
mld_joy = joy ^ 0.3
-- Typing derivation:
--   joy : Word                                  (by WORD)
--   joy ^ 0.3 : !_0.3 Word                     (by GRADE, r=0.3)
-- Compiles to: DUP(weight=0.3) node on joy-atom
```

**Subtraction — Removing an aspect:**
```limn
pur_lov = (joy ± fea) \ fea
-- Typing derivation:
--   joy ± fea : Word ⊕ Word                    (by SUPERPOSE)
--   (joy ± fea) \ fea : Guarded(Word, match_left)  (by SUBTRACT)
-- Note: subtraction requires a sum type as input.
--   The sugar `lov \ fea` first lifts lov into a superposition
--   of its semantic aspects, then subtracts the fea branch.
-- Compiles to: MAT on SUP, ERA on fea-matching arm
```

**Conditional — Guarded computation:**
```limn
hop = joy : (∎ fth_present)
-- Typing derivation:
--   ∎ fth_present : Observable                  (by GROUND)
--   joy : Word                                  (by WORD)
--   joy : (∎ fth_present) : Guarded(Word, ∎ fth_present)  (by GUARD)
-- Only reduces when the ground truth observation holds
-- Compiles to: LAM(blocked) with guard=∎ fth_present, body=joy
```

**Temporal access:**
```limn
mem = ∿ was (con * abt * pro)
-- Typing derivation:
--   con * abt * pro : Word ⊗ Word ⊗ Word       (by TENSOR)
--   ∿ was (...) : Observable ! TemporalAccess   (by TEMPORAL)
-- Recall past conversation about project
-- Compiles to: ORC(TemporalAccess.recall) with temporal=Was
```

**Complex program — Multi-operator composition:**
```limn
-- Define a region of emotional response
emo_res =
  (joy * lov^0.8) : (∎ usr * sai * pos)
  → ~ (sel * res * emo) @ (con * emo)

-- Typing derivation:
--   lov^0.8 : !_0.8 Word                       (by GRADE)
--   joy * lov^0.8 : Word ⊗ !_0.8 Word          (by TENSOR)
--   ∎ usr * sai * pos : Observable              (by GROUND)
--   (...) : ∎(...) : Guarded(Word ⊗ !_0.8 Word, obs)  (by GUARD)
--   ~ (...) @ (...) : Word ! LLMOracle          (by ORACLE)
--   full expr : Guarded(Word ⊗ !_0.8 Word, obs) → Word ! LLMOracle
-- When ground truth (user said positive), construct joy⊗love(0.8),
-- then oracle-generate emotional response in that context.
```

---

## 7. LLM Generation Protocol: Constrained Typed AST

### 7.1 Overview (Debate 3 Verdict)

The LLM does **not** generate free-form text that is then parsed. Instead, it
generates a sequence of **typed AST construction actions** from a finite vocabulary,
validated incrementally by the type checker.

### 7.2 Generation Actions

The LLM selects from this fixed action vocabulary at each step:

```
Action ::= WORD(w)              -- Emit CVC word atom (w ∈ LimnDict)
         | TENSOR               -- Begin left operand of A * B
         | TENSOR_RIGHT         -- Transition to right operand of A * B
         | SUPERPOSE            -- Begin left branch of A ± B
         | SUPERPOSE_RIGHT      -- Transition to right branch of A ± B
         | PROJECT              -- Begin function operand of f @ x
         | PROJECT_ARG          -- Transition to argument of f @ x
         | GRADE(n)             -- Apply gradient ^ (n/10), n ∈ {0,1,...,10}
         | SUBTRACT             -- Begin operand of A \ B
         | SUBTRACT_TYPE        -- Specify type being subtracted
         | GUARD                -- Begin guarded expression A : g
         | GUARD_OBS            -- Transition to observation/guard
         | ORACLE               -- Begin oracle call (~ expr @ ctx)
         | ORACLE_CTX           -- Transition to oracle context
         | GROUND               -- Insert ground truth assertion (∎)
         | TEMPORAL(t)          -- Insert temporal access (∿ was|now|wil)
         | OPEN_GROUP           -- Open parenthesis for grouping
         | CLOSE_GROUP          -- Close parenthesis
         | SEQUENCE             -- Sequential composition (→)
         | PIPE                 -- Parallel composition (|) = alias for TENSOR
         | DEF(name)            -- Begin named definition (type inferred)
         | DEF_BODY             -- Transition from name to definition body
         | REF(name)            -- Reference a named definition
         | DONE                 -- Close current construct / end expression
         | END_PROGRAM          -- Signal program completion
```

Binary operators (TENSOR, SUPERPOSE, PROJECT, SUBTRACT, GUARD, ORACLE) use
a **two-phase** protocol: the first action opens the left operand, the
corresponding *_RIGHT/*_ARG/*_CTX/*_OBS action transitions to the second
operand, and DONE closes the construct. This eliminates ambiguity about
operand boundaries.

GRADE uses a discrete parameter n ∈ {0,1,...,10} where the actual grade is n/10.
This gives 11 possible grades, sufficient for practical use.

### 7.3 Incremental Validation

At each generation step, the type checker computes the set of **valid next actions**
and the LLM samples only from that set:

```
state = initial_typing_context()
for each generation step:
  valid_actions = type_checker.valid_next(state)
  action = llm.sample(valid_actions)  -- constrained sampling
  state = type_checker.apply(state, action)
  if state.is_complete():
    return state.to_program()
```

This guarantees:
1. **Every generated program is well-typed** — no post-hoc validation needed
2. **No syntactic errors** — the grammar is enforced by construction
3. **Finite action space** — the LLM chooses from ~30 action types + vocabulary
4. **Incremental** — partial programs are always in a valid prefix state

**Backtracking:** If `valid_next(state)` returns the empty set (a dead-end),
the generator backtracks to the most recent choice point and selects a
different action. Maximum backtracking depth is bounded by the maximum program
size parameter. In practice, dead-ends should be rare because the type checker
prunes obviously-failing paths eagerly.

### 7.4 FSM Integration

The constrained generation integrates with the existing grammar-constrained
FSM infrastructure (`tools/limn-slm/`). The Limn 2.0 FSM is:

```
States: { START, EXPR, OPERATOR, TYPE_CHECK, EFFECT, TEMPORAL, GROUP, COMPLETE }
Transitions: determined by type checker valid_next()
Alphabet: Action vocabulary (§7.2)
```

This is NOT a static FSM — it is a **dynamically-constrained decoding** system
where the type checker computes valid transitions at each step. This is more
expressive than a fixed grammar FSM but still guarantees termination (bounded
by max program size). The "states" are typing contexts, and "transitions" are
the valid actions that maintain well-typedness.

---

## 8. Execution Model

### 8.1 Compilation Pipeline

```
LLM Generation (constrained)
  ↓ typed AST actions
Type-Checked AST
  ↓ compile()
Interaction Net Graph
  ↓ serialize
HVM bytecode (.inet)
  ↓ load
HVM Runtime
  ↓ reduce (GPU-parallel)
Result (with suspended oracle nodes)
  ↓ handle effects
Final Result
```

### 8.2 Phase 1: Bend Compilation (Bootstrap)

For bootstrapping, Limn 2.0 compiles to **Bend source code** which is then
compiled to HVM by the Bend compiler:

```
Limn 2.0 AST → Bend source → HVM bytecode → Execution
```

This leverages the existing Bend infrastructure and allows incremental development.
The Bend backend handles:
- Lambda calculus compilation
- Superposition compilation (SUP/DUP pairs)
- Pattern matching compilation
- Basic interaction net optimization

### 8.3 Phase 2: Direct Inet Compilation (Target)

The mature compiler generates **interaction net graph descriptions directly**,
bypassing Bend:

```
Limn 2.0 AST → .inet graph → HVM runtime → Execution
```

This enables:
- Oracle nodes (ORC) that Bend doesn't support
- Custom reduction rules for Limn-specific interactions
- Finer control over interaction net topology
- Direct GPU kernel generation for specialized patterns

### 8.4 Oracle Execution

When HVM encounters an ORC node during reduction:

1. **Suspend**: The subgraph rooted at ORC is frozen
2. **Extract context**: The context port's value is serialized
3. **Dispatch to handler**: The effect handler receives (effect_type, context)
4. **Handler returns**: A replacement interaction net subgraph
5. **Graft**: The replacement is wired into the frozen subgraph's ports
6. **Resume**: HVM continues reducing from the graft point

Multiple ORC nodes in different parts of the graph can be handled in parallel
(they are independent subcomputations). An ORC inside a SUP produces a
superposition of oracle results.

---

## 9. Resolved Design Debates

### D1: Linear vs Dependent Types → **Graded Linear**

Dependent types (where types depend on values) are too complex for AI generation
and don't map cleanly to interaction nets. Graded linear types provide:
- Resource tracking (how many times a value is used)
- Intensity semantics (the `^` operator's grade)
- Clean HVM mapping (DUP nodes carry weights)
- Simpler type inference for the LLM generator

### D2: Sync vs Async Oracles → **Algebraic Effects (Async)**

Blocking oracle calls would stall HVM reduction. Algebraic effects allow:
- Non-blocking oracle invocation
- Multiple handler strategies (production, cached, superposition)
- Composability with HVM's native superposition
- Deterministic testing without mocks

### D3: CVC Vocabulary Survival → **Yes, Preserved**

CVC vocabulary provides semantic signal, density advantage, and debuggability.
The alternative (opaque token IDs) sacrifices all three for marginal
machine efficiency that doesn't matter when the bottleneck is LLM generation
latency, not token encoding.

### D4: Surface Syntax → **Typed AST Actions**

The LLM generates typed AST construction actions, not free-form text. This
eliminates parsing ambiguity and guarantees well-typedness. The CVC words
appear as `WORD(w)` actions, preserving their semantic role while enforcing
structural correctness.

### D5: Validation Approach → **Incremental Type Checking**

Programs are validated incrementally during generation, not post-hoc. The type
checker maintains state and computes valid next actions at each step. This
means the LLM never generates an invalid program.

---

## 10. Migration from Limn 1.0

### 10.1 What Survives

| Limn 1.0 | Limn 2.0 | Notes |
|-----------|----------|-------|
| CVC vocabulary | CVC vocabulary | Unchanged |
| 6 compositional operators | 6 operators with formal algebra | Same symbols, precise semantics |
| `~` delegation | `~` oracle effect | Now typed with handlers |
| `∎` ground truth | `∎` ground truth effect | Now an algebraic effect |
| `∿` temporal | `∿` temporal effect | Now an algebraic effect |
| `→` causality | `→` sequential composition | Formalized |
| `\|` pipe | `\|` parallel composition | Formalized as tensor |
| Triad structure | Still valid but not mandatory | Type system supersedes |
| Constraint regions | Region types | Formalized as Region(p, r) |

### 10.2 What Changes

| Limn 1.0 | Limn 2.0 | Why |
|-----------|----------|-----|
| Informal semantics | 24 algebraic laws (Heyting lattice) | Compositionality requires precision |
| Prolog implementation | HVM execution | Performance, parallelism |
| Human-writable | AI-only generation | P1: AI authorship |
| Unbounded regions | Finite types | P4: finite by construction |
| Runtime errors | Compile-time rejection | P7: unrepresentable ill-types |
| Free-form generation | Constrained AST actions | §7: well-typedness by construction |
| Blocking oracle | Algebraic effects | §5: composable, testable |

### 10.3 Training Data Compatibility

Existing Limn training data (`tools/limn-slm/data/v5*`) remains valuable:
- CVC vocabulary and operator usage patterns transfer directly
- Semantic embeddings from the trained SLM can seed the type checker
- The extended tokenizer (2,593 tokens) covers the Limn 2.0 action vocabulary
- Conversation examples inform oracle handler training

---

## 11. Implementation Roadmap

### Phase 0: Specification Validation (this document)
- [ ] Recursive debate synthesis (complete)
- [ ] Stress-test specification for internal consistency
- [ ] Identify implementation-blocking gaps

### Phase 1: Type Checker Prototype
- [ ] Implement grade semiring (tropical over [0,1])
- [ ] Implement 24 algebraic laws as rewrite rules
- [ ] Implement valid_next() for constrained generation
- [ ] Test with hand-written Limn 2.0 programs

### Phase 2: Bend Backend
- [ ] Compile Limn 2.0 AST → Bend source
- [ ] Map operators to Bend constructs (CON, SUP, DUP, LAM, APP)
- [ ] Handle oracle nodes via Bend FFI or custom pass
- [ ] Verify round-trip: generate → compile → execute → verify

### Phase 3: LLM Generator
- [ ] Train Limn SLM on typed AST action sequences
- [ ] Integrate FSM-constrained generation with type checker
- [ ] Benchmark: generation quality with/without type constraints
- [ ] Test oracle handler composition

### Phase 4: Direct Inet Compiler
- [ ] Generate .inet graph descriptions directly (bypass Bend)
- [ ] Implement ORC node reduction rules
- [ ] GPU kernel specialization for common patterns
- [ ] Performance benchmarks vs Bend backend

### Phase 5: Full Integration
- [ ] End-to-end: LLM prompt → Limn 2.0 → HVM → result
- [ ] Oracle handler ecosystem (production, cached, superposition)
- [ ] Temporal effect handlers (memory integration)
- [ ] Ground truth effect handlers (external state)

---

## Appendix A: Formal Category Theory Summary

Limn 2.0's type system is a **graded linear/non-linear adjunction** over the
category **Limn** where:

- Objects are Limn types (§3.3)
- Morphisms are typed Limn programs
- Tensor product ⊗ = interference `*` (symmetric monoidal structure)
- Internal hom ⊸ = function types (closed structure)
- `@` at expression level = application (consuming a ⊸ value)
- Coproduct ⊕ = superposition `±` (tagged linear sum)
- Graded comonad !_r = gradient `^` (gives controlled duplication)
- Residual = subtraction `\` (gives Heyting algebra structure on types)
- Guardedness = conditional `:` (gives guarded computation)

The grade semiring (T, max, ·, 0, 1) is the **max-product (Viterbi) semiring**,
making this a **quantitative type theory** in the sense of Atkey (2018) and
McBride (2016), specialized to intensity/certainty semantics.

**Important:** The type lattice is a **Heyting algebra** (constructive), NOT a
Boolean algebra. Only one direction of De Morgan holds (L21). The full classical
De Morgan dual is deliberately excluded to maintain constructive computation
semantics — you cannot "negate" a tensor product into a sum.

The interaction net model provides **optimal reduction** for the pure lambda
calculus fragment (in the sense of Lamping/Lévy). Oracle nodes (ORC) and
weighted DUP nodes extend beyond the classical optimality result; termination
of the pure fragment follows from the finite type discipline (P4), while the
effectful fragment is inherently non-terminating (oracle calls may diverge).
HVM's native support for superposition means `±` is zero-cost at the
execution level.

## Appendix B: Comparison with Related Work

| System | Relationship to Limn 2.0 |
|--------|--------------------------|
| **Granule** (Orchard et al.) | Graded linear types; Limn 2.0 adds quantale structure and AI generation |
| **Idris 2** (Brady) | Quantitative types; Limn 2.0 is simpler (no dependent types) but has effects |
| **HVM/Bend** (HigherOrderCO) | Execution target; Limn 2.0 adds typed oracle nodes |
| **Frank** (Lindley et al.) | Algebraic effects; Limn 2.0 effects compose over superposition |
| **Limn 1.0** | Predecessor; Limn 2.0 formalizes and mechanizes |

---

*lan 2.0 = typ for | gra lin | eff com | hvm exe*
*(language 2.0 = types formal | graded linear | effects composable | HVM executable)*
