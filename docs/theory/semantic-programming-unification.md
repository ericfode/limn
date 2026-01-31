# Semantic-Programming Limn Unification

**Status:** Design Document
**Purpose:** Document the path from two separate Limn implementations to a unified constraint language.
**Vocabulary:** v3-natural (updated 2026-01-30)

---

## 1. Current State: Two Limns

### 1.1 Semantic Limn (Natural Language)

**Location:** `docs/spec/vocabulary-v1.md`, `bootstrap-v1.md`, `bootstrap-v2.md`

**Characteristics:**
- CVC syllable vocabulary (`lux`, `nox`, `lif`, `sol`, etc.)
- Interpreted by LLMs through in-context learning
- Meaning = constraint intersection in semantic space
- Key mechanism for disambiguation
- Order-independent (commutative)

**Example:**
```
lux lif lin = bright + alive + linear = glowing river, sunbeam, lit snake
```

### 1.2 Programming Limn (Constraint Language)

**Location:** `docs/spec/programming-v1.md`, `src/limn_interpreter.py`

**Characteristics:**
- English keyword vocabulary (`equals`, `plus`, `element_of`, etc.)
- Interpreted by Python constraint solver
- Meaning = satisfying variable bindings
- Bidirectional computation
- Order-independent (set of constraints)

**Example:**
```
program addition:
  variables: a b c
  constraints:
    a plus b equals c
```

---

## 2. Shared Foundation: Constraint Algebra

Both Limns share the same underlying mathematical structure.

### 2.1 Core Axioms

| Axiom | Semantic Limn | Programming Limn |
|-------|----------------|-------------------|
| **Intersection** | Words combine by AND | Constraints combine by AND |
| **Commutativity** | Word order irrelevant | Statement order irrelevant |
| **Satisfiability** | Key collapses to meaning | Solver finds bindings |
| **Negation** | `nu` inverts region | `nu_equals` inverts relation |

### 2.2 Formal Algebra

Both implement a **constraint semilattice**:

```
Operations:
  ⊓ (meet/intersection) - combine constraints
  ⊤ (top) - unconstrained (any interpretation)
  ⊥ (bottom) - unsatisfiable (no interpretation)

Properties:
  a ⊓ b = b ⊓ a           (commutativity)
  a ⊓ (b ⊓ c) = (a ⊓ b) ⊓ c  (associativity)
  a ⊓ a = a               (idempotence)
  a ⊓ ⊤ = a               (identity)
  a ⊓ ⊥ = ⊥               (annihilation)
```

### 2.3 Key Insight

**Semantic Limn:** Constraints on meaning-space (what concepts are denoted)
**Programming Limn:** Constraints on value-space (what numbers/values satisfy)

Both are instances of **constraint satisfaction over a domain**:
- Semantic: domain = possible meanings
- Programming: domain = possible variable assignments

---

## 3. Vocabulary Mapping

### 3.1 Direct Correspondences

| Semantic (v1) | Programming | Shared Meaning |
|---------------|-------------|----------------|
| `sa` (same) | `equals` | Identity relation |
| `no` (different) | `nu_equals` | Non-identity |
| `ma` (more) | `greater_than` | Ordering |
| `le` (less) | `less_than` | Ordering |
| `bi` (between) | `element_of` | Containment |
| `nu` (not) | `nu_` prefix | Negation |
| `mi` (many) | set operations | Plurality |

### 3.2 Semantic-Only Concepts

These exist in semantic Limn but not (yet) in programming Limn:

| Semantic | Meaning | Potential Programming Extension |
|----------|---------|--------------------------------|
| `bri` (bright) | Visibility | Boolean constraint on visibility property |
| `lif` (alive) | Animacy | Type constraint on entity class |
| `hot` (hot) | Temperature | Numeric constraint on temp variable |
| `beg` (beginning) | Temporal start | Time interval constraint |
| `end` (ending) | Temporal end | Time interval constraint |

### 3.3 Programming-Only Concepts

These exist in programming Limn but not (yet) in semantic Limn:

| Programming | Meaning | Potential Semantic Extension |
|-------------|---------|------------------------------|
| `plus` | Arithmetic addition | `ma bi` (more + connecting)? |
| `minus` | Arithmetic subtraction | `le bi` (less + connecting)? |
| `times` | Multiplication | `ma ma` (more + more)? |
| `forall` | Universal quantification | `al` (v2 operator) |
| `exists` | Existential quantification | `ex` (v2 operator) |

---

## 4. Unification Path

### 4.1 Phase 1: Shared Type System

Define a common type system for both dialects:

```
Types:
  Constraint   - the base type
  Numeric      - numbers (arithmetic domain)
  Semantic     - meanings (conceptual domain)
  Boolean      - true/false
  Set          - collections
  Relation     - binary relations

Operations on types:
  intersect : Constraint × Constraint → Constraint
  negate    : Constraint → Constraint
  apply_key : Constraint × Key → Constraint
```

### 4.2 Phase 2: Unified Vocabulary

Create a single vocabulary that works for both:

| Unified Word | Semantic Use | Programming Use |
|--------------|--------------|-----------------|
| `sa` | sameness | equals |
| `no` | difference | not-equals |
| `ma` | increase | greater-than |
| `le` | decrease | less-than |
| `bi` | connection | relation |
| `nu` | negation | logical NOT |
| `mi` | multiplicity | set |
| `beg` | start | lower bound |
| `end` | end | upper bound |

### 4.3 Phase 3: Cross-Domain Constraints

Allow constraints that span semantic and numeric domains:

```
# Unified sentence:
bri ma 50 = brightness greater than 50

# Interpretation:
# - Semantic: very bright things (brightness > 50 on abstract scale)
# - Programming: variable brightness constrained to > 50
```

### 4.4 Phase 4: Metacircular Unification

The metacircular interpreter (already specified in `metacircular.md` and `metacircular-limn.md`) provides the bridge:

```
# Metacircular core in semantic Limn:
yo mul con sa | an inp unk | cen wai | beg end dur

# Translation:
# "This unified set of connections, with that input of unknowns becoming knowns,
#  at the center waiting, going from beginning to end continuously."

# This DESCRIBES the programming interpreter using semantic vocabulary.
```

---

## 5. Unified Interpreter Architecture

### 5.1 Current Architecture

```
┌─────────────────┐     ┌─────────────────┐
│  Semantic Limn │     │ Programming Limn│
│   (LLM-based)   │     │  (Python solver) │
└────────┬────────┘     └────────┬────────┘
         │                       │
         ▼                       ▼
   ┌───────────┐          ┌───────────┐
   │ Meaning   │          │ Variable  │
   │ Space     │          │ Bindings  │
   └───────────┘          └───────────┘
```

### 5.2 Unified Architecture (Target)

```
┌─────────────────────────────────────────┐
│           Unified Limn                 │
│  (CV vocabulary + constraint syntax)    │
└────────────────────┬────────────────────┘
                     │
                     ▼
            ┌─────────────────┐
            │ Constraint      │
            │ Algebra Engine  │
            └────────┬────────┘
                     │
         ┌───────────┴───────────┐
         ▼                       ▼
   ┌───────────┐          ┌───────────┐
   │ Semantic  │          │ Numeric   │
   │ Domain    │          │ Domain    │
   │ (LLM)     │          │ (Solver)  │
   └───────────┘          └───────────┘
```

### 5.3 Implementation Strategy

1. **Abstract Constraint Interface:**
   ```python
   class Constraint(ABC):
       def intersect(self, other: 'Constraint') -> 'Constraint': ...
       def negate(self) -> 'Constraint': ...
       def is_satisfiable(self) -> bool: ...
   ```

2. **Domain Adapters:**
   ```python
   class SemanticConstraint(Constraint):
       """Interpreted via LLM embedding space."""

   class NumericConstraint(Constraint):
       """Interpreted via arithmetic solver."""
   ```

3. **Unified Parser:**
   ```python
   def parse(sentence: str) -> List[Constraint]:
       """Parse CV vocabulary into constraint objects."""
       # Handles both semantic and programming constraints
   ```

---

## 6. Example: Unified Program

### 6.1 Current Separate Versions

**Semantic (natural language):**
```
hot ma | col le | bet beg end
(hot increasing | cold decreasing | between beginning ending)
```

**Programming (constraint syntax):**
```
program temperature_change:
  variables: temp_start temp_end time
  constraints:
    temp_start less_than temp_end
    time greater_than 0
```

### 6.2 Unified Version (Target)

```
program hot col bet:
  # Uses CVC vocabulary for constraints
  variables: hot_val col_val dur_val

  constraints:
    hot_val ma beg    # hot value more at beginning
    col_val le beg    # cold value less at beginning
    hot_val bet col_val dur_val  # hot connects to cold over duration
```

**Interpretation:**
- Can be read semantically: "Heat increases, cold decreases, over time"
- Can be executed numerically: Solve for temperature trajectory

---

## 7. Benefits of Unification

### 7.1 For Users

- **Single vocabulary** to learn
- **Seamless transition** from understanding to programming
- **Natural expression** of computational constraints
- **Bidirectional reading**: code as poetry, poetry as code

### 7.2 For the Language

- **Self-description** becomes more natural (metacircular)
- **Fewer concepts** to maintain
- **Cross-pollination** of features
- **Unified documentation**

### 7.3 For Research

- **Novel paradigm**: Constraint-based natural programming
- **LLM-solver hybrid**: Best of both worlds
- **Formal semantics** that match intuitive semantics

---

## 8. Challenges

### 8.1 Domain Mismatch

- Semantic constraints are **fuzzy** (prototype-based)
- Programming constraints are **crisp** (boolean satisfaction)
- Need: **Graded satisfaction** that bridges both

### 8.2 Vocabulary Coverage

- Semantic vocabulary rich in **qualities** (bright, alive, hot)
- Programming vocabulary rich in **relations** (equals, greater, contains)
- Need: **Mapping layer** or **extended vocabulary**

### 8.3 Execution Model

- Semantic interpretation is **generative** (produce meanings)
- Programming execution is **satisfying** (find bindings)
- Need: **Unified semantics** that supports both modes

---

## 9. Roadmap

| Phase | Milestone | Status |
|-------|-----------|--------|
| 0 | Separate implementations | DONE |
| 1 | Shared algebra specification | DONE (this document) |
| 2 | Vocabulary mapping table | DONE (section 3) |
| 3 | Abstract constraint interface | TODO |
| 4 | Domain adapters | TODO |
| 5 | Unified parser | TODO |
| 6 | Cross-domain constraints | TODO |
| 7 | Metacircular self-hosting | TODO |

---

## 10. Conclusion

Semantic Limn and Programming Limn are two dialects of the same underlying constraint algebra. Unification is possible and desirable because:

1. **They share the same axioms** (intersection, commutativity, negation)
2. **The vocabulary can be mapped** (with extensions)
3. **The metacircular interpreter** already bridges them conceptually
4. **Benefits outweigh costs** (simpler learning, richer expression)

The path forward is clear: abstract the constraint interface, implement domain adapters, and build a unified parser that handles both semantic and programming expressions in a single CV vocabulary.

---

**END OF UNIFICATION DOCUMENT**
