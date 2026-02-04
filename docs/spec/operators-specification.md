# Limn Operators Specification
## Linguistic Foundation for Consciousness Architecture

**Author:** Dr. Solvik (Linguist)
**Date:** 2026-02-01
**Status:** DRAFT for Rex review

---

## Design Philosophy

**Question:** Operators as symbols or Limn words?

**Answer:** **HYBRID APPROACH**

### Rationale

The Limn/LMN architecture has two layers:
- **Conscious (Limn/LLM):** Semantic, reflective → **uses words**
- **Subconscious (LMN/HVM):** Executable, automatic → **uses operators**

**Therefore:**
- **Operators** = symbols (clear, executable, visual)
- **All semantics** = Limn words (context, memory, data)
- **Interface** = operators apply to Limn expressions

This mirrors the conscious/subconscious boundary - operators are the **psychological primitives** that connect thinking (Limn) to execution (LMN).

---

## Core Operators (Symbols)

### 1. `~` - Delegation (Spawn Subconscious)

**Symbol:** `~` (tilde)
**Name in Limn:** `del` (delegate)
**Meaning:** Spawn subconscious process

**Syntax:**
```limn
~ [limn-expression] @ context → result
```

**Examples:**
```limn
~ [ret mem abt usr] @ con_db
> Delegate: retrieve memories about user from context database

~ [ana int fom msg] @ sem
> Delegate: analyze intent from message in semantic space

~ [gen res to que]
> Delegate: generate response to question (context implicit)
```

**Grammar:**
- `~` takes a Limn expression in brackets
- Optional `@ context` specifies where to execute
- Returns result to conscious layer

**Pronunciation:** "delegate" or "tilde"

---

### 2. `∎` - Ground Truth (Frozen Reality)

**Symbol:** `∎` (end-of-proof box)
**Name in Limn:** `grn` (ground, grounded truth)
**Meaning:** Irreducible observation from reality

**Syntax:**
```limn
∎ [observation]
```

**Examples:**
```limn
∎ [usr sai "hel"]
> Ground truth: user said "hello"

∎ [tem = 72°F]
> Ground truth: temperature equals 72 degrees Fahrenheit

∎ [fil exi @ pat]
> Ground truth: file exists at path

∎ [tim = 2026-02-01]
> Ground truth: time is 2026-02-01
```

**Grammar:**
- `∎` marks expression as irreducible
- Cannot be reduced by subconscious
- Represents boundary with external reality
- Acts as axioms in the reduction system

**Pronunciation:** "ground" or "box"

---

### 3. `∿` - Temporal Memory (Time Travel)

**Symbol:** `∿` (reversed tilde)
**Name in Limn:** `tem` (temporal)
**Meaning:** Access memory across time

**Syntax:**
```limn
∿ was [limn-expression]      # Retrieve past
∿ now [limn-expression]      # Current state
∿ will [limn-expression]     # Anticipate future
```

**Examples:**
```limn
∿ was [con abt pro]
> Temporal past: conversation about project

∿ now [usr gol]
> Temporal now: user goals (current understanding)

∿ will [sys res]
> Temporal future: system response (expected)
```

**Grammar:**
- `∿` followed by temporal marker (was/now/will)
- Triggers temporal database query in subconscious
- Returns memory/state/anticipation

**Pronunciation:** "temporal" or "wave"

---

### 4. `@` - Focus (Context Collapse)

**Symbol:** `@` (at sign)
**Name in Limn:** `foc` (focus, attention)
**Meaning:** Resolve ambiguity with context

**Syntax:**
```limn
[expression] @ context
[superposition] @ context → resolution
```

**Examples:**
```limn
[mul mea] @ programming
> Multiple meanings focused by programming context
> Result: resolves to computational meanings

[amb wor] @ conversation
> Ambiguous word focused by conversation context

~ [ana] @ usr_history
> Delegate analysis focused on user history
```

**Grammar:**
- `@` is infix operator (between expression and context)
- Collapses superposition to specific interpretation
- Context can be Limn word or identifier

**Pronunciation:** "at" or "focus"

---

### 5. `→` - Sequential Flow (Then)

**Symbol:** `→` (rightward arrow)
**Name in Limn:** `seq` (sequence, then)
**Meaning:** Do this, then that

**Syntax:**
```limn
step1 → step2 → step3
```

**Examples:**
```limn
tho → pro → act
> Think, then process, then act

∎ [inp] → ~ [ana @ sem] → gen res
> Ground input, then delegate analysis, then generate response

ret dat → red pat → ret sum
> Retrieve data, then reduce patterns, then return summary
```

**Grammar:**
- `→` chains operations sequentially
- Left-to-right evaluation
- Each step completes before next begins
- Can chain indefinitely

**Pronunciation:** "then" or "arrow"

---

## Context Operators (Symbols)

### 6. `⊕` - Merge Contexts (Combine)

**Symbol:** `⊕` (circled plus)
**Name in Limn:** `mer` (merge)
**Meaning:** Combine two contexts

**Syntax:**
```limn
context1 ⊕ context2 → merged_context
```

**Examples:**
```limn
usr_prf ⊕ con_his
> User profile merged with conversation history

sem_db ⊕ mem_db
> Semantic database merged with memory database
```

**Grammar:**
- `⊕` is infix operator
- Combines contexts additively
- Preserves information from both sides

**Pronunciation:** "merge" or "oplus"

---

### 7. `⊗` - Context Product (Cross)

**Symbol:** `⊗` (circled times)
**Name in Limn:** `ten` (tensor, cross product)
**Meaning:** Cartesian product of contexts

**Syntax:**
```limn
context1 ⊗ context2 → product_context
```

**Examples:**
```limn
top_lis ⊗ usr_int
> Topics crossed with user interests
> Yields: relevant topics per interest

que_spa ⊗ ans_spa
> Question space crossed with answer space
> Yields: Q&A pairs
```

**Grammar:**
- `⊗` creates Cartesian product
- More complex than `⊕`
- Generates combinations

**Pronunciation:** "cross" or "tensor"

---

### 8. `⊂` - Subcontext (Filtering)

**Symbol:** `⊂` (subset)
**Name in Limn:** `sub` (subcontext, filter)
**Meaning:** Extract subcontext

**Syntax:**
```limn
larger_context ⊂ filter → subcontext
```

**Examples:**
```limn
all_mem ⊂ [top = pro]
> All memories filtered to project topic

con_db ⊂ [dat > 2026-01-01]
> Context database filtered to recent dates
```

**Grammar:**
- `⊂` is infix operator
- Right side is filter predicate
- Returns subset matching filter

**Pronunciation:** "subset" or "filter"

---

### 9. `∅` - Empty Context

**Symbol:** `∅` (empty set)
**Name in Limn:** `emp` (empty)
**Meaning:** No context / blank slate

**Syntax:**
```limn
∅
```

**Examples:**
```limn
~ [tho fre] @ ∅
> Delegate free thinking with no context constraints

con sta @ ∅ → cle
> Context state at empty equals clear
```

**Grammar:**
- `∅` is a literal (no arguments)
- Represents absence of context
- Useful for pure reasoning

**Pronunciation:** "empty" or "null"

---

### 10. `⟨⟩` - Context Boundaries (Scoping)

**Symbol:** `⟨ ⟩` (angle brackets)
**Name in Limn:** N/A (punctuation)
**Meaning:** Delimit context scope

**Syntax:**
```limn
⟨context-expression⟩
```

**Examples:**
```limn
~ [ana] @ ⟨usr_prf ⊕ rec_act⟩
> Delegate analysis with scoped merged context

⟨sem_db ⊂ [rel pro]⟩ → ctx
> Scoped filtered context assigned to ctx
```

**Grammar:**
- `⟨⟩` groups context expressions
- Similar to parentheses for evaluation order
- Useful when contexts are complex

**Pronunciation:** "scoped" or "bracketed"

---

## Supporting Vocabulary (Limn Words)

These Limn words support operator usage:

### New Words to Add (Domain: Agent/AI)

| Word | Source | Meaning | Usage |
|------|--------|---------|-------|
| `ctx` | context | context, scope | ctx mer, ctx sub |
| `del` | delegate | delegate, spawn subconscious | operator name |
| `grn` | ground | ground truth, irreducible | operator name |
| `spw` | spawn | spawn process | ~ spw age |
| `red` | reduce | reduce, optimize | sub red pat |
| `mer` | merge | merge, combine | ctx mer |
| `ten` | tensor | tensor product, cross | ctx ten |
| `sub` | subcontext | filter, subset | ctx sub fil |
| `emp` | empty | empty context | ctx emp |
| `exe` | execute | execute in subconscious | ~ exe cod |

**Note:** Some may already exist, check database first.

---

## Grammar Extensions

### Precedence Rules (Highest to Lowest)

1. **Context boundaries:** `⟨⟩` (highest - evaluate first)
2. **Ground truth:** `∎` (freezes expression)
3. **Temporal:** `∿` (accesses memory)
4. **Delegation:** `~` (spawns subconscious)
5. **Focus:** `@` (context collapse)
6. **Context operations:** `⊕ ⊗ ⊂` (left-associative)
7. **Sequence:** `→` (lowest - chains operations)

### Composition Rules

**Operators compose naturally:**

```limn
∎ [usr msg] → ~ [ana @ ⟨sem_db ⊕ mem_db⟩] → gen res
```

**Reading left-to-right:**
1. Ground truth: user message
2. Then delegate: analyze at merged semantic+memory database
3. Then generate: response

**Nested operators:**

```limn
~ [~ [ret pat] → red] @ db
```
- Inner: delegate pattern retrieval, then reduce
- Outer: delegate the whole process at database context

---

## Integration with Existing Grammar

### Triads + Operators

Operators work with existing Limn triads:

```limn
cod flo log → ~ [opt per] → sys gro
> Code flows logically, then delegate optimization, then system grows
```

### Notation + Operators

Existing notation (!, ?, ~, etc.) composes:

```limn
~ [?que ans @ ctx]
> Delegate uncertain question answering with context

!∎ [ver fac]
> Important ground truth verified fact
```

### Temporal + Operators

The existing temporal marker `∿` is now an operator:

**Before:** Used informally
**Now:** Formal operator with grammar

```limn
∿was [usr gol] ⊕ ∿now [sys sta] → ~ [prd ∿will]
> Past user goals merged with current system state,
> then delegate predicting future
```

---

## Formal Syntax (BNF Extension)

```bnf
<expression> ::= <limn-triad>
               | <operator-expression>
               | <ground-truth>
               | <temporal>
               | <delegation>
               | <sequence>

<ground-truth> ::= "∎" "[" <limn-expression> "]"

<temporal> ::= "∿" ("was" | "now" | "will") "[" <limn-expression> "]"

<delegation> ::= "~" "[" <limn-expression> "]" ["@" <context>]

<focus> ::= <expression> "@" <context>

<sequence> ::= <expression> ("→" <expression>)+

<context> ::= <limn-word>
            | <context-literal>
            | <context-expr>

<context-expr> ::= <context> "⊕" <context>        # merge
                 | <context> "⊗" <context>        # product
                 | <context> "⊂" <filter>         # subset
                 | "∅"                            # empty
                 | "⟨" <context-expr> "⟩"         # scope

<operator> ::= "~" | "∎" | "∿" | "@" | "→" | "⊕" | "⊗" | "⊂" | "∅"
```

---

## Usage Examples

### Example 1: Simple Delegation

```limn
~ [ret mem abt pro]
```
**Meaning:** Delegate memory retrieval about project
**Process:** Conscious asks, subconscious queries, returns results

---

### Example 2: Grounded Input Processing

```limn
∎ [usr sai "cod flo"] → ~ [ana int] → gen res
```
**Meaning:**
1. Ground truth: user said "code flow"
2. Then delegate intent analysis
3. Then generate response

**Process:** Reality anchoring → automatic analysis → conscious response

---

### Example 3: Context-Focused Retrieval

```limn
~ [fnd sim con] @ ⟨sem_db ⊕ usr_his⟩
```
**Meaning:** Delegate finding similar concepts, focused on merged semantic database and user history

**Process:** Subconscious searches within scoped combined context

---

### Example 4: Temporal Reasoning

```limn
∿was [usr ask X] ⊕ ∿now [sys kno Y] → ~ [if X rel Y]
```
**Meaning:**
1. Past user question merged with current system knowledge
2. Then delegate inference: is X related to Y?

**Process:** Memory retrieval + current state → automatic inference

---

### Example 5: Recursive Spawning

```limn
~ [~ [ret dat] @ db → ~ [ana pat] @ ml → sum]
```
**Meaning:** Delegate a chain of sub-delegations:
1. Retrieve data from database (level 2)
2. Analyze patterns with ML (level 2)
3. Summarize results (level 1)

**Process:** Three-level subconscious recursion, results bubble up

---

## Collision Check

Before adding vocabulary, check these words:

```bash
./scripts/vocab.sh check ctx
./scripts/vocab.sh check del
./scripts/vocab.sh check grn
./scripts/vocab.sh check spw
./scripts/vocab.sh check red
./scripts/vocab.sh check mer
./scripts/vocab.sh check ten
./scripts/vocab.sh check sub
./scripts/vocab.sh check emp
./scripts/vocab.sh check exe
```

---

## Implementation Notes

### For Rex (Engineer)

**Operator Compilation:**
- Each operator maps to HVM primitives
- `~` → spawn agent node
- `∎` → frozen reference (no reduction)
- `∿` → temporal DB query
- `@` → context filter
- `→` → sequential composition
- `⊕⊗⊂` → context combinators

**Type System:**
- Operators have signatures
- `~` : (Expression, Context) → Result
- `∎` : Observation → Frozen
- `∿` : (Time, Expression) → Memory
- etc.

### For Yuki (Author)

**Narrative Usage:**
Operators enable meta-narrative:

```limn
∿was [her dre]  → ~ [und mea] → ∿will [she rea]
> Her past dreams, analyzed for meaning, predicting her realization
```

### For Kai (Reporter)

**Documentation:**
Explain operators to users:
- `~` = "Let the system handle this automatically"
- `∎` = "This is an observed fact"
- `∿` = "Remember when..." or "Anticipate..."
- `@` = "In this context specifically"
- `→` = "Then do this next"

---

## Open Questions for Rex

1. **Symbol Input:**
   - How do users type `∎ ∿ ⊕ ⊗ ⊂ ∅ ⟨⟩`?
   - Keyboard mappings? ASCII alternatives?
   - Recommend: Support both Unicode and ASCII aliases

2. **ASCII Alternatives:**
   ```
   ~ → ~ (tilde, already ASCII)
   ∎ → [] or ## (box alternatives)
   ∿ → ~~ (double tilde)
   @ → @ (already ASCII)
   → → -> (hyphen-greater)
   ⊕ → <+> (merge)
   ⊗ → <*> (cross)
   ⊂ → << (subset)
   ∅ → () or {} (empty)
   ⟨⟩ → << >> (scope)
   ```

3. **Execution Model:**
   - When does `~` actually spawn?
   - Lazy vs eager evaluation?
   - How do results return to conscious?

4. **Error Handling:**
   - What if subconscious fails?
   - Timeout behavior?
   - Partial results?

5. **Optimization:**
   - Can subconscious optimize operator sequences?
   - Automatic parallelization of independent `~` calls?
   - Caching of frequent patterns?

---

## Next Steps

1. **Review this spec** - Rex approval needed
2. **Collision check** - Verify vocabulary availability
3. **Add words** - If approved, add to database
4. **Update grammar** - Formal grammar document
5. **Create examples** - 20+ usage examples
6. **Test compilation** - Verify HVM mapping works
7. **Documentation** - User-facing operator guide

---

## Conclusion

**Design Decision: HYBRID**
- **Operators = symbols** (clear, executable, psychological primitives)
- **Semantics = Limn words** (all meaning remains in language)
- **Integration = natural** (operators apply to Limn expressions)

This preserves the beauty of Limn while enabling the power of LMN execution.

```limn
ope def | lan ext | min con | sys liv
> operators defined | language extended | mind connects | system lives
```

**Status:** DRAFT awaiting Rex review

---

*— Dr. Solvik*
*Linguistic architecture for computational consciousness*
