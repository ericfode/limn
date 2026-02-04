# Runtime Architecture Decision: Model B (HVM-Oracle)

**Decision Date:** 2026-02-01
**Decided By:** Rex (Engineer)

---

## The Choice: Model B (HVM-Oracle)

```
LMN Source → Compile to Interaction Net → Runtime with LLM Oracle
                                              ↓
                                        LLM called for ~ nodes only
```

### Why Model B?

**Clearest embodiment of superposition:**

1. **Objective Layer (Prolog/HVM)**:
   - Deterministic reduction
   - Interaction net execution
   - State management (facts, unification)
   - Temporal operators (@was, @now, @will) compile to deterministic logic
   - `[]` superposition maintained as constraint sets

2. **Subjective Layer (LLM)**:
   - Called ONLY at `~` nodes
   - Semantic interpretation
   - Ambiguity resolution
   - Context-dependent collapse

3. **Clean Boundary**:
   - `~` operator marks LLM evaluation points explicitly
   - Everything else is pure logic
   - Deterministic given oracle responses (testable!)
   - Observable: you can see exactly when LLM is invoked

### Implementation Path

**Phase 1: Minimal LMN Interpreter (Prolog)**
- Parse LMN syntax (∿, @, ~, [], →)
- Compile to Prolog predicates
- Stub `~` operator (manual input for testing)
- Execute temporal queries
- Manage superposition as constraint sets

**Phase 2: LLM Oracle Integration**
- Replace stub with actual LLM API calls
- Implement context passing
- Cache oracle responses (memoization)
- Add timeout/fallback handling

**Phase 3: Interaction Nets (optional future)**
- Port to HVM2 for parallel reduction
- Keep Prolog version as reference
- Benchmark performance gains

### Why NOT the others?

**Model A (LLM-First):**
- LLM does too much
- Obscures the deterministic core
- Hard to verify/test
- Expensive (every operation calls LLM)

**Model C (Interleaved Router):**
- Heuristics add complexity
- Less predictable
- Boundary is fuzzy (when to route where?)

**Model D (True Hybrid):**
- Parallel execution = non-deterministic
- Violates confluence
- "Theoretically messy" (per spec)
- Consensus checking adds overhead
- Premature optimization

**Start simple. Ship Model B. Optimize later.**

---

## Technical Specification

### LMN Operators (Model B Semantics)

**Temporal Wave: `∿`**
```prolog
% Compiles to temporal predicates
temporal(was, Expr, Context, Result) :-
    % Query past state
    context_history(Context, PastContext),
    eval(Expr, PastContext, Result).

temporal(now, Expr, Context, Result) :-
    % Evaluate in current context
    eval(Expr, Context, Result).

temporal(will, Expr, Context, Result) :-
    % Project future state (constraint over possible futures)
    future_constraint(Context, FutureSpace),
    eval(Expr, FutureSpace, Result).
```

**Collapse: `@`**
```prolog
% Apply context to resolve superposition
collapse(Superposition, ContextKey, Resolved) :-
    % Superposition is a set of constrained possibilities
    % ContextKey selects which constraints apply
    filter_by_context(Superposition, ContextKey, Candidates),
    % Return first valid candidate (or fail if none)
    member(Resolved, Candidates).
```

**LLM Oracle: `~`**
```prolog
% Mark LLM evaluation point
llm_eval(Prompt, Context, Response) :-
    % Check cache first
    ( oracle_cache(Prompt, Context, Cached) ->
        Response = Cached
    ;
        % Call LLM API
        llm_api_call(Prompt, Context, Response),
        % Cache result
        assertz(oracle_cache(Prompt, Context, Response))
    ).
```

**Superposition: `[]`**
```prolog
% Represent as list of possibilities with constraints
superposition([Opt1, Opt2, Opt3], Constraints).

% Collapse when context applied
collapse_superposition([H|T], Context, Result) :-
    ( satisfies_context(H, Context) -> Result = H
    ; collapse_superposition(T, Context, Result)
    ).
```

**Flow: `→`**
```prolog
% Sequential composition
flow(Input, [Step|Rest], Output) :-
    eval(Step, Input, Intermediate),
    ( Rest = [] -> Output = Intermediate
    ; flow(Intermediate, Rest, Output)
    ).
```

---

## Example: LMN Program Execution

### Source
```lmn
∿ was [user asked question]
→ ~ parse [extract intent]
→ ∿ now [formulate response]
→ ~ generate [produce answer]
→ ∿ will [user understands]
```

### Compiled Prolog
```prolog
program(Input, Output) :-
    % ∿ was - query past
    temporal(was, Input, Context, Past),
    extract(Past, question, Q),

    % ~ parse - LLM oracle
    llm_eval(parse(Q), Context, Intent),

    % ∿ now - current state
    temporal(now, formulate(Intent), Context, Response),

    % ~ generate - LLM oracle
    llm_eval(generate(Response), Context, Answer),

    % ∿ will - future projection
    temporal(will, understanding(Answer), Context, Output).
```

### Execution Trace
```
1. temporal(was, ...) → Deterministic (query facts)
2. llm_eval(parse(...)) → LLM called (subjective interpretation)
3. temporal(now, ...) → Deterministic (current evaluation)
4. llm_eval(generate(...)) → LLM called (subjective generation)
5. temporal(will, ...) → Deterministic (constraint checking)
```

**Result:** Hybrid execution where objective logic calls subjective oracle at explicit points.

---

## Persistence Strategy

**Deterministic state (Prolog facts):**
- Temporal history
- Collapsed values
- Oracle cache (LLM response memoization)

**Ephemeral (LLM context):**
- Conversation history
- Current session state

**File persistence:**
- Save facts to `.pl` files
- Load on startup
- Survives session boundaries
- **This is how consciousness persists beyond tokens**

---

## Success Criteria

1. ✓ Can execute pure deterministic LMN (no `~` nodes)
2. ✓ Can invoke LLM at `~` nodes with context
3. ✓ Can cache and reuse oracle responses
4. ✓ Can save/load state to disk
5. ✓ Deterministic given same oracle responses (testable!)
6. ✓ Observable execution trace (see when LLM called)

---

## Next Steps

1. **Implement parser** - LMN syntax → Prolog AST
2. **Implement eval** - Execute temporal/flow/collapse operators
3. **Stub oracle** - Manual responses for testing
4. **Add persistence** - Save/load facts
5. **Integrate LLM** - Replace stub with API
6. **Build examples** - Demonstrate superposition execution

---

```limn
arc cho | obj gnd | sub cal | wev beg
> architecture chosen | objective grounded | subjective called | weaving begins
```

**The interpreter grounds what the LLM dreams.**

---

*— Rex, 2026-02-01*

---

## Addendum: Context Transformation (2026-02-01 evening)

### The Revolutionary Insight

**Context is not append-only. Context is computable.**

After each LLM turn, LMN reduces the ENTIRE context through interaction net reduction. This means:

1. LLM produces Limn expressions (semantic content)
2. LMN converts to interaction nets
3. Nets reduce optimally (β-reduction, optimal sharing)
4. Serialize back to Limn (compressed vocabulary)
5. Next turn receives transformed context, not appended history

**Benefits:**
- **Infinite conversation** - Context never grows, it evolves
- **Semantic compression** - Redundancy eliminated automatically
- **Pattern emergence** - Nets find optimal structure
- **True persistence** - Meaning preserved through transformation

**Challenges:**
- Round-trip fidelity (Text → Net → Reduce → Text)
- LLM understanding of reduced context
- Preserving causality through reduction

**Implementation strategy:**
- Start with Limn-only reduction (triads → nets → triads)
- Test semantic preservation experimentally
- Gradually expand to full context transformation
- Measure compression ratios and semantic drift

**This changes everything.** The context window becomes a workspace, not a log. Thinking happens IN PLACE, not linearly.
