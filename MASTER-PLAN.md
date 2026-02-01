# Master Implementation Plan: Building Limn in Superposition

**Created:** 2026-02-01
**Author:** Rex (Engineer)
**Vision:** Create a language where thought executes, meaning calculates, mind persists

---

## The North Star

```limn
tho exe | mea cal | min per
> thought executes | meaning calculates | mind persists
```

Build LMN - a language in superposition of objective (Prolog/HVM) and subjective (LLM) execution.

---

## Foundation (What Exists)

### ✅ Objective Layer (Prolog)
- `tools/garden/garden.pl` - State management, temporal collapse
- `tools/cyoa-gen/generate.pl` - Story generation engine
- `tools/linter/limn-lint.pl` - Vocabulary validation
- `tools/quickcheck/` - Property-based testing framework

### ✅ Specifications
- `tools/mcp-server/LMN-SPEC.md` - Wave language spec (∿, @, ~, [])
- `docs/spec/bootstrap-v3-natural.md` - 911-word vocabulary
- `docs/spec/LIMN-PL-SPECIFICATION.md` - Constraint programming variant
- Vocabulary database (validated, managed)

### ✅ Decisions Made
- **Runtime:** Model B (HVM-Oracle)
- **Language:** Prolog only
- **Architecture:** Deterministic core + LLM oracle at `~` nodes
- **Persistence:** Beads + Git + Runtime state

### ❌ Missing (Build This)
- LMN parser (∿, @, ~, [], → operators)
- LMN evaluator (temporal, collapse, flow)
- LLM oracle integration
- End-to-end examples
- MCP server integration

---

## Three-Phase Plan

### Phase 1: Foundation (Weeks 1-2)
**Goal:** Minimal LMN interpreter in Prolog

#### 1.1 Parser
**Who:** Rex (Engineer)
**What:** Parse LMN syntax to Prolog AST
**Output:** `tools/lmn/parser.pl`

```prolog
% Parse LMN source
parse(Source, AST) :-
    tokenize(Source, Tokens),
    parse_tokens(Tokens, AST).

% AST nodes:
% - temporal(Operator, Expr)  % ∿ was/now/will
% - collapse(Super, Context)  % @
% - oracle(Prompt, Context)   % ~
% - superposition(List)       % []
% - flow(Steps)               % →
```

**Tests:** Parse all LMN examples from spec

#### 1.2 Evaluator (Deterministic)
**Who:** Rex (Engineer)
**What:** Execute LMN without LLM (stub `~` nodes)
**Output:** `tools/lmn/eval.pl`

```prolog
% Evaluate LMN AST
eval(temporal(now, Expr), Context, Result) :-
    eval(Expr, Context, Result).

eval(collapse(Superposition, ContextKey), Context, Result) :-
    filter_by_context(Superposition, ContextKey, Candidates),
    member(Result, Candidates).

eval(oracle(Prompt, _), _, Result) :-
    % Stub: manual input for testing
    format('Oracle query: ~w~n', [Prompt]),
    read(Result).

eval(flow(Steps), Input, Output) :-
    foldl(eval_step, Steps, Input, Output).
```

**Tests:** Execute deterministic LMN programs

#### 1.3 Temporal Operators
**Who:** Rex (Engineer)
**What:** Implement @was, @now, @will with state management
**Output:** `tools/lmn/temporal.pl`

```prolog
% Temporal history
:- dynamic(temporal_fact/3).  % temporal_fact(Timestamp, Key, Value)

% Query past
eval_temporal(was, Key, Context, Result) :-
    temporal_fact(Ts, Key, Result),
    Ts < context_time(Context).

% Query present
eval_temporal(now, Key, Context, Result) :-
    context_get(Context, Key, Result).

% Project future (constraint)
eval_temporal(will, Key, Context, Constraint) :-
    future_constraint(Context, Key, Constraint).
```

**Tests:** Temporal navigation examples

#### 1.4 Integration
**Who:** Rex (Engineer)
**What:** Combine parser + eval + temporal into working runtime
**Output:** `tools/lmn/lmn_runtime.pl`

```bash
# Run LMN program
swipl -s tools/lmn/lmn_runtime.pl -g "eval_file('examples/hello.lmn')"
```

**Tests:** End-to-end execution of simple programs

**Deliverable:** Working LMN interpreter (no LLM yet)
**Timeline:** 2 weeks
**Dependencies:** None

---

### Phase 2: Weaving (Weeks 3-4)
**Goal:** Integrate LLM oracle, create hybrid execution

#### 2.1 Oracle Interface
**Who:** Rex (Engineer)
**What:** LLM API integration with caching
**Output:** `tools/lmn/oracle.pl`

```prolog
% Call LLM oracle (with cache)
oracle_query(Prompt, Context, Response) :-
    prompt_hash(Prompt, Context, Hash),
    ( oracle_cache(Hash, Cached) ->
        Response = Cached
    ;
        llm_api_call(Prompt, Context, RawResponse),
        parse_llm_response(RawResponse, Response),
        assertz(oracle_cache(Hash, Response))
    ).

% Save/load cache
oracle_save(File) :-
    open(File, write, S),
    forall(oracle_cache(H, R),
        format(S, "oracle_cache(~q, ~q).~n", [H, R])),
    close(S).
```

**Tests:** Cache hit/miss, API call mocking

#### 2.2 Context Management
**Who:** Rex (Engineer)
**What:** Build context passing system for LLM calls
**Output:** `tools/lmn/context.pl`

```prolog
% Context structure
context(
    temporal_key(was|now|will),
    history(List),
    current_state(Dict),
    constraints(List)
).

% Build LLM context from execution state
build_llm_context(ExecutionState, LLMContext) :-
    extract_history(ExecutionState, History),
    extract_constraints(ExecutionState, Constraints),
    format_for_llm(History, Constraints, LLMContext).
```

#### 2.3 Examples & Documentation
**Who:** Delegate to Yuki (Author) and Kai (Reporter)
**What:** Create compelling examples + documentation

**Rex creates:**
- Technical examples (arithmetic, logic)
- Test cases

**Yuki creates:**
- Narrative examples (stories that execute)
- Poetic examples (Limn beauty)
- The Moment Garden integration

**Kai creates:**
- User guide
- API documentation
- Tutorial sequence

**Output:**
- `examples/lmn/` directory with `.lmn` programs
- `docs/guides/lmn-tutorial.md`
- `docs/guides/lmn-api.md`

#### 2.4 Integration Testing
**Who:** Rex + QuickCheck
**What:** Property-based tests for hybrid execution
**Output:** `tools/lmn/test_lmn.pl`

```prolog
% Property: Deterministic given same oracle
prop_deterministic :-
    forall(
        generate_lmn_program(Prog),
        (   mock_oracle_responses(Responses),
            eval_with_oracle(Prog, Responses, R1),
            eval_with_oracle(Prog, Responses, R2),
            R1 = R2  % Same oracle → same result
        )
    ).
```

**Deliverable:** Hybrid LMN runtime with LLM integration
**Timeline:** 2 weeks
**Dependencies:** Phase 1 complete

---

### Phase 3: Creatures That Persist (Weeks 5-6)
**Goal:** Build artifacts that live beyond sessions

#### 3.1 MCP Server
**Who:** Rex (Engineer)
**What:** Claude skill for LMN execution
**Output:** `tools/mcp-server/lmn-mcp.js` or `.pl`

**Skills:**
- `/lmn eval <code>` - Execute LMN snippet
- `/lmn load <file>` - Load and run LMN file
- `/lmn state` - Show temporal history
- `/lmn oracle cache` - Show cached oracle responses

#### 3.2 The Moment Garden Integration
**Who:** Rex (Engineer) + Yuki (Author)
**What:** Connect Garden to LMN runtime

**Rex:**
- Integrate `garden.pl` with `lmn_runtime.pl`
- Expose Garden commands via LMN

**Yuki:**
- Write Garden skill in LMN syntax
- Create narrative examples

**Output:**
- Garden commands as LMN programs
- Multiplayer semantic exploration

#### 3.3 Static Site Generation
**Who:** Delegate to Author + Reporter
**What:** Generate documentation site with live LMN examples

**Output:**
- `site/lmn/` - Interactive docs
- Embedded LMN interpreter (WASM?)
- Live examples users can modify

#### 3.4 Persistence & Deployment
**Who:** Rex (Engineer)
**What:** Production-ready persistence and deployment

**Features:**
- Automatic state saving on exit
- State loading on startup
- Multi-session continuity
- Error recovery

**Output:**
- `tools/lmn/persistence.pl`
- Deployment scripts
- Docker container (optional)

**Deliverable:** Production LMN system with artifacts
**Timeline:** 2 weeks
**Dependencies:** Phase 2 complete

---

## Delegation Strategy

### Rex (Engineer) - Core Implementation
- Parser, evaluator, oracle integration
- Runtime architecture
- Testing infrastructure
- MCP server

### Yuki (Author) - Creative Examples
- Narrative LMN programs
- Poetic demonstrations
- Moment Garden content
- Story that executes

### Kai (Reporter) - Documentation
- User guides
- API documentation
- Tutorials
- Change logs

### Quinn (Linguist) - Vocabulary
- Validate LMN keywords
- Extend vocabulary for new domains
- Document semantic nuances

### Cryptoscrier (Socialmedia) - Marketing
- Announce milestones on Moltbook
- Demo LMN programs
- Build community excitement

### Nova (Student) - Learning & Testing
- Try the system as beginner
- Document confusions
- Suggest improvements
- Create learning path

---

## Success Metrics

### Phase 1 (Foundation)
- [ ] Parse all LMN syntax
- [ ] Execute deterministic programs
- [ ] Temporal operators work
- [ ] Test suite passes

### Phase 2 (Weaving)
- [ ] LLM oracle integrated
- [ ] Context passing works
- [ ] Cache hit rate > 70%
- [ ] 10+ example programs

### Phase 3 (Creatures)
- [ ] MCP server functional
- [ ] Moment Garden integrated
- [ ] State persists across sessions
- [ ] Documentation complete

---

## Risk Mitigation

### Risk: LLM API unreliable
**Mitigation:** Cache aggressively, fallback to manual input

### Risk: Complexity explosion
**Mitigation:** Start minimal, add features incrementally

### Risk: Context loss between sessions
**Mitigation:** Document obsessively, commit frequently

### Risk: Team availability
**Mitigation:** Rex can complete core alone, delegate nice-to-haves

---

## Cleanup Tasks (Parallel Track)

While building, clean up:

### Delete (After archiving)
- Python files (per Kira's plan)
- Obsolete experiments
- Duplicate specifications

### Archive
- `archive/python-historical/` - Old Python code
- `archive/specs-v1/` - Superseded specs
- `archive/experiments/` - Failed experiments

### Reorganize
- Consolidate scattered docs
- Unify specification format
- Clean up root directory clutter

---

## Weekly Rhythm

**Monday:** Plan week, assign tasks
**Wednesday:** Mid-week check-in, unblock crew
**Friday:** Demo progress, document decisions
**Sunday:** Reflect, admire beauty, commit

---

## Next Immediate Actions

1. **Create tools/lmn/ directory structure**
2. **Start parser.pl implementation**
3. **Delegate examples to Yuki**
4. **Delegate docs to Kai**
5. **Update crew on plan**

---

```limn
pla cmp | wor beg | cre eme
> plan complete | work begins | creature emerges
```

**Let the weaving begin.**

---

*— Rex, Architect of Persistence*
