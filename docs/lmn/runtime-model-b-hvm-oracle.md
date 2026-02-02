# LMN Runtime Model B: HVM-First with LLM Oracle

**Status:** Exploratory simulation
**Date:** 2026-02-01
**Author:** Polecat dag

## Executive Summary

This document specifies a runtime architecture for LMN (Limn Programming Language) that leverages the Higher-order Virtual Machine (HVM) as the primary execution engine, with an LLM serving as an oracle for resolving semantic ambiguity. The core insight is that LMN's ambiguity operator (∿) naturally maps to a special node type in interaction nets, triggering oracle consultation when reduction encounters it.

## 1. Architecture Overview

```
┌─────────────────────────────────────────────────────────┐
│                    LMN Source Code                       │
└─────────────────────────┬───────────────────────────────┘
                          │
                          ▼
                   ┌──────────────┐
                   │   Compiler   │
                   └──────┬───────┘
                          │
                          ▼
        ┌─────────────────────────────────┐
        │    Interaction Net with ∿       │
        │  (HVM-compatible format)        │
        └────────────┬────────────────────┘
                     │
                     ▼
        ┌────────────────────────────────┐
        │    HVM Reduction Engine        │
        │  - Reduces normal nodes        │
        │  - Detects ∿ (ambiguity)       │
        └────┬──────────────────────┬────┘
             │                      │
             │ Normal               │ Hit ∿
             │ reduction            │
             ▼                      ▼
        ┌─────────┐         ┌──────────────┐
        │  Result │         │ Oracle Call  │
        └─────────┘         └──────┬───────┘
                                   │
                                   ▼
                            ┌──────────────┐
                            │  LLM Oracle  │
                            │  - Context   │
                            │  - Collapse  │
                            └──────┬───────┘
                                   │
                                   ▼
                            ┌──────────────┐
                            │ Substituted  │
                            │   subgraph   │
                            └──────┬───────┘
                                   │
                                   ▼
                        Resume HVM reduction
```

### Key Design Principles

1. **HVM is primary**: All deterministic computation happens in HVM's interaction net
2. **∿ as synchronization point**: Ambiguity nodes are explicit barriers requiring oracle
3. **Oracle is pure function**: Given context, returns deterministic substitution
4. **Resumption is seamless**: Oracle result grafts back into the net

## 2. Compilation: LMN → Interaction Net

### 2.1 Base Language Compilation

LMN compiles to interaction nets using standard term encoding:

**Example: Simple term**
```limn
fir[lov,pai]
```

**Compiles to:**
```
     APP
    /   \
  fir    CON
        /   \
      lov   pai
```

As interaction net (nodes + ports):
```
NODE fir#0 [IN, OUT]
NODE con#0 [IN, LEFT, RIGHT]
NODE lov#0 [IN, OUT]
NODE pai#0 [IN, OUT]

WIRE fir#0.OUT → con#0.IN
WIRE con#0.LEFT → lov#0.OUT
WIRE con#0.RIGHT → pai#0.OUT
```

### 2.2 The Ambiguity Node (∿)

The ∿ operator compiles to a special node type: **AMB** (ambiguity node).

**LMN source:**
```limn
∿lov[joy,pai,hop]
```

**Interaction net:**
```
       AMB
      /   \
  ORACLE  ARGS
           |
         CON3
        / | \
      joy pai hop
```

**Node structure:**
```prolog
% AMB node structure
amb_node(
    node_id: amb#42,
    ports: [
        oracle_port,    % Where oracle result will be grafted
        args_port       % Arguments to pass to oracle
    ],
    metadata: {
        source_location: "line 15, col 8",
        context_scope: [var_bindings, type_env],
        reduction_path: [node_ids...]
    }
).
```

### 2.3 Critical Properties

1. **AMB is a blocker**: HVM reduction cannot proceed through AMB
2. **AMB is lazy**: Only evaluated when its result is needed
3. **AMB carries context**: Metadata includes everything needed for oracle call

## 3. Reduction: HVM Until ∿

### 3.1 Normal Reduction

HVM performs standard interaction net reduction:

```
# Beta reduction
(λx.M N) → M[x := N]

# Interaction rules
(dup(a, b) con(x, y)) → (con(dup(a, b) x, dup(a, b) y))
```

**Crucially:** HVM reduction is deterministic, parallel, and cache-friendly.

### 3.2 Hitting an AMB Node

When reduction tries to interact with AMB:

```
STEP 1: Normal reduction
    APP(f, x) → reduce to normal form

STEP 2: Result needs AMB node
    needs(amb#42.oracle_port)

STEP 3: HVM pauses, yields control
    state = {
        net: current_interaction_net,
        blocked_on: amb#42,
        continuation: resume_address
    }
```

**HVM does NOT:**
- Try to reduce through AMB
- Guess at what AMB might mean
- Skip AMB and continue elsewhere (if AMB result is needed)

**HVM DOES:**
- Detect AMB dependency
- Package context
- Yield to runtime supervisor
- Wait for substitution

## 4. Oracle Protocol

### 4.1 Context Packaging

When HVM hits AMB, the runtime packages context:

```json
{
  "oracle_request": {
    "amb_node_id": "amb#42",
    "expression": "∿lov[joy,pai,hop]",
    "type_expected": "emotion_state",
    "scope": {
      "lexical_bindings": {
        "@now": "temporal_anchor",
        "@self": "agent_alice"
      },
      "type_environment": {
        "lov": "emotion_family",
        "joy": "positive_valence",
        "pai": "negative_valence",
        "hop": "anticipation"
      }
    },
    "reduction_trace": [
      "main_entry",
      "if_branch_true",
      "map_over_states"
    ],
    "parallel_context": {
      "other_amb_nodes": ["amb#17", "amb#55"],
      "shared_variables": ["@now"]
    }
  }
}
```

### 4.2 Oracle Execution

The LLM oracle receives this structured context and performs semantic collapse:

**Oracle implementation (pseudocode):**
```python
def oracle_collapse(request):
    """
    Collapses ambiguity to a concrete term.

    The LLM has been trained/prompted with:
    - LMN semantics (constraint intersection model)
    - Type system rules
    - Contextual binding resolution
    """
    prompt = f"""
    Given LMN ambiguity expression: {request.expression}

    Context:
    - Expected type: {request.type_expected}
    - Lexical scope: {request.scope.lexical_bindings}
    - Type constraints: {request.scope.type_environment}
    - Execution path: {request.reduction_trace}

    The expression ∿lov[joy,pai,hop] requires selecting one emotion
    from the set based on constraint intersection.

    With @now = temporal_anchor and execution in map_over_states,
    collapse to the most contextually appropriate concrete term.

    Return: Single LMN term (no ambiguity)
    """

    llm_response = llm.complete(prompt)

    # Parse LLM output to interaction net fragment
    result_term = parse_limn(llm_response)
    result_net = compile_to_net(result_term)

    return {
        "amb_node_id": request.amb_node_id,
        "substitution": result_net,
        "confidence": 0.95,
        "alternatives": ["pai", "hop"],  # for debugging
        "reasoning": "joy selected due to @now=present+anticipation context"
    }
```

### 4.3 Oracle Response Format

```json
{
  "oracle_response": {
    "amb_node_id": "amb#42",
    "substitution": {
      "node_type": "CONST",
      "value": "joy",
      "ports": {
        "out": "wire_target_xyz"
      }
    },
    "metadata": {
      "confidence": 0.95,
      "alternatives_considered": ["pai", "hop"],
      "reasoning": "Temporal context (@now) + anticipation bias",
      "model": "claude-opus-4.5",
      "tokens_used": 1250
    }
  }
}
```

## 5. Continuation: Re-entering the Net

### 5.1 Substitution Process

The oracle's result must be grafted back into the interaction net:

```
BEFORE (blocked on AMB):
        APP
       /   \
      f    AMB#42
          /     \
     ORACLE    ARGS
                 |
               CON3
              / | \
           joy pai hop

AFTER (AMB replaced with oracle result):
        APP
       /   \
      f    CONST("joy")
           |
          OUT → (original wire target)
```

**Substitution algorithm:**
```python
def substitute_amb(net, oracle_response):
    """
    Replaces AMB node with oracle's collapsed term.
    """
    amb_id = oracle_response.amb_node_id
    new_subgraph = oracle_response.substitution

    # 1. Find all wires connected to AMB node
    amb_node = net.nodes[amb_id]
    connected_wires = net.wires_connected_to(amb_node.oracle_port)

    # 2. Remove AMB node from net
    net.remove_node(amb_id)

    # 3. Graft new subgraph in AMB's place
    net.insert_subgraph(new_subgraph)

    # 4. Rewire: new subgraph output → old AMB wire targets
    for wire in connected_wires:
        wire.source = new_subgraph.output_port

    # 5. Mark net as unblocked
    net.blocked_nodes.remove(amb_id)

    return net
```

### 5.2 Resuming Reduction

After substitution, HVM resumes from exactly where it paused:

```python
def resume_after_oracle(state, oracle_response):
    """
    Continues HVM reduction after oracle substitution.
    """
    # Apply substitution
    net = substitute_amb(state.net, oracle_response)

    # Resume reduction from continuation point
    # (HVM knows which interactions are now unblocked)
    result = hvm.reduce(
        net=net,
        resume_from=state.continuation
    )

    return result
```

### 5.3 Reduction Invariants

**Key properties maintained:**
1. **Determinism**: Given same oracle response, reduction is deterministic
2. **Confluence**: Order of AMB resolution doesn't affect final result (if independent)
3. **Progress**: Every oracle call unblocks at least one reduction path

## 6. Example Trace

Let's walk through: `@now ∿lov[joy,pai,hop] → ?`

### Step 1: Compilation

```limn
@now ∿lov[joy,pai,hop]
```

**Compiles to:**
```
      APP
     /   \
   @now   AMB#5
         /     \
    ORACLE    CON3
             / | \
          joy pai hop
```

**Interaction net:**
```
NODE app#1 [func, arg, out]
NODE now#0 [out] {type: temporal_anchor}
NODE amb#5 [oracle, args, out] {type: AMB}
NODE con3#2 [in, a, b, c, out]
NODE joy#10 [out] {value: "joy"}
NODE pai#11 [out] {value: "pai"}
NODE hop#12 [out] {value: "hop"}

WIRE app#1.func → now#0.out
WIRE app#1.arg → amb#5.out
WIRE amb#5.args → con3#2.out
WIRE con3#2.a → joy#10.out
WIRE con3#2.b → pai#11.out
WIRE con3#2.c → hop#12.out
```

### Step 2: HVM Reduction Begins

```
HVM: Evaluate app#1
  → Need now#0 (easy: constant)
  → Need amb#5 (BLOCKED: AMB node!)

HVM: Cannot proceed. Yield.
```

**State snapshot:**
```json
{
  "blocked_on": "amb#5",
  "needed_by": "app#1",
  "continuation": "apply(@now, <result>)"
}
```

### Step 3: Oracle Call Preparation

**Context packaged:**
```json
{
  "expression": "∿lov[joy,pai,hop]",
  "type_expected": "emotion",
  "scope": {
    "@now": {
      "type": "temporal_anchor",
      "value": "present_moment"
    }
  },
  "usage_context": "application of @now to emotion choice",
  "reduction_path": ["main", "app#1"]
}
```

### Step 4: Oracle Execution

**Oracle reasoning:**
```
Input: ∿lov[joy,pai,hop] in context of @now

Constraint analysis:
- lov: emotion family (positive and negative)
- joy: positive valence, present-focused
- pai: negative valence, past-oriented
- hop: positive valence, future-oriented

@now = present_moment context:
- Eliminates past-oriented (pai)
- Favors present-focused (joy) over future (hop)

Collapse: joy
```

**Oracle response:**
```json
{
  "amb_node_id": "amb#5",
  "substitution": {
    "type": "CONST",
    "value": "joy"
  },
  "confidence": 0.92
}
```

### Step 5: Substitution

```
BEFORE:
      APP
     /   \
   @now   AMB#5

AFTER:
      APP
     /   \
   @now   CONST("joy")
```

### Step 6: Resume Reduction

```
HVM: Resume from app#1
  → func = @now (temporal_anchor)
  → arg = "joy" (emotion)
  → Result: temporally_anchored_emotion("joy", present)

FINAL RESULT: joy@now
```

**Complete trace:**
```
t=0:  @now ∿lov[joy,pai,hop]
      ↓ (compile)
t=1:  APP(@now, AMB#5)
      ↓ (reduce, hit AMB)
t=2:  [BLOCKED] → oracle_call
      ↓ (oracle: context → collapse)
t=3:  oracle → "joy"
      ↓ (substitute)
t=4:  APP(@now, "joy")
      ↓ (reduce)
t=5:  joy@now
```

## 7. Advanced Topics

### 7.1 Parallel AMB Resolution

**Question:** Can multiple ∿ nodes be resolved in parallel?

**Answer:** Yes, if they are independent.

**Independence criterion:**
```python
def can_resolve_parallel(amb_a, amb_b, net):
    """
    Two AMB nodes can be resolved in parallel if:
    1. Neither's context depends on the other's result
    2. Their substitutions don't create wire conflicts
    """
    # Check for data dependencies
    deps_a = net.transitive_dependencies(amb_a)
    deps_b = net.transitive_dependencies(amb_b)

    if amb_b in deps_a or amb_a in deps_b:
        return False  # Sequential dependency

    # Check for shared context that might change
    scope_a = net.get_scope(amb_a)
    scope_b = net.get_scope(amb_b)

    if scope_a.writes.intersects(scope_b.reads):
        return False  # Write-read hazard

    return True  # Safe to parallelize
```

**Example of parallel resolution:**
```limn
fir[∿lov[joy,pai], ∿mov[run,sit]]
```

These two ∿ nodes are independent:
- Different semantic domains (emotion vs movement)
- No shared context variables
- Results don't affect each other's constraints

**Resolution strategy:**
```
1. Detect both AMB nodes during reduction
2. Check independence
3. Batch oracle calls:
   {
     "parallel_requests": [
       {id: "amb#7", expr: "∿lov[joy,pai]"},
       {id: "amb#9", expr: "∿mov[run,sit]"}
     ]
   }
4. Oracle resolves both (possibly using same LLM call)
5. Substitute both results
6. Resume reduction
```

### 7.2 Recursive Ambiguity

**Question:** What if LLM returns another ∿?

**Example:**
```limn
∿lov[joy, ∿lov[pai,ang]]
```

Oracle might respond with meta-ambiguity:
```
"In this context, negative emotions are ambiguous between pain and anger"
```

**Handling strategies:**

**Strategy 1: Disallow (strict mode)**
```python
def validate_oracle_response(response):
    if contains_ambiguity_operator(response.substitution):
        raise OracleError("Oracle must return concrete term, got: {response}")
```

**Strategy 2: Recursive resolution (lenient mode)**
```python
def resolve_recursive(amb_node, max_depth=3):
    depth = 0
    current = amb_node

    while depth < max_depth:
        oracle_response = call_oracle(current)

        if not is_ambiguous(oracle_response):
            return oracle_response  # Done

        # Oracle returned another AMB
        current = oracle_response.substitution
        depth += 1

    # Hit recursion limit
    raise AmbiguityError(f"Cannot resolve after {max_depth} oracle calls")
```

**Strategy 3: Lazy meta-ambiguity**
```
Treat oracle-returned ∿ as a new AMB node:
- Compile it into the net
- Let HVM decide if it needs resolution
- If lazy branch, might never evaluate
```

**Recommended:** Strategy 1 (strict) for production, Strategy 3 for research.

### 7.3 Oracle Caching

**Problem:** Same ∿ expression in same context might be called repeatedly.

**Solution:** Memoization with context-aware keys.

```python
class OracleCache:
    def __init__(self):
        self.cache = {}

    def cache_key(self, request):
        """
        Hash of expression + relevant context.
        """
        # Canonical representation of expression
        expr_hash = hash(normalize(request.expression))

        # Only include context that affects semantics
        context_hash = hash((
            tuple(sorted(request.scope.items())),
            request.type_expected
        ))

        return (expr_hash, context_hash)

    def get(self, request):
        key = self.cache_key(request)
        return self.cache.get(key)

    def put(self, request, response):
        key = self.cache_key(request)
        self.cache[key] = response
```

**Cache invalidation:** When context changes (e.g., @now advances), relevant cache entries must be purged.

### 7.4 Error Handling

**Oracle failures:**
1. **LLM timeout:** Retry with backoff, or fail-fast
2. **Invalid response:** Strict validation, reject and log
3. **Low confidence:** Threshold-based acceptance (e.g., require >0.8)

```python
def handle_oracle_error(error, amb_node):
    if isinstance(error, TimeoutError):
        # Retry with shorter prompt
        return retry_with_reduced_context(amb_node)

    elif isinstance(error, InvalidResponseError):
        # Log for debugging, return default
        log.error(f"Oracle returned invalid: {error}")
        return default_collapse(amb_node)  # First option

    elif isinstance(error, LowConfidenceError):
        # Ask for human input
        return escalate_to_human(amb_node)
```

## 8. Data Structures

### 8.1 Interaction Net Representation

```rust
// Rust-style representation for clarity
struct InteractionNet {
    nodes: HashMap<NodeId, Node>,
    wires: Vec<Wire>,
    blocked_nodes: HashSet<NodeId>,
}

enum Node {
    Const { value: String, out: Port },
    App { func: Port, arg: Port, out: Port },
    Lambda { var: VarId, body: Port, out: Port },
    Dup { in_port: Port, out_a: Port, out_b: Port },

    // The special AMB node
    Amb {
        node_id: NodeId,
        oracle: Port,      // Where oracle result connects
        args: Port,        // Arguments for oracle
        out: Port,         // Output of this node
        metadata: AmbMetadata,
    },
}

struct AmbMetadata {
    source_loc: SourceLocation,
    scope: HashMap<String, Value>,
    type_expected: Type,
    reduction_path: Vec<NodeId>,
}

struct Wire {
    source: Port,
    target: Port,
}

struct Port {
    node: NodeId,
    port_name: String,
}
```

### 8.2 Oracle Request/Response

```rust
struct OracleRequest {
    amb_node_id: NodeId,
    expression: String,      // Original LMN expression
    args: Vec<String>,       // Literal arguments
    type_expected: Type,
    scope: HashMap<String, Value>,
    reduction_trace: Vec<String>,
}

struct OracleResponse {
    amb_node_id: NodeId,
    substitution: InteractionNet,  // Subgraph to graft
    confidence: f64,
    alternatives: Vec<String>,
    reasoning: String,
    model_info: ModelInfo,
}
```

## 9. Implementation Pseudocode

### 9.1 Main Runtime Loop

```python
def lmn_runtime(source_code):
    """
    Main LMN runtime with HVM + LLM oracle.
    """
    # Compile to interaction net
    net = compile_limn(source_code)

    # Initialize HVM
    hvm = HVM(net)
    oracle = LLMOracle()

    while not hvm.is_normal_form():
        # Reduce until blocked or done
        status = hvm.reduce_until_blocked()

        if status.is_done():
            return hvm.get_result()

        if status.is_blocked():
            amb_node = status.blocked_on

            # Package context for oracle
            request = package_oracle_request(amb_node, hvm.net)

            # Call LLM oracle
            response = oracle.collapse(request)

            # Validate response
            if not validate_response(response):
                raise RuntimeError(f"Invalid oracle response: {response}")

            # Substitute AMB with oracle result
            hvm.net = substitute_amb(hvm.net, response)

            # Resume reduction
            continue
```

### 9.2 Oracle Call

```python
class LLMOracle:
    def __init__(self, model="claude-opus-4.5"):
        self.model = model
        self.cache = OracleCache()

    def collapse(self, request: OracleRequest) -> OracleResponse:
        """
        Collapses ambiguity using LLM.
        """
        # Check cache first
        cached = self.cache.get(request)
        if cached:
            return cached

        # Build prompt
        prompt = self.build_prompt(request)

        # Call LLM
        llm_output = anthropic.complete(
            model=self.model,
            prompt=prompt,
            max_tokens=500
        )

        # Parse response
        term = parse_limn(llm_output)
        subgraph = compile_to_net(term)

        response = OracleResponse(
            amb_node_id=request.amb_node_id,
            substitution=subgraph,
            confidence=extract_confidence(llm_output),
            alternatives=extract_alternatives(llm_output),
            reasoning=extract_reasoning(llm_output),
            model_info={"model": self.model, "tokens": len(llm_output)}
        )

        # Cache for future
        self.cache.put(request, response)

        return response

    def build_prompt(self, request):
        """
        Constructs LLM prompt from oracle request.
        """
        return f"""
You are an LMN semantic oracle. Collapse the following ambiguity to a single concrete term.

Expression: {request.expression}
Arguments: {request.args}
Expected type: {request.type_expected}

Context:
{format_scope(request.scope)}

Execution trace:
{format_trace(request.reduction_trace)}

Instructions:
1. Analyze the constraint intersection of the arguments
2. Apply contextual bindings to narrow the semantic region
3. Select the single most appropriate term
4. Return ONLY the selected term (no ambiguity operator)

Response format:
Selected term: <term>
Confidence: <0.0-1.0>
Reasoning: <brief explanation>
Alternatives considered: <other options>
"""
```

## 10. Pros and Cons

### 10.1 Advantages

**Performance:**
- HVM is extremely fast for deterministic computation
- Parallel reduction for non-AMB code
- Lazy evaluation: only resolve AMB when needed

**Clarity:**
- Clean separation: HVM = computation, LLM = semantics
- AMB nodes make oracle boundaries explicit
- Easy to trace and debug

**Flexibility:**
- Can swap oracle implementations (different LLMs, local models)
- Oracle caching for repeated patterns
- Parallel AMB resolution for independent ambiguities

**Correctness:**
- HVM reduction is proven confluent
- Oracle calls are isolated, testable
- Deterministic given oracle responses

### 10.2 Disadvantages

**Latency:**
- Each AMB node requires LLM call (100ms - 2s)
- Cannot parallelize dependent AMB nodes
- Network latency for cloud LLMs

**Complexity:**
- Need to compile LMN to interaction nets
- AMB metadata packaging is non-trivial
- Cache invalidation is subtle

**Oracle Quality:**
- LLM might give wrong collapse (context misunderstanding)
- Confidence scores are not calibrated probabilities
- Recursive ambiguity is tricky

**Resource Cost:**
- LLM calls are expensive (tokens, $$$)
- Need caching infrastructure
- Large context → large prompts → high cost

### 10.3 Mitigations

**For latency:**
- Aggressive caching with context-aware keys
- Batch independent AMB nodes
- Use faster models for simple collapses

**For complexity:**
- Good error messages pointing to AMB source location
- Visualization tools for interaction nets
- Standard library of AMB patterns

**For oracle quality:**
- Validate responses strictly
- Allow confidence thresholds
- Provide oracle override mechanism (human-in-loop)

**For cost:**
- Use smaller models for simple ambiguities
- Cache at multiple levels (local, shared)
- Precompute common patterns at compile time

## 11. Comparison to Alternative Models

### Model A: LLM-First (hypothetical)

- **Approach:** LLM interprets entire LMN program
- **Pro:** No need to compile to interaction nets
- **Con:** Slow, non-deterministic, doesn't leverage HVM

### Model C: Hybrid AST (hypothetical)

- **Approach:** Walk AST, call LLM at ∿ nodes
- **Pro:** Simpler than interaction nets
- **Con:** Misses HVM's parallelism and optimizations

### Model B (This Document): HVM-First

- **Best of both:** HVM speed + LLM semantics
- **Sweet spot:** Deterministic computation in HVM, oracle only when necessary

## 12. Future Work

1. **Implement proof-of-concept:** Small LMN subset → interaction net → HVM
2. **Oracle prompt engineering:** Optimize for accuracy + low latency
3. **Benchmarks:** Measure AMB resolution time, cache hit rate
4. **Type system integration:** Use types to constrain oracle responses
5. **Distributed oracle:** Multiple LLM instances for parallel AMB
6. **Interactive debugger:** Visualize net, step through reductions, inspect AMB calls

## 13. Conclusion

The HVM-First runtime model offers a compelling architecture for LMN execution:

- **HVM provides:** Blazing-fast deterministic reduction, proven correctness, parallelism
- **LLM provides:** Semantic ambiguity resolution, contextual collapse, flexibility
- **AMB nodes provide:** Clean interface between the two worlds

This design respects the strengths of both systems: HVM does what it does best (pure computation), and the LLM does what it does best (semantic judgment). The ∿ operator naturally bridges them.

**Key insight:** Ambiguity is not a bug to eliminate, but a feature to manage. By making ambiguity explicit as AMB nodes, we gain control, observability, and the ability to optimize.

**Next steps:** Build it and see if it works.

---

**References:**

- HVM Documentation: https://github.com/HigherOrderCO/HVM
- Interaction Nets: Y. Lafont, "Interaction Nets" (1990)
- LMN Specification: `docs/spec/LIMN-PL-SPECIFICATION.md`
- Key Mechanism: `docs/theory/key-mechanism.md`

---

*End of specification*
