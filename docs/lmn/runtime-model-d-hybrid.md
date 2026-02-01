# LMN Runtime Model D: True Hybrid (Simultaneous)

**Status:** Exploratory Specification
**Date:** 2026-02-01
**Author:** limn/polecats/valkyrie

---

## Executive Summary

This document explores a **truly simultaneous** LLM+HVM runtime model where:
- LLM and HVM execute in parallel (not sequentially)
- LLM is modeled as a **native node type** in the interaction net
- Both are "agents" in the same computational graph
- Communication happens through shared state and interaction net ports

This is a deep integration model where the LLM isn't just preprocessing or postprocessing—it's part of the computation fabric itself.

**Key Question:** Can we treat LLM inference as an interaction net node, subject to the same reduction rules as HVM nodes?

---

## 1. Background: The Problem Space

### Current Model (Sequential)

```
┌─────────────────────────────────────────────┐
│ LLM Phase                                   │
│                                             │
│ Input: "∿lov[joy,pai,hop]"                 │
│    ↓                                        │
│ Parse/Interpret → Constraint AST            │
│    ↓                                        │
│ Generate: x ∈ {joy, pai, hop}              │
└─────────────────────────────────────────────┘
              ↓
┌─────────────────────────────────────────────┐
│ HVM Phase                                   │
│                                             │
│ Input: Constraint graph                     │
│    ↓                                        │
│ Parallel reduction                          │
│    ↓                                        │
│ Output: Solved values                       │
└─────────────────────────────────────────────┘
```

**Problems:**
- LLM must complete before HVM starts
- No feedback loop during computation
- LLM can't observe HVM's intermediate states
- HVM can't request clarification mid-reduction

### Model D Goal (Simultaneous)

```
┌─────────────────────────────────────────────┐
│ Unified Computation Graph                   │
│                                             │
│  ┌────┐    ┌────┐    ┌────┐               │
│  │HVM │◄──►│LLM │◄──►│HVM │               │
│  │Node│    │Node│    │Node│               │
│  └────┘    └────┘    └────┘               │
│     ▲         ▲         ▲                  │
│     └─────────┴─────────┘                  │
│        Interaction Net                      │
└─────────────────────────────────────────────┘
```

**Goals:**
- LLM participates in reduction process itself
- Bidirectional communication during execution
- LLM can query HVM state / HVM can invoke LLM
- True co-computation

---

## 2. LLM as Node: Interaction Net Representation

### 2.1 What is an Interaction Net Node?

An interaction net node has:
1. **Type/Tag** - identifies the operation (e.g., `ADD`, `DUP`, `LAM`)
2. **Ports** - connection points (principal port + auxiliary ports)
3. **Reduction Rules** - how it interacts with other nodes

Example HVM node (Addition):
```
     Port 0 (result)
         ▲
         │
    ┌────┴────┐
    │   ADD   │
    └────┬────┘
         │
    ┌────┴────┐
    │         │
  Port 1   Port 2
 (operand) (operand)
```

### 2.2 LLM Node Design

```
      Port 0 (output)
          ▲
          │
    ┌─────┴─────┐
    │    LLM    │
    │  [model]  │
    │  [state]  │
    └─────┬─────┘
          │
    ┌─────┴─────┐
    │           │
  Port 1      Port 2
 (prompt)  (context)
```

**Node Structure:**
```rust
struct LLMNode {
    tag: NodeTag::LLM,
    ports: [Port; 3],        // output, prompt, context
    model_id: String,        // "claude-3-5-sonnet-20241022"
    state: LLMState,         // inference state
    cache: Option<KVCache>,  // prompt cache (if supported)
}

enum LLMState {
    Ready,                   // not started
    Streaming(Channel),      // actively streaming tokens
    Blocked(Reason),         // waiting for input
    Complete(String),        // finished with result
}
```

### 2.3 Reduction Rules

**Rule 1: LLM ↔ CON (constructor/data)**
```
┌────┐     ┌────┐
│LLM │────►│CON │
└────┘     └────┘
   ↓         ↓
 prompt    value

Reduces to:
┌────┐
│CON │  (LLM output as data)
└────┘
```

When LLM connects to data node, invoke inference with that data as prompt.

**Rule 2: LLM ↔ DUP (duplication)**
```
     ┌────┐
    ─┤DUP ├─
     └──┬─┘
        │
     ┌──┴──┐
     │ LLM │
     └─────┘

Reduces to:
     ┌────┐
    ─┤DUP ├─
     └──┬─┘
        │
     ┌──┴────┐
  ┌─┤  LLM  ├─┐
  │ └───────┘ │
  ▼           ▼
LLM₁         LLM₂
```

Duplicate the LLM node (share prompt cache if possible).

**Rule 3: LLM ↔ LLM (composition)**
```
┌────┐     ┌────┐
│LLM₁│────►│LLM₂│
└────┘     └────┘

Reduces to:
┌─────────────┐
│ LLM₁ ◦ LLM₂ │  (pipelined inference)
└─────────────┘
```

Chain LLM inferences (output of LLM₁ becomes input to LLM₂).

### 2.4 Special Property: Non-Deterministic Reduction

Unlike pure HVM nodes, LLM nodes are:
- **Non-deterministic** (temperature > 0)
- **Stateful** (prompt cache)
- **Expensive** (API calls, latency)
- **External** (not locally computable)

This breaks some interaction net assumptions:
- **Confluence** - LLM output varies across reductions
- **Locality** - LLM may need global context
- **Efficiency** - LLM is 1000x slower than HVM ops

**Implication:** Model D requires **probabilistic interaction nets** with non-confluent reductions.

---

## 3. Shared State: Memory Model

### 3.1 State Components

Both LLM and HVM need access to:

```rust
struct SharedState {
    // Interaction Net State
    net: InteractionNet,          // current net topology
    active_pairs: Queue<(Port, Port)>,  // reduction frontier

    // LLM-Specific State
    symbol_table: HashMap<String, Value>,  // variable bindings
    context_window: Vec<Token>,   // shared context
    prompt_cache: KVCache,        // cached prefixes

    // HVM-Specific State
    heap: Vec<Node>,              // node storage
    reduction_stats: Stats,       // reductions performed

    // Synchronization
    locks: HashMap<NodeId, RwLock>,  // per-node locks
}
```

### 3.2 Memory Access Patterns

**HVM reads/writes:**
- Net topology (frequently)
- Heap nodes (frequently)
- Symbol table (occasionally)

**LLM reads/writes:**
- Symbol table (frequently) - reading current bindings
- Context window (read-only) - observing net state
- Prompt cache (write-mostly) - building cached context

**Conflict potential:** Low. Different access patterns, minimal overlap.

### 3.3 Synchronization Strategy

**Option A: Coarse-Grained Locking**
```rust
// HVM reduction step
lock(net) {
    let pair = active_pairs.pop();
    reduce(pair);
}

// LLM inference
lock(symbol_table) {
    let prompt = build_prompt(symbol_table);
    let result = llm.infer(prompt);
    symbol_table.insert(var, result);
}
```

**Pros:** Simple, correct
**Cons:** Serializes LLM and HVM (not truly simultaneous)

**Option B: Lock-Free with Epochs**
```rust
struct Epoch {
    id: u64,
    net_snapshot: InteractionNet,
    pending_llm: Vec<LLMRequest>,
}

// HVM runs freely, creating epochs
while !active_pairs.empty() {
    reduce_batch(1000);  // 1000 reductions
    if needs_llm() {
        epoch = save_snapshot();
        spawn_llm_worker(epoch);
    }
}

// LLM runs on snapshot, merges results
fn llm_worker(epoch: Epoch) {
    let results = infer(epoch.pending_llm);
    merge_results(epoch.id, results);  // CAS on symbol table
}
```

**Pros:** True parallelism, LLM doesn't block HVM
**Cons:** Complex, requires result merging, non-deterministic

**Recommendation:** Start with Option A, migrate to Option B for performance.

---

## 4. Communication: Message Passing vs Ports

### 4.1 Port-Based Communication (Pure Interaction Nets)

LLM node connects via ports, like any other node:

```
┌────────┐         ┌─────────┐
│  HVM   │         │   LLM   │
│  Node  │         │  Node   │
└───┬────┘         └────┬────┘
    │                   │
    └──────►Port◄───────┘
         (wired)
```

**Reduction triggers communication:**
```rust
fn reduce(port_a: Port, port_b: Port) {
    match (node_type(port_a), node_type(port_b)) {
        (NodeTag::CON, NodeTag::LLM) => {
            let prompt = get_value(port_a);
            let result = invoke_llm(prompt);  // ⚠️ BLOCKING
            replace_node(port_b, CON(result));
        }
        // ... other rules
    }
}
```

**Problem:** LLM inference is slow (100-1000ms). Blocking reduction wastes CPU.

### 4.2 Message-Based Communication (Asynchronous)

Separate LLM inference from reduction:

```rust
enum Message {
    LLMRequest { node_id: NodeId, prompt: String },
    LLMResponse { node_id: NodeId, result: String },
    HVMReduced { epoch: u64, pairs: usize },
}

fn hvm_thread(rx: Receiver<Message>, tx: Sender<Message>) {
    loop {
        while !active_pairs.empty() {
            let pair = active_pairs.pop();
            match reduce(pair) {
                NeedsLLM(node_id, prompt) => {
                    tx.send(LLMRequest { node_id, prompt });
                }
                Complete => {}
            }
        }

        // Check for LLM results
        if let Ok(LLMResponse { node_id, result }) = rx.try_recv() {
            inject_result(node_id, result);
        }
    }
}

fn llm_thread(rx: Receiver<Message>, tx: Sender<Message>) {
    loop {
        let req = rx.recv();  // Block until LLM needed
        let result = llm_api.infer(req.prompt);
        tx.send(LLMResponse { node_id: req.node_id, result });
    }
}
```

**Pros:** Non-blocking, HVM continues during LLM
**Cons:** Breaks pure interaction net model, introduces async complexity

**Hybrid Approach:**
- LLM nodes start reduction synchronously
- Immediately yield with `Streaming` state
- Background thread handles actual API call
- Reduction continues when result ready

### 4.3 Example Communication Flow

Trace for `@now ∿lov[joy,pai,hop]`:

```
T=0ms   [HVM] Parse: lov is a CON node with list [joy, pai, hop]
T=0ms   [HVM] Encounter LLM node: "resolve ambiguity"
T=0ms   [HVM] Send message: LLMRequest("∿lov[joy,pai,hop]")
T=0ms   [HVM] Mark LLM node as Streaming, continue other reductions

T=5ms   [LLM] Receive request, start inference
T=5ms   [LLM] Read context: "lov = {love, lovely, beloved}"
T=5ms   [LLM] Build prompt: "Resolve: ∿lov[joy,pai,hop]. Context: ..."

T=50ms  [HVM] Reduced 10,000 other pairs while waiting

T=120ms [LLM] Inference complete: "lov → love (verb form)"
T=120ms [LLM] Send message: LLMResponse("love")

T=121ms [HVM] Receive response, inject into net
T=121ms [HVM] Continue reduction with resolved value
```

**Key insight:** HVM performs ~10,000 reductions during a single LLM call.

---

## 5. Parallelism: Avoiding Conflicts

### 5.1 Sources of Conflict

**Read-Write Conflicts:**
- HVM reducing pair (A, B) while LLM reads A's value
- LLM writing to symbol table while HVM reads same symbol

**Write-Write Conflicts:**
- Two LLM nodes trying to bind same variable
- HVM and LLM both updating context window

**Structural Conflicts:**
- HVM changing net topology while LLM traversing it
- LLM node removed before inference completes

### 5.2 Conflict Resolution Strategies

**Strategy 1: Static Partitioning**

Divide net into regions:
```
┌──────────────────────────────────────┐
│  Net Partition 1: Pure HVM          │
│  (no LLM nodes, reduce freely)       │
└──────────────────────────────────────┘

┌──────────────────────────────────────┐
│  Net Partition 2: LLM Boundary       │
│  (contains LLM nodes, needs sync)    │
└──────────────────────────────────────┘
```

HVM reduces partition 1 at full speed, synchronizes only at boundaries.

**Strategy 2: Optimistic Transactions**

```rust
fn reduce_transaction(pair: (Port, Port)) -> Result<(), Conflict> {
    let tx = start_transaction();

    tx.read(pair.0);
    tx.read(pair.1);

    let new_net = reduce_pure(pair);

    tx.write(new_net);

    tx.commit()  // Fails if LLM modified involved nodes
}

// Retry on conflict
loop {
    match reduce_transaction(pair) {
        Ok(()) => break,
        Err(Conflict) => continue,  // Retry with fresh snapshot
    }
}
```

**Strategy 3: Actor Model**

Each node is an actor with a mailbox:

```rust
struct NodeActor {
    id: NodeId,
    state: NodeState,
    inbox: Channel<Message>,
}

// Actors never share memory, only pass messages
actor_llm.send(ResolveMsg { input: "∿lov[joy,pai,hop]" });
actor_hvm.send(ReduceMsg { pair: (A, B) });
```

**Pros:** No locks, naturally concurrent
**Cons:** Overhead of message passing, actor spawning

**Recommendation:** Start with Strategy 1 (partitioning), add Strategy 2 (transactions) for boundaries.

### 5.3 Memory Consistency Model

What guarantees do we provide?

**Option A: Sequential Consistency**
- All operations appear to execute in some total order
- Easy to reason about, but expensive to implement

**Option B: Release Consistency**
- Synchronization only at explicit barriers (e.g., LLM node boundaries)
- HVM and LLM may observe different orderings between barriers
- Fast, but complex semantics

**Option C: Eventual Consistency**
- LLM and HVM eventually converge on same state
- Allows maximum parallelism
- Non-deterministic, may not be acceptable for Limn

**Recommendation:** Option B (Release Consistency) with barriers at LLM node boundaries.

---

## 6. Example Trace: `@now ∿lov[joy,pai,hop] → ?`

### 6.1 Input Expression

```limn
@now ∿lov[joy,pai,hop]
```

**Meaning:**
- `@now` - temporal modifier: "in the present"
- `∿` - wave operator: indicates ambiguity/superposition
- `lov` - ambiguous word (love/lovely/beloved)
- `[joy,pai,hop]` - list arguments (joy, pain, hope)

### 6.2 Initial Net

```
            ┌────────┐
            │  App   │ (application node)
            └───┬────┘
                │
        ┌───────┴────────┐
        │                │
    ┌───┴───┐       ┌────┴─────┐
    │  Now  │       │   Wave   │
    └───────┘       └────┬─────┘
                         │
                    ┌────┴────┐
                    │   LLM   │ (resolve ambiguity)
                    └────┬────┘
                         │
                    ┌────┴────┐
                    │   App   │
                    └────┬────┘
                         │
                    ┌────┴───────┐
                    │            │
                ┌───┴───┐   ┌────┴────┐
                │  Lov  │   │  List   │
                └───────┘   └────┬────┘
                                 │
                            [joy,pai,hop]
```

### 6.3 Reduction Steps (Simultaneous Model)

**Step 1: Initial State (T=0ms)**
```
HVM Thread: Scan net, identify LLM node
LLM Thread: Idle, waiting for request
```

**Step 2: Encounter LLM Node (T=1ms)**
```
HVM: Reach LLM node for "∿lov"
HVM: Build prompt from context:
  - Input: "lov"
  - Domain: temporal (from @now)
  - Arguments: [joy, pai, hop]
  - Vocabulary: {love (verb), lovely (adj), beloved (noun)}

HVM: Send message to LLM thread
HVM: Mark LLM node as Streaming(channel_id: 42)
HVM: Continue with other reductions
```

**Step 3: HVM Continues (T=1-100ms)**
```
HVM: Reduce other parts of net
  - Evaluate @now → bind temporal context
  - Construct list [joy, pai, hop] → CON nodes
  - Prepare argument ports for when LLM completes

Active pairs reduced: 8,234
Net size: 512 nodes
```

**Step 4: LLM Inference (T=1-100ms, parallel)**
```
LLM: Receive request
LLM: Query vocabulary database:
  - lov: love (primary), lovely (secondary), beloved (tertiary)

LLM: Analyze context:
  - @now suggests present tense verb
  - [joy,pai,hop] are abstract concepts (likely objects)
  - Pattern matches: "to love [things]"

LLM: Reasoning:
  "Given temporal modifier 'now' and list of abstract concepts,
   lov should resolve to verb 'love' in present tense.

   Output: love (verb)"

LLM: Generate constraint:
  lov → love
  Type: Verb
  Arity: 1 (transitive)
```

**Step 5: LLM Response (T=100ms)**
```
LLM: Send message to HVM thread
  Result: { resolved: "love", type: "verb", confidence: 0.92 }
```

**Step 6: Result Injection (T=101ms)**
```
HVM: Receive LLM response
HVM: Locate LLM node (id: 42, state: Streaming)
HVM: Replace LLM node with CON("love")
HVM: Connect CON to waiting ports
HVM: Continue reduction
```

**Step 7: Final Reduction (T=101-105ms)**
```
Net after injection:
            ┌────────┐
            │  App   │
            └───┬────┘
                │
        ┌───────┴────────┐
        │                │
    ┌───┴───┐       ┌────┴─────┐
    │  Now  │       │  CON     │  ← "love" (from LLM)
    └───────┘       └────┬─────┘
                         │
                    ┌────┴────┐
                    │   App   │
                    └────┬────┘
                         │
                    ┌────┴───────┐
                    │            │
                ┌───┴───┐   ┌────┴────┐
                │ "love"│   │  List   │
                └───────┘   └────┬────┘
                                 │
                            [joy,pai,hop]

HVM: Apply "love" to list
HVM: Final form: love([joy, pai, hop])
HVM: Reduction complete
```

**Step 8: Output (T=105ms)**
```
Result: @now love[joy, pai, hop]

English: "In the present, to love joy, pain, and hope"
```

### 6.4 Trace Summary

| Time | HVM State | LLM State | Communication |
|------|-----------|-----------|---------------|
| 0ms | Parsing net | Idle | - |
| 1ms | Encounter LLM node | Idle | HVM → LLM: Request |
| 2-100ms | Reducing other pairs (8,234) | Inferring | - |
| 100ms | Waiting at barrier | Complete | LLM → HVM: Response |
| 101ms | Inject result | Idle | - |
| 102-105ms | Final reduction | Idle | - |
| 105ms | Complete | Idle | - |

**Key Observations:**
1. **Parallelism achieved:** HVM performed 8,234 reductions during LLM inference
2. **Barrier at LLM node:** HVM can't proceed past LLM node until result arrives
3. **Non-blocking overall:** Other branches of net continue reducing
4. **Latency dominated by LLM:** 100ms LLM vs 5ms total HVM time

---

## 7. Data Structures

### 7.1 Node Representation

```rust
#[repr(C)]
struct Node {
    tag: u8,           // Node type (0-255)
    ports: [Port; 3],  // Principal + aux ports
    data: NodeData,    // Type-specific data
}

#[repr(C)]
union NodeData {
    // Pure HVM nodes
    num: i64,
    dup: DupData,
    lam: LamData,

    // LLM node
    llm: LLMData,
}

struct LLMData {
    model: ModelId,          // Which LLM (8 bytes)
    state: AtomicU64,        // State flags (8 bytes)
    channel: ChannelId,      // Message channel (8 bytes)
    cache_key: u64,          // Prompt cache key (8 bytes)
    // Total: 32 bytes (fits in cache line)
}

// State encoding in AtomicU64
// Bits 0-7:   State (Ready=0, Streaming=1, Complete=2, Error=3)
// Bits 8-31:  Request ID (24 bits, 16M concurrent requests)
// Bits 32-63: Timestamp (32 bits, for timeout detection)
```

### 7.2 Message Queue

```rust
struct MessageQueue {
    // Lock-free MPSC queue (multiple HVM threads → one LLM thread)
    llm_requests: ConcurrentQueue<LLMRequest>,

    // Lock-free SPMC queue (one LLM thread → multiple HVM threads)
    llm_responses: ConcurrentQueue<LLMResponse>,
}

struct LLMRequest {
    node_id: NodeId,
    epoch: u64,              // For snapshot-based execution
    prompt: String,
    context: Vec<Token>,     // Relevant net context
    deadline: Instant,       // Timeout
}

struct LLMResponse {
    node_id: NodeId,
    result: String,
    metadata: ResponseMeta,  // Confidence, token count, etc.
}
```

### 7.3 Synchronization Primitives

```rust
struct Barrier {
    count: AtomicUsize,      // Threads waiting
    sense: AtomicBool,       // Barrier sense (for reuse)
}

impl Barrier {
    fn wait(&self, local_sense: &mut bool) {
        // Sense-reversing barrier (no allocation)
        let count = self.count.fetch_add(1, Ordering::SeqCst);
        if count == NUM_THREADS - 1 {
            self.count.store(0, Ordering::SeqCst);
            self.sense.store(!*local_sense, Ordering::Release);
        } else {
            while self.sense.load(Ordering::Acquire) == *local_sense {
                hint::spin_loop();
            }
        }
        *local_sense = !*local_sense;
    }
}
```

---

## 8. Communication Protocol

### 8.1 Protocol Specification

**Phase 1: Request**
```
HVM → LLM

Message: LLMRequest {
    node_id: 0x1a2b3c,
    epoch: 42,
    prompt: "∿lov[joy,pai,hop]",
    context: [Token(now), Token(lov), ...],
    deadline: now() + 5s,
}
```

**Phase 2: Acknowledgment (Optional)**
```
LLM → HVM

Message: LLMAck {
    node_id: 0x1a2b3c,
    estimated_time: 100ms,
}
```

**Phase 3: Streaming (Optional)**
```
LLM → HVM (multiple messages)

Message: LLMChunk {
    node_id: 0x1a2b3c,
    partial: "lo",     // Streaming token
}

Message: LLMChunk {
    node_id: 0x1a2b3c,
    partial: "ve",
}
```

**Phase 4: Completion**
```
LLM → HVM

Message: LLMResponse {
    node_id: 0x1a2b3c,
    result: "love",
    metadata: {
        confidence: 0.92,
        tokens: 45,
        latency: 98ms,
    }
}
```

### 8.2 Error Handling

**Timeout:**
```rust
if now() > request.deadline {
    return LLMResponse {
        node_id: request.node_id,
        result: Err(Timeout),
        metadata: { ... }
    };
}
```

**API Failure:**
```rust
match llm_api.infer(prompt) {
    Ok(result) => LLMResponse { result: Ok(result), ... },
    Err(e) => LLMResponse { result: Err(APIError(e)), ... },
}
```

**Network Partition:**
```rust
// HVM detects missing response
if elapsed > timeout {
    log_error("LLM node {} timed out", node_id);
    replace_node(node_id, CON("<TIMEOUT>"));
    continue_reduction();
}
```

---

## 9. Pros and Cons

### 9.1 Advantages

**1. True Co-Computation**
- LLM and HVM execute simultaneously
- HVM doesn't block on LLM for unrelated reductions
- Potential speedup: 10-100x for nets with multiple LLM nodes

**2. Feedback Loops**
- LLM can query intermediate HVM state
- HVM can refine LLM input based on partial results
- Enables iterative clarification

**3. Natural Integration**
- LLM is just another node type
- Existing interaction net theory applies (mostly)
- Can reason about LLM formally

**4. Composability**
- Multiple LLM nodes can work in parallel
- Can pipeline LLM inferences
- Can duplicate LLM nodes (with cache sharing)

### 9.2 Disadvantages

**1. Breaks Confluence**
- Interaction nets are confluent (order doesn't matter)
- LLM is non-deterministic (order matters)
- Multiple reductions may yield different results

**2. Latency Mismatch**
- HVM: microseconds per reduction
- LLM: 10-1000ms per inference
- HVM may be idle waiting for LLM bottleneck

**3. Synchronization Overhead**
- Locks, barriers, message passing add cost
- May negate parallelism gains for small nets

**4. Non-Local Computation**
- LLM inference requires external API call
- Not suitable for GPU-local execution
- Introduces network failure modes

**5. Debugging Nightmare**
- Non-deterministic + concurrent = hard to reproduce bugs
- Difficult to trace execution
- LLM errors are opaque

### 9.3 Comparison with Sequential Model

| Aspect | Sequential | Simultaneous (Model D) |
|--------|-----------|------------------------|
| **Correctness** | Easy to verify | Non-deterministic |
| **Performance** | Slow (LLM blocks HVM) | Fast (parallel) |
| **Complexity** | Simple | High (sync, messages) |
| **Debugging** | Easy | Hard |
| **Scalability** | Poor (serial bottleneck) | Good (parallel LLMs) |
| **Theory** | Clean (confluent nets) | Messy (non-confluent) |

---

## 10. Feasibility Analysis

### 10.1 Is This Possible?

**Yes, with caveats.**

**Technical Blockers:**

1. **Non-Confluence**
   - Solution: Accept non-determinism, use probabilistic semantics
   - Precedent: Probabilistic programming, stochastic computation graphs

2. **Latency**
   - Solution: Batch LLM requests, use prompt caching aggressively
   - Precedent: Neural net batching, GPU kernel fusion

3. **API Reliability**
   - Solution: Timeouts, retries, fallback strategies
   - Precedent: Distributed systems, microservices

4. **Debugging**
   - Solution: Deterministic replay, structured logging
   - Precedent: Record-replay debugging, distributed tracing

**Theoretical Blockers:**

1. **Interaction Net Theory Assumes Confluence**
   - Fix: Extend theory to non-confluent nets
   - Research: "Probabilistic Interaction Nets" (doesn't exist yet)

2. **Optimal Reduction Undefined**
   - Fix: Define "optimal" as minimizing LLM calls (not reduction steps)
   - Metric: LLM invocations + HVM time, weighted by latency

### 10.2 What Would It Take?

**Implementation Requirements:**

1. **Extended HVM Runtime**
   - Add LLM node type to HVM
   - Implement async reduction (non-blocking LLM)
   - Message passing layer between HVM and LLM threads

2. **LLM Orchestration Layer**
   - Request batching and deduplication
   - Prompt cache management
   - Retry logic and fallback strategies

3. **Synchronization Framework**
   - Fine-grained locking or lock-free data structures
   - Barrier synchronization at LLM boundaries
   - Memory consistency protocol

4. **Debugging Tools**
   - Net state snapshot/replay
   - Trace visualization (HVM + LLM timeline)
   - Non-determinism detection

**Estimated Effort:**
- Core runtime: 4-6 weeks
- Orchestration: 2-3 weeks
- Debugging tools: 3-4 weeks
- **Total: 9-13 weeks**

### 10.3 Open Questions

1. **How to handle infinite LLM loops?**
   - Example: LLM₁ → LLM₂ → LLM₃ → ... (never terminates)
   - Solution: Depth limit? Timeout? Cycle detection?

2. **What's the memory model?**
   - Sequential consistency (expensive)?
   - Eventual consistency (non-deterministic)?
   - Release consistency (complex)?

3. **Can we prove properties?**
   - Termination? (No, LLM may loop)
   - Progress? (Only if LLM responds)
   - Correctness? (Depends on LLM accuracy)

4. **How to optimize?**
   - Batching multiple LLM nodes?
   - Speculative execution?
   - Prompt cache sharing across nodes?

5. **How to test?**
   - Unit tests (mock LLM)?
   - Integration tests (real LLM, but flaky)?
   - Property tests (non-deterministic)?

---

## 11. Alternatives Considered

### Model A: Sequential (Baseline)
```
LLM (parse) → HVM (compute) → Done
```
**Pros:** Simple, correct
**Cons:** Slow, no feedback

### Model B: Pre-Compute + HVM
```
LLM (generate all possible interpretations) → HVM (filter via constraints)
```
**Pros:** Single LLM call, HVM does filtering
**Cons:** Exponential interpretations, huge search space

### Model C: HVM with LLM Oracles
```
HVM (compute until ambiguous) → LLM (resolve) → HVM (continue)
```
**Pros:** Lazy LLM invocation, minimal calls
**Cons:** Still sequential, no parallelism

### Model D: True Hybrid (This Document)
```
HVM + LLM (simultaneous, message-passing)
```
**Pros:** Parallel, composable
**Cons:** Complex, non-deterministic

### Model E: LLM as Compiler (Not Explored)
```
LLM (Limn → pure HVM code) → HVM (execute)
```
**Pros:** No runtime LLM, pure HVM execution
**Cons:** Loses ambiguity, requires full specification upfront

---

## 12. Conclusion

### Summary

Model D (True Hybrid) is **technically feasible but theoretically messy**:

✅ **Can be implemented** with async runtime and message passing
✅ **Provides parallelism** (10-100x speedup potential)
✅ **Enables feedback loops** between LLM and HVM

❌ **Breaks confluence** (non-deterministic results)
❌ **Complex synchronization** (locks, barriers, consistency)
❌ **Debugging is hard** (concurrent + non-deterministic)

### Recommendations

**For Research:**
- Build prototype to validate performance claims
- Develop theory of probabilistic interaction nets
- Explore deterministic subsets (fixed seed, memoization)

**For Production:**
- Start with Model C (simpler, predictable)
- Migrate to Model D for performance-critical paths
- Maintain deterministic mode for testing

**For Limn:**
- Model D fits Limn's ambiguity-resolution semantics well
- LLM node is natural for `∿` (wave) operator
- Enables new language features (runtime key updates, contextual interpretation)

### Next Steps

1. **Prototype:** Implement minimal Model D in HVM
2. **Benchmark:** Compare Model C (sequential) vs Model D (parallel)
3. **Theory:** Formalize probabilistic interaction nets
4. **Safety:** Define subset where determinism is guaranteed
5. **Tools:** Build trace visualizer for debugging

---

## Appendix: Pseudocode

### A.1 Core Runtime Loop

```rust
fn runtime_loop(
    net: &mut InteractionNet,
    llm_tx: Sender<LLMRequest>,
    llm_rx: Receiver<LLMResponse>,
) {
    let mut active_pairs = net.active_pairs();

    loop {
        // Process LLM responses (non-blocking)
        while let Ok(response) = llm_rx.try_recv() {
            inject_llm_result(net, response);
        }

        // Reduce HVM pairs
        while let Some(pair) = active_pairs.pop() {
            match reduce_pair(net, pair) {
                ReductionResult::Complete => {},
                ReductionResult::NeedsLLM { node_id, prompt } => {
                    let request = LLMRequest {
                        node_id,
                        prompt,
                        context: extract_context(net, node_id),
                        deadline: now() + Duration::from_secs(5),
                    };
                    llm_tx.send(request).unwrap();
                    mark_streaming(net, node_id);
                },
            }
        }

        // Check if done (no active pairs, no pending LLM)
        if active_pairs.is_empty() && pending_llm_count(net) == 0 {
            break;
        }

        // Wait for LLM if blocked
        if active_pairs.is_empty() {
            let response = llm_rx.recv().unwrap();
            inject_llm_result(net, response);
            active_pairs = net.active_pairs();
        }
    }
}
```

### A.2 LLM Worker Thread

```rust
fn llm_worker(
    rx: Receiver<LLMRequest>,
    tx: Sender<LLMResponse>,
    llm_client: LLMClient,
) {
    loop {
        let request = rx.recv().unwrap();

        // Check deadline
        if now() > request.deadline {
            tx.send(LLMResponse {
                node_id: request.node_id,
                result: Err(Timeout),
                metadata: Default::default(),
            }).unwrap();
            continue;
        }

        // Invoke LLM
        let result = llm_client.infer(&request.prompt);

        // Send response
        tx.send(LLMResponse {
            node_id: request.node_id,
            result: result.map(|s| s.to_string()),
            metadata: ResponseMeta {
                confidence: 0.9,  // TODO: extract from LLM
                tokens: result.as_ref().map(|s| s.len()).unwrap_or(0),
                latency: elapsed_since(request),
            },
        }).unwrap();
    }
}
```

### A.3 Reduction Function

```rust
fn reduce_pair(
    net: &mut InteractionNet,
    pair: (Port, Port),
) -> ReductionResult {
    let node_a = net.node(pair.0);
    let node_b = net.node(pair.1);

    match (node_a.tag, node_b.tag) {
        // Standard HVM rules
        (NodeTag::LAM, NodeTag::APP) => {
            // Beta reduction
            reduce_beta(net, node_a, node_b);
            ReductionResult::Complete
        },
        (NodeTag::DUP, NodeTag::CON) => {
            // Duplication
            reduce_dup(net, node_a, node_b);
            ReductionResult::Complete
        },

        // LLM rules
        (NodeTag::LLM, NodeTag::CON) => {
            let prompt = extract_value(net, node_b);
            ReductionResult::NeedsLLM {
                node_id: node_a.id,
                prompt,
            }
        },
        (NodeTag::LLM, NodeTag::DUP) => {
            // Duplicate LLM node (share cache)
            duplicate_llm(net, node_a, node_b);
            ReductionResult::Complete
        },

        _ => panic!("Invalid reduction rule"),
    }
}
```

---

**END OF SPECIFICATION**
