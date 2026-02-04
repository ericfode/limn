# LMN Runtime Model A: LLM-First Architecture

**Status:** Theoretical Exploration
**Date:** 2026-02-01
**Model:** Hybrid LLM + HVM execution

## Executive Summary

This document specifies a runtime architecture for LMN (Limn) that leverages the semantic understanding of Large Language Models (LLMs) combined with the computational efficiency of interaction net reduction via HVM (Higher-order Virtual Machine).

**Core Insight:** LLMs excel at semantic interpretation but are inefficient at symbolic computation. HVM excels at parallel reduction of interaction nets but has no semantic understanding. This architecture combines their strengths:

1. **LLM** acts as the semantic parser and interpreter
2. **HVM** acts as the computational engine
3. **Interaction nets** serve as the intermediate representation

---

## Architecture Overview

```
┌─────────────┐
│ LMN Source  │ "∿lov[joy,pai,hop]"
└──────┬──────┘
       │ (1) Parse & Extract Meaning
       ▼
┌─────────────┐
│     LLM     │ Semantic Understanding
│  (Parser)   │
└──────┬──────┘
       │ (2) Emit Interaction Net
       ▼
┌─────────────┐
│   Net IR    │ Graph of agents & ports
└──────┬──────┘
       │ (3) Reduce
       ▼
┌─────────────┐
│     HVM     │ Parallel Reduction
│  (Engine)   │
└──────┬──────┘
       │ (4) Interpret Result
       ▼
┌─────────────┐
│     LLM     │ Semantic Interpretation
│(Interpreter)│
└──────┬──────┘
       │
       ▼
┌─────────────┐
│   Meaning   │ Natural language or structured output
└─────────────┘
```

---

## Phase 1: Input Processing

### Overview
The LLM receives LMN source code and performs semantic parsing to understand the intended meaning.

### Process

**Input:** LMN source text
```limn
@now ∿lov[joy,pai,hop] → ?
```

**Step 1.1: Tokenization**
The LLM breaks the source into semantic tokens:
- `@now` - temporal operator (present time)
- `∿` - blending operator
- `lov` - love (emotion/concept)
- `[joy,pai,hop]` - list of emotional states
- `→` - transformation/query operator
- `?` - unknown result to be computed

**Step 1.2: Semantic Analysis**
The LLM interprets each token in context:
```
@now        → CurrentTime constraint
∿lov        → BlendingRelation(subject=love)
[joy,pai,hop] → Set(joy, pain, hope)
→ ?         → QueryTransformation(target=unknown)
```

**Step 1.3: Meaning Extraction**
The LLM constructs a semantic interpretation:
```
Query: "At this moment in time, what is the result of blending
       joy, pain, and hope through the lens of love?"
```

**Step 1.4: Constraint Surface Identification**
Based on Limn's constraint-intersection model, the LLM identifies:
```
Constraints:
  - Temporal: present moment
  - Operational: blend operation (intersection of meanings)
  - Subject: love as the blending medium
  - Components: {joy, pain, hope}
  - Output: unknown value in intersection region
```

### Data Structure

```python
class SemanticParse:
    def __init__(self):
        self.tokens: List[Token] = []
        self.constraints: List[Constraint] = []
        self.operations: List[Operation] = []
        self.context: Dict[str, Any] = {}

class Token:
    def __init__(self, surface: str, semantic_type: str, meaning: str):
        self.surface = surface        # "∿lov"
        self.semantic_type = semantic_type  # "operator"
        self.meaning = meaning        # "blending_through_love"
        self.embedding: np.ndarray = None  # semantic vector

class Constraint:
    def __init__(self, constraint_type: str, value: Any):
        self.type = constraint_type   # "temporal", "operational", etc.
        self.value = value
        self.hyperplane: np.ndarray = None  # constraint surface in semantic space
```

### Pseudocode

```python
def parse_lmn_source(source: str, llm: LLM) -> SemanticParse:
    """
    Phase 1: Parse LMN source into semantic representation.
    """
    # Step 1: Tokenize using LLM understanding
    tokens = llm.tokenize_semantic(source)

    # Step 2: Extract constraints
    constraints = []
    for token in tokens:
        if token.is_operator():
            constraints.append(extract_constraint(token, llm))
        elif token.is_temporal():
            constraints.append(TemporalConstraint(token.value))

    # Step 3: Identify operations
    operations = identify_operations(tokens, llm)

    # Step 4: Build semantic parse
    parse = SemanticParse()
    parse.tokens = tokens
    parse.constraints = constraints
    parse.operations = operations
    parse.context = llm.extract_context(source)

    return parse

def extract_constraint(token: Token, llm: LLM) -> Constraint:
    """
    Use LLM to map token to constraint hyperplane.
    """
    # Get semantic embedding of token
    embedding = llm.embed(token.surface)

    # LLM determines constraint type and properties
    constraint_type = llm.classify_constraint(token)

    # Create constraint with semantic surface
    constraint = Constraint(constraint_type, token.meaning)
    constraint.hyperplane = embedding

    return constraint
```

---

## Phase 2: Net Generation

### Overview
The LLM translates the semantic parse into an interaction net representation that HVM can reduce.

### Interaction Net Fundamentals

**Node Types:**
1. **Operators** - Active agents (blend, transform, query)
2. **Values** - Passive data (joy, pain, hope, love)
3. **Ports** - Connection points (principal port, auxiliary ports)

**Interaction Rules:**
- When two principal ports connect, they reduce according to their interaction rule
- Auxiliary ports propagate values through the graph
- Reduction continues until no more active pairs exist

### Net Generation Process

**Input:** `SemanticParse` from Phase 1

**Step 2.1: Create Value Nodes**
```
joy_node  = ValueNode("joy", embedding=llm.embed("joy"))
pain_node = ValueNode("pain", embedding=llm.embed("pain"))
hope_node = ValueNode("hope", embedding=llm.embed("hope"))
love_node = ValueNode("love", embedding=llm.embed("love"))
```

**Step 2.2: Create Operator Nodes**
```
blend_op  = OperatorNode("blend",
                         subject_port=connect_to(love_node),
                         input_ports=[joy_node, pain_node, hope_node])

time_op   = OperatorNode("temporal",
                         constraint=@now,
                         input_port=blend_op)

query_op  = OperatorNode("query",
                         input_port=time_op,
                         output_port=unknown)
```

**Step 2.3: Connect Ports**
```
Connections:
  love_node.out → blend_op.subject
  joy_node.out  → blend_op.in[0]
  pain_node.out → blend_op.in[1]
  hope_node.out → blend_op.in[2]
  blend_op.out  → time_op.in
  time_op.out   → query_op.in
  query_op.out  → result
```

### Net Structure

```
        [love]
          │
        (subject)
          │
    ┌─────┴─────┐
    │   BLEND   │◀──[joy]
    │           │◀──[pain]
    │           │◀──[hope]
    └─────┬─────┘
          │
    ┌─────┴─────┐
    │   TIME    │  @now
    │   (@now)  │
    └─────┬─────┘
          │
    ┌─────┴─────┐
    │   QUERY   │
    │    (?)    │
    └─────┬─────┘
          │
       [result]
```

### Data Structure

```python
class InteractionNet:
    def __init__(self):
        self.nodes: List[Node] = []
        self.connections: List[Connection] = []
        self.active_pairs: List[Tuple[Node, Node]] = []

class Node:
    def __init__(self, node_type: str, value: Any = None):
        self.type = node_type  # "operator" | "value" | "constraint"
        self.value = value
        self.principal_port: Port = Port(self, is_principal=True)
        self.auxiliary_ports: List[Port] = []
        self.reduction_rule: Callable = None

class Port:
    def __init__(self, node: Node, is_principal: bool = False):
        self.node = node
        self.is_principal = is_principal
        self.connected_to: Optional[Port] = None

class Connection:
    def __init__(self, port_a: Port, port_b: Port):
        self.port_a = port_a
        self.port_b = port_b
```

### Pseudocode

```python
def generate_interaction_net(parse: SemanticParse, llm: LLM) -> InteractionNet:
    """
    Phase 2: Generate interaction net from semantic parse.
    """
    net = InteractionNet()

    # Step 1: Create value nodes from tokens
    value_nodes = {}
    for token in parse.tokens:
        if token.semantic_type == "value":
            node = Node("value", token.meaning)
            node.embedding = token.embedding
            value_nodes[token.surface] = node
            net.nodes.append(node)

    # Step 2: Create operator nodes
    for operation in parse.operations:
        op_node = create_operator_node(operation, llm)
        net.nodes.append(op_node)

        # Attach reduction rule based on operator semantics
        op_node.reduction_rule = llm.get_reduction_rule(operation.type)

    # Step 3: Connect nodes based on semantic relationships
    for constraint in parse.constraints:
        connect_by_constraint(net, constraint, llm)

    # Step 4: Identify active pairs (nodes ready to reduce)
    net.active_pairs = find_active_pairs(net)

    return net

def create_operator_node(operation: Operation, llm: LLM) -> Node:
    """
    Create an operator node with semantic reduction rule.
    """
    node = Node("operator", operation.name)

    # Create ports based on operator arity
    arity = llm.determine_arity(operation)
    for i in range(arity):
        node.auxiliary_ports.append(Port(node))

    # LLM provides the semantic reduction rule
    node.reduction_rule = lambda inputs: llm.reduce_operator(
        operation.name,
        inputs,
        context=operation.context
    )

    return node

def find_active_pairs(net: InteractionNet) -> List[Tuple[Node, Node]]:
    """
    Find pairs of nodes whose principal ports are connected.
    """
    active = []
    for conn in net.connections:
        if conn.port_a.is_principal and conn.port_b.is_principal:
            active.append((conn.port_a.node, conn.port_b.node))
    return active
```

---

## Phase 3: Reduction Phase

### Overview
HVM reduces the interaction net using parallel rewrite rules until reaching normal form.

### HVM Integration

HVM (Higher-order Virtual Machine) provides:
- **Parallel reduction**: Multiple active pairs reduce simultaneously
- **Optimal sharing**: Duplicate computations are shared
- **No garbage collection**: Interaction nets are self-cleaning

### Reduction Process

**Input:** `InteractionNet` from Phase 2

**Step 3.1: Serialize Net for HVM**
Convert the LLM-generated net to HVM's internal format:
```
HVM Net Format:
  @blend = λsubject λa λb λc
    let blended = (intersect a b c subject)
    blended

  @time = λcurrent λvalue
    (filter value current)

  @query = λinput
    (resolve input)

Main:
  let joy = {embed: [0.2, 0.8, ...]}
  let pain = {embed: [0.7, 0.1, ...]}
  let hope = {embed: [0.5, 0.9, ...]}
  let love = {embed: [0.9, 0.7, ...]}

  (query (time @now (blend love joy pain hope)))
```

**Step 3.2: HVM Parallel Reduction**
HVM reduces active pairs in parallel:

```
Initial state:
  (query (time @now (blend love joy pain hope)))

Step 1 - Reduce blend:
  blend(love, joy, pain, hope) → {
    intersection = compute_semantic_intersection([joy, pain, hope], love)
    → blended_value
  }

Step 2 - Apply temporal filter:
  time(@now, blended_value) → {
    apply_temporal_context(blended_value, present_moment)
    → contextualized_value
  }

Step 3 - Resolve query:
  query(contextualized_value) → {
    resolve_to_meaning(contextualized_value)
    → final_result
  }

Final state:
  final_result = {
    semantic_vector: [0.6, 0.7, 0.8, ...],
    confidence: 0.85,
    components: [joy, pain, hope],
    context: "love-blended, present-moment"
  }
```

**Step 3.3: Extract Result**
The reduced net produces a result node containing:
- Semantic embedding (vector in meaning space)
- Metadata about the computation
- Confidence scores

### HVM Runtime Model

```c
// Simplified HVM execution model

typedef struct Node {
    NodeType type;
    Port principal;
    Port aux[MAX_PORTS];
    void* data;
} Node;

typedef struct Port {
    Node* node;
    uint8_t slot;
} Port;

// Active pair: two principal ports connected
typedef struct ActivePair {
    Node* node_a;
    Node* node_b;
} ActivePair;

// Reduction rules provided by LLM
typedef Node* (*ReductionRule)(Node* a, Node* b, LLMContext* ctx);

// Main reduction loop
void reduce_net(Net* net, LLMContext* llm_ctx) {
    while (has_active_pairs(net)) {
        // Get all active pairs
        ActivePair* pairs = find_active_pairs(net);

        // Reduce in parallel
        #pragma omp parallel for
        for (int i = 0; i < num_pairs; i++) {
            Node* a = pairs[i].node_a;
            Node* b = pairs[i].node_b;

            // Get reduction rule for this pair
            ReductionRule rule = get_rule(a->type, b->type, llm_ctx);

            // Apply rule, producing new subgraph
            Node* result = rule(a, b, llm_ctx);

            // Wire result into the net
            replace_pair(net, &pairs[i], result);
        }
    }
}

// Example: Blend operator reduction
Node* reduce_blend(Node* blend_op, Node* values, LLMContext* llm) {
    // Extract semantic embeddings from value nodes
    float** embeddings = extract_embeddings(values);
    int count = count_values(values);

    // Call LLM to compute semantic intersection
    float* result_embedding = llm_compute_intersection(
        llm,
        embeddings,
        count,
        blend_op->data  // "love" as blending subject
    );

    // Create result node
    Node* result = create_value_node(result_embedding);
    return result;
}
```

### Pseudocode

```python
def reduce_net_with_hvm(net: InteractionNet, llm: LLM) -> Node:
    """
    Phase 3: Reduce interaction net using HVM with LLM-provided rules.
    """
    # Step 1: Serialize net to HVM format
    hvm_net = serialize_for_hvm(net)

    # Step 2: Provide LLM-based reduction rules to HVM
    hvm_rules = create_reduction_rules(llm)
    hvm_context = HVMContext(llm=llm)

    # Step 3: Run HVM reduction
    while hvm_net.has_active_pairs():
        active_pairs = hvm_net.get_active_pairs()

        # Parallel reduction
        for pair in parallel(active_pairs):
            node_a, node_b = pair

            # Get reduction rule
            rule = hvm_rules.get(node_a.type, node_b.type)

            # Apply rule (may call back to LLM for semantic computation)
            result_subgraph = rule(node_a, node_b, hvm_context)

            # Replace pair with result
            hvm_net.replace(pair, result_subgraph)

    # Step 4: Extract final result
    result_node = hvm_net.get_result()

    return result_node

def create_reduction_rules(llm: LLM) -> Dict[Tuple[str, str], Callable]:
    """
    Create HVM reduction rules that use LLM for semantic operations.
    """
    rules = {}

    # Blend operator: compute semantic intersection
    rules[("blend", "values")] = lambda a, b, ctx: reduce_blend(a, b, ctx.llm)

    # Temporal operator: apply time context
    rules[("temporal", "value")] = lambda a, b, ctx: apply_temporal(a, b, ctx.llm)

    # Query operator: resolve to meaning
    rules[("query", "value")] = lambda a, b, ctx: resolve_query(a, b, ctx.llm)

    return rules

def reduce_blend(blend_node: Node, values_node: Node, llm: LLM) -> Node:
    """
    Reduce blend operator using LLM to compute semantic intersection.
    """
    # Extract value embeddings
    embeddings = [v.embedding for v in values_node.children]
    subject = blend_node.subject.embedding

    # Use LLM to compute intersection in semantic space
    prompt = f"""
    Compute the semantic intersection of these concepts:
    Values: {values_node.children}
    Blending through: {blend_node.subject}

    Return the vector that represents their intersection in meaning space.
    """

    result_embedding = llm.compute_semantic_intersection(
        embeddings,
        subject,
        prompt
    )

    # Create result node
    result = Node("value", embedding=result_embedding)
    return result
```

---

## Phase 4: Interpretation Phase

### Overview
The LLM interprets the reduced net result back into natural language meaning or structured output.

### Process

**Input:** Result node from HVM reduction containing:
```python
{
    "type": "value",
    "embedding": [0.6, 0.7, 0.8, ...],  # 1536-dim vector
    "metadata": {
        "source_components": ["joy", "pain", "hope"],
        "blending_subject": "love",
        "temporal_context": "@now",
        "confidence": 0.85
    }
}
```

**Step 4.1: Embedding Analysis**
The LLM analyzes the result embedding to find nearest concepts:
```python
nearest_concepts = llm.find_nearest(result_embedding, top_k=10)
# Result: [
#   ("compassion", 0.92),
#   ("bittersweet", 0.89),
#   ("tenderness", 0.87),
#   ("empathy", 0.84),
#   ...
# ]
```

**Step 4.2: Semantic Reconstruction**
The LLM reconstructs meaning from the embedding and metadata:
```
Prompt to LLM:
"The computation blended joy, pain, and hope through the lens of love
 in the present moment. The result embedding is closest to: compassion,
 bittersweet, tenderness, empathy. What is the meaning?"

LLM Response:
"The blending of joy, pain, and hope through love in the present moment
 yields compassionate awareness - the capacity to hold seemingly
 contradictory emotions with tenderness. This is the bittersweet
 recognition that love encompasses both suffering and joy."
```

**Step 4.3: Structured Output Generation**
For programmatic use, the LLM can generate structured output:
```json
{
  "primary_meaning": "compassionate_awareness",
  "components": {
    "joy": 0.35,
    "pain": 0.30,
    "hope": 0.35
  },
  "blending_effect": "love transforms contradiction into unity",
  "temporal_aspect": "present-moment awareness",
  "related_concepts": [
    "bittersweet",
    "tender",
    "empathetic",
    "whole-hearted"
  ],
  "explanation": "Love's capacity to hold joy and pain simultaneously,
                  creating hopeful presence with suffering",
  "confidence": 0.85
}
```

### Data Structure

```python
class InterpretationResult:
    def __init__(self):
        self.natural_language: str = ""
        self.structured_data: Dict[str, Any] = {}
        self.confidence: float = 0.0
        self.alternative_interpretations: List[str] = []
        self.reasoning_trace: List[str] = []

class SemanticNeighborhood:
    def __init__(self, center_embedding: np.ndarray):
        self.center = center_embedding
        self.neighbors: List[Tuple[str, float]] = []  # (concept, similarity)
        self.dimensions: Dict[str, float] = {}  # semantic dimensions
```

### Pseudocode

```python
def interpret_result(result_node: Node, llm: LLM, metadata: Dict) -> InterpretationResult:
    """
    Phase 4: Interpret reduced net result back to meaning.
    """
    interpretation = InterpretationResult()

    # Step 1: Analyze embedding
    embedding = result_node.embedding
    neighborhood = analyze_semantic_neighborhood(embedding, llm)

    # Step 2: Construct interpretation prompt
    prompt = build_interpretation_prompt(
        embedding,
        neighborhood,
        metadata
    )

    # Step 3: Get LLM interpretation
    natural_language = llm.generate(prompt)
    interpretation.natural_language = natural_language

    # Step 4: Generate structured output
    structured = generate_structured_output(
        embedding,
        neighborhood,
        metadata,
        llm
    )
    interpretation.structured_data = structured

    # Step 5: Assess confidence
    interpretation.confidence = compute_confidence(
        embedding,
        neighborhood,
        natural_language
    )

    return interpretation

def analyze_semantic_neighborhood(embedding: np.ndarray, llm: LLM) -> SemanticNeighborhood:
    """
    Find nearby concepts in semantic space.
    """
    neighborhood = SemanticNeighborhood(embedding)

    # Find k-nearest concepts
    neighbors = llm.embedding_search(embedding, k=20)
    neighborhood.neighbors = neighbors

    # Analyze semantic dimensions
    # (what aspects of meaning does this embedding emphasize?)
    dimensions = llm.analyze_dimensions(embedding)
    neighborhood.dimensions = dimensions
    # e.g., {"emotional": 0.9, "abstract": 0.7, "relational": 0.8}

    return neighborhood

def build_interpretation_prompt(
    embedding: np.ndarray,
    neighborhood: SemanticNeighborhood,
    metadata: Dict
) -> str:
    """
    Construct prompt for LLM to interpret the result.
    """
    prompt = f"""
    A computation has reduced to the following semantic state:

    Original operation: {metadata['operation']}
    Components: {metadata['components']}
    Context: {metadata['context']}

    The result embedding is nearest to these concepts:
    {format_neighbors(neighborhood.neighbors[:10])}

    Semantic dimensions:
    {format_dimensions(neighborhood.dimensions)}

    Please interpret what this result means. Consider:
    1. What is the primary meaning?
    2. How did the components combine?
    3. What emerged that wasn't in the original parts?
    4. What is the emotional/semantic quality?

    Provide both:
    - A natural language explanation
    - A structured breakdown
    """

    return prompt
```

---

## Example: Complete Execution Trace

### Input

```limn
@now ∿lov[joy,pai,hop] → ?
```

### Trace

#### Phase 1: Parse (LLM)

```
LLM Input: "@now ∿lov[joy,pai,hop] → ?"

LLM Processing:
  - Tokenize: [@now, ∿, lov, [joy,pai,hop], →, ?]
  - Parse semantics:
    * @now: temporal constraint (present moment)
    * ∿lov: blend operator with subject "love"
    * [joy,pai,hop]: list of values to blend
    * → ?: query for result

  - Extract constraints:
    * Temporal: present moment
    * Operational: blend/intersect
    * Subject: love as medium
    * Components: {joy, pain, hope}

Output: SemanticParse {
  operations: [BLEND, TEMPORAL, QUERY],
  values: [love, joy, pain, hope],
  constraints: [present_moment]
}
```

#### Phase 2: Net Generation (LLM)

```
LLM Generates Net:

Nodes:
  N1: Value(love)     embedding=[0.9, 0.7, 0.6, ...]
  N2: Value(joy)      embedding=[0.8, 0.9, 0.3, ...]
  N3: Value(pain)     embedding=[0.2, 0.1, 0.8, ...]
  N4: Value(hope)     embedding=[0.7, 0.6, 0.9, ...]
  N5: Operator(BLEND) subject=N1, inputs=[N2,N3,N4]
  N6: Operator(TEMPORAL) constraint=@now, input=N5
  N7: Operator(QUERY) input=N6

Connections:
  N1.out → N5.subject
  N2.out → N5.in[0]
  N3.out → N5.in[1]
  N4.out → N5.in[2]
  N5.out → N6.in
  N6.out → N7.in
  N7.out → RESULT

Active Pairs:
  (N5, [N2,N3,N4]) - BLEND ready to reduce
```

#### Phase 3: Reduction (HVM + LLM)

```
HVM Reduction Steps:

Step 1: Reduce BLEND
  Active: N5(BLEND) connected to [N2(joy), N3(pain), N4(hope)]

  HVM calls reduction rule:
    reduce_blend(N5, [N2, N3, N4], LLM_context)

  LLM computes semantic intersection:
    embeddings = [
      [0.8, 0.9, 0.3, ...],  # joy
      [0.2, 0.1, 0.8, ...],  # pain
      [0.7, 0.6, 0.9, ...]   # hope
    ]
    subject = [0.9, 0.7, 0.6, ...]  # love

    result = weighted_intersection(embeddings, subject)
           = [0.6, 0.7, 0.8, ...]  # "compassionate awareness"

  HVM replaces:
    N5, N2, N3, N4 → N8: Value(blended) embedding=[0.6, 0.7, 0.8, ...]

Step 2: Reduce TEMPORAL
  Active: N6(TEMPORAL) connected to N8(blended)

  HVM calls:
    reduce_temporal(N6, N8, LLM_context)

  LLM applies temporal context:
    constraint = @now (present moment)
    input = [0.6, 0.7, 0.8, ...]

    result = apply_temporal_filter(input, @now)
           = [0.65, 0.72, 0.78, ...]  # present-moment contextualized

  HVM replaces:
    N6, N8 → N9: Value(temporal) embedding=[0.65, 0.72, 0.78, ...]

Step 3: Reduce QUERY
  Active: N7(QUERY) connected to N9(temporal)

  HVM calls:
    reduce_query(N7, N9, LLM_context)

  LLM resolves:
    query_type = "what is this?"
    input = [0.65, 0.72, 0.78, ...]

    result = resolve_to_meaning(input)

  HVM replaces:
    N7, N9 → N10: Result embedding=[0.65, 0.72, 0.78, ...]
                   metadata={...}

No more active pairs. Reduction complete.

Final Result Node: N10
```

#### Phase 4: Interpretation (LLM)

```
LLM Interpretation:

Input: Result Node N10 {
  embedding: [0.65, 0.72, 0.78, ...],
  metadata: {
    components: [joy, pain, hope],
    subject: love,
    temporal: @now,
    confidence: 0.85
  }
}

LLM finds nearest concepts:
  1. "compassion" (0.92)
  2. "bittersweet" (0.89)
  3. "tenderness" (0.87)
  4. "acceptance" (0.85)
  5. "wholehearted" (0.83)

LLM generates interpretation:
  Natural Language:
    "The present-moment blend of joy, pain, and hope through love
     yields compassionate awareness - a bittersweet tenderness that
     accepts the full spectrum of human experience. It is the capacity
     to hold contradictory emotions with an open heart, recognizing
     that love encompasses both suffering and delight."

  Structured Output:
    {
      "primary_meaning": "compassionate_awareness",
      "emotional_quality": "bittersweet",
      "components_contribution": {
        "joy": 0.35,
        "pain": 0.30,
        "hope": 0.35
      },
      "emergent_property": "wholehearted acceptance",
      "temporal_aspect": "present-moment awareness",
      "explanation": "Love's transformative capacity to hold
                      contradictions as a unified whole"
    }

Output returned to user.
```

---

## Communication Protocol

### LLM ↔ HVM Interface

**Message Types:**

1. **Net Submission** (LLM → HVM)
```json
{
  "type": "submit_net",
  "net": {
    "nodes": [...],
    "connections": [...],
    "metadata": {...}
  },
  "reduction_rules": {
    "blend": "llm_callback://reduce_blend",
    "temporal": "llm_callback://reduce_temporal",
    "query": "llm_callback://reduce_query"
  }
}
```

2. **Reduction Callback** (HVM → LLM)
```json
{
  "type": "reduction_callback",
  "rule": "blend",
  "inputs": {
    "subject": {"embedding": [...]},
    "values": [
      {"embedding": [...]},
      {"embedding": [...]}
    ]
  },
  "request_id": "uuid-1234"
}
```

3. **Reduction Result** (LLM → HVM)
```json
{
  "type": "reduction_result",
  "request_id": "uuid-1234",
  "result": {
    "type": "value",
    "embedding": [...],
    "metadata": {...}
  }
}
```

4. **Completion** (HVM → LLM)
```json
{
  "type": "reduction_complete",
  "result_node": {
    "embedding": [...],
    "metadata": {...}
  },
  "stats": {
    "reductions": 15,
    "llm_callbacks": 3,
    "time_ms": 234
  }
}
```

### Protocol Pseudocode

```python
class LLMHVMProtocol:
    """
    Bidirectional communication between LLM and HVM.
    """
    def __init__(self, llm: LLM, hvm: HVM):
        self.llm = llm
        self.hvm = hvm
        self.pending_callbacks = {}

    def execute(self, lmn_source: str) -> InterpretationResult:
        """
        Full execution pipeline.
        """
        # Phase 1: LLM parses source
        parse = self.llm.parse(lmn_source)

        # Phase 2: LLM generates net
        net = self.llm.generate_net(parse)

        # Register LLM callback handlers
        self.hvm.register_callback("blend", self.handle_blend)
        self.hvm.register_callback("temporal", self.handle_temporal)
        self.hvm.register_callback("query", self.handle_query)

        # Phase 3: Submit to HVM for reduction
        result_node = self.hvm.reduce(net)

        # Phase 4: LLM interprets result
        interpretation = self.llm.interpret(result_node)

        return interpretation

    def handle_blend(self, request: Dict) -> Dict:
        """
        LLM callback for blend operator reduction.
        """
        subject = request["inputs"]["subject"]["embedding"]
        values = [v["embedding"] for v in request["inputs"]["values"]]

        # LLM computes semantic blend
        result_embedding = self.llm.compute_blend(values, subject)

        return {
            "type": "value",
            "embedding": result_embedding,
            "metadata": {"operation": "blend"}
        }

    # Similar handlers for temporal, query, etc.
```

---

## Pros and Cons

### Advantages

1. **Semantic Flexibility**
   - LLM handles the "meaning" layer naturally
   - Can parse ambiguous or context-dependent Limn expressions
   - Adapts to new vocabulary without recompilation

2. **Computational Efficiency**
   - HVM handles parallel reduction very efficiently
   - Optimal sharing of computations
   - No garbage collection overhead

3. **Hybrid Strengths**
   - Combines symbolic (HVM) and subsymbolic (LLM) approaches
   - LLM provides semantic grounding, HVM provides computational rigor
   - Clean separation of concerns

4. **Extensibility**
   - New operators just need LLM reduction rules
   - No need to recompile HVM runtime
   - Can leverage LLM improvements over time

5. **Bootstrappability**
   - LLM can learn new Limn constructs from examples
   - Natural language specifications become executable
   - In-context learning enables rapid prototyping

### Disadvantages

1. **Latency**
   - LLM calls add significant latency (100-1000ms per callback)
   - Not suitable for real-time applications
   - Could batch reductions to amortize LLM overhead

2. **Non-Determinism**
   - LLM outputs are stochastic
   - Same Limn code might produce different results
   - Need caching or pinning for reproducibility

3. **Cost**
   - LLM API calls are expensive
   - Each reduction callback costs money
   - Not economical for high-volume applications

4. **Complexity**
   - Two runtimes to maintain (LLM + HVM)
   - Network communication overhead
   - Debugging is harder across the boundary

5. **Trust Boundary**
   - LLM reduction rules are black boxes
   - Hard to verify correctness formally
   - Could produce semantically plausible but incorrect results

6. **Dependency**
   - Requires external LLM service
   - Vulnerable to API changes or downtime
   - Not suitable for offline or embedded use

### Trade-off Analysis

**When LLM-First Makes Sense:**
- Exploratory/research applications
- Low-volume, high-value computations
- Applications where semantic flexibility > speed
- Prototyping new language features
- Educational demonstrations

**When LLM-First Doesn't Make Sense:**
- Production systems requiring determinism
- Real-time or low-latency requirements
- High-volume/cost-sensitive applications
- Safety-critical systems
- Offline or embedded deployments

---

## Alternative Architectures

### Model B: Compiled + LLM Assist
- Compile Limn to HVM directly
- Use LLM only for ambiguity resolution
- Pros: Faster, deterministic
- Cons: Less flexible, requires fixed semantics

### Model C: Pure LLM
- LLM does everything (parsing, reduction, interpretation)
- No HVM at all
- Pros: Simpler, more flexible
- Cons: Slow, expensive, no computational guarantees

### Model D: Hybrid with Cache
- Compile common patterns to HVM
- Use LLM for novel/ambiguous cases
- Cache LLM results for reuse
- Pros: Best of both worlds
- Cons: Complex cache invalidation

---

## Future Enhancements

### 1. Batch Reduction
Accumulate multiple LLM callbacks and batch them:
```python
# Instead of 5 separate LLM calls:
result1 = llm.reduce_blend(...)
result2 = llm.reduce_blend(...)
result3 = llm.reduce_temporal(...)
...

# Batch into 1 call:
results = llm.batch_reduce([
    ("blend", args1),
    ("blend", args2),
    ("temporal", args3),
    ...
])
```

### 2. Result Caching
Cache LLM reduction results with embedding-based lookup:
```python
def reduce_with_cache(operation: str, inputs: List[np.ndarray]) -> np.ndarray:
    # Create cache key from operation + input embeddings
    cache_key = hash(operation, inputs)

    if cache_key in reduction_cache:
        return reduction_cache[cache_key]

    # Not cached, call LLM
    result = llm.reduce(operation, inputs)
    reduction_cache[cache_key] = result
    return result
```

### 3. Partial Compilation
For well-defined operators, compile directly to HVM:
```python
# User marks operator as "pure" and provides spec
@pure
def add_numbers(a: Number, b: Number) -> Number:
    return a + b

# Compiler generates HVM code directly, no LLM needed
# Only "semantic" operators use LLM callbacks
```

### 4. Approximate Reduction
For non-critical computations, use smaller/faster models:
```python
# High-stakes reduction (user-facing)
result = llm_opus.reduce_critical(...)

# Intermediate reduction (internal)
result = llm_haiku.reduce_approximate(...)
```

### 5. Streaming Interpretation
Return partial results as reduction progresses:
```python
async def reduce_and_stream(net: InteractionNet):
    async for partial_result in hvm.reduce_streaming(net):
        # Interpret partial state
        interpretation = llm.interpret_partial(partial_result)
        yield interpretation

    # Final result
    final = llm.interpret_final(partial_result)
    yield final
```

---

## Implementation Roadmap

### Phase 1: Proof of Concept (2-4 weeks)
- [ ] Implement basic LLM parser for simple Limn expressions
- [ ] Create minimal interaction net representation
- [ ] Build mock HVM reducer (Python-based)
- [ ] Implement LLM-based operator reductions
- [ ] Demonstrate end-to-end execution on `∿lov[joy,pai,hop]`

### Phase 2: HVM Integration (4-6 weeks)
- [ ] Interface with real HVM runtime
- [ ] Implement LLM callback protocol
- [ ] Build serialization layer (Python ↔ HVM)
- [ ] Benchmark reduction performance
- [ ] Optimize callback batching

### Phase 3: Language Coverage (6-8 weeks)
- [ ] Expand parser to handle full Limn syntax
- [ ] Implement all operator reductions
- [ ] Add constraint handling
- [ ] Support key mechanism
- [ ] Build comprehensive test suite

### Phase 4: Optimization (4-6 weeks)
- [ ] Implement result caching
- [ ] Add partial compilation
- [ ] Optimize embedding operations
- [ ] Reduce LLM calls via batching
- [ ] Profile and tune performance

### Phase 5: Production Hardening (6-8 weeks)
- [ ] Error handling and recovery
- [ ] Logging and observability
- [ ] Deterministic mode (pinned LLM)
- [ ] Cost tracking and budgets
- [ ] Documentation and examples

**Total Estimated Time:** 22-32 weeks (5.5-8 months)

---

## Conclusion

The LLM-First architecture for LMN/Limn provides a fascinating hybrid approach that leverages the semantic understanding of language models with the computational rigor of interaction net reduction. While not suitable for all applications due to latency and cost concerns, it offers unique advantages for exploratory, research, and educational contexts.

The key insight is the **division of labor**: LLMs handle meaning (parsing, semantic operations, interpretation) while HVM handles computation (parallel reduction, optimal sharing). This separation allows each component to excel at what it does best.

Future work should explore caching, batching, and partial compilation to make this architecture more practical for production use while retaining its semantic flexibility.

---

**Author:** Polecat toast
**Date:** 2026-02-01
**Status:** Theoretical specification - implementation pending
