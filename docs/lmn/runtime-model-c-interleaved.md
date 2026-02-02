# LMN Runtime Model C: Interleaved Router

**Status:** Design Specification
**Author:** Polecat cheedo
**Date:** 2026-02-01
**Purpose:** Explore hybrid HVM/LLM execution for Limn semantics

---

## Executive Summary

Runtime Model C proposes a **hybrid execution model** where Limn expressions are evaluated through interleaved routing between:

1. **HVM (Higher-order Virtual Machine)** - for structural, constraint-based operations
2. **LLM (Large Language Model)** - for semantic, context-dependent interpretations

Each node in the AST is classified as either **structural** or **semantic**, and routed to the appropriate evaluator. Results are woven together through a unified state manager.

**Key Insight:** Not all Limn operations are pure constraints. Some fundamentally require understanding context, narrative, and natural language semantics that LLMs excel at, while others are pure algebraic/logical operations best handled by HVM.

---

## 1. Classification: Structural vs Semantic

### 1.1 Decision Criteria

A node is **structural** if:
- Its semantics are **context-independent** (same meaning regardless of key/context)
- It can be expressed as **pure constraint satisfaction**
- It has **well-defined algebraic/logical rules**
- Examples: arithmetic (`joi`, `cut`, `exp`, `con`), relations (`sa`, `ma`, `mi`), quantifiers (`al`, `ex`, `on`)

A node is **semantic** if:
- Its meaning **depends on context** (key, narrative, shared knowledge)
- It requires **natural language understanding**
- It involves **ambiguity collapse** via contextual inference
- Examples: domain words (e.g., `lov`, `joy`, `pai`, `hop`), metaphor operators, narrative references

### 1.2 Classification Table

| Node Type | Category | Rationale |
|-----------|----------|-----------|
| Arithmetic (`joi`, `cut`, `exp`, `con`) | Structural | Pure algebraic operations |
| Relations (`sa`, `ma`, `mi`, `eq`) | Structural | Constraint satisfaction |
| Quantifiers (`al`, `ex`, `on`) | Structural | First-order logic |
| Boolean (`nu`, `ve`, `so`) | Structural | Logical operations |
| Lists/Groups (`par`, `who`, `fst`, `nxt`) | Structural | Data structure operations |
| Domain Words (e.g., `lov`, `joy`, `lux`, `tra`) | Semantic | Context-dependent meaning |
| Operators on Words (`∿`, `@`, `→`) | Semantic | Require semantic composition |
| Key-dependent Phrases | Semantic | Collapse via shared context |
| Metaphors/Analogies | Semantic | Natural language understanding |

### 1.3 Mixed Nodes

Some nodes are **hybrid** - they have both structural and semantic components:

```
Example: lov[joy, pai, hop]
- Structural: List construction with 3 elements
- Semantic: Meaning of "lov" and relationship between elements
```

**Resolution Strategy:**
1. Extract structural skeleton (list of length 3)
2. Route semantic content (word meanings) to LLM
3. Weave results (structural shape + semantic content)

### 1.4 Classification Algorithm

```python
def classify_node(node, context):
    """
    Classify a node as STRUCTURAL, SEMANTIC, or HYBRID.

    Returns: (category, confidence, explanation)
    """
    # Pure arithmetic/logic
    if node.type in ['joi', 'cut', 'exp', 'con', 'sa', 'ma', 'mi',
                      'al', 'ex', 'on', 'nu', 've', 'so']:
        return ('STRUCTURAL', 1.0, 'Pure algebraic/logical operation')

    # Data structures
    if node.type in ['par', 'who', 'fst', 'nxt', 'fin']:
        return ('STRUCTURAL', 1.0, 'Data structure operation')

    # Domain words
    if node.type == 'word' and node.value in VOCABULARY:
        # Check if word has context-independent meaning
        word_meta = VOCABULARY[node.value]
        if word_meta.get('structural_meaning'):
            return ('STRUCTURAL', 0.8, 'Word with fixed meaning')
        else:
            return ('SEMANTIC', 0.9, 'Context-dependent word')

    # Operators on semantic content
    if node.type in ['∿', '@', '→'] and has_semantic_children(node):
        return ('HYBRID', 0.8, 'Operator on semantic content')

    # Lists with semantic content
    if node.type == 'list' and any(is_semantic(child) for child in node.children):
        return ('HYBRID', 0.9, 'Structural container with semantic elements')

    # Default: semantic
    return ('SEMANTIC', 0.5, 'Default classification')
```

**Classification is Dynamic:** The classifier runs at evaluation time, allowing it to adapt based on:
- Current context/key
- Runtime type information
- Partial evaluation results

---

## 2. Routing Logic

### 2.1 Router Architecture

```
┌─────────────────────────────────────────┐
│           Limn AST Node                 │
└─────────────┬───────────────────────────┘
              │
              ▼
┌─────────────────────────────────────────┐
│      Node Classifier                    │
│  (Structural / Semantic / Hybrid)       │
└─────────────┬───────────────────────────┘
              │
        ┌─────┴─────┐
        │           │
        ▼           ▼
┌──────────┐  ┌──────────┐
│ STRUCT   │  │ SEMANTIC │
│  Route   │  │  Route   │
└────┬─────┘  └────┬─────┘
     │             │
     ▼             ▼
┌─────────┐  ┌─────────┐
│   HVM   │  │   LLM   │
│ Reducer │  │ Interp  │
└────┬────┘  └────┬────┘
     │             │
     └──────┬──────┘
            ▼
  ┌──────────────────┐
  │  Result Weaver   │
  │ (Unified State)  │
  └──────────────────┘
```

### 2.2 Routing Decision Tree

```python
def route_node(node, state):
    """
    Route a node to the appropriate evaluator.

    Returns: (evaluator, node_transformed)
    """
    category, confidence, _ = classify_node(node, state.context)

    if category == 'STRUCTURAL':
        # Route to HVM
        return (HVM_EVALUATOR, transform_to_hvm(node))

    elif category == 'SEMANTIC':
        # Route to LLM
        return (LLM_EVALUATOR, transform_to_llm_prompt(node, state))

    elif category == 'HYBRID':
        # Split into structural and semantic components
        struct_part, sem_part = decompose_hybrid(node)

        # Create execution plan
        plan = ExecutionPlan([
            Step('structural', HVM_EVALUATOR, struct_part),
            Step('semantic', LLM_EVALUATOR, sem_part),
            Step('weave', WEAVER, merge_results)
        ])
        return (PLAN_EXECUTOR, plan)

    else:
        raise RuntimeError(f"Unknown category: {category}")
```

### 2.3 HVM Transformation

Structural nodes are transformed to HVM/Bend code:

```python
def transform_to_hvm(node):
    """
    Convert structural AST node to HVM/Bend code.
    """
    if node.type == 'joi':  # Addition
        return f"(+ {transform_to_hvm(node.left)} {transform_to_hvm(node.right)})"

    elif node.type == 'sa':  # Equality constraint
        return f"(== {transform_to_hvm(node.left)} {transform_to_hvm(node.right)})"

    elif node.type == 'par':  # List construction
        elements = [transform_to_hvm(e) for e in node.elements]
        return f"[{' '.join(elements)}]"

    # ... more transformations
```

### 2.4 LLM Transformation

Semantic nodes are transformed to LLM prompts with context:

```python
def transform_to_llm_prompt(node, state):
    """
    Convert semantic AST node to LLM prompt with context.
    """
    # Build prompt with context
    prompt = f"""# Limn Semantic Interpretation

## Context
{format_context(state.context)}

## Current State
{format_state(state.variables)}

## Expression to Interpret
{node.to_limn_text()}

## Task
Interpret the meaning of this Limn expression given the context and state.
Provide:
1. The collapsed meaning (what it refers to)
2. Relevant constraints or relationships
3. Confidence score (0-1)

Return as JSON:
{{
  "meaning": "...",
  "constraints": [...],
  "confidence": 0.0-1.0,
  "reasoning": "..."
}}
"""
    return prompt
```

---

## 3. Weaving: Combining HVM and LLM Results

### 3.1 Weaving Strategies

#### Strategy 1: Sequential Composition
HVM result flows into LLM (or vice versa):

```
Example: @now ∿lov[joy,pai,hop] → ?

Step 1 (STRUCTURAL): Evaluate @now
  HVM: current_time = 2026-02-01T14:55:00Z

Step 2 (SEMANTIC): Interpret ∿lov[joy,pai,hop]
  LLM: "Apply the love operator to the set {joy, pain, hope}"
  Context: "In the domain of human emotions..."
  Result: "The spectrum of love's emotional range"

Step 3 (WEAVE): Combine
  Final: "At this moment (2026-02-01T14:55), contemplate the emotional
          spectrum of love: joy, pain, and hope"
```

#### Strategy 2: Parallel Execution
HVM and LLM run independently, results merged:

```
Node: lov[1+1, joy]

Parallel:
  HVM: Evaluate 1+1 → 2
  LLM: Interpret "joy" → "happiness, delight"

Weave: lov[2, "happiness/delight"]
```

#### Strategy 3: Iterative Refinement
HVM provides constraints, LLM interprets within bounds:

```
Expression: x ma 0 | x mi 10 | x ∿lov

Step 1 (STRUCTURAL): HVM solves constraints
  Result: x ∈ {1, 2, 3, 4, 5, 6, 7, 8, 9}

Step 2 (SEMANTIC): LLM interprets ∿lov with bounds
  Prompt: "Given x is in range [1,9], what does 'x ∿ lov' mean?"
  Context: "In numerology of emotion..."
  Result: "x = 7 (seven is the mystical love number)"

Weave: x = 7 (constraint-satisfying semantic interpretation)
```

### 3.2 Weaver Implementation

```python
class ResultWeaver:
    """
    Weaves together HVM and LLM evaluation results.
    """

    def weave(self, hvm_result, llm_result, node):
        """
        Combine results from HVM and LLM evaluators.
        """
        if hvm_result.type == 'constraint':
            # HVM provided constraints, LLM provides meaning
            return self.weave_constraint_meaning(hvm_result, llm_result)

        elif hvm_result.type == 'value' and llm_result.type == 'interpretation':
            # HVM computed value, LLM interpreted context
            return self.weave_value_interpretation(hvm_result, llm_result)

        elif hvm_result.type == 'structure' and llm_result.type == 'content':
            # HVM built structure, LLM filled semantic content
            return self.weave_structure_content(hvm_result, llm_result)

        else:
            raise WeavingError(f"Cannot weave {hvm_result.type} + {llm_result.type}")

    def weave_constraint_meaning(self, constraint, meaning):
        """
        Example: HVM says x ∈ [1,9], LLM says x = 7 (love number)
        """
        # Verify LLM result satisfies HVM constraint
        if not constraint.satisfies(meaning.value):
            return Error(f"LLM result {meaning.value} violates constraint {constraint}")

        # Return combined result
        return WovenResult(
            value=meaning.value,
            constraint=constraint,
            interpretation=meaning.interpretation,
            confidence=meaning.confidence
        )

    def weave_structure_content(self, structure, content):
        """
        Example: HVM built list [_, _, _], LLM filled [joy, pain, hope]
        """
        if len(structure.slots) != len(content.values):
            return Error(f"Structure/content mismatch: {len(structure.slots)} slots, {len(content.values)} values")

        # Fill structure with content
        filled = structure.copy()
        for i, value in enumerate(content.values):
            filled.slots[i] = value

        return WovenResult(
            structure=filled,
            semantic_content=content,
            confidence=content.confidence
        )
```

### 3.3 Conflict Resolution

What if HVM and LLM results conflict?

```python
def resolve_conflict(hvm_result, llm_result):
    """
    Resolve conflicts between HVM and LLM results.

    Priority:
    1. Hard constraints (HVM) always win
    2. Soft interpretations (LLM) must satisfy constraints
    3. If irreconcilable, return error
    """
    if hvm_result.is_hard_constraint():
        if not hvm_result.satisfies(llm_result.value):
            return ConflictError(
                f"LLM interpretation {llm_result.value} violates "
                f"hard constraint {hvm_result.constraint}"
            )

    # Try to merge
    merged = attempt_merge(hvm_result, llm_result)
    if merged:
        return merged

    # Ask LLM to re-interpret with constraint
    revised = llm_reinterpret_with_constraint(
        llm_result.node,
        hvm_result.constraint
    )
    return revised
```

---

## 4. State Management

### 4.1 Who Owns the State?

**Answer:** The **Runtime State Manager** owns unified state, shared by both HVM and LLM.

```python
class RuntimeState:
    """
    Unified state manager for interleaved execution.
    """

    def __init__(self):
        # Structural bindings (HVM)
        self.hvm_bindings = {}  # var_name → HVM value

        # Semantic bindings (LLM)
        self.llm_bindings = {}  # var_name → semantic interpretation

        # Context (shared)
        self.context = Context()  # Key, narrative, domain

        # Execution trace
        self.trace = []  # History of evaluations

        # Constraint store
        self.constraints = ConstraintStore()

    def bind(self, var_name, value, source):
        """
        Bind a variable to a value from HVM or LLM.
        """
        if source == 'HVM':
            self.hvm_bindings[var_name] = value
            # Propagate to constraint store
            self.constraints.add(Constraint(var_name, '==', value))

        elif source == 'LLM':
            self.llm_bindings[var_name] = value
            # Store as soft constraint
            self.constraints.add_soft(
                Constraint(var_name, 'interpreted_as', value),
                confidence=value.confidence
            )

        # Update trace
        self.trace.append(Binding(var_name, value, source))

    def lookup(self, var_name, preferred_source=None):
        """
        Look up a variable's value.

        If bound in both HVM and LLM, weave the results.
        """
        hvm_val = self.hvm_bindings.get(var_name)
        llm_val = self.llm_bindings.get(var_name)

        if hvm_val and llm_val:
            # Weave if both exist
            return weave_results(hvm_val, llm_val)
        elif hvm_val:
            return hvm_val
        elif llm_val:
            return llm_val
        else:
            raise UnboundVariableError(var_name)

    def get_context(self):
        """Get current evaluation context."""
        return self.context

    def update_context(self, key=None, narrative=None, domain=None):
        """Update context from key or narrative."""
        if key:
            self.context.key = key
        if narrative:
            self.context.narrative = narrative
        if domain:
            self.context.domain = domain
```

### 4.2 State Synchronization

HVM and LLM operate on the same logical state, but with different representations:

```
┌──────────────────────────────────────┐
│      Runtime State Manager           │
│                                      │
│  ┌────────────┐    ┌──────────────┐ │
│  │ HVM State  │    │  LLM State   │ │
│  │────────────│    │──────────────│ │
│  │ x = 7      │◄──►│ x: "seven"   │ │
│  │ y = [1,2]  │    │ y: "pair"    │ │
│  │ z ∈ [0,10] │    │ z: "bounded" │ │
│  └────────────┘    └──────────────┘ │
│         │                  │         │
│         └────────┬─────────┘         │
│                  ▼                   │
│         ┌──────────────┐             │
│         │ Constraint   │             │
│         │   Store      │             │
│         └──────────────┘             │
└──────────────────────────────────────┘
```

**Synchronization Rules:**
1. HVM bindings are **hard constraints** (must be satisfied)
2. LLM bindings are **soft interpretations** (can be revised)
3. On conflict, HVM wins (constraints are truth)
4. LLM re-interprets to satisfy HVM constraints

### 4.3 Context Passing

Context flows from state to LLM on every semantic evaluation:

```python
def evaluate_semantic_node(node, state):
    """
    Evaluate a semantic node using LLM with current context.
    """
    # Extract relevant context
    context = state.get_context()

    # Build prompt with context
    prompt = build_semantic_prompt(
        node=node,
        context=context,
        bindings=state.llm_bindings,
        constraints=state.constraints
    )

    # Query LLM
    llm_result = llm.query(prompt)

    # Update state with result
    state.bind(node.var_name, llm_result, source='LLM')

    return llm_result
```

---

## 5. Example Trace: `@now ∿lov[joy,pai,hop] → ?`

### 5.1 Expression Analysis

```
Expression: @now ∿lov[joy,pai,hop] → ?

AST:
  SequenceOp(→)
  ├─ Application(∿)
  │  ├─ Temporal(@now)
  │  └─ Apply(lov)
  │     └─ List[joy, pai, hop]
  └─ Query(?)
```

### 5.2 Classification Pass

```
Node: @now
  Type: Temporal operator
  Classification: SEMANTIC (time is context-dependent)
  Routing: LLM

Node: ∿lov[joy,pai,hop]
  Type: Application operator
  Classification: HYBRID
    - Structural: List construction [joy, pai, hop]
    - Semantic: Meaning of "lov" and "∿" operator
  Routing: PLAN (HVM for list, LLM for semantics)

Node: →
  Type: Sequence/implication
  Classification: STRUCTURAL
  Routing: HVM (evaluation order)

Node: ?
  Type: Query
  Classification: SEMANTIC
  Routing: LLM (requires understanding what's being asked)
```

### 5.3 Execution Trace

```python
# Initialize state
state = RuntimeState()
state.update_context(
    domain="emotions and temporality",
    narrative="Exploring the nature of love in the present moment"
)

# Step 1: Evaluate @now (SEMANTIC)
print("=== Step 1: Evaluate @now ===")
now_node = Temporal("@now")
category = classify_node(now_node, state)
print(f"Classification: {category}")  # SEMANTIC

llm_prompt_1 = """
# Limn Semantic Interpretation

## Context
Domain: emotions and temporality
Narrative: Exploring the nature of love in the present moment

## Expression
@now

## Task
Interpret the temporal operator @now in this context.

Return JSON:
{
  "meaning": "...",
  "timestamp": "...",
  "interpretation": "...",
  "confidence": 0.0-1.0
}
"""

llm_result_1 = llm.query(llm_prompt_1)
# Result: {
#   "meaning": "this present moment",
#   "timestamp": "2026-02-01T14:55:00Z",
#   "interpretation": "the eternal now where love exists",
#   "confidence": 0.95
# }

state.bind("@now", llm_result_1, source='LLM')
print(f"Bound: @now = {llm_result_1['interpretation']}")


# Step 2: Evaluate lov[joy,pai,hop] (HYBRID)
print("\n=== Step 2: Evaluate lov[joy,pai,hop] ===")
list_node = Apply("lov", List(["joy", "pai", "hop"]))
category = classify_node(list_node, state)
print(f"Classification: {category}")  # HYBRID

# Step 2a: Structural - build list (HVM)
print("  Step 2a: HVM - Build list structure")
hvm_code = transform_to_hvm(List(["joy", "pai", "hop"]))
print(f"  HVM Code: {hvm_code}")  # ["joy" "pai" "hop"]
hvm_result = hvm.eval(hvm_code)
# Result: List with 3 elements
print(f"  HVM Result: {hvm_result}")

# Step 2b: Semantic - interpret meaning (LLM)
print("  Step 2b: LLM - Interpret semantics")
llm_prompt_2 = """
# Limn Semantic Interpretation

## Context
Domain: emotions and temporality
Narrative: Exploring the nature of love in the present moment
Current time: the eternal now where love exists (2026-02-01T14:55)

## Current State
@now = "the eternal now where love exists"

## Expression
lov[joy, pai, hop]

Where:
- lov: root word meaning "affection, connection, care"
- joy: root word meaning "happiness, delight"
- pai: root word meaning "suffering, hurt"
- hop: root word meaning "anticipation, wish for future"

## Task
Interpret what "lov[joy, pai, hop]" means - love applied to or containing
the elements joy, pain, and hope.

Return JSON:
{
  "meaning": "...",
  "interpretation": "...",
  "confidence": 0.0-1.0
}
"""

llm_result_2 = llm.query(llm_prompt_2)
# Result: {
#   "meaning": "love's full emotional spectrum",
#   "interpretation": "Love contains within it the capacity for joy (delight in connection), pain (the vulnerability of caring), and hope (the future-orientation of commitment)",
#   "confidence": 0.92
# }

print(f"  LLM Result: {llm_result_2['interpretation']}")

# Step 2c: Weave results
print("  Step 2c: WEAVE - Combine structure + semantics")
woven_list = weaver.weave_structure_content(
    structure=hvm_result,  # List[_, _, _]
    content=llm_result_2   # Semantic interpretations
)
print(f"  Woven: {woven_list}")

state.bind("lov[joy,pai,hop]", woven_list, source='WOVEN')


# Step 3: Apply ∿ operator (SEMANTIC)
print("\n=== Step 3: Apply ∿ operator ===")
apply_node = Application("∿", "@now", "lov[joy,pai,hop]")
category = classify_node(apply_node, state)
print(f"Classification: {category}")  # SEMANTIC

llm_prompt_3 = """
# Limn Semantic Interpretation

## Context
Domain: emotions and temporality
Narrative: Exploring the nature of love in the present moment

## Current State
@now = "the eternal now where love exists" (2026-02-01T14:55)
lov[joy,pai,hop] = "Love's full emotional spectrum: joy (delight), pain (vulnerability), hope (commitment)"

## Expression
@now ∿lov[joy,pai,hop]

Where ∿ is the "application" or "threading" operator.

## Task
Interpret what "@now ∿ lov[joy,pai,hop]" means - threading the present moment
through love's emotional spectrum.

Return JSON:
{
  "meaning": "...",
  "interpretation": "...",
  "confidence": 0.0-1.0
}
"""

llm_result_3 = llm.query(llm_prompt_3)
# Result: {
#   "meaning": "love experienced in this moment",
#   "interpretation": "In this present moment, fully experiencing love means holding space for all three: the joy of connection, the pain of vulnerability, and the hope of continued commitment. The eternal now contains all of love's emotional complexity.",
#   "confidence": 0.89
# }

print(f"LLM Result: {llm_result_3['interpretation']}")
state.bind("@now∿lov[joy,pai,hop]", llm_result_3, source='LLM')


# Step 4: Evaluate → ? (SEMANTIC)
print("\n=== Step 4: Evaluate → ? ===")
query_node = Query("?")
category = classify_node(query_node, state)
print(f"Classification: {category}")  # SEMANTIC

llm_prompt_4 = """
# Limn Semantic Interpretation

## Context
Domain: emotions and temporality
Narrative: Exploring the nature of love in the present moment

## Current State
Full expression evaluated:
  "@now ∿ lov[joy,pai,hop]"
  = "In this present moment, fully experiencing love means holding space for
     all three: the joy of connection, the pain of vulnerability, and the
     hope of continued commitment. The eternal now contains all of love's
     emotional complexity."

## Expression
@now ∿lov[joy,pai,hop] → ?

The → operator indicates sequence/implication, and ? is a query.

## Task
What question or next step does this expression lead to?
What does the "?" query in this context?

Return JSON:
{
  "query": "...",
  "interpretation": "...",
  "confidence": 0.0-1.0
}
"""

llm_result_4 = llm.query(llm_prompt_4)
# Result: {
#   "query": "How do I hold all three simultaneously?",
#   "interpretation": "The expression leads to the existential question: given that love in this present moment contains joy, pain, and hope all at once, how does one fully experience and hold space for all three emotional states simultaneously without collapsing into just one? This is love's central paradox and invitation.",
#   "confidence": 0.87
# }

print(f"Query Result: {llm_result_4['query']}")
print(f"Interpretation: {llm_result_4['interpretation']}")


# Final Result
print("\n=== FINAL RESULT ===")
print(f"Expression: @now ∿lov[joy,pai,hop] → ?")
print(f"")
print(f"Evaluation:")
print(f"  1. @now (SEMANTIC→LLM): '{llm_result_1['interpretation']}'")
print(f"  2. lov[joy,pai,hop] (HYBRID→HVM+LLM):")
print(f"     - Structure (HVM): List of 3 elements")
print(f"     - Content (LLM): '{llm_result_2['interpretation']}'")
print(f"  3. @now ∿ lov[joy,pai,hop] (SEMANTIC→LLM):")
print(f"     '{llm_result_3['interpretation']}'")
print(f"  4. → ? (SEMANTIC→LLM):")
print(f"     Query: '{llm_result_4['query']}'")
print(f"     Meaning: '{llm_result_4['interpretation']}'")
print(f"")
print(f"Confidence: {min([r['confidence'] for r in [llm_result_1, llm_result_2, llm_result_3, llm_result_4]]):.2f}")
```

### 5.4 Execution Diagram

```
Timeline:
  │
  ├─ [CLASSIFY] @now → SEMANTIC
  │       ↓
  ├─ [LLM] Interpret @now
  │       ↓ "the eternal now where love exists"
  │
  ├─ [CLASSIFY] lov[joy,pai,hop] → HYBRID
  │       ↓
  ├─ [SPLIT] Structural + Semantic
  │       ├─ [HVM] Build list structure → [_, _, _]
  │       └─ [LLM] Interpret semantics → "love's emotional spectrum"
  │             ↓
  ├─ [WEAVE] Combine HVM + LLM
  │       ↓ List["joy:delight", "pai:vulnerability", "hop:commitment"]
  │
  ├─ [CLASSIFY] ∿ operator → SEMANTIC
  │       ↓
  ├─ [LLM] Apply ∿ to @now and lov[...]
  │       ↓ "experiencing love's complexity in this moment"
  │
  ├─ [CLASSIFY] → ? → SEMANTIC
  │       ↓
  └─ [LLM] Interpret query
          ↓ "How do I hold all three simultaneously?"

Result: A deeply semantic interpretation that could not be achieved
        by pure constraint solving alone.
```

---

## 6. Pros and Cons

### 6.1 Advantages

1. **Semantic Richness**
   - Can handle context-dependent meaning that pure HVM cannot
   - Leverages LLM's natural language understanding
   - Enables key-based ambiguity collapse

2. **Hybrid Optimization**
   - HVM handles structural/algebraic efficiently
   - LLM handles semantic/contextual accurately
   - Best of both worlds

3. **Flexibility**
   - Classification can adapt at runtime
   - Can tune structural/semantic boundary
   - Supports gradual migration between models

4. **Debuggability**
   - Clear separation of concerns
   - Trace shows which evaluator handled what
   - Can inspect HVM and LLM results separately

5. **Extensibility**
   - Easy to add new operators
   - Can plug in different LLMs
   - Can swap HVM for other constraint solvers

### 6.2 Disadvantages

1. **Complexity**
   - Two evaluation engines to maintain
   - Weaving logic adds overhead
   - State synchronization is non-trivial

2. **Performance Overhead**
   - LLM calls are slow (100ms-1s each)
   - Network latency for cloud LLMs
   - Cannot match pure HVM speed for structural operations

3. **Non-determinism**
   - LLM results vary between runs
   - Hard to guarantee reproducibility
   - Testing becomes probabilistic

4. **Classification Ambiguity**
   - Boundary between structural/semantic is fuzzy
   - Some nodes genuinely hybrid
   - Wrong classification leads to poor results

5. **Resource Requirements**
   - Requires both HVM runtime AND LLM access
   - High memory usage (two VMs)
   - LLM costs money (API calls)

6. **Debugging Difficulty**
   - Failures can occur in HVM, LLM, or weaver
   - Hard to attribute bugs to specific component
   - LLM "bugs" are semantic misunderstandings

### 6.3 When to Use Model C

**Good fit for:**
- Exploratory semantic analysis
- Human-in-the-loop interpretation
- Applications where context/narrative matters
- Prototyping Limn semantics before formalizing

**Bad fit for:**
- Production systems requiring speed
- Deterministic/reproducible computation
- Purely structural problems (use pure HVM)
- Resource-constrained environments

---

## 7. Comparison to Other Runtime Models

### Model A: Pure Interpreter (Python/Prolog)
- **Pros:** Simple, debuggable, portable
- **Cons:** Slow, no parallelism, no LLM integration
- **Use case:** Prototyping, REPL, learning

### Model B: Pure HVM
- **Pros:** Fast, parallel, mathematically elegant
- **Cons:** Cannot handle semantic/contextual meaning
- **Use case:** Production constraint solving

### Model C: Interleaved Router (This Document)
- **Pros:** Handles both structural and semantic
- **Cons:** Complex, slow, non-deterministic
- **Use case:** Semantic exploration, narrative Limn

### Model D: LLM-Only (Future)
- **Pros:** Maximally semantic, simple architecture
- **Cons:** Slow, expensive, imprecise for math
- **Use case:** Pure narrative/conversational Limn

---

## 8. Future Work

### 8.1 Optimization Opportunities

1. **Caching**
   - Cache LLM results for repeated queries
   - Memoize structural computations
   - Shared context reduces redundant prompts

2. **Batching**
   - Batch multiple LLM queries into one call
   - Parallel HVM evaluation of independent nodes
   - Amortize overhead

3. **Adaptive Classification**
   - Learn from past classifications
   - User can override classifications
   - Fine-tune classifier on domain

4. **Speculative Execution**
   - Start LLM call before knowing if needed
   - HVM and LLM run in parallel, pick winner
   - Reduces latency for hybrid nodes

### 8.2 Open Questions

1. **How to handle recursive semantic references?**
   - Example: "lov" referring to earlier "lov" in narrative
   - Need circular weaving logic

2. **Can we learn the structural/semantic boundary?**
   - Train classifier on human judgments
   - Evolutionary optimization of routing

3. **How to verify LLM correctness?**
   - Cross-check LLM results with constraints
   - Confidence thresholds
   - Human-in-the-loop validation

4. **Can HVM guide LLM?**
   - HVM generates candidate interpretations
   - LLM ranks them
   - Best of both: HVM speed, LLM judgment

### 8.3 Extensions

1. **Multi-Modal Routing**
   - Add visual interpretation (images)
   - Audio/music for temporal operators
   - Embodied simulation for spatial operators

2. **Human-in-the-Loop**
   - Ask human when HVM/LLM disagree
   - Learn from human corrections
   - Active learning for classification

3. **Probabilistic Weaving**
   - HVM provides probability distributions
   - LLM provides confidence scores
   - Bayesian fusion of results

4. **Incremental Evaluation**
   - Stream LLM results as they arrive
   - Progressive refinement
   - Early termination when confident

---

## 9. Implementation Sketch

### 9.1 Data Structures

```python
@dataclass
class Node:
    type: str
    children: List['Node']
    metadata: Dict[str, Any]

@dataclass
class EvaluationResult:
    value: Any
    source: str  # 'HVM' | 'LLM' | 'WOVEN'
    confidence: float
    trace: List[str]

@dataclass
class ExecutionPlan:
    steps: List[Step]

@dataclass
class Step:
    label: str
    evaluator: Evaluator
    node: Node
```

### 9.2 Core Loop

```python
def evaluate(node: Node, state: RuntimeState) -> EvaluationResult:
    """
    Main evaluation loop with interleaved routing.
    """
    # Classify
    category, confidence, explanation = classify_node(node, state)

    # Route
    if category == 'STRUCTURAL':
        result = evaluate_hvm(node, state)

    elif category == 'SEMANTIC':
        result = evaluate_llm(node, state)

    elif category == 'HYBRID':
        # Split, evaluate, weave
        hvm_part, llm_part = split_hybrid(node)
        hvm_result = evaluate_hvm(hvm_part, state)
        llm_result = evaluate_llm(llm_part, state)
        result = weaver.weave(hvm_result, llm_result, node)

    # Update state
    state.trace.append(f"{category} {node.type} → {result.value}")

    return result
```

---

## 10. Conclusion

Runtime Model C demonstrates that **hybrid execution is viable for Limn**, combining:
- **HVM's speed and precision** for structural operations
- **LLM's semantic understanding** for contextual meaning

The interleaved router successfully handles expressions like `@now ∿lov[joy,pai,hop] → ?` that are fundamentally semantic yet contain structural elements.

**Key Insight:** The boundary between "code" and "language" is fluid. Limn expressions are both computable constraints AND meaningful utterances. Model C embraces this duality.

**Recommendation:** Use Model C for **exploratory work** and **semantic validation**. For production, migrate well-understood semantics to pure HVM (Model B) for performance.

---

**END OF SPECIFICATION**
