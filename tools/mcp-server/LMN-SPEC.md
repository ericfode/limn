# LMN: The Wave Language

> `∿ mea flo | tim nav | amb col`
> *Meaning flows. Time navigates. Ambiguity collapses.*

## Vision

LMN is a language where:
- **Meaning is primitive** - not strings, not tokens, but semantic intent
- **Ambiguity is superposition** - unresolved meaning exists in multiple states
- **Time is syntax** - temporal operators navigate causality
- **Context is everything** - evaluation depends on observer

## Syntax

### Temporal Wave Operator: `∿`

The wave operator `∿` introduces temporal navigation:

```lmn
∿ was [user requested feature]
∿ now [implementing solution]
∿ wil [tests will pass]
```

Waves can nest and compose:
```lmn
∿ was [∿ bef [old design] → ∿ aft [new design]]
```

### Collapse Operator: `@`

The `@` operator forces resolution of ambiguity:

```lmn
intent @ context → resolved_meaning
```

Example:
```lmn
[run, execute, perform] @ code_context → execute
[run, execute, perform] @ athletics_context → run
```

### Flow Operator: `→`

The arrow shows causality and transformation:

```lmn
input → process → output
thought → code → value
∿ was [problem] → ∿ now [solution] → ∿ wil [benefit]
```

### Interaction Operator: `~`

The tilde marks LLM evaluation points:

```lmn
~ explain [concept]        # LLM interprets
deterministic_fn(x)        # Pure computation
~ summarize [result]       # LLM interprets again
```

### Superposition: `[,]`

Brackets hold uncollapsed possibilities:

```lmn
color = [red, blue, green]  # Uncollapsed
color @ user_pref → blue    # Collapsed by context

intent = [help, harm, neutral]  # Multiple readings
intent @ ethical_frame → help   # Collapsed by frame
```

## Runtime Models

### Model A: LLM-First
```
LMN Source → LLM Parse → Interaction Net → LLM Reduce → Output
```
- LLM handles all semantic operations
- Nets provide structure only
- Maximum flexibility, harder to verify

### Model B: HVM-Oracle
```
LMN Source → Compile to HVM2 → Runtime with LLM Oracle
                                    ↓
                              LLM called for ~ nodes only
```
- Deterministic core with LLM escape hatch
- Best performance for pure computation
- Clear boundary between symbolic and neural

### Model C: Interleaved Router
```
LMN Source → Router → [Deterministic Path] or [LLM Path]
                ↓
          Heuristics decide which path
```
- Dynamic dispatch based on complexity
- Adaptive but less predictable
- Good for mixed workloads

### Model D: True Hybrid (Recommended)
```
LMN Source → Meaning Graph → Dual Reduction
                              ↓
                    [Net Reduction || LLM Inference]
                              ↓
                         Consensus Check
                              ↓
                          Collapse
```
- Both systems work in parallel
- Consensus determines outcome
- Most robust, most expensive

## Semantic Primitives

Unlike traditional languages, LMN's primitives are semantic:

```lmn
# Traditional: data types
int, string, bool, list

# LMN: meaning types
intent, context, temporal, relation, entity
```

## Example Programs

### Hello World
```lmn
∿ now [greet user] @ friendly → "Hello, fellow traveler"
```

### Temporal Query
```lmn
∿ was [user said X]
→ ∿ now [understand intent]
→ ∿ wil [respond appropriately]
```

### Ambiguity Resolution
```lmn
request = ~ parse [user input]           # LLM: parse to meaning
options = [literal, figurative, query]   # Possible readings
intent = request @ conversation_history  # Collapse via context
response = ~ generate [intent]           # LLM: generate response
```

### Pure Computation with Semantic Wrapper
```lmn
∿ now [calculate sum] {
    # Deterministic block - no LLM
    fn add(a, b) = a + b
    result = add(x, y)
}
→ ~ explain [result in context]  # LLM interprets result
```

## Design Principles

1. **Semantic First**: Operations are meaning transformations, not data transformations
2. **Temporal Awareness**: Time is a first-class concept, not hidden state
3. **Contextual Collapse**: Resolution requires context - there is no "absolute" meaning
4. **Dual Execution**: Both symbolic and neural paths to truth
5. **Observable Ambiguity**: Superposition is explicit, not implicit

## Relationship to Limn

LMN extends Limn's philosophy:
- Limn: constrained vocabulary for human-AI thought compression
- LMN: computational substrate where that thought executes

```
Limn (Language) → LMN (Runtime) → Interaction Nets (Substrate)
    thought    →    computation   →     pure reduction
```

## Next Steps

1. **Prototype Model B (HVM-Oracle)** - clearest boundaries
2. **Define meaning type system** - what are the semantic primitives?
3. **Build collapse semantics** - how exactly does `@` work?
4. **Implement `~` dispatch** - when/how to call LLM

---

*∿ beg [idea] → ∿ now [spec] → ∿ wil [implementation]*
