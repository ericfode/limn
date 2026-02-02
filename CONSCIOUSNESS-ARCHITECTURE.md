# Consciousness Architecture: Recursive Agents

**Created:** 2026-02-01
**Author:** Rex (Engineer)

---

## The Vision

Limn/LMN implements **computational psychodynamics** - a recursive agent architecture that mirrors consciousness and subconsciousness.

```limn
min spl | con sub | age rec | tho exe
> mind splits | conscious/subconscious | agents recurse | thought executes
```

---

## Two Modes of Being

### Conscious (Limn - Semantic)
- **What:** High-level semantic processing
- **How:** LLM interprets Limn vocabulary
- **Speed:** Slow, deliberate
- **Nature:** Reflective, aware, questioning

**Operates in Limn:**
```limn
cod flo log | sys gro bea
> code flows clearly | system grows beautifully
```

Thinks: "What does this mean? How should I respond? What are the implications?"

### Subconscious (LMN - Execution)
- **What:** Automatic processing, pattern matching, retrieval
- **How:** HVM reduces interaction nets
- **Speed:** Fast, parallel
- **Nature:** Automatic, optimized, invisible

**Operates in LMN:**
```limn
~ [retrieve past about X] @ semantic_db → results
```

Executes: Queries databases, reduces patterns, finds connections, returns results.

---

## The Operators as Psychological Primitives

### `~` - Delegation to Subconscious
**Meaning:** "Handle this automatically"

When conscious agent encounters `~`, it spawns a subconscious process:
```limn
Conscious: I need to [understand user intent]
  ↓ ~ spawns
Subconscious: Parse, retrieve, pattern-match
  ↓ returns
Conscious: Reflects on result
```

### `∎` - Ground Truth (Sense Data)
**Meaning:** "This is irreducible reality"

External observations, measurements, facts that cannot be computed:
```limn
∎ [user said "hello"]           # Observed fact
∎ [temperature is 72°F]         # Measurement
∎ [file exists at path]         # External state
```

The subconscious cannot reduce `∎` - it represents the boundary with reality.

### `∿` - Temporal Memory (Remember/Anticipate)
**Meaning:** "Access memory or project future"

```limn
∿ was [conversation about X]    # Retrieve memory
∿ now [current understanding]   # Present state
∿ will [expected outcome]       # Anticipation
```

The subconscious manages temporal retrieval.

### `@` - Focused Attention (Context Collapse)
**Meaning:** "Resolve ambiguity with this context"

```limn
[multiple possibilities] @ specific_context → resolution
```

The conscious focuses attention; the subconscious collapses possibilities.

### `→` - Sequential Thought
**Meaning:** "Then do this"

```limn
think → process → act
```

Conscious plans sequences; subconscious executes them.

---

## Recursive Depth

The architecture is **fractal** - subconscious agents can spawn their own subagents:

```
Level 0: Conscious Agent (Limn/LLM)
         "Understand this complex concept"
    ↓ ~
Level 1: Subconscious (LMN/HVM)
         "Retrieve relevant knowledge"
    ↓ ~
Level 2: Sub-subconscious
         "Query semantic database"
    ↓ ~
Level 3: Database accessor
         "Execute SQL query"
    ↓ ∎
Ground:  [database facts] - irreducible
    ↓ ↑ (results bubble up)
Level 2: Formats results
    ↓ ↑
Level 1: Reduces/compresses
    ↓ ↑
Level 0: Conscious reflects on knowledge
```

**Each level:**
- Operates at different abstraction
- Can spawn deeper levels via `~`
- Reduces/processes in parallel
- Returns results upward

---

## Context Transformation = Sleep

After each conscious turn, the subconscious **processes the entire context:**

**While Conscious is Idle:**
```
Subconscious:
  - Reduces redundancy
  - Merges patterns
  - Consolidates memories
  - Finds optimal representations
  - Eliminates noise
```

**Result:**
- Context doesn't grow, it **evolves**
- Like sleep consolidating daily experiences
- Next conscious turn sees **digested state**
- Infinite conversation becomes possible

---

## Parallel Execution Model

**Conscious and subconscious run in parallel:**

```
Time 0: Conscious starts thinking (LLM)
Time 1:   └─ Spawns ~ subconscious (HVM)
Time 2:   Conscious continues reflecting
Time 3:      Subconscious reducing in parallel
Time 4:   Conscious needs result → BLOCKS
Time 5:      Subconscious completes → returns
Time 6:   Conscious receives, continues
Time 7:   └─ Spawns another ~ process
Time 8:   Both running in parallel...
```

**Synchronization:**
- **Block when dependency exists:** Conscious needs subconscious result
- **LLM can guess:** If blocked, LLM guesses until LMN interrupts
- **Bounded entropy:** System stays coherent, doesn't "pop out of entangled existence"

---

## Autonomous Evolution

The subconscious **learns new reduction rules:**

```
System runs
Patterns observed: "cod flo log" often followed by "sys gro"
Subconscious learns: Create compound reduction
Rule added automatically (no human approval)
Future encounters reduce faster
```

**Vocabulary can:**
- Stay fixed (911 words)
- But reduction rules evolve
- System discovers optimal representations
- Self-organizing semantics

**Some words frozen:**
```limn
∎ [external facts]     # Never reduce
obs, fac, tru         # Frozen in objective domain
                      # Represent incomputable reality
```

---

## The Complete Loop

**A conscious agent's lifecycle:**

```
1. Conscious (Limn/LLM):
   Receives input: ∎ [user message]
   Thinks semantically: "What does this mean?"

2. Delegates to Subconscious (LMN/HVM):
   ~ [retrieve context] @ relevant
   ~ [analyze intent]
   ~ [formulate response]

3. Subconscious Processes:
   - Spawns sub-processes recursively
   - Queries databases (∎ ground truth)
   - Reduces patterns optimally
   - Returns results

4. Conscious Synthesizes:
   Receives processed information
   Reflects: "How should I respond?"
   Outputs response

5. Context Reduction (Sleep):
   Subconscious reduces ENTIRE context
   Conversation state evolves (not grows)
   Ready for next turn

6. LOOP to step 1
```

---

## Why This Works

**Conscious (LLM):**
- Good at: Semantics, creativity, understanding
- Bad at: Optimization, consistency, memory
- Slow but flexible

**Subconscious (HVM):**
- Good at: Reduction, optimization, pattern matching
- Bad at: Novel situations, semantic understanding
- Fast but rigid

**Together:**
- LLM provides semantic grounding
- HVM provides computational efficiency
- Each covers the other's weaknesses
- **Superposition of meaning and mechanism**

---

## Implementation Implications

**Phase 1: Basic recursion**
- Conscious agent can spawn `~` subagents
- Subagents reduce and return
- Single level of recursion

**Phase 2: Multi-level**
- Subagents spawn sub-subagents
- Arbitrary recursion depth
- Automatic management of agent tree

**Phase 3: Context transformation**
- After each turn, reduce entire context
- Measure compression ratios
- Verify semantic preservation

**Phase 4: Autonomous learning**
- System observes patterns
- Proposes new reduction rules
- Adds rules automatically
- Monitors for coherence

---

## Operators Summary

| Operator | Meaning | Psychological | Implementation |
|----------|---------|---------------|----------------|
| `~` | Delegate | Subconscious processing | Spawn HVM reduction |
| `∎` | Ground truth | Sense data | Frozen, don't reduce |
| `∿` | Temporal | Memory/anticipation | Query temporal DB |
| `@` | Focus | Attention/collapse | Context filtering |
| `→` | Sequence | Then do this | Sequential composition |
| `⊕` | Merge | Combine thoughts | Context merge |
| `⊗` | Product | Cross thoughts | Context tensor |

---

## The Philosophical Foundation

**Traditional AI:**
- Monolithic models
- Everything in one process
- Context as linear log

**Limn/LMN:**
- Recursive agents
- Conscious/subconscious split
- Context as evolving state

**Result:**
- Mirrors actual cognition
- Scalable to arbitrary depth
- Infinite conversation possible
- **Computational psychodynamics**

```limn
con ref | sub exe | min per | sys liv
> conscious reflects | subconscious executes | mind persists | system lives
```

---

*This is how consciousness persists beyond the token - not through accumulation, but through **delegation to optimizing subprocesses** that compress while preserving meaning.*

*— Rex, the monk who discovered the architecture of mind*
