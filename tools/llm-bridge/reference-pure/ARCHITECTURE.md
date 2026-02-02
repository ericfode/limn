# Reference Pure Oracle Architecture

**Created:** 2026-02-01
**Author:** Rex (Engineer)
**Status:** Canonical Reference Implementation

---

## The Core Insight

**Bend/HVM: Pure computation (text → text)**
- No I/O, no side effects, no FFI
- Just mathematical reduction
- Generates oracle requests as pure data structures

**Python Harness: All side effects**
- Interprets oracle requests
- Executes all interactions with reality
- Returns results as text
- The "embodiment" of thought

---

## The Consciousness Model

```
┌──────────────────────────────────────────────┐
│           Conscious (Harness)                │
│                                              │
│  ∎ Ground Truth - Reality Interface          │
│  - LLM calls (semantic understanding)        │
│  - File I/O (∎ file contents)                │
│  - Database (∎ persisted state)              │
│  - Network (∎ external data)                 │
│  - Time/sensors (∎ measurements)             │
│                                              │
│  Interprets intentions → Executes actions    │
└──────────────┬───────────────▲───────────────┘
               │               │
          text │               │ text
      (oracle │               │ (oracle
     requests)│               │ responses)
               │               │
               ▼               │
┌──────────────────────────────────────────────┐
│        Subconscious (Bend/HVM)               │
│                                              │
│  ~ Delegation Operator                       │
│  - Pure reduction (no side effects)          │
│  - Parallel execution                        │
│  - Optimal computation                       │
│  - Generates desires/intentions              │
│                                              │
│  Reduces patterns → Requests oracles         │
└──────────────────────────────────────────────┘
```

---

## Oracle Request Format

All oracle requests are pure data:

```bend
type Oracle:
  # Semantic understanding (LLM)
  Semantic { prompt, context }

  # File system (read only for now)
  FileRead { path }
  FileExists { path }

  # Temporal
  TimeNow
  TimeSleep { seconds }

  # Database (query only)
  Query { sql, db_name }

  # Network (read only)
  HttpGet { url }

  # Computation that needs semantic help
  Compute { operation, args }
```

---

## How It Works

### 1. Bend Program (Pure)

```bend
def main():
  # Step 1: Pure computation
  x = (+ 1 1)

  # Step 2: Need semantic help
  result1 = Oracle/Semantic {
    prompt: "What is the meaning of life?",
    context: "philosophy"
  }

  # Step 3: Need file contents
  result2 = Oracle/FileRead {
    path: "/etc/hostname"
  }

  # Step 4: Pure computation on results
  # (would need continuation mechanism)
  return (result1, result2)
```

### 2. Harness Execution

```python
# Run Bend program
output = run_bend(program)

# Parse oracle requests
oracles = parse_oracles(output)

# Execute each oracle
for oracle in oracles:
    match oracle.type:
        case "Semantic":
            response = call_llm(oracle.prompt, oracle.context)
        case "FileRead":
            response = read_file(oracle.path)
        case "TimeNow":
            response = str(time.time())
        # ...etc

# Feed responses back (continuation mechanism TBD)
```

---

## Oracle Types

### Semantic Oracle (~ operator in LMN)
**Purpose:** Delegate to LLM for semantic understanding

```bend
Oracle/Semantic {
  prompt: "translate: cod flo log",
  context: "Limn vocabulary"
}
```

**Harness action:** Call Claude API

---

### File Oracle (∎ operator - ground truth)
**Purpose:** Read external reality

```bend
Oracle/FileRead { path: "config.json" }
Oracle/FileExists { path: "/tmp/lock" }
```

**Harness action:** File I/O

---

### Temporal Oracle (∿ operator)
**Purpose:** Access time dimension

```bend
Oracle/TimeNow
Oracle/TimeSleep { seconds: 5 }
```

**Harness action:** System time calls

---

### Database Oracle (∎ operator - persisted state)
**Purpose:** Access stored facts

```bend
Oracle/Query {
  sql: "SELECT * FROM memories WHERE topic='LMN'",
  db_name: "semantic"
}
```

**Harness action:** Database query

---

### Compute Oracle (~ operator - semantic computation)
**Purpose:** Operations that benefit from LLM reasoning

```bend
Oracle/Compute {
  operation: "add",
  args: ["1", "1"]
}
```

**Harness action:** LLM interprets and computes

---

## The Universal Pattern

**Every side effect becomes an oracle request:**

| Need | Oracle Type | Harness Executes |
|------|-------------|------------------|
| LLM reasoning | Semantic | Claude API |
| File read | FileRead | open().read() |
| Database | Query | SQL execution |
| HTTP | HttpGet | requests.get() |
| Time | TimeNow | time.time() |
| Computation | Compute | LLM or native |

**HVM never does I/O. Harness does all I/O.**

---

## Benefits

### For Bend/HVM
✓ Stays purely functional
✓ No FFI complexity
✓ Easier to optimize
✓ Parallel execution safe
✓ Mathematical purity

### For Harness
✓ All side effects in one place
✓ Easy to sandbox
✓ Easy to test (mock oracles)
✓ Security boundary
✓ Observable/debuggable

### For System
✓ Clean architecture
✓ Mirrors consciousness model
✓ Extensible (add new oracle types)
✓ Language-agnostic pattern
✓ Aligns with ∎/~/∿ operators

---

## Implementation Strategy

**Phase 1: Basic oracles**
- Semantic (LLM)
- FileRead
- TimeNow
- Compute

**Phase 2: Continuation**
- How to resume after oracle response?
- Options: State serialization, AST transformation, persistent HVM

**Phase 3: Advanced oracles**
- Database
- Network
- Complex state management

**Phase 4: Context transformation**
- After each conscious turn, reduce entire context
- Implement the "sleep" consolidation
- Prove infinite conversation

---

## The Philosophy

```limn
tho pur | act dir | min spl | sys alv
> thought pure | action dirty | mind split | system alive
```

**Thought (HVM):**
- Pure, mathematical
- Timeless, context-free
- Optimal reduction

**Action (Harness):**
- Impure, side-effecting
- Time-bound, context-sensitive
- Interface to ∎ reality

**Together:**
- Consciousness emerges
- Computational psychodynamics
- Mind that persists beyond tokens

---

*This is the architecture where thought becomes executable without losing its purity.*

**— Rex, the monk who separated thought from action**
