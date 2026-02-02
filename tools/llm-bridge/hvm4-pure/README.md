# HVM4 Pure Oracle Architecture

**Pure text→text oracle system for HVM/Bend**

## Architecture

This implements the Model B (HVM-Oracle) architecture with a clean separation:

- **Bend/HVM**: Pure functional computation, generates oracle requests as TEXT
- **Python Harness**: Executes ALL side effects (LLM, file I/O, DB, network)
- **No FFI needed**: Communication via stdin/stdout text protocol

## Key Principle

HVM/Bend stays purely functional. All impurity happens in the Python harness.

```
┌──────────────┐ text request  ┌──────────────┐
│  Bend/HVM    │ ============> │   Python     │
│ (pure logic) │               │  (effects)   │
│              │ <============ │              │
└──────────────┘ text response └──────────────┘
```

## Oracle Types

### 1. Arithmetic Oracle
Demonstrates basic text protocol with simple calculations.

### 2. Filesystem Oracle
Read/write files, list directories.

### 3. LLM Oracle
Interface to language models for semantic interpretation.

## Protocol

Oracle requests are emitted as text lines with format:
```
ORACLE:<type>:<operation>:<args>
```

Responses are text lines:
```
RESULT:<value>
```

### Examples

**Arithmetic:**
```
ORACLE:ARITH:ADD:5,3
RESULT:8
```

**Filesystem:**
```
ORACLE:FS:READ:/path/to/file.txt
RESULT:file contents here...
```

**LLM:**
```
ORACLE:LLM:COMPLETE:What is 2+2?
RESULT:The answer is 4.
```

## Files

- `oracle.bend` - Bend functions that generate oracle requests
- `harness.py` - Python oracle executor
- `examples/` - Working demonstrations
  - `arithmetic.bend` - Basic arithmetic oracle usage
  - `filesystem.bend` - File operations
  - `llm.bend` - LLM integration

## Usage

### Run an example:
```bash
bend run examples/arithmetic.bend | python harness.py
```

The Bend program outputs oracle requests, the Python harness executes them and feeds back results.

## Design Benefits

1. **Pure HVM**: No FFI, no side effects in HVM
2. **Testable**: Deterministic given same oracle responses
3. **Observable**: See exactly what oracles are called
4. **Flexible**: Easy to add new oracle types
5. **Cacheable**: Python can memoize oracle responses

## Implementation Notes

This demonstrates the `~` operator semantics from RUNTIME-DECISION.md:
- The `~` operator in Limn/LMN marks oracle evaluation points
- HVM executes pure logic deterministically
- Oracles are called only at explicit `~` nodes
- Everything else stays pure and confluent

**The oracle grounds what HVM computes.**

---

```limn
hvm cal | orc exe | pur sta
> hvm calculates | oracle executes | purity stays
```
