# Pure HVM2 Oracle Architecture

**Strategy A Implementation: Pure text→text oracle system**

## Architecture

```
┌──────────────────────────────────────┐
│         Bend/HVM2 (Pure)             │
│  • Generates oracle requests (text)  │
│  • No side effects                   │
│  • Deterministic computation         │
└────────────┬─────────────────────────┘
             │ Oracle requests (JSON)
             ▼
┌──────────────────────────────────────┐
│      Python Harness (Impure)         │
│  • Executes oracle requests          │
│  • ALL side effects happen here:     │
│    - LLM API calls                   │
│    - File I/O                        │
│    - Database queries                │
│    - Network requests                │
│    - Temporal/time operations        │
└────────────┬─────────────────────────┘
             │ Oracle responses (JSON)
             ▼
┌──────────────────────────────────────┐
│         Bend/HVM2 (Pure)             │
│  • Continues reduction                │
│  • Incorporates responses            │
│  • Emits more requests or output     │
└──────────────────────────────────────┘
```

## Universal Oracle Pattern

All oracles follow the same text-based protocol:

**Request format (JSON):**
```json
{
  "oracle": "arithmetic|filesystem|temporal|llm",
  "operation": "<operation-name>",
  "args": {...}
}
```

**Response format (JSON):**
```json
{
  "success": true|false,
  "result": <value>,
  "error": "<error-message>"
}
```

## Oracle Types

### 1. Arithmetic Oracle
Pure computation that could be done in Bend, but demonstrates the pattern.

**Operations:**
- `add`, `sub`, `mul`, `div`
- `pow`, `mod`
- `sqrt`, `abs`

**Example request:**
```json
{"oracle": "arithmetic", "operation": "add", "args": {"a": 5, "b": 3}}
```

**Example response:**
```json
{"success": true, "result": 8}
```

### 2. Filesystem Oracle
File operations (read, write, list, exists).

**Operations:**
- `read` - Read file contents
- `write` - Write file contents
- `list` - List directory
- `exists` - Check if path exists

**Example request:**
```json
{"oracle": "filesystem", "operation": "read", "args": {"path": "data/facts.txt"}}
```

**Example response:**
```json
{"success": true, "result": "file contents..."}
```

### 3. Temporal Oracle
Time-based operations (now, was, will).

**Operations:**
- `now` - Current timestamp
- `was` - Query past state (retrieve from history)
- `will` - Project future state (constraint over possibilities)

**Example request:**
```json
{"oracle": "temporal", "operation": "now", "args": {}}
```

**Example response:**
```json
{"success": true, "result": {"timestamp": 1738454700, "iso": "2026-02-01T17:15:00Z"}}
```

### 4. LLM Oracle
Language model inference.

**Operations:**
- `complete` - Text completion
- `parse` - Extract structured data
- `generate` - Generate text from template

**Example request:**
```json
{
  "oracle": "llm",
  "operation": "complete",
  "args": {
    "prompt": "The meaning of life is",
    "max_tokens": 100
  }
}
```

**Example response:**
```json
{"success": true, "result": "a question that each person must answer for themselves."}
```

## Implementation Status

- [x] Architecture design
- [x] README documentation
- [ ] Bend oracle request types
- [ ] Python harness core
- [ ] Arithmetic oracle implementation
- [ ] Filesystem oracle implementation
- [ ] Temporal oracle implementation
- [ ] LLM oracle implementation
- [ ] Example programs demonstrating each oracle
- [ ] Integration test suite

## Why This Design?

**Pure HVM2 code:**
- Deterministic (testable, reproducible)
- Can be optimized by HVM's optimal reduction
- No dependency on external systems
- Easy to reason about

**Python harness handles impurity:**
- Side effects isolated and explicit
- Easy to mock for testing
- Can swap implementations (local LLM vs API)
- Clear boundary between logic and effects

**Text-based protocol:**
- Language-agnostic
- Easy to log and debug
- Can be inspected/modified by humans
- Enables caching and replay

## Relationship to Model B

This is **Strategy A** - an alternative to the Model B (Prolog-Oracle) approach.

**Model B (Prolog):**
- Prolog interpreter with `~` oracle operator
- LLM called at `~` nodes
- Deterministic + subjective oracle

**Strategy A (HVM2):**
- Pure HVM2 with text-based oracle protocol
- Python harness executes ALL oracles (not just LLM)
- Demonstrates universal oracle pattern

Both prove the same concept: **objective computation + subjective oracle = hybrid intelligence**.

## Usage

```bash
# Run example program
python harness.py examples/arithmetic_demo.bend

# Run with LLM oracle
python harness.py --llm-api=anthropic examples/llm_demo.bend

# Run with mock oracles (for testing)
python harness.py --mock examples/test_all_oracles.bend
```

## Files

- `README.md` - This file
- `oracles.bend` - Bend types for oracle requests/responses
- `harness.py` - Python oracle execution harness
- `oracles/` - Individual oracle implementations
  - `arithmetic.py`
  - `filesystem.py`
  - `temporal.py`
  - `llm.py`
- `examples/` - Demonstration programs
  - `arithmetic_demo.bend`
  - `filesystem_demo.bend`
  - `temporal_demo.bend`
  - `llm_demo.bend`
  - `hybrid_demo.bend` - Uses multiple oracles

---

```limn
pur tex | orc exe | sys cle
> pure text | oracles execute | system clean
```

**The boundary between pure and impure is explicit, observable, and beautiful.**
