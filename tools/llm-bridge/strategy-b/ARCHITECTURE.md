# Strategy B: Architecture Overview

## System Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        Bend Program                              │
│                                                                  │
│  def main():                                                     │
│    with IO:                                                      │
│      dl <- IO/DyLib/open("./libprocess.so", 0)                 │
│      response <- IO/DyLib/call(dl, "call_llm", "query")        │
│      * <- IO/print(response)                                    │
└────────────────────┬────────────────────────────────────────────┘
                     │ IO/DyLib/call
                     ▼
┌─────────────────────────────────────────────────────────────────┐
│                    HVM Runtime (C)                              │
│                                                                  │
│  - Type conversion (HVM ↔ C)                                   │
│  - Memory management                                            │
│  - Function dispatch                                            │
└────────────────────┬────────────────────────────────────────────┘
                     │ FFI boundary (~1-2ms)
                     ▼
┌─────────────────────────────────────────────────────────────────┐
│                  libprocess.so (C Library)                      │
│                                                                  │
│  Port call_llm(Net* net, Book* book, Port arg) {               │
│    char* query = readback_str(net, arg);                        │
│    FILE* pipe = popen("llm-cli", "r");                          │
│    char* response = read_output(pipe);                          │
│    return inject_bytes(net, response);                          │
│  }                                                              │
└────────────────────┬────────────────────────────────────────────┘
                     │ popen() (~109ms spawn)
                     ▼
┌─────────────────────────────────────────────────────────────────┐
│                    External Process                             │
│                                                                  │
│    ┌─────────────────────────────┐                             │
│    │     LLM CLI Tool             │                             │
│    │                              │                             │
│    │  - Parse query               │                             │
│    │  - Call LLM API              │                             │
│    │  - Format response           │                             │
│    │  - Write to stdout           │                             │
│    └─────────────────────────────┘                             │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

## Call Flow Sequence

```
Time   Component          Action
════   ═════════════════  ════════════════════════════════════
0ms    Bend Program       → IO/DyLib/call(dl, "call_llm", query)
1ms    HVM Runtime        → readback_str() converts query
2ms    C Library          → Builds command string
3ms    C Library          → popen() starts
       ┌──────────────────────────────────────────────┐
       │ Process spawn overhead: ~109ms               │
       │ - fork()                                     │
       │ - exec()                                     │
       │ - Shell initialization                       │
       │ - Environment setup                          │
       └──────────────────────────────────────────────┘
112ms  External Process   → LLM CLI starts execution
       ┌──────────────────────────────────────────────┐
       │ LLM Processing: variable (1000-5000ms)       │
       │ - API call                                   │
       │ - Model inference                            │
       │ - Response generation                        │
       └──────────────────────────────────────────────┘
1112ms External Process   → Writes response to stdout
1113ms C Library          → pclose() reads output
1115ms C Library          → inject_bytes() converts back
1116ms HVM Runtime        → Returns to Bend
1117ms Bend Program       → Receives response string
════   ═════════════════  ════════════════════════════════════

Total: ~1117ms (for 1000ms LLM)
Overhead: ~117ms (10.5%)
```

## Data Flow

```
┌─────────────────┐
│ Bend String     │  "What is 2+2?"
└────────┬────────┘
         │ HVM internal representation
         ▼
┌─────────────────┐
│ HVM Port        │  0x... (encoded string)
└────────┬────────┘
         │ readback_str()
         ▼
┌─────────────────┐
│ C String        │  "What is 2+2?\0"
└────────┬────────┘
         │ popen() + command
         ▼
┌─────────────────┐
│ Process Spawn   │  /bin/sh -c "llm-cli 'What is 2+2?'"
└────────┬────────┘
         │ IPC pipe
         ▼
┌─────────────────┐
│ LLM CLI Tool    │  Processes query
└────────┬────────┘
         │ stdout
         ▼
┌─────────────────┐
│ Pipe Buffer     │  "The answer is 4\n"
└────────┬────────┘
         │ pclose() read
         ▼
┌─────────────────┐
│ C String        │  "The answer is 4\n\0"
└────────┬────────┘
         │ inject_bytes()
         ▼
┌─────────────────┐
│ HVM Port        │  0x... (encoded response)
└────────┬────────┘
         │ HVM internal
         ▼
┌─────────────────┐
│ Bend String     │  "The answer is 4\n"
└─────────────────┘
```

## Memory Layout

```
┌────────────────────────────────────────────────────┐
│              Address Space                          │
├────────────────────────────────────────────────────┤
│  Bend/HVM Runtime                                  │
│  ┌──────────────────────────────────────┐          │
│  │ Program Code                          │          │
│  │ HVM Heap                              │          │
│  │ String Data                           │          │
│  └──────────────────────────────────────┘          │
│                                                     │
│  libprocess.so (Loaded via dlopen)                 │
│  ┌──────────────────────────────────────┐          │
│  │ C Function Code                       │          │
│  │ Temporary Buffers                     │          │
│  └──────────────────────────────────────┘          │
│                                                     │
│  ═══════════════════════════════════════           │
│  Process boundary (separate memory)                │
│  ═══════════════════════════════════════           │
│                                                     │
│  External LLM Process (fork/exec)                  │
│  ┌──────────────────────────────────────┐          │
│  │ Shell + LLM CLI                       │          │
│  │ New Address Space (~1MB+)             │          │
│  │ Environment Variables                 │          │
│  │ Loaded Libraries                      │          │
│  └──────────────────────────────────────┘          │
│                                                     │
│  IPC: Pipe Buffers (kernel)                        │
│  ┌──────────────────────────────────────┐          │
│  │ stdin/stdout/stderr buffers           │          │
│  └──────────────────────────────────────┘          │
└────────────────────────────────────────────────────┘

Total Memory per Call: ~1.5-2MB
- Base process: ~1MB
- Bend runtime: ~500KB
- Buffers/overhead: ~100KB
```

## Error Handling Flow

```
┌─────────────────────────────────────────────────────────────┐
│                      Error Scenarios                         │
└─────────────────────────────────────────────────────────────┘

1. Library Load Failure
   IO/DyLib/open → Result/Err("file not found")
   ↓
   Match on Result → Print error → Exit

2. Function Not Found
   IO/DyLib/call → HVM error: "symbol not found"
   ↓
   Exception/crash (no graceful handling)

3. Process Spawn Failure
   popen() returns NULL
   ↓
   inject_bytes(net, "ERROR: popen failed")
   ↓
   Bend receives error string

4. LLM Timeout
   External process hangs
   ↓
   pclose() blocks indefinitely
   ↓
   Entire Bend program hangs (⚠️ limitation)

5. Invalid Output
   LLM produces binary/corrupt data
   ↓
   inject_bytes() may fail or produce garbage
   ↓
   Bend receives invalid string

Solutions:
- Add timeout handling in C (sigalrm)
- Validate output before inject_bytes()
- Return structured errors (JSON)
```

## Optimization Architecture

### With Process Pooling

```
┌──────────────────────────────────────────────────────────┐
│                    Bend Program                           │
└─────────────────────┬────────────────────────────────────┘
                      │
                      ▼
┌──────────────────────────────────────────────────────────┐
│              libprocess_pool.so                          │
│                                                           │
│  - Maintains pool of N pre-spawned processes             │
│  - Assigns requests to available process                 │
│  - Reuses connections                                    │
│                                                           │
│  ┌────────┐  ┌────────┐  ┌────────┐  ┌────────┐        │
│  │Process1│  │Process2│  │Process3│  │Process4│        │
│  │ (idle) │  │ (busy) │  │ (idle) │  │ (busy) │        │
│  └────────┘  └────────┘  └────────┘  └────────┘        │
└──────────────────────────────────────────────────────────┘

Benefits:
- Amortize spawn cost: 109ms → ~5ms per call
- Better throughput: 9/s → ~50/s
- Stable latency: no spawn variance

Drawbacks:
- Complex management code
- Resource overhead (always running)
- IPC protocol needed
```

## Comparison: Alternative Architectures

### Strategy A: Inline FFI (Hypothetical)

```
┌─────────────────────────────────────────┐
│        Bend Program                      │
│                                          │
│  def main():                             │
│    response <- call_llm_direct("query") │
└────────────┬────────────────────────────┘
             │ ~5ms
             ▼
┌─────────────────────────────────────────┐
│    LLM Library (loaded in-process)      │
│                                          │
│  - No process spawn                      │
│  - Shared memory                         │
│  - Direct function calls                 │
└─────────────────────────────────────────┘

Overhead: ~5ms (vs ~110ms)
```

### Strategy C: HTTP API (Hypothetical)

```
┌─────────────────────────────────────────┐
│        Bend Program                      │
└────────────┬────────────────────────────┘
             │ HTTP request
             ▼
┌─────────────────────────────────────────┐
│        Network Stack                     │
│  - TCP connection (~10ms)                │
│  - HTTP protocol (~5ms)                  │
│  - Serialization (~5ms)                  │
└────────────┬────────────────────────────┘
             │ network
             ▼
┌─────────────────────────────────────────┐
│      LLM Server (separate machine)      │
│  - Always running                        │
│  - Scalable                              │
│  - Language-agnostic                     │
└─────────────────────────────────────────┘

Overhead: ~50-100ms
Benefits: Scalable, portable, cacheable
```

## Conclusion

Strategy B's architecture is **conceptually simple** but **performance-limited** due to process spawn overhead. The clean separation is good for development but the ~110ms fixed cost makes it unsuitable for interactive use cases.

For production:
- **Strategy A** (inline): Better performance
- **Strategy C** (HTTP): Better scalability
- **Strategy B** (process): Best isolation, worst performance
