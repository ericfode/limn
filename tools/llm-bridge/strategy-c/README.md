# Strategy C: Custom HVM4 Primitive

**Status:** Feasible - Analysis Complete
**Effort:** 3-5 days (basic implementation)
**Recommendation:** Conditional Yes (see analysis)

---

## Overview

This strategy implements LLM access as a native HVM4 primitive `%llm`, providing first-class language integration at the runtime level.

---

## Documents

| Document | Purpose | Status |
|----------|---------|--------|
| [ANALYSIS.md](./ANALYSIS.md) | Comprehensive feasibility analysis | ✅ Complete |
| [PRIMITIVE_SPEC.md](./PRIMITIVE_SPEC.md) | Detailed primitive specification | ✅ Complete |
| [EFFORT_ESTIMATE.md](./EFFORT_ESTIMATE.md) | Time and resource estimates | ✅ Complete |
| [llm_poc.c](./llm_poc.c) | Proof-of-concept implementation | ✅ Complete |

---

## Key Findings

### ✅ Feasibility: CONFIRMED

HVM4's primitive system is well-designed and straightforward to extend:

```c
// 1. Implement function
fn Term prim_fn_llm(Term *args) {
  char *query = hvm_list_to_cstring(args[0]);
  char *response = call_anthropic_api(query);
  return cstring_to_hvm_list(response);
}

// 2. Register
fn void prim_llm_init(void) {
  prim_register("llm", 3, 1, prim_fn_llm);
}
```

### 📊 Effort: MEDIUM

| Phase | Time | Deliverable |
|-------|------|-------------|
| **P0: Core** | 3 days | Basic working primitive |
| **P1: Hardening** | +1.5 days | Production-ready |
| **P2: Advanced** | +2.5 days | Async, caching |

### 🔧 Maintenance: LOW

- Clean interface via primitive API
- Isolated implementation
- Follows HVM4 conventions
- No core runtime changes

### ⚠️ Main Concerns

1. **Blocking I/O:** Stalls evaluator thread during API call (500ms-3s)
2. **Tight Coupling:** Requires HVM4 fork maintenance
3. **Limited Flexibility:** Hard to change without recompiling

---

## Usage Example

```hvm4
// Basic query
@main = %llm("What is 2+2?")
// Returns: "4"

// With superposition
@questions = &{"What is 2+2?", "What is 3+3?"}
@main = %llm(@questions)
// Returns: &{"4", "6"}

// Composed
@ask_twice = λq.
  let a = %llm(q)
  %llm(("Respond to: " ++ a))

@main = @ask_twice("Hello")
```

---

## Architecture

```
┌─────────────┐
│ HVM4 Code   │ %llm("query")
└──────┬──────┘
       │
       ▼
┌─────────────┐
│ wnf_pri()   │ Primitive evaluator
└──────┬──────┘
       │
       ▼
┌─────────────────┐
│ prim_fn_llm()   │ 1. Extract string
│                 │ 2. HTTP POST (libcurl)
│                 │ 3. Parse JSON
│                 │ 4. Return HVM list
└─────────────────┘
```

---

## Implementation Structure

```
hvm4/refinery/rig/clang/
├── prim/
│   ├── init.c              # Add prim_llm_init()
│   ├── register.c          # (no changes)
│   └── fn/
│       ├── llm.c           # NEW: Main primitive
│       ├── llm_http.c      # NEW: HTTP client
│       └── llm_json.c      # NEW: JSON parsing
└── main.c                  # Add -lcurl to build
```

---

## Dependencies

| Dependency | Purpose | Availability |
|------------|---------|--------------|
| **libcurl** | HTTP requests | Universal |
| **libcjson** (opt) | JSON parsing | Common |
| **ANTHROPIC_API_KEY** | API authentication | User-provided |

Install on Ubuntu/Debian:
```bash
sudo apt-get install libcurl4-openssl-dev libcjson-dev
```

---

## Build

```bash
cd hvm4/refinery/rig/clang
clang -O2 -o main main.c -lcurl
```

---

## Configuration

```bash
# Required
export ANTHROPIC_API_KEY="sk-ant-..."

# Optional
export LLM_MODEL="claude-sonnet-4-5-20250929"
export LLM_MAX_TOKENS="1024"
export LLM_TIMEOUT="30"
```

---

## Testing

```bash
# Basic test
echo '@main = %llm("Say hello")' > test.hvm4
./main test.hvm4

# With statistics
./main test.hvm4 -s

# Collapse superpositions
./main test.hvm4 -C10
```

---

## Comparison with Other Strategies

| Criteria | Strategy A (Subprocess) | Strategy B (FFI) | **Strategy C (Primitive)** |
|----------|-------------------------|------------------|----------------------------|
| **Effort** | 1 day | 2-3 days | **3-5 days** |
| **Performance** | Low (IPC overhead) | Medium | **Medium** |
| **Flexibility** | High (external tool) | High (library) | **Low (compiled)** |
| **Integration** | Loose | Medium | **Tight** |
| **Maintenance** | Very Low | Low | **Low** |
| **HVM4 Changes** | None | None | **Requires fork** |

---

## When to Use This Strategy

### ✅ Good Fit

- Building a **dedicated Limn-PL runtime**
- LLM calls are **first-class language feature**
- Willing to **maintain HVM4 fork**
- Need **tight integration** with evaluation
- Performance is **important but not critical**

### ❌ Not Recommended

- Want to keep **HVM4 unmodified**
- Need **frequent API changes**
- Require **multiple LLM providers**
- In **prototyping/experimentation** phase
- Need **async/non-blocking** immediately

---

## Recommended Approach

### Phase 1: Prototype (Strategy A)

Start with subprocess approach:
- Validate use cases
- Test API integration
- Measure performance needs
- **Effort:** 1 day

### Phase 2: Evaluate

If subprocess performance is:
- ✅ **Acceptable:** Stick with it
- ❌ **Too slow:** Consider primitive

### Phase 3: Implement (Strategy C)

If justified:
- Implement P0 (basic primitive)
- Measure improvement
- Add P1 (hardening) if needed
- **Effort:** 3-5 days

---

## Performance Characteristics

| Metric | Value | Notes |
|--------|-------|-------|
| **API Latency** | 500-2000ms | Anthropic Claude |
| **String Conversion** | <1ms | HVM ↔ C |
| **JSON Parsing** | <10ms | Simple extraction |
| **Memory** | ~10KB-10MB | Depends on response |
| **Concurrency** | Blocking | Stalls thread |

### Optimization Strategies

1. **Async I/O:** Separate thread pool (+2 days)
2. **Caching:** Memoize responses (+1 day)
3. **Batching:** Group multiple queries (+1 day)
4. **Streaming:** Token-by-token results (+3 days)

---

## Security Considerations

| Concern | Mitigation |
|---------|------------|
| **API key exposure** | Environment variable only |
| **JSON injection** | Escape special characters |
| **Buffer overflow** | Bounds checking |
| **Network attacks** | HTTPS + SSL verification |
| **Large responses** | max_tokens limit |

---

## Known Limitations

1. **No streaming:** Response arrives all at once
2. **No context:** Each call independent
3. **Fixed model:** Hardcoded (configurable via env)
4. **Synchronous:** Blocks evaluation
5. **No retry:** Single attempt per call
6. **No caching:** Every call hits API

---

## Future Extensions

### 1. Streaming API

```hvm4
%llm_stream(query, callback)
```

### 2. Contextual Conversations

```hvm4
%llm_ctx(history, query)
```

### 3. Multi-model Support

```hvm4
%llm_with("opus-4", query)
```

### 4. Async/Non-blocking

```hvm4
%llm_async(query)  // Returns future
```

### 5. Advanced Options

```hvm4
%llm_opts(query, {
  max_tokens: 2048,
  temperature: 0.7
})
```

---

## Risk Assessment

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| API changes | High | Medium | Version abstraction |
| Performance bottleneck | Medium | High | Async implementation |
| Memory leaks | High | Low | Careful management |
| Security vulnerabilities | High | Low | Input validation |
| HVM4 API changes | High | Low | Follow upstream |

---

## Files in This Directory

```
strategy-c/
├── README.md              # This file
├── ANALYSIS.md            # Comprehensive analysis
├── PRIMITIVE_SPEC.md      # Detailed specification
├── EFFORT_ESTIMATE.md     # Time/resource estimates
└── llm_poc.c              # Proof-of-concept code
```

---

## Next Steps

1. **Review:** Team reviews analysis documents
2. **Decide:** Choose strategy (A, B, or C)
3. **Prototype:** If C chosen, start with Phase 1
4. **Measure:** Benchmark against requirements
5. **Iterate:** Add Phase 2/3 as needed

---

## Questions?

**Implementation questions:**
- See PRIMITIVE_SPEC.md for detailed interface
- See llm_poc.c for code example

**Feasibility questions:**
- See ANALYSIS.md sections:
  - "HVM4 Primitive System Architecture"
  - "Existing Primitive: %log"

**Effort questions:**
- See EFFORT_ESTIMATE.md for breakdown

**Comparison questions:**
- See ANALYSIS.md "Alternatives Comparison"

---

## References

- **HVM4 source:** `/home/eric/src/limntown/hvm4/refinery/rig/`
- **Core docs:** `/home/eric/src/limntown/hvm4/refinery/rig/docs/hvm4/core.md`
- **Existing primitive:** `/home/eric/src/limntown/hvm4/refinery/rig/clang/prim/fn/log.c`
- **libcurl:** https://curl.se/libcurl/c/
- **Anthropic API:** https://docs.anthropic.com/

---

## Status Summary

**Analysis:** ✅ Complete
**Specification:** ✅ Complete
**Proof-of-Concept:** ✅ Complete
**Effort Estimate:** ✅ Complete
**Implementation:** ⏳ Pending decision
**Testing:** ⏳ Pending implementation

---

**Author:** Rex (Engineer)
**Date:** 2026-02-01
**Version:** 1.0

---

*cod flo | bui fst | shi val*
*(code flows | build fast | ship value)*
