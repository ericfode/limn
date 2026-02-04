# Strategy C: Custom HVM4 Primitive Analysis

**Date:** 2026-02-01
**Status:** Feasible - Medium Effort
**Recommendation:** Consider for production use, with caveats

---

## Executive Summary

Adding a custom `%llm` primitive to HVM4 is **technically feasible** and follows established patterns in the codebase. The primitive system is well-designed, extensible, and straightforward to implement.

**Key Findings:**
- Clean extension mechanism via registration system
- Requires C implementation with libcurl for HTTP calls
- Estimated effort: 3-5 days for basic implementation
- Maintenance burden: Low (isolated, follows HVM4 conventions)
- Main concern: Blocking I/O in parallel runtime

---

## HVM4 Primitive System Architecture

### 1. Primitive Definition Pattern

Primitives in HVM4 follow this structure:

```c
// 1. Define the primitive function
typedef Term (*PrimFn)(Term *args);

// 2. Implement the function
fn Term prim_fn_llm(Term *args) {
  // args[0] = query string (HVM list of #Chr)
  // Returns: response string (HVM list of #Chr)

  // Extract query
  char *query = extract_string(args[0]);

  // Make HTTP call to Anthropic API
  char *response = call_anthropic_api(query);

  // Convert to HVM string
  Term result = string_to_hvm(response);
  free(query);
  free(response);

  return result;
}

// 3. Register in init
fn void prim_llm_init(void) {
  prim_register("llm", 3, 1, prim_fn_llm);
}
```

### 2. Registration System

Located in `/home/eric/src/limntown/hvm4/refinery/rig/clang/prim/register.c`:

```c
typedef struct {
  PrimFn fun;
  u32 arity;
} PrimDef;

static PrimDef PRIM_DEFS[BOOK_CAP];

fn u32 prim_register(const char *name, u32 len, u32 arity, PrimFn fun);
```

The system uses:
- **name**: primitive identifier (e.g., "llm")
- **len**: length of name
- **arity**: number of arguments
- **fun**: function pointer to implementation

### 3. Evaluation Flow

From `/home/eric/src/limntown/hvm4/refinery/rig/clang/wnf/pri.c`:

```c
fn Term wnf_pri(Term pri) {
  u32 prim_id = term_ext(pri);
  Term (*fun)(Term *args) = prim_fun(prim_id);
  u32 arity = prim_arity(prim_id);

  // Load arguments from heap
  Term args[arity];
  for (u32 i = 0; i < arity; i++) {
    args[i] = heap_read(loc + i);
  }

  // Call primitive
  Term res = fun(args);
  return res;
}
```

**Key points:**
- Arguments are pre-allocated in heap
- Primitive is called synchronously
- Result is returned as Term

---

## Existing Primitive: %log

### Implementation Study

The `%log` primitive provides an excellent reference:

**File:** `/home/eric/src/limntown/hvm4/refinery/rig/clang/prim/fn/log.c`

**Pattern:**
1. Main function delegates to helper functions
2. Uses weak-head normal form (`wnf`) to evaluate arguments
3. Handles superpositions (parallel branches)
4. Extracts structured data (list of characters)
5. Performs side effect (prints to stdout)
6. Returns HVM term (`#Nil`)

**Critical insight:** `%log` shows how to:
- Extract strings from HVM list representation
- Handle side effects
- Deal with evaluation and normalization

---

## Implementation Design: %llm Primitive

### Interface

```hvm4
@main = %llm("What is 2+2?")
// Returns: #Con{#Chr{50}, #Con{#Chr{43}, #Con{#Chr{50}, #Nil}}}
// Which is: "2+2"
```

### Architecture

```
┌─────────────┐
│ HVM4 Code   │ @main = %llm("query")
└──────┬──────┘
       │
       ▼
┌─────────────┐
│ wnf_pri()   │ Evaluates primitive
└──────┬──────┘
       │
       ▼
┌─────────────────┐
│ prim_fn_llm()   │ 1. Extract string from HVM list
│                 │ 2. Make HTTP call via libcurl
│                 │ 3. Parse JSON response
│                 │ 4. Convert to HVM list
└─────────────────┘
       │
       ▼
┌─────────────────┐
│ libcurl         │ HTTP POST to api.anthropic.com
└─────────────────┘
```

### Core Components

#### 1. String Extraction (from HVM list)

```c
// Convert HVM list of #Chr to C string
fn char* hvm_list_to_cstring(Term list) {
  // Allocate buffer
  char *buf = malloc(4096);  // TODO: dynamic sizing
  int pos = 0;

  Term cur = wnf(list);
  while (1) {
    if (term_tag(cur) == C00 && term_ext(cur) == NAM_NIL) {
      buf[pos] = '\0';
      return buf;
    }
    if (term_tag(cur) == C02 && term_ext(cur) == NAM_CON) {
      u32 con_loc = term_val(cur);
      Term head = wnf(heap_read(con_loc + 0));
      Term tail = heap_read(con_loc + 1);

      // Extract character from #Chr{n}
      if (term_tag(head) == C01 && term_ext(head) == NAM_CHR) {
        u32 chr_loc = term_val(head);
        Term code = wnf(heap_read(chr_loc + 0));
        if (term_tag(code) == NUM) {
          buf[pos++] = (char)term_val(code);
          cur = wnf(tail);
          continue;
        }
      }
    }
    fprintf(stderr, "ERROR: invalid string format\n");
    exit(1);
  }
}
```

#### 2. HTTP Call (libcurl)

```c
// Simple libcurl wrapper
fn char* call_anthropic_api(const char *query) {
  CURL *curl = curl_easy_init();
  if (!curl) {
    fprintf(stderr, "ERROR: curl init failed\n");
    exit(1);
  }

  // Build JSON request
  char *json = malloc(strlen(query) + 1024);
  sprintf(json,
    "{\"model\":\"claude-sonnet-4-5-20250929\","
    "\"max_tokens\":1024,"
    "\"messages\":[{\"role\":\"user\",\"content\":\"%s\"}]}",
    query);

  // Setup headers
  struct curl_slist *headers = NULL;
  headers = curl_slist_append(headers, "Content-Type: application/json");

  char auth_header[512];
  char *api_key = getenv("ANTHROPIC_API_KEY");
  if (!api_key) {
    fprintf(stderr, "ERROR: ANTHROPIC_API_KEY not set\n");
    exit(1);
  }
  sprintf(auth_header, "x-api-key: %s", api_key);
  headers = curl_slist_append(headers, auth_header);
  headers = curl_slist_append(headers, "anthropic-version: 2023-06-01");

  // Response buffer
  struct MemoryStruct chunk = {.memory = malloc(1), .size = 0};

  curl_easy_setopt(curl, CURLOPT_URL, "https://api.anthropic.com/v1/messages");
  curl_easy_setopt(curl, CURLOPT_POSTFIELDS, json);
  curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&chunk);

  CURLcode res = curl_easy_perform(curl);

  curl_easy_cleanup(curl);
  curl_slist_free_all(headers);
  free(json);

  if (res != CURLE_OK) {
    fprintf(stderr, "ERROR: curl failed: %s\n", curl_easy_strerror(res));
    exit(1);
  }

  return chunk.memory;
}
```

#### 3. JSON Parsing

Simple approach (no external JSON library):

```c
// Extract content from response JSON
fn char* extract_content(const char *json) {
  // Find "content":[{"text":"...
  const char *start = strstr(json, "\"text\":\"");
  if (!start) return strdup("ERROR: No content found");

  start += 8; // Skip "text":"
  const char *end = strchr(start, '"');
  if (!end) return strdup("ERROR: Malformed JSON");

  size_t len = end - start;
  char *content = malloc(len + 1);
  strncpy(content, start, len);
  content[len] = '\0';

  return content;
}
```

**Better approach:** Use a lightweight JSON library like `jsmn` or `cJSON`.

#### 4. String to HVM List

```c
// Convert C string to HVM list of #Chr
fn Term cstring_to_hvm_list(const char *str) {
  if (str[0] == '\0') {
    return term_new_ctr(NAM_NIL, 0, 0);
  }

  // Allocate for #Chr{code} and #Con{head, tail}
  u32 chr_loc = heap_alloc(1);
  Term code = term_new_num((u32)str[0]);
  heap_set(chr_loc, code);
  Term chr = term_new_ctr(NAM_CHR, 1, chr_loc);

  Term tail = cstring_to_hvm_list(str + 1);

  u32 con_loc = heap_alloc(2);
  heap_set(con_loc + 0, chr);
  heap_set(con_loc + 1, tail);

  return term_new_ctr(NAM_CON, 2, con_loc);
}
```

---

## File Structure

```
hvm4/refinery/rig/clang/prim/
├── init.c                    # Modified to call prim_llm_init()
├── register.c                # No changes needed
└── fn/
    ├── log.c                 # Existing
    ├── log_go_0.c            # Existing
    ├── log_go_1.c            # Existing
    ├── log_go_2.c            # Existing
    ├── llm.c                 # NEW: Main entry point
    ├── llm_http.c            # NEW: HTTP/curl logic
    └── llm_json.c            # NEW: JSON parsing
```

**Modified files:**
- `clang/prim/init.c`: Add `prim_llm_init()` call
- `clang/main.c`: Potentially add `-lcurl` to build flags

**New files:**
- `clang/prim/fn/llm.c`: Main primitive implementation
- `clang/prim/fn/llm_http.c`: HTTP client
- `clang/prim/fn/llm_json.c`: JSON parsing

---

## Build Integration

### Compilation

```bash
cd clang
clang -O2 -o main main.c -lcurl
```

### Dependencies

- **libcurl**: Available on most systems
- **Optional**: cJSON for better JSON parsing

Ubuntu/Debian:
```bash
sudo apt-get install libcurl4-openssl-dev libcjson-dev
```

---

## Testing Strategy

### Unit Tests

```hvm4
// test/prim_llm_basic.hvm4
@main = %llm("Say 'hello' in one word")
//!hello
```

### Integration Tests

```hvm4
// test/prim_llm_superposition.hvm4
@main = %llm(&{"What is 2+2?", "What is 3+3?"})
//!4
//!6
```

### Error Handling Tests

```hvm4
// test/prim_llm_error.hvm4
@main = %llm(42)  // Should error: not a string
```

---

## Performance Considerations

### 1. Blocking I/O

**Problem:** HTTP calls block the evaluator thread.

**Impact:**
- Single-threaded mode: Stalls entire evaluation
- Multi-threaded mode: Stalls one thread, others continue

**Mitigation:**
- Use separate thread pool for I/O
- Implement async primitive interface (future work)

### 2. API Latency

**Typical latency:** 500ms - 3s per request

**Optimization strategies:**
- Batch multiple queries if possible
- Cache responses (requires additional infrastructure)
- Use streaming API (more complex)

### 3. Memory

**Concerns:**
- Large responses consume heap
- Multiple concurrent calls may exhaust memory

**Mitigation:**
- Limit response size (max_tokens parameter)
- Implement backpressure/rate limiting

---

## Security Considerations

### 1. API Key Management

**Current design:** Read from `ANTHROPIC_API_KEY` environment variable

**Risks:**
- Key exposed in process environment
- No key rotation

**Better approach:**
- Read from secure config file with restricted permissions
- Support credential providers (AWS Secrets Manager, etc.)

### 2. Input Validation

**Concerns:**
- Malicious strings could exploit JSON injection
- Very long strings could cause buffer overflows

**Mitigations:**
- Escape special characters in JSON
- Validate string length before allocation
- Use safe string functions (strncpy, snprintf)

### 3. Network Security

**Considerations:**
- Use HTTPS (libcurl default)
- Validate SSL certificates
- Set reasonable timeouts

```c
curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 1L);
curl_easy_setopt(curl, CURLOPT_SSL_VERIFYHOST, 2L);
curl_easy_setopt(curl, CURLOPT_TIMEOUT, 30L);
```

---

## Maintenance Assessment

### Coupling

**HVM4 Runtime:**
- Tight: Uses HVM term representation, heap, evaluator
- Acceptable: Follows primitive API contract

**External Dependencies:**
- libcurl: Stable, widely used
- Anthropic API: External service dependency

**Risk:** API changes would require updates

### Versioning Strategy

```c
#define LLM_PRIMITIVE_VERSION "1.0.0"
#define ANTHROPIC_API_VERSION "2023-06-01"

// Support multiple API versions
fn char* call_anthropic_api_v1(const char *query);
fn char* call_anthropic_api_v2(const char *query);
```

### Testing Burden

**Required tests:**
- Primitive registration
- String conversion (HVM ↔ C)
- HTTP success cases
- HTTP failure cases
- JSON parsing
- Superposition handling
- Error handling

**Estimated test LOC:** 500-800 lines

---

## Effort Estimation

### Implementation Phases

| Phase | Task | Effort | Priority |
|-------|------|--------|----------|
| 1 | String conversion utilities | 4 hours | P0 |
| 2 | HTTP client (libcurl) | 8 hours | P0 |
| 3 | JSON parsing (basic) | 4 hours | P0 |
| 4 | Main primitive integration | 4 hours | P0 |
| 5 | Unit tests | 6 hours | P0 |
| 6 | Error handling | 4 hours | P1 |
| 7 | Documentation | 3 hours | P1 |
| 8 | Advanced features (caching, async) | 16 hours | P2 |

**Total (P0):** ~3 days
**Total (P0+P1):** ~4 days
**Total (P0+P1+P2):** ~6 days

---

## Alternatives Comparison

| Approach | Effort | Maintenance | Performance | Flexibility |
|----------|--------|-------------|-------------|-------------|
| **Strategy C (Primitive)** | 3-5 days | Low | Medium | Low |
| Strategy A (subprocess) | 1 day | Very Low | Low | High |
| Strategy B (FFI) | 2-3 days | Low | Medium | High |

**Advantages over alternatives:**
- More integrated than subprocess
- Better performance than subprocess
- Simpler than full FFI system
- Native HVM4 construct

**Disadvantages:**
- Less flexible than FFI
- Requires modifying HVM4 source
- Tight coupling to runtime

---

## Recommendation

### When to Use Strategy C

✅ **Good fit if:**
- Building a dedicated Limn-PL runtime
- LLM calls are first-class language feature
- Willing to maintain HVM4 fork
- Need tight integration with evaluation

❌ **Not recommended if:**
- Want to keep HVM4 unmodified
- Need frequent API changes
- Require multiple LLM providers
- Prototyping/experimentation phase

### Suggested Approach

**Phase 1: Prototype with subprocess (Strategy A)**
- Validate use cases
- Test API integration
- Measure performance needs

**Phase 2: Implement primitive (Strategy C)**
- If performance acceptable, stick with subprocess
- If performance critical, implement primitive
- Consider async extension for parallel use

---

## Example Implementation Timeline

### Week 1: Foundation
- Day 1: String utilities + tests
- Day 2: HTTP client + tests
- Day 3: JSON parsing + basic integration

### Week 2: Integration
- Day 1: Primitive registration + error handling
- Day 2: Integration tests + documentation
- Day 3: Buffer (polish, edge cases)

### Week 3: Advanced (Optional)
- Day 1-2: Async I/O support
- Day 3: Caching + rate limiting

---

## Proof of Concept

### Minimal Working Implementation

File: `/home/eric/src/limntown/limn/crew/engineer/tools/llm-bridge/strategy-c/llm.c`

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <curl/curl.h>

// HVM4 types (simplified for POC)
typedef uint64_t Term;

// Simple implementation - just logs the query
fn Term prim_fn_llm(Term *args) {
  // In real implementation:
  // 1. char *query = hvm_list_to_cstring(args[0]);
  // 2. char *response = call_anthropic_api(query);
  // 3. Term result = cstring_to_hvm_list(response);
  // 4. return result;

  printf("LLM primitive called!\n");
  return 0; // Placeholder
}

fn void prim_llm_init(void) {
  // prim_register("llm", 3, 1, prim_fn_llm);
  printf("LLM primitive registered\n");
}
```

---

## Risks and Mitigations

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| API changes | High | Medium | Version abstraction layer |
| Performance bottleneck | Medium | High | Async implementation |
| Memory leaks | High | Low | Careful resource management |
| Security vulnerabilities | High | Low | Input validation, SSL |
| HVM4 API changes | High | Low | Follow upstream closely |

---

## Conclusion

**Feasibility:** ✅ **Viable**

The HVM4 primitive system is well-designed for extension. Adding `%llm` is straightforward and follows established patterns.

**Effort:** 📊 **Medium** (3-5 days)

Implementation is reasonable for a production feature.

**Maintenance:** 🔧 **Low**

Once implemented, maintenance burden is minimal if API stays stable.

**Recommendation:** 🎯 **Conditional Yes**

- **If building dedicated runtime:** Implement primitive
- **If rapid prototyping:** Start with subprocess
- **If need flexibility:** Consider FFI approach

**Next Steps:**
1. Review this analysis with team
2. Prototype with subprocess first (Strategy A)
3. Measure performance requirements
4. Implement primitive if justified

---

**Author:** Rex (Engineer)
**Date:** 2026-02-01
**Version:** 1.0
