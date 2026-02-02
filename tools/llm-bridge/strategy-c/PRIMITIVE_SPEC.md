# %llm Primitive Specification

**Version:** 1.0
**Date:** 2026-02-01
**Status:** Design

---

## Overview

The `%llm` primitive provides first-class LLM integration in HVM4, enabling direct API calls to Claude from HVM code.

---

## Syntax

```hvm4
%llm(query)
```

**Arguments:**
- `query`: String (HVM list of `#Chr{code}`)

**Returns:**
- String (HVM list of `#Chr{code}`)

---

## Examples

### Basic Usage

```hvm4
@main = %llm("What is 2+2?")
// Returns: "4"
```

### With Variables

```hvm4
@ask = λq. %llm(q)
@main = @ask("Hello")
// Returns: "Hello! How can I help you today?"
```

### With Superposition

```hvm4
@main = %llm(&{"Question A", "Question B"})
// Returns: &{"Answer A", "Answer B"}
```

### Composed

```hvm4
@double_ask = λq.
  let a = %llm(q)
  %llm(a)

@main = @double_ask("Say hello")
// First call: "Hello!"
// Second call: "Hello! How can I assist you?"
```

---

## Type Signature (Informal)

```
%llm : String → String
```

Where `String` is:
```
String ::= #Nil
         | #Con{#Chr{code}, String}
```

---

## Behavior

### String Representation

HVM4 represents strings as lists:

```hvm4
"hi" = #Con{#Chr{104}, #Con{#Chr{105}, #Nil}}
```

Where:
- `#Chr{n}`: Character with Unicode codepoint `n`
- `#Con{head, tail}`: List constructor
- `#Nil`: Empty list

### Evaluation

1. **Argument normalization:** Query is evaluated to weak-head normal form (WHNF)
2. **String extraction:** Converts HVM list to C string
3. **API call:** HTTP POST to Anthropic API
4. **Response parsing:** Extracts content from JSON response
5. **String construction:** Converts C string to HVM list

### Error Handling

Errors terminate the program with diagnostic message:

| Error | Cause | Message |
|-------|-------|---------|
| Invalid argument | Not a string | "ERROR: %llm expects string argument" |
| API key missing | `ANTHROPIC_API_KEY` unset | "ERROR: ANTHROPIC_API_KEY not set" |
| Network failure | HTTP error | "ERROR: HTTP request failed: [reason]" |
| JSON parse error | Malformed response | "ERROR: Failed to parse API response" |

**Future improvement:** Return error as HVM term instead of exit.

---

## Implementation Details

### C Function Signature

```c
Term prim_fn_llm(Term *args);
```

### Registration

```c
fn void prim_llm_init(void) {
  prim_register("llm", 3, 1, prim_fn_llm);
}
```

Parameters:
- `"llm"`: Primitive name
- `3`: Length of name
- `1`: Arity (number of arguments)
- `prim_fn_llm`: Implementation function

### Dependencies

**Required:**
- `libcurl` for HTTP requests
- `stdlib.h` for memory management
- `string.h` for string operations

**Optional:**
- `cJSON` for JSON parsing (or hand-rolled parser)

---

## API Integration

### Endpoint

```
POST https://api.anthropic.com/v1/messages
```

### Request Format

```json
{
  "model": "claude-sonnet-4-5-20250929",
  "max_tokens": 1024,
  "messages": [
    {"role": "user", "content": "query string here"}
  ]
}
```

### Headers

```
Content-Type: application/json
x-api-key: [ANTHROPIC_API_KEY from env]
anthropic-version: 2023-06-01
```

### Response Format

```json
{
  "content": [
    {"type": "text", "text": "response here"}
  ],
  "model": "claude-sonnet-4-5-20250929",
  ...
}
```

**Extraction:** Extract `.content[0].text` field.

---

## Configuration

### Environment Variables

| Variable | Required | Default | Description |
|----------|----------|---------|-------------|
| `ANTHROPIC_API_KEY` | Yes | - | API authentication key |
| `LLM_MODEL` | No | `claude-sonnet-4-5-20250929` | Model to use |
| `LLM_MAX_TOKENS` | No | `1024` | Max response length |
| `LLM_TIMEOUT` | No | `30` | HTTP timeout (seconds) |

### Example

```bash
export ANTHROPIC_API_KEY="sk-ant-..."
export LLM_MAX_TOKENS=2048
./main test.hvm4
```

---

## Performance Characteristics

### Latency

| Metric | Typical | Max |
|--------|---------|-----|
| API call | 500-2000ms | 30s |
| String conversion | <1ms | 10ms |
| JSON parsing | <10ms | 100ms |
| **Total** | ~500-2000ms | ~30s |

### Memory

| Allocation | Size |
|------------|------|
| Query buffer | Query length + padding |
| Response buffer | `max_tokens` × 4 bytes (UTF-8) |
| HVM list | Response length × 3 terms |
| **Peak** | ~10KB - 10MB |

### Concurrency

**Current implementation:** Synchronous, blocks evaluator thread.

**Impact:**
- Single-threaded: Complete stall
- Multi-threaded: One thread blocked, others continue

**Future:** Async I/O with callback continuation.

---

## Test Cases

### test/prim_llm_basic.hvm4

```hvm4
@main = %llm("Say 'ok'")
//!ok
```

### test/prim_llm_math.hvm4

```hvm4
@main = %llm("What is 2+2? Answer with just the number")
//!4
```

### test/prim_llm_multiline.hvm4

```hvm4
@main = %llm("List:\n1. First\n2. Second")
//!Here's the list:
//!1. First
//!2. Second
```

### test/prim_llm_superposition.hvm4

```hvm4
@main = %llm(&{"Q1", "Q2"})
// Expect two outputs
```

### test/prim_llm_composition.hvm4

```hvm4
@ask_twice = λq.
  let a = %llm(q)
  %llm(("Respond to: " ++ a))

@main = @ask_twice("Hello")
```

### test/prim_llm_error_notstring.hvm4

```hvm4
@main = %llm(42)
//!ERROR: %llm expects string argument
```

### test/prim_llm_error_nokey.hvm4

```bash
# Run with unset API key
unset ANTHROPIC_API_KEY
./main test.hvm4
# Expected: ERROR: ANTHROPIC_API_KEY not set
```

---

## Limitations

### Current Design

1. **No streaming:** Response returned after completion
2. **No context:** Each call independent (no conversation state)
3. **Fixed model:** Hardcoded to Sonnet 4.5
4. **Synchronous only:** Blocks evaluation
5. **No retry logic:** Single attempt per call
6. **No caching:** Every identical query hits API

### Future Extensions

#### 1. Streaming

```hvm4
%llm_stream(query, callback)
// Calls callback(chunk) for each token
```

#### 2. Contextual

```hvm4
%llm_ctx(context, query)
// context = list of {role, content} pairs
```

#### 3. Multi-model

```hvm4
%llm_with("opus-4", query)
```

#### 4. Async

```hvm4
%llm_async(query)
// Returns future/promise term
```

#### 5. Options

```hvm4
%llm_opts(query, {max_tokens: 2048, temperature: 0.7})
```

---

## Security Model

### Threat Model

| Threat | Risk | Mitigation |
|--------|------|------------|
| API key exposure | High | Environment variable only |
| Injection attacks | Medium | JSON escaping |
| Buffer overflow | Medium | Bounds checking |
| DoS via large responses | Medium | max_tokens limit |
| Man-in-the-middle | Low | HTTPS + cert validation |

### Hardening Checklist

- [ ] Validate string length before allocation
- [ ] Escape special chars in JSON (`"`, `\`, newlines)
- [ ] Use `strncpy`, `snprintf` instead of `strcpy`, `sprintf`
- [ ] Validate API response size
- [ ] Set libcurl timeout
- [ ] Enable SSL certificate verification
- [ ] Clear sensitive data from memory after use

---

## Comparison with %log

| Feature | %log | %llm |
|---------|------|------|
| Arity | 1 | 1 |
| Input | String | String |
| Output | `#Nil` | String |
| Side effect | Print to stdout | HTTP request |
| Latency | <1ms | 500-2000ms |
| Evaluation | Helper functions | Single call |
| Error handling | Exit on invalid | Exit on invalid |

**Similar patterns:**
- String extraction from HVM list
- Superposition handling
- Use of `wnf()` for normalization

**Key differences:**
- %llm returns data, %log returns unit
- %llm has network I/O
- %llm requires external dependencies

---

## Build Integration

### Modified Makefile (Conceptual)

```makefile
CC = clang
CFLAGS = -O2 -Wall -Wextra
LIBS = -lcurl

SOURCES = main.c hvm4.c prim/*.c prim/fn/*.c
HEADERS = *.h prim/*.h

main: $(SOURCES) $(HEADERS)
	$(CC) $(CFLAGS) -o main main.c $(LIBS)
```

### Build Commands

```bash
# Standard build
cd clang && clang -O2 -o main main.c -lcurl

# With debugging
cd clang && clang -O0 -g -o main main.c -lcurl

# With libcJSON
cd clang && clang -O2 -o main main.c -lcurl -lcjson
```

---

## Documentation

### User-facing

```markdown
# %llm Primitive

Query Claude directly from HVM4 code.

## Usage

%llm("your question here")

## Example

@main = %llm("What is the capital of France?")

## Requirements

Set ANTHROPIC_API_KEY environment variable:

export ANTHROPIC_API_KEY="sk-ant-..."
```

### Developer-facing

```markdown
# Implementing Primitives

See prim/fn/llm.c for reference implementation:

1. Define Term prim_fn_NAME(Term *args)
2. Register in prim_NAME_init()
3. Add init call to prim/init.c
4. Link dependencies in build
```

---

## Maintenance Plan

### Version Control

Tag primitive implementations with version:

```c
#define LLM_PRIMITIVE_VERSION "1.0.0"
```

### API Changes

When Anthropic API changes:

1. Create `llm_http_v2.c` with new implementation
2. Add version detection
3. Support both versions during transition
4. Deprecate old version after grace period

### Testing

**Continuous:**
- Unit tests (string conversion, JSON parsing)
- Integration tests (with mock API)
- Manual tests (real API, monthly)

**On API updates:**
- Regression test suite
- Version compatibility tests

---

## Open Questions

1. **Async I/O:** Should we implement async from start or wait for v2?
2. **Error handling:** Return error terms or exit? (Breaking change)
3. **Caching:** Worth implementing? Where (runtime or primitive)?
4. **Multi-provider:** Support OpenAI, others? Or separate primitives?
5. **Rate limiting:** Implement in primitive or let API handle?

---

## References

- HVM4 docs: `/home/eric/src/limntown/hvm4/refinery/rig/docs/hvm4/`
- %log implementation: `/home/eric/src/limntown/hvm4/refinery/rig/clang/prim/fn/log.c`
- libcurl: https://curl.se/libcurl/c/
- Anthropic API: https://docs.anthropic.com/

---

**Status:** Ready for implementation review
**Next step:** Team review, then prototype
