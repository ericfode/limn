# Strategy C: Executive Summary

**Date:** 2026-02-01
**Engineer:** Rex
**Status:** Analysis Complete

---

## TL;DR

Adding a `%llm` primitive to HVM4 is **feasible and straightforward**. The primitive system is well-designed for extension. Estimated **3-5 days** for basic implementation.

**Recommendation:** Implement if building dedicated Limn-PL runtime. Otherwise, start with subprocess approach (Strategy A).

---

## What We Found

### 1. HVM4 Primitive System

**Location:** `/home/eric/src/limntown/hvm4/refinery/rig/clang/prim/`

**Pattern:**
```c
// Define function
fn Term prim_fn_llm(Term *args) {
  // Extract HVM string → C string
  // Make HTTP call
  // Convert response → HVM string
}

// Register
fn void prim_llm_init(void) {
  prim_register("llm", 3, 1, prim_fn_llm);
}
```

**Reference:** Existing `%log` primitive shows exact pattern to follow.

### 2. Implementation Requirements

**Core components:**
1. String conversion (HVM list ↔ C string)
2. HTTP client (libcurl wrapper)
3. JSON parsing (extract response text)
4. Primitive registration

**Dependencies:**
- libcurl (universally available)
- ANTHROPIC_API_KEY environment variable

### 3. Effort Breakdown

| Phase | Time | Deliverable |
|-------|------|-------------|
| P0: Core | 3 days | Working primitive |
| P1: Hardening | +1.5 days | Production-ready |
| P2: Advanced | +2.5 days | Async, caching |

**Confidence:** 70% (3-5 day range)

---

## How It Works

### User Perspective

```hvm4
@main = %llm("What is 2+2?")
// Returns: "4"
```

### System Perspective

```
HVM4 evaluator
    ↓
wnf_pri() [evaluate primitive]
    ↓
prim_fn_llm()
    ↓
┌─────────────────────────┐
│ 1. HVM list → C string  │
│ 2. HTTP POST to API     │
│ 3. Parse JSON response  │
│ 4. C string → HVM list  │
└─────────────────────────┘
    ↓
Return HVM string term
```

**Latency:** ~500-2000ms per call (API-dependent)

---

## Key Advantages

1. **Native integration:** First-class language construct
2. **Simple interface:** Just `%llm(query)`
3. **Superposition support:** Parallel queries work automatically
4. **Low maintenance:** Isolated, follows conventions
5. **Clean code:** ~500 LOC total

---

## Key Disadvantages

1. **Blocking I/O:** Stalls evaluator thread during API call
2. **Tight coupling:** Requires maintaining HVM4 fork
3. **Limited flexibility:** Hard to change without recompiling
4. **Single provider:** Locked to Anthropic (for now)
5. **No streaming:** Response arrives all at once

---

## Comparison

| Approach | Effort | Integration | Flexibility | HVM4 Changes |
|----------|--------|-------------|-------------|--------------|
| **A: Subprocess** | 1 day | Loose | High | None |
| **B: FFI** | 2-3 days | Medium | High | None |
| **C: Primitive** | 3-5 days | Tight | Low | Required |

---

## Decision Framework

### Choose Strategy C if:

- ✅ Building dedicated Limn-PL runtime
- ✅ LLM calls are core language feature
- ✅ Willing to maintain HVM4 fork
- ✅ Performance matters (but not critical)
- ✅ Want native integration

### Choose Strategy A instead if:

- ❌ Want to keep HVM4 unmodified
- ❌ Need rapid prototyping
- ❌ Require multi-provider support
- ❌ Flexibility more important than speed
- ❌ Experimentation phase

### Choose Strategy B instead if:

- ❌ Need multiple LLM providers
- ❌ Want library-level reuse
- ❌ Require dynamic linking
- ❌ Need fine-grained control

---

## Risk Analysis

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| **API changes** | High | Medium | Version abstraction layer |
| **Performance** | Medium | High | Async implementation (Phase 3) |
| **Security** | High | Low | Input validation, SSL checks |
| **Maintenance** | Medium | Low | Follow HVM4 conventions |

**Overall risk:** Low-Medium

---

## Recommended Path

### Phase 1: Prototype with Strategy A (1 day)

```bash
# Quick subprocess implementation
echo '@sys = λcmd. ...'
./limn-pl test.lmn
```

**Goal:** Validate use cases, measure needs

### Phase 2: Evaluate Performance

**If acceptable:** Stop here, keep subprocess
**If too slow:** Proceed to Phase 3

### Phase 3: Implement Strategy C (3-5 days)

**P0:** Basic primitive
**P1:** Error handling, docs
**P2:** Async (if needed)

---

## Technical Highlights

### String Conversion

```c
// HVM: #Con{#Chr{104}, #Con{#Chr{105}, #Nil}}
// ↓
// C: "hi"

char* hvm_list_to_cstring(Term list) {
  // Walk list, extract codepoints, encode UTF-8
}

Term cstring_to_hvm_list(const char *str) {
  // Decode UTF-8, build #Con/#Chr list
}
```

### HTTP Client

```c
char* call_anthropic_api(const char *query) {
  // libcurl POST to api.anthropic.com
  // Headers: API key, content-type
  // Body: JSON with model, query, max_tokens
}
```

### JSON Parsing

```c
char* extract_text_from_json(const char *json) {
  // Find "text":"..." in response
  // Handle escapes, return content
}
```

---

## Deliverables

All documents completed:

1. ✅ **ANALYSIS.md** - Deep dive into HVM4 primitives
2. ✅ **PRIMITIVE_SPEC.md** - Interface specification
3. ✅ **EFFORT_ESTIMATE.md** - Detailed breakdown
4. ✅ **llm_poc.c** - Proof-of-concept code
5. ✅ **README.md** - Implementation guide
6. ✅ **EXECUTIVE_SUMMARY.md** - This document

**Location:** `/home/eric/src/limntown/limn/crew/engineer/tools/llm-bridge/strategy-c/`

---

## Next Actions

1. **Review:** Team reviews analysis (30 min meeting)
2. **Decide:** Choose strategy A, B, or C
3. **Implement:** Follow recommended path
4. **Test:** Validate with real use cases
5. **Iterate:** Refine based on feedback

---

## Questions to Answer

Before proceeding:

1. **Use case:** How frequently will LLM calls happen?
2. **Performance:** What latency is acceptable?
3. **Flexibility:** How often will API change?
4. **Maintenance:** Who will maintain HVM4 fork?
5. **Timeline:** What's the deadline?

---

## Success Criteria

**MVP (P0):**
- [ ] `%llm(query)` works with basic strings
- [ ] Returns valid HVM string
- [ ] Handles errors gracefully
- [ ] Documented

**Production (P1):**
- [ ] All MVP criteria
- [ ] Security hardened
- [ ] Error messages clear
- [ ] Tests comprehensive

**Optimal (P2):**
- [ ] All Production criteria
- [ ] Async/non-blocking
- [ ] Response caching
- [ ] Multiple models

---

## Bottom Line

**Feasibility:** ✅ Yes, straightforward
**Effort:** 📊 3-5 days (basic)
**Quality:** 🔧 Production-ready possible
**Risk:** ⚠️ Low-Medium
**Recommendation:** 👍 Conditional Yes

**When:** After validating with subprocess prototype
**Why:** Native integration worth effort if building dedicated runtime
**How:** Follow PRIMITIVE_SPEC.md, use llm_poc.c as reference

---

**Author:** Rex (Engineer)
**Date:** 2026-02-01
**Sign-off:** Ready for review

---

*bui fst | cod cle | val shi*
*(build fast | code clear | value shipped)*
