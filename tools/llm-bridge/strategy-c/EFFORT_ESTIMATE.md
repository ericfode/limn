# Strategy C: %llm Primitive - Effort Estimate

**Date:** 2026-02-01
**Estimator:** Rex (Engineer)

---

## Summary

**Total Effort (P0 only):** 3 days
**Total Effort (P0 + P1):** 4 days
**Total Effort (Complete):** 6 days

**Confidence:** Medium (70%)

---

## Task Breakdown

### Phase 1: Core Implementation (P0)

| Task | Subtasks | Hours | Notes |
|------|----------|-------|-------|
| **String Utilities** | | **4h** | |
| ├─ HVM list → C string | 2h | UTF-8 decoding |
| ├─ C string → HVM list | 1.5h | UTF-8 encoding |
| └─ Unit tests | 0.5h | Edge cases |
| **HTTP Client** | | **8h** | |
| ├─ libcurl setup | 1h | Basic POST |
| ├─ Request formatting | 1h | JSON construction |
| ├─ Response handling | 1h | Buffer management |
| ├─ Error handling | 2h | Network errors, timeouts |
| ├─ Environment vars | 0.5h | API key, config |
| └─ Unit tests | 2.5h | Mock/real API |
| **JSON Parsing** | | **4h** | |
| ├─ Simple parser | 2h | Extract text field |
| ├─ Escape handling | 1h | Unicode, quotes |
| └─ Tests | 1h | Various responses |
| **Integration** | | **4h** | |
| ├─ Primitive registration | 0.5h | prim_register() |
| ├─ Term construction | 1h | Call helpers |
| ├─ Build integration | 0.5h | Add -lcurl |
| ├─ Memory management | 1h | Free allocations |
| └─ Smoke test | 1h | End-to-end |
| **Testing** | | **6h** | |
| ├─ Basic tests | 2h | Simple queries |
| ├─ Superposition tests | 1h | Parallel branches |
| ├─ Error tests | 1h | Invalid inputs |
| ├─ Integration tests | 2h | With real API |

**Phase 1 Total:** 26 hours (~3 days)

### Phase 2: Hardening (P1)

| Task | Subtasks | Hours | Notes |
|------|----------|-------|-------|
| **Error Handling** | | **4h** | |
| ├─ Validate inputs | 1h | Type checks |
| ├─ Network errors | 1h | Retries, fallback |
| ├─ API errors | 1h | Rate limits, 4xx/5xx |
| └─ User messages | 1h | Clear diagnostics |
| **Security** | | **3h** | |
| ├─ Input sanitization | 1h | JSON injection |
| ├─ SSL validation | 0.5h | Cert checks |
| ├─ Buffer limits | 1h | Overflow protection |
| └─ Review | 0.5h | Security audit |
| **Documentation** | | **3h** | |
| ├─ User guide | 1h | How to use %llm |
| ├─ API reference | 1h | Parameters, behavior |
| └─ Examples | 1h | Sample code |

**Phase 2 Total:** 10 hours (~1.5 days)

### Phase 3: Advanced Features (P2)

| Task | Subtasks | Hours | Notes |
|------|----------|-------|-------|
| **Async I/O** | | **12h** | |
| ├─ Thread pool | 4h | Worker threads |
| ├─ Callback mechanism | 3h | Continuations |
| ├─ Queue management | 2h | Request queue |
| ├─ Synchronization | 2h | Thread safety |
| └─ Tests | 1h | Concurrent access |
| **Caching** | | **4h** | |
| ├─ Cache data structure | 1.5h | Hash table |
| ├─ Cache policy | 1h | LRU, TTL |
| ├─ Integration | 1h | Check before call |
| └─ Tests | 0.5h | Hit/miss cases |
| **Enhanced Parsing** | | **3h** | |
| ├─ Use cJSON library | 1.5h | Integration |
| ├─ Better error messages | 1h | JSON path |
| └─ Tests | 0.5h | Edge cases |

**Phase 3 Total:** 19 hours (~2.5 days)

---

## Dependencies

### External

| Dependency | Install | Availability | Risk |
|------------|---------|--------------|------|
| libcurl | `apt install libcurl4-openssl-dev` | Universal | Low |
| libcJSON (opt) | `apt install libcjson-dev` | Common | Low |
| API key | Anthropic account | Requires signup | Low |

### Internal

| Dependency | Owner | Status | Risk |
|------------|-------|--------|------|
| HVM4 runtime | HVM4 team | Stable | Low |
| Term API | HVM4 team | Stable | Low |
| Primitive system | HVM4 team | Stable | Low |

---

## Risk Assessment

### Technical Risks

| Risk | Probability | Impact | Mitigation | Hours |
|------|-------------|--------|------------|-------|
| libcurl issues | Low | Medium | Use standard API | +2h |
| UTF-8 edge cases | Medium | Medium | Thorough testing | +4h |
| Memory leaks | Medium | High | Valgrind testing | +3h |
| API changes | Low | High | Version abstraction | +6h |
| Performance issues | High | Medium | Profile, optimize | +8h |

**Risk Buffer:** 23 hours (~3 days)

### Process Risks

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Scope creep | High | Medium | Stick to phases |
| API downtime | Low | Low | Mock for tests |
| HVM4 API changes | Low | High | Pin version |
| Review delays | Medium | Low | Schedule reviews |

---

## Effort by Role

### Developer

| Phase | Hours | % |
|-------|-------|---|
| Phase 1 | 26 | 50% |
| Phase 2 | 10 | 19% |
| Phase 3 | 19 | 37% |
| **Total** | **55** | **100%** |

### Reviewer

| Activity | Hours |
|----------|-------|
| Code review | 4 |
| Security review | 2 |
| **Total** | **6** |

### Tester

| Activity | Hours |
|----------|-------|
| Manual testing | 3 |
| Integration testing | 2 |
| **Total** | **5** |

---

## Timeline

### Option 1: Minimal (P0 only)

```
Week 1:
  Mon: String utilities (4h)
  Tue: HTTP client (8h)
  Wed: JSON parsing + integration (8h)
  Thu: Testing (6h)
  Fri: Buffer

Total: 3-4 days
```

### Option 2: Production-ready (P0 + P1)

```
Week 1:
  Mon-Thu: Phase 1 (26h)
  Fri: Phase 2 start (3h)

Week 2:
  Mon: Phase 2 finish (7h)
  Tue: Review + polish

Total: 5-6 days
```

### Option 3: Complete (P0 + P1 + P2)

```
Week 1: Phase 1
Week 2: Phase 2 + Phase 3 start
Week 3: Phase 3 finish + review

Total: 10-12 days
```

---

## Comparison: Actual vs Estimated (Post-mortem Template)

| Task | Estimated | Actual | Variance | Notes |
|------|-----------|--------|----------|-------|
| String utils | 4h | ? | ? | |
| HTTP client | 8h | ? | ? | |
| JSON parsing | 4h | ? | ? | |
| Integration | 4h | ? | ? | |
| Testing | 6h | ? | ? | |
| **Total** | **26h** | **?** | **?** | |

---

## Assumptions

1. **Developer familiarity:** Moderate C experience, familiar with HVM4
2. **Environment:** Dev machine with curl, compiler, API access
3. **Interruptions:** 20% buffer for meetings, questions
4. **Code quality:** Production-grade, not prototype
5. **Testing:** Manual + automated, no CI/CD setup time
6. **Documentation:** Inline comments + separate guide
7. **Review:** Single round, no major rework

---

## What Could Go Wrong

### Optimistic (50% probability)

- Everything works first try
- No API surprises
- Clean integration
- **Result:** 2 days (P0 only)

### Realistic (70% probability)

- Minor issues with UTF-8
- Some API edge cases
- Memory management tweaks
- **Result:** 3-4 days (P0 only)

### Pessimistic (90% probability)

- UTF-8 bugs require rework
- HVM4 API misunderstanding
- Performance issues need fixes
- API rate limiting problems
- **Result:** 5-6 days (P0 only)

### Worst Case (95% probability)

- Major HVM4 incompatibility
- Need to implement async from start
- Security vulnerabilities found
- Extensive rework required
- **Result:** 8-10 days (P0 only)

---

## Recommendations

### For Prototyping

**Use Option 1 (P0 only):** 3-4 days

- Bare minimum functionality
- No polish, just working code
- Good for proof-of-concept

### For Production

**Use Option 2 (P0 + P1):** 5-6 days

- Robust error handling
- Security hardening
- User-facing documentation
- **Recommended**

### For Long-term Product

**Use Option 3 (Complete):** 10-12 days

- Async I/O for performance
- Caching for efficiency
- Full feature set
- Only if justified by usage

---

## Resource Requirements

### Hardware

- Development machine: Linux/macOS
- Internet connection (for API calls)
- 1-2 GB free disk space

### Software

- Clang compiler
- libcurl-dev
- (optional) libcjson-dev
- (optional) valgrind for memory testing

### Access

- Anthropic API key
- HVM4 source code
- Git repository access

### Budget

- API costs: ~$0.01-0.10 per test (negligible)
- Developer time: 3-12 days @ $X/day
- **Total:** Mostly time, minimal expenses

---

## Sign-off

**Estimate prepared by:** Rex (Engineer)
**Date:** 2026-02-01
**Reviewed by:** [TBD]
**Approved by:** [TBD]

**Confidence level:** 70%
- 50% confidence: 2 days
- 70% confidence: 3-4 days
- 90% confidence: 5-6 days
- 95% confidence: 8-10 days

**Recommendation:** Start with Phase 1 (P0), re-assess after completion.
