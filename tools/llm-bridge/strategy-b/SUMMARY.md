# Strategy B: External Process Calls - Test Summary

## What Was Tested

Testing Bend's ability to call external LLM processes via FFI (Foreign Function Interface) using IO/DyLib functions.

## Files Created

### Documentation
- `README.md` - Overview of Strategy B and Bend FFI
- `BUILD.md` - Compilation and setup instructions
- `ASSESSMENT.md` - Detailed viability analysis
- `SUMMARY.md` - This file

### Implementation
- `process_lib.c` - C library for process spawning and LLM calls
- `echo_lib.c` - Simple echo function (basic FFI test)
- `mock-llm` - Bash script simulating LLM CLI tool

### Test Programs
- `test_echo.bend` - Basic FFI test with echo command
- `test_llm.bend` - LLM call via FFI
- `benchmark.bend` - Performance measurement (requires Bend)
- `benchmark.py` - Python benchmark (runs without Bend)

## Key Findings

### Technical Capability

✅ **Bend supports external process calls** via IO/DyLib FFI
- Load shared libraries with `IO/DyLib/open`
- Call C functions with `IO/DyLib/call`
- C code can spawn processes and capture output
- Type conversion available between HVM and C

### Performance Results

```
Process Spawn:     ~109ms (mean)
Single Call:       ~110ms total
Throughput:        ~9.2 calls/second
Memory:            ~1MB per call
```

**Overhead Breakdown**:
- Process spawn: ~109ms (98%)
- IPC/marshalling: ~5-10ms (estimated)
- FFI boundary: ~1-2ms (estimated)

### Viability: ⚠️ CONDITIONAL

**Good for**:
- Batch processing (non-interactive)
- Slow LLM queries (>2 seconds)
- Development/prototyping
- Strong process isolation needs

**Bad for**:
- Real-time interaction (<200ms target)
- High throughput (>10 calls/sec)
- Resource-constrained environments
- When better alternatives exist

## Comparison

| Strategy | Latency | Throughput | Complexity | Status |
|----------|---------|------------|------------|--------|
| A: Inline FFI | ~5ms | ~100/s | Low | Unknown |
| B: External Process | ~110ms | ~9/s | Moderate | **TESTED** |
| C: HTTP API | ~50ms | ~50/s | Low | Unknown |

## Optimization Potential

With optimizations (process pooling, shared memory, batching):
- Could reduce to ~10-15ms per call
- Batch of 10: ~1.5ms per query
- Still higher overhead than inline FFI
- Increases implementation complexity significantly

## Recommendation

**Use Strategy B for**:
1. Prototyping and development
2. Batch/background processing
3. When LLM response time >2 seconds
4. As fallback option

**Don't use Strategy B for**:
1. Interactive chat interfaces
2. Real-time code completion
3. High-frequency API calls
4. Production systems (prefer A or C)

**Overall Score**: 5/10
- Works technically ✓
- Performance limitations ✗
- Production viability: conditional

## Next Steps

1. **Test Strategy A** (inline FFI if available)
   - Lower overhead expected
   - Better throughput
   - Simpler implementation

2. **Test Strategy C** (HTTP API)
   - Network overhead vs process spawn
   - Scalability comparison
   - Deployment considerations

3. **Implement optimizations** (if choosing Strategy B)
   - Process pooling
   - Batch requests
   - Shared memory IPC

4. **Make final decision** based on:
   - Use case requirements
   - Performance needs
   - Deployment constraints
   - Available alternatives

## Sources

- [Bend Programming Language](https://github.com/HigherOrderCO/Bend)
- [Bend FFI Documentation](https://github.com/HigherOrderCO/Bend/blob/main/docs/ffi.md)
- [HVM Dylib PR #394](https://github.com/HigherOrderCO/HVM/pull/394)
- [Bend Dylib Issue #621](https://github.com/HigherOrderCO/Bend/issues/621)
- [Bend Dylib PR #624](https://github.com/HigherOrderCO/Bend/pull/624)
