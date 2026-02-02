# LMN Oracle Performance Benchmarks

Comprehensive performance testing suite for the LMN oracle system.

## Benchmark Suite

### 1. `benchmark_oracle_execution.py`
Measures individual oracle execution times across all oracle types.

**Metrics:**
- Min/max/avg/p95/p99 execution times per oracle type
- Throughput (oracles/second)
- Cache effectiveness

### 2. `benchmark_concurrency.py`
Tests concurrent oracle handling and thread safety.

**Metrics:**
- Concurrent execution throughput
- Resource contention overhead
- Thread safety validation

### 3. `benchmark_scaling.py`
Stress tests with increasing oracle counts (1, 10, 100, 1000, 10000).

**Metrics:**
- Linear vs sublinear scaling
- Memory growth patterns
- Performance degradation points

### 4. `benchmark_compilation.py`
Measures Bend compilation overhead.

**Metrics:**
- Compilation time vs program size
- Cached compilation benefits
- Compilation bottlenecks

### 5. `benchmark_memory.py`
Profiles memory usage patterns.

**Metrics:**
- Peak memory per oracle type
- Memory leaks detection
- Cache memory overhead

### 6. `benchmark_comparison.py`
Compares different implementation approaches.

**Metrics:**
- Production vs strategy-a vs reference-pure
- Performance/complexity tradeoffs

### 7. `profiler.py`
CPU profiling of Python harness using cProfile.

**Outputs:**
- Hotspot identification
- Call graph analysis
- Optimization recommendations

## Running Benchmarks

```bash
# Run all benchmarks
python3 benchmarks/run_all.py

# Run specific benchmark
python3 benchmarks/benchmark_oracle_execution.py

# Run with profiling
python3 benchmarks/profiler.py

# Generate comparison report
python3 benchmarks/benchmark_comparison.py --report
```

## Output Format

All benchmarks output JSON for programmatic analysis:

```json
{
  "benchmark": "oracle_execution",
  "timestamp": "2026-02-01T19:00:00Z",
  "metrics": {
    "semantic": {"avg_ms": 150.5, "p95_ms": 200.0},
    "arith": {"avg_ms": 0.05, "p95_ms": 0.1}
  }
}
```

## Performance Targets

| Metric | Target | Critical |
|--------|--------|----------|
| Semantic oracle | < 200ms | < 500ms |
| Arith oracle | < 1ms | < 5ms |
| Cache hit | < 0.1ms | < 1ms |
| 1000 oracles | < 60s | < 120s |
| Memory per oracle | < 1MB | < 10MB |

## Continuous Monitoring

Benchmarks run automatically on:
- Every commit (regression detection)
- Nightly (long-running tests)
- Release candidates (full suite)
