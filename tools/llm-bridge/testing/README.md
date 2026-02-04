# Novel Testing Approaches for Pure Oracle System

**Unconventional test infrastructure for the LMN text→text oracle architecture**

## Philosophy

Traditional unit tests verify specific behaviors. These tests explore **emergent properties** of the oracle system:

- **Property-based testing** - Invariants that should hold across infinite oracle combinations
- **Fuzzing** - What happens when oracles get malformed/adversarial inputs?
- **Chaos engineering** - How resilient is the system to random failures?
- **Load testing** - Performance characteristics at scale
- **Execution tracing** - Visual understanding of oracle flow
- **Composition testing** - Semantic interactions between oracle types

## Testing Frameworks

### 1. Property-Based Testing (`property_tests.py`)

Tests oracle system **invariants** using randomized inputs:

```python
# Example properties:
# - All arithmetic oracles are deterministic
# - Memory store→retrieve is identity
# - File operations preserve content
# - Time operations are monotonic
```

**Run:**
```bash
python3 property_tests.py
```

### 2. Oracle Fuzzer (`fuzzer.py`)

Generates random/malformed oracle requests to test robustness:

```python
# Fuzzing strategies:
# - Invalid oracle types
# - Missing required fields
# - Type mismatches
# - Extreme values
# - Malicious paths
# - SQL injection attempts
```

**Run:**
```bash
python3 fuzzer.py --iterations=1000
```

### 3. Chaos Engineering (`chaos_harness.py`)

Modified harness that injects random failures:

```python
# Chaos modes:
# - Random oracle failures (simulates network/LLM outages)
# - Delayed responses (simulates latency)
# - Corrupted results (simulates data corruption)
# - Partial failures (some oracles fail, others succeed)
```

**Run:**
```bash
python3 chaos_harness.py --failure-rate=0.1
```

### 4. Load Testing (`load_test.py`)

Tests oracle system at scale:

```python
# Test scenarios:
# - Thousands of arithmetic oracles (fast path)
# - Hundreds of semantic oracles (slow path)
# - Mixed workload (realistic usage)
# - Sequential vs parallel execution patterns
```

**Run:**
```bash
python3 load_test.py --oracles=10000
```

### 5. Execution Tracer (`trace_visualizer.py`)

Generates visual traces of oracle execution:

```
TimeNow (0.36ms)
  └─> {"timestamp": 1770000633, ...}
FileRead("/etc/hostname") (0.42ms)
  └─> "hostname\n"
Semantic("What is 2+2?", "math") (512ms)
  └─> "4"
```

**Run:**
```bash
python3 trace_visualizer.py program.bend --output=trace.html
```

### 6. Composition Tester (`composition_tests.py`)

Tests semantic interactions between oracle types:

```python
# Test compositions:
# - File → Semantic (read file, analyze with LLM)
# - Memory → Semantic (recall context, reason about it)
# - Time → Database (timestamp queries)
# - Network → Semantic (fetch data, interpret)
```

**Run:**
```bash
python3 composition_tests.py
```

## Architecture

All tests interact with the oracle system through the same pure text→text interface:

```
Test Framework
    ↓
Bend Program (generates oracle requests)
    ↓
Harness (executes side effects)
    ↓
Results (verified by test framework)
```

This ensures tests exercise the **actual production path**, not mocks.

## Quick Start

Run all tests:
```bash
./run_all_tests.sh
```

Run specific category:
```bash
python3 property_tests.py    # Property-based
python3 fuzzer.py            # Fuzzing
python3 chaos_harness.py     # Chaos engineering
```

## Test Philosophy

**Why these tests matter:**

1. **Property tests** find edge cases unit tests miss
2. **Fuzzing** reveals security vulnerabilities
3. **Chaos engineering** proves system resilience
4. **Load tests** expose performance bottlenecks
5. **Execution traces** enable debugging complex oracle chains
6. **Composition tests** verify semantic correctness

Traditional tests answer "does X work?" These tests answer:
- "What are X's fundamental properties?"
- "How does X break?"
- "How does X behave under stress?"
- "How do components interact?"

## Future Extensions

- **Semantic drift detection** - Track LLM oracle consistency over time
- **Time-travel debugging** - Replay oracle sequences with different inputs
- **Interactive debugger** - Step through oracle execution
- **Differential testing** - Compare oracle results across harness versions
- **Coverage analysis** - Which oracle paths are exercised?

---

*Testing the subconscious-conscious boundary*
