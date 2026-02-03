#!/usr/bin/env python3
"""Test parallel conscious/subconscious execution."""

import time
from harness import ProductionHarness, OracleRequest, OracleType

print("Testing Parallel Execution...")
print("=" * 70)

harness = ProductionHarness(enable_real_llm=False, max_concurrency=4)

# Create multiple oracle requests
requests = [
    OracleRequest(type=OracleType.SEMANTIC, params={'prompt': 'sys sta obs'}),
    OracleRequest(type=OracleType.TIME_NOW, params={}),
    OracleRequest(type=OracleType.ARITH, params={'operation': 'add', 'a': 5, 'b': 3}),
    OracleRequest(type=OracleType.SEMANTIC, params={'prompt': 'mem acc ctx'}),
]

print(f"Submitting {len(requests)} oracle requests...\n")

# Sequential execution (baseline)
print("SEQUENTIAL MODE:")
start = time.time()
sequential_results = []
for i, req in enumerate(requests):
    print(f"  Executing oracle {i+1}/{len(requests)}...")
    resp = harness.execute_oracle(req)
    sequential_results.append(resp)
sequential_time = time.time() - start
print(f"  ✓ Completed in {sequential_time:.2f}s\n")

# Parallel execution
print("PARALLEL MODE:")
start = time.time()

# Launch all oracles asynchronously
futures = []
for i, req in enumerate(requests):
    print(f"  ⚡ Launching oracle {i+1}/{len(requests)} async...")
    future = harness.execute_oracle_async(req)
    futures.append(future)

# Wait for all to complete
parallel_results = []
for i, future in enumerate(futures):
    print(f"  ⏳ Waiting for oracle {i+1}/{len(futures)}...")
    resp = harness.wait_for_oracle(future)
    parallel_results.append(resp)

parallel_time = time.time() - start
print(f"  ✓ Completed in {parallel_time:.2f}s\n")

# Summary
print("=" * 70)
print("PERFORMANCE COMPARISON:")
print(f"  Sequential: {sequential_time:.2f}s")
print(f"  Parallel:   {parallel_time:.2f}s")
speedup = sequential_time / parallel_time if parallel_time > 0 else 1.0
print(f"  Speedup:    {speedup:.2f}x")
print()

# Verify results match
print("RESULTS VERIFICATION:")
all_match = True
for i, (seq, par) in enumerate(zip(sequential_results, parallel_results)):
    match = "✓" if seq.success == par.success else "✗"
    print(f"  Oracle {i+1}: {match} Sequential={seq.success}, Parallel={par.success}")
    if seq.success != par.success:
        all_match = False

if all_match:
    print("\n✓ All results match - parallel execution is correct!")
else:
    print("\n✗ Results differ - parallel execution has issues")
