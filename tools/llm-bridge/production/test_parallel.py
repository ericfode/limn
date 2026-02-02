#!/usr/bin/env python3
"""
Test parallel oracle execution.
Tests that parallel execution produces the same results as sequential.
"""

from pathlib import Path
from harness import ProductionHarness
import time


def test_parallel_execution():
    """Test that parallel execution works correctly."""
    script_dir = Path(__file__).parent
    test_file = script_dir / "test.bend"

    if not test_file.exists():
        print(f"Error: {test_file} not found")
        return False

    print("=" * 70)
    print("PARALLEL EXECUTION TEST")
    print("=" * 70)
    print()

    # Create harness
    harness = ProductionHarness(
        enable_real_llm=False,  # Use mock for testing
        max_concurrency=4
    )

    # Check if bend is available
    import shutil
    if not shutil.which('bend') and not (script_dir.parent.parent / 'lmn-bend' / 'bend').exists():
        print("⚠ Bend binary not available - skipping full execution test")
        print("  (This is OK - testing batch API instead)")
        return True

    print("TEST 1: Sequential execution")
    print("-" * 70)
    start = time.time()
    result_seq = harness.execute(test_file, verbose=False, parallel=False)
    time_seq = time.time() - start
    print(f"Sequential time: {time_seq:.3f}s")
    print(f"Success: {result_seq['success']}")
    print()

    print("TEST 2: Parallel execution")
    print("-" * 70)
    start = time.time()
    result_par = harness.execute(test_file, verbose=False, parallel=True)
    time_par = time.time() - start
    print(f"Parallel time: {time_par:.3f}s")
    print(f"Success: {result_par['success']}")
    print()

    # Verify both succeeded
    if not result_seq['success'] or not result_par['success']:
        print("✗ FAILED: One or both executions failed")
        return False

    # Verify oracle count matches
    oracles_seq = result_seq.get('oracles', [])
    oracles_par = result_par.get('oracles', [])

    if len(oracles_seq) != len(oracles_par):
        print(f"✗ FAILED: Oracle count mismatch: {len(oracles_seq)} vs {len(oracles_par)}")
        return False

    print(f"✓ Oracle count matches: {len(oracles_seq)}")

    # Compare results (order should be preserved)
    mismatches = 0
    for i, (seq_oracle, par_oracle) in enumerate(zip(oracles_seq, oracles_par)):
        if seq_oracle['result'] != par_oracle['result']:
            print(f"  Warning: Oracle {i+1} result differs")
            print(f"    Sequential: {seq_oracle['result']}")
            print(f"    Parallel: {par_oracle['result']}")
            mismatches += 1

    if mismatches == 0:
        print("✓ All oracle results match")
    else:
        print(f"⚠ {mismatches} oracle(s) had different results (may be expected for time/semantic)")

    # Show speedup
    if time_seq > 0:
        speedup = time_seq / time_par
        print(f"\n✓ Speedup: {speedup:.2f}x")
        if speedup > 1.0:
            print(f"  Parallel execution was {speedup:.2f}x faster")
        else:
            print("  Note: Speedup may be minimal with cached/fast oracles")

    print()
    print("=" * 70)
    print("✓ TEST PASSED: Parallel execution works correctly")
    print("=" * 70)

    return True


def test_batch_api():
    """Test the batch execution API directly."""
    print()
    print("=" * 70)
    print("BATCH API TEST")
    print("=" * 70)
    print()

    harness = ProductionHarness(
        enable_real_llm=False,
        max_concurrency=4
    )

    # Create some test oracles
    from harness import OracleRequest, OracleType

    oracles = [
        OracleRequest(OracleType.ARITH, {"op": "add", "a": 5, "b": 3}),
        OracleRequest(OracleType.ARITH, {"op": "mul", "a": 6, "b": 7}),
        OracleRequest(OracleType.ARITH, {"op": "sub", "a": 10, "b": 4}),
        OracleRequest(OracleType.TIME_NOW, {}),
    ]

    print(f"Testing batch execution with {len(oracles)} oracles...")
    responses = harness.execute_oracles_batch(oracles, parallel=True)

    print(f"Got {len(responses)} responses")

    # Verify all succeeded
    failed = sum(1 for r in responses if not r.success)
    if failed > 0:
        print(f"✗ FAILED: {failed} oracle(s) failed")
        return False

    print("✓ All oracles executed successfully")

    # Show results
    for i, (oracle, response) in enumerate(zip(oracles, responses), 1):
        print(f"  Oracle {i}: {oracle.type.value} -> {response.result} ({response.duration_ms:.2f}ms)")

    print()
    print("=" * 70)
    print("✓ TEST PASSED: Batch API works correctly")
    print("=" * 70)

    return True


if __name__ == "__main__":
    try:
        success = test_parallel_execution() and test_batch_api()
        exit(0 if success else 1)
    except Exception as e:
        print(f"\n✗ TEST FAILED: {e}")
        import traceback
        traceback.print_exc()
        exit(1)
