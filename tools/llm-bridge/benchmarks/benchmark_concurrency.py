#!/usr/bin/env python3
"""
Concurrency Benchmarks
======================

Tests concurrent oracle handling.

Measures:
- Concurrent execution throughput
- Thread safety
- Resource contention

Author: Performance Testing Suite
Date: 2026-02-01
"""

import sys
import time
import json
import threading
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Any
from concurrent.futures import ThreadPoolExecutor, as_completed

# Add parent to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from production.harness import ProductionHarness, OracleRequest, OracleType


def execute_oracle_batch(
    harness: ProductionHarness,
    batch_size: int,
    oracle_type: OracleType = OracleType.ARITH
) -> Dict[str, Any]:
    """Execute a batch of oracles in the current thread.

    Args:
        harness: Production harness instance
        batch_size: Number of oracles to execute
        oracle_type: Type of oracle

    Returns:
        Execution metrics
    """
    start = time.perf_counter()
    successes = 0
    failures = 0

    for i in range(batch_size):
        oracle = OracleRequest(
            type=oracle_type,
            params={"op": "add", "a": i, "b": i + 1}
        )

        response = harness.execute_oracle(oracle)
        if response.success:
            successes += 1
        else:
            failures += 1

    elapsed = time.perf_counter() - start

    return {
        "batch_size": batch_size,
        "successes": successes,
        "failures": failures,
        "elapsed_sec": elapsed,
        "throughput": batch_size / elapsed if elapsed > 0 else 0
    }


def benchmark_concurrent_execution(
    thread_counts: List[int],
    oracles_per_thread: int = 100
) -> List[Dict[str, Any]]:
    """Benchmark concurrent oracle execution.

    Args:
        thread_counts: List of thread counts to test
        oracles_per_thread: Oracles each thread executes

    Returns:
        Concurrency metrics
    """
    results = []

    for num_threads in thread_counts:
        print(f"[Concurrency] {num_threads} threads...")

        # Each thread gets its own harness (thread-safe)
        harnesses = [ProductionHarness(enable_real_llm=False) for _ in range(num_threads)]

        start = time.perf_counter()

        with ThreadPoolExecutor(max_workers=num_threads) as executor:
            futures = [
                executor.submit(execute_oracle_batch, harness, oracles_per_thread)
                for harness in harnesses
            ]

            batch_results = [future.result() for future in as_completed(futures)]

        elapsed = time.perf_counter() - start

        # Aggregate results
        total_oracles = sum(r["successes"] for r in batch_results)
        total_failures = sum(r["failures"] for r in batch_results)
        overall_throughput = total_oracles / elapsed if elapsed > 0 else 0

        result = {
            "num_threads": num_threads,
            "oracles_per_thread": oracles_per_thread,
            "total_oracles": total_oracles,
            "total_failures": total_failures,
            "elapsed_sec": elapsed,
            "throughput_per_sec": overall_throughput,
            "throughput_per_thread": overall_throughput / num_threads if num_threads > 0 else 0
        }

        results.append(result)

        print(f"  Oracles:    {total_oracles}")
        print(f"  Time:       {elapsed:.3f} sec")
        print(f"  Throughput: {overall_throughput:.1f} oracles/sec")
        print(f"  Per thread: {result['throughput_per_thread']:.1f} oracles/sec")
        print()

    return results


def test_thread_safety(iterations: int = 1000) -> Dict[str, Any]:
    """Test thread safety of harness.

    Args:
        iterations: Number of concurrent operations

    Returns:
        Safety test results
    """
    print("[Safety] Testing thread safety...")

    harness = ProductionHarness(enable_real_llm=False)
    errors = []
    lock = threading.Lock()

    def worker(worker_id: int):
        """Worker thread."""
        try:
            for i in range(iterations // 10):
                oracle = OracleRequest(
                    type=OracleType.MEMORY_STORE,
                    params={"key": f"worker_{worker_id}_{i}", "value": f"value_{i}"}
                )
                response = harness.execute_oracle(oracle)

                if not response.success:
                    with lock:
                        errors.append(f"Worker {worker_id}: {response.error}")

        except Exception as e:
            with lock:
                errors.append(f"Worker {worker_id}: {str(e)}")

    # Run 10 workers concurrently
    threads = [threading.Thread(target=worker, args=(i,)) for i in range(10)]

    start = time.perf_counter()
    for t in threads:
        t.start()
    for t in threads:
        t.join()
    elapsed = time.perf_counter() - start

    is_safe = len(errors) == 0

    result = {
        "iterations": iterations,
        "threads": 10,
        "elapsed_sec": elapsed,
        "errors": errors,
        "is_thread_safe": is_safe
    }

    status = "✓" if is_safe else "✗"
    print(f"  {status} Thread safety: {len(errors)} errors")
    print(f"  Time: {elapsed:.3f} sec")
    print()

    return result


def run_benchmarks() -> Dict[str, Any]:
    """Run concurrency benchmarks."""
    print("=" * 70)
    print("Concurrency Benchmarks")
    print("=" * 70)
    print()

    # Test with increasing thread counts
    thread_counts = [1, 2, 4, 8, 16]

    concurrency_results = benchmark_concurrent_execution(thread_counts)

    # Test thread safety
    safety_result = test_thread_safety(iterations=1000)

    results = {
        "benchmark": "concurrency",
        "timestamp": datetime.now(datetime.UTC).isoformat() + "Z",
        "concurrency_results": concurrency_results,
        "thread_safety": safety_result
    }

    # Analysis
    print("=" * 70)
    print("Analysis")
    print("=" * 70)
    print()

    # Scaling efficiency
    if len(concurrency_results) > 1:
        baseline = concurrency_results[0]
        best = max(concurrency_results, key=lambda x: x["throughput_per_sec"])

        speedup = best["throughput_per_sec"] / baseline["throughput_per_sec"]
        efficiency = speedup / best["num_threads"]

        print(f"Best throughput: {best['throughput_per_sec']:.1f} oracles/sec ({best['num_threads']} threads)")
        print(f"Speedup:         {speedup:.2f}x")
        print(f"Efficiency:      {efficiency*100:.1f}%")
        print()

    # Thread safety
    if safety_result["is_thread_safe"]:
        print("✓ Thread safety: PASS")
    else:
        print("✗ Thread safety: FAIL")
        print(f"  Errors: {len(safety_result['errors'])}")
        for error in safety_result['errors'][:5]:
            print(f"    - {error}")

    print()

    return results


def main():
    """Main entry point."""
    results = run_benchmarks()

    # Save results
    output_file = Path(__file__).parent / "results_concurrency.json"
    with open(output_file, "w") as f:
        json.dump(results, f, indent=2)

    print(f"Results saved to: {output_file}")
    print()

    return 0


if __name__ == "__main__":
    exit(main())
