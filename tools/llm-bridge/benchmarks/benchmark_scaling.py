#!/usr/bin/env python3
"""
Scaling Benchmarks
==================

Stress tests with increasing oracle counts: 1, 10, 100, 1000, 10000 oracles.

Measures:
- Linear vs sublinear scaling
- Performance degradation
- Cache effectiveness at scale

Author: Performance Testing Suite
Date: 2026-02-01
"""

import sys
import time
import json
import psutil
import os
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Any

# Add parent to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from production.harness import ProductionHarness, OracleRequest, OracleType


def benchmark_scale(
    harness: ProductionHarness,
    oracle_counts: List[int],
    oracle_type: OracleType = OracleType.ARITH
) -> Dict[str, Any]:
    """Benchmark scaling with increasing oracle counts.

    Args:
        harness: Production harness instance
        oracle_counts: List of oracle counts to test
        oracle_type: Type of oracle to use

    Returns:
        Scaling metrics
    """
    results = []

    process = psutil.Process(os.getpid())

    for count in oracle_counts:
        print(f"[Scale] Testing {count} oracles...")

        # Reset stats
        harness.stats = {
            "total_oracles": 0,
            "cache_hits": 0,
            "total_time_ms": 0
        }

        # Get baseline memory
        mem_before = process.memory_info().rss / 1024 / 1024  # MB

        # Execute oracles
        start = time.perf_counter()

        for i in range(count):
            oracle = OracleRequest(
                type=oracle_type,
                params={"op": "add", "a": i, "b": i + 1}
            )
            harness.execute_oracle(oracle)

        elapsed = time.perf_counter() - start

        # Get final memory
        mem_after = process.memory_info().rss / 1024 / 1024  # MB
        mem_delta = mem_after - mem_before

        # Calculate metrics
        throughput = count / elapsed if elapsed > 0 else 0
        avg_time_ms = (elapsed * 1000) / count if count > 0 else 0
        cache_rate = harness.stats["cache_hits"] / count if count > 0 else 0

        result = {
            "count": count,
            "total_time_sec": elapsed,
            "avg_time_ms": avg_time_ms,
            "throughput_per_sec": throughput,
            "cache_hit_rate": cache_rate,
            "memory_mb": mem_after,
            "memory_delta_mb": mem_delta,
            "memory_per_oracle_kb": (mem_delta * 1024) / count if count > 0 else 0
        }

        results.append(result)

        print(f"  Time:       {elapsed:.3f} sec")
        print(f"  Throughput: {throughput:.1f} oracles/sec")
        print(f"  Cache rate: {cache_rate*100:.1f}%")
        print(f"  Memory:     {mem_delta:.2f} MB delta")
        print()

    return {
        "benchmark": "scaling",
        "timestamp": datetime.utcnow().isoformat() + "Z",
        "oracle_type": oracle_type.value,
        "results": results,
        "analysis": analyze_scaling(results)
    }


def analyze_scaling(results: List[Dict]) -> Dict[str, Any]:
    """Analyze scaling characteristics.

    Args:
        results: Scaling benchmark results

    Returns:
        Analysis metrics
    """
    if len(results) < 2:
        return {"error": "Insufficient data"}

    # Check if scaling is linear
    # Linear: throughput stays constant
    # Sublinear: throughput decreases

    throughputs = [r["throughput_per_sec"] for r in results]
    first_throughput = throughputs[0]
    last_throughput = throughputs[-1]

    scaling_factor = last_throughput / first_throughput if first_throughput > 0 else 0

    if scaling_factor > 0.9:
        scaling_type = "linear"
    elif scaling_factor > 0.5:
        scaling_type = "sublinear"
    else:
        scaling_type = "poor"

    # Memory growth
    mem_per_oracle = [r["memory_per_oracle_kb"] for r in results if r["count"] > 10]
    avg_mem_per_oracle = sum(mem_per_oracle) / len(mem_per_oracle) if mem_per_oracle else 0

    return {
        "scaling_type": scaling_type,
        "scaling_factor": scaling_factor,
        "throughput_degradation_pct": (1 - scaling_factor) * 100,
        "avg_memory_per_oracle_kb": avg_mem_per_oracle,
        "max_throughput": max(throughputs),
        "min_throughput": min(throughputs)
    }


def run_benchmarks() -> Dict[str, Any]:
    """Run scaling benchmarks."""
    print("=" * 70)
    print("Scaling Benchmarks")
    print("=" * 70)
    print()

    harness = ProductionHarness(enable_real_llm=False)

    # Test with arithmetic (fast oracle) to see pure scaling
    oracle_counts = [1, 10, 100, 1000, 10000]

    results = benchmark_scale(harness, oracle_counts)

    # Print analysis
    print("=" * 70)
    print("Analysis")
    print("=" * 70)
    print()

    analysis = results["analysis"]
    print(f"Scaling type:          {analysis['scaling_type']}")
    print(f"Scaling factor:        {analysis['scaling_factor']:.3f}")
    print(f"Throughput degradation: {analysis['throughput_degradation_pct']:.1f}%")
    print(f"Memory per oracle:     {analysis['avg_memory_per_oracle_kb']:.2f} KB")
    print()

    # Target: 1000 oracles in < 60 seconds
    result_1000 = next((r for r in results["results"] if r["count"] == 1000), None)
    if result_1000:
        target_time = 60.0
        actual_time = result_1000["total_time_sec"]
        status = "✓" if actual_time < target_time else "✗"
        print(f"{status} 1000 oracles: {actual_time:.2f} sec (target: < {target_time} sec)")

    # Target: 10000 oracles in < 600 seconds
    result_10000 = next((r for r in results["results"] if r["count"] == 10000), None)
    if result_10000:
        target_time = 600.0
        actual_time = result_10000["total_time_sec"]
        status = "✓" if actual_time < target_time else "✗"
        print(f"{status} 10000 oracles: {actual_time:.2f} sec (target: < {target_time} sec)")

    print()

    return results


def main():
    """Main entry point."""
    results = run_benchmarks()

    # Save results
    output_file = Path(__file__).parent / "results_scaling.json"
    with open(output_file, "w") as f:
        json.dump(results, f, indent=2)

    print(f"Results saved to: {output_file}")
    print()

    return 0


if __name__ == "__main__":
    exit(main())
