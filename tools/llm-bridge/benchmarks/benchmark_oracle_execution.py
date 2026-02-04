#!/usr/bin/env python3
"""
Oracle Execution Benchmarks
============================

Measures execution times for all oracle types.

Author: Performance Testing Suite
Date: 2026-02-01
"""

import sys
import time
import json
import statistics
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Any

# Add parent to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from production.harness import ProductionHarness, OracleRequest, OracleType


def benchmark_oracle_type(
    harness: ProductionHarness,
    oracle_type: OracleType,
    params: Dict[str, Any],
    iterations: int = 100
) -> Dict[str, float]:
    """Benchmark a specific oracle type.

    Args:
        harness: Production harness instance
        oracle_type: Oracle type to benchmark
        params: Parameters for the oracle
        iterations: Number of iterations

    Returns:
        Performance metrics
    """
    times = []
    cache_hits = 0

    # Clear cache for first run
    harness.stats = {
        "total_oracles": 0,
        "cache_hits": 0,
        "total_time_ms": 0
    }

    for i in range(iterations):
        oracle = OracleRequest(type=oracle_type, params=params)

        start = time.perf_counter()
        response = harness.execute_oracle(oracle)
        elapsed = (time.perf_counter() - start) * 1000  # ms

        if response.success:
            times.append(elapsed)
            if response.cached:
                cache_hits += 1

    if not times:
        return {"error": "All executions failed"}

    times_sorted = sorted(times)

    return {
        "count": len(times),
        "min_ms": min(times),
        "max_ms": max(times),
        "mean_ms": statistics.mean(times),
        "median_ms": statistics.median(times),
        "stdev_ms": statistics.stdev(times) if len(times) > 1 else 0,
        "p95_ms": times_sorted[int(len(times) * 0.95)],
        "p99_ms": times_sorted[int(len(times) * 0.99)],
        "cache_hit_rate": cache_hits / len(times),
        "throughput_per_sec": 1000 / statistics.mean(times) if times else 0
    }


def run_benchmarks() -> Dict[str, Any]:
    """Run all oracle execution benchmarks."""
    print("=" * 70)
    print("Oracle Execution Benchmarks")
    print("=" * 70)
    print()

    harness = ProductionHarness(enable_real_llm=False)

    # Define benchmarks for each oracle type
    benchmarks = {
        "Semantic": {
            "type": OracleType.SEMANTIC,
            "params": {"prompt": "What is 2+2?", "context": "math"}
        },
        "Arith": {
            "type": OracleType.ARITH,
            "params": {"op": "add", "a": 42, "b": 13}
        },
        "FileExists": {
            "type": OracleType.FILE_EXISTS,
            "params": {"path": "/tmp"}
        },
        "TimeNow": {
            "type": OracleType.TIME_NOW,
            "params": {}
        },
        "MemoryStore": {
            "type": OracleType.MEMORY_STORE,
            "params": {"key": "test", "value": "benchmark"}
        },
        "MemoryRetrieve": {
            "type": OracleType.MEMORY_RETRIEVE,
            "params": {"key": "test"}
        }
    }

    results = {
        "benchmark": "oracle_execution",
        "timestamp": datetime.now(datetime.UTC).isoformat() + "Z",
        "iterations": 100,
        "metrics": {}
    }

    for name, config in benchmarks.items():
        print(f"[Benchmark] {name}...")

        metrics = benchmark_oracle_type(
            harness,
            config["type"],
            config["params"],
            iterations=100
        )

        results["metrics"][name] = metrics

        print(f"  Mean:       {metrics.get('mean_ms', 'N/A'):>8.3f} ms")
        print(f"  Median:     {metrics.get('median_ms', 'N/A'):>8.3f} ms")
        print(f"  P95:        {metrics.get('p95_ms', 'N/A'):>8.3f} ms")
        print(f"  P99:        {metrics.get('p99_ms', 'N/A'):>8.3f} ms")
        print(f"  Throughput: {metrics.get('throughput_per_sec', 0):>8.1f} ops/sec")
        print(f"  Cache rate: {metrics.get('cache_hit_rate', 0)*100:>7.1f}%")
        print()

    # Summary
    print("=" * 70)
    print("Summary")
    print("=" * 70)
    print()

    # Find fastest and slowest
    by_speed = sorted(
        results["metrics"].items(),
        key=lambda x: x[1].get("mean_ms", float('inf'))
    )

    print(f"Fastest: {by_speed[0][0]} ({by_speed[0][1]['mean_ms']:.3f} ms)")
    print(f"Slowest: {by_speed[-1][0]} ({by_speed[-1][1]['mean_ms']:.3f} ms)")
    print()

    # Check against targets
    print("Target Compliance:")
    targets = {
        "Semantic": 200.0,
        "Arith": 1.0,
        "MemoryStore": 1.0,
        "MemoryRetrieve": 1.0,
        "FileExists": 5.0,
        "TimeNow": 1.0
    }

    for oracle, target in targets.items():
        if oracle in results["metrics"]:
            actual = results["metrics"][oracle]["mean_ms"]
            status = "✓" if actual < target else "✗"
            print(f"  {status} {oracle:15s}: {actual:>8.3f} ms (target: {target:>6.1f} ms)")

    print()

    return results


def main():
    """Main entry point."""
    results = run_benchmarks()

    # Save results
    output_file = Path(__file__).parent / "results_oracle_execution.json"
    with open(output_file, "w") as f:
        json.dump(results, f, indent=2)

    print(f"Results saved to: {output_file}")
    print()

    return 0


if __name__ == "__main__":
    exit(main())
