#!/usr/bin/env python3
"""
Implementation Comparison Benchmarks
=====================================

Compares different oracle implementation approaches:
- Production harness
- Strategy-A (if available)
- Reference-pure (if available)

Measures performance/complexity tradeoffs.

Author: Performance Testing Suite
Date: 2026-02-01
"""

import sys
import time
import json
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Any, Optional

# Add parent to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from production.harness import ProductionHarness, OracleRequest, OracleType


def benchmark_implementation(
    harness,
    name: str,
    iterations: int = 100
) -> Optional[Dict[str, Any]]:
    """Benchmark a specific implementation.

    Args:
        harness: Harness instance
        name: Implementation name
        iterations: Number of iterations

    Returns:
        Performance metrics or None if unavailable
    """
    print(f"[Compare] Benchmarking {name}...")

    try:
        # Test different oracle types
        test_cases = [
            ("Semantic", OracleType.SEMANTIC, {"prompt": "test", "context": "bench"}),
            ("Arith", OracleType.ARITH, {"op": "add", "a": 1, "b": 2}),
            ("Memory", OracleType.MEMORY_STORE, {"key": "test", "value": "data"}),
        ]

        results = {}

        for test_name, oracle_type, params in test_cases:
            times = []

            for _ in range(iterations):
                oracle = OracleRequest(type=oracle_type, params=params)

                start = time.perf_counter()
                response = harness.execute_oracle(oracle)
                elapsed = (time.perf_counter() - start) * 1000  # ms

                if response.success:
                    times.append(elapsed)

            if times:
                results[test_name] = {
                    "mean_ms": sum(times) / len(times),
                    "min_ms": min(times),
                    "max_ms": max(times)
                }

        print(f"  ✓ {name} benchmarked")
        return results

    except Exception as e:
        print(f"  ✗ {name} failed: {e}")
        return None


def compare_implementations() -> Dict[str, Any]:
    """Compare all available implementations."""
    implementations = {}

    # Production harness (always available)
    print("[Compare] Loading production harness...")
    prod_harness = ProductionHarness(enable_real_llm=False)
    implementations["production"] = benchmark_implementation(prod_harness, "Production")

    # Strategy-A (if available)
    try:
        sys.path.insert(0, str(Path(__file__).parent.parent / "strategy-a"))
        # Import would go here if strategy-a has a harness
        print("[Compare] Strategy-A not yet implemented for comparison")
        implementations["strategy_a"] = None
    except Exception as e:
        print(f"[Compare] Strategy-A unavailable: {e}")
        implementations["strategy_a"] = None

    # Reference-pure (if available)
    try:
        sys.path.insert(0, str(Path(__file__).parent.parent / "reference-pure"))
        # Import would go here if reference-pure has a harness
        print("[Compare] Reference-pure not yet implemented for comparison")
        implementations["reference_pure"] = None
    except Exception as e:
        print(f"[Compare] Reference-pure unavailable: {e}")
        implementations["reference_pure"] = None

    return implementations


def analyze_comparison(implementations: Dict[str, Any]) -> List[str]:
    """Analyze implementation comparison.

    Args:
        implementations: Benchmark results for each implementation

    Returns:
        Analysis insights
    """
    insights = []

    available = {k: v for k, v in implementations.items() if v is not None}

    if len(available) < 2:
        insights.append("Only one implementation available - no comparison possible")
        return insights

    # Compare performance for each oracle type
    for oracle_type in ["Semantic", "Arith", "Memory"]:
        times = {}
        for impl_name, results in available.items():
            if oracle_type in results:
                times[impl_name] = results[oracle_type]["mean_ms"]

        if len(times) > 1:
            fastest = min(times, key=times.get)
            slowest = max(times, key=times.get)
            speedup = times[slowest] / times[fastest]

            insights.append(
                f"{oracle_type}: {fastest} is {speedup:.2f}x faster than {slowest}"
            )

    return insights


def run_benchmarks() -> Dict[str, Any]:
    """Run comparison benchmarks."""
    print("=" * 70)
    print("Implementation Comparison Benchmarks")
    print("=" * 70)
    print()

    implementations = compare_implementations()

    print()
    print("=" * 70)
    print("Results")
    print("=" * 70)
    print()

    for impl_name, results in implementations.items():
        if results:
            print(f"{impl_name}:")
            for oracle_type, metrics in results.items():
                print(f"  {oracle_type:10s}: {metrics['mean_ms']:.3f} ms avg")
            print()

    # Analysis
    insights = analyze_comparison(implementations)

    if insights:
        print("=" * 70)
        print("Analysis")
        print("=" * 70)
        print()

        for insight in insights:
            print(f"  • {insight}")
        print()

    results = {
        "benchmark": "comparison",
        "timestamp": datetime.utcnow().isoformat() + "Z",
        "implementations": implementations,
        "insights": insights
    }

    return results


def main():
    """Main entry point."""
    results = run_benchmarks()

    # Save results
    output_file = Path(__file__).parent / "results_comparison.json"
    with open(output_file, "w") as f:
        json.dump(results, f, indent=2)

    print(f"Results saved to: {output_file}")
    print()

    return 0


if __name__ == "__main__":
    exit(main())
