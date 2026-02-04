#!/usr/bin/env python3
"""
Benchmark Orchestrator
======================

Runs all benchmarks and generates comprehensive report.

Author: Performance Testing Suite
Date: 2026-02-01
"""

import sys
import json
import time
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Any

# Import all benchmarks
import benchmark_oracle_execution
import benchmark_scaling
import benchmark_memory
import benchmark_compilation
import benchmark_concurrency
import profiler


def run_all_benchmarks(skip_slow: bool = False) -> Dict[str, Any]:
    """Run all benchmarks.

    Args:
        skip_slow: Skip slow benchmarks (compilation, large scaling)

    Returns:
        Aggregate results
    """
    results = {
        "run_timestamp": datetime.now(datetime.UTC).isoformat() + "Z",
        "benchmarks": {}
    }

    benchmarks = [
        ("oracle_execution", benchmark_oracle_execution.run_benchmarks),
        ("scaling", benchmark_scaling.run_benchmarks),
        ("memory", benchmark_memory.run_benchmarks),
        ("concurrency", benchmark_concurrency.run_benchmarks),
        ("profiler", profiler.run_profiler),
    ]

    if not skip_slow:
        benchmarks.append(("compilation", benchmark_compilation.run_benchmarks))

    total_time = 0

    for name, benchmark_func in benchmarks:
        print()
        print("█" * 70)
        print(f"Running: {name}")
        print("█" * 70)
        print()

        start = time.time()

        try:
            result = benchmark_func()
            results["benchmarks"][name] = result
            elapsed = time.time() - start
            total_time += elapsed

            print()
            print(f"✓ {name} complete ({elapsed:.1f}s)")
            print()

        except Exception as e:
            print()
            print(f"✗ {name} failed: {e}")
            print()
            results["benchmarks"][name] = {
                "error": str(e),
                "benchmark": name
            }

    results["total_time_sec"] = total_time

    return results


def generate_summary_report(results: Dict[str, Any]) -> str:
    """Generate human-readable summary report.

    Args:
        results: Aggregate benchmark results

    Returns:
        Formatted report
    """
    lines = []

    lines.append("=" * 70)
    lines.append("PERFORMANCE BENCHMARK SUMMARY")
    lines.append("=" * 70)
    lines.append("")
    lines.append(f"Run timestamp: {results['run_timestamp']}")
    lines.append(f"Total time:    {results['total_time_sec']:.1f} seconds")
    lines.append("")

    # Oracle Execution Summary
    if "oracle_execution" in results["benchmarks"]:
        oracle_results = results["benchmarks"]["oracle_execution"]
        if "metrics" in oracle_results:
            lines.append("Oracle Execution Performance")
            lines.append("-" * 70)

            for oracle_name, metrics in oracle_results["metrics"].items():
                mean = metrics.get("mean_ms", 0)
                p95 = metrics.get("p95_ms", 0)
                throughput = metrics.get("throughput_per_sec", 0)

                lines.append(f"  {oracle_name:15s}: {mean:>8.3f} ms avg, {p95:>8.3f} ms p95, {throughput:>8.1f} ops/s")

            lines.append("")

    # Scaling Summary
    if "scaling" in results["benchmarks"]:
        scaling_results = results["benchmarks"]["scaling"]
        if "analysis" in scaling_results:
            analysis = scaling_results["analysis"]
            lines.append("Scaling Characteristics")
            lines.append("-" * 70)
            lines.append(f"  Scaling type:       {analysis['scaling_type']}")
            lines.append(f"  Throughput degrad:  {analysis['throughput_degradation_pct']:.1f}%")
            lines.append(f"  Memory/oracle:      {analysis['avg_memory_per_oracle_kb']:.2f} KB")
            lines.append("")

    # Memory Summary
    if "memory" in results["benchmarks"]:
        memory_results = results["benchmarks"]["memory"]
        if "metrics" in memory_results:
            lines.append("Memory Usage")
            lines.append("-" * 70)

            for oracle_name, metrics in memory_results["metrics"].items():
                per_oracle = metrics.get("memory_per_oracle_kb", 0)
                lines.append(f"  {oracle_name:15s}: {per_oracle:>8.2f} KB/oracle")

            lines.append("")

    # Concurrency Summary
    if "concurrency" in results["benchmarks"]:
        concurrency_results = results["benchmarks"]["concurrency"]
        if "concurrency_results" in concurrency_results:
            best = max(
                concurrency_results["concurrency_results"],
                key=lambda x: x["throughput_per_sec"]
            )

            lines.append("Concurrency")
            lines.append("-" * 70)
            lines.append(f"  Best throughput:    {best['throughput_per_sec']:.1f} oracles/sec ({best['num_threads']} threads)")

            if "thread_safety" in concurrency_results:
                safety = concurrency_results["thread_safety"]
                status = "PASS" if safety["is_thread_safe"] else "FAIL"
                lines.append(f"  Thread safety:      {status}")

            lines.append("")

    # Profiler Summary
    if "profiler" in results["benchmarks"]:
        profiler_results = results["benchmarks"]["profiler"]
        if "recommendations" in profiler_results:
            lines.append("Optimization Recommendations")
            lines.append("-" * 70)

            for i, rec in enumerate(profiler_results["recommendations"], 1):
                lines.append(f"  {i}. {rec}")

            lines.append("")

    # Overall Assessment
    lines.append("=" * 70)
    lines.append("OVERALL ASSESSMENT")
    lines.append("=" * 70)
    lines.append("")

    # Check key metrics against targets
    assessments = []

    # Oracle execution targets
    if "oracle_execution" in results["benchmarks"]:
        oracle_results = results["benchmarks"]["oracle_execution"]
        if "metrics" in oracle_results:
            semantic_mean = oracle_results["metrics"].get("Semantic", {}).get("mean_ms", 0)
            if semantic_mean > 0:
                if semantic_mean < 200:
                    assessments.append("✓ Semantic oracle performance: EXCELLENT")
                elif semantic_mean < 500:
                    assessments.append("~ Semantic oracle performance: ACCEPTABLE")
                else:
                    assessments.append("✗ Semantic oracle performance: NEEDS OPTIMIZATION")

            arith_mean = oracle_results["metrics"].get("Arith", {}).get("mean_ms", 0)
            if arith_mean > 0:
                if arith_mean < 1:
                    assessments.append("✓ Arithmetic oracle performance: EXCELLENT")
                elif arith_mean < 5:
                    assessments.append("~ Arithmetic oracle performance: ACCEPTABLE")
                else:
                    assessments.append("✗ Arithmetic oracle performance: NEEDS OPTIMIZATION")

    # Scaling targets
    if "scaling" in results["benchmarks"]:
        scaling_results = results["benchmarks"]["scaling"]
        if "results" in scaling_results:
            result_1000 = next(
                (r for r in scaling_results["results"] if r["count"] == 1000),
                None
            )
            if result_1000:
                time_1000 = result_1000["total_time_sec"]
                if time_1000 < 60:
                    assessments.append("✓ 1000 oracle throughput: EXCELLENT")
                elif time_1000 < 120:
                    assessments.append("~ 1000 oracle throughput: ACCEPTABLE")
                else:
                    assessments.append("✗ 1000 oracle throughput: NEEDS OPTIMIZATION")

    # Memory targets
    if "memory" in results["benchmarks"]:
        memory_results = results["benchmarks"]["memory"]
        if "metrics" in memory_results:
            max_mem = max(
                m.get("memory_per_oracle_kb", 0)
                for m in memory_results["metrics"].values()
            )
            if max_mem < 1024:  # 1MB
                assessments.append("✓ Memory usage: EXCELLENT")
            elif max_mem < 10240:  # 10MB
                assessments.append("~ Memory usage: ACCEPTABLE")
            else:
                assessments.append("✗ Memory usage: NEEDS OPTIMIZATION")

    # Thread safety
    if "concurrency" in results["benchmarks"]:
        concurrency_results = results["benchmarks"]["concurrency"]
        if "thread_safety" in concurrency_results:
            if concurrency_results["thread_safety"]["is_thread_safe"]:
                assessments.append("✓ Thread safety: PASS")
            else:
                assessments.append("✗ Thread safety: FAIL - CRITICAL")

    for assessment in assessments:
        lines.append(assessment)

    lines.append("")
    lines.append("=" * 70)

    return "\n".join(lines)


def main():
    """Main entry point."""
    import argparse

    parser = argparse.ArgumentParser(description="Run all performance benchmarks")
    parser.add_argument("--skip-slow", action="store_true", help="Skip slow benchmarks")
    args = parser.parse_args()

    print("=" * 70)
    print("LMN Oracle Performance Benchmark Suite")
    print("=" * 70)
    print()

    if args.skip_slow:
        print("⚠  Running in fast mode (skipping slow benchmarks)")
        print()

    # Run all benchmarks
    results = run_all_benchmarks(skip_slow=args.skip_slow)

    # Generate summary report
    summary = generate_summary_report(results)

    print()
    print()
    print(summary)

    # Save results
    output_dir = Path(__file__).parent
    results_file = output_dir / "results_all.json"
    summary_file = output_dir / "BENCHMARK_REPORT.txt"

    with open(results_file, "w") as f:
        json.dump(results, f, indent=2)

    with open(summary_file, "w") as f:
        f.write(summary)

    print()
    print(f"Detailed results: {results_file}")
    print(f"Summary report:   {summary_file}")
    print()

    return 0


if __name__ == "__main__":
    exit(main())
