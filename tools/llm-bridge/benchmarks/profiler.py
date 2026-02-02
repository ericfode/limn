#!/usr/bin/env python3
"""
Python Harness Profiler
========================

CPU profiling using cProfile to identify bottlenecks.

Outputs:
- Hotspot identification
- Call graph analysis
- Optimization recommendations

Author: Performance Testing Suite
Date: 2026-02-01
"""

import sys
import cProfile
import pstats
import io
import json
from pathlib import Path
from datetime import datetime
from typing import Dict, Any

# Add parent to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from production.harness import ProductionHarness, OracleRequest, OracleType


def profile_workload():
    """Representative workload for profiling."""
    harness = ProductionHarness(enable_real_llm=False)

    # Mix of different oracle types
    oracles = []

    # Fast operations (arithmetic)
    for i in range(500):
        oracles.append(
            OracleRequest(
                type=OracleType.ARITH,
                params={"op": "add", "a": i, "b": i + 1}
            )
        )

    # Medium operations (file checks)
    for i in range(100):
        oracles.append(
            OracleRequest(
                type=OracleType.FILE_EXISTS,
                params={"path": "/tmp"}
            )
        )

    # Slow operations (semantic)
    for i in range(50):
        oracles.append(
            OracleRequest(
                type=OracleType.SEMANTIC,
                params={"prompt": f"test {i}", "context": "benchmark"}
            )
        )

    # Memory operations
    for i in range(100):
        oracles.append(
            OracleRequest(
                type=OracleType.MEMORY_STORE,
                params={"key": f"key_{i}", "value": f"value_{i}"}
            )
        )

    # Execute all
    for oracle in oracles:
        harness.execute_oracle(oracle)

    return len(oracles)


def analyze_profile(stats: pstats.Stats) -> Dict[str, Any]:
    """Analyze profiling stats.

    Args:
        stats: Profile statistics

    Returns:
        Analysis results
    """
    # Capture stats output
    s = io.StringIO()
    stats.stream = s
    stats.sort_stats('cumulative')
    stats.print_stats(30)
    cumulative_report = s.getvalue()

    s = io.StringIO()
    stats.stream = s
    stats.sort_stats('time')
    stats.print_stats(30)
    time_report = s.getvalue()

    # Get top functions
    top_cumulative = []
    top_time = []

    stats.sort_stats('cumulative')
    for func, (cc, nc, tt, ct, callers) in list(stats.stats.items())[:10]:
        top_cumulative.append({
            "function": f"{func[0]}:{func[1]}:{func[2]}",
            "calls": nc,
            "cumulative_time": ct
        })

    stats.sort_stats('time')
    for func, (cc, nc, tt, ct, callers) in list(stats.stats.items())[:10]:
        top_time.append({
            "function": f"{func[0]}:{func[1]}:{func[2]}",
            "calls": nc,
            "self_time": tt
        })

    return {
        "top_cumulative": top_cumulative,
        "top_time": top_time,
        "cumulative_report": cumulative_report,
        "time_report": time_report
    }


def generate_recommendations(analysis: Dict[str, Any]) -> List[str]:
    """Generate optimization recommendations.

    Args:
        analysis: Profile analysis

    Returns:
        List of recommendations
    """
    recommendations = []

    # Check for obvious bottlenecks
    top_funcs = [f["function"] for f in analysis["top_time"]]

    if any("sqlite" in f.lower() for f in top_funcs):
        recommendations.append(
            "DATABASE: SQLite operations in hot path. Consider batching or connection pooling."
        )

    if any("regex" in f.lower() or "re.py" in f for f in top_funcs):
        recommendations.append(
            "REGEX: Regular expressions in hot path. Consider precompiling patterns."
        )

    if any("json" in f.lower() for f in top_funcs):
        recommendations.append(
            "JSON: JSON serialization overhead. Consider faster alternatives (orjson, ujson)."
        )

    if any("subprocess" in f.lower() for f in top_funcs):
        recommendations.append(
            "SUBPROCESS: Process spawning overhead. Consider keeping processes alive."
        )

    if any("cache" in f.lower() for f in top_funcs):
        recommendations.append(
            "CACHE: Cache operations taking significant time. Review cache implementation."
        )

    if not recommendations:
        recommendations.append(
            "No obvious bottlenecks detected. Profile suggests efficient implementation."
        )

    return recommendations


def run_profiler() -> Dict[str, Any]:
    """Run profiler and generate report."""
    print("=" * 70)
    print("Python Harness Profiler")
    print("=" * 70)
    print()

    print("[Profile] Running workload with cProfile...")

    # Profile the workload
    profiler = cProfile.Profile()
    profiler.enable()

    oracle_count = profile_workload()

    profiler.disable()

    print(f"[Profile] Executed {oracle_count} oracles")
    print()

    # Analyze
    print("[Profile] Analyzing results...")
    stats = pstats.Stats(profiler)
    analysis = analyze_profile(stats)

    print()
    print("=" * 70)
    print("Top Functions by Cumulative Time")
    print("=" * 70)
    print()

    for i, func in enumerate(analysis["top_cumulative"][:10], 1):
        print(f"{i:2d}. {func['function']}")
        print(f"    Calls: {func['calls']}, Cumulative: {func['cumulative_time']:.4f}s")

    print()
    print("=" * 70)
    print("Top Functions by Self Time")
    print("=" * 70)
    print()

    for i, func in enumerate(analysis["top_time"][:10], 1):
        print(f"{i:2d}. {func['function']}")
        print(f"    Calls: {func['calls']}, Self time: {func['self_time']:.4f}s")

    print()
    print("=" * 70)
    print("Optimization Recommendations")
    print("=" * 70)
    print()

    recommendations = generate_recommendations(analysis)
    for i, rec in enumerate(recommendations, 1):
        print(f"{i}. {rec}")

    print()

    results = {
        "benchmark": "profiler",
        "timestamp": datetime.utcnow().isoformat() + "Z",
        "oracle_count": oracle_count,
        "analysis": {
            "top_cumulative": analysis["top_cumulative"],
            "top_time": analysis["top_time"]
        },
        "recommendations": recommendations
    }

    return results


def main():
    """Main entry point."""
    results = run_profiler()

    # Save results (without full reports - those are huge)
    output_file = Path(__file__).parent / "results_profiler.json"
    with open(output_file, "w") as f:
        json.dump(results, f, indent=2)

    print(f"Results saved to: {output_file}")
    print()

    return 0


if __name__ == "__main__":
    exit(main())
