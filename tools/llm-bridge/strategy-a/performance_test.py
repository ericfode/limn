#!/usr/bin/env python3
"""
Performance Analysis for Strategy A
====================================

Measures overhead of:
1. Bend compilation
2. HVM execution
3. Process spawning
4. Output parsing
5. Total roundtrip time

Author: Rex (Engineer)
Date: 2026-02-01
"""

import subprocess
import time
from pathlib import Path
from typing import Dict, Any


def measure_compilation(bend_binary: str, bend_file: Path) -> Dict[str, Any]:
    """Measure Bend compilation time.

    Args:
        bend_binary: Path to bend
        bend_file: Bend source file

    Returns:
        Timing metrics
    """
    times = []

    # Run multiple times for average
    for _ in range(5):
        start = time.perf_counter()

        result = subprocess.run(
            [bend_binary, "gen-hvm", str(bend_file)],
            capture_output=True,
            timeout=10
        )

        elapsed = time.perf_counter() - start

        if result.returncode == 0:
            times.append(elapsed)

    if not times:
        return {"error": "Compilation failed"}

    return {
        "min": min(times) * 1000,  # ms
        "max": max(times) * 1000,
        "avg": sum(times) / len(times) * 1000,
        "samples": len(times)
    }


def measure_execution(bend_binary: str, bend_file: Path) -> Dict[str, Any]:
    """Measure HVM execution time.

    Args:
        bend_binary: Path to bend
        bend_file: Bend source file

    Returns:
        Timing metrics
    """
    times = []

    for _ in range(5):
        start = time.perf_counter()

        result = subprocess.run(
            [bend_binary, "run-rs", str(bend_file)],
            capture_output=True,
            timeout=10
        )

        elapsed = time.perf_counter() - start

        if result.returncode == 0:
            times.append(elapsed)

    if not times:
        return {"error": "Execution failed"}

    return {
        "min": min(times) * 1000,
        "max": max(times) * 1000,
        "avg": sum(times) / len(times) * 1000,
        "samples": len(times)
    }


def measure_roundtrip(bend_binary: str, bend_file: Path) -> Dict[str, Any]:
    """Measure full roundtrip (compile + execute + parse).

    Args:
        bend_binary: Path to bend
        bend_file: Bend source file

    Returns:
        Timing metrics
    """
    times = []

    for _ in range(5):
        start = time.perf_counter()

        # Full pipeline
        # 1. Compile
        subprocess.run(
            [bend_binary, "gen-hvm", str(bend_file)],
            capture_output=True,
            timeout=10
        )

        # 2. Execute
        result = subprocess.run(
            [bend_binary, "run-rs", str(bend_file)],
            capture_output=True,
            timeout=10
        )

        # 3. Parse (simple regex)
        if result.returncode == 0:
            output = result.stdout.decode()
            _ = "OracleRequest/Ask" in output

        elapsed = time.perf_counter() - start
        times.append(elapsed)

    return {
        "min": min(times) * 1000,
        "max": max(times) * 1000,
        "avg": sum(times) / len(times) * 1000,
        "samples": len(times)
    }


def main():
    """Run performance analysis."""
    print("=" * 70)
    print("Strategy A Performance Analysis")
    print("=" * 70)
    print()

    script_dir = Path(__file__).parent
    bend_binary = script_dir.parent.parent / "lmn-bend" / "bend"
    example_file = script_dir / "oracle_example.bend"

    if not bend_binary.exists():
        print(f"Error: Bend binary not found at {bend_binary}")
        return 1

    if not example_file.exists():
        print(f"Error: Example file not found at {example_file}")
        return 1

    # Test 1: Compilation time
    print("[Perf] Measuring compilation time...")
    comp_metrics = measure_compilation(str(bend_binary), example_file)
    print(f"  Min:  {comp_metrics['min']:.2f} ms")
    print(f"  Max:  {comp_metrics['max']:.2f} ms")
    print(f"  Avg:  {comp_metrics['avg']:.2f} ms")
    print()

    # Test 2: Execution time
    print("[Perf] Measuring execution time...")
    exec_metrics = measure_execution(str(bend_binary), example_file)
    print(f"  Min:  {exec_metrics['min']:.2f} ms")
    print(f"  Max:  {exec_metrics['max']:.2f} ms")
    print(f"  Avg:  {exec_metrics['avg']:.2f} ms")
    print()

    # Test 3: Full roundtrip
    print("[Perf] Measuring full roundtrip...")
    round_metrics = measure_roundtrip(str(bend_binary), example_file)
    print(f"  Min:  {round_metrics['min']:.2f} ms")
    print(f"  Max:  {round_metrics['max']:.2f} ms")
    print(f"  Avg:  {round_metrics['avg']:.2f} ms")
    print()

    # Analysis
    print("=" * 70)
    print("Analysis")
    print("=" * 70)
    print()

    overhead = round_metrics['avg']

    print(f"Process overhead: ~{overhead:.0f} ms per oracle call")
    print()

    print("Implications:")
    if overhead < 50:
        print("  ✓ Very fast - suitable for interactive use")
    elif overhead < 200:
        print("  ✓ Acceptable - usable for most applications")
    elif overhead < 500:
        print("  ~ Moderate - might be noticeable in tight loops")
    else:
        print("  ✗ High - consider optimization or alternative strategy")

    print()
    print("Optimization opportunities:")
    print("  - Keep HVM process running (persistent)")
    print("  - Use stdin/stdout for communication")
    print("  - Batch multiple oracle requests")
    print("  - Cache oracle responses (already implemented)")
    print("  - Consider Strategy B (direct embedding)")

    print()
    print("[Perf] Performance analysis complete.")

    return 0


if __name__ == "__main__":
    exit(main())
