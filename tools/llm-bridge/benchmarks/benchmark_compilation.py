#!/usr/bin/env python3
"""
Bend Compilation Benchmarks
============================

Measures Bend compilation overhead.

Measures:
- Compilation time vs program size
- Cached compilation benefits
- Compilation bottlenecks

Author: Performance Testing Suite
Date: 2026-02-01
"""

import sys
import subprocess
import time
import json
import tempfile
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Any

# Add parent to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))


def generate_bend_program(oracle_count: int) -> str:
    """Generate a Bend program with specified number of oracles.

    Args:
        oracle_count: Number of oracle calls to include

    Returns:
        Bend source code
    """
    lines = [
        "def main():",
        "  # Test program with multiple oracle calls"
    ]

    for i in range(oracle_count):
        lines.append(f"  let x{i} = Oracle/Arith(add, {i}, {i+1})")

    lines.append(f"  x{oracle_count-1}")

    return "\n".join(lines)


def benchmark_compilation(
    bend_binary: str,
    program_sizes: List[int]
) -> List[Dict[str, Any]]:
    """Benchmark compilation times for different program sizes.

    Args:
        bend_binary: Path to bend executable
        program_sizes: List of oracle counts to test

    Returns:
        Compilation metrics
    """
    results = []

    for size in program_sizes:
        print(f"[Compile] Program with {size} oracles...")

        # Generate program
        source = generate_bend_program(size)
        program_chars = len(source)

        # Write to temp file
        with tempfile.NamedTemporaryFile(mode='w', suffix='.bend', delete=False) as f:
            f.write(source)
            temp_file = Path(f.name)

        try:
            # Measure compilation (cold)
            times_cold = []
            for _ in range(5):
                start = time.perf_counter()
                result = subprocess.run(
                    [bend_binary, "gen-hvm", str(temp_file)],
                    capture_output=True,
                    timeout=30
                )
                elapsed = (time.perf_counter() - start) * 1000  # ms

                if result.returncode == 0:
                    times_cold.append(elapsed)

            # Measure compilation (warm - already in system cache)
            times_warm = []
            for _ in range(5):
                start = time.perf_counter()
                result = subprocess.run(
                    [bend_binary, "gen-hvm", str(temp_file)],
                    capture_output=True,
                    timeout=30
                )
                elapsed = (time.perf_counter() - start) * 1000  # ms

                if result.returncode == 0:
                    times_warm.append(elapsed)

            if times_cold and times_warm:
                avg_cold = sum(times_cold) / len(times_cold)
                avg_warm = sum(times_warm) / len(times_warm)
                speedup = avg_cold / avg_warm if avg_warm > 0 else 1.0

                result = {
                    "oracle_count": size,
                    "program_chars": program_chars,
                    "compile_cold_ms": avg_cold,
                    "compile_warm_ms": avg_warm,
                    "cache_speedup": speedup,
                    "chars_per_ms": program_chars / avg_cold if avg_cold > 0 else 0
                }

                results.append(result)

                print(f"  Cold:    {avg_cold:.2f} ms")
                print(f"  Warm:    {avg_warm:.2f} ms")
                print(f"  Speedup: {speedup:.2f}x")
                print()

        finally:
            temp_file.unlink()

    return results


def run_benchmarks() -> Dict[str, Any]:
    """Run compilation benchmarks."""
    print("=" * 70)
    print("Bend Compilation Benchmarks")
    print("=" * 70)
    print()

    # Find bend binary
    script_dir = Path(__file__).parent
    bend_binary = script_dir.parent.parent / "lmn-bend" / "bend"

    if not bend_binary.exists():
        # Try system bend
        import shutil
        bend_path = shutil.which("bend")
        if not bend_path:
            print("Error: bend binary not found")
            print("  Looked in: ../lmn-bend/bend")
            print("  Looked in: $PATH")
            return {
                "error": "bend binary not found",
                "benchmark": "compilation",
                "timestamp": datetime.utcnow().isoformat() + "Z"
            }
        bend_binary = bend_path

    print(f"Using bend: {bend_binary}")
    print()

    # Test with increasing program sizes
    program_sizes = [1, 10, 50, 100, 500]

    compilation_results = benchmark_compilation(str(bend_binary), program_sizes)

    results = {
        "benchmark": "compilation",
        "timestamp": datetime.utcnow().isoformat() + "Z",
        "bend_binary": str(bend_binary),
        "results": compilation_results
    }

    # Analysis
    print("=" * 70)
    print("Analysis")
    print("=" * 70)
    print()

    if compilation_results:
        # Compilation scaling
        first = compilation_results[0]
        last = compilation_results[-1]

        scaling_factor = (last["compile_cold_ms"] / last["oracle_count"]) / \
                        (first["compile_cold_ms"] / first["oracle_count"])

        print(f"Compilation scaling: {scaling_factor:.2f}x")
        print(f"  (1.0 = linear, <1.0 = sublinear, >1.0 = superlinear)")
        print()

        # Cache effectiveness
        avg_speedup = sum(r["cache_speedup"] for r in compilation_results) / len(compilation_results)
        print(f"Average cache speedup: {avg_speedup:.2f}x")
        print()

        # Throughput
        print("Compilation throughput:")
        for r in compilation_results:
            print(f"  {r['oracle_count']:3d} oracles: {r['chars_per_ms']:.1f} chars/ms")
        print()

    return results


def main():
    """Main entry point."""
    results = run_benchmarks()

    if "error" not in results:
        # Save results
        output_file = Path(__file__).parent / "results_compilation.json"
        with open(output_file, "w") as f:
            json.dump(results, f, indent=2)

        print(f"Results saved to: {output_file}")
        print()
        return 0
    else:
        print(f"Error: {results['error']}")
        return 1


if __name__ == "__main__":
    exit(main())
