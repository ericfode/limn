#!/usr/bin/env python3
"""
Async Oracle Harness - Parallel Execution
==========================================

Extends ProductionHarness with async/parallel oracle execution.

When multiple independent oracles are requested, execute them
concurrently for massive speedup.

Example:
    Sequential: 5 oracles × 100ms = 500ms
    Parallel:   max(5 × 100ms) = 100ms
    Speedup:    5x

Author: Rex (Engineer)
Date: 2026-02-01
"""

import asyncio
import time
from pathlib import Path
from typing import List, Dict, Any
from concurrent.futures import ThreadPoolExecutor, ProcessPoolExecutor

# Import base harness
import sys
sys.path.insert(0, str(Path(__file__).parent))
from harness import ProductionHarness, OracleRequest, OracleResponse


class AsyncHarness(ProductionHarness):
    """Async/parallel oracle execution harness."""

    def __init__(self, *args, max_workers: int = 8, **kwargs):
        """Initialize with thread pool.

        Args:
            max_workers: Maximum parallel oracle executions
        """
        super().__init__(*args, **kwargs)
        self.max_workers = max_workers
        self.executor = ThreadPoolExecutor(max_workers=max_workers)

    def execute_oracles_parallel(self, oracles: List[OracleRequest]) -> List[OracleResponse]:
        """Execute multiple oracles in parallel.

        Args:
            oracles: List of oracle requests

        Returns:
            List of responses in same order
        """
        if not oracles:
            return []

        # Single oracle - no parallelization needed
        if len(oracles) == 1:
            return [self.execute_oracle(oracles[0])]

        # Execute in parallel
        futures = [
            self.executor.submit(self.execute_oracle, oracle)
            for oracle in oracles
        ]

        # Wait for all and collect results
        responses = [future.result() for future in futures]

        return responses

    def analyze_dependencies(self, oracles: List[OracleRequest]) -> Dict[str, List[str]]:
        """Analyze oracle dependencies for optimal scheduling.

        Args:
            oracles: List of oracle requests

        Returns:
            Dictionary mapping oracle index to list of dependent indices
        """
        dependencies = {}

        # Simple heuristic: assume oracles are independent unless they touch same resource
        # In reality, Bend ensures purity so oracles should be independent

        for i, oracle in enumerate(oracles):
            dependencies[str(i)] = []  # No dependencies by default

            # Special case: file writes to same path
            if oracle.type.value == "FileWrite":
                path = oracle.params.get("path")
                for j in range(i):
                    if j < i and oracles[j].type.value == "FileWrite":
                        if oracles[j].params.get("path") == path:
                            dependencies[str(i)].append(str(j))

            # Special case: memory operations on same key
            if oracle.type.value in ["MemoryStore", "MemoryRetrieve"]:
                key = oracle.params.get("key")
                for j in range(i):
                    if j < i and oracles[j].type.value == "MemoryStore":
                        if oracles[j].params.get("key") == key:
                            dependencies[str(i)].append(str(j))

        return dependencies

    def execute_with_scheduling(self, oracles: List[OracleRequest]) -> List[OracleResponse]:
        """Execute oracles with dependency-aware scheduling.

        Args:
            oracles: List of oracle requests

        Returns:
            List of responses in same order
        """
        if not oracles:
            return []

        if len(oracles) == 1:
            return [self.execute_oracle(oracles[0])]

        # Analyze dependencies
        deps = self.analyze_dependencies(oracles)

        # Build execution waves (batches of independent oracles)
        waves = []
        executed = set()
        remaining = set(range(len(oracles)))

        while remaining:
            # Find oracles with no unexecuted dependencies
            wave = []
            for i in remaining:
                oracle_deps = deps.get(str(i), [])
                if all(int(d) in executed for d in oracle_deps):
                    wave.append(i)

            if not wave:
                # Circular dependency or error - execute remaining in order
                wave = list(remaining)

            # Execute wave in parallel
            wave_requests = [oracles[i] for i in wave]
            wave_responses = self.execute_oracles_parallel(wave_requests)

            # Store results
            for idx, response in zip(wave, wave_responses):
                waves.append((idx, response))
                executed.add(idx)
                remaining.remove(idx)

        # Sort by original order and return
        waves.sort(key=lambda x: x[0])
        return [response for _, response in waves]

    def execute_async(self, bend_file: Path, verbose: bool = True) -> Dict[str, Any]:
        """Execute with async/parallel oracles.

        Args:
            bend_file: Bend program to execute
            verbose: Print progress

        Returns:
            Execution result with timing
        """
        if verbose:
            print("=" * 70)
            print("Async Oracle Harness (Parallel Execution)")
            print("=" * 70)
            print(f"\n[Conscious] Executing: {bend_file.name}")
            print(f"[Conscious] Max workers: {self.max_workers}")
            print("[Conscious] Running subconscious (Bend/HVM)...\n")

        start = time.time()

        # Run Bend
        result = self.run_bend(bend_file)

        if not result["success"]:
            if verbose:
                print(f"[Conscious] ✗ Execution failed: {result.get('error', 'Unknown')}")
            return result

        # Parse oracles
        oracles = self.parse_oracles(result["stdout"])

        if not oracles:
            if verbose:
                print("[Conscious] ✓ Subconscious completed (no oracles)")
            return {**result, "oracles": [], "timing": {"total": (time.time() - start) * 1000}}

        if verbose:
            print(f"[Conscious] ✓ Subconscious completed")
            print(f"[Conscious] Found {len(oracles)} oracle(s)\n")

        # Execute with scheduling
        oracle_start = time.time()
        responses = self.execute_with_scheduling(oracles)
        oracle_time = (time.time() - oracle_start) * 1000

        # Report results
        if verbose:
            for i, (oracle, response) in enumerate(zip(oracles, responses), 1):
                status = "✓" if response.success else "✗"
                cached = " (cached)" if response.cached else ""
                print(f"[Conscious] {status} Oracle {i}/{len(oracles)}: {oracle.type.value}{cached}")
                if not response.success:
                    print(f"    Error: {response.error}")

            print(f"\n[Conscious] Total oracle time: {oracle_time:.2f}ms")
            print(f"[Conscious] Average per oracle: {oracle_time/len(oracles):.2f}ms")

            # Sequential estimate
            sequential_estimate = sum(r.duration_ms for r in responses)
            speedup = sequential_estimate / oracle_time if oracle_time > 0 else 1
            print(f"[Conscious] Sequential estimate: {sequential_estimate:.2f}ms")
            print(f"[Conscious] Speedup: {speedup:.2f}x")

        total_time = (time.time() - start) * 1000

        return {
            **result,
            "oracles": [
                {
                    "type": o.type.value,
                    "params": o.params,
                    "success": r.success,
                    "result": r.result,
                    "duration_ms": r.duration_ms,
                    "cached": r.cached
                }
                for o, r in zip(oracles, responses)
            ],
            "timing": {
                "total": total_time,
                "oracle_execution": oracle_time,
                "speedup": speedup if verbose else None
            }
        }

    def __del__(self):
        """Cleanup thread pool."""
        if hasattr(self, 'executor'):
            self.executor.shutdown(wait=True)


def main():
    """Test async harness."""
    script_dir = Path(__file__).parent

    # Setup
    bend_binary = script_dir.parent.parent / "lmn-bend" / "bend"
    if not bend_binary.exists():
        bend_binary = "bend"

    harness = AsyncHarness(
        bend_binary=str(bend_binary),
        enable_real_llm=False,
        max_workers=8
    )

    # Test with complete demo (12 oracles)
    demo_file = script_dir / "demo_complete.bend"
    if not demo_file.exists():
        demo_file = script_dir / "demo_interesting.bend"

    if not demo_file.exists():
        print("No demo file found")
        return 1

    print("Testing async execution...\n")
    result = harness.execute_async(demo_file)

    if result["success"]:
        print("\n" + "=" * 70)
        print("SUCCESS")
        print("=" * 70)
        timing = result.get("timing", {})
        print(f"Total time: {timing.get('total', 0):.2f}ms")
        print(f"Oracle time: {timing.get('oracle_execution', 0):.2f}ms")
        if timing.get('speedup'):
            print(f"Speedup: {timing['speedup']:.2f}x")
        return 0
    else:
        print("\n" + "=" * 70)
        print("FAILED")
        print("=" * 70)
        return 1


if __name__ == "__main__":
    exit(main())
