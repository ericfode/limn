#!/usr/bin/env python3
"""
Memory Profiling Benchmarks
============================

Profiles memory usage patterns for oracle execution.

Measures:
- Peak memory per oracle type
- Memory leak detection
- Cache memory overhead

Author: Performance Testing Suite
Date: 2026-02-01
"""

import sys
import gc
import time
import json
import psutil
import os
from pathlib import Path
from datetime import datetime
from typing import Dict, Any

# Add parent to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from production.harness import ProductionHarness, OracleRequest, OracleType


def profile_oracle_memory(
    harness: ProductionHarness,
    oracle_type: OracleType,
    params: Dict[str, Any],
    iterations: int = 1000
) -> Dict[str, float]:
    """Profile memory usage for a specific oracle type.

    Args:
        harness: Production harness instance
        oracle_type: Oracle type to profile
        params: Parameters for the oracle
        iterations: Number of iterations

    Returns:
        Memory metrics
    """
    process = psutil.Process(os.getpid())

    # Force garbage collection
    gc.collect()
    time.sleep(0.1)

    # Get baseline
    mem_before = process.memory_info().rss / 1024 / 1024  # MB

    # Execute oracles
    for i in range(iterations):
        oracle = OracleRequest(type=oracle_type, params=params)
        harness.execute_oracle(oracle)

    # Force garbage collection again
    gc.collect()
    time.sleep(0.1)

    # Get final memory
    mem_after = process.memory_info().rss / 1024 / 1024  # MB
    mem_delta = mem_after - mem_before

    # Check for leaks by running again
    gc.collect()
    mem_check = process.memory_info().rss / 1024 / 1024  # MB

    return {
        "iterations": iterations,
        "memory_before_mb": mem_before,
        "memory_after_mb": mem_after,
        "memory_delta_mb": mem_delta,
        "memory_per_oracle_kb": (mem_delta * 1024) / iterations,
        "potential_leak_mb": max(0, mem_check - mem_after),
        "cache_entries": len(harness._cache_get.__self__.cache_db.name) if hasattr(harness, '_cache_get') else 0
    }


def profile_cache_overhead(harness: ProductionHarness) -> Dict[str, Any]:
    """Measure cache memory overhead.

    Args:
        harness: Production harness instance

    Returns:
        Cache metrics
    """
    process = psutil.Process(os.getpid())

    # Clear cache
    import shutil
    if harness.cache_dir.exists():
        shutil.rmtree(harness.cache_dir)
    harness._init_cache()

    gc.collect()
    mem_before = process.memory_info().rss / 1024 / 1024  # MB

    # Fill cache with various oracles
    oracle_types = [
        (OracleType.ARITH, {"op": "add", "a": i, "b": i})
        for i in range(1000)
    ]

    for oracle_type, params in oracle_types:
        oracle = OracleRequest(type=oracle_type, params=params)
        harness.execute_oracle(oracle)

    gc.collect()
    mem_after = process.memory_info().rss / 1024 / 1024  # MB

    # Get cache size on disk
    cache_size_mb = sum(
        f.stat().st_size for f in harness.cache_dir.rglob('*') if f.is_file()
    ) / 1024 / 1024

    return {
        "cache_entries": len(oracle_types),
        "memory_overhead_mb": mem_after - mem_before,
        "disk_size_mb": cache_size_mb,
        "memory_per_entry_kb": ((mem_after - mem_before) * 1024) / len(oracle_types)
    }


def run_benchmarks() -> Dict[str, Any]:
    """Run memory profiling benchmarks."""
    print("=" * 70)
    print("Memory Profiling Benchmarks")
    print("=" * 70)
    print()

    harness = ProductionHarness(enable_real_llm=False)

    results = {
        "benchmark": "memory",
        "timestamp": datetime.now(datetime.UTC).isoformat() + "Z",
        "metrics": {}
    }

    # Profile each oracle type
    oracle_configs = {
        "Semantic": {
            "type": OracleType.SEMANTIC,
            "params": {"prompt": "test", "context": "test"}
        },
        "Arith": {
            "type": OracleType.ARITH,
            "params": {"op": "add", "a": 1, "b": 2}
        },
        "MemoryStore": {
            "type": OracleType.MEMORY_STORE,
            "params": {"key": "k", "value": "v"}
        }
    }

    for name, config in oracle_configs.items():
        print(f"[Memory] Profiling {name}...")

        metrics = profile_oracle_memory(
            harness,
            config["type"],
            config["params"],
            iterations=1000
        )

        results["metrics"][name] = metrics

        print(f"  Memory delta:     {metrics['memory_delta_mb']:.2f} MB")
        print(f"  Per oracle:       {metrics['memory_per_oracle_kb']:.2f} KB")
        print(f"  Potential leak:   {metrics['potential_leak_mb']:.2f} MB")
        print()

    # Profile cache overhead
    print("[Memory] Profiling cache overhead...")
    cache_metrics = profile_cache_overhead(harness)
    results["cache_overhead"] = cache_metrics

    print(f"  Entries:          {cache_metrics['cache_entries']}")
    print(f"  Memory overhead:  {cache_metrics['memory_overhead_mb']:.2f} MB")
    print(f"  Disk size:        {cache_metrics['disk_size_mb']:.2f} MB")
    print(f"  Per entry:        {cache_metrics['memory_per_entry_kb']:.2f} KB")
    print()

    # Summary
    print("=" * 70)
    print("Summary")
    print("=" * 70)
    print()

    # Check against target: < 1MB per oracle (critical < 10MB)
    target_kb = 1024  # 1MB
    critical_kb = 10240  # 10MB

    print("Memory per oracle targets (< 1MB target, < 10MB critical):")
    for name, metrics in results["metrics"].items():
        per_oracle = metrics["memory_per_oracle_kb"]
        if per_oracle < target_kb:
            status = "✓"
        elif per_oracle < critical_kb:
            status = "~"
        else:
            status = "✗"
        print(f"  {status} {name:15s}: {per_oracle:>8.2f} KB/oracle")

    # Check for leaks
    print()
    print("Leak detection:")
    for name, metrics in results["metrics"].items():
        leak = metrics["potential_leak_mb"]
        status = "✓" if leak < 1.0 else "⚠"
        print(f"  {status} {name:15s}: {leak:.2f} MB potential leak")

    print()

    return results


def main():
    """Main entry point."""
    results = run_benchmarks()

    # Save results
    output_file = Path(__file__).parent / "results_memory.json"
    with open(output_file, "w") as f:
        json.dump(results, f, indent=2)

    print(f"Results saved to: {output_file}")
    print()

    return 0


if __name__ == "__main__":
    exit(main())
