#!/usr/bin/env python3
"""
benchmark.py - Performance measurement for external process LLM calls

This script simulates the overhead of calling external LLM processes
to estimate performance characteristics of Strategy B.

Measurements:
- Process spawn overhead
- Single call latency
- Multiple calls throughput
- Memory overhead
"""

import subprocess
import time
import statistics
import sys
from pathlib import Path

MOCK_LLM = Path(__file__).parent / "mock-llm"

def measure_process_spawn():
    """Measure overhead of spawning a process"""
    times = []

    for _ in range(10):
        start = time.perf_counter()
        subprocess.run([str(MOCK_LLM), "test"], capture_output=True)
        end = time.perf_counter()
        times.append((end - start) * 1000)  # Convert to ms

    return {
        "mean": statistics.mean(times),
        "median": statistics.median(times),
        "stdev": statistics.stdev(times) if len(times) > 1 else 0,
        "min": min(times),
        "max": max(times),
    }

def measure_single_call():
    """Measure single LLM call latency"""
    query = "What is the airspeed velocity of an unladen swallow?"

    start = time.perf_counter()
    result = subprocess.run(
        [str(MOCK_LLM), query],
        capture_output=True,
        text=True
    )
    end = time.perf_counter()

    latency_ms = (end - start) * 1000
    response_size = len(result.stdout)

    return {
        "latency_ms": latency_ms,
        "response_size": response_size,
        "response": result.stdout.strip()
    }

def measure_multiple_calls(n=10):
    """Measure multiple sequential calls"""
    times = []

    start_total = time.perf_counter()
    for i in range(n):
        start = time.perf_counter()
        subprocess.run([str(MOCK_LLM), f"Query {i}"], capture_output=True)
        end = time.perf_counter()
        times.append((end - start) * 1000)

    end_total = time.perf_counter()
    total_time = (end_total - start_total) * 1000

    return {
        "total_ms": total_time,
        "avg_ms": statistics.mean(times),
        "median_ms": statistics.median(times),
        "throughput": n / (total_time / 1000),  # calls per second
    }

def measure_memory_overhead():
    """Estimate memory overhead of approach"""
    # Measure process size
    result = subprocess.run(
        [str(MOCK_LLM), "test"],
        capture_output=True
    )

    # Rough estimate based on subprocess overhead
    # In reality, this would require more sophisticated measurement
    return {
        "estimated_per_call_kb": 1024,  # Typical subprocess overhead
        "note": "Actual measurement requires process instrumentation"
    }

def print_section(title):
    """Print section header"""
    print(f"\n{'='*60}")
    print(f"{title}")
    print('='*60)

def main():
    print("=== Strategy B: External Process LLM Calls - Benchmark ===")
    print(f"Mock LLM: {MOCK_LLM}")

    # Check if mock LLM exists
    if not MOCK_LLM.exists():
        print(f"Error: Mock LLM not found at {MOCK_LLM}")
        sys.exit(1)

    # Benchmark 1: Process spawn overhead
    print_section("Benchmark 1: Process Spawn Overhead")
    spawn_stats = measure_process_spawn()
    print(f"Mean:   {spawn_stats['mean']:.2f} ms")
    print(f"Median: {spawn_stats['median']:.2f} ms")
    print(f"Stdev:  {spawn_stats['stdev']:.2f} ms")
    print(f"Min:    {spawn_stats['min']:.2f} ms")
    print(f"Max:    {spawn_stats['max']:.2f} ms")

    # Benchmark 2: Single call latency
    print_section("Benchmark 2: Single LLM Call Latency")
    single = measure_single_call()
    print(f"Latency:       {single['latency_ms']:.2f} ms")
    print(f"Response size: {single['response_size']} bytes")
    print(f"Response:      {single['response'][:80]}...")

    # Benchmark 3: Multiple calls
    print_section("Benchmark 3: Multiple Sequential Calls (n=10)")
    multi = measure_multiple_calls(10)
    print(f"Total time:    {multi['total_ms']:.2f} ms")
    print(f"Average:       {multi['avg_ms']:.2f} ms per call")
    print(f"Median:        {multi['median_ms']:.2f} ms per call")
    print(f"Throughput:    {multi['throughput']:.2f} calls/sec")

    # Benchmark 4: Multiple calls (larger batch)
    print_section("Benchmark 4: Larger Batch (n=50)")
    multi50 = measure_multiple_calls(50)
    print(f"Total time:    {multi50['total_ms']:.2f} ms")
    print(f"Average:       {multi50['avg_ms']:.2f} ms per call")
    print(f"Median:        {multi50['median_ms']:.2f} ms per call")
    print(f"Throughput:    {multi50['throughput']:.2f} calls/sec")

    # Benchmark 5: Memory overhead
    print_section("Benchmark 5: Memory Overhead (Estimated)")
    mem = measure_memory_overhead()
    print(f"Per-call:      ~{mem['estimated_per_call_kb']} KB")
    print(f"Note:          {mem['note']}")

    # Summary and analysis
    print_section("Summary & Analysis")
    print(f"Total overhead per call: ~{single['latency_ms']:.2f} ms")
    print(f"  - Process spawn:       ~{spawn_stats['mean']:.2f} ms")
    print(f"  - LLM processing:      ~{single['latency_ms'] - spawn_stats['mean']:.2f} ms")
    print(f"  - IPC overhead:        ~5-10 ms (estimated)")
    print()
    print("Viability Assessment:")

    # Calculate if this is production-viable
    if single['latency_ms'] < 200:
        print("  ✓ Latency acceptable for interactive use")
    else:
        print("  ✗ Latency too high for interactive use")

    if multi['throughput'] > 5:
        print("  ✓ Throughput acceptable for batch processing")
    else:
        print("  ✗ Throughput too low for batch processing")

    if spawn_stats['mean'] < 50:
        print("  ✓ Process spawn overhead reasonable")
    else:
        print("  ✗ Process spawn overhead high")

    print("\n" + "="*60)

if __name__ == "__main__":
    main()
