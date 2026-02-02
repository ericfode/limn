#!/usr/bin/env python3
"""
Load Testing for LMN Oracle System

Tests performance characteristics at scale:
- Thousands of arithmetic oracles (fast path)
- Hundreds of semantic oracles (slow path)
- Mixed workload scenarios
- Performance analysis
"""

import argparse
import time
import sys
import statistics
from typing import List, Tuple, Dict, Any
from dataclasses import dataclass


@dataclass
class OracleCall:
    """Represents a single oracle call"""
    oracle_type: str
    params: Dict[str, Any]
    start_time: float = 0
    end_time: float = 0
    result: Any = None
    error: str = None

    @property
    def duration_ms(self) -> float:
        """Call duration in milliseconds"""
        return (self.end_time - self.start_time) * 1000

    @property
    def success(self) -> bool:
        """Whether call succeeded"""
        return self.error is None


class OracleExecutor:
    """Mock oracle executor for load testing"""

    def __init__(self, enable_delays: bool = True):
        self.enable_delays = enable_delays

    def execute(self, oracle_type: str, params: Dict[str, Any]) -> Any:
        """Execute oracle with realistic timing"""

        # Simulate realistic oracle latencies
        if self.enable_delays:
            if oracle_type == "Arith":
                time.sleep(0.0001)  # 0.1ms - very fast
            elif oracle_type == "TimeNow":
                time.sleep(0.0001)  # 0.1ms
            elif oracle_type == "FileRead":
                time.sleep(0.0005)  # 0.5ms
            elif oracle_type == "Semantic":
                time.sleep(0.5)  # 500ms - slow LLM call
            elif oracle_type == "DbQuery":
                time.sleep(0.01)  # 10ms
            elif oracle_type == "HttpGet":
                time.sleep(0.2)  # 200ms

        # Return mock results
        if oracle_type == "Arith":
            op = params.get("op")
            a = params.get("a", 0)
            b = params.get("b", 0)

            if op == "add":
                return a + b
            elif op == "sub":
                return a - b
            elif op == "mul":
                return a * b
            elif op == "div":
                return a / b if b != 0 else None

        elif oracle_type == "Semantic":
            return f"Mock LLM response"

        elif oracle_type == "TimeNow":
            return int(time.time())

        elif oracle_type == "FileRead":
            return "Mock file content"

        elif oracle_type == "DbQuery":
            return [{"id": 1, "name": "test"}]

        elif oracle_type == "HttpGet":
            return {"status": 200, "body": "OK"}

        return None


class LoadTest:
    """Load test scenario"""

    def __init__(self, name: str, executor: OracleExecutor):
        self.name = name
        self.executor = executor
        self.calls: List[OracleCall] = []

    def add_call(self, oracle_type: str, params: Dict[str, Any]) -> None:
        """Add oracle call to test"""
        call = OracleCall(oracle_type=oracle_type, params=params)
        self.calls.append(call)

    def run(self) -> None:
        """Execute all oracle calls"""
        print(f"\nRunning: {self.name}")
        print(f"  Oracle calls: {len(self.calls)}")

        start = time.time()

        for i, call in enumerate(self.calls):
            call.start_time = time.time()

            try:
                call.result = self.executor.execute(call.oracle_type, call.params)
            except Exception as e:
                call.error = str(e)

            call.end_time = time.time()

            # Progress indicator
            if (i + 1) % 1000 == 0:
                print(f"    Progress: {i + 1}/{len(self.calls)}")

        end = time.time()
        total_time = (end - start) * 1000

        print(f"  ✓ Completed in {total_time:.2f}ms")

    def analyze(self) -> Dict[str, Any]:
        """Analyze test results"""
        durations = [c.duration_ms for c in self.calls]
        successes = [c for c in self.calls if c.success]

        return {
            "total_calls": len(self.calls),
            "successes": len(successes),
            "failures": len(self.calls) - len(successes),
            "success_rate": len(successes) / len(self.calls) * 100,
            "total_time_ms": sum(durations),
            "mean_time_ms": statistics.mean(durations),
            "median_time_ms": statistics.median(durations),
            "min_time_ms": min(durations),
            "max_time_ms": max(durations),
            "stddev_ms": statistics.stdev(durations) if len(durations) > 1 else 0,
        }


def create_arithmetic_load_test(executor: OracleExecutor, count: int) -> LoadTest:
    """Create test with many arithmetic oracles (fast path)"""
    test = LoadTest(f"Arithmetic Load Test ({count} oracles)", executor)

    ops = ["add", "sub", "mul", "div"]
    for i in range(count):
        op = ops[i % len(ops)]
        test.add_call("Arith", {"op": op, "a": i, "b": i + 1})

    return test


def create_semantic_load_test(executor: OracleExecutor, count: int) -> LoadTest:
    """Create test with semantic oracles (slow path)"""
    test = LoadTest(f"Semantic Load Test ({count} oracles)", executor)

    prompts = [
        "What is the meaning of life?",
        "Explain quantum mechanics",
        "Write a haiku",
        "Solve this problem"
    ]

    for i in range(count):
        prompt = prompts[i % len(prompts)]
        test.add_call("Semantic", {"prompt": prompt, "context": "general"})

    return test


def create_mixed_workload_test(executor: OracleExecutor, count: int) -> LoadTest:
    """Create test with mixed oracle types"""
    test = LoadTest(f"Mixed Workload Test ({count} oracles)", executor)

    oracle_types = [
        ("Arith", {"op": "add", "a": 5, "b": 3}),
        ("TimeNow", {}),
        ("FileRead", {"path": "/etc/hostname"}),
        ("Semantic", {"prompt": "test", "context": "general"}),
        ("DbQuery", {"sql": "SELECT 1", "db": "test.db"}),
    ]

    for i in range(count):
        oracle_type, params = oracle_types[i % len(oracle_types)]
        test.add_call(oracle_type, params)

    return test


def print_analysis(name: str, analysis: Dict[str, Any]) -> None:
    """Print test analysis"""
    print(f"\n{'='*60}")
    print(f"Analysis: {name}")
    print(f"{'='*60}")
    print(f"Total calls: {analysis['total_calls']}")
    print(f"Success rate: {analysis['success_rate']:.2f}%")
    print(f"\nTiming:")
    print(f"  Total time: {analysis['total_time_ms']:.2f}ms")
    print(f"  Mean time: {analysis['mean_time_ms']:.3f}ms")
    print(f"  Median time: {analysis['median_time_ms']:.3f}ms")
    print(f"  Min time: {analysis['min_time_ms']:.3f}ms")
    print(f"  Max time: {analysis['max_time_ms']:.3f}ms")
    print(f"  Std dev: {analysis['stddev_ms']:.3f}ms")

    # Throughput
    throughput = analysis['total_calls'] / (analysis['total_time_ms'] / 1000)
    print(f"\nThroughput: {throughput:.1f} oracles/sec")


def main():
    parser = argparse.ArgumentParser(description="Load test LMN oracle system")
    parser.add_argument("--oracles", type=int, default=1000,
                       help="Number of oracles per test")
    parser.add_argument("--no-delays", action="store_true",
                       help="Disable simulated delays (test pure overhead)")
    parser.add_argument("--test", choices=["arithmetic", "semantic", "mixed", "all"],
                       default="all", help="Test scenario to run")

    args = parser.parse_args()

    print("\n" + "="*60)
    print("LMN ORACLE LOAD TESTING")
    print("="*60)

    executor = OracleExecutor(enable_delays=not args.no_delays)

    if args.no_delays:
        print("\n⚠ Running with delays disabled (pure overhead)")

    tests = []

    if args.test in ["arithmetic", "all"]:
        tests.append(create_arithmetic_load_test(executor, args.oracles))

    if args.test in ["semantic", "all"]:
        # Use fewer semantic oracles (they're slow)
        count = min(args.oracles, 100)
        tests.append(create_semantic_load_test(executor, count))

    if args.test in ["mixed", "all"]:
        tests.append(create_mixed_workload_test(executor, args.oracles))

    # Run tests
    for test in tests:
        test.run()

    # Analyze results
    print("\n" + "="*60)
    print("RESULTS")
    print("="*60)

    for test in tests:
        analysis = test.analyze()
        print_analysis(test.name, analysis)

    # Overall summary
    print("\n" + "="*60)
    print("SUMMARY")
    print("="*60)

    total_calls = sum(len(t.calls) for t in tests)
    total_time = sum(t.analyze()["total_time_ms"] for t in tests)
    overall_throughput = total_calls / (total_time / 1000)

    print(f"Total oracles executed: {total_calls}")
    print(f"Total time: {total_time:.2f}ms")
    print(f"Overall throughput: {overall_throughput:.1f} oracles/sec")

    # Performance assessment
    print("\nPerformance assessment:")
    if overall_throughput > 1000:
        print("  ✓ EXCELLENT - >1000 oracles/sec")
    elif overall_throughput > 100:
        print("  ✓ GOOD - >100 oracles/sec")
    elif overall_throughput > 10:
        print("  ⚠ MODERATE - >10 oracles/sec")
    else:
        print("  ✗ POOR - <10 oracles/sec")

    return 0


if __name__ == "__main__":
    sys.exit(main())
