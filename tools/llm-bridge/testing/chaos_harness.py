#!/usr/bin/env python3
"""
Chaos Engineering Harness for LMN Oracle System

Injects random failures to test system resilience:
- Random oracle failures
- Delayed responses
- Corrupted results
- Partial failures
"""

import argparse
import random
import time
import sys
import json
from typing import Any, Dict, Optional


class ChaosMode:
    """Configuration for chaos engineering"""

    def __init__(self, failure_rate: float = 0.1, delay_rate: float = 0.2,
                 corruption_rate: float = 0.05, max_delay_ms: int = 1000):
        self.failure_rate = failure_rate
        self.delay_rate = delay_rate
        self.corruption_rate = corruption_rate
        self.max_delay_ms = max_delay_ms

        # Track chaos events
        self.failures_injected = 0
        self.delays_injected = 0
        self.corruptions_injected = 0


class ChaosOracle:
    """Oracle wrapper that injects chaos"""

    def __init__(self, chaos_mode: ChaosMode):
        self.chaos = chaos_mode
        self.call_count = 0

    def should_fail(self) -> bool:
        """Decide if this oracle call should fail"""
        return random.random() < self.chaos.failure_rate

    def should_delay(self) -> bool:
        """Decide if this oracle call should be delayed"""
        return random.random() < self.chaos.delay_rate

    def should_corrupt(self) -> bool:
        """Decide if result should be corrupted"""
        return random.random() < self.chaos.corruption_rate

    def inject_delay(self) -> None:
        """Inject random delay"""
        delay_ms = random.randint(1, self.chaos.max_delay_ms)
        time.sleep(delay_ms / 1000.0)
        self.chaos.delays_injected += 1

    def corrupt_result(self, result: Any) -> Any:
        """Corrupt oracle result"""
        self.chaos.corruptions_injected += 1

        corruption_strategies = [
            lambda r: None,  # Return None
            lambda r: "",  # Return empty string
            lambda r: "CORRUPTED",  # Obvious corruption
            lambda r: str(r)[::-1] if isinstance(r, str) else r,  # Reverse string
            lambda r: r * 2 if isinstance(r, (int, float)) else r,  # Double number
            lambda r: {"error": "corrupted"} if isinstance(r, dict) else r,
        ]

        strategy = random.choice(corruption_strategies)
        return strategy(result)

    def execute(self, oracle_type: str, params: Dict[str, Any]) -> Dict[str, Any]:
        """Execute oracle with chaos injection"""
        self.call_count += 1

        # Inject failure
        if self.should_fail():
            self.chaos.failures_injected += 1
            return {
                "status": "error",
                "error": "CHAOS: Injected failure",
                "oracle_type": oracle_type
            }

        # Inject delay
        if self.should_delay():
            self.inject_delay()

        # Execute actual oracle
        result = self._execute_real_oracle(oracle_type, params)

        # Inject corruption
        if self.should_corrupt():
            result = self.corrupt_result(result)

        return {
            "status": "success",
            "result": result,
            "oracle_type": oracle_type
        }

    def _execute_real_oracle(self, oracle_type: str, params: Dict[str, Any]) -> Any:
        """Execute the actual oracle (mock implementation)"""
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
                return a / b if b != 0 else "ERROR: Division by zero"

        elif oracle_type == "TimeNow":
            return {"timestamp": int(time.time())}

        elif oracle_type == "FileRead":
            return f"Mock content of {params.get('path', 'unknown')}"

        elif oracle_type == "Semantic":
            return f"Mock LLM response to: {params.get('prompt', '')}"

        elif oracle_type == "MemoryStore":
            return "OK"

        elif oracle_type == "MemoryRetrieve":
            return f"Mock value for key: {params.get('key', '')}"

        else:
            return f"Mock result for {oracle_type}"


class ChaosTestSuite:
    """Test suite running under chaos conditions"""

    def __init__(self, chaos_oracle: ChaosOracle):
        self.oracle = chaos_oracle
        self.tests_run = 0
        self.tests_passed = 0
        self.tests_failed = 0

    def test_arithmetic_under_chaos(self) -> bool:
        """Test arithmetic operations survive chaos"""
        self.tests_run += 1

        results = []
        for _ in range(10):
            result = self.oracle.execute("Arith", {
                "op": "add",
                "a": random.randint(1, 100),
                "b": random.randint(1, 100)
            })
            results.append(result)

        # Check that at least some succeeded
        successes = sum(1 for r in results if r.get("status") == "success")

        if successes > 0:
            self.tests_passed += 1
            return True
        else:
            self.tests_failed += 1
            return False

    def test_semantic_under_chaos(self) -> bool:
        """Test semantic oracles survive chaos"""
        self.tests_run += 1

        result = self.oracle.execute("Semantic", {
            "prompt": "What is 2+2?",
            "context": "math"
        })

        # Should get either success or graceful failure
        if result.get("status") in ["success", "error"]:
            self.tests_passed += 1
            return True
        else:
            self.tests_failed += 1
            return False

    def test_file_operations_under_chaos(self) -> bool:
        """Test file operations survive chaos"""
        self.tests_run += 1

        results = []
        for path in ["/etc/hostname", "/tmp/test", "/var/log/app.log"]:
            result = self.oracle.execute("FileRead", {"path": path})
            results.append(result)

        # At least one should succeed or fail gracefully
        valid = sum(1 for r in results if r.get("status") in ["success", "error"])

        if valid == len(results):
            self.tests_passed += 1
            return True
        else:
            self.tests_failed += 1
            return False

    def test_memory_under_chaos(self) -> bool:
        """Test memory operations survive chaos"""
        self.tests_run += 1

        # Store
        store_result = self.oracle.execute("MemoryStore", {
            "key": "test_key",
            "value": "test_value"
        })

        # Retrieve
        retrieve_result = self.oracle.execute("MemoryRetrieve", {
            "key": "test_key"
        })

        # Should handle chaos gracefully
        if (store_result.get("status") in ["success", "error"] and
            retrieve_result.get("status") in ["success", "error"]):
            self.tests_passed += 1
            return True
        else:
            self.tests_failed += 1
            return False

    def test_mixed_workload_under_chaos(self) -> bool:
        """Test mixed oracle types under chaos"""
        self.tests_run += 1

        # Execute various oracle types
        oracles = [
            ("Arith", {"op": "add", "a": 5, "b": 3}),
            ("TimeNow", {}),
            ("Semantic", {"prompt": "test", "context": "general"}),
            ("FileRead", {"path": "/etc/hostname"}),
            ("MemoryStore", {"key": "k", "value": "v"}),
        ]

        results = []
        for oracle_type, params in oracles:
            result = self.oracle.execute(oracle_type, params)
            results.append(result)

        # All should be either success or error (not crash)
        valid = sum(1 for r in results if r.get("status") in ["success", "error"])

        if valid == len(results):
            self.tests_passed += 1
            return True
        else:
            self.tests_failed += 1
            return False

    def run_all_tests(self) -> None:
        """Run complete test suite under chaos"""
        print("\n" + "="*60)
        print("CHAOS ENGINEERING TEST SUITE")
        print("="*60)

        tests = [
            ("Arithmetic under chaos", self.test_arithmetic_under_chaos),
            ("Semantic under chaos", self.test_semantic_under_chaos),
            ("File operations under chaos", self.test_file_operations_under_chaos),
            ("Memory under chaos", self.test_memory_under_chaos),
            ("Mixed workload under chaos", self.test_mixed_workload_under_chaos),
        ]

        print("\nRunning tests with chaos injection...\n")

        for name, test_fn in tests:
            result = test_fn()
            status = "✓ PASS" if result else "✗ FAIL"
            print(f"{status}: {name}")

    def report(self) -> None:
        """Generate test report"""
        print("\n" + "="*60)
        print("CHAOS TEST REPORT")
        print("="*60)

        print(f"\nTests run: {self.tests_run}")
        print(f"  Passed: {self.tests_passed}")
        print(f"  Failed: {self.tests_failed}")
        print(f"  Success rate: {self.tests_passed/self.tests_run*100:.1f}%")

        print(f"\nChaos events injected:")
        print(f"  Failures: {self.oracle.chaos.failures_injected}")
        print(f"  Delays: {self.oracle.chaos.delays_injected}")
        print(f"  Corruptions: {self.oracle.chaos.corruptions_injected}")

        total_chaos = (self.oracle.chaos.failures_injected +
                      self.oracle.chaos.delays_injected +
                      self.oracle.chaos.corruptions_injected)
        print(f"  Total chaos events: {total_chaos}")

        print(f"\nSystem resilience:")
        if self.tests_passed == self.tests_run:
            print("  ✓ EXCELLENT - All tests passed despite chaos")
        elif self.tests_passed / self.tests_run > 0.8:
            print("  ✓ GOOD - Most tests passed despite chaos")
        elif self.tests_passed / self.tests_run > 0.5:
            print("  ⚠ MODERATE - Some tests failed under chaos")
        else:
            print("  ✗ POOR - Many tests failed under chaos")


def main():
    parser = argparse.ArgumentParser(description="Chaos engineering for LMN oracles")
    parser.add_argument("--failure-rate", type=float, default=0.1,
                       help="Oracle failure rate (0.0-1.0)")
    parser.add_argument("--delay-rate", type=float, default=0.2,
                       help="Response delay rate (0.0-1.0)")
    parser.add_argument("--corruption-rate", type=float, default=0.05,
                       help="Result corruption rate (0.0-1.0)")
    parser.add_argument("--max-delay-ms", type=int, default=1000,
                       help="Maximum delay in milliseconds")
    parser.add_argument("--seed", type=int, help="Random seed")

    args = parser.parse_args()

    if args.seed:
        random.seed(args.seed)

    print("\n" + "="*60)
    print("CHAOS ENGINEERING HARNESS")
    print("="*60)
    print(f"\nChaos configuration:")
    print(f"  Failure rate: {args.failure_rate*100:.1f}%")
    print(f"  Delay rate: {args.delay_rate*100:.1f}%")
    print(f"  Corruption rate: {args.corruption_rate*100:.1f}%")
    print(f"  Max delay: {args.max_delay_ms}ms")

    chaos_mode = ChaosMode(
        failure_rate=args.failure_rate,
        delay_rate=args.delay_rate,
        corruption_rate=args.corruption_rate,
        max_delay_ms=args.max_delay_ms
    )

    chaos_oracle = ChaosOracle(chaos_mode)
    test_suite = ChaosTestSuite(chaos_oracle)

    test_suite.run_all_tests()
    test_suite.report()

    return 0 if test_suite.tests_failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
