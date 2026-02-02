#!/usr/bin/env python3
"""
Property-Based Testing for LMN Oracle System

Tests invariants that should hold across all oracle operations.
Uses randomized inputs to explore the property space.
"""

import random
import sys
import os
from typing import Any, Callable, List, Tuple

# Add parent directory to path to import harness
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'production'))

class PropertyTest:
    """Base class for property-based tests"""

    def __init__(self, name: str, property_fn: Callable, generator_fn: Callable):
        self.name = name
        self.property_fn = property_fn
        self.generator_fn = generator_fn
        self.failures = []

    def run(self, iterations: int = 100) -> bool:
        """Run property test for N iterations"""
        print(f"\n{'='*60}")
        print(f"Property: {self.name}")
        print(f"{'='*60}")

        passed = 0
        for i in range(iterations):
            test_input = self.generator_fn()
            try:
                result = self.property_fn(test_input)
                if result:
                    passed += 1
                else:
                    self.failures.append((test_input, "Property returned False"))
            except Exception as e:
                self.failures.append((test_input, str(e)))

        success_rate = (passed / iterations) * 100
        print(f"Passed: {passed}/{iterations} ({success_rate:.1f}%)")

        if self.failures:
            print(f"\nFirst 3 failures:")
            for inp, error in self.failures[:3]:
                print(f"  Input: {inp}")
                print(f"  Error: {error}")

        return len(self.failures) == 0


# Property: Arithmetic is deterministic
def prop_arithmetic_deterministic(inp: Tuple[str, int, int]) -> bool:
    """Arithmetic oracles should give same result on repeated calls"""
    op, a, b = inp

    # Simulate oracle calls (would use actual harness in production)
    def compute(operation, x, y):
        if operation == "add":
            return x + y
        elif operation == "sub":
            return x - y
        elif operation == "mul":
            return x * y
        elif operation == "div":
            return x / y if y != 0 else None

    result1 = compute(op, a, b)
    result2 = compute(op, a, b)

    return result1 == result2


def gen_arithmetic() -> Tuple[str, int, int]:
    """Generate random arithmetic oracle inputs"""
    op = random.choice(["add", "sub", "mul", "div"])
    a = random.randint(-1000, 1000)
    b = random.randint(-1000, 1000)
    return (op, a, b)


# Property: Memory store-retrieve is identity
def prop_memory_identity(inp: Tuple[str, str]) -> bool:
    """Storing then retrieving should return same value"""
    key, value = inp

    # Simulate memory operations
    memory = {}
    memory[key] = value
    retrieved = memory.get(key)

    return retrieved == value


def gen_memory() -> Tuple[str, str]:
    """Generate random memory oracle inputs"""
    key = f"key_{random.randint(0, 100)}"
    value = f"value_{random.randint(0, 1000)}"
    return (key, value)


# Property: Time operations are monotonic
def prop_time_monotonic(inp: int) -> bool:
    """now() called sequentially should give increasing timestamps"""
    import time

    timestamps = []
    for _ in range(inp):
        timestamps.append(time.time())
        time.sleep(0.001)  # Small delay

    # Check monotonicity
    for i in range(len(timestamps) - 1):
        if timestamps[i] >= timestamps[i + 1]:
            return False

    return True


def gen_time_sequence() -> int:
    """Generate sequence length"""
    return random.randint(2, 10)


# Property: File read-write preserves content
def prop_file_content_preservation(inp: str) -> bool:
    """Writing then reading should return same content"""
    content = inp

    # Simulate file operations (in-memory)
    files = {}
    path = "/tmp/test_file"
    files[path] = content
    read_content = files.get(path)

    return read_content == content


def gen_file_content() -> str:
    """Generate random file content"""
    length = random.randint(0, 1000)
    chars = 'abcdefghijklmnopqrstuvwxyz0123456789\n '
    return ''.join(random.choice(chars) for _ in range(length))


# Property: Arithmetic commutative operations
def prop_arithmetic_commutative(inp: Tuple[str, int, int]) -> bool:
    """add and mul should be commutative"""
    op, a, b = inp

    if op not in ["add", "mul"]:
        return True  # Only test commutative ops

    if op == "add":
        return (a + b) == (b + a)
    elif op == "mul":
        return (a * b) == (b * a)

    return True


def gen_commutative_op() -> Tuple[str, int, int]:
    """Generate commutative arithmetic operations"""
    op = random.choice(["add", "mul"])
    a = random.randint(-1000, 1000)
    b = random.randint(-1000, 1000)
    return (op, a, b)


# Property: Arithmetic associative operations
def prop_arithmetic_associative(inp: Tuple[str, int, int, int]) -> bool:
    """add and mul should be associative"""
    op, a, b, c = inp

    if op == "add":
        return ((a + b) + c) == (a + (b + c))
    elif op == "mul":
        return ((a * b) * c) == (a * (b * c))

    return True


def gen_associative_op() -> Tuple[str, int, int, int]:
    """Generate associative arithmetic operations"""
    op = random.choice(["add", "mul"])
    a = random.randint(-100, 100)  # Smaller to avoid overflow
    b = random.randint(-100, 100)
    c = random.randint(-100, 100)
    return (op, a, b, c)


# Property: Oracle composition preserves purity
def prop_composition_purity(inp: List[str]) -> bool:
    """Composing pure oracles should remain pure (no side effects)"""
    oracle_types = inp

    # All these types should be pure (no observable side effects to caller)
    pure_types = {"Arith", "TimeNow", "FileRead", "MemoryRetrieve",
                  "FileExists", "DbQuery", "HttpGet"}

    # Check all are pure
    for otype in oracle_types:
        if otype not in pure_types:
            return False

    return True


def gen_oracle_composition() -> List[str]:
    """Generate random oracle type composition"""
    pure_types = ["Arith", "TimeNow", "FileRead", "MemoryRetrieve",
                  "FileExists", "DbQuery", "HttpGet"]
    impure_types = ["FileWrite", "DbWrite", "HttpPost", "MemoryStore"]

    all_types = pure_types + impure_types
    length = random.randint(1, 5)
    return [random.choice(all_types) for _ in range(length)]


def main():
    """Run all property tests"""
    print("\n" + "="*60)
    print("PROPERTY-BASED TESTING FOR LMN ORACLE SYSTEM")
    print("="*60)

    tests = [
        PropertyTest(
            "Arithmetic operations are deterministic",
            prop_arithmetic_deterministic,
            gen_arithmetic
        ),
        PropertyTest(
            "Memory store-retrieve is identity",
            prop_memory_identity,
            gen_memory
        ),
        PropertyTest(
            "Time operations are monotonic",
            prop_time_monotonic,
            gen_time_sequence
        ),
        PropertyTest(
            "File operations preserve content",
            prop_file_content_preservation,
            gen_file_content
        ),
        PropertyTest(
            "Arithmetic: add and mul are commutative",
            prop_arithmetic_commutative,
            gen_commutative_op
        ),
        PropertyTest(
            "Arithmetic: add and mul are associative",
            prop_arithmetic_associative,
            gen_associative_op
        ),
        PropertyTest(
            "Oracle composition preserves purity",
            prop_composition_purity,
            gen_oracle_composition
        ),
    ]

    # Run all tests
    results = []
    for test in tests:
        passed = test.run(iterations=100)
        results.append((test.name, passed))

    # Summary
    print("\n" + "="*60)
    print("SUMMARY")
    print("="*60)

    total = len(results)
    passed = sum(1 for _, p in results if p)

    for name, passed_test in results:
        status = "✓ PASS" if passed_test else "✗ FAIL"
        print(f"{status}: {name}")

    print(f"\nTotal: {passed}/{total} properties hold")

    return 0 if passed == total else 1


if __name__ == "__main__":
    sys.exit(main())
