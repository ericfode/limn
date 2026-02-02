#!/usr/bin/env python3
"""
Interactive demo showing the oracle harness in action.
Demonstrates the pure HVM4 oracle architecture.
"""

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from harness import OracleHarness


def demo_arithmetic():
    """Demonstrate arithmetic oracle operations."""
    print("=== Arithmetic Oracle Demo ===\n")

    harness = OracleHarness()

    requests = [
        ("ORACLE:ARITH:ADD:5,3", "5 + 3"),
        ("ORACLE:ARITH:MUL:7,6", "7 * 6"),
        ("ORACLE:ARITH:SUB:10,4", "10 - 4"),
        ("ORACLE:ARITH:DIV:20,5", "20 / 5"),
    ]

    for request, description in requests:
        result = harness.execute_request(request)
        value = result.split(':')[1]
        print(f"{description} = {value}")

    print()


def demo_caching():
    """Demonstrate oracle response caching."""
    print("=== Caching Demo ===\n")

    harness = OracleHarness()

    # First call - not cached
    request = "ORACLE:ARITH:ADD:100,200"
    result1 = harness.execute_request(request)
    print(f"First call:  {request}")
    print(f"Result:      {result1}")
    print(f"Cache size:  {len(harness.cache)}")

    # Second call - cached
    result2 = harness.execute_request(request)
    print(f"\nSecond call: {request}")
    print(f"Result:      {result2} (from cache)")
    print(f"Cache size:  {len(harness.cache)}")

    print()


def demo_error_handling():
    """Demonstrate error handling."""
    print("=== Error Handling Demo ===\n")

    harness = OracleHarness()

    bad_requests = [
        ("ORACLE:ARITH:DIV:10,0", "Division by zero"),
        ("ORACLE:FS:READ:/nonexistent/file.txt", "File not found"),
        ("ORACLE:UNKNOWN:OP:args", "Unknown oracle type"),
        ("INVALID_REQUEST", "Invalid format"),
    ]

    for request, description in bad_requests:
        result = harness.execute_request(request)
        print(f"{description}:")
        print(f"  Request: {request}")
        print(f"  Result:  {result}")
        print()


def main():
    """Run all demos."""
    print("\n" + "="*60)
    print("HVM4 Pure Oracle Architecture - Interactive Demo")
    print("="*60 + "\n")

    demo_arithmetic()
    demo_caching()
    demo_error_handling()

    print("="*60)
    print("Demo complete!")
    print("\nKey insights:")
    print("- HVM emits requests as pure text (no FFI)")
    print("- Python harness executes all side effects")
    print("- Results are cached for determinism")
    print("- Errors are handled gracefully")
    print("="*60 + "\n")


if __name__ == '__main__':
    main()
