#!/usr/bin/env python3
"""Test harness for knapsack solutions. Runs canonical exercism test cases."""
import sys, importlib.util, json, traceback

CASES = [
    {"desc": "no items (empty list)", "input": {"maximumWeight": 100, "items": []}, "expected": 0},
    {"desc": "one item, too heavy", "input": {"maximumWeight": 10, "items": [{"weight": 100, "value": 1}]}, "expected": 0},
    {"desc": "five items (cannot be greedy by weight)", "input": {"maximumWeight": 10, "items": [
        {"weight": 2, "value": 5}, {"weight": 2, "value": 5}, {"weight": 2, "value": 5},
        {"weight": 2, "value": 5}, {"weight": 10, "value": 21}]}, "expected": 21},
    {"desc": "five items (cannot be greedy by value)", "input": {"maximumWeight": 10, "items": [
        {"weight": 2, "value": 20}, {"weight": 2, "value": 20}, {"weight": 2, "value": 20},
        {"weight": 2, "value": 20}, {"weight": 10, "value": 50}]}, "expected": 80},
    {"desc": "example knapsack", "input": {"maximumWeight": 10, "items": [
        {"weight": 5, "value": 10}, {"weight": 4, "value": 40},
        {"weight": 6, "value": 30}, {"weight": 4, "value": 50}]}, "expected": 90},
    {"desc": "8 items", "input": {"maximumWeight": 104, "items": [
        {"weight": 25, "value": 350}, {"weight": 35, "value": 400}, {"weight": 45, "value": 450},
        {"weight": 5, "value": 20}, {"weight": 25, "value": 70}, {"weight": 3, "value": 8},
        {"weight": 2, "value": 5}, {"weight": 2, "value": 5}]}, "expected": 900},
    {"desc": "15 items", "input": {"maximumWeight": 750, "items": [
        {"weight": 70, "value": 135}, {"weight": 73, "value": 139}, {"weight": 77, "value": 149},
        {"weight": 80, "value": 150}, {"weight": 82, "value": 156}, {"weight": 87, "value": 163},
        {"weight": 90, "value": 173}, {"weight": 94, "value": 184}, {"weight": 98, "value": 192},
        {"weight": 106, "value": 201}, {"weight": 110, "value": 210}, {"weight": 113, "value": 214},
        {"weight": 115, "value": 221}, {"weight": 118, "value": 229}, {"weight": 120, "value": 240}]},
        "expected": 1458},
]

def run_tests(module_path):
    spec = importlib.util.spec_from_file_location("solution", module_path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    fn = mod.maximum_value
    passed, failed, errors = 0, 0, 0
    for c in CASES:
        try:
            result = fn(c["input"]["maximumWeight"], c["input"]["items"])
            if result == c["expected"]:
                passed += 1
                print(f"  PASS: {c['desc']}")
            else:
                failed += 1
                print(f"  FAIL: {c['desc']} — got {result}, expected {c['expected']}")
        except Exception as e:
            errors += 1
            print(f"  ERROR: {c['desc']} — {e}")
    total = len(CASES)
    print(f"\nResult: {passed}/{total} passed, {failed} failed, {errors} errors")
    return passed, total

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python test_knapsack.py <solution.py>")
        sys.exit(1)
    try:
        p, t = run_tests(sys.argv[1])
        sys.exit(0 if p == t else 1)
    except Exception as e:
        print(f"LOAD ERROR: Could not load solution — {e}")
        traceback.print_exc()
        sys.exit(2)
