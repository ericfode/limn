#!/usr/bin/env python3
"""Test harness for Forth interpreter solutions. Runs canonical exercism test cases."""
import sys, importlib.util, traceback

CASES = [
    # Parsing
    {"desc": "numbers pushed onto stack", "input": [["1 2 3 4 5"]], "expected": [1,2,3,4,5]},
    {"desc": "negative numbers", "input": [["-1 -2 -3 -4 -5"]], "expected": [-1,-2,-3,-4,-5]},
    # Addition
    {"desc": "add two numbers", "input": [["1 2 +"]], "expected": [3]},
    {"desc": "add: empty stack error", "input": [["+"]], "expected": "error"},
    {"desc": "add: only one value", "input": [["1 +"]], "expected": "error"},
    {"desc": "add: more than two values", "input": [["1 2 3 +"]], "expected": [1,5]},
    # Subtraction
    {"desc": "subtract two numbers", "input": [["3 4 -"]], "expected": [-1]},
    {"desc": "subtract: empty stack", "input": [["-"]], "expected": "error"},
    {"desc": "subtract: only one value", "input": [["1 -"]], "expected": "error"},
    {"desc": "subtract: more than two", "input": [["1 12 3 -"]], "expected": [1,9]},
    # Multiplication
    {"desc": "multiply two numbers", "input": [["2 4 *"]], "expected": [8]},
    {"desc": "multiply: empty stack", "input": [["*"]], "expected": "error"},
    {"desc": "multiply: only one value", "input": [["1 *"]], "expected": "error"},
    {"desc": "multiply: more than two", "input": [["1 2 3 *"]], "expected": [1,6]},
    # Division
    {"desc": "divide two numbers", "input": [["12 3 /"]], "expected": [4]},
    {"desc": "integer division", "input": [["8 3 /"]], "expected": [2]},
    {"desc": "divide by zero", "input": [["4 0 /"]], "expected": "error"},
    {"desc": "divide: empty stack", "input": [["/"]], "expected": "error"},
    {"desc": "divide: only one value", "input": [["1 /"]], "expected": "error"},
    {"desc": "divide: more than two", "input": [["1 12 3 /"]], "expected": [1,4]},
    # Combined arithmetic
    {"desc": "addition and subtraction", "input": [["1 2 + 4 -"]], "expected": [-1]},
    {"desc": "multiplication and division", "input": [["2 4 * 3 /"]], "expected": [2]},
    # DUP
    {"desc": "dup: copies value", "input": [["1 dup"]], "expected": [1,1]},
    {"desc": "dup: copies top", "input": [["1 2 dup"]], "expected": [1,2,2]},
    {"desc": "dup: empty stack", "input": [["dup"]], "expected": "error"},
    # DROP
    {"desc": "drop: only value", "input": [["1 drop"]], "expected": []},
    {"desc": "drop: top value", "input": [["1 2 drop"]], "expected": [1]},
    {"desc": "drop: empty stack", "input": [["drop"]], "expected": "error"},
    # SWAP
    {"desc": "swap: two values", "input": [["1 2 swap"]], "expected": [2,1]},
    {"desc": "swap: three values", "input": [["1 2 3 swap"]], "expected": [1,3,2]},
    {"desc": "swap: empty stack", "input": [["swap"]], "expected": "error"},
    {"desc": "swap: only one value", "input": [["1 swap"]], "expected": "error"},
    # OVER
    {"desc": "over: two values", "input": [["1 2 over"]], "expected": [1,2,1]},
    {"desc": "over: three values", "input": [["1 2 3 over"]], "expected": [1,2,3,2]},
    {"desc": "over: empty stack", "input": [["over"]], "expected": "error"},
    {"desc": "over: only one value", "input": [["1 over"]], "expected": "error"},
    # User-defined words
    {"desc": "user word: built-in words", "input": [[": dup-twice dup dup ;", "1 dup-twice"]], "expected": [1,1,1]},
    {"desc": "user word: right order", "input": [[": countup 1 2 3 ;", "countup"]], "expected": [1,2,3]},
    {"desc": "user word: override user word", "input": [[": foo dup ;", ": foo dup dup ;", "1 foo"]], "expected": [1,1,1]},
    {"desc": "user word: override built-in", "input": [[": swap dup ;", "1 swap"]], "expected": [1,1]},
    {"desc": "user word: override operator", "input": [[": + * ;", "3 4 +"]], "expected": [12]},
    {"desc": "user word: different words same name", "input": [[": foo 5 ;", ": bar foo ;", ": foo 6 ;", "bar foo"]], "expected": [5,6]},
    {"desc": "user word: redefine using self", "input": [[": foo 10 ;", ": foo foo 1 + ;", "foo"]], "expected": [11]},
    {"desc": "user word: cannot redefine numbers", "input": [[": 1 2 ;"]], "expected": "error"},
    {"desc": "user word: cannot redefine negative numbers", "input": [[": -1 2 ;"]], "expected": "error"},
    {"desc": "user word: non-existent word", "input": [["foo"]], "expected": "error"},
    # Case insensitivity
    {"desc": "case: DUP", "input": [["1 DUP Dup dup"]], "expected": [1,1,1,1]},
    {"desc": "case: DROP", "input": [["1 2 3 4 DROP Drop drop"]], "expected": [1]},
    {"desc": "case: SWAP", "input": [["1 2 SWAP 3 Swap 4 swap"]], "expected": [2,3,4,1]},
    {"desc": "case: OVER", "input": [["1 2 OVER Over over"]], "expected": [1,2,1,2,1]},
    {"desc": "case: user words", "input": [[": foo dup ;", "1 FOO Foo foo"]], "expected": [1,1,1,1]},
    {"desc": "case: definitions", "input": [[": SWAP DUP Dup dup ;", "1 swap"]], "expected": [1,1,1,1]},
]

def run_tests(module_path):
    spec = importlib.util.spec_from_file_location("solution", module_path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    fn = mod.evaluate
    passed, failed, errors = 0, 0, 0
    for c in CASES:
        try:
            result = fn(c["input"][0])
            if c["expected"] == "error":
                failed += 1
                print(f"  FAIL: {c['desc']} — expected error, got {result}")
            elif result == c["expected"]:
                passed += 1
                print(f"  PASS: {c['desc']}")
            else:
                failed += 1
                print(f"  FAIL: {c['desc']} — got {result}, expected {c['expected']}")
        except Exception as e:
            if c["expected"] == "error":
                passed += 1
                print(f"  PASS: {c['desc']} (raised {type(e).__name__})")
            else:
                errors += 1
                print(f"  ERROR: {c['desc']} — {e}")
    total = len(CASES)
    print(f"\nResult: {passed}/{total} passed, {failed} failed, {errors} errors")
    return passed, total

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python test_forth.py <solution.py>")
        sys.exit(1)
    try:
        p, t = run_tests(sys.argv[1])
        sys.exit(0 if p == t else 1)
    except Exception as e:
        print(f"LOAD ERROR: Could not load solution — {e}")
        traceback.print_exc()
        sys.exit(2)
