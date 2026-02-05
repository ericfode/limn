#!/usr/bin/env python3
"""Test harness for two-bucket solutions. Runs canonical exercism test cases."""
import sys, importlib.util, traceback

CASES = [
    {"desc": "bucket1=3, bucket2=5, goal=1, start=one",
     "input": {"bucketOne": 3, "bucketTwo": 5, "goal": 1, "startBucket": "one"},
     "expected": {"moves": 4, "goalBucket": "one", "otherBucket": 5}},
    {"desc": "bucket1=3, bucket2=5, goal=1, start=two",
     "input": {"bucketOne": 3, "bucketTwo": 5, "goal": 1, "startBucket": "two"},
     "expected": {"moves": 8, "goalBucket": "two", "otherBucket": 3}},
    {"desc": "bucket1=7, bucket2=11, goal=2, start=one",
     "input": {"bucketOne": 7, "bucketTwo": 11, "goal": 2, "startBucket": "one"},
     "expected": {"moves": 14, "goalBucket": "one", "otherBucket": 11}},
    {"desc": "bucket1=7, bucket2=11, goal=2, start=two",
     "input": {"bucketOne": 7, "bucketTwo": 11, "goal": 2, "startBucket": "two"},
     "expected": {"moves": 18, "goalBucket": "two", "otherBucket": 7}},
    {"desc": "bucket1=1, bucket2=3, goal=3, start=two",
     "input": {"bucketOne": 1, "bucketTwo": 3, "goal": 3, "startBucket": "two"},
     "expected": {"moves": 1, "goalBucket": "two", "otherBucket": 0}},
    {"desc": "bucket1=2, bucket2=3, goal=3, start=one",
     "input": {"bucketOne": 2, "bucketTwo": 3, "goal": 3, "startBucket": "one"},
     "expected": {"moves": 2, "goalBucket": "two", "otherBucket": 2}},
    {"desc": "bucket1=5, bucket2=1, goal=2, start=one (big-small)",
     "input": {"bucketOne": 5, "bucketTwo": 1, "goal": 2, "startBucket": "one"},
     "expected": {"moves": 6, "goalBucket": "one", "otherBucket": 1}},
    {"desc": "bucket1=3, bucket2=15, goal=9, start=one (small-big)",
     "input": {"bucketOne": 3, "bucketTwo": 15, "goal": 9, "startBucket": "one"},
     "expected": {"moves": 6, "goalBucket": "two", "otherBucket": 0}},
    {"desc": "impossible (6,15,goal=5)",
     "input": {"bucketOne": 6, "bucketTwo": 15, "goal": 5, "startBucket": "one"},
     "expected": "error"},
    {"desc": "same buckets different goal (6,15,goal=9)",
     "input": {"bucketOne": 6, "bucketTwo": 15, "goal": 9, "startBucket": "one"},
     "expected": {"moves": 10, "goalBucket": "two", "otherBucket": 0}},
    {"desc": "goal larger than both (impossible)",
     "input": {"bucketOne": 5, "bucketTwo": 7, "goal": 8, "startBucket": "one"},
     "expected": "error"},
]

def run_tests(module_path):
    spec = importlib.util.spec_from_file_location("solution", module_path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    fn = mod.measure
    passed, failed, errors = 0, 0, 0
    for c in CASES:
        try:
            result = fn(c["input"]["bucketOne"], c["input"]["bucketTwo"],
                       c["input"]["goal"], c["input"]["startBucket"])
            if c["expected"] == "error":
                failed += 1
                print(f"  FAIL: {c['desc']} — expected error, got {result}")
            elif (result["moves"] == c["expected"]["moves"] and
                  result["goalBucket"] == c["expected"]["goalBucket"] and
                  result["otherBucket"] == c["expected"]["otherBucket"]):
                passed += 1
                print(f"  PASS: {c['desc']}")
            else:
                failed += 1
                print(f"  FAIL: {c['desc']} — got {result}, expected {c['expected']}")
        except (ValueError, Exception) as e:
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
        print("Usage: python test_two_bucket.py <solution.py>")
        sys.exit(1)
    try:
        p, t = run_tests(sys.argv[1])
        sys.exit(0 if p == t else 1)
    except Exception as e:
        print(f"LOAD ERROR: Could not load solution — {e}")
        traceback.print_exc()
        sys.exit(2)
