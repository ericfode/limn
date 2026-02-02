#!/bin/bash
# Test the oracle harness with example requests

set -e

cd "$(dirname "$0")"

echo "Testing Oracle Harness..."
echo

echo "=== Arithmetic Tests ==="
cat examples/test_arithmetic.txt | grep -v '^#' | ./harness.py
echo

echo "=== Filesystem Tests ==="
cat examples/test_filesystem.txt | grep -v '^#' | ./harness.py
echo

echo "=== Caching Test (should return cached result) ==="
echo "ORACLE:ARITH:ADD:5,3" | ./harness.py
echo

echo "All tests passed!"
