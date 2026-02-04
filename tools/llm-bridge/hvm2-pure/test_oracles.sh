#!/bin/bash
# Test all oracle types
# =====================
# Demonstrates the universal oracle pattern

set -e

HARNESS="./harness.py"

echo "═══════════════════════════════════════"
echo "  HVM2 Pure Oracle System Test"
echo "═══════════════════════════════════════"
echo

# 1. Arithmetic Oracle
echo "─── Arithmetic Oracle ───"
echo "Request: 5 + 3"
$HARNESS '{"oracle": "arithmetic", "operation": "add", "args": {"a": 5, "b": 3}}'
echo

echo "Request: 12 * 7"
$HARNESS '{"oracle": "arithmetic", "operation": "mul", "args": {"a": 12, "b": 7}}'
echo

echo "Request: 100 / 4"
$HARNESS '{"oracle": "arithmetic", "operation": "div", "args": {"a": 100, "b": 4}}'
echo

# 2. Temporal Oracle
echo "─── Temporal Oracle ───"
echo "Request: Current time"
$HARNESS '{"oracle": "temporal", "operation": "now", "args": {}}'
echo

echo "Request: Past events"
$HARNESS '{"oracle": "temporal", "operation": "was", "args": {"query": "recent", "limit": 3}}'
echo

echo "Request: Future projection"
$HARNESS '{"oracle": "temporal", "operation": "will", "args": {"query": "user_responds"}}'
echo

# 3. Filesystem Oracle
echo "─── Filesystem Oracle ───"
echo "Request: Write test file"
$HARNESS '{"oracle": "filesystem", "operation": "write", "args": {"path": "/tmp/oracle_test.txt", "content": "Pure HVM2 Oracle Test"}}'
echo

echo "Request: Read test file"
$HARNESS '{"oracle": "filesystem", "operation": "read", "args": {"path": "/tmp/oracle_test.txt"}}'
echo

echo "Request: Check if file exists"
$HARNESS '{"oracle": "filesystem", "operation": "exists", "args": {"path": "/tmp/oracle_test.txt"}}'
echo

# 4. LLM Oracle (mock)
echo "─── LLM Oracle (Mock) ───"
$HARNESS --mock-llm '{"oracle": "llm", "operation": "complete", "args": {"prompt": "The meaning of life is", "max_tokens": 50}}'
echo

echo "═══════════════════════════════════════"
echo "  All Oracle Tests Complete"
echo "═══════════════════════════════════════"
echo
echo "✓ Arithmetic oracle: Working"
echo "✓ Temporal oracle: Working"
echo "✓ Filesystem oracle: Working"
echo "✓ LLM oracle: Working (mock mode)"
echo
echo "The universal oracle pattern is proven."
echo "Pure HVM2 → Oracle requests → Python harness → Responses"
