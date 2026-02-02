#!/bin/bash
# Run all LMN oracle tests

set -e

echo "======================================================================"
echo "LMN ORACLE TEST SUITE"
echo "======================================================================"
echo ""
echo "Running comprehensive test suite for the pure text→text oracle system"
echo ""

# Get directory of this script
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$DIR"

# Make all Python scripts executable
chmod +x *.py

echo "======================================================================"
echo "1. PROPERTY-BASED TESTS"
echo "======================================================================"
python3 property_tests.py
echo ""

echo "======================================================================"
echo "2. FUZZING TESTS"
echo "======================================================================"
python3 fuzzer.py --iterations=500
echo ""

echo "======================================================================"
echo "3. CHAOS ENGINEERING"
echo "======================================================================"
python3 chaos_harness.py --failure-rate=0.15
echo ""

echo "======================================================================"
echo "4. LOAD TESTING"
echo "======================================================================"
python3 load_test.py --oracles=1000 --test=all
echo ""

echo "======================================================================"
echo "5. EXECUTION TRACING"
echo "======================================================================"
python3 trace_visualizer.py --mock --format=ascii
echo ""

echo "======================================================================"
echo "6. COMPOSITION TESTING"
echo "======================================================================"
python3 composition_tests.py
echo ""

echo "======================================================================"
echo "ALL TESTS COMPLETE"
echo "======================================================================"
echo ""
echo "✓ Property-based testing - Tests oracle invariants"
echo "✓ Fuzzing - Tests robustness against malformed inputs"
echo "✓ Chaos engineering - Tests resilience under failures"
echo "✓ Load testing - Tests performance at scale"
echo "✓ Execution tracing - Visualizes oracle flow"
echo "✓ Composition testing - Tests semantic interactions"
echo ""
echo "Novel testing infrastructure complete!"
