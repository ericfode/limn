#!/usr/bin/env python3
"""Quick test runner for Limn interpreter."""

import sys
sys.path.insert(0, 'src')

from limn_pl_interpreter import run_all_tests

if __name__ == "__main__":
    run_all_tests()
