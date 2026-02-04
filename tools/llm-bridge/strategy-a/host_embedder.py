#!/usr/bin/env python3
"""
Host Embedder for Bend/HVM - Strategy A Proof of Concept
=========================================================

This script demonstrates embedding Bend/HVM in Python and intercepting
oracle calls to provide LLM responses.

Approach:
1. Run Bend program as subprocess
2. Parse output for oracle requests
3. Call mock LLM (or real API)
4. Continue execution with response

Author: Rex (Engineer)
Date: 2026-02-01
"""

import subprocess
import json
import re
from typing import Dict, Any, Optional
from pathlib import Path


class BendOracle:
    """Host environment that runs Bend programs with LLM oracle support."""

    def __init__(self, bend_binary: str = None):
        """Initialize the oracle host.

        Args:
            bend_binary: Path to bend binary (default: search PATH)
        """
        self.bend_binary = bend_binary or "bend"
        self.oracle_cache = {}  # Cache oracle responses

    def mock_llm_call(self, prompt: str, context: str) -> str:
        """Mock LLM API call.

        In a real implementation, this would call Claude API.
        For testing, we return canned responses.

        Args:
            prompt: The prompt for the LLM
            context: Context information

        Returns:
            Mock LLM response
        """
        # Simple pattern matching for demo
        if "translate" in prompt.lower():
            if "cod flo log" in prompt:
                return "code flows clearly"

        # Handle arithmetic
        if "add" in prompt.lower():
            if "1 1" in prompt or "1+1" in prompt:
                return "2"
            # Generic addition parsing
            import re
            nums = re.findall(r'\d+', prompt)
            if len(nums) >= 2:
                return str(int(nums[0]) + int(nums[1]))

        # Default response
        return f"[LLM Response to: {prompt[:50]}...]"

    def run_bend_program(self, bend_file: Path) -> Dict[str, Any]:
        """Run a Bend program and capture output.

        Args:
            bend_file: Path to .bend file

        Returns:
            Dictionary with execution results
        """
        try:
            # First, check the program
            check_result = subprocess.run(
                [self.bend_binary, "check", str(bend_file)],
                capture_output=True,
                text=True,
                timeout=10
            )

            if check_result.returncode != 0:
                return {
                    "success": False,
                    "error": "Syntax/semantic error",
                    "stderr": check_result.stderr
                }

            # Compile to HVM
            gen_result = subprocess.run(
                [self.bend_binary, "gen-hvm", str(bend_file)],
                capture_output=True,
                text=True,
                timeout=10
            )

            if gen_result.returncode != 0:
                return {
                    "success": False,
                    "error": "Compilation error",
                    "stderr": gen_result.stderr
                }

            # Run with Rust runtime
            run_result = subprocess.run(
                [self.bend_binary, "run-rs", str(bend_file)],
                capture_output=True,
                text=True,
                timeout=10
            )

            return {
                "success": run_result.returncode == 0,
                "stdout": run_result.stdout,
                "stderr": run_result.stderr,
                "returncode": run_result.returncode
            }

        except subprocess.TimeoutExpired:
            return {
                "success": False,
                "error": "Execution timeout"
            }
        except FileNotFoundError:
            return {
                "success": False,
                "error": f"Bend binary not found: {self.bend_binary}"
            }
        except Exception as e:
            return {
                "success": False,
                "error": str(e)
            }

    def parse_oracle_requests(self, output: str) -> list:
        """Parse Bend output for oracle requests.

        In a more sophisticated implementation, we would:
        1. Instrument the HVM runtime to yield on specific nodes
        2. Use a protocol (JSON, msgpack) for requests
        3. Resume execution after providing responses

        For now, we parse text output looking for patterns.

        Args:
            output: Program output

        Returns:
            List of oracle requests found
        """
        requests = []

        # Look for our OracleRequest/Ask pattern in output
        # Bend outputs lambda terms like: λa (a OracleRequest/Ask/tag "prompt" "context")
        pattern = r'OracleRequest/Ask[^"]*"([^"]+)"\s+"([^"]+)"'
        matches = re.finditer(pattern, output)

        for match in matches:
            requests.append({
                "prompt": match.group(1),
                "context": match.group(2)
            })

        return requests

    def execute_with_oracle(self, bend_file: Path) -> Dict[str, Any]:
        """Execute a Bend program with oracle support.

        This is the main entry point for Strategy A.

        Args:
            bend_file: Path to .bend file

        Returns:
            Execution results with oracle responses
        """
        print(f"[Host] Executing Bend program: {bend_file}")

        # Run the program
        result = self.run_bend_program(bend_file)

        if not result["success"]:
            print(f"[Host] Execution failed: {result.get('error', 'Unknown error')}")
            if "stderr" in result:
                print(f"[Host] Error output:\n{result['stderr']}")
            return result

        print(f"[Host] Program output:\n{result['stdout']}")

        # Parse for oracle requests
        oracle_requests = self.parse_oracle_requests(result["stdout"])

        if oracle_requests:
            print(f"\n[Host] Found {len(oracle_requests)} oracle request(s)")

            # Process each request
            oracle_responses = []
            for req in oracle_requests:
                prompt = req["prompt"]
                context = req["context"]

                # Check cache
                cache_key = (prompt, context)
                if cache_key in self.oracle_cache:
                    response = self.oracle_cache[cache_key]
                    print(f"[Host] Cache hit for: {prompt}")
                else:
                    print(f"[Host] Calling LLM oracle...")
                    print(f"[Host]   Prompt: {prompt}")
                    print(f"[Host]   Context: {context}")

                    response = self.mock_llm_call(prompt, context)
                    self.oracle_cache[cache_key] = response

                    print(f"[Host]   Response: {response}")

                oracle_responses.append({
                    "request": req,
                    "response": response
                })

            result["oracle_requests"] = oracle_requests
            result["oracle_responses"] = oracle_responses

        else:
            print("[Host] No oracle requests found (pure deterministic execution)")

        return result


def main():
    """Main entry point for demonstration."""
    print("=" * 70)
    print("Strategy A: Host Embedding with LLM Oracle")
    print("=" * 70)
    print()

    # Setup
    script_dir = Path(__file__).parent
    bend_binary = script_dir.parent.parent / "lmn-bend" / "bend"

    if not bend_binary.exists():
        print(f"Error: Bend binary not found at {bend_binary}")
        print("Please compile or download bend first.")
        return 1

    # Create oracle host
    oracle = BendOracle(bend_binary=str(bend_binary))

    # Test with oracle example
    example_file = script_dir / "oracle_example.bend"

    if not example_file.exists():
        print(f"Error: Example file not found: {example_file}")
        return 1

    # Execute
    result = oracle.execute_with_oracle(example_file)

    print("\n" + "=" * 70)
    print("Execution Summary")
    print("=" * 70)
    print(f"Success: {result['success']}")

    if "oracle_responses" in result:
        print(f"Oracle calls: {len(result['oracle_responses'])}")
        for i, resp in enumerate(result['oracle_responses'], 1):
            print(f"\n  Call {i}:")
            print(f"    Prompt: {resp['request']['prompt']}")
            print(f"    Response: {resp['response']}")

    print("\n[Host] Strategy A demonstration complete.")

    return 0 if result["success"] else 1


if __name__ == "__main__":
    exit(main())
