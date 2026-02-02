#!/usr/bin/env python3
"""
Pure HVM2 Oracle Harness
========================

Executes oracle requests from Bend/HVM2 programs.

Bend code emits JSON oracle requests (pure, no side effects).
This harness executes them (impure, all side effects here).

Architecture:
  Bend (pure) → JSON requests → Python (impure) → JSON responses → Bend (pure)
"""

import json
import sys
import os
from typing import Dict, Any, Optional
from datetime import datetime
from pathlib import Path


class OracleResponse:
    """Standardized oracle response"""

    @staticmethod
    def success(result: Any) -> Dict[str, Any]:
        return {"success": True, "result": result, "error": None}

    @staticmethod
    def failure(error: str) -> Dict[str, Any]:
        return {"success": False, "result": None, "error": error}


class ArithmeticOracle:
    """Arithmetic operations oracle"""

    @staticmethod
    def execute(operation: str, args: Dict[str, Any]) -> Dict[str, Any]:
        try:
            a = args.get("a", 0)
            b = args.get("b", 0)

            operations = {
                "add": lambda: a + b,
                "sub": lambda: a - b,
                "mul": lambda: a * b,
                "div": lambda: a / b if b != 0 else None,
                "pow": lambda: a**b,
                "mod": lambda: a % b if b != 0 else None,
            }

            if operation not in operations:
                return OracleResponse.failure(f"Unknown operation: {operation}")

            result = operations[operation]()
            if result is None:
                return OracleResponse.failure(f"Invalid operation: {operation}({a}, {b})")

            return OracleResponse.success(result)

        except Exception as e:
            return OracleResponse.failure(f"Arithmetic error: {str(e)}")


class FilesystemOracle:
    """Filesystem operations oracle"""

    @staticmethod
    def _validate_path(path: str) -> bool:
        """Validate path to prevent directory traversal attacks"""
        # Convert to absolute path and normalize
        abs_path = os.path.abspath(path)
        # For this demo, we allow all paths but normalize them
        # In production, you would restrict to a specific directory
        return True

    @staticmethod
    def execute(operation: str, args: Dict[str, Any]) -> Dict[str, Any]:
        try:
            path = args.get("path", "")

            # Validate and normalize path
            if not FilesystemOracle._validate_path(path):
                return OracleResponse.failure(f"Invalid path: {path}")
            path = os.path.abspath(path)

            if operation == "read":
                if not os.path.exists(path):
                    return OracleResponse.failure(f"File not found: {path}")
                with open(path, "r") as f:
                    content = f.read()
                return OracleResponse.success(content)

            elif operation == "write":
                content = args.get("content", "")
                os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
                with open(path, "w") as f:
                    f.write(content)
                return OracleResponse.success(f"Wrote {len(content)} bytes to {path}")

            elif operation == "list":
                if not os.path.exists(path):
                    return OracleResponse.failure(f"Directory not found: {path}")
                items = os.listdir(path)
                return OracleResponse.success(items)

            elif operation == "exists":
                exists = os.path.exists(path)
                return OracleResponse.success(exists)

            else:
                return OracleResponse.failure(f"Unknown operation: {operation}")

        except Exception as e:
            return OracleResponse.failure(f"Filesystem error: {str(e)}")


class TemporalOracle:
    """Temporal/time operations oracle"""

    def __init__(self):
        self.history = []  # Store temporal history

    def execute(self, operation: str, args: Dict[str, Any]) -> Dict[str, Any]:
        try:
            if operation == "now":
                now = datetime.now()
                result = {
                    "timestamp": int(now.timestamp()),
                    "iso": now.isoformat(),
                    "unix": now.timestamp(),
                }
                # Record in history
                self.history.append({"time": now, "event": "now", "data": result})
                return OracleResponse.success(result)

            elif operation == "was":
                # Query past state from history
                query = args.get("query", "")
                # Simple implementation: return last N events
                n = args.get("limit", 10)
                past_events = self.history[-n:] if self.history else []
                serialized = [
                    {
                        "time": event["time"].isoformat(),
                        "event": event["event"],
                        "data": event["data"],
                    }
                    for event in past_events
                ]
                return OracleResponse.success(serialized)

            elif operation == "will":
                # Project future state (constraint over possibilities)
                query = args.get("query", "")
                # Simple implementation: return constraint specification
                result = {
                    "constraint": query,
                    "possibilities": "unbounded",  # Would be computed based on constraints
                    "note": "Future projection - constraint-based, not prediction",
                }
                return OracleResponse.success(result)

            else:
                return OracleResponse.failure(f"Unknown operation: {operation}")

        except Exception as e:
            return OracleResponse.failure(f"Temporal error: {str(e)}")


class LlmOracle:
    """LLM inference oracle (stub for now)"""

    def __init__(self, api_key: Optional[str] = None, mock: bool = False):
        self.api_key = api_key
        self.mock = mock

    def execute(self, operation: str, args: Dict[str, Any]) -> Dict[str, Any]:
        try:
            prompt = args.get("prompt", "")
            max_tokens = args.get("max_tokens", 100)

            if self.mock:
                # Mock response for testing
                result = f"[MOCK LLM] Response to: {prompt[:50]}..."
                return OracleResponse.success(result)

            if operation == "complete":
                # TODO: Integrate actual LLM API (Anthropic, OpenAI, etc.)
                return OracleResponse.failure(
                    "LLM API not configured. Use --mock for testing."
                )

            elif operation == "parse":
                # TODO: Implement structured parsing
                return OracleResponse.failure(
                    "LLM parsing not implemented. Use --mock for testing."
                )

            elif operation == "generate":
                # TODO: Implement template-based generation
                return OracleResponse.failure(
                    "LLM generation not implemented. Use --mock for testing."
                )

            else:
                return OracleResponse.failure(f"Unknown operation: {operation}")

        except Exception as e:
            return OracleResponse.failure(f"LLM error: {str(e)}")


class OracleHarness:
    """Main harness that routes oracle requests to appropriate handlers"""

    def __init__(self, mock_llm: bool = False, llm_api_key: Optional[str] = None):
        self.arithmetic = ArithmeticOracle()
        self.filesystem = FilesystemOracle()
        self.temporal = TemporalOracle()
        self.llm = LlmOracle(api_key=llm_api_key, mock=mock_llm)

    def execute_request(self, request: Dict[str, Any]) -> Dict[str, Any]:
        """Execute a single oracle request"""
        oracle_type = request.get("oracle", "")
        operation = request.get("operation", "")
        args = request.get("args", {})

        # Route to appropriate oracle
        if oracle_type == "arithmetic":
            return self.arithmetic.execute(operation, args)
        elif oracle_type == "filesystem":
            return self.filesystem.execute(operation, args)
        elif oracle_type == "temporal":
            return self.temporal.execute(operation, args)
        elif oracle_type == "llm":
            return self.llm.execute(operation, args)
        else:
            return OracleResponse.failure(f"Unknown oracle type: {oracle_type}")

    def run_interactive(self):
        """Interactive REPL for testing oracle requests"""
        print("Oracle Harness Interactive Mode")
        print("================================")
        print("Enter oracle requests as JSON (one per line)")
        print('Example: {"oracle": "arithmetic", "operation": "add", "args": {"a": 5, "b": 3}}')
        print("Type 'exit' to quit\n")

        while True:
            try:
                line = input("> ")
                if line.strip().lower() == "exit":
                    break

                request = json.loads(line)
                response = self.execute_request(request)
                print(json.dumps(response, indent=2))

            except json.JSONDecodeError as e:
                print(f"Invalid JSON: {e}")
            except KeyboardInterrupt:
                print("\nExiting...")
                break
            except Exception as e:
                print(f"Error: {e}")


def main():
    """Main entry point"""
    import argparse

    parser = argparse.ArgumentParser(description="HVM2 Pure Oracle Harness")
    parser.add_argument("--interactive", "-i", action="store_true", help="Interactive REPL mode")
    parser.add_argument("--mock-llm", action="store_true", help="Use mock LLM responses")
    parser.add_argument("--llm-api-key", help="LLM API key")
    parser.add_argument("request", nargs="?", help="JSON oracle request")

    args = parser.parse_args()

    harness = OracleHarness(mock_llm=args.mock_llm, llm_api_key=args.llm_api_key)

    if args.interactive:
        harness.run_interactive()
    elif args.request:
        try:
            request = json.loads(args.request)
            response = harness.execute_request(request)
            print(json.dumps(response, indent=2))
        except json.JSONDecodeError as e:
            print(f"Invalid JSON: {e}", file=sys.stderr)
            sys.exit(1)
    else:
        parser.print_help()


if __name__ == "__main__":
    main()
