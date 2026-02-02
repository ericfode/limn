#!/usr/bin/env python3
"""
Oracle Harness for HVM/Bend
Executes side effects requested by pure HVM programs.

Protocol:
  Input (from HVM):  ORACLE:<type>:<operation>:<args>
  Output (to HVM):   RESULT:<value>
  Error:             ERROR:<message>
"""

import sys
import os
from pathlib import Path
from typing import Optional, Dict, Callable


class OracleHarness:
    """Executes oracle requests and returns results."""

    def __init__(self):
        self.cache: Dict[str, str] = {}
        self.handlers = {
            'ARITH': self.handle_arithmetic,
            'FS': self.handle_filesystem,
            'LLM': self.handle_llm,
        }

    def handle_arithmetic(self, operation: str, args: str) -> str:
        """Execute arithmetic operations."""
        parts = args.split(',')

        if operation == 'ADD':
            a, b = int(parts[0]), int(parts[1])
            return str(a + b)
        elif operation == 'SUB':
            a, b = int(parts[0]), int(parts[1])
            return str(a - b)
        elif operation == 'MUL':
            a, b = int(parts[0]), int(parts[1])
            return str(a * b)
        elif operation == 'DIV':
            a, b = int(parts[0]), int(parts[1])
            if b == 0:
                raise ValueError("Division by zero")
            return str(a // b)
        else:
            raise ValueError(f"Unknown arithmetic operation: {operation}")

    def handle_filesystem(self, operation: str, args: str) -> str:
        """Execute filesystem operations."""
        if operation == 'READ':
            path = Path(args)
            if not path.exists():
                raise FileNotFoundError(f"File not found: {args}")
            return path.read_text()

        elif operation == 'WRITE':
            parts = args.split(':', 1)
            if len(parts) != 2:
                raise ValueError("WRITE requires path:content")
            path, content = parts
            Path(path).write_text(content)
            return "OK"

        elif operation == 'LIST':
            path = Path(args)
            if not path.exists():
                raise FileNotFoundError(f"Directory not found: {args}")
            if not path.is_dir():
                raise ValueError(f"Not a directory: {args}")
            entries = [e.name for e in path.iterdir()]
            return ','.join(sorted(entries))

        else:
            raise ValueError(f"Unknown filesystem operation: {operation}")

    def handle_llm(self, operation: str, args: str) -> str:
        """Execute LLM operations (stub - would integrate with actual LLM API)."""
        if operation == 'COMPLETE':
            # Stub: In real implementation, call LLM API
            # For demonstration, return a placeholder
            return f"[LLM response to: {args[:50]}...]"

        elif operation == 'CLASSIFY':
            parts = args.split('|', 1)
            if len(parts) != 2:
                raise ValueError("CLASSIFY requires text|categories")
            text, categories = parts
            # Stub: In real implementation, use LLM for classification
            cat_list = categories.split(',')
            return cat_list[0] if cat_list else "unknown"

        else:
            raise ValueError(f"Unknown LLM operation: {operation}")

    def execute_request(self, request: str) -> str:
        """
        Execute an oracle request.

        Args:
            request: Oracle request string (ORACLE:TYPE:OP:ARGS)

        Returns:
            Result string (RESULT:value or ERROR:message)
        """
        # Check cache first
        if request in self.cache:
            return f"RESULT:{self.cache[request]}"

        try:
            # Parse request
            if not request.startswith('ORACLE:'):
                raise ValueError(f"Invalid request format: {request}")

            parts = request[7:].split(':', 2)  # Skip "ORACLE:"
            if len(parts) < 2:
                raise ValueError(f"Incomplete request: {request}")

            oracle_type = parts[0]
            operation = parts[1]
            args = parts[2] if len(parts) > 2 else ""

            # Execute
            handler = self.handlers.get(oracle_type)
            if not handler:
                raise ValueError(f"Unknown oracle type: {oracle_type}")

            result = handler(operation, args)

            # Cache result
            self.cache[request] = result

            return f"RESULT:{result}"

        except Exception as e:
            return f"ERROR:{str(e)}"

    def run(self, input_stream=None, output_stream=None):
        """
        Main execution loop: read requests from stdin, write results to stdout.

        Args:
            input_stream: Input stream (defaults to sys.stdin)
            output_stream: Output stream (defaults to sys.stdout)
        """
        input_stream = input_stream or sys.stdin
        output_stream = output_stream or sys.stdout

        for line in input_stream:
            line = line.strip()
            if not line:
                continue

            result = self.execute_request(line)
            output_stream.write(result + '\n')
            output_stream.flush()


def main():
    """CLI entry point."""
    harness = OracleHarness()
    harness.run()


if __name__ == '__main__':
    main()
