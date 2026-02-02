#!/usr/bin/env python3
"""
Mock harness integration example.

Shows how to integrate TraceCollector with a harness-like system.
"""

from pathlib import Path
import sys
import time

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from viz import TraceCollector, OracleVisualizer


class MockHarness:
    """
    Mock harness demonstrating trace collection integration.

    This shows how the production harness would integrate trace collection.
    """

    def __init__(self, trace_collector=None):
        self.trace_collector = trace_collector

    def execute_oracle(self, oracle_type, params):
        """Execute an oracle with trace collection."""
        # Notify collector of start
        oracle_id = None
        if self.trace_collector:
            oracle_id = self.trace_collector.on_oracle_start(
                oracle_type,
                params
            )

        # Simulate execution
        start = time.time()
        try:
            result = self._mock_execute(oracle_type, params)
            success = True
            error = None
        except Exception as e:
            result = None
            success = False
            error = str(e)

        duration_ms = (time.time() - start) * 1000

        # Notify collector of completion
        if self.trace_collector and oracle_id is not None:
            self.trace_collector.on_oracle_complete(
                oracle_id,
                success=success,
                result=result,
                duration_ms=duration_ms,
                cached=False
            )

        return result

    def _mock_execute(self, oracle_type, params):
        """Mock oracle execution."""
        time.sleep(0.001)  # Simulate work

        if oracle_type == "Arith":
            op = params["op"]
            a = params["a"]
            b = params["b"]
            if op == "add":
                return a + b
            elif op == "mul":
                return a * b
        elif oracle_type == "Semantic":
            return f"[Mock LLM: {params['prompt'][:30]}...]"
        elif oracle_type == "TimeNow":
            return {"timestamp": int(time.time())}

        return f"[Result for {oracle_type}]"

    def execute_program(self, program_name):
        """Execute a mock program."""
        if self.trace_collector:
            self.trace_collector.start_program(program_name)

        # Simulate program execution
        self.execute_oracle("Arith", {"op": "add", "a": 1, "b": 1})
        self.execute_oracle("TimeNow", {})
        self.execute_oracle("Semantic", {"prompt": "What is life?", "context": "philosophy"})
        self.execute_oracle("Arith", {"op": "mul", "a": 6, "b": 7})

        if self.trace_collector:
            self.trace_collector.complete_program()


def main():
    """Run mock harness example."""
    print("=" * 70)
    print("Mock Harness Integration Example")
    print("=" * 70)
    print()

    # Create collector
    collector = TraceCollector()

    # Create harness with collector
    harness = MockHarness(trace_collector=collector)

    # Execute program
    print("[1] Executing mock program with trace collection...")
    harness.execute_program("mock_program.bend")
    print(f"    Executed {len(collector.trace.events)} oracles")
    print()

    # Visualize
    viz = OracleVisualizer(collector)

    print("[2] Execution trace:")
    print()
    viz.print_terminal()
    print()

    print("[3] Summary:")
    summary = viz.summary()
    for key, value in summary.items():
        print(f"    {key}: {value}")
    print()

    print("=" * 70)
    print("This demonstrates how to integrate TraceCollector with a harness.")
    print("See production harness for real integration.")
    print("=" * 70)


if __name__ == "__main__":
    main()
