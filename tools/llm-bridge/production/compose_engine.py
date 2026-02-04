#!/usr/bin/env python3
"""
Oracle Composition Engine
==========================

Chain oracles together where output of one feeds input of next.

Example composition:
    now() → format_time() → ask("what does this time mean?")

    1. Get timestamp
    2. Format it nicely
    3. Ask LLM to interpret

Enables complex workflows from simple building blocks.

Author: Rex (Engineer)
Date: 2026-02-01
"""

from typing import List, Dict, Any, Callable, Optional
from dataclasses import dataclass
from enum import Enum


class CompositionType(Enum):
    """Types of oracle compositions."""
    SEQUENTIAL = "sequential"      # A → B → C (output feeds input)
    PARALLEL = "parallel"          # A, B, C (all execute together)
    CONDITIONAL = "conditional"    # A → if(result) then B else C
    LOOP = "loop"                  # A → B → A (until condition)
    MAP = "map"                    # [A, B, C].map(transform)
    REDUCE = "reduce"              # [A, B, C].reduce(accumulate)


@dataclass
class CompositionStep:
    """A single step in a composition."""
    oracle_type: str
    params: Dict[str, Any]
    transform: Optional[Callable] = None  # Transform output before next step
    condition: Optional[Callable] = None  # For conditional execution


@dataclass
class Composition:
    """A composition of multiple oracles."""
    name: str
    type: CompositionType
    steps: List[CompositionStep]
    description: str = ""


class ComposeEngine:
    """Build and execute oracle compositions."""

    def __init__(self, harness):
        """Initialize with harness for oracle execution.

        Args:
            harness: ProductionHarness instance for executing oracles
        """
        self.harness = harness
        self.compositions = {}  # name -> Composition

    def register(self, composition: Composition):
        """Register a composition for reuse.

        Args:
            composition: Composition to register
        """
        self.compositions[composition.name] = composition

    def execute_sequential(self, steps: List[CompositionStep], initial_input: Any = None) -> Any:
        """Execute steps sequentially, piping output to next input.

        Args:
            steps: List of composition steps
            initial_input: Starting input for first step

        Returns:
            Final result
        """
        result = initial_input

        for i, step in enumerate(steps):
            # Build params, injecting previous result if available
            params = step.params.copy()

            # Inject result into params (smart injection)
            if result is not None:
                params = self._inject_result(params, result, step.oracle_type)

            # Execute oracle
            from harness import OracleRequest, OracleType
            oracle = OracleRequest(
                type=OracleType[step.oracle_type.upper()],
                params=params
            )

            response = self.harness.execute_oracle(oracle)

            if not response.success:
                raise Exception(f"Step {i} failed: {response.error}")

            result = response.result

            # Apply transformation if present
            if step.transform:
                result = step.transform(result)

        return result

    def _inject_result(self, params: Dict, result: Any, oracle_type: str) -> Dict:
        """Intelligently inject previous result into params.

        Args:
            params: Current parameters
            result: Previous result to inject
            oracle_type: Type of oracle

        Returns:
            Updated parameters
        """
        # For semantic oracles, inject as part of prompt
        if oracle_type.lower() == "semantic":
            if "prompt" in params:
                # Append result to prompt
                params["prompt"] = f"{params['prompt']}\n\nContext: {result}"
            else:
                params["prompt"] = str(result)

        # For file operations, might be a path
        elif oracle_type.lower() in ["fileread", "filewrite"]:
            if isinstance(result, str) and "path" not in params:
                params["path"] = result

        # For arithmetic, might be operand
        elif oracle_type.lower() == "arith":
            if isinstance(result, (int, float)):
                if "a" not in params:
                    params["a"] = int(result)
                elif "b" not in params:
                    params["b"] = int(result)

        # Generic: add as "input" field
        else:
            if "input" not in params:
                params["input"] = result

        return params

    def execute_parallel(self, steps: List[CompositionStep]) -> List[Any]:
        """Execute steps in parallel.

        Args:
            steps: List of composition steps

        Returns:
            List of results
        """
        from harness import OracleRequest, OracleType

        # Build all oracle requests
        oracles = [
            OracleRequest(
                type=OracleType[step.oracle_type.upper()],
                params=step.params
            )
            for step in steps
        ]

        # Execute in parallel if async harness available
        if hasattr(self.harness, 'execute_oracles_parallel'):
            responses = self.harness.execute_oracles_parallel(oracles)
        else:
            # Fallback to sequential
            responses = [self.harness.execute_oracle(o) for o in oracles]

        results = []
        for i, response in enumerate(responses):
            if not response.success:
                raise Exception(f"Parallel step {i} failed: {response.error}")

            result = response.result
            if steps[i].transform:
                result = steps[i].transform(result)
            results.append(result)

        return results

    def execute_conditional(self, condition_step: CompositionStep,
                          true_steps: List[CompositionStep],
                          false_steps: List[CompositionStep],
                          initial_input: Any = None) -> Any:
        """Execute conditional composition (if-then-else).

        Args:
            condition_step: Step that evaluates condition
            true_steps: Steps to execute if condition is true
            false_steps: Steps to execute if condition is false
            initial_input: Starting input

        Returns:
            Result from chosen branch
        """
        # Evaluate condition
        condition_result = self.execute_sequential([condition_step], initial_input)

        # Check condition
        is_true = bool(condition_result)
        if condition_step.condition:
            is_true = condition_step.condition(condition_result)

        # Execute chosen branch
        if is_true:
            return self.execute_sequential(true_steps, condition_result)
        else:
            return self.execute_sequential(false_steps, condition_result)

    def execute_loop(self, steps: List[CompositionStep],
                    condition: Callable[[Any], bool],
                    initial_input: Any = None,
                    max_iterations: int = 10) -> Any:
        """Execute loop composition (repeat until condition).

        Args:
            steps: Steps to repeat
            condition: Function that returns True to continue
            initial_input: Starting input
            max_iterations: Safety limit

        Returns:
            Final result
        """
        result = initial_input
        iterations = 0

        while condition(result) and iterations < max_iterations:
            result = self.execute_sequential(steps, result)
            iterations += 1

        return result

    def execute_map(self, step: CompositionStep, inputs: List[Any]) -> List[Any]:
        """Execute step for each input (map operation).

        Args:
            step: Step to apply to each input
            inputs: List of inputs

        Returns:
            List of results
        """
        results = []
        for input_val in inputs:
            params = self._inject_result(step.params.copy(), input_val, step.oracle_type)

            from harness import OracleRequest, OracleType
            oracle = OracleRequest(
                type=OracleType[step.oracle_type.upper()],
                params=params
            )

            response = self.harness.execute_oracle(oracle)
            if not response.success:
                raise Exception(f"Map step failed: {response.error}")

            result = response.result
            if step.transform:
                result = step.transform(result)
            results.append(result)

        return results

    def execute_reduce(self, step: CompositionStep, inputs: List[Any],
                      accumulator: Callable[[Any, Any], Any]) -> Any:
        """Execute reduce operation over inputs.

        Args:
            step: Step to apply
            inputs: List of inputs
            accumulator: Function to combine results

        Returns:
            Accumulated result
        """
        result = None

        for input_val in inputs:
            params = self._inject_result(step.params.copy(), input_val, step.oracle_type)

            from harness import OracleRequest, OracleType
            oracle = OracleRequest(
                type=OracleType[step.oracle_type.upper()],
                params=params
            )

            response = self.harness.execute_oracle(oracle)
            if not response.success:
                raise Exception(f"Reduce step failed: {response.error}")

            step_result = response.result
            if step.transform:
                step_result = step.transform(step_result)

            result = accumulator(result, step_result) if result else step_result

        return result

    def execute(self, composition: Composition, initial_input: Any = None, **kwargs) -> Any:
        """Execute a composition.

        Args:
            composition: Composition to execute
            initial_input: Starting input
            **kwargs: Additional parameters

        Returns:
            Composition result
        """
        if composition.type == CompositionType.SEQUENTIAL:
            return self.execute_sequential(composition.steps, initial_input)
        elif composition.type == CompositionType.PARALLEL:
            return self.execute_parallel(composition.steps)
        elif composition.type == CompositionType.MAP:
            inputs = kwargs.get('inputs', [initial_input])
            return self.execute_map(composition.steps[0], inputs)
        elif composition.type == CompositionType.REDUCE:
            inputs = kwargs.get('inputs', [])
            accumulator = kwargs.get('accumulator', lambda a, b: a + b if a else b)
            return self.execute_reduce(composition.steps[0], inputs, accumulator)
        else:
            raise ValueError(f"Composition type {composition.type} not implemented")


def main():
    """Test composition engine."""
    print("=== Oracle Composition Engine ===\n")

    # Would need actual harness to test
    print("Composition engine ready.")
    print("Supports: sequential, parallel, conditional, loop, map, reduce")
    print("\nExample compositions:")
    print("  1. now() → format() → ask('meaning?')")
    print("  2. [file1, file2, file3].map(read) → concat")
    print("  3. loop: ask() → evaluate() → if(done) break else ask()")


if __name__ == "__main__":
    main()
