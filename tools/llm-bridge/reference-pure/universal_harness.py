#!/usr/bin/env python3
"""
Universal Oracle Harness - Reference Implementation
===================================================

The "Conscious" layer that executes all side effects.
Bend/HVM (subconscious) stays pure - just text→text.
This harness (conscious) interfaces with reality (∎).

Architecture:
- Bend generates oracle requests as pure data
- Harness parses requests from output
- Harness executes side effects
- Harness returns results

Author: Rex (Engineer)
Date: 2026-02-01
"""

import subprocess
import re
import json
import time
from pathlib import Path
from typing import Dict, Any, List, Optional
from dataclasses import dataclass
from enum import Enum


class OracleType(Enum):
    """Types of oracle requests."""
    SEMANTIC = "Semantic"
    FILE_READ = "FileRead"
    FILE_EXISTS = "FileExists"
    TIME_NOW = "TimeNow"
    TIME_SLEEP = "TimeSleep"
    DB_QUERY = "DbQuery"
    HTTP_GET = "HttpGet"
    COMPUTE = "Compute"


@dataclass
class OracleRequest:
    """Parsed oracle request from Bend output."""
    type: OracleType
    params: Dict[str, Any]


@dataclass
class OracleResponse:
    """Response from oracle execution."""
    request: OracleRequest
    result: Any
    error: Optional[str] = None
    duration_ms: float = 0


class UniversalHarness:
    """
    The Conscious layer - interfaces with reality.

    Responsibilities:
    - Run pure Bend programs
    - Parse oracle requests
    - Execute all side effects
    - Return results
    """

    def __init__(self, bend_binary: str = "bend", enable_real_llm: bool = False):
        """Initialize the harness.

        Args:
            bend_binary: Path to bend executable
            enable_real_llm: If True, call real Claude API (needs ANTHROPIC_API_KEY)
        """
        self.bend_binary = bend_binary
        self.enable_real_llm = enable_real_llm
        self.oracle_cache = {}  # Cache responses

    # =========================================================================
    # Bend Execution (Pure)
    # =========================================================================

    def run_bend(self, bend_file: Path) -> Dict[str, Any]:
        """Run a Bend program (pure computation).

        Args:
            bend_file: Path to .bend file

        Returns:
            Execution results
        """
        try:
            # Run with Rust runtime for speed
            result = subprocess.run(
                [self.bend_binary, "run-rs", str(bend_file)],
                capture_output=True,
                text=True,
                timeout=30
            )

            return {
                "success": result.returncode == 0,
                "stdout": result.stdout,
                "stderr": result.stderr,
                "returncode": result.returncode
            }

        except subprocess.TimeoutExpired:
            return {"success": False, "error": "Timeout"}
        except FileNotFoundError:
            return {"success": False, "error": f"Bend not found: {self.bend_binary}"}
        except Exception as e:
            return {"success": False, "error": str(e)}

    # =========================================================================
    # Oracle Parsing
    # =========================================================================

    def parse_oracles(self, output: str) -> List[OracleRequest]:
        """Parse oracle requests from Bend output.

        Bend outputs lambda terms like:
        λa (a Oracle/Semantic/tag "prompt" "context")

        Args:
            output: Bend program output

        Returns:
            List of parsed oracle requests
        """
        oracles = []

        # Pattern for each oracle type (handles /tag suffix from Bend)
        patterns = {
            OracleType.SEMANTIC: r'Oracle/Semantic(?:/\w+)?\s+"([^"]+)"\s+"([^"]+)"',
            OracleType.FILE_READ: r'Oracle/FileRead(?:/\w+)?\s+"([^"]+)"',
            OracleType.FILE_EXISTS: r'Oracle/FileExists(?:/\w+)?\s+"([^"]+)"',
            OracleType.TIME_NOW: r'Oracle/TimeNow(?:/\w+)?(?:\s|,|\))',
            OracleType.TIME_SLEEP: r'Oracle/TimeSleep(?:/\w+)?\s+(\d+)',
            OracleType.COMPUTE: r'Oracle/Compute(?:/\w+)?\s+"([^"]+)"\s+"([^"]+)"',
        }

        for oracle_type, pattern in patterns.items():
            for match in re.finditer(pattern, output):
                params = {}

                if oracle_type == OracleType.SEMANTIC:
                    params = {"prompt": match.group(1), "context": match.group(2)}
                elif oracle_type == OracleType.FILE_READ:
                    params = {"path": match.group(1)}
                elif oracle_type == OracleType.FILE_EXISTS:
                    params = {"path": match.group(1)}
                elif oracle_type == OracleType.TIME_NOW:
                    params = {}
                elif oracle_type == OracleType.TIME_SLEEP:
                    params = {"duration": int(match.group(1))}
                elif oracle_type == OracleType.COMPUTE:
                    params = {"op": match.group(1), "args": match.group(2)}

                oracles.append(OracleRequest(type=oracle_type, params=params))

        return oracles

    # =========================================================================
    # Oracle Execution (Side Effects - The Conscious Layer)
    # =========================================================================

    def execute_oracle(self, oracle: OracleRequest) -> OracleResponse:
        """Execute an oracle request (side effect).

        This is where ∎ ground truth happens.
        This is the interface to reality.

        Args:
            oracle: The oracle request to execute

        Returns:
            Oracle response with result or error
        """
        start = time.time()

        try:
            # Dispatch to appropriate handler
            if oracle.type == OracleType.SEMANTIC:
                result = self._oracle_semantic(oracle.params)
            elif oracle.type == OracleType.FILE_READ:
                result = self._oracle_file_read(oracle.params)
            elif oracle.type == OracleType.FILE_EXISTS:
                result = self._oracle_file_exists(oracle.params)
            elif oracle.type == OracleType.TIME_NOW:
                result = self._oracle_time_now()
            elif oracle.type == OracleType.TIME_SLEEP:
                result = self._oracle_time_sleep(oracle.params)
            elif oracle.type == OracleType.COMPUTE:
                result = self._oracle_compute(oracle.params)
            else:
                result = None
                error = f"Unknown oracle type: {oracle.type}"
                duration = (time.time() - start) * 1000
                return OracleResponse(oracle, result, error, duration)

            duration = (time.time() - start) * 1000
            return OracleResponse(oracle, result, None, duration)

        except Exception as e:
            duration = (time.time() - start) * 1000
            return OracleResponse(oracle, None, str(e), duration)

    # =========================================================================
    # Oracle Handlers (Conscious Actions)
    # =========================================================================

    def _oracle_semantic(self, params: Dict) -> str:
        """Handle semantic oracle (~ operator - LLM call).

        This delegates to Claude for semantic understanding.

        Args:
            params: {prompt: str, context: str}

        Returns:
            LLM response
        """
        prompt = params["prompt"]
        context = params["context"]

        # Check cache
        cache_key = (prompt, context)
        if cache_key in self.oracle_cache:
            return self.oracle_cache[cache_key]

        if self.enable_real_llm:
            # TODO: Implement real Claude API call
            # import anthropic
            # client = anthropic.Anthropic(api_key=os.environ.get("ANTHROPIC_API_KEY"))
            # response = client.messages.create(...)
            result = f"[Real LLM would be called: {prompt}]"
        else:
            # Mock LLM for testing
            result = self._mock_llm(prompt, context)

        self.oracle_cache[cache_key] = result
        return result

    def _mock_llm(self, prompt: str, context: str) -> str:
        """Mock LLM for testing."""
        # Simple pattern matching
        if "add" in prompt.lower():
            nums = re.findall(r'\d+', prompt)
            if len(nums) >= 2:
                return str(int(nums[0]) + int(nums[1]))
            return "2"

        if "translate" in prompt.lower() and "cod flo log" in prompt:
            return "code flows clearly"

        if "meaning of life" in prompt.lower():
            return "42"

        return f"[Mock LLM response to: {prompt[:50]}...]"

    def _oracle_file_read(self, params: Dict) -> str:
        """Handle file read oracle (∎ operator - ground truth).

        Args:
            params: {path: str}

        Returns:
            File contents
        """
        path = Path(params["path"])
        if not path.exists():
            raise FileNotFoundError(f"File not found: {path}")

        return path.read_text()

    def _oracle_file_exists(self, params: Dict) -> bool:
        """Handle file exists oracle (∎ operator).

        Args:
            params: {path: str}

        Returns:
            True if file exists
        """
        path = Path(params["path"])
        return path.exists()

    def _oracle_time_now(self) -> float:
        """Handle time now oracle (∿ operator).

        Returns:
            Current Unix timestamp
        """
        return time.time()

    def _oracle_time_sleep(self, params: Dict) -> str:
        """Handle time sleep oracle (∿ operator).

        Args:
            params: {duration: int}

        Returns:
            Confirmation message
        """
        duration = params["duration"]
        time.sleep(duration)
        return f"Slept for {duration} seconds"

    def _oracle_compute(self, params: Dict) -> str:
        """Handle compute oracle (~ operator - semantic computation).

        Args:
            params: {op: str, args: str}

        Returns:
            Computation result
        """
        op = params["op"]
        args = params["args"]

        # Delegate to semantic oracle
        prompt = f"{op}: {args}"
        return self._oracle_semantic({"prompt": prompt, "context": "computation"})

    # =========================================================================
    # Main Execution Loop
    # =========================================================================

    def execute(self, bend_file: Path) -> Dict[str, Any]:
        """Execute a Bend program with oracle support.

        This is the main consciousness loop:
        1. Subconscious (Bend) generates intentions (oracle requests)
        2. Conscious (Harness) executes actions (side effects)
        3. Results returned to context

        Args:
            bend_file: Path to Bend program

        Returns:
            Execution summary
        """
        print("=" * 70)
        print("Universal Oracle Harness - Reference Implementation")
        print("=" * 70)
        print(f"\n[Conscious] Executing: {bend_file.name}")
        print("[Conscious] Running subconscious (Bend/HVM)...\n")

        # Step 1: Run pure Bend program (subconscious)
        result = self.run_bend(bend_file)

        if not result["success"]:
            print(f"[Conscious] ✗ Execution failed: {result.get('error', 'Unknown')}")
            if "stderr" in result:
                print(f"[Conscious] Error output:\n{result['stderr']}")
            return result

        print(f"[Conscious] ✓ Subconscious completed\n")
        print(f"[Conscious] Raw output:\n{result['stdout']}\n")

        # Step 2: Parse oracle requests
        oracles = self.parse_oracles(result["stdout"])

        if not oracles:
            print("[Conscious] No oracle requests (pure deterministic execution)")
            return result

        print(f"[Conscious] Found {len(oracles)} oracle request(s)\n")

        # Step 3: Execute each oracle (side effects)
        responses = []
        for i, oracle in enumerate(oracles, 1):
            print(f"[Conscious] Oracle {i}/{len(oracles)}: {oracle.type.value}")
            print(f"[Conscious]   Params: {oracle.params}")

            response = self.execute_oracle(oracle)

            if response.error:
                print(f"[Conscious]   ✗ Error: {response.error}")
            else:
                print(f"[Conscious]   ✓ Result: {response.result}")
                print(f"[Conscious]   ⏱ Duration: {response.duration_ms:.2f}ms")

            responses.append(response)
            print()

        # Step 4: Summary
        result["oracles"] = [
            {
                "type": r.request.type.value,
                "params": r.request.params,
                "result": r.result,
                "error": r.error,
                "duration_ms": r.duration_ms
            }
            for r in responses
        ]

        return result


def main():
    """Main entry point."""
    script_dir = Path(__file__).parent

    # Setup harness
    bend_binary = script_dir.parent.parent / "lmn-bend" / "bend"
    if not bend_binary.exists():
        bend_binary = "bend"  # Try system bend

    harness = UniversalHarness(bend_binary=str(bend_binary))

    # Run universal oracle example
    example_file = script_dir / "universal_oracle.bend"

    if not example_file.exists():
        print(f"Error: {example_file} not found")
        return 1

    result = harness.execute(example_file)

    # Final summary
    print("=" * 70)
    print("Execution Summary")
    print("=" * 70)
    print(f"Success: {result['success']}")

    if "oracles" in result:
        print(f"Oracles executed: {len(result['oracles'])}")
        for i, oracle in enumerate(result['oracles'], 1):
            print(f"\n  Oracle {i}:")
            print(f"    Type: {oracle['type']}")
            print(f"    Params: {oracle['params']}")
            if oracle['error']:
                print(f"    Error: {oracle['error']}")
            else:
                print(f"    Result: {oracle['result']}")
                print(f"    Duration: {oracle['duration_ms']:.2f}ms")

    print("\n" + "=" * 70)
    print("Consciousness Architecture Demonstrated:")
    print("  Subconscious (HVM) generates intentions (pure)")
    print("  Conscious (Harness) executes actions (impure)")
    print("  System lives through delegation")
    print("=" * 70)

    return 0 if result["success"] else 1


if __name__ == "__main__":
    exit(main())
