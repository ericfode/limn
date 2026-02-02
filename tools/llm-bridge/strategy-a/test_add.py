#!/usr/bin/env python3
"""
Simple test for add_example.bend - Demonstrates minimal LMN oracle
"""

from pathlib import Path
from host_embedder import BendOracle

def main():
    print("=" * 70)
    print("LMN Oracle Test: add 1 1")
    print("=" * 70)
    print()
    print("This demonstrates the consciousness architecture:")
    print("  - Subconscious (HVM/Bend) runs deterministic code")
    print("  - Encounters ~ operator (oracle)")
    print("  - Delegates to Conscious (LLM)")
    print("  - Receives semantic response")
    print()

    script_dir = Path(__file__).parent

    # Look for bend binary
    bend_binary = script_dir.parent.parent / "lmn-bend" / "bend"

    if not bend_binary.exists():
        # Try system bend
        bend_binary = "bend"

    # Create oracle host
    oracle = BendOracle(bend_binary=str(bend_binary))

    # Test with add example
    example_file = script_dir / "add_example.bend"

    if not example_file.exists():
        print(f"Error: Example file not found: {example_file}")
        return 1

    # Execute with oracle support
    result = oracle.execute_with_oracle(example_file)

    print("\n" + "=" * 70)
    print("Result Summary")
    print("=" * 70)

    if result["success"]:
        print("✓ Execution successful")

        if "oracle_responses" in result:
            print(f"✓ Oracle invoked {len(result['oracle_responses'])} time(s)")
            for i, resp in enumerate(result['oracle_responses'], 1):
                print(f"\n  Oracle Call {i}:")
                print(f"    Question: {resp['request']['prompt']}")
                print(f"    Answer: {resp['response']}")
        else:
            print("⚠ No oracle calls detected")
    else:
        print(f"✗ Execution failed: {result.get('error', 'Unknown error')}")
        return 1

    print("\n" + "=" * 70)
    print("Consciousness Architecture Demonstrated:")
    print("  Subconscious (HVM) → Oracle (~) → Conscious (LLM) → Result")
    print("=" * 70)

    return 0


if __name__ == "__main__":
    exit(main())
