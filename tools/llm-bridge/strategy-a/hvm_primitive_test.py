#!/usr/bin/env python3
"""
HVM Primitive Extension Test
=============================

This demonstrates how to add custom primitives to HVM for oracle support.

Strategy:
1. Generate HVM code from Bend
2. Inject custom primitive definitions
3. Run modified HVM code
4. Intercept oracle primitive calls

This is closer to a true embedding where the host can inject
behavior directly into the HVM runtime.

Author: Rex (Engineer)
Date: 2026-02-01
"""

import subprocess
from pathlib import Path


def inject_oracle_primitive(hvm_code: str) -> str:
    """Inject oracle primitive into HVM code.

    Args:
        hvm_code: Generated HVM code

    Returns:
        Modified HVM code with oracle primitive
    """
    # In HVM, we can define custom primitives
    # For now, this is a stub - real implementation would:
    # 1. Define a %primitive in HVM4
    # 2. Link to Rust/C FFI function
    # 3. Function calls back to Python host

    oracle_primitive = """
// Oracle primitive (would be implemented as FFI in real version)
// This would yield to host with (prompt, context)
// Host provides response, execution continues
@oracle/primitive = (prompt (context result))
  & @oracle/yield ~ (prompt (context result))
"""

    # Prepend our primitive
    return oracle_primitive + "\n" + hvm_code


def test_primitive_injection():
    """Test primitive injection workflow."""
    print("=" * 70)
    print("HVM Primitive Extension Test")
    print("=" * 70)
    print()

    script_dir = Path(__file__).parent
    bend_binary = script_dir.parent.parent / "lmn-bend" / "bend"
    example_file = script_dir / "oracle_example.bend"

    # Step 1: Generate HVM code
    print("[Test] Generating HVM code from Bend...")
    result = subprocess.run(
        [str(bend_binary), "gen-hvm", str(example_file)],
        capture_output=True,
        text=True
    )

    if result.returncode != 0:
        print(f"[Test] Error: {result.stderr}")
        return 1

    hvm_code = result.stdout
    print(f"[Test] Generated {len(hvm_code)} bytes of HVM code")

    # Step 2: Inject oracle primitive
    print("[Test] Injecting oracle primitive...")
    modified_code = inject_oracle_primitive(hvm_code)
    print(f"[Test] Modified code: {len(modified_code)} bytes")

    # Step 3: Save modified HVM
    modified_file = script_dir / "oracle_with_primitive.hvm"
    modified_file.write_text(modified_code)
    print(f"[Test] Saved to: {modified_file}")

    # Step 4: Document findings
    print()
    print("[Test] Findings:")
    print("  - Bend compiles to HVM interaction nets")
    print("  - HVM code is human-readable text format")
    print("  - Can inject custom definitions")
    print("  - Real implementation needs:")
    print("    * HVM4 %primitive support")
    print("    * FFI to Rust/C/Python")
    print("    * Yield/resume mechanism")
    print("    * State preservation across calls")

    print()
    print("[Test] Next steps for true embedding:")
    print("  1. Build HVM as library (not binary)")
    print("  2. Expose primitive registration API")
    print("  3. Implement oracle primitive in host language")
    print("  4. Add yield/resume to execution loop")
    print("  5. Test with stateful oracle calls")

    return 0


if __name__ == "__main__":
    exit(test_primitive_injection())
