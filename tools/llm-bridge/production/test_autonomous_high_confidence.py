#!/usr/bin/env python3
"""Test autonomous learning with high-confidence patterns (auto-apply)."""

from harness import ProductionHarness, OracleRequest, OracleType

# Create content with highly repetitive patterns (10+ occurrences)
# This should trigger auto-application of rules
repetitions = []
for i in range(12):
    repetitions.append(f"~ cod flo log → sys gro und | tas exe cmp")
    repetitions.append(f"~ git sta exe → fil cha det | ana run nee")

test_content = '\n'.join(repetitions)

print("Testing High-Confidence Autonomous Learning...")
print("=" * 70)
print(f"Content length: {len(test_content)} chars")
print(f"Repetitions: 12 of each pattern")
print()

harness = ProductionHarness(enable_real_llm=False)

request = OracleRequest(
    type=OracleType.CTX_REDUCE,
    params={'content': test_content, 'threshold': 100}
)

response = harness.execute_oracle(request)

if response.success:
    result = response.result
    print("✓ Reduction with autonomous learning successful!")
    print()
    print("COMPRESSION:")
    print(f"  Original: {result['original_size']} chars")
    print(f"  Reduced: {result['reduced_size']} chars")
    print(f"  Compression: {(1 - result['compression_ratio']) * 100:.1f}%")
    print(f"  Patterns merged: {result['patterns_merged']}")
    print()

    if 'autonomous_learning' in result:
        learning = result['autonomous_learning']
        print("AUTONOMOUS LEARNING:")
        print(f"  Patterns discovered: {learning['patterns_discovered']}")
        print(f"  Rules proposed: {learning['rules_proposed']}")
        print(f"  Rules APPLIED (auto): {learning['rules_applied']}")
        print(f"  Total learned rules: {learning['total_learned_rules']}")
        print()

    # Show learned rules
    if harness.learned_rules:
        print(f"LEARNED REDUCTION RULES ({len(harness.learned_rules)}):")
        for pattern, reduction in list(harness.learned_rules.items())[:5]:
            print(f"  '{pattern}' → '{reduction}'")
        if len(harness.learned_rules) > 5:
            print(f"  ... and {len(harness.learned_rules) - 5} more")
        print()

    print("REDUCED CONTENT (first 500 chars):")
    print("=" * 70)
    print(result['reduced'][:500])
    if len(result['reduced']) > 500:
        print(f"\n... ({len(result['reduced']) - 500} more chars)")
    print("=" * 70)
else:
    print(f"✗ Reduction failed: {response.error}")
