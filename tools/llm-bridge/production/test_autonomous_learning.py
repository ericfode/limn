#!/usr/bin/env python3
"""Test autonomous pattern learning."""

from harness import ProductionHarness, OracleRequest, OracleType

# Sample content with recurring patterns
test_content = """~ cod flo log → sys gro und | mod rea exe
~ cod flo log → sys gro und | tes run suc
~ cod flo log → sys gro und | ver cmp pas
~ git sta exe → fil cha det | dif ana nee
~ git sta exe → fil cha det | cmt rea per
~ git sta exe → fil cha det | pus exe nex
~ mem acc ctx → pat eme det | kno gro ext
~ mem acc ctx → pat eme det | lea rul cre
~ mem acc ctx → pat eme det | red opt ena
~ tho exe seq → con ref res | act det gen
~ tho exe seq → con ref res | ora qry run
~ tho exe seq → con ref res → min gro sta"""

print("Testing Autonomous Learning...")
print("=" * 70)
print(f"Content length: {len(test_content)} chars")
print()

harness = ProductionHarness(enable_real_llm=False)

request = OracleRequest(
    type=OracleType.CTX_REDUCE,
    params={'content': test_content, 'threshold': 100}
)

response = harness.execute_oracle(request)

if response.success:
    result = response.result
    print("✓ Reduction with pattern learning successful!")
    print()
    print("COMPRESSION:")
    print(f"  Original: {result['original_size']} chars")
    print(f"  Reduced: {result['reduced_size']} chars")
    print(f"  Ratio: {result['compression_ratio']:.2%}")
    print(f"  Patterns merged: {result['patterns_merged']}")
    print()

    if 'autonomous_learning' in result:
        learning = result['autonomous_learning']
        print("AUTONOMOUS LEARNING:")
        print(f"  Patterns discovered: {learning['patterns_discovered']}")
        print(f"  Rules proposed: {learning['rules_proposed']}")
        print(f"  Rules applied (auto): {learning['rules_applied']}")
        print(f"  Total learned rules: {learning['total_learned_rules']}")
        print()

    print("REDUCED CONTENT:")
    print("=" * 70)
    print(result['reduced'])
    print("=" * 70)
    print()

    # Check learned rules
    if harness.learned_rules:
        print("LEARNED REDUCTION RULES:")
        for pattern, reduction in harness.learned_rules.items():
            print(f"  '{pattern}' → '{reduction}'")
        print()

    # Check proposal log
    import os
    log_path = harness.rule_proposals_path
    if os.path.exists(log_path):
        print(f"Rule proposals logged to: {log_path}")
        print("View with: tail -20 pattern_discoveries.log")
else:
    print(f"✗ Reduction failed: {response.error}")
