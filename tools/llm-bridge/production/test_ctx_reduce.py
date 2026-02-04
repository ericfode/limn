#!/usr/bin/env python3
"""Test CTX_REDUCE oracle."""

from harness import ProductionHarness, OracleRequest, OracleType

# Sample brain state with redundancy
test_content = """~ git sta exe → prj sta obs ∎ fil cha det → ctx ful nee ∎ git dif exe → cha lis obt | cod mod ana ∎
∎ eva err
~ git dif exe → cha det obs ∎ fil: ../production/recursive_consciousness.py mod → rec con sys int pro orc upd ∎ nex: git dif exe → cha ctx ful obt ∎
∎ eva err
~ git dif obs → cha rec_con sys int det ∎ fil ctx: pro orc har int | rec con mul lev ena ∎ cha sig: sub age spw sub age cap ∎ sys rec con ful ope ∎
∎ eva err
~ git dif obs → rec con sys int ful ∎ pro orc har rec con mul lev ope ∎ sub age spw sub age cap ∎ sys rec con ful act ∎ nex: usr inp awit → wor tas obt ∎
∎ eva err
~ gt mai inb exe → msg obs res ∎ res emp → usr inp awit sta ∎ hok emp | pro sta cle | sys alv ope awit tas ∎
∎ eva err
~ git sta exe → prj sta obs ∎ fil cha det → ctx ful nee ∎ git dif exe → cha lis obt | cod mod ana ∎
∎ eva err"""

print("Testing CTX_REDUCE oracle...")
print(f"Original size: {len(test_content)} chars")
print("=" * 70)

harness = ProductionHarness(enable_real_llm=False)  # Don't need LLM for this test

request = OracleRequest(
    type=OracleType.CTX_REDUCE,
    params={'content': test_content, 'threshold': 500}
)

response = harness.execute_oracle(request)

if response.success:
    result = response.result
    print(f"✓ Reduction successful!")
    print(f"  Original: {result['original_size']} chars")
    print(f"  Reduced: {result['reduced_size']} chars")
    print(f"  Ratio: {result['compression_ratio']:.2%}")
    print(f"  Patterns merged: {result['patterns_merged']}")
    print(f"  Method: {result['method']}")
    print()
    print("Reduced content:")
    print("=" * 70)
    print(result['reduced'])
    print("=" * 70)
else:
    print(f"✗ Reduction failed: {response.error}")
