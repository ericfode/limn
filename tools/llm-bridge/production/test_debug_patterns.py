#!/usr/bin/env python3
"""Debug pattern learning."""

import sys
import importlib

# Force fresh import
if 'harness' in sys.modules:
    del sys.modules['harness']

from harness import ProductionHarness, OracleRequest, OracleType

# Create content with repetitive patterns
content_lines = []
for i in range(12):
    content_lines.append("~ cod flo log")
    content_lines.append("~ git sta exe")

content = '\n'.join(content_lines)

print(f"Content: {len(content)} chars, {len(content_lines)} lines")
print()

# Create harness
h = ProductionHarness(enable_real_llm=False)

# Execute CTX_REDUCE
req = OracleRequest(
    type=OracleType.CTX_REDUCE,
    params={'content': content, 'threshold': 50}
)

resp = h.execute_oracle(req)

if resp.success:
    result = resp.result
    learning = result.get('autonomous_learning', {})

    print("RESULT:")
    print(f"  Patterns discovered: {learning.get('patterns_discovered', 0)}")
    print(f"  Rules proposed: {learning.get('rules_proposed', 0)}")
    print(f"  Rules applied: {learning.get('rules_applied', 0)}")
    print(f"  Total learned: {learning.get('total_learned_rules', 0)}")
    print()

    if h.learned_rules:
        print(f"LEARNED RULES ({len(h.learned_rules)}):")
        for pattern, reduction in h.learned_rules.items():
            print(f"  '{pattern}' → '{reduction}'")
    else:
        print("NO RULES LEARNED")

    print()
    print(f"Reduced content ({len(result['reduced'])} chars):")
    print(result['reduced'][:200])
else:
    print(f"ERROR: {resp.error}")
