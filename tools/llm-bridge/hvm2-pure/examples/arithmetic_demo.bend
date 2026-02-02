# Arithmetic Oracle Demo
# =======================
# Demonstrates pure HVM2 code emitting oracle requests
# All computation happens via oracle (even though HVM2 could do it)
# This proves the oracle pattern works

type OracleRequest:
  Add { a, b }
  Mul { a, b }
  Div { a, b }

# Emit oracle request for: (10 + 5) * 2
def main():
  # Step 1: Request 10 + 5
  req1 = OracleRequest/Add { a: 10, b: 5 }

  # Step 2: Would get response: 15
  # Step 3: Request 15 * 2
  req2 = OracleRequest/Mul { a: 15, b: 2 }

  # Step 4: Would get response: 30

  # Return the requests (in practice, harness executes them)
  return req2

# To test:
# $ python harness.py '{"oracle": "arithmetic", "operation": "add", "args": {"a": 10, "b": 5}}'
# $ python harness.py '{"oracle": "arithmetic", "operation": "mul", "args": {"a": 15, "b": 2}}'
