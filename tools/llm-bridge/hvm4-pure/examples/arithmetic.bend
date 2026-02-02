# Arithmetic Oracle Example
# Demonstrates pure HVM generating oracle requests

# This is a simplified example showing the concept.
# A full implementation would include I/O primitives for reading oracle results.

def main():
  # This demonstrates how a Bend program would generate oracle requests
  # In practice, these would be emitted to stdout for the harness to process

  # Request: Add 5 + 3
  let req1 = "ORACLE:ARITH:ADD:5,3"

  # Request: Multiply 7 * 6
  let req2 = "ORACLE:ARITH:MUL:7,6"

  # Request: Subtract 10 - 4
  let req3 = "ORACLE:ARITH:SUB:10,4"

  # Request: Divide 20 / 5
  let req4 = "ORACLE:ARITH:DIV:20,5"

  # In a complete system:
  # 1. Bend emits these to stdout
  # 2. Python harness reads, executes, returns RESULT:X
  # 3. Bend reads result and continues computation

  # For this demo, we return the first request to show the format
  return req1

# Expected output when piped through harness:
# RESULT:8
