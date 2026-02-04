# Stateful Oracle Example - Multiple oracle calls with state flow
# ================================================================
# This demonstrates a more realistic LMN program:
# 1. Pure computation
# 2. Oracle call (LLM needed)
# 3. More pure computation using oracle result
# 4. Another oracle call
# 5. Final result

type OracleRequest:
  Ask { prompt, context }
  Response { value }

type ComputeState:
  Step1 { data }
  Step2 { data, oracle_result }
  Step3 { data, oracle_result1, oracle_result2 }
  Done { result }

# Deterministic computation step
def pure_compute(input):
  # In real LMN, this would be temporal/collapse operators
  return input

# Oracle wrapper
def oracle(prompt, context):
  return OracleRequest/Ask { prompt: prompt, context: context }

# Multi-step program with interleaved oracle calls
def lmn_multistep():
  # Step 1: Pure computation
  step1_data = "initial state"

  # Step 2: First oracle call
  oracle1 = oracle("analyze: initial state", "conversation")

  # Step 3: Pure computation (would use oracle1 result in real version)
  step2_data = "processed state"

  # Step 4: Second oracle call
  oracle2 = oracle("synthesize: processed state", "memory")

  # Return the final oracle request (in real version, would be complete state)
  return oracle2

def main():
  result = lmn_multistep()
  return result
