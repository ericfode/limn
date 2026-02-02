# Oracle Example - Bend program that needs external input
# =========================================================
# This program represents an LMN Oracle call (~) that yields
# control to the host to get an LLM response

type OracleRequest:
  # Request with prompt and context
  Ask { prompt, context }
  # Oracle response (injected by host)
  Response { value }

# Oracle operation - this is where we need host intervention
def oracle(prompt, context):
  # In a real implementation, this would:
  # 1. Yield to host with (prompt, context)
  # 2. Host calls LLM API
  # 3. Host injects response back
  # 4. Execution continues

  # For now, return a request marker
  return OracleRequest/Ask { prompt: prompt, context: context }

# Simple LMN program with oracle call
def lmn_program():
  # First step: deterministic computation
  prompt = "translate: cod flo log"
  context = "Limn vocabulary"

  # Second step: oracle call (needs LLM)
  oracle_result = oracle(prompt, context)

  # Third step: continue with result
  # (in real version, this would use oracle_result)
  return oracle_result

def main():
  result = lmn_program()
  return result
