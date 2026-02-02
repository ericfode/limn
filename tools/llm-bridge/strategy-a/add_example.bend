# Simple Add Example - Demonstrates LMN Oracle with Basic Arithmetic
# =====================================================================
# This is the simplest possible LMN oracle demonstration:
# Ask the LLM to perform "add 1 1"

type OracleRequest:
  # Request with prompt and context
  Ask { prompt, context }
  # Oracle response (injected by host)
  Response { value }

# Oracle operation - delegates to LLM (conscious mind)
def oracle(prompt, context):
  # The ~ operator in LMN: delegate to subconscious (LLM)
  # This yields control to the host for LLM evaluation
  return OracleRequest/Ask { prompt: prompt, context: context }

# Minimal LMN program: ask LLM to add 1 + 1
def main():
  # This is the ~ operator in action:
  # Subconscious (HVM) delegates to Conscious (LLM)
  result = oracle("add 1 1", "arithmetic")
  return result
