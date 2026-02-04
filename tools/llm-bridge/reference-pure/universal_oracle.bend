# Universal Oracle - Reference Implementation
# ===========================================
# Demonstrates pure text→text architecture
# All side effects handled by harness

type Oracle:
  # Semantic understanding (~ operator - delegate to LLM)
  Semantic { prompt, context }

  # File system (∎ operator - ground truth)
  FileRead { path }
  FileExists { path }

  # Temporal (∿ operator - time dimension)
  TimeNow
  TimeSleep { duration }

  # Database (∎ operator - persisted facts)
  DbQuery { query, db }

  # Network (∎ operator - external data)
  HttpGet { url }

  # Computation with semantic help (~ operator)
  Compute { op, args }

# Core oracle function - pure, no side effects
def oracle(req):
  return req

# Example 1: Semantic oracle (LLM reasoning)
def semantic_example():
  return oracle(Oracle/Semantic {
    prompt: "add 1 1",
    context: "arithmetic"
  })

# Example 2: File oracle (ground truth)
def file_example():
  return oracle(Oracle/FileRead {
    path: "/etc/hostname"
  })

# Example 3: Temporal oracle (time dimension)
def time_example():
  return oracle(Oracle/TimeNow)

# Example 4: Compute oracle (semantic computation)
def compute_example():
  return oracle(Oracle/Compute {
    op: "translate",
    args: "cod flo log"
  })

# Example 5: Multiple oracles in sequence
def multi_oracle():
  # In a real implementation with continuation:
  # time = oracle(Oracle/TimeNow)
  # file = oracle(Oracle/FileRead { path: "config.json" })
  # result = oracle(Oracle/Semantic {
  #   prompt: concat("At time ", time, " config is ", file),
  #   context: "analysis"
  # })

  # For now, just show the pattern
  return oracle(Oracle/Semantic {
    prompt: "demonstrate multi-oracle pattern",
    context: "architecture"
  })

# Main entry point
def main():
  # Generate different oracle requests
  # Each one is pure data - no side effects executed in HVM

  sem = semantic_example()
  # Output: Oracle/Semantic{...}

  file = file_example()
  # Output: Oracle/FileRead{...}

  time = time_example()
  # Output: Oracle/TimeNow

  compute = compute_example()
  # Output: Oracle/Compute{...}

  multi = multi_oracle()
  # Output: Oracle/Semantic{...}

  # Return all oracle requests
  # Harness will parse and execute each one
  return (sem, file, time, compute, multi)
