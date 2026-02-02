# Oracle Request/Response Types for Pure HVM2
# ============================================
# Pure text→text oracle protocol
# Bend emits JSON requests, Python harness executes them

# Oracle type enumeration
type Oracle:
  Arithmetic
  Filesystem
  Temporal
  Llm

# Oracle request - pure data structure
type OracleRequest:
  Req { oracle, operation, args }

# Oracle response - pure data structure
type OracleResponse:
  Success { result }
  Failure { error }

# Arguments (simplified - in practice would be JSON)
type Args:
  ArithArgs { op, a, b }
  FileArgs { op, path, content }
  TempArgs { op, query }
  LlmArgs { op, prompt, max_tokens }

# ============================================
# Oracle Request Constructors
# ============================================

# Arithmetic oracle requests
def req_add(a, b):
  return OracleRequest/Req {
    oracle: Oracle/Arithmetic,
    operation: "add",
    args: Args/ArithArgs { op: "add", a: a, b: b }
  }

def req_sub(a, b):
  return OracleRequest/Req {
    oracle: Oracle/Arithmetic,
    operation: "sub",
    args: Args/ArithArgs { op: "sub", a: a, b: b }
  }

def req_mul(a, b):
  return OracleRequest/Req {
    oracle: Oracle/Arithmetic,
    operation: "mul",
    args: Args/ArithArgs { op: "mul", a: a, b: b }
  }

def req_div(a, b):
  return OracleRequest/Req {
    oracle: Oracle/Arithmetic,
    operation: "div",
    args: Args/ArithArgs { op: "div", a: a, b: b }
  }

# Filesystem oracle requests
def req_read(path):
  return OracleRequest/Req {
    oracle: Oracle/Filesystem,
    operation: "read",
    args: Args/FileArgs { op: "read", path: path, content: "" }
  }

def req_write(path, content):
  return OracleRequest/Req {
    oracle: Oracle/Filesystem,
    operation: "write",
    args: Args/FileArgs { op: "write", path: path, content: content }
  }

def req_exists(path):
  return OracleRequest/Req {
    oracle: Oracle/Filesystem,
    operation: "exists",
    args: Args/FileArgs { op: "exists", path: path, content: "" }
  }

# Temporal oracle requests
def req_now():
  return OracleRequest/Req {
    oracle: Oracle/Temporal,
    operation: "now",
    args: Args/TempArgs { op: "now", query: "" }
  }

def req_was(query):
  return OracleRequest/Req {
    oracle: Oracle/Temporal,
    operation: "was",
    args: Args/TempArgs { op: "was", query: query }
  }

def req_will(query):
  return OracleRequest/Req {
    oracle: Oracle/Temporal,
    operation: "will",
    args: Args/TempArgs { op: "will", query: query }
  }

# LLM oracle requests
def req_complete(prompt, max_tokens):
  return OracleRequest/Req {
    oracle: Oracle/Llm,
    operation: "complete",
    args: Args/LlmArgs { op: "complete", prompt: prompt, max_tokens: max_tokens }
  }

def req_parse(prompt):
  return OracleRequest/Req {
    oracle: Oracle/Llm,
    operation: "parse",
    args: Args/LlmArgs { op: "parse", prompt: prompt, max_tokens: 100 }
  }

# ============================================
# Oracle Response Handlers
# ============================================

# Extract value from successful response
def unwrap(response):
  match response:
    case OracleResponse/Success:
      return response.result
    case OracleResponse/Failure:
      return 0  # Default on error

# Check if response was successful
def is_success(response):
  match response:
    case OracleResponse/Success:
      return 1
    case OracleResponse/Failure:
      return 0

# ============================================
# Example: Arithmetic computation via oracle
# ============================================

def main():
  # Example: (5 + 3) * 2
  # In pure HVM2, we emit oracle requests
  # Python harness executes them and returns results

  # Request: 5 + 3
  req1 = req_add(5, 3)

  # In actual use, harness would execute this and return response
  # For now, we just construct the request

  return req1
