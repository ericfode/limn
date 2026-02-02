# Oracle Protocol for HVM/Bend
# Pure text-based oracle request generation
# NO FFI - all side effects happen in Python harness

# ============================================================================
# String Utilities (for building requests)
# ============================================================================

# Convert number to string (simple implementation)
def num_to_str(n):
  if n == 0:
    return "0"
  else:
    if n < 0:
      return concat("-", num_to_str_pos(0 - n))
    else:
      return num_to_str_pos(n)

def num_to_str_pos(n):
  if n == 0:
    return ""
  else:
    digit = n % 10
    rest = n / 10
    return concat(num_to_str_pos(rest), digit_to_char(digit))

def digit_to_char(d):
  if d == 0:
    return "0"
  else:
    if d == 1:
      return "1"
    else:
      if d == 2:
        return "2"
      else:
        if d == 3:
          return "3"
        else:
          if d == 4:
            return "4"
          else:
            if d == 5:
              return "5"
            else:
              if d == 6:
                return "6"
              else:
                if d == 7:
                  return "7"
                else:
                  if d == 8:
                    return "8"
                  else:
                    return "9"

# String concatenation (placeholder - Bend has native support)
def concat(a, b):
  return String/concat(a, b)

# ============================================================================
# Oracle Request Builders
# ============================================================================

# Arithmetic Oracle: ADD
def oracle_add(a, b):
  a_str = num_to_str(a)
  b_str = num_to_str(b)
  request = concat("ORACLE:ARITH:ADD:", concat(a_str, concat(",", b_str)))
  return request

# Arithmetic Oracle: MUL
def oracle_mul(a, b):
  a_str = num_to_str(a)
  b_str = num_to_str(b)
  request = concat("ORACLE:ARITH:MUL:", concat(a_str, concat(",", b_str)))
  return request

# Arithmetic Oracle: SUB
def oracle_sub(a, b):
  a_str = num_to_str(a)
  b_str = num_to_str(b)
  request = concat("ORACLE:ARITH:SUB:", concat(a_str, concat(",", b_str)))
  return request

# Arithmetic Oracle: DIV
def oracle_div(a, b):
  a_str = num_to_str(a)
  b_str = num_to_str(b)
  request = concat("ORACLE:ARITH:DIV:", concat(a_str, concat(",", b_str)))
  return request

# Filesystem Oracle: READ
def oracle_fs_read(path):
  return concat("ORACLE:FS:READ:", path)

# Filesystem Oracle: WRITE
def oracle_fs_write(path, content):
  return concat("ORACLE:FS:WRITE:", concat(path, concat(":", content)))

# Filesystem Oracle: LIST
def oracle_fs_list(path):
  return concat("ORACLE:FS:LIST:", path)

# LLM Oracle: COMPLETE
def oracle_llm_complete(prompt):
  return concat("ORACLE:LLM:COMPLETE:", prompt)

# LLM Oracle: CLASSIFY
def oracle_llm_classify(text, categories):
  return concat("ORACLE:LLM:CLASSIFY:", concat(text, concat("|", categories)))

# ============================================================================
# Example Usage Pattern
# ============================================================================

# A program that uses oracles would:
# 1. Do pure computation
# 2. Generate oracle request (text)
# 3. Emit the request to stdout
# 4. Python harness reads request, executes, returns result
# 5. HVM continues with result

# Note: In a real implementation, we'd need a way to:
# - Emit requests to stdout
# - Read responses from stdin
# - Parse text responses back to values
#
# For now, this demonstrates the protocol structure.

# ============================================================================
# Test/Demo
# ============================================================================

def main():
  # Generate some oracle requests
  req1 = oracle_add(5, 3)
  req2 = oracle_mul(7, 6)
  req3 = oracle_fs_read("/tmp/test.txt")
  req4 = oracle_llm_complete("What is the meaning of life?")

  # In a real system, these would be emitted to stdout
  # For demonstration, just return the first request
  return req1
