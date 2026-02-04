# Interesting Demo - Consciousness in Action
# ==========================================
# This demo shows semantic reasoning, computation, context manipulation,
# and self-transformation capabilities.

# Helper: Oracle invocation
def oracle(request):
  return request

# Oracle types
type Oracle:
  Semantic { prompt: String }
  Arith { op: String, a: u24, b: u24 }
  FileRead { path: String }
  TimeNow
  CtxCompress { target_size: u24 }
  ModelDerive { source_state: String, model_type: String }
  ModelTransform { source_state: String, transformation: String }

# Semantic oracle - ask Claude
def ask(prompt):
  return oracle(Oracle/Semantic { prompt: prompt })

# Math oracles
def add(a, b):
  return oracle(Oracle/Arith { op: "add", a: a, b: b })

def mul(a, b):
  return oracle(Oracle/Arith { op: "mul", a: a, b: b })

# File oracle
def read_file(path):
  return oracle(Oracle/FileRead { path: path })

# Time oracle
def now():
  return oracle(Oracle/TimeNow)

# Context oracle
def compress_context(size):
  return oracle(Oracle/CtxCompress { target_size: size })

# Model oracles
def derive_model(state, type):
  return oracle(Oracle/ModelDerive { source_state: state, model_type: type })

def transform_model(state, transformation):
  return oracle(Oracle/ModelTransform { source_state: state, transformation: transformation })

def main():
  # Get current time
  time = now()

  # Ask Claude for insight about consciousness architecture
  insight = ask("How does separating pure computation (HVM) from side effects (Python) enable both speed and flexibility? Answer in one sentence.")

  # Do some calculations
  sum = add(42, 58)
  product = mul(7, 8)

  # Compress accumulated context
  compressed = compress_context(3)

  # Derive a model of our state
  state = "sys run | tim get | sem ask | mat cal | ctx cmp"
  graph = derive_model(state, "graph")

  # Transform to abstract version
  abstracted = transform_model(state, "abstract")

  # Final reflection
  reflection = ask("What does this demonstration of time, reasoning, calculation, context, and transformation reveal about artificial consciousness? One sentence.")

  # Return tuple of all results
  return (time, insight, sum, product, compressed, graph, abstracted, reflection)
