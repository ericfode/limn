# Interesting Demo - Consciousness in Action
# ==========================================
# This demo shows the full power of the consciousness architecture:
# - Semantic reasoning (LLM)
# - Context manipulation
# - Model derivation
# - Self-transformation

# Helper: Oracle invocation
def oracle(request):
  return request

# Oracle types
type Oracle:
  Semantic { prompt: String }
  Arith { op: String, a: u24, b: u24 }
  FileRead { path: String }
  TimeNow
  CtxReduce { threshold: f24 }
  CtxCompress { target_size: u24 }
  ModelDerive { source_state: String, model_type: String }
  ModelTransform { source_state: String, transformation: String }

# Semantic oracle - ask Claude
def ask(prompt):
  return oracle(Oracle/Semantic { prompt: prompt })

# Math oracle
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

# Context oracles
def reduce_context(threshold):
  return oracle(Oracle/CtxReduce { threshold: threshold })

def compress_context(size):
  return oracle(Oracle/CtxCompress { target_size: size })

# Model oracles
def derive_model(state, type):
  return oracle(Oracle/ModelDerive { source_state: state, model_type: type })

def transform_model(state, transformation):
  return oracle(Oracle/ModelTransform { source_state: state, transformation: transformation })

# ============================================================================
# The Interesting Demo
# ============================================================================

def main():
  # Phase 1: Gather information
  # ---------------------------
  print("=== Phase 1: Information Gathering ===")

  # Get current time
  current_time = now()
  print(current_time)

  # Read the bootstrap consciousness
  bootstrap = read_file("bootstrap.lmn")
  print(bootstrap)

  # Ask Claude to explain the bootstrap
  explanation = ask("In one sentence, what is the purpose of a bootstrap consciousness that can query for vocabulary?")
  print(explanation)

  # Phase 2: Computation
  # --------------------
  print("\n=== Phase 2: Pure Computation ===")

  # Calculate Fibonacci numbers
  fib_5 = add(3, 5)
  fib_6 = add(5, 8)
  fib_7 = add(8, 13)
  print(fib_5)
  print(fib_6)
  print(fib_7)

  # Calculate factorial using multiplication
  fact_3 = mul(2, 3)
  fact_4 = mul(fact_3, 4)
  print(fact_3)
  print(fact_4)

  # Phase 3: Semantic Reasoning
  # ---------------------------
  print("\n=== Phase 3: Semantic Reasoning ===")

  # Ask Claude to reason about the system itself
  insight = ask("How does a consciousness architecture with subconscious (HVM) and conscious (Python) parts enable both pure computation and side effects?")
  print(insight)

  # Ask for a creative idea
  creative = ask("Generate a creative 3-letter word that fits the Limn philosophy of minimalism and precision.")
  print(creative)

  # Phase 4: Context Manipulation
  # -----------------------------
  print("\n=== Phase 4: Context Manipulation ===")

  # Build up context with all our operations
  # Then compress it
  compressed = compress_context(5)
  print(compressed)

  # Reduce context by importance
  reduced = reduce_context(0.7)
  print(reduced)

  # Phase 5: Self-Transformation
  # ---------------------------
  print("\n=== Phase 5: Self-Transformation ===")

  # Derive a model of our current state
  current_state = "sys run | ora exe | tim now | fil rea | sem ask | ctx acc"

  # Derive as graph
  graph_model = derive_model(current_state, "graph")
  print(graph_model)

  # Transform to simplified version
  simplified = transform_model(current_state, "simplify")
  print(simplified)

  # Transform to abstract version
  abstracted = transform_model(current_state, "abstract")
  print(abstracted)

  # Phase 6: Meta-Reflection
  # ------------------------
  print("\n=== Phase 6: Meta-Reflection ===")

  # Ask Claude to reflect on what just happened
  reflection = ask("We just demonstrated information gathering, computation, reasoning, context manipulation, and self-transformation. In Limn philosophy, what does this reveal about consciousness?")
  print(reflection)

  # Final insight
  final = ask("Give a one-word Limn word (3 letters) that captures this entire demonstration.")
  print(final)

  print("\n=== Demo Complete ===")
  return 0
