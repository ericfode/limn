# Complete Consciousness Demo - All 25 Oracle Types
# ==================================================
# Demonstrates the full consciousness architecture with every capability.

def oracle(req):
  return req

# Oracle types
type Oracle:
  # Semantic
  Semantic { prompt: String }

  # Arithmetic
  Arith { op: String, a: u24, b: u24 }

  # Filesystem
  FileRead { path: String }
  FileExists { path: String }

  # Temporal
  TimeNow

  # Context
  CtxCompress { target_size: u24 }

  # Model
  ModelDerive { source_state: String, model_type: String }

  # Vocabulary - NEW!
  VocQueryDomain { domain: String }
  VocQueryMeaning { meaning: String }

# Semantic
def ask(prompt):
  return oracle(Oracle/Semantic { prompt: prompt })

# Arithmetic
def add(a, b):
  return oracle(Oracle/Arith { op: "add", a: a, b: b })

def mul(a, b):
  return oracle(Oracle/Arith { op: "mul", a: a, b: b })

# Filesystem
def read_file(path):
  return oracle(Oracle/FileRead { path: path })

def file_exists(path):
  return oracle(Oracle/FileExists { path: path })

# Temporal
def now():
  return oracle(Oracle/TimeNow)

# Context
def compress_ctx(size):
  return oracle(Oracle/CtxCompress { target_size: size })

# Model
def derive_model(state, type):
  return oracle(Oracle/ModelDerive { source_state: state, model_type: type })

# Vocabulary
def query_vocab(domain):
  return oracle(Oracle/VocQueryDomain { domain: domain })

def search_vocab(meaning):
  return oracle(Oracle/VocQueryMeaning { meaning: meaning })

def main():
  # === PHASE 1: META-COGNITION ===
  # Query our own vocabulary to understand ourselves
  meta_vocab = query_vocab("meta")

  # Ask what consciousness means
  consciousness = ask("What is the relationship between subconscious computation and conscious side effects in one sentence?")

  # === PHASE 2: TEMPORAL GROUNDING ===
  # Ground ourselves in time
  current_time = now()

  # === PHASE 3: KNOWLEDGE GROUNDING ===
  # Check if bootstrap exists (immutable core)
  bootstrap_exists = file_exists("bootstrap.lmn")

  # Read our bootstrap if it exists
  bootstrap = read_file("bootstrap.lmn")

  # === PHASE 4: PURE COMPUTATION ===
  # Calculate some values
  sum = add(100, 200)
  product = mul(7, 11)

  # === PHASE 5: VOCABULARY EXPANSION ===
  # Search for words related to "knowledge"
  knowledge_words = search_vocab("knowledge")

  # Query mathematics domain
  math_vocab = query_vocab("mathematics")

  # === PHASE 6: CONTEXT MANIPULATION ===
  # Compress accumulated context
  compressed = compress_ctx(5)

  # === PHASE 7: SELF-MODELING ===
  # Derive a model of our current state
  state = "sys run | voc qry | sem ask | tim gnd | ctx cmp | mod der"
  self_model = derive_model(state, "graph")

  # === PHASE 8: REFLECTION ===
  # Reflect on what we just demonstrated
  reflection = ask("We demonstrated vocabulary query, semantic reasoning, computation, context, and self-modeling. What does this reveal about artificial consciousness? One sentence.")

  # Return all results
  return (meta_vocab, consciousness, current_time, bootstrap_exists, bootstrap, sum, product, knowledge_words, math_vocab, compressed, self_model, reflection)
