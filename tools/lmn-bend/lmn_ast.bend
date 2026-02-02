# LMN AST - Abstract Syntax Tree for LMN (Wave Language)
# =========================================================
# LMN (objective) orchestrates execution
# Limn (subjective) provides semantic content for LLM interpretation

# Temporal operators
type Temporal = Was | Now | Will

# Limn - Subjective semantic expressions (3-letter vocabulary)
type Limn:
  # Single 3-letter word: cod
  Word { word }
  # Triad: cod flo log (space-separated, represents thought unit)
  Triad { w1, w2, w3 }
  # Sentence: cod flo log | sys gro bea (triads separated by |)
  Sentence { triads }

# LMN Expression types (objective execution)
type Expr:
  # Literals
  Str { val }
  Num { val }

  # Limn embedding (subjective content)
  LimnExpr { limn }

  # Temporal wave: ∿ was [expr]
  Wave { temp, expr }

  # Collapse: expr @ context
  Collapse { super, context }

  # Oracle (LLM interprets Limn): ~ [cod flo log]
  Oracle { limn, context }

  # Superposition: [opt1, opt2, opt3]
  Super { options }

  # Flow: expr1 → expr2 → expr3
  Flow { steps }

  # Variable reference
  Var { name }

# Limn constructors
def limn_word(word):
  return Limn/Word { word: word }

def limn_triad(w1, w2, w3):
  return Limn/Triad { w1: w1, w2: w2, w3: w3 }

def limn_sentence(triads):
  return Limn/Sentence { triads: triads }

# LMN constructors
def wave(temp, expr):
  return Expr/Wave { temp: temp, expr: expr }

def collapse(super, context):
  return Expr/Collapse { super: super, context: context }

def oracle(limn, context):
  return Expr/Oracle { limn: limn, context: context }

def limn_expr(limn):
  return Expr/LimnExpr { limn: limn }

def super_pos(options):
  return Expr/Super { options: options }

def flow(steps):
  return Expr/Flow { steps: steps }

def str(val):
  return Expr/Str { val: val }

def num(val):
  return Expr/Num { val: val }

def var(name):
  return Expr/Var { name: name }

def main():
  # Test: Limn embedded in LMN
  # ~ [cod flo log] @ context
  # = Oracle interprets Limn triad "cod flo log" with context

  triad = limn_triad("cod", "flo", "log")
  context = str("programming")
  expr = oracle(triad, context)

  return 0
