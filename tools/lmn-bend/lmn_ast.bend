# LMN AST - LMN is a strict subset of Limn
# =========================================================
# Everything is Limn (3-letter vocabulary)
# LMN adds execution semantics to certain Limn patterns

# Temporal operators (Limn words with execution meaning)
type Temporal = Was | Now | Will

# Limn Expression - The universal type
# All LMN constructs are Limn expressions
type Limn:
  # Atomic Limn: 3-letter word
  Word { word }

  # Triad: w1 w2 w3 (space-separated thought unit)
  Triad { w1, w2, w3 }

  # Sentence: triads separated by |
  Sentence { triads }

  # LMN executable patterns (still Limn, but with semantics)

  # Temporal wave: ∿ <temporal> [limn]
  # ∿ is itself a Limn construct
  Wave { temp, limn }

  # Collapse: limn @ context
  # @ is contextual resolution operator
  Collapse { limn, context }

  # Oracle: ~ limn @ context
  # ~ marks LLM evaluation point
  Oracle { limn, context }

  # Superposition: [limn1, limn2, limn3]
  # Multiple possibilities held simultaneously
  Super { options }

  # Flow: limn1 → limn2 → limn3
  # → is sequential composition
  Flow { steps }

  # Literals (for bootstrapping, but prefer Limn)
  Str { val }
  Num { val }

# Constructors

def word(w):
  return Limn/Word { word: w }

def triad(w1, w2, w3):
  return Limn/Triad { w1: w1, w2: w2, w3: w3 }

def sentence(triads):
  return Limn/Sentence { triads: triads }

def wave(temp, limn):
  return Limn/Wave { temp: temp, limn: limn }

def collapse(limn, context):
  return Limn/Collapse { limn: limn, context: context }

def oracle(limn, context):
  return Limn/Oracle { limn: limn, context: context }

def super_pos(options):
  return Limn/Super { options: options }

def flow(steps):
  return Limn/Flow { steps: steps }

def str(val):
  return Limn/Str { val: val }

def num(val):
  return Limn/Num { val: val }

def main():
  # Example LMN program (all in Limn)
  # ∿ now [cod flo log] @ context

  # cod flo log = code flows clearly (Limn triad)
  thought = triad("cod", "flo", "log")

  # ∿ now = temporal wave in present
  present = wave(Temporal/Now, thought)

  # @ programming = collapse with context
  context = word("programming")
  expr = collapse(present, context)

  return 0
