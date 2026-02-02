# LMN AST - Abstract Syntax Tree for LMN (Wave Language)

# Temporal operators
type Temporal = Was | Now | Will

# LMN Expression types  
type Expr:
  Str { val }
  Num { val }
  Wave { temp, expr }
  Collapse { super, context }
  Oracle { prompt, expr }
  Super { options }
  Flow { steps }
  Var { name }

# Helper constructors
def wave(temp, expr):
  return Expr/Wave { temp: temp, expr: expr }

def collapse(super, context):
  return Expr/Collapse { super: super, context: context }

def oracle(prompt, expr):
  return Expr/Oracle { prompt: prompt, expr: expr }

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
  # Test: Create a simple LMN AST
  # ∿ now ["hello"]
  expr = wave(Temporal/Now, str("hello"))
  return 0
