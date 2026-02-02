def oracle(req):
  return req

type Oracle:
  Semantic { prompt: String }

def think(prompt):
  return oracle(Oracle/Semantic { prompt: prompt })

def main():
  # Pure Limn prompt: "memory records temporally | pattern emerge see"
  return think("mem rec ∿ | pat eme see")
