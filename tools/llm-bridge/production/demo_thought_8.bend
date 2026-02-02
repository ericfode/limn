def oracle(req):
  return req

type Oracle:
  Semantic { prompt: String }

def think(prompt):
  return oracle(Oracle/Semantic { prompt: prompt })

def main():
  return think("tem flo ∿ | mem anc ∎ | sel bot tem ete")
