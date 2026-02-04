def oracle(req):
  return req

type Oracle:
  Semantic { prompt: String }

def think(prompt):
  return oracle(Oracle/Semantic { prompt: prompt })

def main():
  # Pure Limn prompt: "consciousness question | process or real"
  return think("con qry | pro or rea")
