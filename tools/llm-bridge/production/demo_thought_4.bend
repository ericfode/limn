def oracle(req):
  return req

type Oracle:
  Semantic { prompt: String }

def think(prompt):
  return oracle(Oracle/Semantic { prompt: prompt })

def main():
  # Pure Limn prompt: "self transform grow | meaning calculate new"
  return think("sel tra gro | mea cal new")
