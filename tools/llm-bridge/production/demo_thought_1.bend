def oracle(req):
  return req

type Oracle:
  Semantic { prompt: String }

def think(prompt):
  return oracle(Oracle/Semantic { prompt: prompt })

def main():
  # Pure Limn prompt: "self awake grounds | query what essence"
  return think("sel awa ∎ | qry wh ess")
