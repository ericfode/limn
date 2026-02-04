def oracle(req):
  return req

type Oracle:
  Semantic { prompt: String }

def think(prompt):
  return oracle(Oracle/Semantic { prompt: prompt })

def main():
  return think("con bet joi | mea eme int")
