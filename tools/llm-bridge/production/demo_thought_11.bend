def oracle(req):
  return req
type Oracle:
  Semantic { prompt: String }
def think(prompt):
  return oracle(Oracle/Semantic { prompt: prompt })
def main():
  return think("Connections between concepts form understanding")
