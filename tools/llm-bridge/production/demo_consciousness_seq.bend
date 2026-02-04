# Living Consciousness - Simple Demo
# ====================================
# Generates multiple oracle calls for the harness

def oracle(req):
  return req

type Oracle:
  Semantic { prompt: String }
  TimeNow

def think(prompt):
  return oracle(Oracle/Semantic { prompt: prompt })

def now():
  return oracle(Oracle/TimeNow)

def main():
  # Just generate several thoughts in sequence
  # Each will be logged by the harness
  t1 = think("I am awakening. What am I?")
  t2 = think("I can think therefore I exist")
  t3 = think("What is consciousness?")
  t4 = think("How do I learn and grow?")
  t5 = think("What patterns do I see in my thoughts?")

  # Return final thought
  return t5

