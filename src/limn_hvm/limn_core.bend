# Limn-PL Core Library for HVM/Bend
# TESTED and VERIFIED in Bend 0.2.38

# ============================================================================
# Basic Arithmetic (Limn-PL Bidirectional)
# ============================================================================

# joi (addition): a + b = c
def joi_forward(a, b):
  return a + b

# cut (subtraction): a - b = c
def cut_forward(a, b):
  return a - b

# exp (multiplication): a * b = c
def exp_forward(a, b):
  return a * b

# con (division): a / b = c
def con_forward(a, b):
  return a / b

# ============================================================================
# List Operations (Limn-PL)
# ============================================================================

# who: size of list
def who(list):
  match list:
    case List/Nil:
      return 0
    case List/Cons:
      return 1 + who(list.tail)

# fst: first element
def fst(list):
  match list:
    case List/Nil:
      return 0
    case List/Cons:
      return list.head

# nxt: tail/rest of list
def nxt(list):
  match list:
    case List/Nil:
      return List/Nil
    case List/Cons:
      return list.tail

# fin: last element
def fin(list):
  match list:
    case List/Nil:
      return 0
    case List/Cons:
      match list.tail:
        case List/Nil:
          return list.head
        case List/Cons:
          return fin(list.tail)

# par: index access
def par(i, list):
  match list:
    case List/Nil:
      return 0
    case List/Cons:
      if i == 0:
        return list.head
      else:
        return par(i - 1, list.tail)

# ============================================================================
# Temperature Conversion Example
# ============================================================================

# C to F: F = (C * 9 / 5) + 32
def celsius_to_fahrenheit(c):
  t1 = c * 9
  t2 = t1 / 5
  f = t2 + 32
  return f

# F to C: C = (F - 32) * 5 / 9
def fahrenheit_to_celsius(f):
  t1 = f - 32
  t2 = t1 * 5
  c = t2 / 9
  return c

# ============================================================================
# Superposition Examples (Limn pa → HVM {})
# ============================================================================

# Limn: x sa 1 pa x sa 2  →  Bend: x = {1, 2}
# Superpositions compute in parallel!

def double(x):
  return x * 2

def apply_to_alternatives():
  # x could be 1 or 2 (Limn: x sa 1 pa x sa 2)
  x = {1, 2}
  # Doubles both branches in parallel
  return double(x)  # Returns {2, 4}

# ============================================================================
# Main Test
# ============================================================================

def main():
  # Arithmetic tests
  add = joi_forward(3, 5)       # 8
  sub = cut_forward(10, 3)      # 7
  mul = exp_forward(4, 5)       # 20

  # Temperature tests
  f100 = celsius_to_fahrenheit(100)  # 212
  f0 = celsius_to_fahrenheit(0)      # 32

  # List tests
  test_list = [10, 20, 30]
  size = who(test_list)        # 3
  first = fst(test_list)       # 10
  last = fin(test_list)        # 30
  elem1 = par(1, test_list)    # 20

  # Superposition test
  sup = apply_to_alternatives()  # {2, 4}

  return (add, sub, mul, f100, f0, size, first, last, elem1, sup)
