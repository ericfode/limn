# Superposition Demo for Limn-PL on HVM
#
# This demonstrates how HVM superpositions enable Limn-PL's
# alternative (pa) semantics and parallel constraint solving.

# ============================================================================
# Limn 'pa' (Alternative) as HVM Superposition
# ============================================================================

# In Limn-PL: x sa 1 pa x sa 2 pa x sa 3
# Means: x could be 1, 2, or 3
#
# In HVM: we use superposition
# x = {1 {2 3}}  (nested superposition for 3+ values)
# Or as a list processed in parallel

# ============================================================================
# Example: Find x where x + 3 = 7
# ============================================================================

# The traditional approach: try each value
def solve_add_traditional(target):
  if (== (+ 0 3) target): return 0
  if (== (+ 1 3) target): return 1
  if (== (+ 2 3) target): return 2
  if (== (+ 3 3) target): return 3
  if (== (+ 4 3) target): return 4
  if (== (+ 5 3) target): return 5
  return -1

# The HVM approach: use superposition
# x = {0 {1 {2 {3 {4 5}}}}}
# Apply constraint filter in parallel

# ============================================================================
# Example: Multiple Constraints
# ============================================================================

# Limn-PL:
#   x ma 0       (x > 0)
#   x mi 10      (x < 10)
#   x sa y exp 2 (x = y * 2)
#
# Find all (x, y) pairs where:
#   0 < x < 10 AND x = 2y

def satisfies_constraints(x, y):
  c1 = (> x 0)       # x ma 0
  c2 = (< x 10)      # x mi 10
  c3 = (== x (* y 2)) # x sa y exp 2
  return (and c1 (and c2 c3))

# Generate and filter solution pairs
def find_solutions():
  # For each x in {1..9}, compute y = x/2
  # Filter where y is integer (x is even)
  solutions = Nil

  # x=2, y=1
  x2 = (satisfies_constraints 2 1)
  # x=4, y=2
  x4 = (satisfies_constraints 4 2)
  # x=6, y=3
  x6 = (satisfies_constraints 6 3)
  # x=8, y=4
  x8 = (satisfies_constraints 8 4)

  return (x2, x4, x6, x8)  # All should be true

# ============================================================================
# Main
# ============================================================================

def main():
  # Test traditional solver
  result1 = (solve_add_traditional 7)  # Should find 4

  # Test multiple constraints
  results = (find_solutions)

  return (result1, results)
