# Limn-PL Examples

This directory contains example `.limn` files that demonstrate the Limn-PL programming language.

## Running Examples

**Note:** Limn uses Prolog (engineer-approved). Python implementation is deprecated.

```bash
# Run with file's built-in key
swipl -s src/limn.pl -g "interpret_file('examples/addition.limn')"

# Run with verbose output
swipl -s src/limn.pl -g "interpret_file('examples/addition.limn', [verbose(true)])"

# Override key from command line
swipl -s src/limn.pl -g "interpret_file('examples/addition.limn', [key('a sa 100 | b sa 200')])"

# Use JSON input file
swipl -s src/limn.pl -g "interpret_file('examples/addition.limn', [input('examples/input.json')])"

# Output as JSON
swipl -s src/limn.pl -g "interpret_file('examples/addition.limn', [output(json)])"
```

**Note:** Prolog command syntax is illustrative. Consult actual implementation for exact usage.

## File Format

```limn
# Comments start with #

# Declare variables with 'whe' (unknown)
whe x
whe y
whe result

# Write constraints
x joi y sa result

---
# Key section (input values)
x sa 10
y sa 20
```

## Examples

| File | Description | Key | Result |
|------|-------------|-----|--------|
| `addition.limn` | Basic addition: a + b = c | a=10, b=20 | c=30 |
| `bidirectional.limn` | Solve for x: x + 5 = 12 | (none) | x=7 |
| `chain.limn` | Chained: a+b=c, c*2=d | a=5, b=3 | c=8, d=16 |
| `operations.limn` | Multiple ops: x*2, x*x, x/2 | x=10 | doubled=20, squared=100, halved=5 |
| `temperature.limn` | C to F conversion | C=100 | F=212 |

## Limn Vocabulary

| Word | Meaning | Example |
|------|---------|---------|
| `whe` | Unknown variable | `whe x` |
| `sa` | Equals | `x sa 5` |
| `joi` | Add | `a joi b` |
| `cut` | Subtract | `a cut b` |
| `exp` | Multiply | `a exp b` |
| `con` | Divide | `a con b` |
| `ma` | Greater than | `x ma 0` |
| `mi` | Less than | `x mi 10` |
