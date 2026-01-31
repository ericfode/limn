# limn-math

Mathematical utilities for the Limn programming language.

## Installation

```bash
limn pak add limn-math
```

Or use directly by CID:

```limn
use cid <published-cid>
```

## Features

### Basic Arithmetic (Bidirectional)

All operations can compute any unknown given the other values:

```limn
# Addition: a + b = result
whe a
whe b
whe result
a joi b sa result

---
# Forward: given a and b, compute result
a sa 5
b sa 3
# Result: result = 8

# Backward: given a and result, compute b
a sa 5
result sa 10
# Result: b = 5
```

### Geometry

```limn
# Circle area (pi * r^2)
whe radius
whe area
# ... constraints from package ...

---
radius sa 5
# Result: area = 78.54...

# Rectangle area
whe width
whe height
whe area

---
width sa 4
height sa 6
# Result: area = 24
```

### Temperature Conversion

Bidirectional Celsius/Fahrenheit conversion:

```limn
whe temp_c
whe temp_f
# ... constraints from package ...

---
# Celsius to Fahrenheit
temp_c sa 100
# Result: temp_f = 212

# Fahrenheit to Celsius
temp_f sa 32
# Result: temp_c = 0
```

### Distance Conversion

```limn
whe dist_km
whe dist_miles

---
dist_km sa 100
# Result: dist_miles = 62.137...
```

### Percentage

```limn
whe pct_value
whe pct_percent
whe pct_result

---
pct_value sa 200
pct_percent sa 15
# Result: pct_result = 30  (15% of 200)
```

## Exports

| Name | Description |
|------|-------------|
| `add_*` | Addition (a, b, result) |
| `sub_*` | Subtraction (a, b, result) |
| `mul_*` | Multiplication (a, b, result) |
| `div_*` | Division (a, b, result) |
| `circle_*` | Circle area (radius, area, pi) |
| `rect_*` | Rectangle area (width, height, area) |
| `tri_*` | Triangle area (base, height, area) |
| `temp_*` | Temperature conversion (c, f) |
| `dist_*` | Distance conversion (km, miles) |
| `pct_*` | Percentage (value, percent, result) |

## License

MIT
