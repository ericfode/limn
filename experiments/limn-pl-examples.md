# Limn-PL Example Programs

This document contains example programs written in Limn-PL, demonstrating the language's key features: constraint-based programming, bidirectional computation, and use of Limn vocabulary.

## 1. Basic Arithmetic

### 1.1 Addition (joi)

```limn
pro joi-demo |
var | whe a | whe b | whe c |
cns | a joi b sa c
```

**Usage (forward):**
```limn
# Key: yo a sa 3 | yo b sa 5
# Result: c = 8
```

**Usage (backward):**
```limn
# Key: yo a sa 3 | yo c sa 10
# Result: b = 7
```

### 1.2 All Four Operations

```limn
# Addition: a + b = sum
pro add |
var | whe a | whe b | whe sum |
cns | a joi b sa sum

# Subtraction: a - b = diff
pro sub |
var | whe a | whe b | whe diff |
cns | a cut b sa diff

# Multiplication: a * b = prod
pro mul |
var | whe a | whe b | whe prod |
cns | a exp b sa prod

# Division: a / b = quot
pro div |
var | whe a | whe b | whe quot |
cns | a con b sa quot
```

## 2. Unit Conversions

### 2.1 Temperature: Celsius to Fahrenheit

```limn
pro tem-con |
var | whe C | whe F | whe t1 | whe t2 |
cns |
t1 sa C exp 9 |
t2 sa t1 con 5 |
F sa t2 joi 32
```

**C to F:**
```limn
# Key: yo C sa 100
# Result: F = 212
```

**F to C:**
```limn
# Key: yo F sa 32
# Result: C = 0
```

### 2.2 Distance: Kilometers to Miles

```limn
pro km-mi |
var | whe km | whe mi | whe factor |
cns |
factor sa 0.621371 |
mi sa km exp factor
```

**km to miles:**
```limn
# Key: yo km sa 100
# Result: mi = 62.1371
```

**miles to km:**
```limn
# Key: yo mi sa 62.1371
# Result: km = 100
```

### 2.3 Currency Conversion

```limn
pro cur-con |
var | whe usd | whe eur | whe rate |
cns |
eur sa usd exp rate
```

**USD to EUR:**
```limn
# Key: yo usd sa 100 | yo rate sa 0.85
# Result: eur = 85
```

**EUR to USD (reverse):**
```limn
# Key: yo eur sa 85 | yo rate sa 0.85
# Result: usd = 100
```

## 3. Geometric Calculations

### 3.1 Rectangle Area

```limn
pro rec-area |
var | whe wid | whe hei | whe are |
cns | are sa wid exp hei
```

**Compute area:**
```limn
# Key: yo wid sa 5 | yo hei sa 10
# Result: are = 50
```

**Find dimension from area:**
```limn
# Key: yo are sa 50 | yo wid sa 5
# Result: hei = 10
```

### 3.2 Circle Area

```limn
pro cir-area |
var | whe r | whe are | whe pi | whe rsq |
cns |
pi sa 3.14159 |
rsq sa r exp r |
are sa pi exp rsq
```

### 3.3 Triangle Area

```limn
pro tri-area |
var | whe bas | whe hei | whe are | whe t1 |
cns |
t1 sa bas exp hei |
are sa t1 con 2
```

### 3.4 Pythagorean Theorem

```limn
# a^2 + b^2 = c^2
pro pyth |
var | whe a | whe b | whe c | whe a2 | whe b2 | whe c2 |
cns |
a2 sa a exp a |
b2 sa b exp b |
c2 sa c exp c |
a2 joi b2 sa c2
```

**Find hypotenuse:**
```limn
# Key: yo a sa 3 | yo b sa 4
# Result: c2 = 25, so c = 5 (need sqrt)
```

**Note:** Finding c requires square root, which needs extension.

## 4. Financial Calculations

### 4.1 Simple Interest

```limn
# I = P * R * T
pro sim-int |
var | whe P | whe R | whe T | whe I | whe t1 |
cns |
t1 sa P exp R |
I sa t1 exp T
```

**Calculate interest:**
```limn
# Key: yo P sa 1000 | yo R sa 0.05 | yo T sa 2
# Result: I = 100
```

**Find rate needed:**
```limn
# Key: yo P sa 1000 | yo I sa 100 | yo T sa 2
# Result: R = 0.05
```

### 4.2 Percentage Calculation

```limn
# percent = (part / whole) * 100
pro pct |
var | whe par | whe who | whe pct | whe t1 |
cns |
t1 sa par con who |
pct sa t1 exp 100
```

**What percent?**
```limn
# Key: yo par sa 25 | yo who sa 100
# Result: pct = 25
```

**Find the part:**
```limn
# Key: yo pct sa 25 | yo who sa 100
# Result: par = 25
```

### 4.3 Profit Margin

```limn
pro mar |
var | whe rev | whe cos | whe prf | whe mar |
cns |
prf sa rev cut cos |
mar sa prf con rev exp 100
```

## 5. Physics Calculations

### 5.1 Speed, Distance, Time

```limn
# distance = speed * time
pro mov |
var | whe dis | whe spe | whe tim |
cns | dis sa spe exp tim
```

**Calculate distance:**
```limn
# Key: yo spe sa 60 | yo tim sa 2
# Result: dis = 120
```

**Calculate time:**
```limn
# Key: yo dis sa 120 | yo spe sa 60
# Result: tim = 2
```

**Calculate speed:**
```limn
# Key: yo dis sa 120 | yo tim sa 2
# Result: spe = 60
```

### 5.2 Force = Mass * Acceleration

```limn
pro for |
var | whe mas | whe acc | whe for |
cns | for sa mas exp acc
```

### 5.3 Energy = Mass * c^2 (E=mc²)

```limn
pro ene |
var | whe mas | whe ene | whe c | whe c2 |
cns |
c sa 299792458 |
c2 sa c exp c |
ene sa mas exp c2
```

**Energy from 1 kg:**
```limn
# Key: yo mas sa 1
# Result: ene = 8.99e16 Joules
```

## 6. Composite Programs

### 6.1 Chain of Conversions

```limn
# Convert Celsius -> Fahrenheit -> Kelvin
pro ful-tem |
var | whe C | whe F | whe K | whe t1 | whe t2 |
cns |
# C to F
t1 sa C exp 9 |
t2 sa t1 con 5 |
F sa t2 joi 32 |
# F to K (approximately)
K sa C joi 273.15
```

### 6.2 BMI Calculator

```limn
# BMI = weight(kg) / height(m)^2
pro bmi |
var | whe wei | whe hei | whe bmi | whe h2 |
cns |
h2 sa hei exp hei |
bmi sa wei con h2
```

**Calculate BMI:**
```limn
# Key: yo wei sa 70 | yo hei sa 1.75
# Result: bmi = 22.86
```

**Find weight for target BMI:**
```limn
# Key: yo bmi sa 22 | yo hei sa 1.75
# Result: wei = 67.375
```

## 7. Bidirectional Demonstration

The key feature of Limn-PL is that the same program can compute in multiple directions.

### 7.1 Three-Variable System

```limn
pro tri |
var | whe x | whe y | whe z |
cns |
x joi y sa z
```

**Any two variables determine the third:**

| Given | Computes |
|-------|----------|
| x=3, y=5 | z=8 |
| x=3, z=8 | y=5 |
| y=5, z=8 | x=3 |

### 7.2 Four-Variable Chain

```limn
pro qua |
var | whe a | whe b | whe c | whe d |
cns |
a joi b sa c |
c exp 2 sa d
```

**Multiple computation directions:**

| Given | Computes |
|-------|----------|
| a=3, b=4 | c=7, d=14 |
| a=3, d=14 | c=7, b=4 |
| b=4, d=14 | c=7, a=3 |
| c=7, a=3 | b=4, d=14 |

## 8. Using Limn Vocabulary

The beauty of Limn-PL is that programs read like Limn sentences:

| Program Element | Limn Word | Meaning |
|-----------------|------------|---------|
| `var` | variable | unknowns |
| `whe` | where | indicates unknown |
| `cns` | constraint | what must be true |
| `joi` | join | addition |
| `cut` | cut | subtraction |
| `exp` | expand | multiplication |
| `con` | contract | division |
| `sa` | same | equality |
| `ma` | more | greater than |
| `mi` | less | less than |

### Example Reading

The program:
```limn
pro mov |
var | whe dis | whe spe | whe tim |
cns | dis sa spe exp tim
```

Reads as: "A program about movement, with unknowns distance, speed, and time, constrained so that distance equals speed expanded by time."

## 9. Running Examples

To run these examples, use the Python interpreter:

```python
from src.limn_pl_interpreter import run_limn_pl

source = """
pro joi-demo |
var | whe a | whe b | whe c |
cns | a joi b sa c
"""

# Forward computation
result = run_limn_pl(source, {"a": 3, "b": 5})
print(result)  # {'a': 3, 'b': 5, 'c': 8}

# Backward computation
result = run_limn_pl(source, {"a": 3, "c": 10})
print(result)  # {'a': 3, 'c': 10, 'b': 7}
```

## 10. Comparison with programming-v1.md

The original spec used English-like syntax:
```
program addition:
  variables: a b c
  constraints:
    a plus b equals c
```

Limn-PL uses pure Limn vocabulary:
```limn
pro joi |
var | whe a | whe b | whe c |
cns | a joi b sa c
```

Both are semantically equivalent - constraint sets that can be solved bidirectionally.

---

**END OF EXAMPLES**
