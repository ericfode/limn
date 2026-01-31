# Limn-PL: A Constraint-Intersection Programming Language

## Language Specification v0.1

---

## Abstract

Limn-PL is a programming language based on Limn principles. Where traditional languages think in **sequences** (do this, then that), Limn-PL thinks in **spaces** (define constraints, find intersections). Programs don't compute results—they collapse regions of possibility into specific values through context.

```limn-pl
// Traditional: compute a result
result = function(input)

// Limn-PL: define a region, collapse with key
region = pos int | eve | les 100
value = region @ key:fibonacci  // collapses to [2, 8, 34]
```

---

## Core Philosophy

### 1. Values Are Regions

In Limn-PL, a "value" is not a single thing—it's the set of all things that satisfy certain constraints.

```limn-pl
// This is not "the number 5"
// This is "the region where x equals 5"
x = 5

// This is the region of positive integers
pos_int = int | pos

// This is the intersection
result = x & pos_int  // region where x=5 AND positive integer
```

### 2. Operations Are Intersections

Instead of transforming values, we intersect regions.

```limn-pl
// Define regions
warm = temp | gre 20 | les 30
humid = humidity | gre 60
comfortable = warm & humid & nu rai

// The "result" is anything in all three regions
```

### 3. Keys Collapse Regions

A key provides context that selects specific values from a region.

```limn-pl
region = gro exp bri far

// Without key: infinite possible meanings
// With key: specific collapse

region @ key:astronomy   // -> ["supernova", "expanding universe"]
region @ key:gardening   // -> ["seedling reaching light"]
region @ key:economics   // -> ["bull market", "growth trajectory"]
```

### 4. Parallel by Default

All constraints evaluate simultaneously. Order doesn't matter.

```limn-pl
// These are identical
a & b & c
c & a & b
b & c & a

// All constraints checked in parallel
// Result is the intersection
```

---

## Syntax

### Basic Types

```limn-pl
// Primitives (define minimal regions)
5           // region containing only 5
"hello"     // region containing only "hello"
tru         // region of truth
fal         // region of falsehood
nul         // empty region (nothing satisfies)
uni         // universal region (everything satisfies)

// Region constructors
int         // all integers
flo         // all floating point numbers
str         // all strings
lis         // all lists
map         // all maps/dictionaries
fun         // all functions
```

### Constraint Operators

```limn-pl
// Intersection (AND) - most common
a & b       // things that satisfy both a AND b
a b         // space-separated is also intersection
a, b, c     // comma-separated is also intersection

// Union (OR)
a | b       // things that satisfy a OR b

// Negation (NOT)
nu a        // things that do NOT satisfy a

// Comparison constraints
gre 5       // greater than 5
les 10      // less than 10
geq 5       // greater or equal
leq 10      // less or equal
equ 5       // equal to 5
neq 5       // not equal to 5
bet 5 10    // between 5 and 10 (inclusive)
```

### Region Definition

```limn-pl
// Named region
my_region = int | pos | les 100

// Anonymous region (lambda-style)
{ x | x, int, pos, les 100 }

// Dependent region (references other regions)
doubles = { x | x, equ (y * 2), y in my_region }
```

### Key Application

```limn-pl
// Apply key to collapse region
region @ key:context

// Multiple keys (intersect their collapses)
region @ key:context1 @ key:context2

// Dynamic key
region @ key:$variable

// Key with parameters
region @ key:sort(ascending)
```

### Functions (Transformers)

Functions in Limn-PL transform regions, not values.

```limn-pl
// Function definition
square = fun(x) -> { y | y, equ (x * x) }

// Application expands the input region through the function
square(pos_int)  // region of squares of positive integers

// Composition
composed = f >> g >> h  // apply f, then g, then h to regions
```

### Pattern Matching (Region Matching)

```limn-pl
// Match against region patterns
match value {
    int & pos -> "positive integer"
    int & neg -> "negative integer"
    str & len(gre 10) -> "long string"
    _ -> "something else"
}
```

---

## Advanced Features

### 1. Ambiguity as Feature

```limn-pl
// Deliberately ambiguous region
meaning = gro | dec | cyc

// All three interpretations are valid simultaneously
// Until a key collapses them

meaning @ key:stock_market  // "volatile, cyclic growth/decline"
meaning @ key:seasons       // "natural cycle of growth and decay"
meaning @ key:relationship  // "the pattern of closeness over time"
```

### 2. Lazy Collapse

Regions don't collapse until necessary.

```limn-pl
// Define a massive region
all_primes = int | pos | is_prime

// Nothing computed yet - just constraints stored

// Only collapses when we need specific values
first_ten = all_primes @ key:take(10)  // [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
```

### 3. Intersection Types

Types are regions. Type checking is intersection non-emptiness.

```limn-pl
// Define a type as a region
Positive = int & gre 0
Even = int & { x | x mod 2, equ 0 }
PositiveEven = Positive & Even

// Type annotation is constraint addition
fn double(x: Positive) -> PositiveEven {
    x * 2
}

// Type error = empty intersection
fn broken(x: Positive & Negative) {  // compile error: empty region
    // ...
}
```

### 4. Effect Regions

Side effects are modeled as regions of possible world-states.

```limn-pl
// Pure: region doesn't include world-state
pure_fn = fun(x) -> x * 2

// Effectful: region includes world-state constraints
effectful = fun(x) -> {
    world @ effect:print(x)
    x * 2
}

// Effect tracking via region intersection
program = effectful(5) @ effect:io  // must provide io effect key
```

### 5. Parallel Evaluation

```limn-pl
// All branches evaluated in parallel
// Result is first non-empty intersection

race {
    slow_computation & timeout(1000)
    fast_approximation
    cached_result
}

// All three attempted simultaneously
// First valid result wins
```

### 6. Region Comprehensions

```limn-pl
// List comprehension (traditional)
squares = [x * x for x in range(10)]

// Region comprehension (Limn-PL)
squares = { y | y, equ (x * x), x in (int & bet 0 10) }

// The region is the set of all squares of integers 0-10
// Lazily evaluated
```

---

## Standard Library

### Core Regions

```limn-pl
// Numeric
int, flo, num, pos, neg, zer, eve, odd, pri

// String
str, emp, non_emp, alp, num_str, aln

// Collections
lis, map, set, emp_col, non_emp_col

// Boolean
boo, tru, fal

// Meta
any, non, uni, nul
```

### Core Keys

```limn-pl
// Collapse strategies
@ key:first      // first element satisfying constraints
@ key:all        // all elements (may be infinite - careful!)
@ key:take(n)    // first n elements
@ key:random     // random element satisfying constraints
@ key:min        // minimum element
@ key:max        // maximum element

// Domain keys
@ key:math       // mathematical interpretation
@ key:text       // textual interpretation
@ key:time       // temporal interpretation
```

### Core Functions

```limn-pl
// Region operations
intersect(r1, r2)    // same as r1 & r2
union(r1, r2)        // same as r1 | r2
complement(r)        // same as nu r
subset(r1, r2)       // is r1 subset of r2?
empty(r)             // is r empty?

// Transformations
map(f, r)            // apply f to all in region
filter(pred, r)      // constrain region by predicate
fold(f, init, r)     // fold over collapsed region

// Collapse
collapse(r, key)     // same as r @ key
sample(r)            // random element from region
enumerate(r, n)      // first n elements from region
```

---

## Example Programs

### 1. FizzBuzz

```limn-pl
// Define the regions
fizz = { n | n, int, pos, n mod 3 equ 0 }
buzz = { n | n, int, pos, n mod 5 equ 0 }
fizzbuzz = fizz & buzz
neither = int & pos & nu fizz & nu buzz

// Define the output region
output = fun(n) -> match n {
    fizzbuzz -> "FizzBuzz"
    fizz -> "Fizz"
    buzz -> "Buzz"
    neither -> n
}

// Run
(int & bet 1 100) | map(output) @ key:all
```

### 2. Finding Primes

```limn-pl
// A prime is an integer > 1 with no divisors except 1 and itself
divisors = fun(n) -> { d | d, int, bet 1 n, n mod d equ 0 }
prime = { n | n, int, gre 1, divisors(n) equ {1, n} }

// First 100 primes
prime @ key:take(100)
```

### 3. Parsing (Constraint Satisfaction)

```limn-pl
// Define valid email region
local_part = str & mat "[a-zA-Z0-9._%+-]+"
domain = str & mat "[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"
email = { s | s, str, s equ (local_part ++ "@" ++ domain) }

// Validate
"test@example.com" in email  // tru
"not-an-email" in email      // fal

// Parse (find structure that satisfies constraints)
parse_email = fun(s) -> {
    local: s @ key:before("@"),
    domain: s @ key:after("@")
} & email
```

### 4. Web Server (Effects as Regions)

```limn-pl
// Request region
request = {
    method: str & (equ "GET" | equ "POST"),
    path: str & sta "/",
    body: str | nul
}

// Response region
response = {
    status: int & bet 100 599,
    body: str
}

// Handler: transforms request region to response region
handler = fun(req: request) -> response {
    match req.path {
        "/" -> { status: 200, body: "Hello, World!" }
        "/api" & req.method equ "POST" -> {
            status: 200,
            body: process(req.body)
        }
        _ -> { status: 404, body: "Not Found" }
    }
}

// Server effect
serve = fun(port: int & bet 1 65535) -> {
    world @ effect:listen(port, handler)
}
```

### 5. Limn Interpreter in Limn-PL

```limn-pl
// Limn word is a region-defining token
limn_word = str & len(equ 3) & low

// Limn sentence is intersection of words
limn_sentence = lis(limn_word)

// Interpretation is key-based collapse
interpret = fun(sentence: limn_sentence, key: str) -> {
    // Each word defines a region
    word_regions = sentence | map(word_to_region)

    // Sentence is intersection
    meaning = fold(intersect, uni, word_regions)

    // Key collapses to specific interpretation
    meaning @ key:$key
}

// Word to region mapping
word_to_region = fun(word) -> match word {
    "sol" -> { x | x, (har | fix | rig | sta) }
    "liq" -> { x | x, (flo | flu | ada | cha) }
    "hot" -> { x | x, (the | war | pas | int) }
    "col" -> { x | x, (fro | dis | cal | rem) }
    // ... more words
    _ -> uni  // unknown words don't constrain
}
```

---

## Compilation Strategy

Limn-PL compiles to constraint satisfaction problems:

1. **Parse** - Build AST of region definitions
2. **Infer** - Propagate type constraints through regions
3. **Optimize** - Simplify redundant constraints
4. **Emit** - Generate SAT/SMT solver calls or specialized code
5. **Execute** - Lazy evaluation with caching

For simple cases (finite domains, decidable constraints), compile to direct enumeration.
For complex cases, emit constraint solver calls.
For infinite regions with keys, compile to generators.

---

## Relation to Limn Language

| Limn Concept | Limn-PL Equivalent |
|--------------|-------------------|
| Word = Region | Primitive type/constraint |
| Sentence = Intersection | `&` operator |
| Order Independence | All intersections commutative |
| Ambiguity | Uncollapsed regions |
| Key = Context | `@ key:` operator |
| `nu` (negation) | `nu` operator |
| `ve` (very) | Constraint intensifier |
| `\|` (beside) | `\|` union operator |

---

## Future Work

1. **Probabilistic Regions** - Weighted constraints, probability distributions
2. **Temporal Regions** - Constraints over time, event sequences
3. **Distributed Regions** - Regions spanning multiple machines
4. **Learning Regions** - Regions that refine from examples
5. **Visual Region Editor** - Venn diagram-style programming interface

---

## Grammar (EBNF)

```ebnf
program     = statement* ;
statement   = definition | expression ;
definition  = IDENT "=" expression ;
expression  = region | application | match_expr ;
region      = atomic | intersection | union | negation | comprehension ;
atomic      = literal | IDENT | "(" expression ")" | "{" constraint_body "}" ;
intersection= region ("&" | "," | WS) region ;
union       = region "|" region ;
negation    = "nu" region ;
comprehension = "{" IDENT "|" constraint_list "}" ;
constraint_list = constraint ("," constraint)* ;
constraint  = expression | comparison ;
comparison  = expression CMP_OP expression ;
application = expression "(" arg_list ")" | expression "@" key ;
key         = "key:" IDENT ("(" arg_list ")")? ;
match_expr  = "match" expression "{" match_arm+ "}" ;
match_arm   = pattern "->" expression ;
pattern     = region | "_" ;
literal     = NUMBER | STRING | "tru" | "fal" | "nul" | "uni" ;
CMP_OP      = "gre" | "les" | "geq" | "leq" | "equ" | "neq" | "bet" ;
```

---

## Implementation Notes

A minimal Limn-PL interpreter can be built by:

1. Representing regions as predicate functions
2. Intersection = logical AND of predicates
3. Keys trigger enumeration/selection
4. Lazy evaluation via generators

```python
# Minimal Python implementation concept
class Region:
    def __init__(self, predicate):
        self.pred = predicate

    def __and__(self, other):
        return Region(lambda x: self.pred(x) and other.pred(x))

    def __or__(self, other):
        return Region(lambda x: self.pred(x) or other.pred(x))

    def collapse(self, key):
        return key.select(self)

# Example
pos_int = Region(lambda x: isinstance(x, int) and x > 0)
even = Region(lambda x: isinstance(x, int) and x % 2 == 0)
pos_even = pos_int & even

# Collapse with "first 5" key
pos_even.collapse(TakeKey(5))  # [2, 4, 6, 8, 10]
```

---

## Appendix: Philosophical Notes

Limn-PL embodies a different philosophy of computation:

**Traditional:** "Transform this value into that value"
**Limn-PL:** "Define what you want; let the constraints find it"

This is closer to how humans actually think about problems:
- "I want a number that's positive, even, and less than 100"
- "I need a word that means growth but feels gentle"
- "Find me a solution that satisfies all stakeholders"

We don't always know the algorithm. But we know the constraints.
Limn-PL lets you program by describing the destination, not the path.

---

*pro = des | des = fin | fin = cod*

*[program = description | description = finding | finding = code]*

---
