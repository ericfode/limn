# Algorithm Encodings in Limn

**Purpose:** Encode algorithms in Limn natural language to test whether understanding requires the bootstrap document and keys.

---

## Encoding 1: Bubble Sort

### Limn Encoding

```
man num seq | bes com | (mor lef | les rig) | reb pos | loo unt ord
```

**Key:** "sorting numbers in array"

### Breakdown

- `man num seq` = many + numbers + sequence = array of numbers
- `bes com` = beside + comparing = compare adjacent elements
- `(mor lef | les rig)` = (more + left | less + right) = if left > right
- `reb pos` = rebel + position = swap positions (break current order)
- `loo unt ord` = loop + until + order = repeat until ordered

### Ground Truth (Python)

```python
def bubble_sort(arr):
    n = len(arr)
    for i in range(n):
        for j in range(0, n-i-1):
            if arr[j] > arr[j+1]:
                arr[j], arr[j+1] = arr[j+1], arr[j]
    return arr
```

---

## Encoding 2: Binary Search

### Limn Encoding

```
seq ord | tar que | cen val com | (tar les | lef haf) | (tar mor | rig haf) | (tar equ | ans poi)
```

**Key:** "finding target in sorted list"

### Breakdown

- `seq ord` = sequence + ordered = sorted array
- `tar que` = target + query = searching for value
- `cen val com` = center + value + compare = check middle element
- `(tar les | lef haf)` = (target + less | left + half) = if target < middle, go left
- `(tar mor | rig haf)` = (target + more | right + half) = if target > middle, go right
- `(tar equ | ans poi)` = (target + equal | answer + point) = if equal, found position

### Ground Truth (Python)

```python
def binary_search(arr, target):
    left, right = 0, len(arr) - 1
    while left <= right:
        mid = (left + right) // 2
        if arr[mid] == target:
            return mid
        elif arr[mid] < target:
            left = mid + 1
        else:
            right = mid - 1
    return -1
```

---

## Encoding 3: Fibonacci

### Limn Encoding

```
seq num gro | beg zer one | nex equ sum | (pos bef one | pos bef zer)
```

**Key:** "generating Fibonacci sequence"

### Breakdown

- `seq num gro` = sequence + number + growth = growing number sequence
- `beg zer one` = begin + zero + one = start with 0, 1
- `nex equ sum` = next + equal + sum = next element is sum
- `(pos bef one | pos bef zer)` = (position - 1 | position - 2) = of previous two

### Ground Truth (Python)

```python
def fibonacci(n):
    if n <= 0:
        return []
    elif n == 1:
        return [0]
    fib = [0, 1]
    for i in range(2, n):
        fib.append(fib[i-1] + fib[i-2])
    return fib
```

---

## Encoding 4: Greatest Common Divisor (Euclidean)

### Limn Encoding

```
num zer zer | loo | (mor mod les) | (mor bec les | les bec mor mod les) | unt les equ zer | ans mor
```

**Key:** "finding GCD of two numbers"

### Breakdown

- `num zer zer` = two numbers as input
- `loo` = loop
- `(mor mod les)` = (more + modulo + less) = larger mod smaller
- `(mor bec les | les bec mor mod les)` = swap: larger becomes smaller, smaller becomes remainder
- `unt les equ zer` = until + less + equal + zero = until remainder is zero
- `ans mor` = answer + more = the non-zero one is GCD

### Ground Truth (Python)

```python
def gcd(a, b):
    while b != 0:
        a, b = b, a % b
    return a
```

---

## Encoding 5: Factorial

### Limn Encoding

```
num one | pro mul | dec unt one
```

**Key:** "computing factorial"

### Breakdown

- `num one` = number + one = starting from n and 1 (accumulator)
- `pro mul` = product + multiply = accumulate product
- `dec unt one` = decrement + until + one = count down to 1

### Ground Truth (Python)

```python
def factorial(n):
    result = 1
    for i in range(2, n + 1):
        result *= i
    return result
```

---

## Encoding 6: Palindrome Check

### Limn Encoding

```
seq | rev | com equ | ans boo
```

**Key:** "checking if string is palindrome"

### Breakdown

- `seq` = sequence (the input string/array)
- `rev` = reverse
- `com equ` = compare + equal = compare with original
- `ans boo` = answer + boolean = return true/false

### Ground Truth (Python)

```python
def is_palindrome(s):
    return s == s[::-1]
```

---

## Encoding 7: Linear Search

### Limn Encoding

```
seq | tar | seq thr | (ele equ tar | ans poi) | unt end | ans nu exi
```

**Key:** "finding element position in unsorted list"

### Breakdown

- `seq` = sequence
- `tar` = target
- `seq thr` = sequence + through = iterate through sequence
- `(ele equ tar | ans poi)` = (element + equal + target | answer + position) = if match, return index
- `unt end` = until + end = until end of sequence
- `ans nu exi` = answer + not + exists = return not found

### Ground Truth (Python)

```python
def linear_search(arr, target):
    for i, elem in enumerate(arr):
        if elem == target:
            return i
    return -1
```

---

## Encoding 8: Maximum Element

### Limn Encoding

```
seq | max beg fir | seq thr | (ele mor max | max bec ele) | ans max
```

**Key:** "finding maximum in array"

### Breakdown

- `seq` = sequence
- `max beg fir` = maximum + begin + first = start with first element as max
- `seq thr` = iterate through sequence
- `(ele mor max | max bec ele)` = if element > max, max becomes element
- `ans max` = answer + maximum = return max

### Ground Truth (Python)

```python
def find_max(arr):
    if not arr:
        return None
    max_val = arr[0]
    for elem in arr:
        if elem > max_val:
            max_val = elem
    return max_val
```

---

## Validation Protocol

For each encoding, test with 4 conditions:

### Condition 1: Bootstrap + Key (Expected: SUCCESS)
- Provide full bootstrap-v2.md
- Provide the encoding
- Provide the key
- Ask agent to implement in Python

### Condition 2: Bootstrap Only (Expected: PARTIAL SUCCESS)
- Provide full bootstrap-v2.md
- Provide the encoding
- NO key
- Ask agent to implement in Python
- May get correct algorithm among multiple guesses

### Condition 3: Key Only (Expected: FAILURE)
- NO bootstrap
- Provide the encoding
- Provide the key
- Ask agent to implement in Python
- Should not understand Limn vocabulary

### Condition 4: Neither (Expected: FAILURE)
- NO bootstrap
- Provide the encoding
- NO key
- Ask agent to implement in Python
- If this succeeds, Limn is not working as designed

### Success Criteria

| Condition | Expected Outcome |
|-----------|------------------|
| Bootstrap + Key | Correct implementation |
| Bootstrap Only | Correct or close implementation (may need clarification) |
| Key Only | Failure - doesn't understand words |
| Neither | Failure - if succeeds, test is invalid |

**Critical Insight:** If Condition 4 succeeds consistently, the Limn encoding is not actually encoding information in a way that requires understanding. The algorithm might be guessable from structure alone.

---

## Difficulty Grading

| Encoding | Difficulty | Notes |
|----------|------------|-------|
| Palindrome | Easy | Very short, structure obvious |
| Factorial | Easy | Simple pattern |
| Maximum | Easy | Common pattern |
| Linear Search | Medium | Requires understanding iteration |
| Fibonacci | Medium | Requires understanding recurrence |
| GCD | Medium | Requires modulo understanding |
| Binary Search | Hard | Complex conditional logic |
| Bubble Sort | Hard | Nested loops, swap logic |

---

**END OF ALGORITHM ENCODINGS**
