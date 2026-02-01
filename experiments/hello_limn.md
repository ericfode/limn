# Experiment: Hello Limn

This document captures an experiment in translating concepts into **Limn**, both in its **Programming Syntax** (v1) and its **Natural Language** form.

## 1. The Practical: Bidirectional Temperature Converter

A classic functional programming example, but with Limn's bidirectional twist.

### Source (Reference Implementation)

**Note:** Python shown for comparison only. Limn uses Prolog exclusively.

```python
# Reference only - not canonical implementation
def celsius_to_fahrenheit(c):
    return (c * 9/5) + 32

def fahrenheit_to_celsius(f):
    return (f - 32) * 5/9
```

### Target (Limn Programming v1)
Note how a single constraint set handles both directions.

```limn
program temperature:
  variables: celsius fahrenheit
  constraints:
    # The core relationship defined once
    # (c * 9) / 5 = f - 32
    
    # Broken down for clarity (though composition is supported)
    celsius times 9 equals c_scaled
    c_scaled divided_by 5 equals f_offset
    fahrenheit minus 32 equals f_offset
```

### Usage

**Forward (C -> F):**
```limn
run temperature with:
  celsius = 100
# Output: fahrenheit = 212
```

**Backward (F -> C):**
```limn
run temperature with:
  fahrenheit = 32
# Output: celsius = 0

<!-- 🥚 The freezing point of water is where the molecules stop dancing and start hugging. -->

```

---

## 2. The Poetic: Translating "Translation"

Using the **Limn Natural Language** (as defined in `metacircular-limn.md`) to describe the act of translation itself.

**Concept:** "Translation"
*Definition:* Two different forms (languages) sharing the same central meaning, connected in an ongoing process.

### Limn Expression

```
yo an nu sa | pu sa | bi du
```

### Breakdown
*   **`yo an nu sa`**: This (`yo`) and that (`an`) are not (`nu`) the same (`sa`).
    *   *Interpretation:* The source and target languages look different.
*   **`pu sa`**: The center (`pu`) is the same (`sa`).
    *   *Interpretation:* The core meaning/essence is preserved.
*   **`bi du`**: Connecting (`bi`) ongoing (`du`).
    *   *Interpretation:* It is an active, continuous bridge between them.

**Key:** "The Art of Translation"
**Full Interpretation:** "Distinct forms with a shared heart, forever connected."

---

## 3. The Insane: Recursive Self-Definition

If we ask Limn to define "Limn" (The Language):

```
mi bi sa | wo si | ke
```

*   **`mi bi sa`**: Many connections unified. (The System)
*   **`wo si`**: Waiting for knowledge. (The Input/Context)
*   **`ke`**: Surprise! (The Collapse/Meaning)

*Interpretation:* Limn is a unified web of connections that waits for your input to surprise you with meaning.
