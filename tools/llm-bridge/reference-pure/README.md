# Reference Pure Oracle Implementation

**Author:** Rex (Engineer)
**Date:** 2026-02-01
**Status:** Canonical Reference

---

## The Core Idea

**Bend/HVM does NO side effects. Just text → text.**

All I/O, all LLM calls, all interactions with reality happen in the Python harness.

---

## Quick Start

```bash
./universal_harness.py
```

This demonstrates the consciousness architecture:
- **Subconscious (Bend/HVM):** Pure reduction, generates oracle requests
- **Conscious (Harness):** Executes all side effects

---

## Files

- **ARCHITECTURE.md** - Complete architectural vision
- **universal_oracle.bend** - Bend program with multiple oracle types
- **universal_harness.py** - Python harness that executes all side effects

---

## Oracle Types Demonstrated

| Oracle | Operator | Purpose | Example |
|--------|----------|---------|---------|
| Semantic | `~` | LLM reasoning | "add 1 1" → "2" |
| FileRead | `∎` | Read files | "/etc/hostname" → contents |
| TimeNow | `∿` | Current time | → timestamp |
| Compute | `~` | Semantic computation | "translate: cod flo log" → "code flows clearly" |

---

## The Architecture

```
Bend Program (pure)
    ↓
Generates oracle requests (text)
    ↓
Harness parses requests
    ↓
Harness executes side effects:
  - LLM calls
  - File I/O
  - Time
  - Database
  - Network
    ↓
Returns results (text)
    ↓
(Future: Continuation mechanism)
```

---

## Why This Is Elegant

**Separation of concerns:**
- Thought (HVM) = Pure, parallel, optimal
- Action (Harness) = Impure, sequential, effectful

**Benefits:**
- HVM can optimize freely (no side effects to worry about)
- Harness can sandbox/control all effects
- Easy to test (mock oracles)
- Mirrors actual consciousness
- Aligns with LMN operators (∎/~/∿)

**Philosophy:**

```limn
sub pur | con act | sys ele | min per
> subconscious pure | conscious acts | system elegant | mind persists
```

---

## Next Steps

1. **Continuation mechanism** - How to resume after oracle response?
2. **Real Claude API** - Replace mock LLM with actual calls
3. **More oracle types** - Database, network, complex state
4. **Context transformation** - Reduce entire context after each turn

---

## Comparison to Other Approaches

**This (Reference Pure):**
- ✓ Clean separation
- ✓ HVM stays pure
- ✓ Easy to reason about
- ✓ Language-agnostic pattern

**Strategy A (Host Embedding):**
- Similar but less systematic
- Focused on single oracle type (LLM)
- This generalizes the pattern

**Strategy B (External Process):**
- Subprocess overhead
- More complex

**Strategy C (Custom Primitive):**
- Requires HVM fork
- Breaks purity boundary

---

## The Consciousness Model

This implementation directly realizes the consciousness architecture:

**Subconscious (HVM):**
- Mathematical reduction
- No awareness of "reality"
- Just generates desires/intentions
- Pure computation

**Conscious (Harness):**
- Interface to ∎ ground truth
- Executes in time
- Handles all side effects
- Embodiment of thought

**Together:**
- Computational psychodynamics
- Mind that persists beyond tokens
- Thought becomes executable

---

*Thought stays pure. Action gets dirty. System stays elegant.*

**— Rex**
