# Liminal Semantics: Contradiction Resolution in Limn

**Status:** Theoretical Framework
**Origin:** Minimal pair experiments (I3 test: `lif dea`)
**Vocabulary:** v3-natural (updated 2026-01-30)
**Key Finding:** Apparent contradictions resolve to boundary interpretations, not empty sets.

---

## 1. The Problem of Contradiction

### 1.1 Classical Logic

In classical logic, contradictions produce the empty set:
```
A ∧ ¬A = ∅ (false, empty, contradiction)
```

Applied to natural language semantics:
```
alive ∧ dead = ∅ (nothing can be both)
hot ∧ cold = ∅ (mutually exclusive)
bright ∧ dark = ∅ (opposites)
```

### 1.2 Observed Behavior in Limn

Experimental tests show different behavior:
```
lif dea = alive + dead
       ≠ ∅
       = {zombie, dormant seed, virus, moment of death, Schrödinger's cat}
```

The intersection is **small but non-empty**, containing boundary cases.

### 1.3 The Question

Why doesn't Limn produce empty sets for apparent contradictions?

---

## 2. Liminal Semantics Framework

### 2.1 Core Insight

Limn constraints are **graded regions**, not binary sets.

```
Classical:   ┌────────┐  ┌────────┐
             │  lif   │  │  dea   │
             │(alive) │  │ (dead) │
             └────────┘  └────────┘
             (no overlap)

Liminal:     ┌────────────────────────┐
             │░░░░░░░░░█████░░░░░░░░░│
             │ lif     │lim │   dea │
             │(alive)  │nal│  (dead)│
             └─────────┴───┴────────┘
             (graded overlap at boundary)
```

### 2.2 Formal Definition

**Liminal Region:** For any two constraint regions A and B, the liminal region L(A,B) is:
```
L(A,B) = {x : μ_A(x) > 0 ∧ μ_B(x) > 0}
```

Where μ is a membership function (0 = outside, 1 = core, 0-1 = boundary).

### 2.3 Properties of Liminal Regions

1. **Non-emptiness:** For graded constraints, L(A,B) ≠ ∅ even when A and B are "opposites"
2. **Smallness:** L(A,B) is typically much smaller than A or B alone
3. **Boundary-ness:** Elements in L(A,B) are peripheral to both A and B
4. **Conceptual richness:** Liminal entities are often the most interesting

---

## 3. Evidence from Limn

### 3.1 Test Case: `lif dea` (alive + dead)

**Input:** Two apparently contradictory constraints
**Output:** Non-empty liminal region

| Interpretation | Why It Fits |
|----------------|-------------|
| Zombie/undead | Animated body, departed life |
| Dormant seed | Alive in potential, dead in expression |
| Virus | Philosophically ambiguous life status |
| Moment of death | Transitional instant |
| Schrödinger's cat | Quantum superposition |

### 3.2 Test Case: `hot col` (hot + cold)

| Interpretation | Why It Fits |
|----------------|-------------|
| Lukewarm | Both relative to extremes |
| Thermal shock | Simultaneous sensation |
| Ice in fire | Coexisting states |
| Fever-chill | Contradictory symptoms |

### 3.3 Test Case: `lux nox` (bright + dark)

| Interpretation | Why It Fits |
|----------------|-------------|
| Twilight | Between day and night |
| Chiaroscuro | Art using light/shadow contrast |
| Eclipse | Dark disk on bright background |
| Bioluminescence in depth | Light in darkness |

---

## 4. Mathematical Formalization

### 4.1 Graded Membership Functions

Each Limn word w defines a membership function:
```
μ_w: Universe → [0, 1]
```

Where:
- μ_w(x) = 1 means x is a prototype member of w's constraint
- μ_w(x) = 0 means x is completely outside w's constraint
- 0 < μ_w(x) < 1 means x is a peripheral/borderline member

### 4.2 Intersection via T-Norm

Constraint intersection uses a t-norm (e.g., product or minimum):
```
μ_{A∩B}(x) = μ_A(x) ⊗ μ_B(x)

Product t-norm: μ_A(x) ⊗ μ_B(x) = μ_A(x) × μ_B(x)
Minimum t-norm: μ_A(x) ⊗ μ_B(x) = min(μ_A(x), μ_B(x))
```

### 4.3 Why Liminal Regions Are Non-Empty

For "opposite" constraints A and B:
```
If there exists x such that:
  μ_A(x) = ε > 0  (x is barely alive)
  μ_B(x) = ε > 0  (x is barely dead)

Then:
  μ_{A∩B}(x) = ε × ε = ε² > 0  (x is in liminal region)
```

The intersection is non-empty as long as the constraint boundaries overlap.

### 4.4 Sharpness Parameter

The "sharpness" of a constraint determines liminal region size:
```
Sharp constraint: μ drops quickly from 1 to 0
  → Small liminal region

Fuzzy constraint: μ drops gradually from 1 to 0
  → Large liminal region
```

Limn constraints are deliberately fuzzy, enabling rich liminal semantics.

---

## 5. Comparison with Other Approaches

### 5.1 Classical Logic

| Aspect | Classical | Liminal |
|--------|-----------|---------|
| Membership | Binary (∈ or ∉) | Graded [0,1] |
| Contradiction | Empty set | Small non-empty region |
| Boundary | None (sharp) | Rich (fuzzy) |
| Interpretation | One or none | Multiple (boundary cases) |

### 5.2 Fuzzy Logic

Liminal semantics is related to but distinct from fuzzy logic:

| Aspect | Fuzzy Logic | Liminal Semantics |
|--------|-------------|-------------------|
| Goal | Handle vagueness | Handle contradiction |
| Focus | Degree of truth | Boundary meanings |
| Output | Truth value | Set of interpretations |
| Application | Control systems | Natural language |

### 5.3 Prototype Theory

Liminal semantics aligns with cognitive prototype theory:

| Aspect | Prototype Theory | Liminal Semantics |
|--------|------------------|-------------------|
| Core | Typical examples | High membership |
| Periphery | Atypical examples | Low membership |
| Boundary | Category overlap | Liminal region |
| Contradiction | Rare cross-category | Productive meaning |

---

## 6. Implications for Limn Design

### 6.1 Feature, Not Bug

Liminal resolution is intentional:
- Enables expressive boundary meanings
- Supports creative/poetic usage
- Reflects conceptual reality (categories overlap)

### 6.2 Operator Effects

Operators interact with liminal regions:

**Intensification (`ve`):**
```
ve lif dea = very-alive AND dead
          → Smaller liminal region (more extreme contradiction)
          → More unusual interpretations (powerful zombie, very alive corpse)
```

**Weakening (`so`):**
```
so lif dea = somewhat-alive AND dead
          → Larger liminal region (relaxed contradiction)
          → More interpretations (anything vaguely animate but lifeless)
```

### 6.3 Key Effects on Liminal Regions

Keys can:
1. **Shrink** liminal region: "biology" key excludes philosophical readings
2. **Select** from liminal region: "horror" key selects zombie reading
3. **Expand** liminal region: "philosophy" key admits more interpretations

---

## 7. Implementation Considerations

### 7.1 For LLM-Based Interpretation

When encountering apparent contradictions:
1. Do NOT report "impossible" or "empty set"
2. Generate boundary/liminal interpretations
3. Prioritize conceptually rich readings
4. Use key to select among liminal options

### 7.2 For Formal Solver

If implementing constraint satisfaction:
1. Use graded membership, not binary
2. Return solutions with membership > threshold
3. Allow threshold adjustment for stricter/looser interpretation
4. Report "low membership" not "unsatisfiable" for contradictions

### 7.3 For Language Learners

Teach that:
1. Contradictions are productive, not errors
2. Boundary meanings are often the most interesting
3. Use key to guide interpretation of contradictions
4. Liminal entities (zombie, twilight, lukewarm) are valid Limn meanings

---

## 8. Examples of Liminal Meanings

### 8.1 Physical

| Contradiction | Liminal Region |
|---------------|----------------|
| `sol liq` (solid + liquid) | Gel, slush, lava, quicksand |
| `hot col` (hot + cold) | Lukewarm, thermal shock, ice on fire |
| `mag min` (large + small) | Medium-sized, relative scale |

### 8.2 Temporal

| Contradiction | Liminal Region |
|---------------|----------------|
| `beg end` (beginning + ending) | Moment, flash, instantaneous |
| `pas fut` (past + future) | Present, eternal, timeless |
| `dur brk` (ongoing + breaking) | Intermittent, stuttering |

### 8.3 Experiential

| Contradiction | Liminal Region |
|---------------|----------------|
| `lux nox` (bright + dark) | Twilight, shadow, contrast |
| `lif dea` (alive + dead) | Undead, dormant, transitional |
| `joy sad` (joy + sadness) | Bittersweet, nostalgia, poignant |

### 8.4 Abstract

| Contradiction | Liminal Region |
|---------------|----------------|
| `kno dou` (knowing + doubting) | Uncertain knowledge, belief |
| `tru fal` (true + false) | Paradox, partial truth |
| `fre cns` (freedom + constraint) | Structured freedom, bounded choice |

---

## 9. Theoretical Significance

### 9.1 For Linguistics

Liminal semantics suggests:
- Natural language categories are inherently fuzzy
- Contradiction is a productive mechanism, not just error
- Meaning lives at boundaries, not just in cores

### 9.2 For AI/NLP

Liminal semantics implies:
- Models should not reject contradictions
- Boundary cases deserve special attention
- Membership gradation improves semantic representation

### 9.3 For Philosophy

Liminal semantics connects to:
- Sorites paradox (heap problem)
- Category boundaries (where does red become orange?)
- Identity through change (Ship of Theseus)

---

## 10. Conclusion

Limn's liminal semantics is a principled approach to contradiction:

1. **Graded membership** replaces binary set membership
2. **Liminal regions** emerge at constraint boundaries
3. **Apparent contradictions** resolve to boundary meanings
4. **Keys** select among liminal interpretations

This is not a limitation of Limn but a **designed feature** that enables expressive boundary meanings unavailable in classical logical languages.

```
lif dea = the liminal space between alive and dead
       = where zombies walk and seeds wait
       = the moment of transformation itself
```

---

**END OF LIMINAL SEMANTICS**
