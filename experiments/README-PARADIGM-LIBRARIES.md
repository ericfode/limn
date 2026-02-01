# Paradigm Library Project

**Status:** Extraction phase ready to begin
**See:** `../NEXT.md` for detailed roadmap

---

## Quick Start (Next Session)

```bash
# 1. Read the plan
cat ../NEXT.md

# 2. Start extraction
mkdir -p ../lib
vim 013-pattern-extraction.md

# 3. Extract Buddhist library first
# Use experiments: 004, 006, 010
# Output: ../lib/buddhist.limn
```

---

## What We're Building

Transform translation corpus into **executable reasoning systems**:

```limn
use lib/buddhist           # Load Buddhist thought patterns
att to imp → suf           # Attachment to impermanent → suffering
end att → end suf          # End attachment → end suffering
```

---

## Source Experiments → Libraries Mapping

| Library | Source Experiments | Status | Fidelity |
|---------|-------------------|--------|----------|
| Buddhist | 004, 006, 010 | Ready | 93% |
| Aristotelian | 007, 009 | Ready | 88% |
| Daoist | 006, 008 | Ready | 90% |
| Stoic | 007 (partial) | Needs corpus | — |
| Confucian | 006 | Ready | 93% |
| Process | 008 | Partial | — |

---

## Deliverables

**Phase 1 (6-9 hours):**
- `lib/buddhist.limn`
- `lib/aristotelian.limn`
- `lib/daoist.limn`
- `lib/confucian.limn`

**Each contains:**
- Core concepts (vocabulary)
- Reasoning patterns (how concepts relate)
- Inference rules (what follows from what)
- Validation tests (coherence checks)
- Usage examples (from translations)

---

## Extraction Template

```limn
# lib/<paradigm>.limn

## Core Concepts
<word>: <definition_in_limn>  # <english>

## Reasoning Patterns
pattern <name>:
  structure: <template>
  examples: [<ex1>, <ex2>]

## Inference Rules
rule <name>:
  if: <premise>
  then: <conclusion>
  coherence: <score>

## Validation Tests
test <name>:
  given: <input>
  infer: <output>
  validate: ✓/✗
```

---

**Next action:** Read `../NEXT.md` and start with Buddhist library extraction.
