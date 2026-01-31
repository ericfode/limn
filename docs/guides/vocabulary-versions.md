# Limn Vocabulary Versions Guide

**Author:** Dr. Maren Solvik, Theoretical Linguist
**Date:** 2026-01-25
**Reference:** limn-land-6k3

---

## Overview

Limn has three vocabulary versions:
- **v1:** Original 40-word core vocabulary (historical)
- **v2:** Expanded 500-word comprehensive vocabulary
- **v3-natural:** Current (~340 core + domains), designed for zero-bootstrap LLM comprehension

**Note:** v3-natural is the current standard (see `docs/spec/vocabulary-v3-natural.md`). This guide explains the v1→v2 migration for historical reference. v3-natural builds on v2 with natural extensions and collision fixes.

This guide explains the v1/v2 relationship for historical reference.

---

## 1. Version Comparison

### v1: Core Vocabulary (40 words)

**Origin:** Initial Limn specification
**Size:** ~40 words
**Design:** Minimal, highly polysemous

**Example words:**
```
ta = action, begin, do
fi = end, finish, pattern
du = change, differ
ga = cause, give
zo = motion, move
lu = light, bright, see
mu = dark, self, hidden
vi = alive, way, path
ko = solid, know, bone
ra = linear, ray, reason
```

**Strengths:**
- Extremely compact
- Forces creative combination
- Good for learning concepts

**Limitations:**
- High ambiguity (hard to narrow meanings)
- Limited domain coverage
- Requires extensive keys for precision

### v2: Comprehensive Vocabulary (500 words)

**Origin:** Extended specification for practical communication
**Size:** ~500 words (13 domains)
**Design:** Broader coverage, still polysemous

**Example words:**
```
sol = solid, rigid
liq = liquid, flowing
lux = bright, luminous
dim = dim, faint
mov = motion, movement
gro = growth, increase
```

**Strengths:**
- Better domain coverage
- More precise communication
- Reduced key dependency

**Limitations:**
- Larger learning curve
- More vocabulary to manage

---

## 2. Compatibility

### Are v1 and v2 compatible?

**Yes, with caveats.**

v1 and v2 can be mixed in the same sentence. However:

1. **Some v1 words are in v2:** `vi`, `mu`, etc.
2. **Some v1 words have different forms in v2:**
   - v1 `ko` (solid, know) → v2 `sol` (solid), `kno` (know)
3. **Some v1 words are abbreviations of v2 concepts:**
   - v1 `lu` (bright) ≈ v2/v3 `lux`

### Practical Guidance

**If using v2:** You may include v1 words where they exist in v2.

**If using v1:** v2 words will still work but may be unfamiliar to v1-only learners.

**Recommendation:** Choose one version for a given context. Don't mix freely unless you understand both.

---

## 3. Deprecation Status

**Is v1 deprecated?**

**No, but it's now considered "educational/historical."**

| Version | Status | Use Case |
|---------|--------|----------|
| v1 | Historical | Learning core concepts, historical reference |
| v2 | Reference | Migration reference, historical documentation |
| v3-natural | Active | Production communication, LLM-native design |

v1 is still valid Limn. It's useful for:
- Teaching core concepts
- Constrained communication (like poetry)
- Historical reference

v2 is preferred for:
- Serious communication
- Full expressiveness
- Domain-specific vocabulary

---

## 4. Migration: v1 → v2

### 4.1 Mapping Table

| v1 Word | v1 Meaning | v2 Equivalent(s) |
|---------|------------|------------------|
| `ta` | action, begin | `act`, `beg`, `ta` |
| `fi` | end, pattern | `end`, `fi`, `pat` |
| `du` | change, differ | `cha`, `dif`, `tra` |
| `ga` | cause, give | `cau`, `giv` |
| `zo` | motion, move | `mov`, `zo` |
| `lu` | light, bright | `bri`, `lux`, `lu` |
| `mu` | dark, self, hidden | `mu`, `nox`, `sel`, `hid` |
| `vi` | alive, way | `vi`, `lif`, `pat` |
| `ko` | solid, know | `sol`, `kno` |
| `ra` | linear, reason | `lin`, `rea` |
| `nu` | negation | `nu` (unchanged) |
| `ve` | intensifier | `ve` (unchanged) |
| `so` | weakener | `so` (unchanged) |
| `pa` | parallel | `|` (scope marker) |

### 4.2 General Migration Rules

1. **Operators unchanged:** `nu`, `ve`, `so`, `pa` remain the same
2. **Split polysemy:** v1 words with multiple meanings → multiple v2 words
3. **Check context:** Choose v2 word based on intended meaning

**Example migration:**

```
v1: ko mu lu ta fi
    solid-dark-bright-begin-end

v2: sol nox bri beg end
    solid darkness bright beginning ending

OR (different interpretation):

v2: kno mu lu ta fi
    know self light action end
```

The v2 version is more explicit about which meaning is intended.

---

## 5. Learning Path

### Recommended Approach

**Phase 1: v2 First Day Tutorial**
- Learn 5 v2 words (`lu`, `mu`, `ta`, `fi`, `vi`)
- These are shared with v1
- Understand intersection semantics

**Phase 2: Expand v2 Vocabulary**
- Learn 50-100 most common v2 words
- Cover major semantic domains
- Practice with keys

**Phase 3: (Optional) Study v1**
- Understand historical development
- Appreciate extreme polysemy
- Use for constrained/poetic expression

### Why v2 First?

1. **More practical:** v2 covers real communication needs
2. **Shared foundations:** The core v2 words include v1 words
3. **Clearer learning:** Less ambiguity means easier initial understanding
4. **Migration optional:** You may never need v1

---

## 6. FAQ

### Q: Can I submit journal entries in v1?
**A:** Yes, v1 is valid Limn. Indicate you're using v1 if context is important.

### Q: Will v1 be removed?
**A:** No. v1 is part of Limn's history and remains valid for appropriate uses.

### Q: Which version should I cite in formal work?
**A:** Cite v2 (vocabulary-v2.md) for current Limn. Cite v1 for historical analysis.

### Q: Can keys disambiguate v1→v2 meaning?
**A:** Yes! A key like "v2: solid state" would narrow v1 `ko` to v2 `sol` meaning.

### Q: What is v3-natural?
**A:** v3-natural is the current standard (~340 core words + domain modules). It builds on v2 with natural extensions, collision fixes (e.g., `bre` for brief, `aud` for hearing), and LLM-native design principles. See `docs/spec/vocabulary-v3-natural.md`.

---

## 7. Quick Reference

### Version Summary

| Property | v1 | v2 | v3-natural |
|----------|-----|-----|------------|
| Word count | ~40 | ~500 | ~340 core + domains |
| Polysemy | Very high | High | Moderate |
| Domain coverage | Limited | Comprehensive | Core + modules |
| Precision | Low | Medium | High |
| Learning curve | Steep | Moderate | Easy (LLM-native) |
| Use case | Education | Reference | Production |
| Status | Historical | Reference | **Current** |

### Shared Words

These words exist in both v1 and v2 with compatible meanings:
```
nu, ve, so, vi, mu, lu, ta, fi
```

### Common Mappings

| v1 | → v2 (primary) |
|----|----------------|
| ko | sol OR kno |
| ra | lin OR rea |
| zo | mov |
| ga | cau OR giv |
| du | cha OR dif |

---

*This guide addresses limn-land-6k3. For questions, consult the linguist.*
