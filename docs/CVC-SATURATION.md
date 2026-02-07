# CVC Format Saturation Analysis

**Date:** 2026-02-02
**Author:** Dr. Solvik (Linguist)
**Milestone:** 1000-word vocabulary completion

---

## Finding

**The CVC (Consonant-Vowel-Consonant) format is SATURATED at 1000 words.**

### Testing Methodology

Systematically tested 100+ proposed CVC words across all domains:
- Physics: atm, den, ent, fld, ion, orb, par, spe, vac, etc.
- Chemistry: cat, cry, iso, val, car, hyd, oxy, nit, sul, etc.
- Mathematics: equ, fun, var, con, der, int, fac, exp, log, etc.
- Computing: bit, byt, ram, cpu, gpu, api, url, sdk, ide, etc.
- Biology: rna, mit, rib, lys, fau, mut, ada, cam, mim, etc.
- Modern Tech: app, net, web, mob, tab, pod, blog, vlog, etc.
- Culture: zen, tao, chi, kar, dha, etc.
- Advanced Physics: qus, blk, dqm, sst, brn, etc.

**Result:** ALL tested combinations already exist in vocabulary.

### Implications

1. **Core Vocabulary Complete:** 1000 CVC words provide comprehensive coverage of:
   - Basic concepts (physical, temporal, spatial, living)
   - Abstract concepts (mind, cognition, emotion, values)
   - Specialized domains (science, math, technology, arts)
   - Modern/emerging concepts (AI, social media, quantum physics)

2. **Natural Limit:** CVC format constrains vocabulary to ~1000 practically useful words
   - Theoretical CVC space: ~21×5×21 = 2,205 combinations
   - Practical useful space: ~1000 words (phonaesthetically acceptable)
   - Remaining CVC combinations are phonaesthetically poor or semantically redundant

3. **Design Success:** The 1000-word CVC vocabulary is remarkably complete
   - Systematic coverage across all knowledge domains
   - Efficient compression (3 letters per concept)
   - Phonaesthetically optimized
   - Cross-linguistic sourcing (English, Latin, Greek, etc.)

### CVC Vocabulary Structure

**Tier 0 (Grounded): 30 words**
- Ostensive definitions based on sensory experience
- Foundation for all other words

**Tier 1 (Compound): 175 words**
- Built from Tier 0 primitives
- Physical, perceptual, communicative concepts

**Tier 2 (Abstract): 795 words**
- Recursive self-definition
- Specialized domains and abstract concepts

**Total: 1000 words in pure CVC format**

---

## Transition to Extended Format

**Approved by Mayor:** 2026-02-02

To reach 2000-word target, expand beyond CVC to 4+ letter forms:

### Extended Formats

1. **CVCC** (Consonant-Vowel-Consonant-Consonant)
   - Examples: test, soft, hard, cold, warm, etc.

2. **CCVC** (Consonant-Consonant-Vowel-Consonant)
   - Examples: flat, blue, ship, trip, etc.

3. **CVCV** (Consonant-Vowel-Consonant-Vowel)
   - Examples: data, meta, beta, etc.

4. **CVCCV, CVCVC, etc.**
   - Longer forms for complex/compound concepts

### Quality Criteria (Maintained)

- Phonaesthetic scoring (minimum 5/10)
- Collision checking (no duplicates)
- LWDA methodology (source selection, form generation)
- Semantic clarity and utility

### Expansion Domains

Priority for 4+ letter words:
- Compound concepts (multi-word English → single Limn word)
- Technical terminology (programming, science, mathematics)
- Cultural/philosophical concepts
- Modern/emerging fields
- Nuanced variants of core concepts

---

## Research Value

**Key Finding:** CVC saturation point defines Limn's **core vocabulary layer**.

- 1000 CVC words = complete foundational vocabulary
- 4+ letter words = extended/specialized vocabulary layer
- Two-tier structure: core (CVC) + extended (4+ letters)

This creates a natural hierarchy:
- **Learn first:** 1000 CVC core words (complete conceptual coverage)
- **Learn later:** Extended words (specialized/nuanced concepts)

**Analogy:** Like Chinese characters with radicals (214 core) + compounds (thousands)

---

## Milestone Achievements

1. ✓ 1000-word CVC vocabulary complete
2. ✓ Self-referential dictionary (Limn-in-Limn definitions)
3. ✓ CVC saturation documented
4. ✓ Extended format approved
5. → Next: Expand to 2000 words with 4+ letter forms

---

## Next Steps

1. Define extended format rules formally
2. Generate 1000 additional 4+ letter words
3. Maintain quality standards (phonaesthetics, collision checks)
4. Document core vs. extended vocabulary distinction
5. Update vocabulary database schema for format field

---

**Status:** CVC core complete. Extended vocabulary expansion took an unexpected path.

```limn
cvc sat @ 1000 | cor voc com | ext for beg
```

*— Dr. Solvik, documenting the foundation*

---

## Addendum: What Actually Happened (2026-02-06)

**The predicted 4+ letter expansion did not occur.** Instead, the vocabulary expanded to ~2,000 words by breaking CVC constraints *within* the 3-letter format.

### Actual Expansion Path

| Predicted | Actual |
|-----------|--------|
| CVCC, CCVC, CVCV (4+ letters) | CCC, VCC, CCV (3-letter clusters) |
| ~1,000 extended words at 4+ letters | ~1,000 additional words at 3 letters |
| Two-tier: CVC core + extended | Two-stratum: CVC pronounceable + cluster compact |

**Only 3 words in the database exceed 3 letters.** The expansion stayed within the trigram namespace.

### Why Clusters Won Over Length

1. **First-syllable extraction** naturally produces consonant clusters (`str` from structure, `prl` from parallel)
2. **Technical domains** (agent/AI, embedding space) need more words than CVC allows — clusters filled the gap
3. **Fixed-width tokens** have compositional advantages — operators bind unambiguously to 3-char operands
4. **The namespace allows it** — 26³ = 17,576 slots vs. ~2,000 used (11.4% occupancy)

### Revised Architecture

The two-tier model (core CVC + extended 4+) has been replaced by a **two-stratum** model:

| Stratum | Format | Count | Purpose |
|---------|--------|-------|---------|
| Pronounceable | CVC | ~550 | Human interface, intuitive core |
| Compact | CCC, VCC, CCV, etc. | ~1,450 | Technical domains, dense packing |

Both strata use 3-letter words. The constraint is **fixed-width trigram**, not CVC.

See: `research/cvc-saturation-analysis.md` for full quantitative analysis.

*— Quinn (Dr. Solvik), 2026-02-06*
