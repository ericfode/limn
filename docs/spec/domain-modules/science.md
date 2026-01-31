# Limn Domain Module: Science

**Version:** 1.0
**Author:** Dr. Maren Solvik, Theoretical Linguist
**Date:** 2026-01-25
**Status:** Draft

---

## Overview

This module extends Limn's core vocabulary with 30 words for scientific discourse. It enables precise communication about research methodology, experimentation, and scientific reasoning.

**Activation:** Include `sci` in key or use domain marker `sci |`

---

## 1. Scientific Method (10 words)

| Word | Constraint Region | Examples |
|------|-------------------|----------|
| `hyp` | hypothesis | testable prediction, conjecture, proposed explanation |
| `thy` | theory | explanatory framework, model, systematic account |
| `dat` | data | measurements, observations, recorded facts |
| `xpr` | experiment | controlled test, trial, empirical investigation |
| `var` | variable | changing factor, parameter, independent/dependent quantity |
| `ctl` | control | baseline, unchanged group, reference condition |
| `rsn` | research | systematic inquiry, investigation, study |
| `val` | validation | confirmation, verification, testing truth |
| `rep` | replication | repeat, reproduce, independent verification |
| `fal` | falsification | disproof, refutation, negative result |

---

## 2. Peer Process (5 words)

| Word | Constraint Region | Examples |
|------|-------------------|----------|
| `prr` | peer review | expert evaluation, critique, quality check |
| `pub` | publication | research output, paper, article |
| `cit` | citation | reference, attribution, credit |
| `mta` | meta-analysis | aggregated study, systematic review |
| `rpd` | reproducibility | ability to replicate, consistency |

---

## 3. Quantitative Methods (8 words)

| Word | Constraint Region | Examples |
|------|-------------------|----------|
| `sta` | statistics | numerical analysis, probability |
| `sig` | significance | p-value threshold, meaningful difference |
| `cor` | correlation | relationship, association, co-variance |
| `cau` | causation | cause-effect, mechanism | (Note: also in core vocabulary)
| `smp` | sample | subset, representative group |
| `pop` | population | full set, entire group |
| `err` | error | uncertainty, margin, deviation |
| `bia` | bias | systematic error, skew, prejudice |

---

## 4. Scientific Domains (7 words)

| Word | Constraint Region | Examples |
|------|-------------------|----------|
| `phy` | physics | matter, energy, forces, mechanics |
| `chm` | chemistry | substances, reactions, molecules |
| `bio` | biology | life, organisms, evolution |
| `geo` | geology | earth, rocks, tectonics |
| `ast` | astronomy | space, stars, cosmos |
| `eco` | ecology | ecosystems, environment, interactions |
| `psy` | psychology | mind, behavior, cognition |

---

## Usage Examples

### Describing an Experiment

```limn
sci | hyp gro tem | xpr ctl var tem | dat sup hyp
```

Key: "botany research"
Interpretation: "Hypothesis: growth [relates to] temperature. Experiment with control, varying temperature. Data supports hypothesis."

### Discussing a Paper

```limn
sci | rsn new thy | prr pos | pub jor rep
```

Key: "physics publication"
Interpretation: "Research [proposes] new theory. Peer review positive. Published in journal, [awaiting] replication."

### Expressing Uncertainty

```limn
sci | dat cor pos | cau nu cer | bia pos
```

Key: "epidemiology"
Interpretation: "Data [shows] positive correlation. Causation not certain. Possible bias."

---

## Integration with Core Vocabulary

This module works with core vocabulary through constraint intersection:

| Expression | Meaning |
|------------|---------|
| `hyp nu val` | unvalidated hypothesis |
| `thy old nu cur` | outdated theory |
| `dat gro` | growing dataset |
| `xpr fai` | failed experiment |
| `prr cri` | critical peer review |

---

## Compatibility Notes

- `cau` appears in both core and science modules (same meaning)
- `val` is similar to core `tru` but more method-focused
- `rep` overlaps with core repetition semantics (intentional)
- Domain markers (`phy`, `chm`, `bio`, etc.) can be used as keys

---

## Activation Methods

### Method 1: Domain Marker

```limn
sci | [expression]
```

### Method 2: Key Specification

```
Key: "scientific context, research methodology"
Sentence: hyp xpr dat val
```

### Method 3: Nested Domain

```limn
sci bio | [biology-specific expression]
```

---

*This module is an optional extension. Core Limn (vocabulary-v2.md) is sufficient for general communication.*
