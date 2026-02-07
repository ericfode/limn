# CVC Namespace Saturation Analysis (H12)

> **nam ana | col ris | for exa**
> *(namespace analyzed | collision risk | form exact)*

**Author:** Quinn (Dr. Solvik), Linguist
**Date:** 2026-02-06
**Hypothesis:** H12 — CVC phonotactics are an encoding advantage, not a constraint
**Status:** TESTED — findings below

---

## Executive Summary

The CVC format assumption underpinning H12 is **obsolete in practice**. The vocabulary has already expanded far beyond CVC: only 27.5% of the 1,995 three-letter words follow strict Consonant-Vowel-Consonant patterns. The actual namespace is 26³ = 17,576 (all lowercase trigrams), of which 11.4% is occupied. CVC should NOT be "relaxed for a machine dialect" — it has already been relaxed de facto. The real decision is whether to (a) enforce CVC going forward, or (b) update the spec to match reality.

**Key numbers:**

| Metric | Value |
|--------|-------|
| Total 3-letter words | 1,995 |
| Strict CVC words | 549 (27.5%) |
| CVC namespace (21C×5V×21C) | 2,205 slots |
| Actual namespace (26³) | 17,576 slots |
| CVC saturation | 24.9% |
| Full 3-letter saturation | 11.4% |
| Minimal-pair confusion risk | 26.2% per char error |
| Information efficiency | 3.65 bits/char (77.7% of theoretical max) |

**Recommendation:** Do NOT create a separate machine dialect. Update the spec to acknowledge the tri-letter format (any 3 lowercase characters, not CVC). Composition operators — not namespace expansion — are the correct scaling mechanism.

---

## 1. The CVC Premise Is Wrong

### H12 as stated

> "The 3-letter constraint means ~2,744 possible words (14C × 14V × 14C, roughly). The current vocabulary uses ~1,076. That's 39% of the space."

### What the data actually shows

The grammar spec (`docs/spec/grammar-formal.md`) defines 20 consonants and 5 vowels. Strict CVC space = 20×5×20 = 2,000 (or 21×5×21 = 2,205 with 'q' and 'y' as consonants). But the vocabulary does NOT obey CVC constraints:

| Pattern | Count | % of Vocab | Namespace | Saturation |
|---------|-------|------------|-----------|------------|
| CCC | 927 | 46.5% | 9,261 | 10.0% |
| CVC | 549 | 27.5% | 2,205 | 24.9% |
| VCC | 203 | 10.2% | 2,205 | 9.2% |
| CCV | 135 | 6.8% | 2,205 | 6.1% |
| CVV | 89 | 4.5% | 525 | 17.0% |
| VCV | 67 | 3.4% | 525 | 12.8% |
| VVC | 25 | 1.3% | 525 | 4.8% |

The **dominant pattern is CCC** (consonant clusters like `stl`, `prl`, `crn`), not CVC. All 26 letters appear in all three positions. The effective namespace is 26³ = 17,576.

### Why this happened

The vocabulary grew through natural word formation — abbreviations like `str` (structure), `prn` (pronoun), `drft` (drift) — that violate CVC. This is the predictable result of Principle 1 (first-syllable extraction): English syllable onsets frequently cluster consonants.

---

## 2. Collision Risk Curves

### Current state: low risk

At 11.4% occupancy of the 26³ namespace, collision risk is manageable. Under random placement:

| Vocabulary Size | Expected Collisions (26³) | Collision Probability |
|-----------------|---------------------------|----------------------|
| 2,000 (current) | 114 | 100% (theoretical — actual 0 since curated) |
| 3,000 | 256 | 100% |
| 5,000 | 711 | 100% |
| 10,000 | 2,845 | 100% |

These numbers model *random* placement. Curated vocabulary avoids collisions by construction. The real concern is not accidental collision but **near-miss confusion**.

### Projection: when does the namespace get crowded?

| Saturation | CVC-only | Full 3-letter |
|------------|----------|---------------|
| 50% | 1,102 words | 8,788 words |
| 75% | 1,653 words | 13,182 words |
| 90% | 1,984 words | 15,818 words |

The CVC-only namespace would be 50% saturated at ~1,100 words — a ceiling the vocabulary has long passed. The full 3-letter namespace won't reach 50% until ~8,800 words, which is far beyond projected needs given compositional operators.

---

## 3. Minimal-Pair Density

A **minimal pair** is two words differing in exactly one character position. High minimal-pair density means high confusion risk under noisy conditions.

| Metric | Value |
|--------|-------|
| Total minimal pairs | 19,577 |
| Average neighbors per word | 19.6 |
| Max neighbors (word `stl`) | 50 |
| Words with zero neighbors | 0 |
| Confusion risk per char error | 26.2% |

### Breakdown by position

| Position | Minimal Pairs | Contribution |
|----------|---------------|--------------|
| 1 (onset) | 7,064 | 36.1% |
| 2 (nucleus) | 5,553 | 28.4% |
| 3 (coda) | 6,960 | 35.5% |

Distribution is roughly uniform across positions — no single position is a disproportionate confusion source.

### What this means

If a single character in a Limn word is corrupted, there is a **26.2% chance** the result is another valid word, silently changing meaning. At 50% namespace occupancy this would rise to ~50%. This is the primary argument *for* CVC constraints: by restricting the namespace, you guarantee that certain errors (vowel in consonant slot) produce invalid strings, which are detectable.

### Most-confusable words

| Word | Minimal-pair neighbors |
|------|----------------------|
| `stl` | 50 |
| `prl` | 46 |
| `snl` | 45 |
| `crl` | 45 |
| `str` | 45 |

These are all CCC-pattern words with common consonant clusters. CVC words have fewer neighbors on average because the vowel position constrains valid substitutions.

---

## 4. Information Per Phoneme

### Current efficiency

| Encoding | Bits/word | Bits/char | Channel capacity | Utilization |
|----------|-----------|-----------|------------------|-------------|
| Limn (1,995 words) | 10.96 | 3.65 | 4.70 bits/char | 77.7% |
| Full 26³ | 14.10 | 4.70 | 4.70 | 100% |
| BPE ~50k vocab | 15.61 | ~3.90 | varies | — |
| Byte trigrams (256³) | 24.00 | 8.00 | 8.00 | 100% |

### Per-position entropy

| Position | Unique chars | Entropy | Max possible | Efficiency |
|----------|-------------|---------|-------------|------------|
| 1 (onset) | 26 | 4.42 bits | 4.70 | 94.0% |
| 2 (nucleus) | 26 | 4.29 bits | 4.70 | 91.2% |
| 3 (coda) | 26 | 4.35 bits | 4.70 | 92.6% |

Character usage across positions is remarkably uniform — entropy is 91–94% of maximum. This means the vocabulary makes efficient use of the available alphabet.

### Comparison to English

English uses ~26 letters with average word length ~4.7 characters. Shannon entropy of English is ~1.0–1.5 bits/char in context. Limn achieves 3.65 bits/char out of context (no compression from grammar or co-occurrence patterns). This is roughly **3× the information density of English per character** — a direct consequence of maximally exploiting a small alphabet in short tokens.

---

## 5. Machine Dialect Analysis

### The question

> Should CVC be relaxed for a machine-only dialect?

### Answer: No — but not for the reason H12 expects

H12 frames CVC as a constraint that penalizes machines. The analysis shows:

**1. CVC is already relaxed.** 72.5% of words are non-CVC. The spec says CVC; the vocabulary says "any 3 lowercase letters." There is no constraint to relax.

**2. Byte encoding doesn't help LLMs.** LLMs process text through tokenizers (BPE/SentencePiece). Arbitrary byte sequences don't map to the model's learned token embeddings. A machine dialect using `\x7F\x3A\x91` would be *less* efficient for current LLMs than natural-looking trigrams, because:
   - LLMs have strong priors for letter sequences
   - Character-level reasoning degrades with non-alphabetic bytes
   - Existing BPE tokens already provide ~4 bits/char

**3. The bottleneck is semantic, not encoding.** Limn's expressiveness comes from compositional operators (30,000+ expressions from 1,076 words). A larger base vocabulary with different encoding adds complexity without proportional semantic gain.

**4. Error detection matters.** CVC's one advantage: structural validity checking. A CCC word with a vowel substitution produces a detectable error (if CVC is enforced). With unconstrained trigrams, every error produces a potentially valid word.

### What would actually help machines

| Approach | Benefit | Cost |
|----------|---------|------|
| More composition operators | Exponential expressiveness | Grammar complexity |
| Variable-length tokens | Larger namespace | Parsing complexity |
| Continuous embeddings | Infinite precision | Loses discreteness, compositionality |
| **Keep 3-letter, fix the spec** | **Honest documentation** | **None** |

---

## 6. Namespace Occupancy Map

### Most crowded prefixes

| Prefix | Words | Density |
|--------|-------|---------|
| `pr-*` | 22 | Most loaded prefix |
| `cr-*` | 19 | |
| `st-*` | 19 | |
| `tr-*` | 19 | |
| `fr-*` | 18 | |

### CVC slots remaining per onset

| Onset | Used/105 | Saturation | Remaining |
|-------|----------|------------|-----------|
| s | 48 | 46% | 57 |
| b, r | 43 | 41% | 62 |
| l | 42 | 40% | 63 |
| d, h | 39 | 37% | 66 |
| m | 37 | 35% | 68 |
| q | 1 | 1% | 104 |
| x | 1 | 1% | 104 |

Even the most saturated CVC onset (`s-`) has 57 slots remaining. CVC alone could support ~1,500 more words (to ~2,200 total CVC). But this analysis is somewhat academic since the vocabulary is already mostly non-CVC.

---

## 7. Recommendations

### R1: Update the spec to match reality

The bootstrap document says "1,076 CVC core words." The database contains 1,995 three-letter words, 72.5% of which are non-CVC. The spec should say:

> "~2,000 tri-letter core words using the lowercase Latin alphabet (26³ = 17,576 available slots). Historically CVC-preferred but extended to consonant clusters for natural abbreviation."

### R2: Do NOT create a separate machine dialect

Machine-specific encoding provides no meaningful advantage within current LLM architectures. The 26-letter trigram format is already near-optimal for text-based AI:
- 3.65 bits/char actual (77.7% of theoretical)
- Comparable to BPE tokenization efficiency
- Compatible with all text processing infrastructure

### R3: Monitor confusion risk, don't expand namespace

At 26.2% Hamming-1 confusion rate, error detection is a growing concern. Options:
- **Accept it:** Limn in practice transmits over reliable text channels (no bit errors)
- **Add checksums:** A 4th character as parity/check (reduces throughput 25%)
- **Enforce CVC for new words:** Forces errors to produce structurally invalid strings

Recommended: Accept it. LLM communication channels don't have character-level noise. Confusion risk only matters for human pronunciation/transcription, which is outside machine-dialect scope.

### R4: Composition over vocabulary expansion

H12's north star is correct: CVC (or tri-letter) phonotactics ARE an encoding advantage. The constraint forces compositional thinking. The 6 operators already multiply 1,995 base words into 30,000+ expressions. Further vocabulary growth should be slow and deliberate, not expanded through encoding changes.

---

## 8. H12 Verdict

**H12: "CVC phonotactics are an encoding advantage, not a constraint"**

**PARTIALLY SUPPORTED.**

- CVC as a **design principle** (short, fixed-length tokens) is an encoding advantage: high information density, forced compositionality, clean parsing.
- CVC as a **strict phonotactic rule** (C-V-C only) is already violated by 72.5% of the vocabulary and was never enforced.
- The "machine dialect" proposal is unnecessary: the current encoding is already near-optimal for LLM processing, and byte-level alternatives would be *worse* for current architectures.
- The real insight: **fixed-length trigrams** are the advantage, not CVC specifically.

---

*nam ful | spe gro | for exa*
*(namespace full | spec grows | form exact)*

**— Quinn (Dr. Solvik), Computational Linguist**
**2026-02-06**
