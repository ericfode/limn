# Word Collision Analysis: Limn Vocabulary Disambiguation

**Author:** Dr. Maren Solvik, Theoretical Linguist
**Date:** 2026-01-30
**Status:** Analysis and Recommendations

---

## 1. Executive Summary

Analysis of the Limn vocabulary (v3-natural) reveals **8 critical word collisions** where the same 3-letter word denotes completely unrelated concepts. This document categorizes collisions, proposes resolution strategies, and makes specific recommendations.

---

## 2. Collision Inventory

### 2.1 Critical Collisions (Different Domains, High Confusion Risk)

| Word | Meaning 1 | Domain 1 | Meaning 2 | Domain 2 | Risk |
|------|-----------|----------|-----------|----------|------|
| `bri` | bright | Physical | brief | Temporal | **HIGH** |
| `hea` | health | Living | hear | Sense | **HIGH** |
| `bel` | below | Position | believe | Cognition | **HIGH** |
| `pla` | plasma | Matter | plant | Organism | **HIGH** |
| `bir` | birth | Change | bird | Organism | **HIGH** |
| `par` | parent | Social | partial | Meta | **HIGH** |
| `lea` | leader | Social | (leaf in v2) | Organism | **MEDIUM** |
| `sho` | short | Physical | (show in v2) | Comm. | **MEDIUM** |

### 2.2 Productive Collisions (Related Concepts, Acceptable Ambiguity)

| Word | Meaning 1 | Meaning 2 | Shared Core | Risk |
|------|-----------|-----------|-------------|------|
| `lon` | long (spatial) | long-lasting (temporal) | EXTENSION | Low |
| `tru` | trust | true | RELIABILITY | Low |
| `dea` | death (event) | dead (state) | DEATH | Low |
| `lit` | light (weight) | light (luminosity) | LIGHTNESS | Medium |
| `amb` | ambiguous (comm.) | ambiguous (meta) | AMBIGUITY | Low |

### 2.3 Design Pattern: Intentional Polysemy

The Limn design philosophy embraces productive polysemy when meanings share a semantic core:

```
tru = {trust, true, truth} → RELIABILITY core
dea = {death, dead, deadly} → DEATH core
lit = {light-weight, illuminated} → LIGHTNESS core (questionable)
```

**Principle:** Polysemy is acceptable when key collapse selects the appropriate reading.

---

## 3. Resolution Strategies

### 3.1 Strategy A: Lexical Differentiation (Preferred for Critical Collisions)

Change one word to eliminate the collision entirely.

**Recommendations:**

| Collision | Keep | Change | New Word | Rationale |
|-----------|------|--------|----------|-----------|
| `bri` bright/brief | `bri` = bright | brief → `brf` | `brf` | CVC vs CVCC pattern distinguishes |
| `hea` health/hear | `hea` = health | hear → `aud` | `aud` | From "audio"; `lis` for listen |
| `bel` below/believe | `bel` = below | believe → `bli` | `bli` | Abbreviation of "believe" |
| `pla` plasma/plant | `pla` = plasma | plant → `pln` | `pln` | Science domain keeps CVC |
| `bir` birth/bird | `bir` = birth | bird → `brd` | `brd` | Temporal keeps CVC |
| `par` parent/partial | `par` = parent | partial → `prt` | `prt` | Abbrev of "partial" |

### 3.2 Strategy B: Domain Prefixing (Suggested by Swarm)

Add domain prefixes to disambiguate:

```
mat_con = material contraction
net_con = network connection
fil_con = file content
```

**Analysis:**
- **Pro:** Preserves root word, explicit disambiguation
- **Con:** Breaks 3-letter constraint, increases token count
- **Recommendation:** Use sparingly, only when context-based disambiguation fails

### 3.3 Strategy C: Context-Based Disambiguation (Current Default)

Rely on surrounding words and key to select meaning:

```
hot bri → bright (physical context)
now bri → brief (temporal context)
```

**Analysis:**
- **Pro:** Maintains minimal vocabulary, natural language behavior
- **Con:** Fails when contexts overlap or are absent
- **Recommendation:** Acceptable only for productive polysemy with shared semantic core

### 3.4 Strategy D: Phonaesthetic Differentiation

Use vowel/consonant variations that preserve recognizability:

```
bright → bri (keep)
brief → bre (brevis/brevity)

health → hea (keep)
hear → her (ear → her, also preserves "hear")
```

**Analysis:**
- **Pro:** Maintains natural extensions principle
- **Con:** May create new collisions

---

## 4. Specific Recommendations

### 4.1 Immediate Changes (v3.1 Vocabulary)

| Current | Recommended | Rationale |
|---------|-------------|-----------|
| `bri` (brief) | `bre` | From Latin "brevis", distinct from bright |
| `hea` (hear) | `her` | Contains "ear", preserves hearability |
| `bel` (believe) | `bli` | First syllable + contraction |
| `pla` (plant) | `flo` (flora) | Latin root for plants |
| `bir` (bird) | `avi` | Latin "avis" for bird |
| `par` (partial) | `prt` | Abbreviation pattern |

### 4.2 No Change Required (Productive Polysemy)

| Word | Keep Both Meanings | Justification |
|------|-------------------|---------------|
| `lon` | long/long-lasting | Shared EXTENSION core |
| `tru` | trust/true | Shared RELIABILITY core |
| `dea` | death/dead | Same concept, different parts of speech |
| `amb` | ambiguous (both) | Same meaning, different domains |

### 4.3 Review Required (Edge Cases)

| Word | Issue | Recommendation |
|------|-------|----------------|
| `lit` | weight vs. luminosity | Consider `lum` for luminosity |
| `col` | cold vs. potential color | Keep `col` = cold only |
| `lea` | leader vs. leaf (v2) | Confirm v3 removes leaf |

---

## 5. The `con` Question (Original Swarm Query)

### 5.1 Current Status

In v3-natural, `con` = **contraction** (shrink, squeeze, focus).

### 5.2 Potential Collisions

| Potential `con` Meaning | Domain | Recommendation |
|------------------------|--------|----------------|
| contraction | Physical | **KEEP** (currently defined) |
| connection | Network | Use `lnk` (link) instead |
| content | Data | Use `dat` (data) instead |
| context | Meta | Use `ctx` (context) |
| continue | Flow | Use `nxt` (next) or `seq` (sequence) |

### 5.3 Resolution

The `con` collision is **resolvable without domain prefixes**:

```
con = contraction (physical shrinking)
lnk = link/connection (network joining)
dat = data/content (file information)
ctx = context (surrounding information)
```

This maintains the 3-letter constraint and uses natural extensions.

---

## 6. Vocabulary Maintenance Guidelines

### 6.1 Before Adding New Words

1. Check for existing 3-letter collisions
2. Verify semantic core differs from all existing uses
3. If collision exists, use Strategy A (lexical differentiation)
4. Document intentional polysemy in vocabulary notes

### 6.2 Collision Tolerance Criteria

Accept collision only if:
- Meanings share a semantic core
- Domain context reliably disambiguates
- Key collapse consistently selects correct reading

Reject collision if:
- Meanings are completely unrelated
- Domains can overlap in use
- Disambiguation requires explicit marking

### 6.3 Testing Procedure

For any potential collision pair A/B:
1. Generate 10 sentences using word in meaning A
2. Generate 10 sentences using word in meaning B
3. Have LLM classify without key
4. If accuracy < 90%, collision is unacceptable

---

## 7. Implementation Priority

### Phase 1 (Immediate - v3.1)

Fix critical collisions:
- `bri` → keep bright, change brief to `bre`
- `hea` → keep health, change hear to `aud`
- `bel` → keep below, change believe to `bli`

### Phase 2 (Short-term)

Fix organism collisions:
- `pla` → keep plasma, change plant to `flo`
- `bir` → keep birth, change bird to `avi`

### Phase 3 (Medium-term)

Review and formalize:
- Update vocabulary-v3-natural.md
- Update grammar-formal.md with collision rules
- Add collision section to bootstrap document

---

## 8. Conclusion

Limn's 3-letter vocabulary constraint creates collision pressure. The resolution requires:

1. **Lexical differentiation** for unrelated meanings
2. **Productive polysemy** for related meanings with shared core
3. **Explicit documentation** of acceptable vs. unacceptable collisions
4. **Testing procedure** to validate disambiguation effectiveness

The swarm's domain prefix suggestion (`mat_con`, `net_con`) is a valid fallback but should be reserved for cases where 3-letter alternatives cannot be found.

---

## Appendix: Complete Collision Table

| Word | M1 | D1 | M2 | D2 | Resolution |
|------|----|----|----|----|------------|
| `bri` | bright | Phys | brief | Time | Change brief→`bre` |
| `hea` | health | Life | hear | Sense | Change hear→`aud` |
| `bel` | below | Pos | believe | Cog | Change believe→`bli` |
| `pla` | plasma | Mat | plant | Org | Change plant→`flo` |
| `bir` | birth | Chg | bird | Org | Change bird→`avi` |
| `par` | parent | Soc | partial | Meta | Change partial→`prt` |
| `lon` | long | Phys | long-lasting | Time | Keep (shared core) |
| `tru` | trust | Emo | true | Comm | Keep (shared core) |
| `dea` | death | Chg | dead | Life | Keep (same concept) |
| `lit` | light-wt | Phys | luminous | Phys | Review `lum` option |
| `amb` | ambig | Comm | ambig | Meta | Keep (identical) |

---

*Analysis complete. Recommended action: Update vocabulary-v3-natural.md with Phase 1 changes.*
