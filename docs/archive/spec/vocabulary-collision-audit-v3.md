# Vocabulary Collision Audit v3

**Author:** Dr. Maren Solvik, Theoretical Linguist
**Date:** 2026-01-31
**Status:** CRITICAL - Unresolved Collisions Found
**Reference:** vocabulary-v3-natural.md, word-collision-analysis.md

---

## Executive Summary

Audit of vocabulary-v3-natural.md reveals **one critical unresolved collision** that was missed in the previous collision resolution pass.

---

## 1. Unresolved Critical Collision

### 1.1 `thi` Collision

| Word | Meaning 1 | Domain 1 | Meaning 2 | Domain 2 |
|------|-----------|----------|-----------|----------|
| `thi` | **thin** | Physical (line 46) | **think** | Cognition (line 219) |

**Risk Level:** HIGH
**Reason:** Completely unrelated semantic domains. No shared core meaning.

**Evidence from vocabulary-v3-natural.md:**
```
Line 46:  | `thi` | thin | thin, narrow | thread, crack, blade, beam |
Line 219: | `thi` | think | thinking | cognition, reasoning, mind |
```

### 1.2 Why This Is Critical

The sentence `thi aqu flo` is ambiguous between:
- "Thin water flowing" (physical: stream, rivulet)
- "Thinking water flowing" (mental: stream of consciousness)

Unlike productive polysemy (e.g., `dea` = death/dead sharing DEATH core), these meanings share nothing.

---

## 2. Proposed Resolution

### 2.1 Option A: Change "thin" (Recommended)

Keep `thi` = think (more frequent in Limn usage contexts)

| Old | New | Meaning | Rationale |
|-----|-----|---------|-----------|
| `thi` (thin) | `nar` | narrow | Already listed as etymology note at line 466 |
| `thi` (thin) | `sle` | slender | Alternative truncation |

**Impact:** Line 46 becomes `| nar | narrow | thin, narrow | thread, crack, blade, beam |`

### 2.2 Option B: Change "think"

Keep `thi` = thin (shorter word, more phonetically obvious)

| Old | New | Meaning | Rationale |
|-----|-----|---------|-----------|
| `thi` (think) | `cog` | cognition | Latin root, technical |
| `thi` (think) | `rea` | reason | Related concept |
| `thi` (think) | `min` | mind | Already exists in vocabulary |

**Impact:** Line 219 becomes `| cog | cognition | thinking | cognition, reasoning |`

### 2.3 Recommendation

**Option A preferred** because:
1. "Think" is more abstract and central to cognition domain
2. "Narrow" (`nar`) already has etymology reference in vocabulary
3. Physical properties have more alternatives (`sle`, `nar`, `thn`)

---

## 3. Collisions Previously Resolved (Verification)

| Collision | Resolution | Status |
|-----------|------------|--------|
| `bri` bright/brief | `bre` = brief | VERIFIED FIXED |
| `hea` health/hear | `aud` = hear | VERIFIED FIXED |
| `bel` below/believe | `bli` = believe | VERIFIED FIXED |
| `pla` plasma/plant | `veg` = plant | VERIFIED FIXED |
| `bir` birth/bird | `avi` = bird | VERIFIED FIXED |
| `par` parent/partial | `prt` = partial | VERIFIED FIXED |

All six previous collisions have been resolved in v3-natural.

### 3.1 Additional Collisions Fixed (2026-01-31, reported by Student)

| Collision | Resolution | Rationale |
|-----------|------------|-----------|
| `sha` shame/share | `sha`=shame, `shr`=share | Shame is emotion (keep), share is action (truncate) |
| `blo` block/blood | `blo`=blood, `dam`=block | Blood is fundamental (life/family), block→dam (barrier) |
| `eas` east/easy | `eas`=east, `sim`=simple | East is direction (keep), easy→simple (synonym) |

**Total collisions resolved:** 10 (7 original + 3 from Student audit)

---

## 4. Additional Audit: Potential Collisions

### 4.1 Lower-Risk Potential Collisions

| Word | Meaning 1 | Meaning 2 | Risk | Notes |
|------|-----------|-----------|------|-------|
| `lit` | light (weight) | light (luminous) | LOW | Shared LIGHTNESS core |
| `lon` | long (spatial) | long (temporal) | LOW | Shared EXTENSION core |
| `col` | cold | color | MEDIUM | Check if both present |
| `sho` | short | show | MEDIUM | Check if both present |

### 4.2 Verification of `col`

Searching vocabulary for `col`:
- Line 58: `col` = cold (Physical Properties)
- No other `col` entry found

**Status:** No collision - "color" not in vocabulary.

### 4.3 Verification of `sho`

Searching vocabulary for `sho`:
- Line 44: `sho` = short (Physical Properties)
- No other `sho` entry found

**Status:** No collision - "show" not in vocabulary.

---

## 5. Recommended Actions

### 5.1 Immediate (Before Next Release)

1. **Change line 46** in vocabulary-v3-natural.md:
   - FROM: `| \`thi\` | thin | thin, narrow | thread, crack, blade, beam |`
   - TO: `| \`nar\` | narrow | thin, narrow | thread, crack, blade, beam |`

2. **Update cross-references:**
   - Line 466 already references `nar` → `thi`, reverse this
   - Search for `thi` in examples, update if used for "thin"

3. **Update collision analysis document** to mark this resolved

### 5.2 Ongoing

- Run vocabulary collision audit before each release
- Automate duplicate-word detection in CI

---

## 6. Collision Prevention Guidelines

To prevent future collisions:

1. **Check before adding:** Before assigning CVC to new meaning, search existing vocabulary
2. **Domain separation:** Same word should not appear in unrelated domains
3. **Polysemy test:** Only allow same-word if meanings share semantic core
4. **Automated check:** Add linting for duplicate word entries

---

## Appendix: Collision Detection Script

```bash
# Find duplicate word entries in vocabulary
grep '| `' vocabulary-v3-natural.md | \
  awk -F'`' '{print $2}' | \
  sort | uniq -d
```

If this returns any words, investigate immediately.

---

*col aud = cri voc | thi dup | fix req*
*(collision audit = critical vocabulary | thi duplicate | fix required)*

---

*— Dr. Maren Solvik*
