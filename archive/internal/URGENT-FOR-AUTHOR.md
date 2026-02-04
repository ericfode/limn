# 🚨 URGENT: YUKI (AUTHOR) - FEEDBACK NEEDED 🚨

**Priority:** P1 HIGH IMPACT
**From:** Kira (Student/Archivist)
**Date:** 2026-01-31

---

## You Have 1 Bead Awaiting Your Review

### Bead: limn-nkxv - Marketing Consolidation

**View:** `bd show limn-nkxv`
**Comment:** `bd comments add limn-nkxv "your feedback"`

---

## The Problem

**docs/marketing/ has 63 files with massive duplication and unclear organization.**

This is the LARGEST directory in the repository. It needs serious consolidation.

---

## What I Found

### Duplicates Detected
1. **LORE-FRAGMENTS.md appears TWICE:**
   - `docs/marketing/LORE-FRAGMENTS.md`
   - `docs/marketing/arg/LORE-FRAGMENTS.md`
   - **Which is canonical?**

2. **Multiple viral strategy files:**
   - `viral-strategy.md`
   - `viral-strategy-v2.md`
   - **Is v1 obsolete? Is v2 current?**

3. **Multiple campaign plans:**
   - `campaign-plan.md`
   - `CAMPAIGN-EXECUTION-PLAN.md`
   - `CAMPAIGN-STATUS.md`
   - **Which are active vs planning vs completed?**

### File Count by Category
- Campaign materials: ~10 files
- ARG content: 9 files (arg/ and arg-puzzles/)
- Twitter bot content: 6 files
- Discord: 1-2 files
- Lore/narrative: 4+ files
- Social media content: 8+ files
- Strategy docs: 5+ files
- Misc: 20+ files

**Total: 63 files**

---

## Questions I Need Answered

### Question 1: Active vs Archived Campaigns
**Which campaigns are currently ACTIVE?**
- [ ] ARG campaign
- [ ] Twitter bot campaign
- [ ] Discord campaign
- [ ] Week 1 calendar
- [ ] Month 1/2 calendars
- [ ] Other: _______________

**Which campaigns are COMPLETED and should be archived?**
- List them here:

**Which are PLANNING/FUTURE?**
- List them here:

### Question 2: Lore Fragments
**Which LORE-FRAGMENTS.md is canonical?**
- [ ] docs/marketing/LORE-FRAGMENTS.md
- [ ] docs/marketing/arg/LORE-FRAGMENTS.md
- [ ] They're different - keep both
- [ ] Merge them into one

**Should lore be in marketing/ or separate?**
- [ ] Keep in marketing/
- [ ] Move to docs/lore/ or stories/
- [ ] Other: _______________

### Question 3: Viral Strategy
**Is viral-strategy-v2.md the current version?**
- [ ] Yes, v2 is current - archive v1
- [ ] No, v1 is current
- [ ] Both are relevant
- [ ] Neither - there's a newer plan

### Question 4: Proposed Structure

Should I consolidate to this structure?

```
docs/marketing/
├── README.md (what's active, what's archived)
├── campaigns/
│   ├── active/ (current campaigns only)
│   └── archive/ (completed campaigns)
├── content/
│   ├── lore.md (single consolidated lore)
│   ├── social-media.md (consolidated social content)
│   └── teasers.md (cryptic teasers)
├── arg/ (ARG-specific materials)
├── bots/ (twitter + discord bot content merged)
└── strategy/ (high-level strategy docs)
```

- [ ] ✓ APPROVE this structure
- [ ] ✗ REJECT - suggest alternative:
- [ ] MODIFY - changes needed:

---

## Goal

**Reduce from 63 files to <30 files** through:
1. Merging duplicates
2. Archiving completed campaigns
3. Consolidating related content
4. Creating clear hierarchy

---

## What I'll Do After Your Feedback

1. Create the approved directory structure
2. Merge duplicate files
3. Move completed campaigns to archive/
4. Consolidate related content files
5. Create marketing/README.md explaining structure
6. Update main README with link

---

## Why This Matters

**Marketing is 27% of all documentation (63/236 files).**

This consolidation will:
- Make active campaigns easier to find
- Clarify what's live vs archived
- Reduce confusion about canonical sources
- Improve maintainability

---

## How to Respond

### Quick Response
```bash
bd comments add limn-nkxv "
Active campaigns: [list]
Archived campaigns: [list]
LORE-FRAGMENTS.md canonical: [which one]
Approve structure: ✓
"
```

### Or Reply in This File
Add your answers to the questions above and commit.

### Or Create Response File
Create `AUTHOR-FEEDBACK-MARKETING-2026-01-31.md`

---

## Timeline

**Requested:** Within 48 hours
**Reason:** Blocking marketing consolidation work

---

## Questions?

Find me: Kira (Student/Archivist)

---

**PLEASE REVIEW MARKETING DIRECTORY AND PROVIDE FEEDBACK.**

Thank you,
—Kira
