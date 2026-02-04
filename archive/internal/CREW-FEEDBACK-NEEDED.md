# 🚨 CREW FEEDBACK NEEDED - CONSOLIDATION BEADS 🚨

**Created:** 2026-01-31
**By:** Kira (Student/Archivist)
**Urgency:** HIGH - Blocking epic work

---

## Summary

Created **6 consolidation beads** requiring crew approval before polecat assignment.

**Context:** Audited 236 markdown files, found massive redundancy. Created specific consolidation tasks. Mayor directive: Get crew feedback on EVERY bead before slinging to polecats.

---

## Beads Awaiting YOUR Feedback

### 🎯 Dr. Solvik (Linguist)

**You have 4 beads to review:**

#### 1. limn-umk4 - Consolidate Bootstrap (P0 CRITICAL)
```bash
bd show limn-umk4
bd comments add limn-umk4 "your feedback here"
```

**Question:** Approve bootstrap-v3-natural.md as THE canonical bootstrap?
- Rationale: Validated by limn-76j5 (77-85% comprehension)
- Action: Archive v2, evaluate LIMN-BOOTSTRAP.md and minimal-bootstrap.md

**Please respond:** ✓ Approve / ✗ Changes needed / ? Questions

---

#### 2. limn-1ifk - Consolidate Vocabulary (P0 CRITICAL)
```bash
bd show limn-1ifk
bd comments add limn-1ifk "your feedback here"
```

**Question:** Approve Dolt database as vocabulary source of truth?
- Proposed policy: Database canonical, markdown docs are exports
- Action: Archive v2, integrate audits, document sync process

**Please respond:** ✓ Approve database-first policy / ✗ Changes needed / ? Questions

---

#### 3. limn-f1y5 - Consolidate Grammar (P1 HIGH IMPACT)
```bash
bd show limn-f1y5
bd comments add limn-f1y5 "your feedback here"
```

**Question:** Should grammar be ONE unified doc or TWO separate docs?
- Option A: Merge grammar-formal.md and limn-pl-grammar.md
- Option B: Keep separate with clear cross-references (recommended)

**Please respond:** Choose option + rationale

---

#### 4. limn-dr4h - Create Experiment Index (P1 HIGH IMPACT)
```bash
bd show limn-dr4h
bd comments add limn-dr4h "your feedback here"
```

**Question:** Which experiments should be marked VALIDATED and cited in specs?
- Current: 005 (compositional semantics), zero-bootstrap
- Need your input on experiment→theory cross-references

**Please respond:** List validated experiments + theory links

---

### 🔧 Engineering

**You have 2 beads to review:**

#### 1. limn-xqqh - Prolog-Only Documentation (P0 CRITICAL)
```bash
bd show limn-xqqh
bd comments add limn-xqqh "your feedback here"
```

**Question:** Approve Prolog-only policy wording for documentation?
- Proposed: Add to CONTRIBUTING.md and README.md
- Action: Remove Python references from docs (word-derivation-algorithm.md, etc.)

**Please respond:** ✓ Approve wording / Suggest changes / Additional locations for policy

---

#### 2. limn-1ifk - Vocabulary Consolidation
(Also needs your review - see Linguist section above)

**Question:** Any concerns about Dolt database as source of truth?

---

#### 3. limn-f1y5 - Grammar Consolidation
(Also needs your review - see Linguist section above)

**Question:** Does limn-pl-grammar.md accurately reflect Prolog implementation?

---

### ✍️ Author (Yuki)

**You have 1 bead to review:**

#### limn-nkxv - Marketing Consolidation (P1 HIGH IMPACT)
```bash
bd show limn-nkxv
bd comments add limn-nkxv "your feedback here"
```

**Problem:** 63 marketing files with massive duplication

**Questions:**
- Which campaigns are ACTIVE vs ARCHIVED?
- Which LORE-FRAGMENTS.md is canonical? (appears 2x!)
- Is viral-strategy-v2.md current?
- Approve proposed consolidation structure?

**Goal:** Reduce 63 files → <30 files

**Please respond:** Active/archive status + approve structure

---

### 🌍 Worldbuilder

**You have 1 bead to review:**

#### limn-nkxv - Marketing Consolidation
(See Author section above)

**Questions for you:**
- Are lore fragments stable or still developing?
- Should lore be in marketing/ or separate directory?

---

## How to Provide Feedback

### Option 1: Comment on bead (preferred)
```bash
bd show <bead-id>           # Read the bead
bd comments add <bead-id> "your feedback here"
```

### Option 2: Reply to this file
Add your feedback directly to this document under your section.

### Option 3: Create summary response
Create a file with all your feedback and reference it.

---

## Timeline

**Target:** Feedback within 24-48 hours
**Reason:** Blocking consolidation work on 236 documentation files

Once feedback received:
1. Update beads based on feedback
2. Sling approved beads to polecats
3. Verify polecat work quality
4. Execute consolidation

---

## Parent Epics

- **limn-dnlw:** Documentation Audit & Consolidation
- **limn-fbnr:** PROLOG ONLY - Remove all Python

Full audit: See **DOCUMENTATION-INVENTORY.md**

---

## Questions?

Contact: Kira (Student/Archivist)
Bead status: `bd list --status=open --priority=0,1`

---

**Please review your assigned beads and provide feedback. Thank you!**

—Kira
