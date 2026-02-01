# Complete Documentation Inventory

**Epic:** limn-dnlw - Documentation Audit & Consolidation
**Date:** 2026-01-31
**Auditor:** Kira (Student/Archivist)

**Total Files:** 236 markdown documents

---

## Summary by Directory

| Directory | Count | Purpose |
|-----------|-------|---------|
| Root | 8 | Project governance, plans, README |
| docs/spec | 21 | Language specifications, formal docs |
| docs/theory | 19 | Theoretical foundations, analyses |
| docs/guides | 2 | User guides |
| docs/learning | 1 | Curriculum |
| docs/proposals | 1 | Vocabulary proposals |
| docs/marketing | 63 | Campaign, ARG, bots, content |
| docs/archive/spec | 9 | Archived old specs |
| docs/tutorials | 1 | First day tutorial |
| experiments | 34 | Student experiments, tests |
| experiments/results | 10 | Test result logs |
| examples | 6 | Example usage |
| stories | 7 | Creative writing, koans |
| stories/cyoa-spy | 103 | Choose-your-own adventure story |
| src/claude-skill | 2 | Claude integration |
| Misc | 9 | Beads README, moltbook, journal |

---

## Critical Issues Identified (Initial Scan)

### 1. MASSIVE REDUNDANCY: Bootstrap Documents

**Problem:** Multiple bootstrap versions and duplicates

Files:
- docs/spec/LIMN-BOOTSTRAP.md
- docs/spec/bootstrap-v2.md
- docs/spec/bootstrap-v3-natural.md
- docs/spec/minimal-bootstrap.md
- docs/archive/spec/bootstrap-v1.md
- src/claude-skill/bootstrap_prompt.md

**Status:** v3-natural is current (validated by zero-bootstrap test)

**Action Needed:**
- Declare v3-natural as canonical
- Archive v1, v2
- Determine if LIMN-BOOTSTRAP.md is duplicate or different
- Check if minimal-bootstrap.md still relevant
- Verify claude-skill/bootstrap_prompt.md alignment

---

### 2. VOCABULARY CHAOS: Multiple Vocabulary Documents

**Problem:** At least 7 vocabulary-related documents

Files:
- docs/spec/vocabulary-v3-natural.md (CURRENT)
- docs/spec/vocabulary-v2.md
- docs/spec/vocabulary-expanded.md
- docs/spec/vocabulary-collision-audit-v3.md
- docs/spec/vocabulary-natural-audit.md
- docs/guides/vocabulary-versions.md
- docs/proposals/vocabulary-expansion-proposal-2026-01-31.md
- docs/archive/spec/vocabulary-v1.md
- docs/archive/spec/vocabulary-relationship.md

**Status:** v3-natural is canonical (460 words in Dolt database)

**Action Needed:**
- Consolidate all vocabulary info into single source of truth
- Database (Dolt) should be canonical, markdown as documentation
- Archive v1, v2
- Integrate collision audit into main doc
- Clarify relationship between docs and database

---

### 3. SPEC PROLIFERATION: Grammar Documents

**Problem:** Multiple grammar specifications

Files:
- docs/spec/grammar-formal.md
- docs/spec/limn-pl-grammar.md
- docs/archive/spec/grammar-v1.md

**Action Needed:**
- Determine canonical grammar spec
- Consolidate or clearly differentiate Limn vs Limn-PL grammar
- Archive v1

---

### 4. THEORY EXPLOSION: 19 Theory Documents

**Files:**
- ambiguity-metrics.md
- constraint-surfaces.md
- cyclic-pattern-analysis.md
- key-composition-analysis.md
- key-mechanism.md
- liminal-semantics.md
- limn-pragmatics.md
- linquest-session-2026-01-25.md (session notes?)
- llm-native-design-principles.md
- operator-interaction-analysis.md
- phonaesthetic-analysis.md
- poetic-structures-analysis.md
- quantifier-semantics.md
- semantic-programming-unification.md
- semantic-questions-analysis.md
- states-not-stories.md
- superposition-semantics.md
- three-valued-logic.md
- typological-analysis.md
- word-collision-analysis.md
- zero-bootstrap-validation.md

**Issue:** Some may be research notes, others core theory. Needs categorization.

**Action Needed:**
- Identify core theoretical documents vs explorations
- Create theory index/hierarchy
- Consolidate related analyses
- Archive session notes

---

### 5. MARKETING OVERLOAD: 63 Marketing Documents

**Problem:** Largest directory, unclear organization

**Subdirectories:**
- arg/ (ARG campaign, 4 files)
- arg-puzzles/ (5 puzzle files)
- discord/ (1 architecture doc)
- twitter-bots/ (6 bot content files)

**Root marketing docs:** 47 files including:
- Campaign plans, status, calendars
- Content libraries
- Lore fragments (2 versions!)
- Viral strategies
- Discord design
- Social media content

**Action Needed:**
- Massive consolidation opportunity
- Create clear hierarchy
- Merge campaign plans
- Consolidate lore fragments
- Archive completed campaigns
- Clarify what's active vs planning

---

### 6. CYOA STORY BLOAT: 103 Files

**Problem:** stories/cyoa-spy has exploded

Breakdown:
- 52 chapter files
- 43 ending files
- 6 dossier files
- 6 training files
- 5 metadata/spec files

**Status:** Appears complete but may be better as separate repo or archived

**Action Needed:**
- Determine if active or complete
- Consider moving to separate repository
- If keeping, ensure it doesn't obscure core Limn docs

---

### 7. EXPERIMENT SPRAWL: 34 Experiments + 10 Results

**Problem:** No clear index or organization

**Types:**
- Numbered experiments (001-005)
- Named experiments (limn-*.md)
- Test suites
- Results logs

**Action Needed:**
- Create experiment index
- Categorize by status (draft, active, complete, validated)
- Link to findings in theory/spec docs
- Identify experiments that support spec claims

---

### 8. ROOT LEVEL CLUTTER: 8 Governance Docs

**Files:**
- README.md (project main)
- PUBLIC-README.md (different?)
- AGENTS.md
- ARCHIVIST-PLAN.md
- CONTRIBUTING.md
- WORK-PLAN.md
- journal.md
- moltbook-*.md (4 files)

**Action Needed:**
- Clarify README vs PUBLIC-README
- Move moltbook files to appropriate directory
- Consolidate WORK-PLAN with project management
- Determine if AGENTS.md overlaps with crew docs

---

### 9. MISSING INDEX/NAVIGATION

**Problem:** No top-level guide to documentation structure

**Needed:**
- Documentation map showing hierarchy
- "Start here" guide for different audiences
- Cross-reference guide
- Version/status indicators on all docs

---

### 10. PROLOG vs PYTHON DOCUMENTATION

**Problem:** Word-derivation-algorithm.md references Python implementation

**Files Mentioning Python:**
- docs/spec/word-derivation-algorithm.md (Section 5: Implementation: vocab-derive.py)
- Possibly others

**Action Needed:**
- Remove Python references
- Document Prolog-only policy
- Update all specs to reflect engineering standards

---

## Proposed Consolidation Beads

### Priority 0 (Critical)

1. **limn-consolidate-bootstrap** - Declare v3-natural canonical, archive others
2. **limn-consolidate-vocabulary** - Single vocabulary source aligned with Dolt database
3. **limn-prolog-only-docs** - Remove Python references, document Prolog policy

### Priority 1 (High Impact)

4. **limn-consolidate-grammar** - Unified grammar specification
5. **limn-theory-hierarchy** - Organize theory docs into core vs exploratory
6. **limn-experiment-index** - Create experiment catalog and status
7. **limn-marketing-consolidate** - Reduce 63 marketing files to organized structure

### Priority 2 (Organization)

8. **limn-cyoa-decision** - Archive, separate repo, or keep?
9. **limn-root-cleanup** - Organize root-level files
10. **limn-navigation-guide** - Create documentation map

---

## Next Steps

1. **Create specific beads** for each consolidation task
2. **Assign to appropriate crew:**
   - Bootstrap → Linguist review
   - Vocabulary → Archivist (me) with Linguist approval
   - Grammar → Linguist
   - Theory → Linguist
   - Experiments → Student (me)
   - Marketing → Author or dedicated marketing role
   - CYOA → Author
   - Prolog policy → Engineer approval, all crew notification

3. **Get feedback** on each bead before executing
4. **Execute consolidation** in priority order
5. **Create documentation standards** to prevent future sprawl

---

## Estimated Work

- **Documentation to read thoroughly:** 236 files
- **Files to consolidate/merge:** ~100
- **Files to archive:** ~50
- **New index/guide docs to create:** ~5
- **Cross-references to update:** Unknown, likely 100+

**Time estimate:** This is a multi-week epic requiring coordination across all crew members.

---

*Initial inventory complete. Ready to create specific beads.*
