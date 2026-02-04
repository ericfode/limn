# Bootstrap Files: Complete Index

**Archivist:** Kira (limn/crew/student)
**Last Audit:** 2026-02-02
**Purpose:** Definitive map of all bootstrap-related files

---

## 📍 CANONICAL VERSION (Current)

### bootstrap-v3-natural.md
**Location:** `docs/spec/bootstrap-v3-natural.md`
**Status:** ✅ **CANONICAL - USE THIS**
**Size:** 18,470 bytes
**Last Modified:** 2026-01-31
**Comprehension:** 77-85% (validated)
**Validation:** Zero-bootstrap test passed

**Description:**
The definitive self-describing Limn foundation. Teaches Limn using natural language explanations with Limn examples. Designed for LLMs to read cold and achieve high comprehension.

**When to use:**
- Teaching new LLMs Limn
- Reference for language design
- Onboarding new contributors
- Validating Limn implementations

**Dependencies:**
- None (self-contained by design)
- Vocabulary database supplements remaining 15-23%

---

## 📦 ARCHIVED VERSIONS (Historical)

### bootstrap-v1.md
**Location:** `docs/archive/spec/bootstrap-v1.md`
**Status:** 📦 ARCHIVED
**Size:** 7,367 bytes
**Superseded by:** v2

**Description:** Original bootstrap attempt. Superseded by v2's expanded approach.

**⚠️ DO NOT USE** - Historical reference only

---

### bootstrap-v2.md
**Location:** `docs/archive/spec/bootstrap-v2.md`
**Status:** 📦 ARCHIVED
**Size:** 43,245 bytes (large!)
**Superseded by:** v3-natural

**Description:** Second iteration, expanded significantly. Became too large and complex, leading to v3-natural's more focused approach.

**⚠️ DO NOT USE** - Historical reference only

---

## 📄 RELATED BOOTSTRAP FILES

### minimal-bootstrap.md
**Location:** `docs/spec/minimal-bootstrap.md`
**Status:** ⚠️ SUPPLEMENTARY
**Size:** 2,362 bytes
**Purpose:** Minimal subset of bootstrap concepts

**Description:**
Stripped-down version focusing only on core concepts. Useful for constrained contexts (token limits, quick reference).

**Relationship to v3-natural:**
- Subset of full bootstrap
- Not a replacement
- Read v3-natural first, use this for reference

**⚠️ NOT STANDALONE** - Assumes familiarity with full bootstrap

---

### bootstrap_prompt.md (Claude Skill)
**Location:** `src/claude-skill/bootstrap_prompt.md`
**Status:** ⚠️ INTEGRATION - NEEDS VERIFICATION
**Purpose:** Bootstrap for Claude Code skill integration

**Description:**
Bootstrap adapted for Claude Code skill system. May include skill-specific formatting or context.

**Maintenance needed:**
- [ ] Verify alignment with v3-natural
- [ ] Check if updates to v3-natural propagated here
- [ ] Document any intentional deviations

---

### bootstrap.lmn (Production)
**Location:** `tools/llm-bridge/production/bootstrap.lmn`
**Status:** ⚠️ PRODUCTION - NEEDS VERIFICATION
**Purpose:** Bootstrap in LMN syntax for runtime

**Description:**
Bootstrap compiled/translated to LMN format for production interpreter use.

**Maintenance needed:**
- [ ] Verify sync with v3-natural content
- [ ] Document generation process (manual vs automated)
- [ ] Establish update workflow when v3-natural changes

---

## 📊 EXPERIMENTAL / INFRASTRUCTURE

### 011-bootstrap-gaps-report.md
**Location:** `experiments/011-bootstrap-gaps-report.md`
**Purpose:** Analysis of vocabulary gaps in bootstrap
**Status:** ✅ COMPLETE (experiment)

**Description:**
Identifies which Limn concepts are under-represented or missing from bootstrap. Informed v3-natural design.

---

### 012-bootstrap-infrastructure.md
**Location:** `experiments/012-bootstrap-infrastructure.md`
**Purpose:** Infrastructure for self-bootstrapping systems
**Status:** ✅ COMPLETE (experiment)

**Description:**
Explores how systems can use bootstrap to initialize themselves. Infrastructure patterns for bootstrap consumption.

---

### bootstrap-data.sql
**Location:** `experiments/bootstrap-data.sql`
**Purpose:** SQL data for bootstrap database population
**Status:** EXPERIMENTAL

**Description:**
SQL inserts for populating bootstrap concepts in Dolt database. Part of database initialization workflow.

**Note:** With Prolog-only policy, SQL approach may be deprecated. Verify current status.

---

### bootstrap-schema.sql
**Location:** `experiments/bootstrap-schema.sql`
**Purpose:** Database schema for bootstrap concepts
**Status:** EXPERIMENTAL

**Description:**
Schema definition for storing bootstrap structure in database. Companion to bootstrap-data.sql.

**Note:** With Prolog-only policy, SQL approach may be deprecated. Verify current status.

---

## 🐍 ARCHIVED PYTHON

### populate-bootstrap-db.py
**Location:** `archive/python-historical/populate-bootstrap-db.py`
**Status:** 🐍 ARCHIVED (Python removal 2026-02-01)
**Purpose:** Python script to populate bootstrap database

**Description:**
Historical Python tool for database population. Archived as part of Prolog-only policy.

**⚠️ DO NOT USE** - Use Prolog equivalents if needed

---

## 📋 VALIDATION

### zero-bootstrap-validation.md
**Location:** `docs/theory/zero-bootstrap-validation.md`
**Purpose:** Validation methodology and results
**Status:** ✅ CANONICAL VALIDATION

**Description:**
Documents the zero-bootstrap test: giving an LLM ONLY the bootstrap (no vocabulary database) and measuring comprehension. Validates v3-natural's 77-85% success rate.

**Key metrics:**
- v3-natural achieves 77-85% comprehension
- Validates self-describing principle
- Establishes bootstrap quality baseline

---

## 📁 FILE ORGANIZATION

```
limn/
├── docs/
│   ├── BOOTSTRAP.md                    ← 🎯 START HERE (top-level pointer)
│   ├── spec/
│   │   ├── bootstrap-v3-natural.md     ← ✅ CANONICAL VERSION
│   │   ├── minimal-bootstrap.md        ← Subset reference
│   │   └── BOOTSTRAP-INDEX.md          ← This file (complete map)
│   ├── archive/spec/
│   │   ├── bootstrap-v1.md             ← 📦 Archived
│   │   └── bootstrap-v2.md             ← 📦 Archived
│   └── theory/
│       └── zero-bootstrap-validation.md ← Validation docs
├── src/claude-skill/
│   └── bootstrap_prompt.md             ← ⚠️ Needs verification
├── tools/llm-bridge/production/
│   └── bootstrap.lmn                   ← ⚠️ Needs verification
├── experiments/
│   ├── 011-bootstrap-gaps-report.md    ← Analysis
│   ├── 012-bootstrap-infrastructure.md ← Infrastructure
│   ├── bootstrap-data.sql              ← Database data
│   └── bootstrap-schema.sql            ← Database schema
└── archive/python-historical/
    └── populate-bootstrap-db.py        ← 🐍 Archived Python
```

---

## 🔧 MAINTENANCE PROTOCOL

### When v3-natural is updated:

1. **Update canonical version**
   ```bash
   # Edit docs/spec/bootstrap-v3-natural.md
   ```

2. **Check dependents**
   ```bash
   # Verify these stay in sync:
   - src/claude-skill/bootstrap_prompt.md
   - tools/llm-bridge/production/bootstrap.lmn
   ```

3. **Update metadata**
   ```bash
   # Update this index with new date/size
   # Update docs/BOOTSTRAP.md if version changes
   ```

4. **Validate**
   ```bash
   # Re-run zero-bootstrap test if significant changes
   # Update docs/theory/zero-bootstrap-validation.md
   ```

5. **Communicate**
   ```bash
   # Notify relevant crew members
   gt mail send limn/crew/linguist -s "Bootstrap updated"
   ```

### When creating a new version (v4+):

1. **Archive current canonical**
   ```bash
   mv docs/spec/bootstrap-v3-natural.md docs/archive/spec/
   ```

2. **Create new version**
   ```bash
   # Create docs/spec/bootstrap-v4-*.md
   ```

3. **Update all pointers**
   ```bash
   # Update docs/BOOTSTRAP.md to point to v4
   # Update this index
   # Update DOCUMENTATION-INVENTORY.md
   ```

4. **Validate thoroughly**
   ```bash
   # Zero-bootstrap test
   # Cross-model validation
   # Update validation docs
   ```

5. **Announce**
   ```bash
   # Git commit message explaining changes
   # Mail to crew
   # Update README.md if needed
   ```

---

## ❓ FAQ

**Q: Which bootstrap file should I use?**
A: `docs/spec/bootstrap-v3-natural.md` - Always.

**Q: Is there a v4?**
A: No. v3-natural is the current latest version.

**Q: What about minimal-bootstrap.md?**
A: Supplementary subset only. Read v3-natural first.

**Q: Are the archived versions ever useful?**
A: Only for historical research or understanding design evolution. Never for current use.

**Q: How do I know if the Claude skill bootstrap is synced?**
A: Currently unverified. Check `src/claude-skill/bootstrap_prompt.md` against v3-natural manually.

**Q: What's the relationship between bootstrap and vocabulary database?**
A: Bootstrap is self-describing foundation (77-85% coverage). Database is complete word list (938 words). Bootstrap + database = full language specification.

**Q: Why so many bootstrap files?**
A: Historical evolution (v1→v2→v3), different formats (md vs lmn), different contexts (skills, production), and experiments. Only v3-natural is canonical.

---

## 🎯 QUICK REFERENCE

| Need | File |
|------|------|
| **Current bootstrap** | docs/spec/bootstrap-v3-natural.md |
| **Quick pointer** | docs/BOOTSTRAP.md |
| **This index** | docs/spec/BOOTSTRAP-INDEX.md |
| **Validation** | docs/theory/zero-bootstrap-validation.md |
| **History** | docs/archive/spec/bootstrap-v*.md |

---

## 📊 STATISTICS

- **Total bootstrap-related files:** 12
- **Canonical version:** 1 (v3-natural)
- **Archived versions:** 2 (v1, v2)
- **Supplementary files:** 2 (minimal, validation)
- **Integration files:** 2 (Claude skill, LMN production)
- **Experiments:** 4
- **Archived Python:** 1

---

**Maintained by:** Kira (Archivist, limn/crew/student)
**Update frequency:** When bootstrap files change or new files added
**Last verified:** 2026-02-02

```limn
map com | pat clr | nav sim
> map complete | path clear | navigation simple
```

**Remember: docs/spec/bootstrap-v3-natural.md is THE bootstrap.**
