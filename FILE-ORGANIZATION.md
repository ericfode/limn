# Limn Repository: File Organization

**Archivist:** Kira (limn/crew/student)
**Last Updated:** 2026-02-02
**Purpose:** Master index of repository structure and critical files

**Lost? Start here.**

---

## 🎯 Critical Files (Start Here)

| File | Purpose |
|------|---------|
| **[README.md](README.md)** | Project overview, quick start |
| **[docs/BOOTSTRAP.md](docs/BOOTSTRAP.md)** | **Bootstrap navigation** (← if you need the bootstrap) |
| **[docs/spec/bootstrap-v3-natural.md](docs/spec/bootstrap-v3-natural.md)** | **THE** bootstrap (canonical) |
| **[docs/guides/VOCAB-MANAGEMENT.md](docs/guides/VOCAB-MANAGEMENT.md)** | Vocabulary database usage |
| **[experiments/INDEX.md](experiments/INDEX.md)** | All experiments cataloged |
| **[docs/spec/BOOTSTRAP-INDEX.md](docs/spec/BOOTSTRAP-INDEX.md)** | Complete bootstrap file map |
| **[DOCUMENTATION-INVENTORY.md](DOCUMENTATION-INVENTORY.md)** | Full documentation audit |

---

## 📁 Directory Structure

```
limn/
├── 📄 Root Documentation
│   ├── README.md                           ← Start here
│   ├── CONTRIBUTING.md                     ← How to contribute
│   ├── ROADMAP.md                          ← Project roadmap
│   ├── FILE-ORGANIZATION.md                ← This file (master index)
│   ├── DOCUMENTATION-INVENTORY.md          ← Full doc audit (236 files)
│   ├── journal.md                          ← Kira's learning journal
│   └── Various planning docs               ← ARCHIVIST-PLAN, MASTER-PLAN, etc.
│
├── 📚 docs/                                ← All formal documentation
│   ├── BOOTSTRAP.md                        ← **Bootstrap navigation (START HERE for bootstrap)**
│   ├── BOOTSTRAP-QUICK-REFERENCE.md        ← Bootstrap quick answers
│   │
│   ├── spec/                               ← Language specifications
│   │   ├── bootstrap-v3-natural.md         ← **CANONICAL BOOTSTRAP**
│   │   ├── BOOTSTRAP-INDEX.md              ← Complete bootstrap file map
│   │   ├── minimal-bootstrap.md            ← Bootstrap subset
│   │   ├── vocabulary-v3-natural.md        ← Vocabulary reference (938 words)
│   │   ├── grammar-formal.md               ← Grammar specification
│   │   └── ...                             ← Other specs
│   │
│   ├── guides/                             ← User guides
│   │   └── VOCAB-MANAGEMENT.md             ← **How to use vocabulary database**
│   │
│   ├── theory/                             ← Theoretical foundations
│   │   ├── zero-bootstrap-validation.md    ← Bootstrap validation results
│   │   └── ...                             ← Other theoretical docs
│   │
│   ├── marketing/                          ← Launch materials
│   │   ├── WIKIPEDIA-ARTICLE-DRAFT.md      ← Wikipedia article (ready)
│   │   ├── REDDIT-LAUNCH-POST-DRAFT.md     ← Reddit post (ready)
│   │   ├── GITHUB-PUBLIC-REPO-SETUP.md     ← GitHub setup guide
│   │   └── ...                             ← Other marketing content
│   │
│   ├── archive/                            ← Archived old versions
│   │   └── spec/
│   │       ├── bootstrap-v1.md             ← OLD (do not use)
│   │       ├── bootstrap-v2.md             ← OLD (do not use)
│   │       └── ...                         ← Other archived specs
│   │
│   └── tutorials/                          ← Learning materials
│
├── 🧪 experiments/                         ← All experiments
│   ├── INDEX.md                            ← **Experiments catalog (START HERE)**
│   ├── phase2/                             ← Phase 2 experiments
│   │   ├── track-a-cognitive/
│   │   ├── track-b-embedding-space/
│   │   ├── track-b-multiagent/
│   │   └── power-prompting/
│   ├── embeddings/                         ← Embedding experiments
│   ├── category-*/                         ← Category tests (A-H)
│   └── 0*.md                               ← Numbered experiments
│
├── 📊 data/                                ← Data files
│   └── vocabulary/                         ← **Dolt vocabulary database (938 words)**
│
├── 🔧 tools/                               ← Tooling
│   ├── llm-bridge/production/
│   │   └── bootstrap.lmn                   ← Production bootstrap (LMN format)
│   └── ...                                 ← Other tools
│
├── 💻 src/                                 ← Source code
│   ├── claude-skill/
│   │   └── bootstrap_prompt.md             ← Bootstrap for Claude integration
│   └── ...                                 ← Other source
│
├── 🗂️ archive/                             ← Archived materials
│   ├── python-historical/                  ← **Archived Python code (Prolog-only now)**
│   │   ├── README.md                       ← Why Python was archived
│   │   └── *.py                            ← Old Python tools (deprecated)
│   └── NEXT-archived-2026-02-02.md         ← Old plan (outdated)
│
├── 📜 scripts/                             ← Utility scripts
│   └── vocab.sh                            ← **Vocabulary database tool (USE THIS)**
│
└── 📖 Other directories
    ├── lib/                                ← Libraries (in development)
    ├── examples/                           ← Usage examples
    ├── stories/                            ← Creative writing
    └── ...

```

---

## 🔍 Finding Specific Things

### "Where is the bootstrap?"

**Answer:** `docs/spec/bootstrap-v3-natural.md`

**Full navigation:** `docs/BOOTSTRAP.md`

**Complete map:** `docs/spec/BOOTSTRAP-INDEX.md`

---

### "Where is the vocabulary?"

**Database (source of truth):** `data/vocabulary/` (Dolt database, 938 words)

**How to query:** `./scripts/vocab.sh stats|search|check|domain`

**Documentation:** `docs/guides/VOCAB-MANAGEMENT.md`

**Reference:** `docs/spec/vocabulary-v3-natural.md`

---

### "Where are the experiments?"

**Index:** `experiments/INDEX.md` ← Start here

**Location:** `experiments/` (32+ documented experiments)

**Categories:** `experiments/category-*/`

**Phase 2:** `experiments/phase2/`

---

### "Where is X documentation?"

**Full inventory:** `DOCUMENTATION-INVENTORY.md` (lists 236 markdown files)

**Specs:** `docs/spec/`

**Guides:** `docs/guides/`

**Theory:** `docs/theory/`

**Marketing:** `docs/marketing/`

---

### "What's been archived?"

**Python code:** `archive/python-historical/` (Prolog-only policy since 2026-02-01)

**Old specs:** `docs/archive/spec/` (bootstrap v1/v2, old vocabularies)

**Old plans:** `archive/NEXT-archived-2026-02-02.md` (referenced deprecated tools)

---

### "How do I use the vocabulary database?"

**Tool:** `./scripts/vocab.sh`

**Examples:**
```bash
./scripts/vocab.sh stats           # Database statistics
./scripts/vocab.sh search <word>   # Search for word
./scripts/vocab.sh check <word>    # Check if word exists
./scripts/vocab.sh domain <id>     # List domain words
```

**Full guide:** `docs/guides/VOCAB-MANAGEMENT.md`

---

### "Where are the Git Town / Gas Town docs?"

**Git Town is the parent workspace.** This repo (limn) is a crew worker in Gas Town.

**For Gas Town docs:** See `~/gt/docs/` (outside this repo)

**Your role context:** Injected by `gt prime` command

---

## 📋 File Type Breakdown

| Type | Count | Locations |
|------|-------|-----------|
| Markdown docs | 236+ | docs/, experiments/, root |
| SQL files | 2 | experiments/ |
| Prolog files | Multiple | tools/, lib/ |
| Shell scripts | Multiple | scripts/, tools/ |
| Python (archived) | ~10 | archive/python-historical/ |

---

## 🎯 Common Tasks

### Task: Learn Limn from scratch
1. Read `README.md`
2. Read `docs/spec/bootstrap-v3-natural.md`
3. Query vocabulary: `./scripts/vocab.sh search <concept>`
4. Practice with `examples/`

### Task: Add a new word
1. Check availability: `./scripts/vocab.sh check <word>`
2. Search existing: `./scripts/vocab.sh search <concept>`
3. See guide: `docs/guides/VOCAB-MANAGEMENT.md`
4. Propose to linguist if adding

### Task: Understand experiments
1. Read `experiments/INDEX.md`
2. Navigate to relevant experiment
3. Check results and validation data

### Task: Find old versions of bootstrap
1. See `docs/BOOTSTRAP.md` for pointers
2. Check `docs/archive/spec/bootstrap-v*.md`
3. Read warnings (archived, don't use)

### Task: Contribute documentation
1. Read `CONTRIBUTING.md`
2. Check `DOCUMENTATION-INVENTORY.md` for gaps
3. Follow established structure
4. Update relevant indexes

---

## 🚨 Important Notes

### What's Current (Use These)

✅ **Bootstrap:** v3-natural only (`docs/spec/bootstrap-v3-natural.md`)

✅ **Vocabulary:** Dolt database (`data/vocabulary/` - 938 words)

✅ **Runtime:** Prolog-only (see `RUNTIME-DECISION.md`)

✅ **Specs:** docs/spec/*-v3-*.md files

### What's Archived (Don't Use)

❌ **Bootstrap v1/v2:** Superseded, archived

❌ **Python code:** Archived to `archive/python-historical/`

❌ **Old vocabularies:** v1/v2 archived

❌ **NEXT.md:** Archived (referenced deprecated Python tools)

### What Needs Attention

⚠️ **Claude skill bootstrap:** Verify alignment with v3-natural

⚠️ **Production bootstrap.lmn:** Verify sync with v3-natural

⚠️ **Marketing docs:** Updated to 938 words, ready for launch

---

## 📊 Repository Statistics

- **Total markdown files:** 236+
- **Vocabulary words:** 938 (26 domains, 23 operators)
- **Experiments documented:** 32+
- **Bootstrap versions:** 3 (v3 current, v1/v2 archived)
- **Last major update:** 2026-02-02 (Archivist documentation overhaul)

---

## 🔗 External Resources

- **Vocabulary Database (DoltHub):** https://www.dolthub.com/repositories/ericfode/limn
- **Main Repo (GitHub):** https://github.com/ericfode/limn
- **Gas Town (parent):** ~/gt/ (local workspace)

---

## 🆘 Still Lost?

1. **For bootstrap:** Read `docs/BOOTSTRAP.md`
2. **For vocabulary:** Read `docs/guides/VOCAB-MANAGEMENT.md`
3. **For experiments:** Read `experiments/INDEX.md`
4. **For everything:** Read `DOCUMENTATION-INVENTORY.md`
5. **For contributing:** Read `CONTRIBUTING.md`

**Or ask the Archivist:** Kira (limn/crew/student)

---

**Last Updated:** 2026-02-02
**Maintained By:** Kira (Archivist, limn/crew/student)
**Update Frequency:** When major reorganizations occur

```limn
fil map | pat clr | nav eas
> files mapped | paths clear | navigation easy
```

**No more loose files. Everything is indexed.**
