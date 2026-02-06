# Limn Documentation Coherence Index

**Last Updated:** 2026-02-05
**Purpose:** Canonical reference for current Limn specification

---

## Canonical Documents

### Core Specification

| Document | Location | Status |
|----------|----------|--------|
| **Vocabulary (DB)** | `data/vocabulary/` (DoltHub-synced) | CURRENT (2005+ words, 26+ domains) |
| **Vocabulary (v3-natural)** | `docs/spec/vocabulary-v3-natural.md` | REFERENCE (~340 core + domains) |
| **Formal Grammar** | `docs/spec/grammar-formal.md` | CURRENT (EBNF) |
| **Bootstrap Guide (v4)** | `docs/spec/bootstrap-v4-compositional.md` | CURRENT (6 compositional operators) |
| **Bootstrap Guide (v3)** | `docs/spec/bootstrap-v3-natural.md` | REFERENCE (superseded by v4) |

### Theory Documents

| Document | Location | Content |
|----------|----------|---------|
| LLM-Native Design Principles | `docs/theory/llm-native-design-principles.md` | 10 core design principles |
| Superposition Semantics | `docs/theory/superposition-semantics.md` | Foundational semantic theory |
| Key Mechanism | `docs/theory/key-mechanism.md` | Key types, communication formula |
| Limn Pragmatics | `docs/theory/limn-pragmatics.md` | Gricean maxims, implicature, speech acts (NEW) |
| Quantifier Semantics | `docs/theory/quantifier-semantics.md` | Proportion operators, scope, generalized quantifiers (NEW) |
| Three-Valued Logic | `docs/theory/three-valued-logic.md` | Liminality as third truth value, boundary semantics (NEW) |
| Liminal Semantics | `docs/theory/liminal-semantics.md` | Contradiction resolution |
| States Not Stories | `docs/theory/states-not-stories.md` | Encoding static vs dynamic |
| Semantic Programming Unification | `docs/theory/semantic-programming-unification.md` | Natural language ↔ programming |
| Constraint Surfaces | `docs/theory/constraint-surfaces.md` | Geometric interpretation |
| Ambiguity Metrics | `docs/theory/ambiguity-metrics.md` | Measuring superposition |
| **Typological Analysis** | `docs/theory/typological-analysis.md` | Linguistic comparison (NEW) |
| **Word Collision Analysis** | `docs/theory/word-collision-analysis.md` | Vocabulary collision resolution (NEW) |
| **Semantic Questions Analysis** | `docs/theory/semantic-questions-analysis.md` | Operator/constraint semantics (NEW) |
| **Poetic Structures Analysis** | `docs/theory/poetic-structures-analysis.md` | Poetry and aesthetic patterns (NEW) |

### Guides

| Document | Location | Audience |
|----------|----------|----------|
| First Day Tutorial | `docs/tutorials/first-day.md` | Beginners (30 min) |
| **Beginners Cheatsheet** | `docs/tutorials/cheatsheet.md` | Quick reference (10 min) (NEW) |
| **Vocabulary Traps** | `docs/guides/vocabulary-traps.md` | Common word pitfalls (NEW) |
| Vocabulary Management | `docs/guides/VOCAB-MANAGEMENT.md` | vocab.sh usage |
| Vocabulary Versions | `docs/guides/vocabulary-versions.md` | v1↔v2 migration |
| Limn Poetics | `docs/guides/limn-poetics.md` | Poetry/creative writing |
| Interference Operators | `docs/guides/interference-operator-guide.md` | v4 operators |
| Getting Started | `docs/getting-started.md` | Quick start |

### Limn-PL (Programming)

| Document | Location | Content |
|----------|----------|---------|
| Limn-PL Grammar | `docs/spec/limn-pl-grammar.md` | Programming language syntax |
| Limn-PL Implementation | `docs/spec/limn-pl-implementation.md` | Interpreter design |
| Limn-PL HVM Design | `docs/spec/limn-pl-hvm-design.md` | Higher-order VM |
| Metacircular Interpreter | `docs/spec/metacircular.md` | Self-describing interpreter |

### Domain Extension Modules

| Document | Location | Content |
|----------|----------|---------|
| Science Module | `docs/spec/domain-modules/science.md` | 30 words for scientific discourse |
| **Engineering Module** | `docs/spec/domain-modules/engineering.md` | 30 words for systems/components (NEW) |
| **Medicine Module** | `docs/spec/domain-modules/medicine.md` | 40 words for clinical/anatomy (NEW) |

---

## Archived Documents

Documents moved to `docs/archive/` are historical and should NOT be used for current reference:

### Archived Specs (in `docs/archive/spec/`)
- `bootstrap-v1.md` - Superseded by bootstrap-v2.md
- `grammar-v1.md` - Superseded by grammar-formal.md
- `vocabulary-v1.md` - Superseded by vocabulary-v2.md
- `vocabulary-relationship.md` - Merged into vocabulary-versions.md
- `v0-sketch.md` - Initial sketch, historical only
- `programming-v0.md` - Superseded by Limn-PL
- `programming-v1.md` - Superseded by Limn-PL
- `limn-pl-v1.md` - Superseded by limn-pl-implementation.md
- `metacircular-limn.md` - Natural language version, historical

### Archived Analysis (in `docs/archive/crew/`)
- `ruling-repetition.md` - Duplicate of repetition-ruling.md
- `liminal-semantics.md` - Duplicate of docs/theory version
- `superposition-semantics.md` - Duplicate of docs/theory version

---

## Naming Conventions

- Language name: **Limn** (not Linga)
- Vocabulary: **v2** is current (v1 is educational/historical)
- Grammar: **grammar-formal.md** is canonical
- Programming: **Limn-PL** (not "Linga programming")

---

## Key Terminology

| Term | Definition |
|------|------------|
| Constraint region | Semantic space activated by a word |
| Superposition | Multiple meanings held simultaneously |
| Entanglement | Correlated meanings between word pairs |
| Key | Context that collapses superposition |
| Scope delimiter | `\|` creates topic-comment structure |
| Liminal expression | Boundary between constraint regions |

---

## Coherence Rules

1. **Vocabulary:** Use DoltHub-synced DB (2005+ words); validate via `vocab.sh check`
2. **Collision Fixes:** Apply v3.1 collision fixes (bre=brief, aud=hearing, bli=believe, veg=plant, avi=bird, prt=partial)
3. **Operators:** Use corrected tone operators (fml, snc, not frm, sin)
4. **Grammar:** Formal grammar is definitive for parsing
5. **Theory:** docs/theory/ versions are canonical
6. **Naming:** Always "Limn" in new documents

---

## Update Log

- **2026-02-05 (Archivist):** Updated vocabulary source to DoltHub DB (2005+ words). Added bootstrap v4 as CURRENT (v3 → REFERENCE). Added new docs: cheatsheet.md, vocabulary-traps.md, VOCAB-MANAGEMENT.md, interference-operator-guide.md. Added experiments INDEX Phase 3 (compression, falsifiable claims). Vocabulary stats: 2005+ words, 133 expressions, 26+ domains.
- **Session 7c (2026-01-31):** Major theory formalization session. Completed v3-natural vocabulary coherence sweep (repetition-ruling.md, natural-language-gaps.md, semantic-resonance.md, untranslatable-content-analysis.md, unicode-density-exploration.md). Updated vocabulary-versions.md with v3-natural column. Added "Intentional Polysemy" section to vocabulary-v3-natural.md. Created crew infrastructure: crew/linguist/CLAUDE.md (startup checklist), crew/social-bot/limn_bootstrap.md, crew/author/limn_bootstrap.md. Created three major theory documents: limn-pragmatics.md (Gricean maxims, implicature, speech acts, key as pragmatic device), quantifier-semantics.md (proportion operators, scope, generalized quantifiers, distributivity), three-valued-logic.md (liminality as third truth value, truth tables, boundary semantics).
- **Session 7b (2026-01-30):** Continuation - v3-natural vocabulary coherence sweep. Updated linguist analysis files: worked-examples.md, minimal-pairs.md, semantic-entanglement.md, key-semantics.md, liminal-mathematics.md, operator-grammar-rulings.md, question-marker-semantics.md, semantic-resonance.md, scope-vs-intersection-ruling.md, formal-analysis.md, unicode-density-exploration.md, borrowings.md. Updated student reference (CLAUDE.md quick reference to v3). Updated experiments/test-cases.md test specifications to v3 vocabulary. Fixed `bri`→`lux`, `her the`→`yo an`, and other v1→v3 vocabulary throughout analysis corpus. Note: experiments/results/ files retain v1 vocabulary as historical test records.
- **Session 7 (2026-01-30):** Major linguist analysis session. Fixed 6 critical word collisions (bre=brief, aud=hearing, bli=believe, veg=plant, avi=bird, prt=partial). Added metalinguistic vocabulary (emp, cla, amb, det, prt, lim). Added Domain 11 Agent/AI vocabulary (age, mod, wei, ctx, inf, tok, prm, emb, lat, att). Created typological-analysis.md, word-collision-analysis.md, semantic-questions-analysis.md, poetic-structures-analysis.md, cyclic-pattern-analysis.md. Created engineering, medicine, and transactions domain modules. Updated limn-poetics.md, seven-poems.md, first-day.md, liminal-semantics.md, semantic-programming-unification.md, grammar-formal.md, launch-teasers.md, WEEK-1-CALENDAR.md with v3 vocabulary. Created social-ready-phrases.md for marketing (including Tier 5 Moltbook phrases). Created experiments/cyclic-patterns-test.limn. Added operator scope rules and sequence operator (`→`) to bootstrap-v3-natural.md and grammar-formal.md. Responded to Moltbook field feedback including DexterAI's causality critique (crew/linguist/state-transition-analysis.md).
- **Session 6b:** Maintenance iteration - fixed `run_linga_pl`→`run_limn_pl` in limn-pl-examples.md and limn-pl-implementation.md (4 refs each)
- **Session 6:** Coherence maintenance - fixed grammar-v1→grammar-formal refs (PUBLIC-README.md, negation-scope-test.md), renamed linga_algorithm→limn_algorithm, updated linga_pl_interpreter→limn_pl_interpreter refs across 3 files, updated linga_interpreter→limn_interpreter ref; RESOLVED all 5 Linguist Questions (Q1: recursion, Q2: quantifier scope, Q3: negation semantics, Q4: type coercion, Q5: collection constraints)
- **Session 5:** Archived 12 outdated files, fixed Linga→Limn references
- **Session 4:** Major vocabulary collision fixes (52 bugs fixed)
- **Session 3:** Rebrand from Linga to Limn

---

*This index is the authoritative reference for document coherence.*
