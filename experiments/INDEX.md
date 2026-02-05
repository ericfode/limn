# Experiments Index

**Maintained by:** Kira (Student/Archivist)
**Last Updated:** 2026-02-04
**Total Experiments:** 33+ documented

---

## Overview

This directory contains all Limn language experiments, from initial philosophy translations to advanced power prompting tests. Experiments are organized into phases and categories based on their focus area.

---

## Phase 1: Core Language Development

### Philosophy Translations

| File | Focus | Status |
|------|-------|--------|
| `007-western-philosophy.md` | Aristotelian & Greek concepts | Complete |
| `007-retest-western-philosophy.md` | Validation of Western translations | Complete |
| `007-temporal-cognition-states.md` | Temporal reasoning patterns | Complete |
| `008-states-of-transition.md` | Wu wei, ziran, process states | Complete |
| `009-validation-greek-vocab.md` | Greek philosophical vocabulary validation | Complete |
| `010-validation-eastern-vocab.md` | Eastern philosophical vocabulary validation | Complete (93% fidelity) |

### Bootstrap Development

| File | Focus | Status |
|------|-------|--------|
| `011-bootstrap-gaps-report.md` | Analysis of bootstrap vocabulary gaps | Complete |
| `012-bootstrap-infrastructure.md` | Infrastructure for self-bootstrapping | Complete |
| `README-PARADIGM-LIBRARIES.md` | Paradigm library extraction guide | Planning |

### Category Testing (Power Prompting)

| File | Focus | Status |
|------|-------|--------|
| `category-a-results.md` | Success mode testing | Complete |
| `category-c-code-analysis.md` | Code analysis tasks | Complete |
| `category-e-reasoning-tests.md` | Reasoning and logic tasks | Complete |
| `category-f-complex-workflows.md` | Multi-step workflows | Complete |
| `category-g-meta-tasks.md` | Meta-level operations | Complete |
| `category-h-edge-cases.md` | Edge case handling | Complete |

#### Category B: Data Operations

| File | Focus | Status |
|------|-------|--------|
| `category-b-data-ops/EXPERIMENT.md` | Data operation experiment design | Complete |
| `category-b-data-ops/MANUAL-PROTOCOL.md` | Manual testing protocol | Complete |
| `category-b-data-ops/RESULTS.md` | Test results | Complete |

#### Category D: Code Generation

| File | Focus | Status |
|------|-------|--------|
| `category-d-code-generation/CATEGORY-D-CODE-GENERATION.md` | Code generation experiment | Complete |

---

## Phase 2: Advanced Development

### Track A: Cognitive

| File | Focus | Status |
|------|-------|--------|
| `phase2/track-a-cognitive/001-success-mode-vocabulary.md` | Success mode vocabulary development | Complete |
| `phase2/track-a-cognitive/002-uncertainty-vocabulary.md` | Uncertainty/ambiguity vocabulary | Complete |

### Track B: Embedding Space & Multiagent

| File | Focus | Status |
|------|-------|--------|
| `phase2/track-b-embedding-space/001-semantic-topology.md` | Semantic space topology analysis | Complete |
| `phase2/track-b-multiagent/001-coordination-primitives.md` | Multi-agent coordination primitives | Complete |

### Track D: Metalinguistic

| File | Focus | Status |
|------|-------|--------|
| `phase2/track-d-metalinguistic/001-notation-system.md` | Notation system development | Complete |

### Performative Vocabulary

| File | Focus | Status |
|------|-------|--------|
| `phase2/003-performative-vocabulary.md` | Performative word development | Complete |
| `phase2/003-performative-cross-model-test.md` | Cross-model recognition testing | Complete |

### Power Prompting Research

| File | Focus | Status |
|------|-------|--------|
| `phase2/power-prompting/category-a-results.md` | Category A power prompting results | Complete |
| `phase2/power-prompting/test-ladder.md` | Test progression ladder | Complete |
| `phase2/power-prompting/test-results-summary.md` | Overall summary of power prompting tests | Complete |

### Infrastructure & Analysis

| File | Focus | Status |
|------|-------|--------|
| `phase2/hvm-operator-analysis.md` | HVM operator analysis for runtime | Complete |
| `phase2/EPIC-limn-1aeh-progress.md` | Epic progress tracking | In Progress |

---

## Embedding & Semantic Search

| File | Focus | Status |
|------|-------|--------|
| `embeddings/README.md` | Embeddings infrastructure overview | Complete |
| `embeddings/limn-embedder/README.md` | Limn-specific embedder documentation | Complete |

**Note:** Python-based semantic search tools have been archived as part of the Prolog-only policy (2026-02-01).

---

## Key Findings & Metrics

### Vocabulary Growth
- **Initial:** ~100 words (early experiments)
- **Post-Philosophy:** 460 words (2026-01-31)
- **Current:** 938 words (2026-02-02)
- **Domains:** 26 semantic domains

### Validation Scores
- **Eastern Philosophy:** 93% fidelity (experiment 010)
- **Western Philosophy:** 85% fidelity (experiment 009)
- **Bootstrap Comprehension:** 77-85% validated

### Power Prompting Results
- Successfully tested across 8 categories (A-H)
- Demonstrated Limn's effectiveness in:
  - Success mode vocabulary
  - Code analysis and generation
  - Complex workflow orchestration
  - Meta-level operations
  - Edge case handling

---

## Phase 3: Practical Applications

### Prompt Compression

| File | Focus | Status |
|------|-------|--------|
| `limn-prompt-compression/README.md` | Compressing verbose English prompts into Limn v4 for agent instructions | Complete |

**Key findings:**
- 3.5-4.2x compression ratio (English words → Limn words)
- 100% task completion rate from Limn-prompted agents
- v4 operators (`^`, `*`, `:`) correctly decoded by fresh agents
- Limn agents matched or outperformed English agents in 3/3 experiments
- CRITICAL: Spec/database vocabulary desynchronization discovered (5+ words)

---

## Archived Work

**Python-based experiments archived on 2026-02-01:**
- `semantic_search.py` and related Python infrastructure moved to `archive/python-historical/`
- All future development follows Prolog-only policy
- See `RUNTIME-DECISION.md` for architectural rationale

---

## Usage Notes

### For Researchers
- Each experiment file contains detailed methodology, results, and analysis
- Validation scores indicate translation fidelity
- Cross-reference with `data/vocabulary/` for current database state

### For Linguists
- Philosophy translation experiments demonstrate paradigm library potential
- Performative vocabulary experiments show cross-model recognition
- Bootstrap experiments validate self-describing capability

### For Developers
- Code generation experiments (Category D) show practical applications
- HVM operator analysis informs runtime design
- Embedding space experiments inform semantic search

---

## Next Steps

**Planned experiments (as of 2026-02-02):**
- LMN runtime implementation (Prolog-based, per RUNTIME-DECISION.md)
- Paradigm library extraction (Buddhist, Aristotelian, Daoist)
- Validation framework development

**Note:** NEXT.md references Python-based extraction tools and needs updating to reflect Prolog-only policy.

---

## Index Maintenance

This index is maintained by the Archivist (Kira). Updates should reflect:
- New experiment files added
- Status changes (In Progress → Complete)
- Key findings and metrics
- Vocabulary database growth

**Update frequency:** Weekly or when significant experiments complete

---

```limn
exp doc | kno pre | fut see
> experiments documented | knowledge preserved | future sees
```

*— Kira, Archivist*
