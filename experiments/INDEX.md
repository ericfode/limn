# Limn Experiments Index

**Last Updated:** 2026-01-31
**Total Experiments:** 49 (39 main + 10 results)
**Curator:** Kira (Student/Archivist)

---

## How to Use This Index

**Status Legend:**
- ✅ **VALIDATED** - Experimentally proven, cited in specs
- ✓ **COMPLETE** - Finished, documented, archived
- 🔄 **ACTIVE** - Ongoing work
- 📝 **DRAFT** - Incomplete or preliminary
- ⚠️ **SUPERSEDED** - Replaced by later work

**Categories:**
- Compositional Semantics
- Learnability & Bootstrap
- Translation & Fidelity
- Practical Applications
- Creative Expression
- Theory Testing

---

## ✅ VALIDATED EXPERIMENTS

### Experiment 005: Compositional Semantics

**Files:**
- [005-COMPREHENSIVE-SUMMARY.md](005-COMPREHENSIVE-SUMMARY.md)
- [005-FINAL-REPORT.md](005-FINAL-REPORT.md)
- [005-RESULTS.md](005-RESULTS.md)
- [005-embedding-compositionality.md](005-embedding-compositionality.md)
- [005-expected-results.md](005-expected-results.md)

**Purpose:** Test Limn's compositional properties in LLM embedding space

**Key Finding:** **Limn phrases are 52% more predictable to language models than equivalent English expressions**
- Limn compositionality: 0.88 mean similarity
- English baseline: 0.58 mean similarity
- Statistical significance: p = 0.0059, Cohen's d = 2.06
- Wins: 10/11 direct comparisons

**Status:** ✅ VALIDATED (2026-01-31)
**Theory Support:** Validates compositional transparency, distributional semantics advantage
**Spec Citations:** README.md Empirical Validation section

**Methods:** 6 tests using sentence-transformers embedding model
**Impact:** Publication-quality evidence for LLM-native design

---

### Zero-Bootstrap Validation

**File:** [zero-bootstrap-validation-log.md](zero-bootstrap-validation-log.md)

**Purpose:** Test if Limn is learnable WITHOUT vocabulary lookup, using only bootstrap-v3-natural.md

**Key Finding:** **77-85% comprehension from bootstrap alone** (exceeded 70% target)
- Natural extensions principle VALIDATED
- First-syllable rule enables confident guessing
- 16 vocabulary gaps identified in bootstrap

**Status:** ✅ VALIDATED (2026-01-31, Task: limn-76j5)
**Theory Support:** Validates zero-bootstrap learnability, natural extensions design
**Spec Citations:** Informs bootstrap-v3-natural.md improvements

**Methods:** 13 test sentences across 4 domains, blind interpretation
**Impact:** Proves bootstrap is sufficient for learning

---

## ✓ COMPLETE EXPERIMENTS

### Experiment 001: Untranslatable Words

**File:** [001-untranslatable-words.md](001-untranslatable-words.md)

**Purpose:** Test Limn's ability to express culturally-specific concepts
**Status:** ✓ COMPLETE
**Finding:** [To be read and documented]

---

### Experiment 002: Poetry Round Trip

**File:** [002-poetry-round-trip.md](002-poetry-round-trip.md)

**Purpose:** Test fidelity of English → Limn → English poetry translation
**Status:** ✓ COMPLETE
**Finding:** [To be read and documented]

---

### Experiment 003: Bootstrap Without English

**File:** [003-bootstrap-without-english.md](003-bootstrap-without-english.md)

**Purpose:** Test if non-English speakers can learn Limn
**Status:** ✓ COMPLETE
**Finding:** Requires secondary education, Latin/Greek familiarity

---

### Experiment 004: Proverbs and Idioms

**Files:**
- [004-proverbs-and-idioms.md](004-proverbs-and-idioms.md)
- [004-retest-with-cultural-vocab.md](004-retest-with-cultural-vocab.md)

**Purpose:** Test cultural expression in Limn
**Status:** ✓ COMPLETE (with retest)
**Finding:** [To be read and documented]

---

### Experiment 006: Eastern Philosophy

**Files:**
- [006-eastern-philosophy.md](006-eastern-philosophy.md)
- [006-retest-eastern-philosophy.md](006-retest-eastern-philosophy.md)

**Purpose:** Express Eastern philosophical concepts
**Status:** ✓ COMPLETE (with retest)
**Finding:** [To be read and documented]

---

## 🔄 STUDENT EXPERIMENTS (Active/Recent)

### Multi-State Cycles

**File:** [limn-multi-state-cycles.md](limn-multi-state-cycles.md)

**Purpose:** Extend cyclic patterns beyond X→Y→X to multi-state cycles
**Status:** 🔄 ACTIVE
**Finding:** Natural cycles prefer 4 states, processes prefer 3, philosophy prefers 2
**Examples:** Seasonal cycles, Hero's Journey, creative processes

**Theory Support:** Informs cyclic-pattern-analysis.md

---

### Domain Applications

**File:** [limn-domain-applications.md](limn-domain-applications.md)

**Purpose:** Test Limn expressiveness across 8 domains
**Status:** 🔄 ACTIVE
**Finding:** Average 7.4/10 expressiveness
- Best: Weather (9/10), Chemistry (8.5/10), Cooking (8/10)
- Challenging: Music (6.5/10), Sports (6.5/10)
- Pattern: Domain expressiveness = State/Process/Transformation %

**Vocabulary Support:** Identifies gaps, informs vocabulary expansion

---

### Games and Riddles

**File:** [limn-games-and-riddles.md](limn-games-and-riddles.md)

**Purpose:** Playful Limn applications for learning
**Status:** 🔄 ACTIVE
**Deliverables:** 14 games, 5 riddles, digital game concepts

---

### Cyclic Patterns Catalog

**File:** [limn-cyclic-patterns-catalog.md](limn-cyclic-patterns-catalog.md)

**Purpose:** Comprehensive catalog of X→Y→X patterns
**Status:** 🔄 ACTIVE
**Finding:** 26+ cyclic patterns identified

**Theory Support:** Core evidence for cyclic-pattern-analysis.md

---

### Heraclitus Translations

**File:** [limn-heraclitus-translations.md](limn-heraclitus-translations.md)

**Purpose:** Translate pre-Socratic fragments to test philosophical expression
**Status:** 🔄 ACTIVE
**Deliverables:** 13 fragments translated

---

### Haiku Collection

**File:** [limn-haiku-collection.md](limn-haiku-collection.md)

**Purpose:** Poetic expression testing
**Status:** 🔄 ACTIVE
**Deliverables:** 20 haiku

---

### Micro-Narratives

**File:** [limn-micro-narratives.md](limn-micro-narratives.md)

**Purpose:** Test narrative vs state fidelity
**Status:** 🔄 ACTIVE
**Deliverables:** 10 micro-stories

---

### Narrative vs States Test

**File:** [limn-narrative-vs-states-test.md](limn-narrative-vs-states-test.md)

**Purpose:** Compare X→Y narrative fidelity to state intersection
**Status:** 🔄 ACTIVE

---

### Translation Fidelity

**File:** [limn-translation-fidelity.md](limn-translation-fidelity.md)

**Purpose:** Measure information loss in round-trip translation
**Status:** 🔄 ACTIVE

---

## 📝 LEARNING MATERIALS

### Limn for Beginners

**File:** [limn-for-beginners.md](limn-for-beginners.md)

**Purpose:** Entry-level tutorial
**Status:** ✓ COMPLETE
**Audience:** New learners

---

### Learner's Path

**File:** [limn-learners-path.md](limn-learners-path.md)

**Purpose:** Progressive learning curriculum
**Status:** ✓ COMPLETE

---

### Quick Reference

**File:** [limn-quick-reference.md](limn-quick-reference.md)

**Purpose:** Cheat sheet for Limn
**Status:** ✓ COMPLETE

---

### Practice Sentences

**File:** [limn-practice-sentences.md](limn-practice-sentences.md)

**Purpose:** 200+ example sentences for practice
**Status:** ✓ COMPLETE

---

### Student Synthesis

**File:** [limn-student-synthesis.md](limn-student-synthesis.md)

**Purpose:** Student perspective synthesis
**Status:** 📝 DRAFT

---

## 🧪 THEORY TESTING

### Ambiguity Test Suite

**File:** [ambiguity-test-suite.md](ambiguity-test-suite.md)

**Purpose:** Test ambiguity metrics and key-collapse
**Status:** 📝 DRAFT

---

### Dialogue: Language and Machine

**File:** [limn-dialogue-language-and-machine.md](limn-dialogue-language-and-machine.md)

**Purpose:** Philosophical dialogue on Limn's nature
**Status:** ✓ COMPLETE

---

### Practical Applications

**File:** [limn-practical-applications.md](limn-practical-applications.md)

**Purpose:** Real-world use cases
**Status:** ✓ COMPLETE

---

### Expressiveness Test

**File:** [limn-expressiveness-test.md](limn-expressiveness-test.md)

**Purpose:** Measure expressiveness across domains
**Status:** ⚠️ SUPERSEDED (by limn-domain-applications.md?)

---

## 🔧 TECHNICAL

### Algorithm Encodings

**File:** [algorithm-encodings.md](algorithm-encodings.md)

**Purpose:** Express algorithms in Limn vs Python/English
**Status:** ✓ COMPLETE (8 algorithms)

---

### Limn-PL Examples

**File:** [limn-pl-examples.md](limn-pl-examples.md)

**Purpose:** Programming language extension examples
**Status:** ✓ COMPLETE

---

### Hello Limn

**File:** [hello_limn.md](hello_limn.md)

**Purpose:** First program tutorial
**Status:** ✓ COMPLETE

---

## 📊 TEST RESULTS

### results/ subdirectory

**Files:**
- [SUMMARY.md](results/SUMMARY.md)
- [ambiguity-test.md](results/ambiguity-test.md)
- [bootstrap-test.md](results/bootstrap-test.md)
- [commutativity-test.md](results/commutativity-test.md)
- [conversation-analysis.md](results/conversation-analysis.md)
- [cross-key-test.md](results/cross-key-test.md)
- [grouping-test.md](results/grouping-test.md)
- [inversion-test.md](results/inversion-test.md)
- [minimal-pairs-results.md](results/minimal-pairs-results.md)
- [negation-scope-test.md](results/negation-scope-test.md)
- [scalability-test.md](results/scalability-test.md)

**Status:** ✓ COMPLETE (archived test results)
**Note:** Many appear to be early validation tests

---

## 📋 META

### Test Cases

**File:** [test-cases.md](test-cases.md)

**Purpose:** Collection of test sentences
**Status:** ✓ COMPLETE

---

### Validation Results

**File:** [validation-results.md](validation-results.md)

**Purpose:** Summary of validation tests
**Status:** ✓ COMPLETE

---

### Learnability Study Protocol

**File:** [learnability-study-protocol.md](learnability-study-protocol.md)

**Purpose:** Formal study design for learnability research
**Status:** 📝 DRAFT (protocol design)

---

### Lore Fragments Learnability Review

**File:** [lore-fragments-learnability-review.md](lore-fragments-learnability-review.md)

**Purpose:** Evaluate LORE-FRAGMENTS.md for teaching potential
**Status:** ✓ COMPLETE (Task: limn-sbpf)
**Finding:** Overall 7.3/10, identified top teaching fragments

---

### SUMMARY.md

**File:** [SUMMARY.md](SUMMARY.md)

**Purpose:** Experiment summary (old?)
**Status:** ⚠️ POSSIBLY SUPERSEDED (check vs this INDEX.md)

---

## Cross-References to Theory/Spec

### Experiments Supporting Theory Documents

**constraint-surfaces.md:**
- Experiment 005 (compositional semantics)
- Zero-bootstrap validation

**cyclic-pattern-analysis.md:**
- limn-cyclic-patterns-catalog.md
- limn-multi-state-cycles.md

**liminal-semantics.md:**
- ambiguity-test-suite.md
- results/ambiguity-test.md

**zero-bootstrap-validation.md:**
- zero-bootstrap-validation-log.md (primary evidence)
- 003-bootstrap-without-english.md

---

## Experiments Cited in Specs

**README.md:**
- Experiment 005 (Empirical Validation section)

**bootstrap-v3-natural.md:**
- Zero-bootstrap validation findings (vocabulary gaps identified)

---

## Gaps & Future Work

### Experiments Needing Completion
- [ ] learnability-study-protocol.md (design → execution)
- [ ] limn-student-synthesis.md (draft → complete)
- [ ] ambiguity-test-suite.md (draft → execution)

### Missing Experiments
- [ ] Quantifier semantics validation
- [ ] Operator interaction empirical tests
- [ ] Cross-lingual compositionality (extend 005)
- [ ] Production LLM API testing (GPT-4, Claude, Gemini)

### Reconciliation Needed
- [ ] SUMMARY.md vs INDEX.md (this file) - merge or deprecate?
- [ ] limn-expressiveness-test.md vs limn-domain-applications.md - same experiment?

---

## Index Maintenance

**Update frequency:** Monthly or after major experiments
**Owner:** Student/Archivist
**Last review:** 2026-01-31

**To add experiment:**
1. Add to appropriate status category
2. Include key finding
3. Link to theory/spec docs
4. Mark status clearly

---

**Total Cataloged:** 49 experiments
**Validated:** 2 (Experiment 005, Zero-bootstrap)
**Complete:** ~25
**Active:** ~12
**Draft:** ~5
**Superseded:** ~2
**Results archived:** 10

---

*Experiment Index v1.0 | Limn Language Project*
