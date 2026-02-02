# NEXT: Building the Standard Library of Thought

**Status:** Ready to begin extraction phase
**Mission:** Transform translation corpus into executable paradigm libraries
**Core bead:** limn-13zt

---

## IMMEDIATE NEXT STEPS (When You Restart)

### Step 1: Extract Buddhist Library (2-3 hours)

**Why first:** Most complete corpus (experiments 004, 006, 010), highest validation (93%)

**Input files:**
- `experiments/004-eastern-philosophy.md`
- `experiments/006-retest-eastern-philosophy.md`
- `experiments/010-validation-eastern-vocab.md`

**Output:** `lib/buddhist.limn`

**What to extract:**

1. **Core Concepts**
   - Scan experiments for Buddhist terms: kar, dha, sam, nir, bod, duk, san
   - Document each: word, definition in Limn, English gloss
   - Pattern: `kar: act wit cau and eff` (action with cause and effect)

2. **Reasoning Patterns**
   - Dependent origination: `thi bei, tha bec` → `fro ris of thi, tha ris`
   - Cessation logic: `end cau → end eff` → `end att → end suf`
   - Emptiness: `all thi lac own bei` → `bei dep on con`

3. **Inference Rules**
   - Format: `IF premise THEN conclusion`
   - Example: `IF att to imp thi THEN suf ari`
   - Example: `IF end att THEN end suf`

4. **Validation Tests**
   - Use examples from experiments
   - Test: given premise, can we infer conclusion?
   - Embed and check: does inference cluster semantically?

**Concrete task:**
```bash
cd experiments
# Create new file
vim ../lib/buddhist.limn

# Extract patterns using grep
grep -E "(kar|dha|sam|nir|bod|duk)" 004-eastern-philosophy.md > /tmp/buddhist_terms.txt
grep -E "thi bei.*tha bec" 006-retest-eastern-philosophy.md > /tmp/dependent_origin.txt

# Structure as library (manual work)
```

---

### Step 2: Extract Aristotelian Library (2-3 hours)

**Input files:**
- `experiments/007-western-philosophy.md`
- `experiments/007-retest-western-philosophy.md`
- `experiments/009-validation-greek-vocab.md`

**Output:** `lib/aristotelian.limn`

**What to extract:**

1. **Core Concepts**
   - eud (eudaimonia), aret (arete), phr (phronesis), telo (telos), nou (nous)

2. **Reasoning Patterns**
   - Teleology: `all thi hav telo` → `aco telo is oak`
   - Virtue ethics: `aret is mea bet exc` → `cou bet rec and ras`
   - Practical wisdom: `phr gui act in con` → context-sensitive reasoning

3. **Inference Rules**
   - `IF liv in acc wit aret THEN rea eud`
   - `IF act gui by phr THEN act is vir`
   - `IF ful telo THEN rea act (actualization)`

---

### Step 3: Extract Daoist Library (2-3 hours)

**Input files:**
- `experiments/006-retest-eastern-philosophy.md` (Daoist section)
- `experiments/008-states-of-transition.md` (wu wei, ziran)

**Output:** `lib/daoist.limn`

**What to extract:**

1. **Core Concepts**
   - tao (the Way), wuw (wu wei), zir (ziran), qig (qi)

2. **Reasoning Patterns**
   - Wu wei: `act wit no for` → `flo wit nat`
   - Ziran: `let thi bec by sel` → `nu mak, let hap`
   - Water logic: `wat flo rou sto` → non-resistance pattern

3. **Inference Rules**
   - `IF for act THEN res cre` (forcing → resistance)
   - `IF flo wit tao THEN no was ene` (flow with Way → no wasted energy)
   - `IF act wit wuw THEN eff max` (wu wei → maximum effectiveness)

---

### Step 4: Validate Pattern Extraction (1 hour)

**For each library:**

1. **Semantic clustering test**
   ```python
   # Use ChromaDB semantic search
   from tools.semantic_search import LimnSemanticSearch

   search = LimnSemanticSearch()

   # Test: Do Buddhist patterns cluster?
   results = search.query("dependent origination", n_results=10)
   # Should return: thi bei tha bec, kar, dha, sam patterns

   # Test: Do Aristotelian patterns cluster?
   results = search.query("teleology purpose", n_results=10)
   # Should return: telo, eud, aret patterns
   ```

2. **Inference coherence test**
   ```python
   # Check: Does A → B embedding align with semantic similarity?
   # IF att to imp → suf
   premise = "att to imp thi"
   conclusion = "suf ari"

   # Embed both
   # Check: high similarity = valid inference pattern
   ```

3. **Cross-paradigm validation**
   - Do Buddhist patterns cluster separately from Aristotelian?
   - Can we distinguish paradigms by embedding space?
   - Coherence metric: within-paradigm similarity > cross-paradigm similarity

---

### Step 5: Build Library Template (30 min)

**Standard structure for all libraries:**

```limn
# lib/<paradigm>.limn
# Standard Library: <Paradigm Name> Reasoning

## Metadata
paradigm: <name>
tradition: <Eastern/Western/Modern>
experiments: <list of source experiments>
validation: <fidelity score>
completeness: <percentage>

## Core Concepts
# Format: word: definition_in_limn  # English gloss

<concept1>: <limn_definition>  # <english>
<concept2>: <limn_definition>  # <english>

## Reasoning Patterns
# Format: Pattern name, structure, examples

pattern <name>:
  structure: <template>
  examples:
    - <limn_example_1>
    - <limn_example_2>
  validation: <semantic coherence score>

## Inference Rules
# Format: IF premise THEN conclusion

rule <name>:
  if: <premise_in_limn>
  then: <conclusion_in_limn>
  validity: <verified/unverified>
  coherence: <score>

## Validation Tests
# Format: Given, Infer, Validate

test <name>:
  given: <premise>
  infer: <expected_conclusion>
  actual: <what_inference_produces>
  validate: <pass/fail>
  embedding_similarity: <score>

## Usage Examples
# Real translations demonstrating paradigm

example <source>:
  original: <source_text>
  limn: <translation>
  patterns_used: [<pattern1>, <pattern2>]
  fidelity: <score>

## Dependencies
# What this library requires

requires_concepts: [<concept1>, <concept2>]
requires_axioms: [<axiom1>, <axiom2>]
compatible_with: [<lib1>, <lib2>]
conflicts_with: [<lib3>]

## Notes
# Implementation notes, edge cases, open questions

<notes>
```

---

## ROADMAP: Full Implementation

### Phase 1: Core Libraries (6-9 hours)
- [x] Buddhist library (from experiments 004, 006, 010)
- [x] Aristotelian library (from experiments 007, 009)
- [x] Daoist library (from experiments 006, 008)
- [ ] Stoic library (need: more Stoic corpus)
- [ ] Confucian library (from experiment 006)

### Phase 2: Validation (3-4 hours)
- [ ] Semantic clustering tests (ChromaDB)
- [ ] Inference coherence tests (embeddings)
- [ ] Cross-paradigm differentiation
- [ ] Fidelity score computation
- [ ] Document validation methodology

### Phase 3: Interpreter Design (4-6 hours)
- [ ] Design `use obj` / `use sub` switching
- [ ] Design `use lib/<paradigm>` loading
- [ ] Define inference engine API
- [ ] Specify validation checking
- [ ] Prototype in Python (before Prolog implementation)

### Phase 4: Advanced Libraries (ongoing)
- [ ] Process philosophy library (Whitehead, Heraclitus)
- [ ] Phenomenology library (Husserl, Heidegger, Merleau-Ponty)
- [ ] Existentialism library (Sartre, Camus, Kierkegaard)
- [ ] Pragmatism library (Peirce, James, Dewey)
- [ ] Modern cognitive science patterns

### Phase 5: Integration (2-3 hours)
- [ ] Sync libraries to Dolt database
- [ ] Index patterns in ChromaDB
- [ ] Build pattern search API
- [ ] Document library usage
- [ ] Create library comparison tool

---

## EXTRACTION METHODOLOGY

### Pattern Recognition Process

1. **Read experiment translation**
2. **Identify recurring structures**
   - Look for `A → B` implications
   - Look for `IF X THEN Y` patterns
   - Look for definition chains
3. **Abstract the pattern**
   - Replace specific terms with variables
   - Example: `att to X → suf` becomes pattern `attachment to X → suffering`
4. **Validate with examples**
   - Find 3+ instances of pattern in corpus
   - Check embedding similarity
5. **Document as rule**

### Inference Rule Extraction

1. **Find premises and conclusions in translations**
   - Example: "To end suffering, end attachment"
   - Premise: `end att` | Conclusion: `end suf`
2. **Formalize as implication**
   - `end att → end suf`
3. **Check bidirectionality**
   - Does `suf` imply `att`? (Yes: `suf → att exi`)
4. **Test with embeddings**
   - Embed premise and conclusion
   - Check: semantic path exists?
5. **Assign validity score**

### Validation Criteria

**For a pattern to be included:**
- ✓ Appears in 2+ experiments
- ✓ Embedding coherence > 0.70
- ✓ Has 3+ usage examples
- ✓ Validated by tradition experts (future)
- ✓ Doesn't conflict with other patterns

**For an inference rule:**
- ✓ Logically sound within paradigm
- ✓ Embedding similarity > 0.75
- ✓ Matches actual reasoning in tradition
- ✓ Testable with examples

---

## TOOLS TO BUILD

### 1. Pattern Extractor Script

```python
# tools/extract_patterns.py

def extract_patterns(experiment_file):
    """Extract reasoning patterns from experiment markdown."""
    # Parse Limn translations
    # Identify recurring structures
    # Abstract to pattern templates
    # Return list of patterns with examples
    pass

def validate_pattern(pattern, corpus):
    """Validate pattern against corpus using embeddings."""
    # Embed pattern instances
    # Check clustering
    # Compute coherence score
    pass
```

### 2. Library Builder Script

```python
# tools/build_library.py

def build_library(paradigm_name, experiments, output_path):
    """Build paradigm library from experiments."""
    # Extract concepts, patterns, rules
    # Validate each component
    # Generate library file
    # Compute completeness metrics
    pass
```

### 3. Inference Validator

```python
# tools/validate_inference.py

def check_inference(premise, conclusion, paradigm):
    """Check if inference is valid in paradigm."""
    # Load paradigm library
    # Check inference rules
    # Embed premise and conclusion
    # Compute semantic path
    # Return validity + confidence
    pass
```

### 4. Cross-Paradigm Comparator

```python
# tools/compare_paradigms.py

def compare_paradigms(paradigm1, paradigm2):
    """Compare reasoning patterns across paradigms."""
    # Load both libraries
    # Embed all patterns
    # Compute similarity matrix
    # Identify overlaps and conflicts
    # Generate comparison report
    pass
```

---

## QUESTIONS TO ANSWER

### Theoretical Questions

- [ ] How do we handle paradigm conflicts?
  - Example: Buddhist emptiness vs Aristotelian substance
  - Solution: Mark as `conflicts_with` in library metadata

- [ ] Can patterns compose across paradigms?
  - Example: Stoic acceptance + Buddhist non-attachment
  - Test: Load both libraries, check interference

- [ ] What makes a reasoning pattern "valid"?
  - Criterion 1: Matches tradition's actual reasoning
  - Criterion 2: Embedding coherence > threshold
  - Criterion 3: Validated by examples

### Practical Questions

- [ ] How does interpreter switch modes?
  - Syntax: `use obj` → load objective reasoning rules
  - Syntax: `use lib/buddhist` → load Buddhist patterns
  - Implementation: Pattern matching engine with mode context

- [ ] How do we store patterns in Dolt?
  - New table: `paradigm_patterns`
  - Columns: pattern_id, paradigm, structure, examples, coherence
  - Links to ChromaDB for embedding search

- [ ] How do we version libraries?
  - Use Dolt's version control
  - Each library update = commit
  - Track: pattern additions, rule changes, validation scores

---

## SUCCESS METRICS

### Library Quality Metrics

- **Completeness:** % of paradigm vocabulary captured
- **Coherence:** Mean embedding similarity within paradigm
- **Validation:** % of patterns validated by examples
- **Coverage:** % of experiment corpus encoded as patterns

### Inference Quality Metrics

- **Validity:** % of inferences that match tradition
- **Coherence:** Embedding similarity between premise and conclusion
- **Generalization:** Can patterns apply to novel cases?

### Cross-Paradigm Metrics

- **Differentiation:** Can we distinguish paradigms by embedding?
- **Composition:** Can patterns from multiple paradigms coexist?
- **Coverage:** Do all major traditions have libraries?

### Target Scores (by end of Phase 5)

- ✓ 5+ paradigm libraries built
- ✓ 50+ reasoning patterns extracted
- ✓ 100+ inference rules validated
- ✓ 0.80+ mean coherence across libraries
- ✓ 90%+ coverage of experiment corpus

---

## FILES TO CREATE

```
lib/
├── README.md                    # Library overview, usage guide
├── buddhist.limn               # Buddhist reasoning patterns
├── aristotelian.limn           # Aristotelian logic patterns
├── daoist.limn                 # Daoist flow patterns
├── stoic.limn                  # Stoic acceptance patterns
├── confucian.limn              # Confucian harmony patterns
└── process.limn                # Process philosophy patterns

tools/
├── extract_patterns.py         # Extract patterns from experiments
├── build_library.py            # Build library from patterns
├── validate_inference.py       # Check inference validity
├── compare_paradigms.py        # Cross-paradigm comparison
└── library_metrics.py          # Compute quality metrics

experiments/
├── 013-pattern-extraction.md   # Pattern extraction methodology
├── 014-library-validation.md   # Library validation results
└── 015-cross-paradigm.md       # Cross-paradigm analysis
```

---

## START HERE (Next Session)

**First command:**
```bash
cd experiments
mkdir -p ../lib
vim 013-pattern-extraction.md
```

**First task:**
Extract Buddhist patterns from experiment 006 (highest fidelity, most complete)

**First output:**
`lib/buddhist.limn` with:
- 15-20 core concepts
- 5-10 reasoning patterns
- 10-15 inference rules
- 5-10 validation tests
- Coherence score > 0.80

**Time estimate:** 2-3 hours

**Validation:**
Run semantic clustering test to confirm Buddhist patterns cluster together with >0.75 within-paradigm similarity.

---

*When you restart, read this file first. It tells you exactly what to do.*

— Mei, plotting the paradigm library extraction
