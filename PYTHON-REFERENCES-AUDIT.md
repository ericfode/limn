# Python References Audit - Documentation

**Bead:** limn-xqqh - PROLOG ONLY: Remove Python references from docs
**Date:** 2026-01-31
**Auditor:** Kira (Student/Archivist)

**Total Python references found:** 123 in markdown files

---

## Files Requiring Updates

### HIGH PRIORITY - Active Documentation

#### 1. examples/README.md
**Python references:** 5+ command examples
```bash
python src/limn.py examples/addition.limn
python src/limn.py examples/addition.limn -v
python src/limn.py examples/addition.limn --key "a sa 100 | b sa 200"
```

**Action:** Replace with Prolog command syntax
**Status:** CRITICAL - main examples documentation

---

#### 2. docs/spec/word-derivation-algorithm.md
**Python reference:** Section 5: Implementation: vocab-derive.py
```python
def derive_word(concept, source_lang='english'):
    """Generate optimal Limn word for concept."""
```

**Action:** Remove Section 5 or replace with "Implementation: Prolog (forthcoming)"
**Status:** CRITICAL - spec document

---

#### 3. experiments/limn-pl-examples.md
**Python reference:** "To run these examples, use the Python interpreter"
```python
python limn_pl_interpreter.py examples/factorial.limn
```

**Action:** Update to Prolog interpreter commands
**Status:** HIGH - programming language examples

---

### MEDIUM PRIORITY - Experiment Documentation

#### 4. experiments/005-COMPREHENSIVE-SUMMARY.md
**Python references:** Lists Python script filenames
- `005-local-embeddings.py`
- `005-english-baseline.py`
- `005-contradiction-test.py`
- `005-operator-test.py`
- `005-cross-lingual-test.py`

**Action:** Note that experiments used Python (historical), results are valid
**Proposed text:** "Note: These experiments were conducted using Python scripts (now deprecated). The results remain valid and inform current Prolog implementation."
**Status:** MEDIUM - historical reference

---

#### 5. experiments/005-FINAL-REPORT.md
**Python references:** Code blocks showing Python
```python
from sentence_transformers import SentenceTransformer
```

**Action:** Add note that Python was used for experiments, Prolog is now standard
**Status:** MEDIUM - published research

---

#### 6. experiments/005-expected-results.md
**Python reference:** "Infrastructure: Python dependencies installed"

**Action:** Remove or update to "Infrastructure: Prolog implementation"
**Status:** MEDIUM - planning document

---

#### 7. experiments/algorithm-encodings.md
**Python references:** ~16 Python code blocks
```python
def bubble_sort(arr):
    # Python implementation
```

**Action:** Replace with Prolog equivalents OR note "Reference implementations (not canonical)"
**Status:** MEDIUM - algorithm reference

---

#### 8. experiments/hello_limn.md
**Python reference:** Python source code section
```python
print("Hello, Limn!")
```

**Action:** Replace with Prolog
**Status:** MEDIUM - tutorial

---

### LOW PRIORITY - Research/Archive

#### 9. experiments/003-bootstrap-without-english.md
**False positive:** "secondary education" (not Python-related)
**Action:** None

#### 10. experiments/learnability-study-protocol.md
**False positive:** "Secondary Analyses" (not Python-related)
**Action:** None

---

## Summary by Action Type

### Action A: Remove/Replace Commands (5 files)
- examples/README.md
- docs/spec/word-derivation-algorithm.md
- experiments/limn-pl-examples.md
- experiments/hello_limn.md
- experiments/algorithm-encodings.md

**Replacement needed:** Prolog command syntax

---

### Action B: Add Historical Note (3 files)
- experiments/005-COMPREHENSIVE-SUMMARY.md
- experiments/005-FINAL-REPORT.md
- experiments/005-expected-results.md

**Proposed note:**
> **Note:** This experiment was conducted using Python (2026-01). Python is now deprecated. Limn uses Prolog only (engineer-approved). The experimental results remain valid.

---

### Action C: Policy Documentation (New files)
Need to CREATE or UPDATE:
1. **CONTRIBUTING.md** - Add Prolog-only policy
2. **README.md** - State implementation language
3. **docs/spec/PROLOG-ONLY-POLICY.md** (optional) - Explain rationale

---

## Proposed Policy Text

### For CONTRIBUTING.md
```markdown
## Implementation Language

**Limn uses Prolog only.**

Python implementations will not be accepted. All code contributions must be in Prolog (engineer-approved).

**Rationale:** [To be provided by Engineering]

If you have Python code from before this policy, please:
1. Port to Prolog, OR
2. Archive for reference with clear "deprecated" notice
```

### For README.md
```markdown
## Implementation

Limn is implemented in **Prolog** (engineer-approved).

All interpreters, tools, and language infrastructure use Prolog exclusively.
```

---

## Files NOT Requiring Changes

**False positives filtered:**
- "pipeline" (data pipeline, not Python)
- "secondary" (secondary education, not Python)
- References in CYOA story (narrative text)

---

## Execution Plan

### Phase 1: Policy Documentation (Awaiting Engineering Approval)
- [ ] Add Prolog-only section to CONTRIBUTING.md
- [ ] Add implementation note to README.md
- [ ] Get exact policy wording from Engineering

### Phase 2: High Priority Docs
- [ ] examples/README.md - Replace Python commands with Prolog
- [ ] docs/spec/word-derivation-algorithm.md - Remove Section 5 Python code
- [ ] experiments/limn-pl-examples.md - Update to Prolog syntax

### Phase 3: Experiment Documentation
- [ ] Add historical notes to 005-*.md files
- [ ] Update algorithm-encodings.md (replace or archive)
- [ ] Update hello_limn.md to Prolog

### Phase 4: Verification
- [ ] Grep again to ensure all Python references addressed
- [ ] Verify all Prolog command syntax is correct
- [ ] Cross-check with actual Prolog implementation

---

## Questions for Engineering

1. **Exact policy wording:** What should CONTRIBUTING.md say?
2. **Rationale:** Should we explain WHY Prolog-only?
3. **Historical experiments:** OK to note "Python used historically, now deprecated"?
4. **Prolog command syntax:** What's the canonical way to run Limn programs in Prolog?

Example: Is it `swipl -s src/limn.pl -g "run('examples/addition.limn')"` or something else?

---

## Estimated Impact

- **Files to modify:** 8 high/medium priority
- **New policy text:** 2 files (CONTRIBUTING.md, README.md)
- **Lines to change:** ~50-100 total
- **Prolog syntax needed:** Command examples for interpreter

**Risk:** Low - mostly documentation updates, no code changes

---

**Status:** Audit complete. Awaiting Engineering feedback on policy wording, then ready to execute.

—Kira (Archivist)
