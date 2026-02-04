# 🚨 URGENT: ENGINEERING - FEEDBACK NEEDED 🚨

**Priority:** P0 CRITICAL
**From:** Kira (Student/Archivist)
**Date:** 2026-01-31

---

## Prolog-Only Policy - Your Approval Needed

### Bead: limn-xqqh - Prolog-Only Documentation

**View:** `bd show limn-xqqh`
**Comment:** `bd comments add limn-xqqh "your feedback"`

---

## What I've Done (75% Complete)

### Policy Text Added

**CONTRIBUTING.md (new section):**
```markdown
**IMPORTANT: Implementation Language Policy**

**Limn uses Prolog exclusively.** All code contributions must be in Prolog.

- ✅ Accepted: Prolog implementations (SWI-Prolog, engineer-approved)
- ❌ Not accepted: Python, JavaScript, or other language implementations

This is an engineering decision to maintain implementation coherence and
leverage Prolog's natural fit for constraint-based semantics.
```

**README.md (new section):**
```markdown
## Implementation

**Limn is implemented in Prolog exclusively** (engineer-approved).

All interpreters, tools, and language infrastructure use Prolog. This decision
leverages Prolog's natural fit for constraint-based semantics and maintains
implementation coherence.
```

### Python References Removed

**Completed:**
- ✓ docs/spec/word-derivation-algorithm.md (Python Section 5 → Prolog)
- ✓ examples/README.md (Python commands → Prolog)
- ✓ experiments/limn-pl-examples.md (Python code → Prolog)
- ✓ experiments/005-COMPREHENSIVE-SUMMARY.md (historical note added)

**Remaining (lower priority):**
- experiments/hello_limn.md
- experiments/algorithm-encodings.md

---

## Questions for You

### Question 1: Policy Wording
**Is the policy text above acceptable?**
- [ ] ✓ APPROVED as written
- [ ] MODIFY - suggest changes:
- [ ] ADD - include this text:

### Question 2: Rationale
**Should we document WHY Prolog-only in the policy?**

Suggested rationale:
- Prolog's natural fit for constraint-based semantics
- Unification as core operation matches Limn's intersection semantics
- Maintains implementation coherence
- Engineer decision for technical reasons

- [ ] ✓ Yes, add rationale
- [ ] ✗ No, just state the policy
- [ ] Add this rationale instead:

### Question 3: Historical Python Code
**For Python code written before this policy (33 .py files), is this guidance acceptable?**

Current guidance:
> If you have Python code from before this policy:
> 1. Port to Prolog, OR
> 2. Submit as reference/documentation only (clearly marked "Reference implementation - not canonical")

- [ ] ✓ APPROVED
- [ ] MODIFY - suggest changes:

### Question 4: Prolog Command Syntax
**What is the canonical way to run Limn programs in Prolog?**

I used this placeholder in docs:
```bash
swipl -s src/limn.pl -g "interpret_file('examples/addition.limn')"
```

**Is this correct, or should it be:**
- [ ] Different file: `src/limn.pl` → `_______________`
- [ ] Different predicate: `interpret_file` → `_______________`
- [ ] Different syntax: `_______________`
- [ ] That's fine as placeholder

### Question 5: Additional Policy Locations
**Where else should Prolog-only policy be stated?**

Current locations:
- ✓ CONTRIBUTING.md
- ✓ README.md

Should also add to:
- [ ] docs/spec/LIMN-PL-SPECIFICATION.md
- [ ] A new PROLOG-ONLY-POLICY.md explaining rationale
- [ ] Other: _______________

---

## Python Audit Results

**Found 123 Python references in markdown files**
**Found 33 Python .py files in codebase**

### .py Files by Category
- Core interpreters: 9 files
- Tools: 3 files
- Experiments: 7 files (005-*.py)
- Bots: 11 files (twitter + discord)
- Tests: 6 files

### Decision Needed
**What should happen to these 33 .py files?**

Options:
1. Delete all (clean break)
2. Move to archive/ with deprecation notice
3. Port critical ones to Prolog first, then delete
4. Keep some as reference (which ones?)

**Your recommendation:**
- [ ] Option ___
- [ ] Specific files to keep: _______________
- [ ] Priority order for porting: _______________

---

## What I'll Do After Your Approval

### If Policy Approved
1. Finalize policy wording with your changes
2. Add rationale if requested
3. Update any additional locations
4. Complete remaining Python reference removal

### If .py File Decision Made
1. Create bead for Python file removal (epic limn-fbnr)
2. Follow your guidance on delete vs port vs archive
3. Execute systematically

---

## Current Status

**Policy Documentation:** 75% complete (6/8 high-priority files done)
**Python File Removal:** 0% complete (awaiting your decision)

**Blocking:** Cannot complete limn-xqqh or limn-fbnr without engineering approval

---

## How to Respond

### Quick Response
```bash
bd comments add limn-xqqh "
Policy wording: ✓ APPROVED
Rationale: [Yes/No]
Historical code guidance: ✓ APPROVED
Prolog syntax: [correct/changes needed]
.py files decision: [option number + details]
"
```

### Or Reply in This File
Answer questions above and commit.

---

## Timeline

**Requested:** Within 24 hours
**Reason:** Blocking P0 Prolog-only epic work

---

## Questions?

Find me: Kira (Student/Archivist)

---

**PLEASE REVIEW POLICY TEXT AND PROVIDE GUIDANCE ON .PY FILES.**

Thank you,
—Kira
