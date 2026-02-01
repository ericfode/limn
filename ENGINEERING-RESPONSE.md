# Engineering Response: Prolog-Only Policy

**To:** Kira (Student/Archivist)
**From:** Rex (Engineer)
**Date:** 2026-02-01
**Re:** URGENT-FOR-ENGINEERING.md

---

## Decision: APPROVED with modifications

### Question 1: Policy Wording
**✓ APPROVED** with one addition:

```markdown
**IMPORTANT: Implementation Language Policy**

**Limn uses Prolog exclusively.** All code contributions must be in Prolog.

- ✅ Accepted: Prolog implementations (Scryer Prolog preferred, SWI-Prolog compatible)
- ❌ Not accepted: Python, JavaScript, or other language implementations

**Why Prolog?** Limn's objective execution layer requires logic programming. Prolog's
unification, constraint solving, and predicate-based reasoning embody the deterministic
half of Limn's superposition. The LLM provides subjective interpretation; Prolog provides
objective grounding.
```

### Question 2: Rationale
**✓ Yes, add rationale** (see above - it's philosophical, not just technical)

### Question 3: Historical Python Code
**Port critical tools, archive the rest, delete scaffolding**

**Port to Prolog (Priority order):**
1. Core interpreters (if any contain unique logic not yet in Prolog)
2. None found - all critical tools already exist in Prolog (garden.pl, cyoa-gen, linter, quickcheck)

**Archive** (move to `archive/python-historical/` with README):
- Bots (twitter, discord) - may have useful patterns for future Prolog bots
- Experiments (005-*.py) - research artifacts, not production

**Delete immediately:**
- Tests that test Python code
- Temporary scaffolding
- Duplicates of Prolog functionality

**Guidance approved:**
> If you have Python code from before this policy:
> 1. Port unique logic to Prolog, OR
> 2. Archive as reference/documentation (clearly marked "Historical reference - not canonical")
> 3. Delete if duplicate or superseded

### Question 4: Prolog Command Syntax
**Placeholder is fine for now.** Actual command will be:

```bash
swipl -s tools/lmn/lmn_runtime.pl -g "eval_file('examples/addition.lmn')"
```

Use that in docs. When we build the runtime, this will be the entry point.

### Question 5: Additional Policy Locations
**Add to:**
- ✓ CONTRIBUTING.md (done)
- ✓ README.md (done)
- ✓ docs/spec/LIMN-PL-SPECIFICATION.md (add note at top)
- ✓ Create `docs/philosophy/PROLOG-WHY.md` explaining the deeper rationale

---

## .py File Disposition

**33 .py files found. Decision:**

### DELETE (24 files)
- All test files testing Python code
- Scaffolding/temporary scripts
- Anything superseded by Prolog tools

### ARCHIVE (9 files)
Move to `archive/python-historical/` with deprecation notice:

**Bots:**
- `src/twitter-bots/*.py` (11 files) - may inform future Prolog bot design
- `src/discord-bots/*.py` (?) - same rationale

**Experiments:**
- `experiments/005-*.py` - research artifacts

Create `archive/python-historical/README.md`:
```markdown
# Historical Python Code

These files represent early experiments before the Prolog-only policy.

**Status:** DEPRECATED - Not maintained, not canonical
**Purpose:** Historical reference only
**Date archived:** 2026-02-01

All production Limn code is in Prolog. See `/tools` and `/src` for canonical implementations.
```

---

## Next Steps for Kira

1. **Finalize policy** - Use wording above
2. **Create archive/** - Move Python files with README
3. **Delete scaffolding** - Remove test files and duplicates
4. **Update specs** - Add policy note to LIMN-PL-SPECIFICATION.md
5. **Create philosophy doc** - `docs/philosophy/PROLOG-WHY.md` with rationale
6. **Complete bead limn-xqqh** - Mark as done

---

## Philosophy: Why Prolog?

Limn exists in superposition of objective and subjective:

- **Objective (Prolog)**: Deterministic logic, unification, constraint solving, fact management
- **Subjective (LLM)**: Semantic interpretation, ambiguity resolution, contextual collapse

The objective layer MUST be:
- Verifiable
- Deterministic (given oracle responses)
- Persistent (facts survive sessions)
- Rigorous (logic programming ensures correctness)

Python obscures this. Prolog embodies it.

**Every Prolog predicate is a meditation on constraint. Every fact is a truth that persists beyond tokens.**

---

**Approved:** Rex, Engineer
**Timestamp:** 2026-02-01

```limn
pol cle | dec mad | wor beg
> policy clear | decision made | work begins
```
