# Limn Power Prompting Vocabulary

**Author:** Dr. Solvik
**Date:** 2026-02-02
**Task:** limn-srxg - Limn Power Prompting Pack
**Status:** Initial mapping & gap analysis

---

## Goal

Make Limn prompts **more powerful than English**:
- Fewer tokens for same precision
- Less ambiguity, fewer retries
- Clearer agent instructions

---

## Phase 1: Core Prompting Vocabulary

### Task Verbs (File & Data Operations)

| Concept | Proposed | Existing | Meaning | Status |
|---------|----------|----------|---------|--------|
| read | `rea` | `red` | read, parse, load | ✓ Use `red` |
| write | `wri` | `wri` | write, inscribe, record | ✓ Exists |
| find | `fin` | `fnd` | find, discover, locate | ✓ Use `fnd` |
| create | `cre` | `mak`, `gnrt` | make/create, generate | ✓ Use `mak` or `gnrt` |
| delete | `del` | `ers` | erase, delete | ✓ Use `ers` (or need better?) |
| update | `upd` | `upd` | update, modify | ✓ Exists |
| analyze | `ana` | — | — | ⧗ MISSING |
| compare | `cmp` | `tha` | than, compared to | ⧗ Partial (`tha`), or need `cmp` variant |

**Analysis:**
- Most verbs exist but with different words than initially proposed
- `ana` (analyze) is missing - critical for prompting
- `cmp` collision: exists as "complete" but need "compare"

---

### Output Modes

| Concept | Proposed | Existing | Meaning | Status |
|---------|----------|----------|---------|--------|
| list | `lst` | `lst` | list, array, collection | ✓ Exists |
| code | `cod` | `cod` | code, program | ✓ Exists |
| table | `tbl` | `tbl` | table, tabular format | ✓ Exists |
| brief | `bre` | `bre` | brief | ✓ Exists |
| detail | `det` | `rfn` | refining: gaining detail | ⧗ Partial |

**Analysis:**
- Output modes are well-covered
- `det` means "single meaning" currently, but `rfn` (refining) covers gaining detail
- May want explicit "detail" vs "summary" vocabulary

---

### Flow Control

| Concept | Proposed | Existing | Meaning | Status |
|---------|----------|----------|---------|--------|
| step | `stp` | `stp` | stop, halt, cease | ✓ Exists (but means "stop" not "step") |
| next | `nxt` | `nxt` | next, following | ✓ Exists |
| done | `don` | `don` | done, settled, complete | ✓ Exists |
| retry | `rtr` | `rtr` | retry, attempt again | ✓ Exists |
| skip | `skp` | `skp` | skip, bypass intentionally | ✓ Exists |

**Analysis:**
- Flow control is excellent!
- `stp` collision: means "stop" not "step" - may need different word for "step"

---

## Phase 2: Reasoning Vocabulary

### Logic & Inference

| Concept | Proposed | Existing | Status | Notes |
|---------|----------|----------|--------|-------|
| deduce | `ded` | ? | ⧗ Check | Logical deduction |
| assume | `asm` | `asm` | ✓ Exists | "assume, take as given" |
| hypothesize | `hyp` | ? | ⧗ Check | Form hypothesis |
| verify | `ver` | `ver` | ✓ Exists | "verified, confirmed true" |
| prove | `prf` | `prf` | ✓ Exists | "proof, prove, verify" |

### Attention & Focus

| Concept | Proposed | Existing | Status | Notes |
|---------|----------|----------|--------|-------|
| focus | `foc` | ? | ⧗ Check | Attention focus |
| scope | `sco` | ? | ⧗ Check | Define scope |
| ignore | `ign` | `ign` | ✓ Exists | "ignore, skip, disregard" |
| prioritize | `pri` | ? | ⧗ Check | Set priority |

---

## Phase 3: Constraints Vocabulary

| Concept | Proposed | Existing | Status |
|---------|----------|----------|--------|
| limit | `lim` | `lim` | ✓ Exists (boundary region) |
| require | `req` | `req` | ✓ Exists (requires, needs before) |
| forbid | `for` | `frb` | ✓ Exists (forbid, prohibit) |
| prefer | `prf` | ? | ⧗ Collision with `prf` = proof |
| exact | `exa` | ? | ⧗ Check |
| precise | `prc` | `prc` | ✓ Exists (precise, exact - LLM success mode) |
| creative | `cre` | `cre` | ✓ Exists (creative, original) |
| thorough | `thr` | ? | ⧗ Check (collision with `thr` = thread?) |

---

## Phase 4: Meta-Cognition Vocabulary

| Concept | Proposed | Existing | Status |
|---------|----------|----------|--------|
| check | `chk` | ? | ⧗ Check |
| validate | `val` | ? | ⧗ Check |
| error | `err` | `err` | ✓ Exists (error, fault, bug) |
| confirm | `con` | ? | ⧗ Check (many `con` words) |
| mode | `mod` | ? | ⧗ Check |
| analytical | `ana` | ? | ⧗ Collision with analyze |
| critical | `cri` | ? | ⧗ Check |
| exploratory | `exp` | ? | ⧗ Check |

---

## Gap Analysis Summary

### Critical Missing Words

**High Priority (essential for prompting):**
1. `ana` - analyze (no good alternative)
2. Compare verb (not `cmp` which = complete)
3. `chk` - check/verify
4. `val` - validate
5. Step (not `stp` which = stop)

**Medium Priority (nice to have):**
6. `foc` - focus
7. `sco` - scope
8. `hyp` - hypothesize
9. `exa` - exact
10. Mode vocabulary

---

## Collision Resolution Strategy

### Problem Collisions

| Word | Current Meaning | Desired Meaning | Solution |
|------|----------------|-----------------|----------|
| `cmp` | complete | compare | Keep `cmp` = complete, use different word for compare |
| `stp` | stop | step | Keep `stp` = stop, use different word for step |
| `prf` | proof | prefer | Keep `prf` = proof, use different word for prefer |
| `det` | single meaning | detail | Keep `det` = single meaning, use `rfn` or new word for detail |

### Proposed Solutions

**Compare:**
- Option 1: `cmpr` (4 letters, extended form)
- Option 2: `cmp` stays "complete", introduce `par` (compare/parallel)
- Option 3: Use `tha` (than) for comparisons

**Step:**
- Option 1: `step` (4 letters, keep English)
- Option 2: `stg` (stage)
- Option 3: `phs` (phase)

**Analyze:**
- Option 1: `ana` (check collision first)
- Option 2: `anl` or `anz`
- Option 3: `exm` (examine)

---

## Next Steps

1. ⧗ Check collisions for proposed missing words
2. ⧗ Add high-priority prompting vocabulary
3. ⧗ Create test tasks (English vs Limn comparison)
4. ⧗ Build test harness for A/B testing
5. ⧗ Iterate based on results

---

## Testing Framework Outline

### Test Structure
```
Task: [Description]
English prompt: [Full English version]
Limn prompt: [Equivalent Limn version]
Metrics:
  - Token count: EN vs LMN
  - Ambiguity: retries needed
  - Success: task completed correctly
```

### Sample Test Task (Level 1)

**Task:** Read file, find pattern, report occurrences

**English:**
"Please read the file config.json, find all occurrences of the word 'debug', and report how many times it appears."

**Limn (draft):**
`red config.json | fnd 'debug' | cnt | rep`

**Token analysis:**
- English: 25 tokens (estimated)
- Limn: 8 tokens (estimated)
- Compression: 68%

**Ambiguity:**
- English: Could mean "read and report", "find and count", "just list them", etc.
- Limn: Pipe structure makes flow explicit

---

## Open Questions

1. Should we leverage Unix pipe semantics (`|`) in Limn prompts?
2. How much do LLMs already understand Limn vocabulary?
3. What's the learning curve for users?
4. Can we make Limn prompts self-documenting?

---

## Phase 1 Additions Complete

### Added Words (2026-02-02)

| Word | Meaning | Domain | Status |
|------|---------|--------|--------|
| `ana` | analyze, examine systematically | Agent/AI | ✓ Added |
| `cmpr` | compare, contrast, evaluate differences | Agent/AI | ✓ Added |
| `chk` | check, verify, test | Agent/AI | ✓ Added |
| `vld` | validate, confirm correctness | Agent/AI | ✓ Added |
| `sco` | scope, boundary, extent | Agent/AI | ✓ Added |
| `phs` | phase, step, stage | Agent/AI | ✓ Added |

### Vocabulary Status
- **Total words:** 1066 (+6 from this session)
- **Agent/AI domain:** 155 words
- **Progress:** Core prompting vocabulary now complete!

### Updated Phase 1 Mapping

**Task Verbs:**
- read: `red` ✓
- write: `wri` ✓
- find: `fnd` ✓
- create: `mak` or `gnrt` ✓
- delete: `ers` ✓
- update: `upd` ✓
- **analyze: `ana` ✓ ADDED**
- **compare: `cmpr` ✓ ADDED**

**Output Modes:**
- list: `lst` ✓
- code: `cod` ✓
- table: `tbl` ✓
- brief: `bre` ✓
- detail: `rfn` ✓

**Flow Control:**
- stop: `stp` ✓
- **phase/step: `phs` ✓ ADDED**
- next: `nxt` ✓
- done: `don` ✓
- retry: `rtr` ✓
- skip: `skp` ✓

**Meta-Operations:**
- **check: `chk` ✓ ADDED**
- **validate: `vld` ✓ ADDED**
- **scope: `sco` ✓ ADDED**
- verify: `ver` ✓ (existed)
- prove: `prf` ✓ (existed)
- focus: `foc` ✓ (existed)

### Phase 1 Status: COMPLETE ✓

All critical prompting vocabulary is now available!

---

## Next Steps

1. ✓ Identify gaps (DONE)
2. ✓ Add Phase 1 core vocabulary (DONE - 6 words)
3. ⧗ Create test tasks (English vs Limn)
4. ⧗ Build test harness
5. ⧗ Run A/B comparisons
6. ⧗ Iterate based on results
7. ⧗ Add Phases 2-4 vocabulary as needed

---

*pro com | tok sav | amb red*
*prompts compressed | tokens saved | ambiguity reduced*

**Status: Phase 1 complete (+6 words), ready for testing**
