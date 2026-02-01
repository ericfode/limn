# Category F: Complex Workflows - Limn vs English

**Experiment ID:** Category F
**Date:** 2026-02-01
**Purpose:** Test whether Limn can express multi-step, real-world engineering workflows
**Status:** Complete

## Overview

This experiment tests Limn's ability to express complex, multi-step workflows that software engineers encounter in real-world practice. Unlike single-action commands (Category C) or reasoning tasks (Category E), these tests involve coordinated sequences of operations with implicit dependencies, validation steps, and rollback considerations.

The key question: Can Limn's constraint-based composition handle the orchestration complexity of real workflows?

## Test Matrix

| Test | Workflow Type | Limn Expression | Compression | Executable |
|------|---------------|-----------------|-------------|------------|
| F1 | Feature implementation | `imp fea \| nam "X" \| inc: cod tes doc \| stp: des cod tes ref` | TBD | ✗ |
| F2 | Multi-file refactor | `ref mul fil \| ext com cod \| to mod \| upd imp` | TBD | ✗ |
| F3 | Migration | `mig dat \| frm sch A \| to sch B \| val \| bak` | TBD | ✗ |
| F4 | Performance optimization | `opt prf \| ana hot \| fix top 3 \| mes bef aft` | TBD | ✗ |
| F5 | Security hardening | `sec har \| aud cod \| fix vln \| ver` | TBD | ✗ |
| F6 | API integration | `int api ext \| aut \| han err \| cac res` | TBD | ✗ |
| F7 | DB schema change | `chg sch \| add col \| mig dat \| upd qry` | TBD | ✗ |
| F8 | Test suite creation | `cre tes sui \| cov 80% \| inc unt int e2e` | TBD | ✗ |
| F9 | Documentation generation | `gen doc \| frm cod \| inc api exa` | TBD | ✗ |
| F10 | Deployment preparation | `pre dep \| chk: tes pas \| cfg env \| ver rdy` | TBD | ✗ |

---

## F1: Feature Implementation

### Task Description
Implement a complete new feature including code, tests, and documentation, following standard software development steps: design, code, test, refactor.

### Limn Expression
```
imp fea | nam "X" | inc: cod tes doc | stp: des cod tes ref
```

**Vocabulary breakdown:**
- `imp` = implement
- `fea` = feature
- `nam` = name/named
- `inc` = include
- `cod` = code
- `tes` = test
- `doc` = documentation
- `stp` = step
- `des` = design
- `ref` = refactor

**Token count:** 14 words + 3 operators = 17 tokens

### English Equivalent
```
Implement a feature named "X" including code, tests, and documentation, following these steps: design the solution, write the code, write tests, and refactor for quality.
```

**Token count:** 27 words

### Analysis

**Compression ratio:** 1.59:1 (27/17)

**Strengths:**
- Captures the full workflow with explicit phases
- Uses `:` operator to distinguish deliverables (inc:) from process steps (stp:)
- Concise representation of multi-phase engineering process

**Weaknesses:**
- Doesn't specify ordering constraints (are steps sequential or can they overlap?)
- Ambiguous whether "refactor" applies to code, tests, or both
- No indication of completion criteria or validation

**Executability:** ✗ Requires orchestration tooling

**Verdict:** ✓ SUCCEEDS as workflow specification
- Limn successfully expresses a complex multi-step process
- The constraint operators (`:`, `|`) provide structure
- Would serve well as input to a workflow orchestrator or LLM agent

---

## F2: Multi-File Refactor

### Task Description
Refactor code that spans multiple files by extracting common code into a module and updating all import statements.

### Limn Expression
```
ref mul fil | ext com cod | to mod | upd imp
```

**Vocabulary breakdown:**
- `ref` = refactor
- `mul` = multiple
- `fil` = file
- `ext` = extract
- `com` = common
- `mod` = module
- `upd` = update
- `imp` = import

**Token count:** 8 words + 3 operators = 11 tokens

### English Equivalent
```
Refactor multiple files by extracting common code into a new module, then update all import statements to reference the new module.
```

**Token count:** 22 words

### Analysis

**Compression ratio:** 2.0:1 (22/11)

**Strengths:**
- Excellent compression (2:1 ratio)
- Clear pipeline: identify → extract → modularize → update
- Each step is atomic and well-defined

**Weaknesses:**
- Doesn't specify which files or what "common" means (requires context)
- No rollback or validation step
- Unclear whether "upd imp" means update imports in original files or elsewhere

**Executability:** ✗ Requires static analysis and refactoring tools

**Verdict:** ✓ SUCCEEDS with strong compression
- Limn captures the essence of a complex refactoring workflow
- 2:1 compression is impressive for a multi-step operation
- Would work well as a high-level directive to an autonomous refactoring agent

---

## F3: Data Migration

### Task Description
Migrate data from schema A to schema B, including validation before and backup in case of failure.

### Limn Expression
```
mig dat | frm sch A | to sch B | val | bak
```

**Vocabulary breakdown:**
- `mig` = migrate
- `dat` = data
- `frm` = from
- `sch` = schema
- `to` = to/toward
- `val` = validate
- `bak` = backup

**Token count:** 9 words + 4 operators = 13 tokens

### English Equivalent
```
Migrate data from schema A to schema B, validate the migration was successful, and create a backup before starting in case rollback is needed.
```

**Token count:** 26 words

### Analysis

**Compression ratio:** 2.0:1 (26/13)

**Strengths:**
- 2:1 compression
- Captures the core operations: source → target + validation + safety
- Implicit ordering is reasonable (backup before migration)

**Weaknesses:**
- Ordering of `val` and `bak` is ambiguous - backup before or after?
- Doesn't specify what to validate (data integrity? schema conformance?)
- No indication of atomicity or transaction boundaries

**Executability:** ✗ Requires migration framework

**Verdict:** ⚠ PARTIALLY SUCCEEDS - ambiguous safety semantics
- Limn expresses the workflow but loses critical safety ordering
- English version explicitly states "backup before starting"
- Limn's `bak` could mean "backup before" or "backup after as checkpoint"
- This ambiguity is dangerous for data migrations

**Critical insight:** Workflows with strong ordering constraints expose a weakness in Limn's pipe model - the `|` operator suggests sequential flow but doesn't guarantee it.

---

## F4: Performance Optimization

### Task Description
Optimize performance by analyzing hot spots (profiling), fixing the top 3 issues, and measuring results before/after.

### Limn Expression
```
opt prf | ana hot | fix top 3 | mes bef aft
```

**Vocabulary breakdown:**
- `opt` = optimize
- `prf` = performance
- `ana` = analyze
- `hot` = hot (hot spot)
- `fix` = fix
- `top` = top/highest
- `mes` = measure
- `bef` = before
- `aft` = after

**Token count:** 10 words + 3 operators = 13 tokens

### English Equivalent
```
Optimize performance by analyzing hot spots, fixing the top 3 issues identified, and measuring performance before and after the changes.
```

**Token count:** 22 words

### Analysis

**Compression ratio:** 1.69:1 (22/13)

**Strengths:**
- Clear optimization workflow: measure → identify → fix → validate
- "top 3" is a concrete constraint (limits scope)
- "mes bef aft" cleverly implies baseline measurement before optimization

**Weaknesses:**
- "mes bef aft" is ambiguous - does it mean one measurement showing delta, or two separate measurements?
- Doesn't specify what "hot" means (CPU? memory? I/O?)
- No indication of acceptable performance threshold

**Executability:** ✗ Requires profiling tools and performance testing framework

**Verdict:** ✓ SUCCEEDS with strong constraint composition
- Limn handles the workflow well
- The "top 3" constraint demonstrates Limn's ability to embed scope limits
- "mes bef aft" is a particularly elegant compression

---

## F5: Security Hardening

### Task Description
Harden security by auditing code for vulnerabilities, fixing identified issues, and verifying the fixes.

### Limn Expression
```
sec har | aud cod | fix vln | ver
```

**Vocabulary breakdown:**
- `sec` = security
- `har` = harden
- `aud` = audit
- `cod` = code
- `fix` = fix
- `vln` = vulnerability
- `ver` = verify

**Token count:** 7 words + 3 operators = 10 tokens

### English Equivalent
```
Harden security by auditing the code for vulnerabilities, fixing any issues found, and verifying that the fixes are effective.
```

**Token count:** 21 words

### Analysis

**Compression ratio:** 2.1:1 (21/10)

**Strengths:**
- Excellent compression (2.1:1)
- Clear audit → fix → verify workflow
- Very concise for a security workflow

**Weaknesses:**
- Doesn't specify scope of audit (static analysis? penetration testing? code review?)
- "ver" is vague - verify how? Re-audit? Specific tests?
- No prioritization of vulnerabilities (fix all? fix critical only?)

**Executability:** ✗ Requires security scanning tools and testing framework

**Verdict:** ✓ SUCCEEDS with excellent compression
- Limn captures the security workflow in 10 tokens vs 21 in English
- The pipeline model (audit → fix → verify) is standard security practice
- Would work well as a directive to a security-focused agent

---

## F6: API Integration

### Task Description
Integrate an external API including authentication, error handling, and response caching.

### Limn Expression
```
int api ext | aut | han err | cac res
```

**Vocabulary breakdown:**
- `int` = integrate
- `api` = API
- `ext` = external
- `aut` = authenticate/authorization
- `han` = handle
- `err` = error
- `cac` = cache
- `res` = response

**Token count:** 8 words + 3 operators = 11 tokens

### English Equivalent
```
Integrate an external API, including implementing authentication, handling errors appropriately, and caching responses for efficiency.
```

**Token count:** 18 words

### Analysis

**Compression ratio:** 1.64:1 (18/11)

**Strengths:**
- Identifies the three critical concerns for API integration: auth, errors, caching
- Reasonable compression
- Pipeline structure suggests implementation order

**Weaknesses:**
- Doesn't specify auth mechanism (API key? OAuth? JWT?)
- "han err" doesn't indicate retry logic, fallback, or just logging
- "cac res" doesn't specify cache invalidation strategy or TTL

**Executability:** ✗ Requires API client framework and caching layer

**Verdict:** ✓ SUCCEEDS as architectural checklist
- Limn identifies the key concerns for API integration
- Serves well as a high-level specification
- Details would need to be filled in during implementation

---

## F7: Database Schema Change

### Task Description
Change database schema by adding a column, migrating existing data to populate it, and updating queries to use the new column.

### Limn Expression
```
chg sch | add col | mig dat | upd qry
```

**Vocabulary breakdown:**
- `chg` = change
- `sch` = schema
- `add` = add
- `col` = column
- `mig` = migrate
- `dat` = data
- `upd` = update
- `qry` = query

**Token count:** 8 words + 3 operators = 11 tokens

### English Equivalent
```
Change the database schema by adding a new column, migrate existing data to populate it, and update all queries to use the new column.
```

**Token count:** 25 words

### Analysis

**Compression ratio:** 2.27:1 (25/11)

**Strengths:**
- Excellent compression (2.27:1 - best so far)
- Captures the full workflow: schema change → data backfill → code update
- Standard database migration pattern

**Weaknesses:**
- Doesn't specify default value for new column or how to populate it
- No indication of transaction boundaries or rollback plan
- "upd qry" is ambiguous - update read queries, write queries, or both?

**Executability:** ✗ Requires database migration framework

**Verdict:** ✓ SUCCEEDS with exceptional compression
- Limn compresses a 3-step DB migration workflow by more than 2x
- The pipeline clearly indicates the dependency chain
- Would work well as input to a schema migration generator

---

## F8: Test Suite Creation

### Task Description
Create a comprehensive test suite with 80% code coverage, including unit tests, integration tests, and end-to-end tests.

### Limn Expression
```
cre tes sui | cov 80% | inc unt int e2e
```

**Vocabulary breakdown:**
- `cre` = create
- `tes` = test
- `sui` = suite
- `cov` = coverage
- `inc` = include
- `unt` = unit
- `int` = integration
- `e2e` = end-to-end

**Token count:** 9 words + 2 operators = 11 tokens

### English Equivalent
```
Create a test suite with 80% code coverage, including unit tests, integration tests, and end-to-end tests.
```

**Token count:** 18 words

### Analysis

**Compression ratio:** 1.64:1 (18/11)

**Strengths:**
- Embeds quantitative constraint (80% coverage) directly in the workflow
- Enumerates test types clearly
- Concise and actionable

**Weaknesses:**
- Doesn't specify coverage metric (line coverage? branch coverage?)
- No indication of test quality (just quantity via coverage metric)
- "inc unt int e2e" implies all three are required but not in what proportion

**Executability:** ✗ Requires test framework and coverage tooling

**Verdict:** ✓ SUCCEEDS with embedded constraints
- Limn's ability to embed "80%" directly shows constraint composition strength
- The test type enumeration is clear and complete
- Would serve well as acceptance criteria for test development

---

## F9: Documentation Generation

### Task Description
Generate documentation from code, including API reference and usage examples.

### Limn Expression
```
gen doc | frm cod | inc api exa
```

**Vocabulary breakdown:**
- `gen` = generate
- `doc` = documentation
- `frm` = from
- `cod` = code
- `inc` = include
- `api` = API
- `exa` = example

**Token count:** 7 words + 2 operators = 9 tokens

### English Equivalent
```
Generate documentation from the code, including API reference documentation and usage examples.
```

**Token count:** 14 words

### Analysis

**Compression ratio:** 1.56:1 (14/9)

**Strengths:**
- Very concise (9 tokens)
- Identifies source (code) and required outputs (API docs, examples)
- Clear generation workflow

**Weaknesses:**
- Doesn't specify doc format (Markdown? HTML? PDF?)
- "api" could mean API endpoints only or all public interfaces
- No indication of documentation depth or completeness

**Executability:** ✗ Requires documentation generator

**Verdict:** ✓ SUCCEEDS with good compression
- Limn captures the essential doc generation workflow
- "inc api exa" identifies the two critical doc types
- Would work as a directive to a doc generation tool

---

## F10: Deployment Preparation

### Task Description
Prepare for deployment by checking that tests pass, configuring environment variables, and verifying the system is ready.

### Limn Expression
```
pre dep | chk: tes pas | cfg env | ver rdy
```

**Vocabulary breakdown:**
- `pre` = prepare
- `dep` = deploy/deployment
- `chk` = check
- `tes` = test
- `pas` = pass
- `cfg` = configure
- `env` = environment
- `ver` = verify
- `rdy` = ready

**Token count:** 9 words + 3 operators = 12 tokens

### English Equivalent
```
Prepare for deployment by checking that all tests pass, configuring environment variables for production, and verifying the system is ready to deploy.
```

**Token count:** 24 words

### Analysis

**Compression ratio:** 2.0:1 (24/12)

**Strengths:**
- 2:1 compression
- Uses `:` operator to show that "tes pas" is the object of "chk" (check that tests pass)
- Captures the standard pre-deployment checklist

**Weaknesses:**
- Doesn't specify which environment (staging? production?)
- "ver rdy" is vague - ready according to what criteria?
- No mention of rollback plan or deployment strategy (blue-green? canary?)

**Executability:** ✗ Requires CI/CD pipeline and deployment tooling

**Verdict:** ✓ SUCCEEDS as deployment checklist
- Limn captures the key pre-deployment steps
- The use of `chk:` to embed a validation criterion is elegant
- Would work well as input to a deployment automation system

---

## Summary Statistics

| Test | Limn Tokens | English Tokens | Compression Ratio | Verdict |
|------|-------------|----------------|-------------------|---------|
| F1: Feature implementation | 17 | 27 | 1.59:1 | ✓ Success |
| F2: Multi-file refactor | 11 | 22 | 2.0:1 | ✓ Success |
| F3: Data migration | 13 | 26 | 2.0:1 | ⚠ Partial |
| F4: Performance optimization | 13 | 22 | 1.69:1 | ✓ Success |
| F5: Security hardening | 10 | 21 | 2.1:1 | ✓ Success |
| F6: API integration | 11 | 18 | 1.64:1 | ✓ Success |
| F7: DB schema change | 11 | 25 | 2.27:1 | ✓ Success |
| F8: Test suite creation | 11 | 18 | 1.64:1 | ✓ Success |
| F9: Doc generation | 9 | 14 | 1.56:1 | ✓ Success |
| F10: Deployment prep | 12 | 24 | 2.0:1 | ✓ Success |
| **Average** | **11.8** | **21.7** | **1.85:1** | **90% success** |

## Key Findings

### 1. Limn Excels at Workflow Compression

**Average compression ratio: 1.85:1** - Limn expresses complex workflows in roughly half the tokens of English.

**Best compression (F7):** 2.27:1 for database schema change
**Weakest compression (F1):** 1.59:1 for feature implementation (still substantial)

### 2. Pipeline Operator (`|`) Works Well for Workflows

The pipe operator naturally expresses sequential workflows:
- `ana hot | fix top 3 | mes bef aft` (profile → fix → measure)
- `add col | mig dat | upd qry` (schema → data → code)

This validates Limn's constraint-composition model for orchestration.

### 3. Constraint Operators Enable Embedded Metadata

Limn can embed constraints and metadata directly:
- `inc: cod tes doc` (deliverables list)
- `stp: des cod tes ref` (process steps)
- `chk: tes pas` (validation criterion)
- `cov 80%` (quantitative threshold)

This is more concise than English's prepositional phrases.

### 4. Ordering Ambiguity is a Critical Weakness

**F3 (migration) exposes a danger:** `val | bak` could mean:
- Validate then backup (wrong - too late)
- Backup then validate (correct - safety first)

For workflows with strict ordering requirements (especially safety-critical ones like data migrations), Limn's pipe model is **ambiguous about causality vs sequence**.

**Recommendation:** Limn needs explicit ordering constraints for safety-critical workflows:
- `bak <- mig <- val` (dependencies)
- `[bak, mig, val]` (strict sequence)
- Or rely on context/convention (dangerous)

### 5. Limn Works Best as High-Level Workflow Spec

Limn excels at expressing **what** needs to happen and **in what order**, but often omits:
- Completion criteria
- Error handling details
- Rollback strategies
- Scope constraints

This makes Limn ideal as:
- Input to workflow orchestrators
- Directives for LLM agents
- High-level architecture specifications

But **not** for:
- Executable scripts (too underspecified)
- Safety-critical procedures (ordering ambiguity)

### 6. None of These Are Directly Executable

All 10 tests are marked as **not executable** - they require tooling and interpretation. This is **by design**:
- These are high-level workflows, not bash commands
- They represent *intentions* that need to be executed by agents or orchestration systems
- They serve as **semantic compression for communication**, not direct execution

This validates Limn's use case: **compressing engineering intent for LLM processing**.

## Comparison to Previous Categories

| Category | Focus | Avg Compression | Executability | Limn Success Rate |
|----------|-------|-----------------|---------------|-------------------|
| A | Basic commands | ~1.5:1 | High | 100% |
| C | Code analysis | 1.66:1 | 40% | 100% |
| E | Reasoning tasks | ~1.4:1 | 0% | 80% |
| **F** | **Complex workflows** | **1.85:1** | **0%** | **90%** |

**Key insight:** As task complexity increases, Limn's compression advantage **grows** (1.85:1 for workflows vs 1.5:1 for basic commands), but executability decreases. This suggests Limn is optimized for **expressing complex engineering intent** rather than simple commands.

## Implications for LLM Agent Systems

### Strengths for Agent Orchestration

1. **Compact Specifications:** Workflows fit in smaller context windows
2. **Structured Decomposition:** Pipe operators create natural task graphs
3. **Constraint Embedding:** Metadata and thresholds are inline (e.g., `cov 80%`)
4. **Consistent Vocabulary:** Same words (`tes`, `ver`, `upd`) across workflows

### Challenges

1. **Ordering Ambiguity:** Safety-critical workflows need explicit sequencing
2. **Underspecification:** Agents need to infer details (e.g., what to validate)
3. **Context Dependency:** "X", "A", "B" require shared context
4. **Error Handling:** No standard pattern for rollback or recovery

### Recommended Use Cases

✅ **Good fit:**
- High-level task decomposition for AI agents
- Workflow specifications for orchestrators
- Architectural intent in technical docs
- Semantic search for similar workflows

❌ **Poor fit:**
- Safety-critical procedures (use formal methods)
- Executable scripts (use shell/Python)
- Public-facing documentation (use English)

## Validation Status

**Category F: Complex Workflows - COMPLETE ✓**

- ✅ All 10 test cases documented
- ✅ Compression ratios measured (1.56:1 to 2.27:1)
- ✅ Detailed analysis for each test
- ✅ Summary statistics and findings
- ✅ Comparison to prior categories
- ✅ Implications and recommendations

## Conclusion

**Can Limn handle complex workflows?**

**Yes, with caveats:**

✅ Limn achieves **1.85:1 compression** over English for multi-step workflows
✅ The pipe operator and constraint model work well for expressing task graphs
✅ Limn is ideal for **semantic compression** in agent communication
⚠ Ordering ambiguity is a risk for safety-critical workflows
⚠ Workflows require interpretation - they are not directly executable

**Bottom line:** Limn succeeds as a **high-level workflow specification language** for LLM agents and orchestration systems, achieving substantial compression while preserving essential structure. However, safety-critical workflows may need additional ordering constraints beyond the pipe operator.
