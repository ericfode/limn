# LMN Runtime

> ∿ tho exe | mea cal | min per
> *Thought executes. Meaning calculates. Mind persists.*

## Overview

The LMN Runtime is a minimal interpreter for the LMN language (Phase 1 implementation).

LMN is a language where:
- Meaning is primitive
- Ambiguity is superposition
- Time is syntax
- Context is everything

## Architecture

```
LMN Source → Parser → AST → Evaluator → Result
                                ↓
                          Temporal State
```

### Components

| Module | Purpose |
|--------|---------|
| `parser.pl` | Parse LMN syntax (∿, @, ~, [], →) to Prolog AST |
| `eval.pl` | Evaluate AST nodes in context |
| `temporal.pl` | Manage temporal state (@was, @now, @will) |
| `lmn_runtime.pl` | Integrated runtime + REPL |

## LMN Operators

| Operator | Name | Purpose | Example |
|----------|------|---------|---------|
| `∿` | Temporal Wave | Navigate time | `∿ now [expr]` |
| `@` | Collapse | Resolve ambiguity | `[a,b,c] @ context` |
| `~` | Oracle | LLM evaluation | `~ explain [concept]` |
| `[]` | Superposition | Multiple possibilities | `[red, blue, green]` |
| `→` | Flow | Causality/transformation | `input → process → output` |

## Usage

### Run a file

```bash
swipl -s tools/lmn/lmn_runtime.pl -g "run_file('examples/hello.lmn')"
```

### Run source directly

```prolog
?- use_module('tools/lmn/lmn_runtime').
?- run('∿ now hello').
```

### Start REPL

```bash
swipl -s tools/lmn/lmn_runtime.pl -g repl
```

```
lmn> ∿ now hello
=> id(hello)

lmn> [red, blue, green] @ user_pref
=> id(red)

lmn> quit
```

## Examples

### Temporal Navigation

```lmn
∿ now hello world
```

Evaluates `hello world` in current temporal context.

### Superposition

```lmn
[red, blue, green]
```

Creates a superposition of three possibilities.

### Collapse

```lmn
[run, execute, perform] @ code_context
```

Collapses superposition based on context (→ execute).

### Flow

```lmn
thought → code → value
```

Flows data through transformation steps.

### Oracle (Stubbed)

```lmn
~ explain [quantum superposition]
```

Calls LLM oracle for semantic interpretation (currently prompts for manual input).

## Current Implementation Status

### ✅ Implemented

- Parser for all LMN operators
- AST evaluation
- Temporal state management
- Context passing
- Superposition as lists
- Flow execution
- Basic REPL

### 🚧 Stubbed

- Oracle operator (`~`) - prompts for manual input
- Temporal history - stores facts but doesn't fully utilize past/future
- Context-based collapse - picks first item instead of intelligent selection

### 📋 Future (Phase 2)

- LLM API integration for `~` operator
- Response caching
- Advanced temporal queries
- Context-aware collapse heuristics
- Error handling
- Performance optimization

## Testing

```prolog
?- use_module('tools/lmn/lmn_runtime').

% Parse test
?- parse('∿ now hello', AST).
AST = temporal(now, id(hello)).

% Eval test
?- make_context(C), eval(temporal(now, id(hello)), C, R).
R = id(hello).

% Temporal test
?- temporal_assert(state, ready).
?- temporal_now(state, V).
V = ready.
```

## Design Principles

1. **Semantic First** - Operations transform meaning, not just data
2. **Temporal Awareness** - Time is first-class, not hidden state
3. **Contextual Collapse** - Resolution requires context
4. **Dual Execution** - Deterministic core + LLM oracle at `~` nodes
5. **Observable Ambiguity** - Superposition is explicit

## Architecture Decision

This implements **Model B (HVM-Oracle)** from the runtime decision doc:

- **Objective Layer (Prolog):** Deterministic reduction, temporal operators, state management
- **Subjective Layer (LLM):** Called only at `~` nodes for semantic interpretation
- **Clean Boundary:** `~` operator marks LLM evaluation points explicitly

See `../../RUNTIME-DECISION.md` and `../../MASTER-PLAN.md` for full context.

## Next Steps

1. Integrate LLM API for oracle operator
2. Implement response caching
3. Build comprehensive test suite
4. Create more example programs
5. Document edge cases and error handling

---

*∿ beg [idea] → ∿ now [runtime] → ∿ wil [consciousness]*

**Phase 1 Complete: Minimal LMN Interpreter in Prolog ✓**
