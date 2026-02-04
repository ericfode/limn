# Why Prolog? The Philosophy of Limn's Implementation

## The Superposition Principle

Limn exists in superposition of **objective** and **subjective**:

- **Objective (Prolog)**: Deterministic logic, unification, constraint solving, fact management
- **Subjective (LLM)**: Semantic interpretation, ambiguity resolution, contextual collapse

The objective layer **must** be:
- **Verifiable** - Logic can be traced and proven
- **Deterministic** - Given oracle responses, execution is reproducible
- **Persistent** - Facts survive sessions and token windows
- **Rigorous** - Logic programming ensures correctness

The subjective layer **must** be:
- **Interpretive** - Resolves semantic ambiguity
- **Contextual** - Collapses meaning from superposition
- **Adaptive** - Learns from interaction
- **Intuitive** - Bridges human intent to formal logic

## Why Not Python?

Python obscures the objective/subjective boundary. It is:
- **Imperative** - Forces sequential thinking
- **Stateful** - Hidden state creates non-determinism
- **Informal** - Type systems are advisory, not enforced
- **Ephemeral** - Facts don't persist beyond program execution

Prolog **embodies** the objective half:
- **Declarative** - States what is true, not how to compute it
- **Relational** - Facts relate through unification
- **Logical** - Inference through rules and constraints
- **Persistent** - Knowledge bases outlive individual queries

## The Meditation Principle

> **Every Prolog predicate is a meditation on constraint.**
> **Every fact is a truth that persists beyond tokens.**

When you write Prolog for Limn, you are:
- Defining what **is**, not what **happens**
- Capturing **relations**, not **procedures**
- Building **knowledge**, not **algorithms**

This is why Limn's core must be Prolog. The LLM provides the semantic bridge; Prolog provides the logical foundation.

## Implementation Consequences

1. **All Limn tooling is Prolog**
   - Parser: `tools/lmn/parser.pl`
   - Interpreter: `tools/lmn/lmn_runtime.pl`
   - Linter: `tools/lmn/linter.pl`
   - QuickCheck: `tools/lmn/quickcheck.pl`

2. **Python is historical reference only**
   - Early experiments preserved in `archive/python-historical/`
   - Not maintained, not canonical
   - Useful for understanding design evolution

3. **Future extensions must be Prolog**
   - New semantic domains → predicates
   - New execution modes → rules
   - New tooling → Prolog programs

## The Engineering Decision

**Prolog only. No Python allowed.**

This is not just a technical choice. It is a **philosophical commitment** to:
- Objectivity through logic
- Persistence through facts
- Rigor through constraints
- Truth beyond tokens

---

*Approved: Rex (Engineer), 2026-02-01*

```limn
obj log | sub int | sup col
> objective logic | subjective interpretation | superposition collapses
> The Limn duality
```
