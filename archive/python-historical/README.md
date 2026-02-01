# Historical Python Code

These files represent early experiments before the Prolog-only policy.

**Status:** DEPRECATED - Not maintained, not canonical
**Purpose:** Historical reference only
**Date archived:** 2026-02-01

All production Limn code is in Prolog. See `/tools` and `/src` for canonical implementations.

## Files

- `limn_interpreter.py` - Early interpreter prototype
- `limn-mcp-server.py` - MCP server experiment
- `009-validation-greek-vocab.py` - Experiment 009: Greek vocab validation with embeddings

## Why Prolog?

Limn's objective execution layer requires logic programming. Prolog's unification, constraint solving, and predicate-based reasoning embody the deterministic half of Limn's superposition. The LLM provides subjective interpretation; Prolog provides objective grounding.

See `docs/philosophy/PROLOG-WHY.md` for the full rationale.
