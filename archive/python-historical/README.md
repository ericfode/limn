# Historical Python Code

These files represent early experiments before the Prolog-only policy.

**Status:** DEPRECATED - Not maintained, not canonical
**Purpose:** Historical reference only
**Date archived:** 2026-02-01

All production Limn code is in Prolog. See `/tools` and `/src` for canonical implementations.

## Files

**Interpreters & MCP:**
- `limn_interpreter.py` - Early interpreter prototype
- `limn-mcp-server.py` - MCP server experiment (replaced by tools/mcp-server/limn-mcp.pl)

**Embedding Experiments:**
- `009-validation-greek-vocab.py` - Experiment 009: Greek vocab validation
- `010-validation-eastern-vocab.py` - Experiment 010: Eastern philosophy vocab validation
- `train_limn_embedder.py` - Fine-tuning embeddings for Limn phrases

**Database & Testing:**
- `populate-bootstrap-db.py` - Bootstrap dictionary parser
- `run_tests.py` - Test runner (category B data operations)

**ML Tools (ml-tools/):**
- `semantic_search.py` - ML-based semantic search using ChromaDB. Archived due to Prolog-only policy. If semantic search needed: Use LMN oracle (~) for semantic queries instead.

## Why Prolog?

Limn's objective execution layer requires logic programming. Prolog's unification, constraint solving, and predicate-based reasoning embody the deterministic half of Limn's superposition. The LLM provides subjective interpretation; Prolog provides objective grounding.

See `docs/philosophy/PROLOG-WHY.md` for the full rationale.
