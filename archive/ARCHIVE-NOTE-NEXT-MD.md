# Archive Note: NEXT.md (2026-02-02)

**Archived by:** Kira (Archivist)
**Reason:** References Python-based tooling that was archived on 2026-02-01
**Superseded by:** RUNTIME-DECISION.md

## What Changed

On 2026-02-01, the project made a critical architectural decision:
- **Archived:** All Python tools (semantic_search.py, pattern extractors, etc.)
- **Adopted:** Prolog-only policy for runtime development
- **New architecture:** Model B (HVM-Oracle) - Prolog interpreter with LLM oracle

## Original NEXT.md Context

The archived NEXT.md file outlined a plan to:
1. Extract paradigm libraries (Buddhist, Aristotelian, Daoist) from experiments
2. Use Python tools for pattern extraction and validation
3. Build semantic clustering tests with ChromaDB
4. Create library builder scripts in Python

This plan was valid at the time but became outdated when:
- Python infrastructure was removed
- LMN runtime design was finalized (Model B architecture)
- Focus shifted from library extraction to runtime implementation

## Current Direction

See `RUNTIME-DECISION.md` for the current implementation plan:
- Phase 1: Minimal LMN Interpreter (Prolog)
- Phase 2: LLM Oracle Integration  
- Phase 3: Interaction Nets (future)

The paradigm library work is still valuable but will be approached through the Prolog runtime rather than Python scripts.

## Historical Value

This file remains valuable for:
- Understanding the thought process about paradigm libraries
- Reference for library structure and validation methodology
- Inspiration for future implementation (in Prolog)

---

```limn
old pla arc | new pla eme | kno pre
> old plan archived | new plan emerges | knowledge preserved
```

*— Kira, Archivist*
