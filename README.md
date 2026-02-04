# Limn

**A constructed language with key-collapsible ambiguity**

*limn (v.): to outline, to illuminate, to draw the boundaries of meaning*

## The Problem

Design a language that is:
- **Plaintext** - grammatically valid, speakable, FCC Part 97 compliant (no codes/ciphers)
- **Maximally ambiguous** to humans without shared context
- **Precisely interpretable** by an LLM with a shared "key"
- **High information density** when sender and receiver share context
- **Bootstrappable** - learnable through in-context examples alone

## Theoretical Foundation

Words define **constraint surfaces** (hyperplanes) in semantic space. A sentence's meaning is the **intersection region** of all word-constraints. Few words = large region (high ambiguity). A "key" provides additional constraints that collapse the region to a specific point.

Analogies:
- Shamir's Secret Sharing (k-of-n planes locate a point)
- Neural network decision boundaries (hyperplane intersections)
- Wyner-Ziv coding (decoder side-information enables compression)

## Empirical Validation

**Experiment 005: Compositional Semantics** ([Full Report](experiments/005-FINAL-REPORT.md) | [Summary](experiments/005-COMPREHENSIVE-SUMMARY.md))

**Key Finding:** Limn phrases are **52% more predictable to language models** than equivalent English expressions.

- **Limn compositionality:** 0.88 mean similarity (embed(A B) ≈ embed(A) + embed(B))
- **English baseline:** 0.58 mean similarity
- **Statistical significance:** p = 0.0059, Cohen's d = 2.06
- **Wins:** 10/11 direct comparisons

**What this means:** Limn's constraint-based design makes it inherently more compositional in LLM embedding space. Language models can better predict the meaning of Limn phrases from their parts compared to natural language, validating Limn's value proposition as an LLM-optimized semantic system.

**Implications:**
- Limn is more efficient for LLM processing (fewer tokens needed for same precision)
- Better suited for semantic search and retrieval
- Natural fit for distributed semantics applications
- Validates constraint-intersection model empirically

## Project Structure

```
limn-land/
├── docs/
│   ├── spec/          # Language specification
│   ├── theory/        # Mathematical foundations
│   └── marketing/     # Campaign materials, manifesto
├── experiments/
│   ├── sentences/     # Test sentences with analysis
│   └── keys/          # Key mechanism experiments
├── crew/
│   ├── student/       # Kira - naive learner perspective
│   ├── linguist/      # Dr. Solvik - formal analysis
│   └── author/        # Yuki - creative stress-testing
├── src/
│   ├── twitter-bots/  # Marketing bot framework
│   └── claude-skill/  # /limn command implementation
└── .beads/            # Issue tracking
```

## Key Documents

**Start here for learning Limn:**

| Document | Purpose |
|----------|---------|
| [Bootstrap v3-Natural](docs/spec/bootstrap-v3-natural.md) | **CANONICAL** - Zero-training bootstrap (validated 77-85% comprehension) - See [docs/BOOTSTRAP.md](docs/BOOTSTRAP.md) for navigation |
| [Vocabulary v3-Natural](docs/spec/vocabulary-v3-natural.md) | Complete word list with etymology |
| [Formal Grammar](docs/spec/grammar-formal.md) | Operator precedence, syntax rules |
| [Liminal Semantics](docs/theory/liminal-semantics.md) | How contradictions resolve |
| [Key Mechanism](docs/theory/key-mechanism.md) | How keys collapse ambiguity |

**For implementers:**

| Document | Purpose |
|----------|---------|
| [LIMN-PL Specification](docs/spec/LIMN-PL-SPECIFICATION.md) | Programming language extension |
| [Operator Interactions](docs/theory/operator-interaction-analysis.md) | How operators combine |
| [Quantifier Semantics](docs/theory/quantifier-semantics.md) | al, ex, on behavior |

**Research & Validation:**

| Document | Purpose |
|----------|---------|
| [Experiment 005 - Final Report](experiments/005-FINAL-REPORT.md) | Compositional semantics study (52% advantage) |
| [Experiment 005 - Comprehensive Summary](experiments/005-COMPREHENSIVE-SUMMARY.md) | All six tests with statistical analysis |
| [Experiment Index](experiments/INDEX.md) | Complete catalog of 49 experiments (2 validated) |

## Vocabulary Database

The vocabulary is stored in a Dolt database, available on DoltHub:

**DoltHub:** https://www.dolthub.com/repositories/ericfode/limn

**For LLMs:** Clone and query the vocabulary directly:
```bash
dolt clone ericfode/limn
cd limn
dolt sql -q "SELECT word, meaning FROM words WHERE domain_id = 5"
```

**Local development:** `data/vocabulary/`
- **Collision prevention** - UNIQUE constraint on word column
- **Queryable vocabulary** - SQL access for tools
- **Version control** - Git-like branching for vocabulary changes

**Quick queries:**
```bash
./scripts/vocab.sh stats           # Vocabulary statistics (938 words, 26 domains, 23 operators)
./scripts/vocab.sh search light    # Search words
./scripts/vocab.sh check xyz       # Check if word available
./scripts/vocab.sh domain 1        # List words in domain
./scripts/vocab.sh operators       # List all operators
./scripts/vocab.sh collisions      # Show resolved collisions
```

**Database is the source of truth:** All vocabulary lookups should query the Dolt database via `vocab.sh`. Markdown documentation may lag behind database updates.

**For vocabulary management:** See [Vocabulary Management Guide](docs/guides/VOCAB-MANAGEMENT.md) for complete guidance on adding words, resolving collisions, and maintaining the database.

## Implementation

**IMPORTANT: Implementation Language Policy**

**Limn uses Prolog exclusively.** All code contributions must be in Prolog.

- ✅ Accepted: Prolog implementations (Scryer Prolog preferred, SWI-Prolog compatible)
- ❌ Not accepted: Python, JavaScript, or other language implementations

**Why Prolog?** Limn's objective execution layer requires logic programming. Prolog's unification, constraint solving, and predicate-based reasoning embody the deterministic half of Limn's superposition. The LLM provides subjective interpretation; Prolog provides objective grounding.

**Running Limn code:**
```bash
swipl -s tools/lmn/lmn_runtime.pl -g "eval_file('examples/addition.lmn')"
```

**For developers:** See [CONTRIBUTING.md](CONTRIBUTING.md) for implementation guidelines and [docs/philosophy/PROLOG-WHY.md](docs/philosophy/PROLOG-WHY.md) for the philosophical rationale.

## Phases

1. **Theoretical Foundation** - Formalize the math
2. **Core Language Design** - Vocabulary, grammar, keys
3. **Experimental Validation** - Test the properties empirically
4. **Deliverables** - Spec, bootstrap prompt, test cases
5. **Order-Independent Programming Language** - The ultimate application
6. **Marketing & Launch** - Build mystery, grow community

## The Crew

Three perspectives constantly experimenting with the language:

- **Kira** (student) - Learns from scratch, finds confusions, tests bootstrappability
- **Dr. Maren Solvik** (linguist) - Formal analysis, typology, linguistic coherence
- **Yuki Tanaka-Morrison** (author) - Stories, poetry, narrative stress-testing

## Commands

```bash
bd ready              # See available work
bd list --status=open # All open issues
gt sling <bead> .     # Dispatch work to a polecat
/limn interpret <sentence>  # Interpret Limn with Claude
```

## Ultimate Goal

An **order-independent programming language** where statements are constraints and program meaning is their intersection - computation as geometry.

## Why "Limn"?

The name captures the essence of the language:
- **To limn** = to outline, to draw boundaries around meaning
- Evokes **liminal** = threshold states, in-between spaces
- 4 letters, one syllable, memorable
- The language limns regions of meaning-space until a key illuminates the specific point

---

*sol liq tra | key = wh? | mea = yo*

*Solid, liquid, transformation. Key is: what? Meaning is: yours to decide.*
