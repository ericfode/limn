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
| [Bootstrap v3-Natural](docs/spec/bootstrap-v3-natural.md) | Zero-training bootstrap prompt |
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

## Vocabulary Database

The vocabulary is stored in a Dolt database at `data/vocabulary/` for:
- **Collision prevention** - UNIQUE constraint on word column
- **Queryable vocabulary** - SQL access for tools
- **Version control** - Git-like branching for vocabulary changes

**Quick queries:**
```bash
./scripts/vocab.sh stats           # Vocabulary statistics
./scripts/vocab.sh search light    # Search words
./scripts/vocab.sh check xyz       # Check if word available
./scripts/vocab.sh domain 1        # List words in domain
./scripts/vocab.sh operators       # List all operators
./scripts/vocab.sh collisions      # Show resolved collisions
```

**Direct SQL:**
```bash
dolt sql -q "SELECT word, meaning FROM words WHERE domain_id = 5"
```

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
