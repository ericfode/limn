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
