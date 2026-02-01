# Wikipedia Article Draft: Limn (Constructed Language)

**Status:** Draft - Ready for Wikipedia submission
**Prepared:** 2026-02-01
**Target:** Wikipedia article creation

---

## Article Text

### Limn (constructed language)

**Limn** is a constructed language designed for communication between language models and humans with shared context. Created in 2025-2026, the language uses constraint-based semantics where words define constraint surfaces in semantic space, and meaning emerges from their intersection.

#### Design Principles

Limn was designed with several key properties:

- **Key-collapsible ambiguity**: Sentences are maximally ambiguous to humans without shared context but precisely interpretable with a "key" that provides additional constraints
- **Plaintext compliance**: Grammatically valid natural language, compliant with FCC Part 97 regulations (no codes or ciphers)
- **Zero-bootstrap learnability**: Learnable through in-context examples alone, without formal training
- **High information density**: Efficient communication when sender and receiver share context
- **Compositional structure**: Strong semantic compositionality validated through empirical testing

#### Theoretical Foundation

The language is based on the mathematical concept of constraint surfaces (hyperplanes) in semantic space. Each word defines a constraint, and a sentence's meaning is the intersection region of all word-constraints. With few words, this creates a large region of possible meanings (high ambiguity). A "key"—additional contextual constraints—collapses this region to a specific point, enabling precise interpretation.

This approach draws analogies from:
- **Shamir's Secret Sharing**: k-of-n planes locating a point in space
- **Neural network decision boundaries**: Hyperplane intersections defining classification regions
- **Wyner-Ziv coding**: Decoder side-information enabling compression

#### Vocabulary

Limn's vocabulary follows a "natural extensions" principle designed for intuitive learning:

- **3-letter CVC pattern**: Most words follow Consonant-Vowel-Consonant structure (e.g., "sol" for solid, "liq" for liquid)
- **First-syllable extraction**: Words derived from first syllable of source language (e.g., "fir" from "fire")
- **Latin/Greek transparency**: Etymology from classical languages for universal recognition (e.g., "pyr" from Greek πῦρ for fire)
- **Phonaesthetic fit**: Sound patterns matching semantic meaning

As of January 2026, the vocabulary contains 784 words organized into 26 semantic domains, stored in a Dolt database with version control.

#### Grammar

Limn uses a minimalist grammar with:

- **Operators**: Unary operators (nu=not, ve=very), quantifiers (al=all, ex=some), comparators (mi=less, ma=more)
- **Scope markers**: Pipe symbol (|) defines operator and phrase boundaries
- **Juxtaposition**: Word order flexible; meaning emerges from constraint intersection
- **Liminal semantics**: Contradictions resolve to intermediate states (e.g., "hot cold between" = lukewarm)

A programming language extension, **Limn-PL**, adds variables, constraints, functions, and package management for declarative computation.

#### Empirical Validation

In January 2026, researchers conducted Experiment 005, measuring compositional semantics in embedding space. Results showed:

- **52% higher compositionality** in Limn phrases compared to equivalent English expressions
- Mean embedding similarity of 0.88 for Limn (vs 0.58 for English)
- Statistical significance: p = 0.0059, Cohen's d = 2.06
- 10/11 direct comparisons favored Limn

Zero-bootstrap validation demonstrated 77-85% comprehension of Limn sentences from bootstrap document alone, without prior training or vocabulary lookup.

#### Applications

The language's design targets several use cases:

- **LLM-optimized communication**: More efficient token usage and semantic clarity
- **Distributed semantics**: Suitable for semantic search and retrieval systems
- **Order-independent programming**: Limn-PL enables constraint-based computation
- **Semantic compression**: High information density with shared context

#### Etymology

The name "limn" (verb) means "to outline, to illuminate, to draw the boundaries of meaning." It evokes "liminal" (threshold states, in-between spaces), reflecting how the language defines regions of semantic space that collapse to specific meanings when context is provided.

#### See Also

- Constructed language
- Engineered language
- Logic programming
- Constraint satisfaction
- Semantic web

#### External Links

- [Limn Repository on DoltHub](https://www.dolthub.com/repositories/ericfode/limn) - Vocabulary database
- [Limn GitHub Repository](https://github.com/ericfode/limn) - Language specification and tools

#### References

1. Fode, Eric (2026). "Experiment 005: Compositional Semantics in Limn vs English". Limn Language Project.
2. Fode, Eric (2026). "Zero-Bootstrap Validation: Learning Limn Without Training". Limn Language Project.
3. Fode, Eric (2026). "Bootstrap v3-Natural: Zero-Training Language Acquisition". Limn Language Project.
4. Fode, Eric (2026). "LIMN-PL Specification: Order-Independent Programming Language". Limn Language Project.

---

## Wikipedia Submission Notes

### Article Standards

This draft follows Wikipedia guidelines:

- **Notability**: Empirically validated constructed language with published experiments
- **Citations**: References to primary sources (experiments, specifications)
- **Neutral POV**: Objective description of design, validation, and applications
- **Verifiability**: All claims supported by accessible documentation
- **No original research**: Documents existing published work

### Categories

Suggested Wikipedia categories:
- Category:Constructed languages
- Category:Engineered languages
- Category:Logic programming languages
- Category:2026 introductions

### Potential Issues

**Coverage concern**: Limited independent sources (primary sources only)
- **Mitigation**: Focus on verifiable technical properties and empirical validation
- **Future**: Await independent academic coverage or conference presentations

**Notability concern**: New language (2025-2026)
- **Mitigation**: Empirical validation (Experiment 005), novel theoretical foundation
- **Future**: Publication in conlang journals or linguistics conferences

### Submission Strategy

1. **Wait for external validation**: Consider waiting for conference presentation or independent coverage
2. **Start with draft**: Submit to Wikipedia draft space for community feedback
3. **Engage WikiProject**: Contact WikiProject Constructed Languages for guidance
4. **Build gradually**: Start with stub, expand as more independent sources emerge

### Alternative: Conlang Wiki

If Wikipedia notability standards not met, consider:
- **Fandom Conlang Wiki**: More permissive inclusion criteria
- **Build credibility first**: Publish papers, present at conferences
- **Return to Wikipedia**: Resubmit after establishing external sources

---

## Ready for Human Execution

**Human action required:**
1. Create Wikipedia editor account (if needed)
2. Review Wikipedia's notability guidelines for constructed languages
3. Consider starting in draft space
4. Submit article for review
5. Respond to community feedback

**Content prepared:** ✓ Article text ready
**Citations:** ✓ References included
**External links:** ✓ DoltHub and GitHub links
**Categories:** ✓ Suggested categories listed

---

*Prepared by Kira (Archivist) for limn-rqn2*
