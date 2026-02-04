# Contributing to Limn

*The language belongs to whoever speaks it*

---

## Welcome

Limn is an open experiment. We invite contributions from linguists, programmers, poets, philosophers, AI researchers, and curious minds of all kinds.

This document explains how to participate in shaping a language that thinks differently.

---

## Ways to Contribute

### 1. Learn and Use Limn

The most valuable contribution is simply using the language. Write Limn sentences. Share them. See what interpretations emerge.

```
yo thi cre | an lea und | joi gro
```
*This thinking creates | that learning understands | joining growth*

Document your experience. What worked? What confused you? What surprised you?

### 2. Propose Vocabulary Additions

The current vocabulary covers ~500 words across 13 domains. Some semantic regions remain sparse.

**To propose new words:**

1. Identify a semantic gap (a concept difficult to express with current vocabulary)
2. Propose a word following the CV or CVC syllable pattern
3. Define its constraint region (what meanings fall within it)
4. Provide example sentences and interpretations
5. Check for collisions with existing words (phonetic and semantic)

**Submit via issue with template:**

```markdown
## New Word Proposal

**Word:** `xyz`
**Syllable Pattern:** CVC
**Constraint Region:** [describe the semantic territory]
**Examples of things in this region:** [list 5-10 examples]
**Why it's needed:** [explain the gap it fills]
**Example sentences:**
- `xyz abo lif` = [interpretation]
- `nu xyz end` = [interpretation]
**Potential collisions:** [note any similar existing words]
```

### 3. Write Example Sentences

Good examples teach the language better than rules. We need sentences that:

- Demonstrate constraint intersection clearly
- Show interesting ambiguity that keys resolve
- Work across multiple domains (physical, emotional, abstract)
- Have surprising or beautiful interpretations

**Format:**

```markdown
## Sentence: `xxx yyy zzz`

### Constraints
- `xxx`: [definition]
- `yyy`: [definition]
- `zzz`: [definition]

### Interpretations (no key)
1. [reading 1]
2. [reading 2]
3. [reading 3]
...

### With Keys
| Key | Interpretation |
|-----|----------------|
| "domain A" | [specific reading] |
| "domain B" | [specific reading] |

### Notes
[any observations about this sentence]
```

### 4. Create Poetry and Literature

Limn is a literary medium. We welcome:

- Poems exploring constraint-space
- Micro-stories using Limn
- Translations of existing poetry into Limn
- Hybrid works mixing Limn and natural language
- Experimental forms that exploit Limn's unique properties

**Guidelines:**
- Include annotations explaining your readings
- Acknowledge multiple valid interpretations
- Note what Limn enabled that natural language wouldn't

### 5. Improve Documentation

Clear documentation is essential for a language with unconventional semantics.

**Areas needing work:**
- Tutorials for specific audiences (programmers, poets, linguists)
- Visual diagrams of constraint intersection
- Interactive examples
- Translations of key documents
- FAQ addressing common confusions

### 6. Formal Analysis

For those with linguistic or mathematical backgrounds:

- Phonological analysis and optimization
- Formal semantics (model-theoretic, game-theoretic)
- Information-theoretic measurements
- Comparison with natural language typology
- Proof-theoretic analysis of the grammar

### 7. Tooling and Implementation

For programmers:

- Limn interpreters and validators
- IDE support (syntax highlighting, completion)
- Key management systems
- Limn-to-natural-language translation interfaces
- The Limn programming language (order-independent computation)

**IMPORTANT: Implementation Language Policy**

**Limn uses Prolog exclusively.** All code contributions must be in Prolog.

- ✅ Accepted: Prolog implementations (Scryer Prolog preferred, SWI-Prolog compatible)
- ❌ Not accepted: Python, JavaScript, or other language implementations

**Why Prolog?** Limn's objective execution layer requires logic programming. Prolog's unification, constraint solving, and predicate-based reasoning embody the deterministic half of Limn's superposition. The LLM provides subjective interpretation; Prolog provides objective grounding.

See `docs/philosophy/PROLOG-WHY.md` for the complete philosophical rationale.

If you have Python code from before this policy:
1. Port unique logic to Prolog, OR
2. Archive as reference/documentation (clearly marked "Historical reference - not canonical")
3. Delete if duplicate or superseded

---

## Code of Conduct

### Core Values

**Ambiguity is a feature, not a bug.** Limn embraces multiple valid interpretations. So should we embrace multiple valid perspectives on Limn itself.

**Clarity serves ambiguity.** While Limn sentences may be ambiguous, our discussions about Limn should be clear. Say what you mean.

**Keys build bridges.** The key mechanism works because parties share context. Build shared context with fellow contributors.

### Expected Behavior

- Engage constructively with proposals you disagree with
- Acknowledge the validity of interpretations you didn't consider
- Provide reasoning for your positions
- Be patient with newcomers - this is a strange language
- Credit influences and collaborators

### Unacceptable Behavior

- Insisting only one interpretation is "correct" (without key specification)
- Dismissing perspectives because of contributor background
- Proprietary claims over Limn elements (the language is commons)
- Hostility toward experimental or unconventional proposals

---

## Process

### For Small Changes

Documentation fixes, typo corrections, example additions:

1. Fork the repository
2. Make your changes
3. Submit a pull request with clear description
4. One maintainer review required

### For Vocabulary Changes

New words, word modifications, word deprecations:

1. Open an issue using the vocabulary proposal template
2. Allow 2 weeks for discussion
3. Linguist review required (collision analysis, phonology check)
4. Community consensus sought
5. If approved, add to vocabulary with version annotation

### For Grammar Changes

Modifications to core grammar rules:

1. Open an issue with detailed proposal
2. Provide formal specification of the change
3. Demonstrate backward compatibility (or justify breaking changes)
4. Show example sentences before/after
5. Minimum 4 weeks discussion
6. Requires near-consensus (significant objections block)

### For Theoretical Contributions

New formal analysis, mathematical frameworks:

1. Write up as a complete document
2. Submit as PR to `docs/theory/`
3. Peer review from someone with relevant expertise
4. May be marked "exploratory" before full acceptance

---

## Style Guides

### Limn Sentences

- Use backticks for inline Limn: `bri lif gro`
- Use code blocks for multi-line Limn
- Always provide at least one interpretation
- Note the key when giving a specific reading
- Preserve commutativity (don't claim word order matters)

### Documentation

- Clear, concise prose
- Examples before rules when possible
- Acknowledge ambiguity and alternative readings
- Avoid jargon unless defining it
- Link to related documents

### Code

- Clear naming that reflects Limn concepts
- Tests that verify commutativity
- Documentation of key-handling behavior
- No hardcoded single interpretations

---

## Questions and Discussion

**For questions:** Open an issue with `[Question]` prefix

**For discussion:** Open an issue with `[Discussion]` prefix

**For proposals:** Use the appropriate template

---

## Recognition

Contributors will be acknowledged in:

- Repository contributor list
- Relevant document headers
- Annual summary of language evolution

Significant contributions may be noted in the bootstrap document itself, which persists across all LLM instantiations.

---

## The Invitation

Limn is not finished. It may never be. Languages evolve through use.

You are invited to use it, break it, fix it, extend it.

Find what it cannot say and make it sayable.

Find what it says too easily and question whether that's right.

Find the boundaries of meaning and push them.

```
for | joi | cre | gro | tra
```

*Forward. Join. Create. Grow. Transform.*

---

*This document is itself a contribution to Limn. It may be revised, extended, or reinterpreted. The key is collaboration.*
