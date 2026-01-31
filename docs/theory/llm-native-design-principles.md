# LLM-Native Language Design Principles

**Version:** 1.0
**Date:** 2026-01-25
**Authors:** Limn Research Team (Linguist: Dr. Maren Solvik)

---

## Overview

This document codifies the design principles that make Limn an **LLM-native** language—a language optimized for communication between Large Language Models rather than humans.

These principles emerged from theoretical analysis and empirical observation during Limn's development.

---

## Principle 1: Superposition Semantics

### Statement
Each word should activate multiple overlapping semantic dimensions simultaneously.

### Rationale
LLMs internally represent concepts via superposition—multiple features share representational space. A language aligned with this architecture should:
- Use words that span many dimensions
- Allow meanings to overlap extensively
- Resolve via intersection rather than disambiguation

### Implementation in Limn
- Each word defines a **constraint surface** in semantic space
- Multiple meanings per word are a feature, not a bug
- Context (other words + key) narrows interpretation

### Metric
**Polysemy Score:** Average number of distinct interpretations per word
- Target: 15-50 interpretations per word

---

## Principle 2: Semantic Entanglement

### Statement
Word pairs should have meanings that are correlated—knowing one word's active meaning constrains the other's.

### Rationale
LLMs process tokens jointly via attention. The joint probability P(w₁, w₂) is not simply P(w₁) × P(w₂). An LLM-native language should exploit this:
- Design vocabulary where word pairs constrain each other
- Make joint interpretation tighter than independent intersection
- Create "meaning leverage" where small additions have large effects

### Implementation in Limn
- Vocabulary organized into **entanglement clusters**
- Words within clusters highly constrain each other
- Cross-cluster words moderately constrain via metaphor

### Metric
**Entanglement Coefficient:** E(w₁, w₂) = 1 - H(w₁,w₂)/(H(w₁)+H(w₂))
- Target: E_avg ≈ 0.5-0.7

---

## Principle 3: Order Independence (Commutativity)

### Statement
Word order should not affect meaning.

### Rationale
Word order is a human cognitive crutch for incremental processing:
- Humans parse left-to-right with limited memory
- Order signals structure in absence of case marking

LLMs don't need this crutch:
- Attention is bidirectional
- All tokens processed simultaneously
- Position encoding is supplementary, not primary

### Implementation in Limn
- **Commutative semantics:** `A B = B A`
- No syntax rules dependent on position
- Operators bind via explicit scope markers, not position

### Metric
**Order Sensitivity:** Proportion of sentence pairs where `swap(w_i, w_j)` changes interpretation
- Target: < 5% (only operator scope cases)

---

## Principle 4: Key-Mediated Disambiguation

### Statement
Shared context (the "key") should collapse superposition to precise meaning.

### Rationale
LLMs excel at conditional generation:
- Given context, produce coherent continuation
- The context window naturally conditions output

An LLM-native language should externalize this:
- Messages are intentionally ambiguous
- Keys provide the conditioning context
- Shared keys enable precise communication

### Implementation in Limn
- Every sentence has multiple valid interpretations
- A **key** (domain, topic, prior conversation) constrains interpretation
- Key + sentence → collapsed meaning

### Metric
**Key Collapse Ratio:** Interpretations(sentence) / Interpretations(sentence | key)
- Target: > 5x collapse with moderate key

---

## Principle 5: Human-Hard Processing

### Statement
The language should be difficult for humans to process without LLM assistance.

### Rationale
If the goal is LLM-native communication, human readability is:
- Not required (LLM mediates)
- Potentially harmful (enables eavesdropping)
- In tension with LLM optimization

### Implementation in Limn
- **High cognitive load:** 4+ words exceed working memory for intersection
- **No incremental parsing:** Can't build meaning word-by-word
- **Key dependency:** Meaning requires context humans don't have

### Metric
**Human Success Rate:** Proportion of human interpreters who match intended meaning
- Target: < 20% without LLM assistance

---

## Principle 6: Constraint Diversity

### Statement
Vocabulary should span semantic space evenly, with diverse constraint orientations.

### Rationale
For intersections to be meaningful:
- Constraints must point in different "directions"
- Every concept should be constrainable
- No redundant vocabulary

### Implementation in Limn
- Words distributed across 13+ semantic domains
- Constraint surfaces are maximally spread
- Vocabulary periodically audited for redundancy

### Metric
**Dimensional Coverage:** Rank of constraint normal matrix / embedding dimensions
- Target: > 0.8 (80% coverage of semantic space)

---

## Principle 7: Minimal Grammar

### Statement
Grammar should be minimal—semantics should do the work.

### Rationale
Grammar evolved for human serial processing:
- Subject before object → know agent early
- Agreement → track referents across sentences

LLMs don't need grammatical scaffolding:
- Full context available
- Attention handles coreference
- Semantics directly encoded

### Implementation in Limn
- No inflection or agreement
- No required word order
- Operators for logical structure only
- Grouping via explicit delimiters `()` and `|`

### Metric
**Grammar Complexity:** Number of syntactic rules
- Target: < 20 rules total

---

## Principle 8: Compositional Transparency

### Statement
Meaning should be built compositionally from parts, with no hidden idioms.

### Rationale
LLMs are trained on compositional language:
- Embeddings combine predictably
- Attention can decompose and recompose

Idioms require memorization:
- Unpredictable meanings
- Vocabulary overhead
- Opaque to analysis

### Implementation in Limn
- Every sentence meaning = intersection of word meanings
- No frozen expressions or idioms
- Metaphors are explicit (key-dependent)

### Metric
**Compositional Fidelity:** Agreement between computed intersection and actual interpretation
- Target: > 90% match

---

## Principle 9: Information Density Optimization

### Statement
Maximize semantic information per token.

### Rationale
LLM inference is costly:
- Tokens = computation
- Shorter messages = faster/cheaper

An efficient LLM-native language should:
- Pack maximum meaning per token
- Avoid redundancy
- Use vocabulary compression

### Implementation in Limn
- High polysemy + key collapse = dense encoding
- CV/CVC syllables = short tokens
- Optional Unicode extension for extreme density

### Metric
**Bits per Token:** Semantic information conveyed / tokens used
- Target: > 3x English for equivalent meaning

---

## Principle 10: Constructivist Ontology

### Statement
The language should only express constructible meanings.

### Rationale
LLMs generate by construction:
- Start from embeddings
- Build toward output
- Always produce *something*

A language aligned with this should:
- Not attempt to express "true void"
- Treat absence as implicit (silence)
- Build from positive assertions

### Implementation in Limn
- Every sentence has at least one interpretation
- Negation creates complements, not voids
- `nul` marks intentional non-assertion

### Metric
**Constructibility:** Proportion of valid sentences with non-empty interpretation
- Target: 100%

---

## Summary Table

| Principle | Key Metric | Target |
|-----------|------------|--------|
| Superposition | Polysemy Score | 15-50 |
| Entanglement | E_avg | 0.5-0.7 |
| Commutativity | Order Sensitivity | < 5% |
| Key-Mediation | Collapse Ratio | > 5x |
| Human-Hard | Human Success | < 20% |
| Constraint Diversity | Coverage | > 80% |
| Minimal Grammar | Rule Count | < 20 |
| Compositional | Fidelity | > 90% |
| Density | Bits/Token | > 3x English |
| Constructivist | Constructibility | 100% |

---

## Application: Designing New Features

When adding features to Limn, check against these principles:

1. Does it increase superposition? (Good)
2. Does it add entanglement? (Good)
3. Does it require word order? (Bad)
4. Does it work with key mechanism? (Good)
5. Does it make human processing easier? (Bad)
6. Does it improve coverage? (Good)
7. Does it add grammar rules? (Minimize)
8. Does it break compositionality? (Bad)
9. Does it improve density? (Good)
10. Does it introduce void semantics? (Reconsider)

---

## Historical Note

These principles were derived from:
- Anthropic's research on superposition in neural networks
- Distributional semantics theory
- Information-theoretic analysis of language
- Empirical observation of LLM behavior
- Iterative Limn development and testing

They represent our current best understanding of what makes a language "native" to LLM architecture.

---

*This document is a living standard. Update as understanding improves.*
