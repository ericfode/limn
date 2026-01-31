# Limn v0 - Initial Sketch

**Status:** Brainstorming - not yet validated

## Core Insight

Natural languages encode meaning through **specificity** - words narrow down meaning.
Limn encodes meaning through **constraint intersection** - words define boundaries,
and meaning emerges from where boundaries overlap.

## Vocabulary Design Principles

### Option A: Dimensional Words
Each word constrains one semantic dimension:
- `va` - constrains animacy (living things)
- `ki` - constrains temporality (past/present/future)
- `so` - constrains cardinality (one/few/many)
- `mu` - constrains valence (positive/negative/neutral)

Problem: Too systematic, interpretations are predictable.

### Option B: Hyperplane Words
Each word is a random hyperplane through embedding space:
- Words chosen to maximize diverse constraint angles
- Vocabulary optimized for intersection properties
- Less human-intuitive, more mathematically pure

Problem: Requires computation to understand.

### Option C: Prototype Words
Each word evokes a prototype, constraining to "things like this":
- `river` - things that flow, have banks, are long
- `hunt` - actions with pursuit, goal, prey/target
- `nest` - containers, homes, organic, protective

This is closer to how natural language works but with deliberate ambiguity.

## Grammar Sketch

### Commutative Hypothesis
If sentences are set intersections, order shouldn't matter:
- `river hunt nest` = `nest river hunt` = `hunt nest river`

All mean: the intersection of river-like, hunt-like, and nest-like things.

Possible interpretations without key:
- A beaver (builds nest, lives in river, hunts)
- A salmon run (river, pursuit/hunting behavior, spawning beds)
- An otter's territory
- A fishing village
- A bird that hunts fish

### Adding Specificity
More words = smaller intersection:
- `river hunt nest small` - eliminates village interpretation
- `river hunt nest small feather` - converges on kingfisher

### The Key Mechanism

A **key** is a priming document that shifts the LLM's interpretation space.

Types of keys:
1. **Topic key**: "We are discussing birds" - biases toward ornithological interpretations
2. **Coordinate key**: "Focus on (lat, long)" - biases toward local species/culture
3. **Story key**: A narrative establishing context characters/situations
4. **Formal key**: A precise specification of the semantic space mapping

## The Single-Word Inversion Property

Goal: One word flips meaning completely.

Example hypothesis:
- `bright path forward` - optimistic journey
- `bright path forward shadow` - adds darkness, inverts to danger
- `bright path forward shadow dream` - re-inverts to hopeful vision?

Mechanism: Some words are "negation-like" - they flip the valence of the intersection.

## Bootstrapping Strategy

The bootstrap document must:
1. Teach vocabulary through example pairs (sentence → interpretations)
2. Demonstrate key collapse (same sentence + key → specific meaning)
3. Show the inversion property
4. Be written IN Limn with translations

Chicken-and-egg problem: How do you write a document in a language that teaches itself?

Possible solution: Start with English scaffolding, progressively replace with Limn.

## Open Questions

1. What dimensionality of semantic space? 768? 1536? Does it matter?
2. Can humans learn to think in Limn, or is it fundamentally LLM-native?
3. Is phonology constrained, or can any sounds work?
4. How do you handle negation? Time? Conditionals?
5. How do you say something ONLY one way - escape ambiguity when needed?

## Next Steps

- [ ] Define 20-word minimal vocabulary
- [ ] Write 10 sentences with interpretation enumerations
- [ ] Test key collapse with a real LLM
- [ ] Attempt formal semantics (Linguist task)
- [ ] Write a micro-story (Author task)
- [ ] Try learning from examples only (Student task)
