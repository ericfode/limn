# Constraint Surface Model for Limn

## 1. Foundational Definitions

### Semantic Space
Let **S** be a semantic vector space of dimension *d* (e.g., d = 768 or 1536, matching common embedding dimensions).

A **meaning** is a point **m** ∈ S.

### Words as Constraint Surfaces
In natural language, words are often modeled as points or regions in embedding space. Limn inverts this:

**Definition 1 (Word as Hyperplane):**
A word *w* defines a hyperplane H_w in S:

```
H_w = { x ∈ S : ⟨n_w, x⟩ = c_w }
```

where **n_w** is the normal vector (direction the word "points") and c_w is the offset.

**Definition 2 (Word as Half-Space):**
More usefully, a word defines a half-space (one side of the hyperplane):

```
H_w⁺ = { x ∈ S : ⟨n_w, x⟩ ≥ c_w }
```

This represents "meanings consistent with word *w*" - the word constrains but doesn't specify.

### Sentences as Intersections
**Definition 3 (Sentence Meaning):**
A sentence consisting of words {w₁, w₂, ..., w_k} has meaning region:

```
M(sentence) = H_w₁⁺ ∩ H_w₂⁺ ∩ ... ∩ H_wₖ⁺
```

The meaning is not a point but a **region** - the set of all points consistent with all constraints.

## 2. Ambiguity as Volume

**Definition 4 (Ambiguity):**
The ambiguity A of a sentence is the volume (Lebesgue measure) of its meaning region:

```
A(sentence) = vol(M(sentence))
```

Properties:
- More words → smaller intersection → lower ambiguity
- Orthogonal constraints reduce volume fastest
- Redundant constraints (similar normal vectors) reduce volume slowly

**Theorem 1 (Ambiguity Reduction):**
Adding word w_{k+1} to a sentence cannot increase ambiguity:

```
A(w₁...w_k w_{k+1}) ≤ A(w₁...w_k)
```

with equality iff H_{w_{k+1}}⁺ ⊇ M(w₁...w_k).

## 3. The Key Mechanism

A **key** is additional constraint information available only to the decoder.

**Definition 5 (Key as Constraint Set):**
A key K is a set of hyperplanes {H_k₁, H_k₂, ..., H_k_m} that further constrain interpretation:

```
M_K(sentence) = M(sentence) ∩ H_k₁⁺ ∩ ... ∩ H_k_m⁺
```

**Definition 6 (Perfect Key):**
A key K is **perfect** for sentence σ if:

```
vol(M_K(σ)) = 0  (point or measure-zero set)
```

This means the sentence + key specifies exactly one meaning.

### Key Types

1. **Point Key**: K specifies a target point **p** directly. M_K = {**p**} if **p** ∈ M(sentence).

2. **Priming Key**: K is a document/context that biases the LLM's embedding space, effectively rotating/translating all H_w.

3. **Coordinate Key**: K provides dimensional constraints ("we are discussing topic T"), collapsing certain dimensions.

## 4. Information Theory

### Bits Per Word
**Definition 7 (Information Content):**
The information content of a sentence σ is:

```
I(σ) = log₂(vol(S) / vol(M(σ)))
```

where vol(S) is the "prior" volume (e.g., a bounding region of valid meanings).

**Corollary:** Each word contributes information proportional to how much it shrinks the volume.

### Conditional Information (with Key)
```
I(σ|K) = log₂(vol(M_K(⊥)) / vol(M_K(σ)))
```

where ⊥ is the "null sentence" (no words, just key).

The key can vastly increase I(σ|K) over I(σ) by providing a smaller prior.

### Information Density
**Definition 8:**
Information density D of a sentence with k words:

```
D(σ) = I(σ) / k  bits per word
```

With a key:
```
D_K(σ) = I(σ|K) / k
```

**Hypothesis:** Limn can achieve D_K >> D_natural_language because:
- Natural language uses words to *specify* meaning (low ambiguity target)
- Limn uses words to *constrain* meaning (key provides specificity)

## 5. Single-Word Inversion

**Definition 9 (Inversion Word):**
Word w_inv is an **inverter** if adding it to sentence σ yields:

```
M(σ ∪ {w_inv}) ∩ M(σ) has small relative volume
```

Geometrically: the new hyperplane cuts through the existing region such that most of the old region is excluded.

**Mechanism:** An inverter has normal vector **n_inv** pointing into (not along) the existing meaning region, with offset c_inv set to bisect or nearly exclude it.

**Example Construction:**
If M(σ) has centroid **c** and we want inversion:
- Set n_inv = direction from **c** to boundary
- Set c_inv such that H_inv passes near **c**

This "flips" which side of the region is valid.

## 6. Bootstrapping Considerations

For the language to be learnable without training:

1. **Vocabulary must be groundable**: Words need connection to natural language concepts (for priming) or to each other (for self-definition).

2. **Constraints must be inferrable**: An LLM must be able to infer H_w from examples of w's usage.

3. **Keys must be natural language compatible**: The priming mechanism relies on the LLM's existing semantic space.

**Self-Reference Challenge:**
The bootstrap document must:
- Use Limn to demonstrate Limn
- But provide enough English scaffolding to ground initial words
- Progressively reduce scaffolding as vocabulary builds

## 7. Open Problems

1. **Dimensionality**: What *d* works best? Too low = constraints collide. Too high = sparse, hard to learn.

2. **Vocabulary Optimization**: How to choose {H_w} to maximize:
   - Coverage of S (any meaning expressible)
   - Efficiency (few words needed for precision)
   - Learnability (inferrable from examples)

3. **Grammar Beyond Intersection**: Pure intersection is commutative. Do we need:
   - Ordered operations?
   - Negation (complement of half-space)?
   - Quantification?

4. **Computational Tractability**: Can we actually compute vol(M(σ))? (Polytope volume is #P-hard in general)

## 8. Connection to Prior Art

| Concept | Limn Analog |
|---------|--------------|
| Shamir Secret Sharing | k words = k hyperplanes; secret = intersection point |
| Neural Network Layers | Each word = one hyperplane in decision boundary |
| Wyner-Ziv Coding | Key = decoder side information enabling compression |
| Prototype Theory | Word = region around prototype (our half-space generalizes this) |
| Possible Worlds Semantics | Meaning region = set of possible worlds consistent with utterance |
