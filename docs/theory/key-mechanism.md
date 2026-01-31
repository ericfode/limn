# The Key Mechanism in Limn

## What is a Key?

A **key** is shared context between sender and receiver that collapses Limn's inherent ambiguity to a specific meaning. Without the key, a sentence supports many interpretations. With the key, it supports one (or few).

This document formalizes key types and their properties.

## 1. Taxonomy of Keys

### 1.1 Point Keys (Direct Specification)

The simplest key: directly specify the intended meaning.

```
Key: "I mean the third item in our shopping list"
Sentence: "bright flow edge"
Combined: "milk" (if milk is third on list)
```

**Properties:**
- Maximum precision
- Minimum elegance (why use Limn at all?)
- Useful for: indexing, codebooks, pre-agreed mappings

**Formal Model:**
K = {(σ₁, m₁), (σ₂, m₂), ...} - explicit sentence→meaning mapping

### 1.2 Topic Keys (Domain Restriction)

Restrict interpretation to a domain/topic.

```
Key: "We are discussing 19th century Russian literature"
Sentence: "brothers conflict soul"
Without key: family drama, sports rivalry, religious struggle, ...
With key: The Brothers Karamazov
```

**Properties:**
- Natural, conversational
- Partial collapse (may still have ambiguity within domain)
- Useful for: casual conversation, plausible deniability

**Formal Model:**
K defines a subspace S_K ⊂ S; interpretations restricted to S_K

### 1.3 Narrative Keys (Story Context)

A story or scenario that establishes characters, setting, relationships.

```
Key: [A paragraph describing Alice and Bob planning a surprise party for Carol]
Sentence: "secret gather joy tomorrow"
With key: The party preparations
Without key: Conspiracy? Cult meeting? Drug deal? Wedding?
```

**Properties:**
- Rich, expressive
- Allows complex, nuanced meanings
- Key can be literary, engaging
- Useful for: extended communication, relationship-specific meaning

**Formal Model:**
K is a probability distribution P_K over meanings; sentence samples from P_K

### 1.4 Coordinate Keys (Dimensional Pinning)

Fix specific semantic dimensions, leave others free.

```
Key: "Location: Tokyo. Time: 1985. Domain: technology"
Sentence: "small revolution pocket"
With key: Sony Walkman or early mobile phones
Without key: French revolution pamphlet? Lint? Rebellion tactics?
```

**Properties:**
- Structured, systematic
- Good for technical/precise communication
- Composable (multiple coordinate keys combine)
- Useful for: databases, structured information, technical specs

**Formal Model:**
K = {(dim_i, value_i)} - pin specific dimensions to values

### 1.5 Relational Keys (Shared Experience)

The key is a shared experience, memory, or relationship.

```
Key: [The sender and receiver both witnessed a specific sunset in Kyoto]
Sentence: "gold fade promise"
With key: References that specific moment and what was said
Without key: Financial decline? Autumn? Broken vow? Alchemy?
```

**Properties:**
- Maximum intimacy and security
- Impossible to reconstruct without shared experience
- Useful for: personal communication, security

**Formal Model:**
K is ineffable - exists only in shared cognitive state

## 2. Key Strength

### Definition
Key strength S(K) measures how much ambiguity K eliminates:

```
S(K) = E_σ[KCR(σ, K)]
```

Expected key collapse ratio across sentences.

### Strength Hierarchy
```
Point Keys:     S ≈ ∞  (complete collapse)
Relational:     S ≈ 100-1000 (high collapse, hard to fake)
Narrative:      S ≈ 50-200 (good collapse, expressive)
Coordinate:     S ≈ 10-100 (moderate collapse, composable)
Topic:          S ≈ 5-50 (partial collapse, natural)
```

### Trade-offs
| Key Type | Strength | Naturalness | Deniability | Expressiveness |
|----------|----------|-------------|-------------|----------------|
| Point | ★★★★★ | ★ | ★ | ★ |
| Relational | ★★★★★ | ★★★★★ | ★★★★★ | ★★★★ |
| Narrative | ★★★★ | ★★★★ | ★★★ | ★★★★★ |
| Coordinate | ★★★ | ★★ | ★★ | ★★★ |
| Topic | ★★ | ★★★★★ | ★★★★ | ★★ |

## 3. Key Transmission

How do sender and receiver establish a shared key?

### 3.1 Pre-Shared Key
Established before communication begins (like symmetric encryption).
- Meeting in person
- Previous relationship
- Organizational membership

### 3.2 Key Exchange
Negotiate key through initial plaintext communication.
- "Remember that time in Paris?" → establishes relational key
- "Let's talk about gardening" → establishes topic key
- Share a story → establishes narrative key

### 3.3 Key Derivation
Derive key from public information both parties have access to.
- Current news events
- Shared cultural knowledge
- Environmental context (both see same thing)

### 3.4 Ephemeral Keys
Keys valid only for one message or session.
- Increases security
- Requires key management overhead

## 4. Key Security Properties

### 4.1 Key Inference Resistance
Can an adversary infer the key from observing (sentence, meaning) pairs?

**Attack model:** Adversary sees multiple Limn sentences and their true meanings.

**Defense:**
- Use high-entropy keys (narrative, relational)
- Vary key across conversations
- Avoid patterns in sentence structure

### 4.2 Key Uniqueness
Do different keys yield different meanings for the same sentence?

**Desired property:** Yes. Same sentence σ with keys K₁, K₂ should yield meanings m₁ ≠ m₂.

This enables:
- Plausible deniability ("I meant X, not Y")
- Multi-recipient messaging (different meaning per recipient)

### 4.3 Key Verifiability
Can receiver verify they have the correct key?

**Methods:**
- Include checksum/MAC in message
- Semantic coherence check (does interpretation make sense?)
- Challenge-response (sender confirms interpretation)

## 5. Implementation for LLMs

### Priming-Based Keys
The key is prepended to the prompt:

```
[KEY]
Context: You are interpreting Limn sentences for a communication between
two marine biologists discussing coral reef conservation in the Great Barrier Reef.
[/KEY]

[SENTENCE]
bright colony fade warm
[/SENTENCE]

Interpret this Limn sentence given the context.
```

### Few-Shot Keys
The key is example (sentence, meaning) pairs:

```
Examples of how to interpret in our current context:
- "flow barrier break" → "ocean currents disrupting reef structure"
- "small bright many" → "juvenile fish populations"
- "slow white spread" → "coral bleaching progression"

Now interpret: "bright colony fade warm"
```

### System Prompt Keys
For persistent context across a conversation:

```
System: You are facilitating communication between Alice and Bob, who are
planning a surprise retirement party for their colleague Dr. Chen, who
studies butterfly migration. All Limn sentences refer to party planning,
with butterfly/migration metaphors for guests and timing.
```

## 6. Failure Modes

### Key Mismatch
Sender and receiver have different keys → misinterpretation.
- Mitigate: Confirmation protocols, checksums

### Key Leakage
Adversary obtains key → security compromised.
- Mitigate: Ephemeral keys, key rotation

### Insufficient Key
Key doesn't collapse ambiguity enough → multiple valid interpretations remain.
- Mitigate: Stronger key types, more context

### Over-Specification
Key is so detailed that Limn adds no value → might as well use plaintext.
- Mitigate: Balance key strength with Limn expressiveness

## 7. The Communication Formula

A fundamental insight about Limn communication:

### 7.1 The Formula

```
Meaning = Word × Key
```

This is a **multiplicative** relationship, not additive. Both factors are necessary:

- **Word without key** → superposition (ambiguous, multiple meanings)
- **Key without word** → reference without content (nothing to interpret)
- **Word × Key** → collapsed, precise meaning

### 7.2 Implications

**Single-word communication is possible:**
```
If: Word × Key ≥ θ (comprehension threshold)
And: |Word| = 1
Then: Key ≥ θ / Word (strong key compensates for minimal word)
```

A single Limn word can communicate a precise concept if the key is sufficiently strong.

**Example:**
```limn
Word: bri
Key: "We're discussing the afternoon light in this room"
Meaning: The sunlight streaming through the window
```

**Compression interpretation:**
```
Compression ratio = W_english / W_limn = K_limn / K_english
```

Limn trades word tokens for key tokens. The key mechanism is not optional overhead—it's the other half of the equation.

### 7.3 Practical Application

| Words | Key Strength | Result |
|-------|--------------|--------|
| Few + Strong Key | ✓ Precise meaning | Optimal Limn usage |
| Many + Weak Key | ✓ Precise meaning | Less compressed |
| Few + Weak Key | ✗ Ambiguous | Underspecified |
| Many + Strong Key | Overkill | Works but redundant |

**Design guidance:** When composing Limn, balance word count against available key context.

---

## 8. Open Questions

1. **Minimum Key Size:** What's the minimum key entropy needed to achieve KCR > 100?

2. **Key Composition:** Can multiple weak keys compose into a strong key?

3. **Key Discovery:** Can an LLM infer a likely key from a (sentence, meaning) pair?

4. **Key Compression:** Can narrative keys be compressed without losing strength?

5. **Universal Keys:** Are there keys that work across many sentences vs. sentence-specific keys?
