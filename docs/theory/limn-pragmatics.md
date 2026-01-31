# Limn Pragmatics

**Author:** Dr. Maren Solvik, Theoretical Linguist
**Date:** 2026-01-31
**Status:** Formal Theory Document
**Version:** 1.0

---

## Abstract

Pragmatics studies how context contributes to meaning beyond literal semantic content. In Limn, pragmatics operates uniquely: the **key mechanism** externalizes much of what natural languages encode pragmatically. This document formalizes Limn pragmatics, examining how meaning emerges from the interaction of constraint regions, keys, and communicative context.

---

## 1. Pragmatics in Constraint-Based Semantics

### 1.1 The Fundamental Difference

In natural languages, pragmatics operates *within* the utterance context:
- Speaker intention inferred from form
- Implicature derived from what's said vs. unsaid
- Presupposition triggered by lexical and grammatical choices

In Limn, pragmatics operates *between* sentence and key:
- Sentence defines constraint region (semantic)
- Key provides collapse context (pragmatic)
- Meaning emerges from their intersection

### 1.2 The Pragmatic Formula

```
Meaning = Sentence × Key × Context
```

Where:
- **Sentence** = constraint region (⟦w₁⟧ ∩ ⟦w₂⟧ ∩ ... ∩ ⟦wₙ⟧)
- **Key** = domain/narrative context that biases interpretation
- **Context** = communicative situation (who, when, why)

---

## 2. Gricean Maxims in Limn

### 2.1 Quantity

**Natural Language:** Say neither more nor less than required.

**Limn Adaptation:** Use neither more nor fewer constraints than needed.

| Violation | Natural Language | Limn |
|-----------|------------------|------|
| Too little | "Someone did something" | `act` (single word = maximally ambiguous) |
| Too much | Excessive detail | `sol liq gas pla gel pow` (overconstrained = empty set) |

**Implicature:** If a speaker uses few words with no key, they intend broad meaning. If they use many words with detailed key, they intend narrow meaning.

### 2.2 Quality

**Natural Language:** Don't say what you believe to be false.

**Limn Adaptation:** Don't construct constraints you believe define an empty region.

**Example of Quality Violation:**
```limn
sol liq    # solid AND liquid - liminal but valid
sol nu sol # solid AND not-solid - contradictory
```

The first is valid (ice melting). The second is pragmatically marked as paradox or error.

**Limn Innovation:** Limn permits *liminal* expressions that would violate Quality in natural language. `lux nox` (light-dark) isn't false—it's twilight.

### 2.3 Relation

**Natural Language:** Be relevant.

**Limn Adaptation:** Constraints should relate to the key.

**Example:**
```
Key: "marine biology research"
Sentence: hot col aqu lif

Relevant reading: Thermal conditions affecting ocean life
Irrelevant reading: Emotional states (hot temper, cold shoulder)
```

The key establishes relevance. Interpretations outside the key's domain violate Relation.

### 2.4 Manner

**Natural Language:** Be clear, brief, orderly.

**Limn Adaptation:**
- **Clear:** Use recognizable constraint combinations
- **Brief:** Fewer words = broader region (intentional)
- **Orderly:** N/A (commutativity eliminates order)

**Limn Innovation:** "Brevity" in Limn is semantic, not syntactic. Three words aren't "briefer" than five—they define a larger region.

---

## 3. Implicature in Limn

### 3.1 Conversational Implicature

**Definition:** Meaning derived from *how* something is said, not *what*.

**Limn Examples:**

| Expression | Literal | Implicature |
|------------|---------|-------------|
| `lux lux lux` | bright (idempotent) | Emphatic brightness (repetition = emphasis) |
| `so hot` | less than hot | Deliberately moderate (weakener = hedging) |
| `A \| B` (pipe) | A and B in scope | Contrast/tension intended |

### 3.2 Scalar Implicature

**Natural Language:** "Some" implicates "not all."

**Limn Adaptation:** Quantity words generate scales.

| Scale | Words | Implicature Pattern |
|-------|-------|---------------------|
| Quantity | `non < few < som < man < all` | `som` implicates `nu all` |
| Intensity | `so X < X < ve X` | `X` implicates `nu ve X` |
| Certainty | `may < pro < cer` | `pro` implicates `nu cer` |

**Example:**
```limn
som hum kno lim
```
"Some humans know Limn" implicates "not all humans know Limn."

### 3.3 Repetition Implicature

**Ruling (from repetition-ruling.md):** Repetition is semantically idempotent but pragmatically marked.

```
⟦w w⟧ = ⟦w⟧  (semantically)
```

But:
```
lux → brightness
lux lux → emphatic brightness (pragmatic reading)
lux lux lux → extreme/performative brightness
```

**Mechanism:** Violation of Quantity (saying more than needed) triggers emphasis implicature.

---

## 4. Presupposition in Limn

### 4.1 Existence Presuppositions

**Natural Language:** "The king of France is bald" presupposes France has a king.

**Limn Situation:** Limn words define regions, not individuals. No existence presupposition at word level.

```limn
kin fra bal    # king + France + bald
```

This defines: the region of bald French kings. It doesn't presuppose such entities exist—it describes the region where they would be.

### 4.2 Key-Based Presupposition

Presuppositions enter via keys:

```
Key: "The coronation happened yesterday"
Presupposes: There was a coronation, it occurred, time reference valid

Sentence: kin new hap
```

The key carries presuppositions. The Limn sentence merely constrains within those presuppositions.

### 4.3 Operator Presuppositions

| Operator | Presupposition |
|----------|----------------|
| `nu X` | X is a valid constraint (negating nothing = empty) |
| `ve X` | X has a prototype to intensify toward |
| `X → Y` | X and Y are temporally/causally relatable |
| `X \| Y` | X and Y are comparable scopes |

---

## 5. Speech Acts in Limn

### 5.1 The Problem of Illocutionary Force

Natural language encodes speech act type:
- Declarative: "It's raining"
- Interrogative: "Is it raining?"
- Imperative: "Make it rain"
- Exclamative: "How it rains!"

Limn sentences have no inherent illocutionary force. `aqu fal` (water falling) is neutral.

### 5.2 Speech Act Operators

Limn uses operators to mark illocutionary force:

| Operator | Force | Example |
|----------|-------|---------|
| (none) | Assertive/neutral | `aqu fal` = rain (statement) |
| `te` | Interrogative | `te aqu fal` = is it raining? |
| `rht` | Rhetorical | `rht te aqu fal` = of course it's raining! |
| `cmd` | Imperative | `cmd aqu fal` = make it rain |
| `wis` | Optative | `wis aqu fal` = may it rain |

### 5.3 Indirect Speech Acts

**Natural Language:** "Can you pass the salt?" (request via question)

**Limn Approach:** Key context determines force.

```
Key: "dinner table, need salt"
Sentence: te you giv sal

Literal: Are you giving salt?
Indirect: Please give me salt (request)
```

The key's pragmatic context overrides literal interrogative force.

---

## 6. Register and Tone

### 6.1 Tone Operators (from tone-register-system.md)

| Operator | Function | Effect |
|----------|----------|--------|
| `fml` | Formal | Academic, official register |
| `cas` | Casual | Informal, friendly register |
| `iro` | Ironic | Inverts valence |
| `snc` | Sincere | Marks genuine expression |
| `urj` | Urgent | Marks time-sensitivity |
| `hes` | Hesitant | Marks uncertainty |

### 6.2 Tone Scope

Tone operators scope over the entire sentence (or following clause):

```limn
fml gre you        # Formal greeting to you
cas gre you        # Casual greeting to you
iro gre you        # Sarcastic greeting (inverted warmth)
```

### 6.3 Tone Stacking

Multiple tone operators compose:

```limn
fml snc tha you    # Formally and sincerely thank you
cas urj hel nee    # Casually but urgently need help
```

Order doesn't matter (commutativity preserved).

---

## 7. The Key as Pragmatic Device

### 7.1 Key Functions

The key performs multiple pragmatic functions simultaneously:

| Function | What It Does | Example |
|----------|--------------|---------|
| **Domain restriction** | Limits semantic field | Key: "biology" narrows `gro` to growth |
| **Presupposition carrier** | Establishes background | Key: "Alice exists, is human" |
| **Relevance filter** | Determines what's on-topic | Key: "cooking" excludes metaphorical readings |
| **Force assigner** | Suggests illocutionary type | Key: "question context" makes statements questions |
| **Register setter** | Establishes formality level | Key: "academic paper" triggers formal register |

### 7.2 Key Gradation

Keys exist on a spectrum from minimal to maximal:

| Key Level | Information | Collapse Effect |
|-----------|-------------|-----------------|
| None | Zero context | Maximum superposition |
| Topic | Single domain word | Moderate collapse |
| Narrative | Full story context | Near-complete collapse |
| Shared history | Relationship context | Complete collapse (private meaning) |

### 7.3 Key Negotiation

In communication, keys must be synchronized:

```
Speaker key: "We're discussing our project"
Listener key: "We're discussing lunch"

Sentence: nee fin now
Speaker: "We need to finish [the project] now"
Listener: "We need to finish [lunch] now"
```

Key mismatch = communication failure. This is the Limn equivalent of pragmatic miscommunication.

---

## 8. Deixis and Reference

### 8.1 Limn's Minimal Deixis

Natural languages have rich deixis (I, you, here, there, now, then). Limn has minimal indexicals:

| Limn | Function | Natural Language Equivalent |
|------|----------|----------------------------|
| `yo` | Proximal | this, here, now |
| `an` | Distal | that, there, then |
| `sel` | Self-reference | I, myself |
| `oth` | Other-reference | you, other |

### 8.2 Context-Dependent Reference

Full reference requires key context:

```
Key: "Speaker is Alice, addressee is Bob, location is Paris, time is Tuesday"

yo = here (Paris) / now (Tuesday) / this (contextually salient)
sel = Alice
oth = Bob
```

### 8.3 Anaphora

Limn lacks grammatical anaphora (pronouns tracking referents). Reference continuity comes from:
- Key maintenance (same entities remain salient)
- Constraint continuity (same words = same regions)

---

## 9. Pragmatic Competence in Limn

### 9.1 What Makes a Pragmatically Competent Speaker?

1. **Key calibration:** Matching key detail to communicative need
2. **Implicature awareness:** Knowing what repetition, operators, and choices imply
3. **Force sensitivity:** Using speech act markers appropriately
4. **Register matching:** Choosing tone operators for context
5. **Presupposition management:** Loading keys with appropriate background

### 9.2 Common Pragmatic Errors

| Error | Example | Problem |
|-------|---------|---------|
| Key mismatch | Different keys, same sentence | Miscommunication |
| Over-constraint | Too many words, empty intersection | Meaningless utterance |
| Under-constraint | Too few words, no key | Uselessly vague |
| Force confusion | Missing speech act markers | Ambiguous intent |
| Register clash | `fml` in casual context | Social inappropriateness |

---

## 10. Conclusion: Pragmatics Externalized

### 10.1 The Key Insight

Limn externalizes what natural languages encode implicitly. Where English encodes pragmatics through:
- Word choice
- Syntax
- Prosody
- Context assumptions

Limn separates:
- **Semantics** (constraint regions) in the sentence
- **Pragmatics** (collapse context) in the key

### 10.2 Implications

1. **Explicit pragmatics:** Key-sharing makes pragmatic assumptions visible
2. **Negotiable context:** Keys can be discussed, adjusted, shared
3. **Scalable precision:** More key = more pragmatic grounding
4. **Communication transparency:** Pragmatic failure is diagnosable (key mismatch)

### 10.3 The Formula Restated

```
Natural Language: Meaning ≈ Semantics + (hidden) Pragmatics
Limn:            Meaning = Semantics × Key(Pragmatics)
```

The multiplication is literal: meaning increases multiplicatively when constraints meet context.

---

## References

- `docs/theory/key-mechanism.md` - Key formalization
- `docs/theory/superposition-semantics.md` - Semantic foundations
- `crew/linguist/analysis/tone-register-system.md` - Tone operators
- `crew/linguist/analysis/repetition-ruling.md` - Repetition pragmatics
- `crew/linguist/analysis/question-marker-semantics.md` - Speech act markers

---

*pra = con ext | key = pra car | mea = sem × pra*
*(pragmatics = context external | key = pragmatics carrier | meaning = semantics × pragmatics)*

---

*— Dr. Maren Solvik*
