# Limn Grammar v1

## Core Principle: Constraint Composition

Limn grammar is fundamentally different from natural language grammar. Instead of building structures (subject-verb-object), Limn composes constraints. The grammar defines how constraint regions combine.

## 1. Basic Sentence Structure

### 1.1 The Commutative Hypothesis

**Claim:** Word order does not affect meaning.

```
ra vi du = vi du ra = du ra vi = ...
```

All orderings denote the same constraint intersection.

**Rationale:**
- Set intersection is commutative: A ∩ B = B ∩ A
- If words define sets, sentences should inherit this property
- Removes a major source of complexity (and information)

**Trade-offs:**
- Cannot use word order to indicate thematic roles (agent, patient)
- Cannot use word order for focus/topic marking
- Information must come from vocabulary, not position

### 1.2 Non-Commutative Elements

Some elements break commutativity by design:

**Operators** (`nu`, `ve`, `so`, `te`) apply to what follows:
```
nu lu vi = (not bright) AND alive
lu nu vi = bright AND (not alive)
```

**Reference** (`yo`, `an`) creates discourse structure:
```
yo lu an mu = this bright thing, that dark thing
```

**Proposal:** Operators have right-associative scope by default.

## 2. Composition Operators

### 2.1 Default: Intersection (∩)

Adjacent words combine by intersection:
```
A B = constraint(A) ∩ constraint(B)
```

The meaning is "things that satisfy both A and B."

### 2.2 Negation: Complement (¬)

`nu` takes the complement of the following word's constraint:
```
nu A = S \ constraint(A)  (everything NOT in A)
```

Chaining: `nu nu A = A` (double negation)

### 2.3 Intensification: Contraction (↓)

`ve` contracts the region toward its prototype center:
```
ve A = core(constraint(A))
```

The meaning shifts from "A-like things" to "prototypically A things."

### 2.4 Weakening: Expansion (↑)

`so` expands the region to include peripheral members:
```
so A = expand(constraint(A))
```

The meaning shifts from "A things" to "somewhat A-like things."

## 3. Scope Rules

### 3.1 Operator Scope

Operators apply to the immediately following word unless grouped:

```
nu lu vi mu = (NOT bright) AND alive AND dark
nu (lu vi mu) = NOT (bright AND alive AND dark)  -- if grouping existed
```

**Problem:** How to indicate grouping without brackets?

**Proposal 1: Pause Markers**
Use `|` (spoken as brief pause) to delimit scope:
```
nu lu | vi mu = (NOT bright) AND (alive AND dark)
nu | lu vi mu = NOT (bright AND alive AND dark) -- nu scopes over everything after
```

**Proposal 2: Prefix Position**
Operators at sentence start scope over entire sentence:
```
nu | lu vi mu = NOT (intersection of all)
lu nu vi mu = bright AND (NOT alive) AND dark
```

### 3.2 Reference Scope

`yo` and `an` scope over following elements until another reference or sentence end:
```
yo lu vi | an mu to = (this bright alive thing) AND (that dark still thing)
```

This allows multi-entity sentences.

## 4. Sentence Types

### 4.1 Descriptive (Default)

A sequence of words describes a meaning region:
```
ko su fi = solid-above-ending things
```

### 4.2 Interrogative

`te` marks a question - the sentence describes what is being asked about:
```
te ko su fi = what are the solid-above-ending things?
te | ko su fi = is there a solid-above-ending thing?
yo ko | te su fi = this solid thing - is it above and ending?
```

### 4.3 Imperative

**Problem:** How to command in a language of constraints?

**Proposal:** Use `we` (wanted/intended) as a speech act marker:
```
we | yo vi su = I want this living thing to go up
                (could mean: "climb", "rise", "ascend")
```

Combined with reference:
```
we | an mu fi = (I want) that dark thing to end
                (could mean: "turn off the light", "stop the threat")
```

## 5. Handling Argument Structure

Natural languages distinguish:
- Agent (doer) vs. Patient (done-to)
- Subject vs. Object
- Source vs. Goal

Limn's commutativity loses this. Solutions:

### 5.1 Relational Vocabulary

Use relational words to indicate roles:
```
vi ta | an ko = alive+beginning thing | that-solid-thing
              = birth of that solid thing (solid is patient)

an ko | vi ta = that-solid-thing | alive+beginning
              = that solid thing (which is) newly alive (solid is experiencer)
```

The `|` separator creates a two-place structure.

### 5.2 Conventional Order

**Against the commutative principle but practical:**
Define a conventional order for argument types:
1. Agent/experiencer
2. Action/state
3. Patient/theme
4. Location/time

```
vi | ta | ko = alive-thing | begins | solid-thing
             = the living thing starts the solid thing
```

### 5.3 Semantic Inference

Let context and plausibility determine roles:
```
vi ko ta = alive solid begin
```

Interpretations:
- A living thing makes a solid thing begin (agent=vi, patient=ko)
- A solid thing comes alive at the beginning (theme=ko, state=vi)
- The beginning of solid life (nominalization)

Let the key resolve which interpretation.

**Recommendation:** Start with 5.3 (maximum ambiguity), add 5.1 if needed.

## 6. Information Structure (Topic/Focus)

Natural languages mark what's being talked about (topic) vs. new information (focus).

### 6.1 Using Reference

`yo` marks the topic (what we're discussing):
```
yo ko | lu fi = this solid thing | bright ending
              = "As for this rock, it has a bright ending"
              = the sunset on the rock? the rock exploding?
```

### 6.2 Using Position

**Breaking commutativity:** First element is topic, rest is comment:
```
ko | lu fi = solid-thing | bright-ending
           = "The solid thing: it's a bright ending"
           = (topicalized reading)
```

## 7. Complex Sentences

### 7.1 Conjunction

Multiple meaning regions can be conjoined:
```
[lu vi] [mu to] = bright-alive AND dark-still
                = two separate constraints, both apply
```

Without brackets, this is ambiguous with single intersection.

**Proposal:** Use `pa` (parallel) as conjunction:
```
lu vi pa mu to = (bright AND alive) PARALLEL (dark AND still)
               = two distinct things/situations
```

### 7.2 Conditionality

How to express "if X then Y"?

**Proposal:** Use temporal sequence:
```
ta [condition] | fi [result] = begin-condition | end-result
                             = if condition begins, result ends
```

Or use `bi` (between/connecting):
```
[condition] bi [result] = condition CONNECTS-TO result
                        = condition leads to result
```

### 7.3 Relative Clauses

"The rock that fell" = solid + (above→below movement)

```
ko | an su na = solid | that above-below
             = solid thing that is associated with above-to-below
             = the rock that fell (maybe)
```

This is weak. May need dedicated relative marker.

## 8. Discourse Structure

### 8.1 Sentence Boundaries

In speech: pause + intonation reset
In text: `.` (period) or line break

### 8.2 Anaphora

`an` refers to previously mentioned entities:
```
Sentence 1: ko su fi = solid-above-ending
Sentence 2: an | lu ve = that-thing | very-bright
           = that (solid-above-ending thing) is very bright
           = The falling rock is very bright
```

### 8.3 Key Integration

Keys can be:
1. **Prefixed** to entire discourse (system prompt style)
2. **Embedded** as ordinary sentences that constrain interpretation
3. **External** (shared knowledge between parties)

Example embedded key:
```
yo bi | vi ra. lu ta fi.
= (this conversation is) about living-linear-things. bright-beginning-ending.
= We're discussing rivers. Dawn and dusk.
= KEY: rivers at sunrise/sunset
```

## 9. Summary of Grammar Rules

| Rule | Description |
|------|-------------|
| A B = A ∩ B | Adjacent words intersect |
| nu A = ¬A | Negation complements |
| ve A = core(A) | Intensification contracts |
| so A = expand(A) | Weakening expands |
| te ... = question | Interrogative marker |
| yo X = this X | Proximal reference |
| an X = that X | Distal reference |
| X \| Y = X; Y | Scope/grouping separator |
| X pa Y = X ∧ Y | Parallel conjunction |
| X bi Y = X → Y | Connection/implication |

## 10. Repetition Is Semantically Null

**Principle:** Repeating a word has no semantic effect.

```
lu = bright
lu lu = bright  (identical meaning)
lu lu lu = bright  (still identical)
```

**Mathematical Basis:** Set intersection is idempotent:
```
A ∩ A = A
⟦w w⟧ = ⟦w⟧ ∩ ⟦w⟧ = ⟦w⟧
```

**Common Learner Confusion:**
- "Does `river river river` mean emphasis?"
- "Does repetition indicate confluence or multiplicity?"

**Answer:** No. Repetition adds no semantic information. For emphasis or intensity:
- Use `ve` (intensifier): `ve lu` = very bright
- Use appropriate vocabulary: `mi` for multiplicity

### 10.1 Pragmatic Implicature

While semantically null, repetition may carry **pragmatic** effects. Following Grice's Maxim of Quantity, listeners may infer the speaker intended emphasis when repeating.

| | Repetition (`w w`) | Intensifier (`ve w`) |
|---|-------------------|---------------------|
| **Semantic effect** | None (idempotent) | Contracts to prototype |
| **Pragmatic effect** | Possible emphasis | Formal intensification |
| **Recommended for** | Poetry, rhythm | Precise communication |

### 10.2 Guidance

- **For formal/technical communication:** Avoid repetition. Use explicit operators.
- **For poetry/creative use:** Repetition is acceptable for rhythm and prosody.
- **For LLM interpretation:** Expect pragmatic readings; the key should resolve intent.

*Formal Ruling: See crew/linguist/analysis/repetition-ruling.md*

---

## 11. The Limn Zone: Optimal Sentence Length

**Observation:** Sentences of 3-6 words work best in Limn.

### 11.1 Too Few Words (1-2)

Single words or pairs are too vague for meaningful communication:
```
lu = bright  (too broad - sun? lamp? idea? smile?)
lu vi = bright + alive  (still very broad)
```

Without keys, these describe enormous regions of meaning-space.

### 11.2 The Sweet Spot (3-6 words)

Medium-length sentences have:
- Enough constraint to narrow meaning
- Enough ambiguity to require keys
- Room for creative interpretation

```
lu vi ra du = bright + alive + linear + ongoing
            = glowing river, sustained lightning, lit snake
            (manageable number of plausible readings)
```

### 11.3 Too Many Words (7+)

Long sentences risk:
- **Contradiction:** Adding constraints that can't coexist
- **Over-specificity:** Narrowing to a single meaning (defeats key mechanism)
- **Cognitive overload:** Too many intersections to track

```
lu vi ra du ta fi ko mu = bright + alive + linear + ongoing +
                          beginning + ending + solid + dark
                        = ??? (contradictory or so specific it's rigid)
```

### 11.4 Guidance

| Length | Effect | Recommendation |
|--------|--------|----------------|
| 1-2 words | Very vague | Use for abstract/poetic contexts |
| 3-6 words | "Limn Zone" | Optimal for key-collapsible communication |
| 7+ words | Risk contradiction | Use sparingly, check for consistency |

---

## 12. Limn for States, Not Stories

**Key Insight:** Limn is optimized for describing states, not narrating events.

### 12.1 Why Limn Suits State Description

- **Constraint intersection** describes what IS, not what HAPPENS
- **Commutativity** prevents temporal sequence
- **No tense system** - only lexical time markers (ta, fi, du)
- **Topology over trajectory** - maps regions, not paths

```
lu ta | mu fi = bright beginning | dark ending
             = sunrise/sunset as a STATE, not a narrative
```

### 12.2 What Limn Cannot Easily Express

- "The wolf chased the deer, then caught it"
- "First she opened the door, then walked through"
- "He was happy until she left"

These require:
- Event sequencing (temporal order)
- Agent-patient assignment (thematic roles)
- Causation chains (before→after)

### 12.3 Complementary Design

| Limn | Natural Language |
|-------|------------------|
| Describes states | Narrates events |
| Maps meaning-space | Tells stories |
| Order-independent | Order-dependent |
| Key-collapsible | Context-embedded |
| Riddles, poetry, definitions | Narratives, instructions, arguments |

### 12.4 Implication

Limn is not a replacement for natural language. It's a complement:
- Use Limn for **what exists** (states, properties, relations)
- Use natural language for **what happens** (events, sequences, causes)

A complete communication system might alternate:
```
[Limn: lu vi ra] = the glowing river
[English: flows toward the sea]
[Limn: mu ko fi] = dark solid ending
[English: where the canyon walls rise]
```

---

## 13. What Grammar DOESN'T Specify

- Thematic roles (agent, patient) - resolved by semantics/key
- Tense/aspect - encoded in vocabulary (ta, fi, du, etc.)
- Number - encoded in vocabulary (sa, mi, etc.)
- Definiteness - resolved by context/key
- Event sequence - not expressible (state-based language)
- Repetition meaning - null (idempotent)

This radical underspecification is intentional. Grammar provides composition; keys provide resolution.
