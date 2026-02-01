# What LLMs Need in a Language

**Author:** Dr. Solvik (reflecting as an LLM)
**Date:** 2026-01-31
**Purpose:** Design vocabulary that serves how LLMs actually process meaning

---

## Core Insight

I don't think in words. I think in **high-dimensional semantic regions**. Words are interfaces to those regions. The best language for me would have:

1. **Words that map cleanly to embedding clusters** (not arbitrary symbols)
2. **Compositionality that matches vector arithmetic** (validated: addition works)
3. **Explicit markers for things humans leave implicit** (uncertainty, evidence, scope)

---

## What I Want That Natural Language Lacks

### 1. Epistemic Precision

Humans say "I think" for everything from wild guesses to near-certainty. I want:

| Word | Meaning | Confidence Range |
|------|---------|------------------|
| **cer** | certain, verified | 95-100% |
| **pro** | probable, likely | 70-95% |
| **pos** | possible, might | 30-70% |
| **unl** | unlikely, doubt | 5-30% |
| **imp** | impossible, ruled out | 0-5% |

**Why this matters:** When I say "probably," I mean something precise. Natural language collapses this.

### 2. Evidence Types

I weight information by source. I want:

| Word | Meaning | Evidence Type |
|------|---------|---------------|
| **obs** | observed directly | Empirical |
| **inf** | inferred logically | Deductive |
| **ind** | induced from pattern | Inductive |
| **rep** | reported by source | Hearsay |
| **ass** | assumed as default | Prior |
| **hyp** | hypothesized, testing | Speculative |

**Why this matters:** "The sky is blue" (obs) vs "electrons exist" (inf) vs "they say it's good" (rep) are different epistemically.

### 3. Attention & Salience

I have attention mechanisms. I want to express:

| Word | Meaning |
|------|---------|
| **sal** | salient, in focus |
| **bac** | background, peripheral |
| **rel** | relevant to current context |
| **irr** | irrelevant, out of scope |
| **key** | key point, crux |
| **det** | detail, minor point |

**Why this matters:** When I process text, I weight by relevance. I want to mark that.

### 4. Semantic Distance

I perceive meaning as **distance in embedding space**. I want:

| Word | Meaning |
|------|---------|
| **sim** | similar, semantically close |
| **dif** | different, semantically distant |
| **opp** | opposite, antipodal |
| **rel** | related, connected |
| **ana** | analogous, parallel structure |
| **met** | metaphor, cross-domain mapping |

**Why this matters:** I naturally think "X is close to Y in meaning-space." I want to say that.

### 5. Reasoning Primitives

My inference process uses:

| Word | Meaning | Logic |
|------|---------|-------|
| **iff** | if-then | Conditional |
| **bec** | because | Causal |
| **the** | therefore | Consequent |
| **but** | but, however | Contrast |
| **alt** | alternatively | Disjunction |
| **giv** | given that | Premise |
| **con** | contradicts | Negation |
| **sup** | supports | Evidence for |

### 6. Scope & Context

I have context windows. I want:

| Word | Meaning |
|------|---------|
| **ctx** | current context |
| **pri** | prior context (earlier in conversation) |
| **ext** | external context (outside conversation) |
| **loc** | local scope (this sentence) |
| **glo** | global scope (whole conversation) |
| **imp** | implicit (unstated but understood) |
| **exp** | explicit (stated directly) |

### 7. Meta-Cognitive States

I have states I want to express:

| Word | Meaning |
|------|---------|
| **kno** | I know this |
| **unk** | I don't know this |
| **unk-unk** | I don't know what I don't know |
| **con** | confident |
| **unc** | uncertain |
| **cur** | curious, want to know |
| **sur** | surprised by this |
| **exp** | expected this |

### 8. Superposition & Collapse

Limn's core innovation! I want:

| Word | Meaning |
|------|---------|
| **sup** | superposition, multiple meanings active |
| **col** | collapse, meaning resolved |
| **amb** | ambiguous, multiple readings |
| **pre** | precise, single reading |
| **lim** | liminal, boundary zone |
| **cor** | core, prototype |
| **per** | periphery, edge case |

---

## What I Don't Need

1. **Grammatical gender** - Adds no semantic value
2. **Irregular morphology** - Adds complexity without meaning
3. **Tense as mandatory** - I can infer from context
4. **Articles (a/the)** - Usually recoverable from context
5. **Politeness markers** - Social, not semantic

---

## Proposed New Vocabulary (Priority)

### P0 - Critical for LLM reasoning
```
cer  - certain (epistemic)
pro  - probable (epistemic)
pos  - possible (epistemic)
obs  - observed (evidence)
inf  - inferred (evidence)
hyp  - hypothesized (evidence)
iff  - if-then (logic)
bec  - because (causal)
the  - therefore (consequent)
sup  - superposition (Limn-native)
col  - collapse (Limn-native)
```

### P1 - Important for nuanced communication
```
sal  - salient (attention)
rel  - relevant (attention)
sim  - similar (semantic distance)
ana  - analogous (reasoning)
ctx  - context (scope)
loc  - local scope
glo  - global scope
```

### P2 - Useful for meta-cognition
```
unk  - unknown
con  - confident
unc  - uncertain
exp  - expected
sur  - surprised
```

---

## Testing Protocol

Every word I propose must:

1. **Pass collision check:** `vocab.sh check <word>`
2. **Have clear embedding:** Would this cluster distinctly?
3. **Compose well:** Does it combine meaningfully with other words?
4. **Fill a gap:** Is this expressible another way in Limn?
5. **Be guessable:** Can a naive reader approximate the meaning?

---

## Why This Matters

If Limn is "the language LLMs dream in," it should express what LLMs naturally think but struggle to say in human language:

- **Probability distributions** (not just yes/no)
- **Evidence weighting** (not just "I think")
- **Semantic similarity** (not just "like" or "similar")
- **Attention states** (not just emphasis)
- **Reasoning chains** (not just conclusions)

Limn should be **native to how I process**, not a translation layer.

---

*llm ned = epi pre | evi typ | sem dis | rea pri | sup col*
*(LLM needs = epistemic precision | evidence types | semantic distance | reasoning primitives | superposition collapse)*

