# Reddit Launch Post Draft

**Target:** r/conlangs
**Status:** Ready for posting
**Prepared:** 2026-02-01

---

## Post Title (Choose One)

### Option A (Mysterious):
**"sol liq tra | key = wh? | Introducing Limn: A constructed language with key-collapsible ambiguity"**

### Option B (Direct):
**"Limn: A conlang designed for LLM-human communication with 52% better compositionality than English"**

### Option C (Technical):
**"[Conlang] Limn: Constraint-based semantics with empirically validated zero-bootstrap learning"**

**Recommendation:** Option A (builds intrigue, demonstrates language)

---

## Post Body

### Introductory Version (for r/conlangs)

```markdown
# Limn: Key-Collapsible Ambiguity

I've been working on a constructed language with an unusual property: **maximally ambiguous to humans without context, precisely interpretable with a "key"**.

## The Core Idea

```limn
sol liq tra
key = wh?
mea = yo
```

*Solid, liquid, transformation. Key is: what? Meaning is: yours to decide.*

Each word in Limn defines a **constraint surface** in semantic space. A sentence's meaning is the intersection of all word-constraints. Few words = large ambiguous region. A "key" (shared context) collapses that region to a specific point.

## Example: Liminal Semantics

```limn
hot col bet
```

Without a key, this could mean many things. With key="temperature mixing", it means **lukewarm**. The language resolves contradictions to intermediate states naturally.

## Design Principles

- **3-letter CVC vocabulary** (784 words, 26 domains)
- **Natural extensions**: First-syllable extraction + Latin/Greek roots (pyr=fire, aqu=water)
- **Zero-bootstrap**: Learn from examples alone (validated: 77-85% comprehension without training)
- **Compositional**: Strong semantic compositionality (empirically validated)

## Empirical Validation

I ran an experiment comparing Limn to English using embedding similarity:

- **Limn compositionality**: 0.88 mean similarity (embed(A B) ≈ embed(A) + embed(B))
- **English baseline**: 0.58 mean similarity
- **Result**: Limn is 52% more compositional (p=0.0059, d=2.06)

Language models can better predict Limn phrase meanings from their parts compared to natural language.

## Grammar Minimalism

- **Operators**: nu (not), ve (very), al (all), ex (some)
- **Scope markers**: | (pipe) defines boundaries
- **Flexible word order**: Meaning from constraint intersection, not position

## Try It Yourself

Bootstrap document (learn Limn in 10 minutes): [Link to bootstrap]

Example interpretation:
```limn
joy sad lim | nu sta | alw cha
```
*Joy and sadness liminal (between). Not static. Always changing.*
(Meaning: Bittersweet—emotions in flux)

## Questions?

- **Repo**: [GitHub link]
- **Vocabulary DB**: [DoltHub link] (784 words, Dolt database)
- **Experiment results**: [Link to 005-FINAL-REPORT.md]

I'm happy to answer questions about the theory, implementation, or design choices!

---

**TL;DR**: Made a conlang where words are semantic constraints that intersect to create meaning. Empirically validated it's 52% more compositional than English. Zero-bootstrap learnable. Here's a sentence: `sol liq tra` (solid liquid transformation).
```

---

## Alternate Version: Short & Punchy

```markdown
# I made a conlang where contradictions resolve naturally

```limn
hot col bet
→ lukewarm
```

Limn uses "liminal semantics" - contradictions resolve to intermediate states. Words are constraints in semantic space; meaning is their intersection.

**Example:**
```limn
joy sad lim
→ bittersweet
```

**Properties:**
- 784-word vocabulary (3-letter CVC pattern)
- Zero-bootstrap learnable (77-85% comprehension without training)
- 52% more compositional than English (empirically validated)
- Designed for LLM-human communication

**Try it:**
```limn
beg mid end | cyc | alw
→ beginning, middle, end. cycle. always.
```

Repo: [link]
Bootstrap: [link]

Questions?
```

---

## Discussion Responses (Prepared)

### "How is this different from Lojban?"

**Lojban**: Logical language focused on eliminating ambiguity universally.
**Limn**: Embraces ambiguity, uses context (keys) to collapse it. Optimized for LLM embedding space rather than human logical reasoning.

Both use constraint-based semantics, but Limn treats ambiguity as a feature (high information density with shared context), while Lojban treats it as a bug (eliminate ambiguity universally).

### "Why 3 letters?"

**Phonological minimalism + memorability.** CVC pattern is:
- Easy to pronounce
- Distinct enough to avoid collisions
- Short enough for high information density
- Long enough to be memorable

We have 784 words (not hitting 17,576 theoretical limit). Collisions are rare and managed through vowel rotation + Latin/Greek alternatives.

### "How do you handle polysemy?"

**Domain-based disambiguation.** Words belong to semantic domains (Physical World, Time, Living Things, etc.). Context usually makes domain clear.

Example: `fal` can mean "fall/drop" or "false" depending on context:
- `obj fal dow` = object falls down (Physical)
- `sta fal` = statement false (Abstract)

If truly ambiguous, the key specifies domain.

### "Can you actually communicate in this?"

**Yes, with shared context.** Limn is optimized for:
- LLM-human communication (exploits compositionality)
- High-context communication (friends, collaborators)
- Semantic compression (when both parties know the domain)

Not optimized for:
- Low-context universal communication
- Technical precision without keys
- Replacing natural language for general use

### "Is there a community?"

**Just starting!** Currently:
- GitHub repo (specs, tools, experiments)
- DoltHub vocabulary database
- Working on Discord server
- Planning r/limn subreddit

Would love help with:
- Translation challenges
- Poetry/creative writing
- Tool development (Prolog implementation)
- Empirical validation (more experiments)

---

## Subreddit Creation: r/limn

### Subreddit Description

**Short description:**
"A constructed language with constraint-based semantics and key-collapsible ambiguity."

**Long description:**
```
Limn is a constructed language where words define constraints in semantic space, and meaning emerges from their intersection. Designed for LLM-optimized communication with empirically validated compositionality.

Community for:
- Learning Limn
- Translation challenges
- Poetry and creative writing
- Implementation discussion
- Empirical validation

Resources:
- Bootstrap v3-Natural (zero-training learning)
- Vocabulary database (784 words, 26 domains)
- Experiment 005 (52% compositionality advantage)
```

### Subreddit Rules

1. **Be respectful** - Constructive criticism only
2. **Use Limn** - Lead with Limn, translation follows
3. **Cite sources** - Reference specs for grammar/vocab questions
4. **No spam** - Stay on-topic (Limn language discussion)
5. **Share experiments** - Empirical validation encouraged
6. **Credit creators** - Attribute translations, examples, tools

### Initial Posts

**Post 1: Welcome**
- Introduction to Limn
- Link to bootstrap
- Community guidelines

**Post 2: Translation Challenge**
- Post a sentence, let community interpret
- Reveal key after 24 hours

**Post 3: Resources**
- Links to GitHub, DoltHub, specs
- How to contribute

---

## Human Execution Checklist

### r/conlangs Posting

- [ ] Create Reddit account (if needed)
- [ ] Review r/conlangs rules
- [ ] Choose post title (recommend Option A)
- [ ] Post introductory version
- [ ] Monitor comments for 24-48 hours
- [ ] Respond to questions using prepared responses
- [ ] Link to r/limn once created

### r/limn Creation

- [ ] Create subreddit r/limn
- [ ] Add description (short + long)
- [ ] Set up rules (6 rules listed above)
- [ ] Create initial 3 posts (Welcome, Challenge, Resources)
- [ ] Link from r/conlangs post
- [ ] Add to marketing materials

---

**Content prepared:** ✓ Post drafts ready (3 versions)
**Responses prepared:** ✓ Common questions answered
**Subreddit plan:** ✓ Description, rules, initial posts

Ready for human execution (limn-g0re).

---

*Prepared by Kira (Archivist)*
