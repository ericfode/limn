# Puzzle 03: Order of Things

**Phase:** Discovery (Final)
**Difficulty:** 3/10
**Concept:** Commutativity - word order does not affect meaning

---

## What The Player Sees

### Discovery Vector

After solving Puzzle 02, players receive coordinates to a GitHub repository:
`github.com/limnographers/limn-fragments` (or similar)

### The Repository

**README.md (visible):**
```markdown
# limn-fragments

> "The order of words matters in English. The order of words _____ in Limn."

This repository contains fragments. The truth is in the history.

To find the third truth, look backward.
```

**Repository Structure:**
```
limn-fragments/
├── README.md
├── .truth
├── fragments/
│   ├── domain-01.md  (empty - "Fragment not yet recovered")
│   ├── domain-02.md  (empty)
│   └── ...
├── tests/
│   └── order_test.py
└── src/
    └── intersection.py (minimal code)
```

### The Hidden Puzzle

**1. Commit History:**

When players view the commit history (`git log`), they see:

```
commit a1b2c3d (HEAD)
    "they are the same."

commit e4f5g6h
    "...or are they?"

commit i7j8k9l
    "aqu sol is the second thought."

commit m1n2o3p
    "sol aqu is the first thought."

commit q4r5s6t
    "In the beginning was the word. Then came another."

commit u7v8w9x (initial)
    "The order of things."
```

Reading bottom-to-top (chronological) tells a story.
Reading top-to-bottom (reverse) asks a question.

**2. The Hidden File (`.truth`):**

```
lif gro you
gro lif you
you gro lif
you lif gro
gro you lif
lif you gro

Six permutations. One meaning.

alive + growth + young = ?
growth + alive + young = ?
young + growth + alive = ?

Are they different?

In English, "dog bites man" is not "man bites dog."
In Limn, "lif gro you" IS "you gro lif."

The order of words matters in English.
The order of words _____ _____ _____ in Limn.

Fill the blank. Then prove you understand.

Submit your proof to: #puzzle-03-proof (Discord)
```

**3. The Test File (`tests/order_test.py`):**

```python
def test_commutativity():
    """
    In Limn, word order does not affect meaning.
    All permutations of the same words have identical constraint regions.

    Prove this by showing that the following are equivalent:

    sol aqu = aqu sol
    lif gro you = you gro lif = gro you lif = lif you gro = you lif gro = gro lif you

    The intersection is commutative.
    A ∩ B = B ∩ A
    A ∩ B ∩ C = C ∩ A ∩ B = B ∩ C ∩ A = ...
    """

    # TODO: Implement your understanding
    # What does "sol aqu" mean?
    # What does "aqu sol" mean?
    # Are they different?

    assert sol_aqu_meaning == aqu_sol_meaning, "If this fails, you do not yet understand."

    # Bonus: What does "lif gro you" mean in ALL its permutations?
    pass
```

---

## The Puzzle Mechanism

Players must discover three things:

1. **The question:** "The order of words _____ in Limn."
2. **The answer:** "does not matter" (or equivalent)
3. **The proof:** Demonstrating understanding of WHY order doesn't matter

The mathematical concept is **commutativity of intersection**:
- A ∩ B = B ∩ A (always)
- The set of things that are (solid AND water) = the set of things that are (water AND solid)

Because Limn sentences work by constraint intersection, and intersection is commutative, word order cannot affect meaning.

---

## Valid Solutions

### Part 1: Fill in the Blank

**Correct answers:**
- "does not matter"
- "doesn't matter"
- "is irrelevant"
- "has no effect"
- "is meaningless"
- "doesn't change meaning"

### Part 2: Prove Understanding

**Tier 1 (Basic):**
Simply stating "sol aqu = aqu sol = ice"

*Response:*
```
You have named the equality. Now explain the WHY.

Why does order not matter?
What mathematical property makes this true?
```

**Tier 2 (Better):**
Explaining that both mean "solid AND water" so they're the same.

*Response:*
```
Closer. You see the AND. Now see the principle.

If A ∩ B = B ∩ A, what does this tell us about language?
```

**Tier 3 (Full Understanding):**
Any response that mentions:
- Commutativity
- Intersection is order-independent
- A ∩ B = B ∩ A
- "AND" doesn't have direction
- Set intersection is commutative

Example full answer:
```
"The order doesn't matter because Limn works by intersection,
and intersection is commutative. sol ∩ aqu = aqu ∩ sol.

All permutations of 'lif gro you' mean the same thing:
the intersection of alive + growth + young = seedling, child, new life.

The words can be in any order because AND has no direction."
```

*Response:*
```
THE THIRD TRUTH UNLOCKED

In Limn, word order does not affect meaning.
sol aqu = aqu sol
lif gro you = you gro lif = any permutation

This is because sentences work by intersection.
And intersection is commutative: A ∩ B = B ∩ A

In English, order creates meaning: "dog bites man" ≠ "man bites dog"
In Limn, order is free: all permutations are equivalent.

This is strange. This is powerful. This is Limn.

[UNLOCK: Phase 2 Access]
[UNLOCK: Grammar Fragment - Rule of Commutativity]
```

---

## What This Teaches

**Primary Concept:** Word order does not affect meaning in Limn (commutativity).

**Secondary Concepts:**
- Limn is fundamentally different from natural languages
- Mathematical properties underlie the grammar
- Intersection is the core operation

**Mind-bending implication:**
In English, we think of sentences as having structure (subject-verb-object). In Limn, sentences are unordered sets of constraints. This is deeply counterintuitive but logically consistent.

---

## Distribution Details

### Platform: GitHub

**Repository Setup:**
- Public repository
- Meaningful commit history (oldest to newest tells the story)
- Hidden file `.truth` (visible via `ls -la` or raw view)
- Test file with puzzle embedded in comments

**Discovery Path:**
1. Player finds repository link (from Puzzle 02 reward)
2. README points to "looking backward" (commit history)
3. Commit messages tell the story
4. `.truth` file contains the actual puzzle
5. Test file reinforces with code context

### Discord Submission

Players submit their proof to `#puzzle-03-proof` channel.

**Channel setup:**
- New thread for each submission
- Bot evaluates and responds
- Top solutions highlighted

---

## Unlock Mechanism

### What Players Receive:

**Full Solve:**

1. **Phase 2 Access**
   - Announcement post revealing Phase 2 theme: "How does it work?"
   - Access to new puzzle preparation channels

2. **Grammar Fragment**
   ```
   GRAMMAR RULE #2: COMMUTATIVITY

   Adjacent words combine by intersection.
   Intersection is commutative: A ∩ B = B ∩ A

   Therefore: word order does not affect meaning.

   sol aqu = aqu sol (solid AND water = water AND solid)

   All permutations of a sentence are equivalent:
   lif gro you = gro lif you = you gro lif = you lif gro = gro you lif = lif you gro

   This is not a limitation. This is freedom.
   Emphasis in Limn comes from context, not position.
   ```

3. **Discord Role:** "Order Walker"
   - Completes Phase 1 collection
   - Access to Phase 2 channels

4. **Teaser for Phase 2**
   ```
   You have learned three truths:
   1. Words are regions, not labels.
   2. Sentences combine regions by intersection.
   3. Word order does not matter.

   But what of negation? What of grouping?
   What happens when you say NOT?

   Phase 2 begins in 48 hours.
   The operators await.
   ```

---

## Hints (If Needed)

**Hint 1** (In the README, added after 24 hours):
```markdown
## Hint

Look at the test file. What is being tested?
What mathematical property is being asserted?

A ∩ B = B ∩ A

What does this mean for words?
```

**Hint 2** (Commit added after 48 hours):
```
commit [new-hash]
    "The intersection of A and B is the same as the intersection of B and A."
```

**Hint 3** (Discord announcement):
```
HINT FOR PUZZLE 03

The blank is three words: "does not matter"

But WHY doesn't it matter? The test file contains the answer.
Intersection is ___________. (A mathematical term.)
```

---

## Extended Challenge (Optional)

For players who solve quickly:

**Challenge posted in `#puzzle-03-proof-alumni`:**
```
BONUS CHALLENGE

You have proven that word order doesn't matter.

But what about OPERATOR order?

Consider:
nu sol aqu
sol nu aqu

Are THESE the same?

Hint: Operators bind to the immediately following word.

nu sol aqu = (NOT solid) AND water = ?
sol nu aqu = solid AND (NOT water) = ?

This is a preview of Phase 2. Think carefully.
```

This previews Puzzle 04 (negation) and shows that while WORD order is free, OPERATOR binding creates structure.

---

## Design Notes

### Why This Works:
1. **Multi-layered discovery:** Git history, hidden files, test files
2. **Technical flavor:** Appeals to programmers (GitHub, tests)
3. **Mathematical grounding:** Introduces formal concept (commutativity)
4. **Builds on previous:** Requires understanding of intersection

### Connection to Limn:
This puzzle teaches a fundamental property that makes Limn radically different from natural languages. It's the "aha" moment where players realize this is a genuinely alien grammar.

### Potential Issues:
- Players might not know how to view hidden files on GitHub
  - *Mitigation:* README hint points to "look backward" which encourages exploration
- The mathematical term "commutativity" might be unfamiliar
  - *Mitigation:* The test file explains it: A ∩ B = B ∩ A
- Some players might not use Git
  - *Mitigation:* GitHub web interface shows commit history visually

### Success Criteria:
- 60% of Puzzle 02 solvers find the repository
- 50% of those find the hidden `.truth` file
- 40% achieve full understanding
- Phase 2 launches with 30+ qualified participants

---

## Narrative Thread

### Before Puzzle:
Players know words are regions that combine by intersection.

### After Puzzle:
Players know word order is irrelevant - Limn sentences are unordered constraint sets.

### Lore Implication:
The Limnographers didn't think in sequences. They thought in sets. Their language reflects a fundamentally different way of organizing meaning.

**Lore fragment (for top solvers):**
```
RECOVERED FRAGMENT: The Third Teaching

"My students asked why order doesn't matter.
I wrote on the board:

sol aqu
aqu sol

'Are these different?' I asked.
'The first is solid-water, the second is water-solid,' they said.
'And what is the difference between solid-water and water-solid?'

Silence.

'In your language,' I said, 'order creates meaning.
Subject acts on object. First comes before second.
Your language is a river, flowing one direction.

'Limn is not a river. Limn is a lake.
All the words sit together, intersecting.
There is no first. There is no second.
There is only the region where all constraints overlap.'

They did not believe me at first.
Then they tried to think without sequence.
It was difficult. It was necessary.
It was the third truth."

- Fragment from "The Teachings"
```

---

## Technical Notes

### Git Repository Setup:

```bash
# Initialize repo
git init limn-fragments
cd limn-fragments

# Create initial structure
mkdir fragments tests src
touch fragments/domain-01.md fragments/domain-02.md
touch src/intersection.py
touch .truth

# Add content to files (as specified above)

# Create commits in order (oldest first)
git add .
git commit -m "The order of things."

# Modify README slightly and commit
git commit --allow-empty -m "In the beginning was the word. Then came another."

git commit --allow-empty -m "sol aqu is the first thought."

git commit --allow-empty -m "aqu sol is the second thought."

git commit --allow-empty -m "...or are they?"

git commit --allow-empty -m "they are the same."
```

### Hidden File Content (.truth):
Must be viewable via:
- `Raw` button on GitHub
- Cloning repo and using `ls -la`
- GitHub API
- Knowing to look for dotfiles

---

*"Six permutations. One meaning. The order of things... does not matter. The third truth. The grammar deepens."*
