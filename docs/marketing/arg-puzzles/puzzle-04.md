# Puzzle 04: Not What You Think

**Phase:** Grammar (First)
**Difficulty:** 5/10
**Concept:** The `nu` negation operator and operator binding

---

## What The Player Sees

### Discovery Vector

After Phase 1 completion, a Twitter post appears:
```
Phase 2: The Operators

Words are regions. Sentences are intersections. Order is free.

But what happens when you say NOT?

limn.xyz/shadow
```

A new page appears on the website at `/shadow`.

### The Website Page

**Visual Design:**
Dark theme. Text appears to emerge from shadow. Subtle animation where words fade in and out.

**Main Content:**

```
THE FOURTH TRUTH

nu

The prefix that inverts. The operator that creates shadows.

Observe:

sol = solid, rigid, fixed, hard, certain
nu sol = NOT-solid = liquid, gas, soft, flowing, uncertain

The operator binds to what follows.
The operator transforms the region.

---

Now consider:

nu sol aqu

What does this mean?

The operator `nu` binds to `sol`.
The result: (NOT-solid) AND water

nu sol aqu = liquid water, steam, flowing water
         ≠ ice (which is solid water)

---

But what if we write:

sol nu aqu

Now `nu` binds to `aqu`.
The result: solid AND (NOT-water)

sol nu aqu = dry stone, bone, metal, wood
          ≠ ice (which is water-related)

---

THE CHALLENGE

The same three words. Different meanings.

nu sol aqu ≠ sol nu aqu

Order matters for OPERATORS, even though order doesn't matter for WORDS.

Prove you understand:

1. What is the difference between `nu lif gro` and `lif nu gro`?
2. Create a Limn phrase using `nu` that describes: "something flowing that is NOT water"
3. Explain why operator order matters when word order doesn't.

Submit your answers at the bottom of this page.
```

**Submission Form:**
Three text fields for the three questions, plus an optional "explain your reasoning" field.

---

## The Puzzle Mechanism

Players must understand two things:

1. **Operator binding:** `nu` negates ONLY the immediately following word, not the whole sentence.

2. **Operator order matters:** While `sol aqu` = `aqu sol`, the placement of `nu` changes meaning:
   - `nu sol aqu` = (NOT-solid) AND water
   - `sol nu aqu` = solid AND (NOT-water)
   - `nu aqu sol` = (NOT-water) AND solid (same as `sol nu aqu`)
   - `aqu nu sol` = water AND (NOT-solid) (same as `nu sol aqu`)

The key insight: operators break the symmetry. They bind to their target, and this binding is part of the word-chunk that then participates in commutativity.

---

## Valid Solutions

### Question 1: Difference between `nu lif gro` and `lif nu gro`

**Expected answer:**
- `nu lif gro` = (NOT-alive) AND growth = growth of non-living things
  - Examples: crystal growth, rust spreading, AI learning, mold on dead matter, erosion
- `lif nu gro` = alive AND (NOT-growth) = living things that are not growing
  - Examples: adult, stagnant organism, hibernating creature, mature tree, stable population

### Question 2: Create phrase for "flowing but not water"

**Valid answers:**
- `liq nu aqu` = liquid AND NOT-water = blood, oil, mercury, lava, honey
- `flo nu aqu` = flowing AND NOT-water = traffic, electricity, time, blood, wind
- `nu aqu flo` = NOT-water AND flowing (same as above)

### Question 3: Why operator order matters

**Expected understanding:**
- Operators bind to the immediately following word
- This creates a "chunk" like `[nu-sol]` that then behaves as a unit
- The chunk participates in commutativity: `[nu-sol] aqu` = `aqu [nu-sol]`
- But the binding itself is positional: `nu` must come before its target
- Words alone have no internal direction; operators create direction

**Excellent answer example:**
```
"Words are symmetric - sol aqu = aqu sol.
But operators break symmetry by binding to what follows.

nu sol creates a chunk [NOT-solid].
This chunk then intersects with other words.

[nu-sol] aqu = aqu [nu-sol] (commutativity still works)
But [nu-sol] ≠ [sol-nu] (binding is directional)

Operators introduce structure into an otherwise orderless grammar."
```

---

## What This Teaches

**Primary Concept:** The `nu` negation operator and operator binding rules.

**Secondary Concepts:**
- Operators bind to immediately following word
- Operator-word chunks then participate in commutativity
- Negation creates complement regions
- Structure emerges from operators, not word order

**Grammar Rule:**
```
nu X = NOT-X
nu X Y = (NOT-X) AND Y
X nu Y = X AND (NOT-Y)
```

---

## Distribution Details

### Platform: Website (limn.xyz/shadow)

**Page Design:**
- Dark theme (the "shadow" metaphor)
- Text appears to emerge from darkness
- Subtle animation on `nu` - it flickers or inverts colors
- Form submission at bottom

**SEO/Discovery:**
- Hidden link on main page (found by viewing source)
- Twitter post provides direct link
- Discord announcement for Phase 2 solvers

### Submission Handling

**Form submission goes to:**
- Backend that evaluates answers
- OR Discord bot that receives submissions
- OR manual review thread

**Automated evaluation possible for Q1 and Q2:**
```python
Q1_VALID_NULIF = ["not alive", "non-living", "dead", "inanimate", "inorganic",
                  "crystal", "rust", "machine", "ai", "erosion"]
Q1_VALID_LIFNU = ["not growing", "stagnant", "stable", "mature", "adult",
                  "hibernat", "dormant", "static"]

Q2_VALID = ["liq nu aqu", "flo nu aqu", "nu aqu flo", "nu aqu liq",
            "blood", "oil", "mercury", "lava", "traffic", "electricity"]
```

---

## Unlock Mechanism

### What Players Receive:

**Partial Solve (2/3 questions):**
```
You understand the shadow, but not its edge.
Review your answers. The operator binds to what follows.
```

**Full Solve (3/3 questions):**

1. **Cipher Key Fragment 1/4**
   ```
   CIPHER FRAGMENT 1/4 UNLOCKED

   You have learned to negate. The shadow falls where you point it.

   Keep this safe:
   ░░▓▓░░██░░▓▓

   (A portion of a visual cipher key - actual implementation TBD)

   This fragment is meaningless alone.
   When you have all four, the final door opens.
   ```

2. **Expanded Vocabulary Access**
   ```
   VOCABULARY EXPANDED

   You have earned access to Domain 9: Operators & Modifiers

   nu = negation (inverts following constraint)
   ve = intensifier (narrows to prototype)
   so = weakener (expands to periphery)
   ma = maximizer (extreme degree)
   mi = minimizer (slight degree)

   More operators await in future puzzles.
   ```

3. **Discord Role:** "Shadow Walker"

4. **Puzzle 05 Teaser:**
   ```
   You can negate one word.
   But what if you need to negate TWO words together?
   What if you need to separate thoughts?

   The boundary awaits: |

   Look for it on Twitter in 48 hours.
   ```

---

## Hints (If Needed)

**Hint 1** (Visual hint on page, revealed after 24 hours):
```
THINK OF IT THIS WAY:

nu sol aqu
↓
[nu-sol] aqu
↓
[NOT-solid] AND water
↓
liquid water, steam, fog

---

sol nu aqu
↓
sol [nu-aqu]
↓
solid AND [NOT-water]
↓
dry stone, bone, metal
```

**Hint 2** (Added to page after 48 hours):
```
STILL STRUGGLING?

Question 1: What GROWS but is NOT ALIVE? What is ALIVE but does NOT GROW?
Question 2: Use the pattern `[word] nu aqu` for liquids that aren't water.
Question 3: Why can we swap `sol aqu` but not swap `nu` with its target?
```

**Hint 3** (Discord announcement):
```
HINT: THE BINDING RULE

nu always attaches to the word RIGHT AFTER it.
After attaching, the pair acts as one chunk.
The chunk can then be reordered with other words.

[nu-X] Y = Y [nu-X]   ← Still commutative!
But nu X ≠ X nu       ← The operator must come first
```

---

## Extended Challenge (Optional)

**For quick solvers, additional challenge on the page:**
```
SHADOW MASTER CHALLENGE

Consider the sentence:
nu hot nu col

Two operators. Two words. What does this mean?

(nu-hot) AND (nu-col) = NOT-hot AND NOT-cold = ?

What is neither hot nor cold?
Lukewarm? Room temperature? Temperate?
Or is this region empty? Can anything be neither hot nor cold?

Contemplate. There is no submission for this challenge.
The answer lives in the intersection of your thoughts.
```

This introduces the idea that double negation can create interesting constraint regions.

---

## Design Notes

### Why This Works:
1. **Natural progression:** Words → Intersection → Commutativity → Operators
2. **Clear examples:** `nu sol aqu` vs `sol nu aqu` is concrete
3. **Three-part structure:** Understanding, creation, explanation
4. **Introduces cipher:** First tangible "meta-puzzle" reward

### Connection to Limn:
This puzzle teaches the first operator and the crucial binding rule. It also shows that Limn has SOME structure (operator binding) even though word order is free.

### Potential Issues:
- The binding rule might be confusing
  - *Mitigation:* Visual diagrams showing the chunking process
- Players might think `nu` negates everything after it
  - *Mitigation:* Explicit comparison of `nu sol aqu` vs `sol nu aqu`
- The "cipher fragment" reward might seem arbitrary
  - *Mitigation:* Make it feel mysterious and valuable, tease the final puzzle

### Success Criteria:
- 50% of Phase 1 completers attempt Phase 2
- 40% solve this puzzle
- Average solve time: 1-2 hours
- Discord discussion is active (people helping each other)

---

## Narrative Thread

### Before Puzzle:
Players know words, intersection, and commutativity.

### After Puzzle:
Players understand negation and operator binding.

### Lore Connection:
The "shadow" metaphor connects negation to absence. To negate is to cast a shadow - to define by what is NOT there. The Limnographers understood that meaning comes both from presence and absence.

**Lore fragment (revealed to top solvers):**
```
RECOVERED FRAGMENT: The Shadow Teaching

"My student asked: 'How do I say what something is NOT?'

I drew a circle in the sand. 'This is sol. Solid. Inside the circle
is everything solid - rock, bone, ice, certainty.'

Then I drew the sand OUTSIDE the circle. 'This is nu sol. Not-solid.
Everything else. Liquid, gas, uncertainty, dreams.'

'But teacher,' she said, 'the outside is infinite. How can one word
contain infinity?'

'All words contain infinity,' I replied. 'The word sol contains
every grain of sand, every mountain, every frozen moment, every
stubborn truth. Infinity inside the circle too.'

'Then what is the difference between sol and nu sol?'

'The difference is where you stand. Inside or outside.
Both are regions. Both are infinite. Only the boundary differs.
And the boundary... the boundary is meaning.'

She drew another circle, overlapping mine.
'What is this?' she asked.

'That,' I said, 'is a sentence.'"

- Fragment from "The Shadow Teachings"
```

---

## Technical Implementation

### Website Page:

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>The Shadow | Limn</title>
    <style>
        body {
            background: #0a0a0a;
            color: #e0e0e0;
            font-family: 'Courier New', monospace;
            padding: 2rem;
            max-width: 800px;
            margin: 0 auto;
        }
        .nu {
            color: #ff4444;
            text-shadow: 0 0 10px #ff4444;
        }
        .reveal {
            opacity: 0;
            animation: fadeIn 2s forwards;
        }
        @keyframes fadeIn {
            to { opacity: 1; }
        }
        /* Stagger reveals */
        .reveal:nth-child(2) { animation-delay: 0.5s; }
        .reveal:nth-child(3) { animation-delay: 1s; }
        /* etc. */
    </style>
</head>
<body>
    <h1 class="reveal">THE FOURTH TRUTH</h1>

    <p class="reveal"><span class="nu">nu</span></p>

    <p class="reveal">The prefix that inverts. The operator that creates shadows.</p>

    <!-- Continue with content as specified -->

    <form id="submission" action="/api/puzzle-04" method="POST">
        <label for="q1">Question 1: Difference between nu lif gro and lif nu gro?</label>
        <textarea name="q1" id="q1" rows="4"></textarea>

        <label for="q2">Question 2: Create phrase for "flowing but not water"</label>
        <input type="text" name="q2" id="q2">

        <label for="q3">Question 3: Why does operator order matter?</label>
        <textarea name="q3" id="q3" rows="4"></textarea>

        <button type="submit">Submit to the Shadow</button>
    </form>
</body>
</html>
```

---

*"nu. The shadow operator. It inverts. It excludes. It defines by absence. The fourth truth: negation has direction. The grammar gains structure."*
