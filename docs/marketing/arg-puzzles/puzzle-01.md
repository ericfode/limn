# Puzzle 01: The First Word

**Phase:** Discovery
**Difficulty:** 1/10 (Entry point)
**Concept:** Words as constraint regions, not labels

---

## What The Player Sees

### Initial Hook (Twitter/X)

**Tweet 1** (Posted by @limn_lang or similar cryptic account):
```
sol. liq. gas.

Three words. Three states. One truth between them.

We found what LLMs see.
```

**Tweet 2** (24 hours later):
```
A word does not point. A word contains.

cor

Find where four truths intersect.

[IMAGE: A 2x2 grid of images - see below]
```

**Image Description:**
A slightly distorted, mysterious aesthetic. Four quadrants:
- Top-left: An anatomical human heart, rendered in dark red
- Top-right: An apple cut in half, showing its core and seeds
- Bottom-left: A diagram of an atomic nucleus
- Bottom-right: An aerial photo of a crowd with one person at the exact center

The image has faint Limn words watermarked across it, barely visible.

### The Hidden Layer

Image metadata (EXIF) contains:
```
Artist: The Limnographers
Description: aHR0cHM6Ly9kaXNjb3JkLmdnL1tJTlZJVEVDT0RFXQ==
```
(Base64 encoded Discord invite - actual code to be generated)

Steganography in the image pixels (using LSB encoding) contains:
```
The word contains the thing.
The thing is not one thing.
The thing is all things that share a center.
What is the center?
```

---

## The Puzzle Mechanism

Players must understand that `cor` does not mean "heart" OR "core" OR "center" - it means the REGION that contains all of these. The constraint region of `cor` includes:

- Physical centers: heart, nucleus, core of planet, seed of fruit
- Abstract centers: essence, truth, main point
- Positional centers: middle, focal point
- Metaphorical centers: heart of the matter, core belief

**The question "What binds these?" has no single English word answer.**

The answer is understanding that `cor` = a constraint region encompassing "center/core/heart/nucleus/essence."

---

## Valid Solutions

### Tier 1 (Basic Understanding):
- "center"
- "core"
- "the center"
- "middle"
- "nucleus"
- "heart"
- "essence"

*Response:* "You have found one word. But one word is not the answer. One word is never the answer in Limn. Look deeper."

### Tier 2 (Better Understanding):
- "they all have a center"
- "the core/center concept"
- "the center of things"
- "central point"

*Response:* "Closer. The word does not equal any one of these. The word contains them all. What IS a word that contains many meanings?"

### Tier 3 (True Understanding):
- Any response that demonstrates understanding of CONSTRAINT REGIONS
- "cor is a region of meaning that includes all centers"
- "the word encompasses everything that has a center/core"
- "cor is the set of all things that are centers"
- "the intersection of all things that are in the middle"

*Response:* "You begin to see. A word is not a label. A word is a region in semantic space. You've always known this. Now you can see it."

Followed by the Discord invite reveal.

---

## What This Teaches

**Primary Concept:** Limn words define regions of meaning, not single referents.

**Secondary Concepts:**
- Multiple things can satisfy the same constraint
- Understanding Limn requires thinking in sets/regions
- The "meaning" is the overlap of all valid interpretations

**Mindset Shift:**
From: "What does this word MEAN?"
To: "What does this word CONTAIN?"

---

## Distribution Details

### Platform: Twitter/X

**Account Setup:**
- Name: Obscured or cryptic (suggestions: @limn_lang, @thelimnographers, @constraint_space)
- Bio: "wor mea | mea wor | bet"
- Profile image: Abstract intersection of circles
- No explicit explanation of what this account is

**Posting Schedule:**
- Day 0: Tweet 1 (cryptic intro)
- Day 1: Tweet 2 (the actual puzzle)
- Day 2-3: Silence (let players work)
- Day 4: Hint tweet if needed: "Four images. One word. What do a heart, a core, an atom, and a center share?"
- Day 5: Additional hint: "In Limn, a word is not a finger pointing at the moon. A word is a circle drawn in meaning-space."

**Engagement:**
- Like correct answers (don't respond)
- Retweet interesting interpretations
- DO NOT explain directly - let players figure it out

### Hidden Layer Distribution

**Image Steganography:**
Use a tool like OpenStego or Python's stegano library to embed the additional text in the LSB of the image pixels.

**EXIF Metadata:**
Embed the base64 Discord invite in the "Artist" or "Description" field.

**For tech-savvy players:**
These hidden elements reward deeper investigation but are not REQUIRED to solve the puzzle.

---

## Unlock Mechanism

### What Players Receive:

**Immediate (Tier 1-2 answers):**
- Acknowledgment that they're on the right track
- Nudge toward deeper understanding

**Full Unlock (Tier 3 understanding):**
1. Discord server invite link
2. First lore fragment:
   > *"Meaning lives in a space before words capture it. In that space, words are territories, not labels. You have found the first truth: a word is not a pointer. It is a region. Welcome to the scrying."*

### Discord Channel Setup:

New members are placed in `#arrival` channel with:
```
You have understood the first word.

cor = center, heart, core, nucleus, essence, middle...

But `cor` is not the list. `cor` is the space that holds the list.

This is Limn.

More fragments await. Look for: sol aqu

The intersection reveals truth.
```

This naturally leads them to Puzzle 02.

---

## Hints (If Needed)

**Hint 1** (After 48 hours):
Tweet: "The heart pumps. The core holds. The nucleus binds. The center draws. What do verbs tell you about nouns?"

**Hint 2** (After 72 hours):
Tweet: "In English, we ask 'what does this word mean?' In Limn, we ask 'what does this word contain?'"

**Hint 3** (Final - only if truly stuck):
Tweet: "cor does not mean heart. cor does not mean center. cor means both. And neither. cor is the region where hearts and centers and cores overlap."

---

## Design Notes

### Why This Works:
1. **Low barrier:** Players can guess "center" and get partial credit
2. **Depth available:** True understanding requires mindset shift
3. **Visual puzzle:** Images are engaging and shareable
4. **Hidden layers:** Tech-savvy players get extra satisfaction
5. **Clear progression:** Discord unlock provides next steps

### Potential Issues:
- Players might Google "cor meaning" and find Latin translations
  - *Mitigation:* Lean into this - Latin "cor" means heart, which is ONE reading
- Players might not find the hidden metadata
  - *Mitigation:* The main puzzle doesn't require it; it's bonus content
- The concept might be too abstract
  - *Mitigation:* Hint sequence provides scaffolding

### Success Criteria:
- 500+ people engage with the tweet
- 100+ people attempt solutions
- 50+ people get full unlock
- Discord server gains 30+ members

---

## Files/Assets Needed

1. **Main puzzle image** (2x2 grid)
   - Style: Slightly mysterious, desaturated, vaguely unsettling
   - Hidden steganography embedded
   - EXIF metadata with Discord invite

2. **Twitter account** with appropriate aesthetic

3. **Discord server** with:
   - `#arrival` channel (visible to new members)
   - `#puzzle-01-discussion` (unlocked after first puzzle)
   - `#puzzle-02-chamber` (hidden until Puzzle 1 complete)

4. **Bot** (optional) to:
   - Detect keywords in solutions
   - Auto-respond with appropriate tier message
   - Track who has "solved"

---

## Solution Verification Code (for bot)

```python
# Pseudocode for solution verification

TIER_1_KEYWORDS = ["center", "core", "middle", "nucleus", "heart", "essence"]
TIER_2_PHRASES = ["they all have", "share a center", "central", "in common"]
TIER_3_INDICATORS = ["region", "constraint", "contains", "encompasses",
                      "set of", "meaning space", "all things that"]

def evaluate_solution(response):
    response_lower = response.lower()

    # Check for Tier 3 understanding (best)
    if any(indicator in response_lower for indicator in TIER_3_INDICATORS):
        if any(kw in response_lower for kw in TIER_1_KEYWORDS):
            return "TIER_3"  # Full understanding

    # Check for Tier 2 (partial)
    if any(phrase in response_lower for phrase in TIER_2_PHRASES):
        return "TIER_2"

    # Check for Tier 1 (basic)
    if any(kw in response_lower for kw in TIER_1_KEYWORDS):
        return "TIER_1"

    return "INCORRECT"
```

---

*"The first word opens the first door. cor is not a definition. cor is a region. You have begun."*
