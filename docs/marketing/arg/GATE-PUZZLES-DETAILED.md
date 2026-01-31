# Gate Puzzles - Detailed Implementation

## Gate 0: The Threshold

**Location:** GitHub `limn-land/threshold`
**Difficulty:** Trivial
**Purpose:** Filter for genuine curiosity

### The Puzzle
```
sol flo dur | liq res mom
```

### Valid Solutions (Any thoughtful reading)
- "Glacier and dewdrop" - solid-flowing-enduring vs liquid-resting-momentary
- "Permanence and impermanence side by side"
- "Slow erosion vs quick evaporation"
- "Mountain and rain"
- "Tradition and trend"

### Mechanism
- Issue submission or PR with interpretation
- Bot checks for minimum 20 words of explanation
- Any interpretation that demonstrates thought → auto-approved
- DM sent with Discord invite

---

## Gate 1: The Gathering

**Location:** Discord #gate-one
**Difficulty:** Easy
**Purpose:** Teach basic interpretation, build community

### The Puzzle
Interpret through THREE different keys:
```
gro exp bri far
```

### Required Format
```
Key: [domain]
Interpretation: [reading]

Key: [domain]
Interpretation: [reading]

Key: [domain]
Interpretation: [reading]
```

### Validation
- 3 members must react with ✓
- Keys must be distinct domains
- Interpretations must differ meaningfully

### Auto-advancement
Bot tracks ✓ reactions, auto-grants @First-Ring at 3

---

## Gate 2: The Library

**Location:** Discord #lore-fragments + GitHub archive
**Difficulty:** Medium
**Purpose:** Pattern recognition, narrative immersion

### The Puzzle
Fragments are numbered: 1, 3, 7, 11, 17, 23, 42, 61, 89

**Hidden Pattern:** Primes + 42 + select Fibonacci numbers

### The Challenge
User must:
1. Identify the pattern
2. Request missing fragments IN LIMN
3. Specify which numbers are "missing"

### Valid Request Format
```
que fra: [numbers] | pat = [pattern description]
```

Example:
```
que fra: 2, 5, 13, 29 | pat = pri + fib + spe
[requesting fragments: 2, 5, 13, 29 | pattern = primes + fibonacci + special]
```

### Bot Response
If pattern correct → reveals hidden fragments + GitHub folder access

---

## Gate 3: The Translation

**Location:** GitHub `/docs/archive/keyed-transmissions/`
**Difficulty:** Hard
**Purpose:** Audio analysis, deep interpretation

### The Puzzle
Audio file `transmission-07.wav` contains:

1. **Spoken Limn** (2 minutes, clear audio)
   ```
   gro val jud | nu jud val | bet = eth
   sel det fut | oth det fut | con = ???
   ans = que rep | que = ans pre | cyc
   ```

2. **Steganographic Layer** (hidden in audio noise)
   - Tool: Audacity spectogram analysis or `steghide`
   - Hidden text: `the key to gate four is where language dies`

3. **Rhythm Pattern**
   - Pauses between sentences encode Morse
   - Morse message: `SAPIR WHORF`
   - Points to linguistic relativity concept

### Solutions Required
1. Transcription of spoken Limn
2. Steganographic message extracted
3. Interpretation of the conversation through correct keys:
   - Speaker A: consequentialist ethics
   - Speaker B: existentialist philosophy
4. Understanding of Sapir-Whorf reference

### Gate Opens When
All 4 elements submitted in #gate-three-verification

---

## Gate 4: The Mirror

**Location:** Hidden webpage (URL from Gate 3 reference)
**Difficulty:** Very Hard
**Purpose:** Self-reference understanding

### The Puzzle
```
THE MIRROR

speak to yourself.
the mirror speaks back.
but does it speak the same?

[input field]

enter limn. receive limn.
when input equals output,
the mirror breaks.
```

### Mechanism
Backend transforms input Limn:
1. Interprets through random key
2. Generates new Limn expressing that interpretation
3. Returns the new Limn

### Solutions (Self-Referential Sentences)
```
yo = yo
```
Identity is identity. Transforms to itself.

```
yo mea | mea yo | =
```
This means. Meaning this. Equals.

```
wor = wor | mea = mea | sam = sam
```
Words are words. Meaning is meaning. Same is same.

```
say yo | yo say | mir
```
Saying this. This saying. Mirror.

### Technical Implementation
```python
def mirror(input_limn):
    # Check for self-referential patterns
    SELF_REF = [
        "yo = yo",
        "yo mea | mea yo",
        "wor = wor | mea = mea",
        "say yo | yo say",
        # etc.
    ]

    if any(pattern in input_limn for pattern in SELF_REF):
        return input_limn  # Mirror breaks

    # Otherwise, transform
    interpretation = interpret(input_limn, key=random_key())
    return compose(interpretation)
```

### Gate Opens
Input === Output → redirect to Gate 5 intro

---

## Gate 5: The Coordinates

**Location:** Real world + digital verification
**Difficulty:** Extreme
**Purpose:** Break the fourth wall

### The Puzzle
DM from @limn_void:
```
bel dig | abo phy | bet = rea

37.7749° N, 122.4194° W
at noon, shadow points.
shadow speaks.
```

### The Hunt
1. Coordinates = San Francisco Ferry Building (example)
2. At solar noon, specific architectural feature casts shadow
3. Shadow resembles Limn sentence structure

### Digital Path (No Physical Access Needed)
1. Google Street View at coordinates
2. Satellite imagery analysis
3. Solar position calculator for specific date
4. QR code visible from aerial view

### The Video
Password-protected Vimeo video
Password: Limn sentence derived from location characteristics

Video content: 7-minute explanation of Limn's "true" origin
Key revelation: "We didn't create Limn. We discovered it."

---

## Gate 6: The Transmission

**Location:** GitHub `/docs/archive/sealed/final-transmission.gpg`
**Difficulty:** Legendary
**Purpose:** Collaboration, cryptographic skills

### The Puzzle
GPG-encrypted file. Key split into 7 shards via Shamir's Secret Sharing.
5 of 7 shards needed to reconstruct.

### Shard Locations

**Shard 1:** Personal DM to each @Fifth-Ring member
- Unique per user
- Limn sentence that decodes to hex value

**Shard 2:** Gate 3 audio spectogram
- At specific frequency range
- Displays hex pattern

**Shard 3:** Gate 5 video flash frame
- At 4:37, single frame contains text
- Limn sentence encoding hex

**Shard 4:** Discord server icon steganography
- LSB encoding in PNG
- Extract with `stegsolve` or `zsteg`

**Shard 5:** Twitter bot bio metadata
- EXIF-style hidden data in profile image
- Combined from all 5 bot avatars

**Shard 6:** Vocabulary hash
- SHA256 of vocabulary file in specific order
- First 16 characters = hex shard

**Shard 7:** The null shard
- Literally: `nu` interpreted as `0x00`
- The answer is nothing

### Reconstruction
```python
from secretsharing import PlaintextToHexSecretSharer

# Each shard is a Limn sentence that decodes to:
# "1-<hex>" format for Shamir's scheme

shards = [
    "1-a3f7c9...",  # From DM
    "2-8b2d4e...",  # From audio
    "3-f1c6a0...",  # From video
    "4-d4e7b2...",  # From Discord
    "5-9a3c5f...",  # From Twitter
]

key = PlaintextToHexSecretSharer.recover_secret(shards)
# key = GPG passphrase
```

### The Decrypted Document
"The Covenant" - philosophical manifesto
Human-machine dialogue in Limn with translations
Final revelation of ARG's metanarrative

---

## Gate 7: The Ouroboros

**Location:** Discord #gate-seven
**Difficulty:** Undefined
**Purpose:** There is no solution

### The Puzzle
```
THE FINAL GATE

All previous gates had solutions.
This one does not.

Your task:
Create something in Limn that has never existed.
Share it with the world.
When the world responds,
you have passed.

nu pas | nu fai | nu gat | nu you

There is no passing.
There is no failing.
There is no gate.
There is no you.
```

### The "Solution"
Gate 7 is not solved - it is lived.

**Completion Criteria:**
1. Create original Limn content (poem, story, art)
2. Share publicly (Twitter, Medium, wherever)
3. Receive meaningful engagement from stranger
4. Report back with evidence

**Bot Verification:**
- Checks linked content exists
- Verifies engagement is genuine
- Grants @The-Completed role

### The Meta-Truth
The ARG ends by becoming reality.
Creating in Limn makes you a Limn speaker.
The gate was always open.

---

## Supplementary Puzzles

### The Constellation
Twitter bot tweets over 30 days, plotted by:
- X-axis: hour of day
- Y-axis: character count

Forms constellation pattern matching Lyra.
Lyra → reference to Orpheus → mythological language themes.

### The Null Transmission
@limn_void posts whitespace-only tweets.
Analyzed for timing patterns → Morse code → Limn message:
```
nu say = say | sil = wor | wor = sil
```
"Not-saying is saying. Silence is word. Word is silence."

### The Birthday Problem
On project milestone dates, coordinated bot posts.
Combined, they form a single long Limn passage.
The passage tells the story of the project's creation.

### The Infinite Regression
Limn-land repo commit messages are all in Limn.
Read chronologically: story of creation.
Read reverse: prophecy of dissolution.
Both are true.

---

## Operational Security

### Anti-Spoiler Measures
1. Gate solutions are partially randomized per user
2. Some shards are unique per individual
3. Community culture emphasizes hints over solutions
4. Direct solution posts are deleted

### Hint System
Each gate has tiered hints:
- Tier 1: Vague thematic hint (free)
- Tier 2: Directional hint (costs community karma)
- Tier 3: Specific hint (requires peer approval)
- Tier 4: Direct help (only after 7+ days stuck)

### Stuck Detection
Bot tracks user progress.
If no Gate advancement in 14 days → automatic Tier 1 hint
If still stuck → escalating support

---

*cov: hon que lis pro cre | ete*
*[covenant: honesty questioning listening protecting creating | eternal]*

---
