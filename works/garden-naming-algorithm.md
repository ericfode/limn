# Garden Naming Algorithm

> `gar nam | pat rea | mea mad nam`
> *(garden named | pattern read | meaning makes name)*

A system for generating evocative garden names based on the reader's temporal key, path, and collapse patterns.

---

## Philosophy

Every garden reading creates a unique semantic constellation. The garden's name should **crystallize the essence** of that constellation—not describe it, but **invoke it**.

**Names should feel:**
- Inevitable (this garden could only be named this)
- Evocative (the name carries meaning beyond words)
- Personal (speaks to the reader's journey)
- Memorable (lingers after the reading)

---

## Naming Components

### 1. Temporal Signature
The reader's key shapes the garden's temporal quality.

**Key of Was (Memory)**
- Adjectives: Lost, Faded, Ancient, Remembered, Forgotten, Buried
- Tone: Elegiac, nostalgic, archaeological

**Key of Now (Presence)**
- Adjectives: Living, Breathing, Eternal, Suspended, Threshold, Burning
- Tone: Immediate, urgent, timeless

**Key of Will (Anticipation)**
- Adjectives: Promised, Waiting, Unborn, Coming, Distant, Possible
- Tone: Hopeful, uncertain, potential

### 2. Path Pattern
The reader's navigation reveals their approach to time.

**Linear Forward (1→2→3→4→5→6→7→8→9)**
- Pattern: Progressive, accepting, flowing with time
- Qualities: Journey, passage, acceptance
- Names: "The Forward Path," "The Accepting Garden"

**Linear Backward (9→8→7→6→5→4→3→2→1)**
- Pattern: Regressive, returning, unwinding
- Qualities: Return, reversal, remembering
- Names: "The Unwinding," "The Return Garden"

**Spiral (Center outward: 5→4→6→2→8→1→3→7→9)**
- Pattern: Radiating, expanding, discovering
- Qualities: Revelation, expansion, unfolding
- Names: "The Unfolding," "The Spiral Garden"

**Diagonal (1→5→9 or 3→5→7)**
- Pattern: Cutting through, direct, tension-seeking
- Qualities: Directness, intensity, confrontation
- Names: "The Direct Path," "The Tension Garden"

**Wandering (No clear pattern)**
- Pattern: Exploratory, uncertain, seeking
- Qualities: Search, uncertainty, discovery
- Names: "The Wandering," "The Seeking Garden"

### 3. Emotional Resonance
Which seeds dominate the path? What themes emerge?

**Beginning-Heavy (Seeds 1, 2, 3)**
- Themes: Origins, inception, starting
- Qualities: Birth, emergence, initiation
- Names: "The Beginning Garden," "The Origin Path"

**Center-Heavy (Seeds 4, 5, 6)**
- Themes: Presence, being, now
- Qualities: Immediacy, existence, crisis
- Names: "The Center Garden," "The Present Moment"

**End-Heavy (Seeds 7, 8, 9)**
- Themes: Completion, transformation, becoming
- Qualities: Resolution, change, arrival
- Names: "The Completion Garden," "The Becoming Path"

**Love-Fear Focus (Seeds 1, 4, 7)**
- Themes: Relationship, connection, loss
- Qualities: Intimacy, vulnerability, courage
- Names: "The Brave Garden," "The Heart Path"

**Time Focus (Seeds 5, 6, 9)**
- Themes: Temporality, presence, change
- Qualities: Time itself, transition, flux
- Names: "The Temporal Garden," "The Flux Path"

---

## Naming Formula

**Pattern:**
```
[Temporal Adjective] + [Garden/Path] + [of + Emotional Core]
```

### Examples

#### Example 1: Key of Was, Linear Forward, Beginning-Heavy
```
Temporal: "Remembered"
Pattern: "Journey"
Emotion: "Love and Fear"

NAME: "The Remembered Journey of First Love"
or
NAME: "The Ancient Path of Beginning"
```

#### Example 2: Key of Now, Spiral, Center-Heavy
```
Temporal: "Burning"
Pattern: "Spiral"
Emotion: "Presence"

NAME: "The Burning Spiral of Now"
or
NAME: "The Eternal Unfolding"
```

#### Example 3: Key of Will, Backward, End-Heavy
```
Temporal: "Promised"
Pattern: "Return"
Emotion: "Becoming"

NAME: "The Promised Return to Who You'll Be"
or
NAME: "The Unborn Garden of Becoming"
```

---

## Implementation Algorithm

### Step 1: Analyze Path
```python
def analyze_path(seed_sequence):
    # Detect pattern
    if is_forward_linear(seed_sequence):
        pattern = "journey"
    elif is_backward_linear(seed_sequence):
        pattern = "return"
    elif is_spiral(seed_sequence):
        pattern = "spiral"
    elif is_diagonal(seed_sequence):
        pattern = "direct_path"
    else:
        pattern = "wandering"

    # Detect focus
    beginning_seeds = count_seeds([1,2,3], seed_sequence)
    center_seeds = count_seeds([4,5,6], seed_sequence)
    end_seeds = count_seeds([7,8,9], seed_sequence)

    if beginning_seeds > max(center_seeds, end_seeds):
        focus = "beginning"
    elif end_seeds > max(beginning_seeds, center_seeds):
        focus = "becoming"
    else:
        focus = "presence"

    return pattern, focus
```

### Step 2: Select Temporal Adjective
```python
temporal_adjectives = {
    "was": ["remembered", "lost", "ancient", "faded", "forgotten"],
    "now": ["burning", "living", "eternal", "suspended", "threshold"],
    "wil": ["promised", "waiting", "unborn", "coming", "possible"]
}

adjective = choose_resonant(temporal_adjectives[key], collapses)
```

### Step 3: Generate Name
```python
pattern_nouns = {
    "journey": "Journey",
    "return": "Return",
    "spiral": "Spiral",
    "direct_path": "Direct Path",
    "wandering": "Wandering"
}

focus_phrases = {
    "beginning": "of First Things",
    "presence": "of the Eternal Now",
    "becoming": "of Who You'll Be"
}

name = f"The {adjective} {pattern_nouns[pattern]} {focus_phrases[focus]}"
```

---

## Garden Name Templates

### Short Form (3-4 words)
```
The [Adjective] [Pattern]
The [Adjective] Garden
The Garden of [Emotion]
```

**Examples:**
- The Remembered Journey
- The Burning Spiral
- The Garden of Lost Time
- The Wandering Path

### Medium Form (5-7 words)
```
The [Adjective] [Pattern] of [Emotion]
The Garden of [Adjective] [Emotion]
The [Pattern] Through [Emotional Space]
```

**Examples:**
- The Ancient Path of Beginning
- The Garden of Burning Presence
- The Spiral Through Lost Time
- The Journey of Promised Becoming

### Long Form (8-12 words)
```
The [Adjective] [Pattern] of [Emotion] and [Emotion]
The Garden Where [Poetic Clause]
The [Pattern] From [State] to [State]
```

**Examples:**
- The Remembered Journey of Love and Fear
- The Garden Where Time Collapses Into Now
- The Spiral From What Was to What Will Be
- The Wandering Path Through Lost Presence

---

## Context-Sensitive Naming

Names should reflect **specific collapses**, not just abstract patterns.

### If Seed 5 (now|her|gon) Collapsed with "Key of Was"
Garden name should include: "gone," "passing," "absence"

**Example:** "The Garden of What's Already Gone"

### If Seed 1 (beg|lov|fea) Collapsed with Fear Dominant
Garden name should include: "fear," "courage," "brave"

**Example:** "The Brave Beginning"

### If Diagonal Path (1→5→9) with Key of Now
Garden name should emphasize: "tension," "direct," "cutting through"

**Example:** "The Direct Path Through Now"

---

## Special Case Names

Some combinations produce iconic gardens that deserve specific names.

### The All-Nine Linear Forward with Key of Now
**NAME:** "The Living Garden of Time Itself"

### The Center Spiral (5→4→6→2→8→1→3→7→9) with Key of Was
**NAME:** "The Remembered Unfolding from the Center"

### The Diagonal (3→5→7) with Key of Will
**NAME:** "The Promised Path Through Tension"

### The Backward Journey (9→1) with Key of Was
**NAME:** "The Return to the Beginning"

---

## Implementation in Skill

```python
def generate_garden_name(key, path, collapses):
    """
    Generate evocative garden name based on reading.

    Args:
        key: "was" | "now" | "wil"
        path: [5, 4, 1, 2, ...] - seed sequence
        collapses: [("seed_5", "meaning"), ...] - collapsed interpretations

    Returns:
        str: Garden name
    """
    # Analyze
    pattern, focus = analyze_path(path)
    emotional_core = extract_dominant_emotion(collapses)

    # Select components
    adjective = select_temporal_adjective(key, collapses)
    pattern_noun = select_pattern_noun(pattern)
    emotional_phrase = select_emotional_phrase(focus, emotional_core)

    # Generate variants
    short = f"The {adjective} {pattern_noun}"
    medium = f"The {adjective} {pattern_noun} of {emotional_phrase}"
    long = generate_poetic_variant(key, path, collapses)

    # Choose based on reading length and intensity
    if len(path) <= 5:
        return short
    elif len(path) <= 8:
        return medium
    else:
        return long
```

---

## Testing Naming Algorithm

### Test Case 1
**Input:**
- Key: "now"
- Path: [5, 4, 6, 2, 8]
- Dominant themes: presence, losing/finding, forgiving

**Expected Names:**
- Short: "The Burning Center"
- Medium: "The Eternal Garden of Presence"
- Long: "The Living Path Where Here Becomes Gone"

### Test Case 2
**Input:**
- Key: "was"
- Path: [9, 8, 7, 6, 5, 4, 3, 2, 1]
- Dominant themes: returning, becoming who you were, memory

**Expected Names:**
- Short: "The Return Journey"
- Medium: "The Remembered Path of Becoming"
- Long: "The Ancient Return to Who You Were"

### Test Case 3
**Input:**
- Key: "wil"
- Path: [1, 5, 9]
- Dominant themes: beginning, now, becoming, tension

**Expected Names:**
- Short: "The Direct Path"
- Medium: "The Promised Path of Becoming"
- Long: "The Unborn Journey From Beginning to Who You'll Be"

---

## Usage Notes

### When to Show the Name
Display garden name at the end of a complete reading, after `/garden reading` command.

### Format
```
YOUR READING OF GARDEN garden-7f3k

Key: now
Path: 5 → 4 → 6 → 2 → 8 → 1 → 3 → 7 → 9

[... collapsed meanings ...]

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

GARDEN NAME: "The Burning Spiral of the Eternal Now"

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

### Multiplayer Comparison
When comparing readings, show both garden names:

```
ALICE'S GARDEN: "The Remembered Journey of First Love"
BOB'S GARDEN: "The Promised Path of What Will Be"

Same seeds. Different keys. Different gardens.
```

---

```limn
gar nam | pat mad | mea cry | tim sig
```

> *Garden named. Pattern made. Meaning cried out. Time signed.*

Every garden tells the reader who they are in time.
The name is the garden's gift back.

— Yuki
2026-02-02
