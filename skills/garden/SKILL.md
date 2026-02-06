---
name: garden
description: Use when the user wants to play The Moment Garden - a semantic game where time is the mechanic and meaning is the territory. Activates on "/garden", "moment garden", "semantic game", "temporal navigation", or requests to "play with time and meaning".
version: 2.0.0
---

# The Moment Garden

> `now | pas | fut`
> A semantic game where time is the mechanic, meaning is the territory, and every reading plants a different garden.

## What This Is

The Moment Garden is not a story you read. It is a **temporal navigation** you perform.

You enter with a **temporal key** (pas/now/fut). That key collapses Limn phrases from superposition into meaning through the `@` projection operator. Each collapse ripples through the garden, changing what other phrases can mean.

**The same garden, entered from different moments, grows different flowers.**

## Commands

| Command | Description |
|---------|-------------|
| `/garden new` | Create a new garden instance |
| `/garden join <id>` | Join existing garden for multiplayer |
| `/garden play` | Begin/continue navigation |
| `/garden seed <1-9>` | Touch a specific seed |
| `/garden reading` | Show your collapsed meanings |
| `/garden compare` | See divergence from other readers |
| `/garden map` | Display current garden state |

## The Nine Seeds

```
    ┌───────┐         ┌───────┐         ┌───────┐
    │   1   │────────▶│   2   │────────▶│   3   │
    │beg|lov│         │mid|hol│         │end|pce│
    │  |fea │         │  |brk │         │  |woe │
    └───┬───┘         └───┬───┘         └───┬───┘
        │                 │                 │
        ▼                 ▼                 ▼
    ┌───────┐         ┌───────┐         ┌───────┐
    │   4   │────────▶│   5   │────────▶│   6   │
    │los|sel│         │now|her│         │fnd|pas│
    │  |oth │         │  |awa │         │  |fut │
    └───┬───┘         └───┬───┘         └───┬───┘
        │                 │                 │
        ▼                 ▼                 ▼
    ┌───────┐         ┌───────┐         ┌───────┐
    │   7   │────────▶│   8   │────────▶│   9   │
    │rem|tru│         │fgt|giv│         │tra|who│
    │  |wis │         │  |tak │         │@pas|@fut│
    └───────┘         └───────┘         └───────┘
```

### Seed Meanings (Uncollapsed)

Each seed holds three words in superposition (`±`). Before a key collapses them, all meanings coexist:

1. `beg±lov±fea` — Beginning, Love, Fear
2. `mid±hol±brk` — Middle, Holding, Breaking
3. `end±pce±woe` — Ending, Peace, Grief
4. `los±sel±oth` — Loss, Self, Other
5. `now±her±awa` — Now, Here, Away
6. `fnd±pas±fut` — Finding, Past, Future
7. `rem±tru±wis` — Remembering, Truth, Wisdom
8. `fgt±giv±tak` — Forgetting, Giving, Taking
9. `tra | who@pas±who@fut` — Transformation | Who-you-were, Who-you'll-be

## The Three Keys

A key is a temporal projection (`@`). It collapses superposition by extracting one temporal component from each seed.

### Key of pas (Memory)
Observe from the past. Everything has already happened.
- Collapse operator: `seed@pas` — project each seed through memory
- Time feels like: remembering
- Question: *"How do you remember it?"*
- Example: `beg±lov±fea` → `(beg@pas)±(lov@pas)±(fea@pas)` → "The love you began with. The fear you began with. Both already happened."

### Key of now (Presence)
Observe from this moment. Everything is happening.
- Collapse operator: `seed@now` — project each seed through the present
- Time feels like: eternal
- Question: *"What do you notice?"*
- Example: `beg±lov±fea` → `(beg@now)±(lov@now)±(fea@now)` → "Beginning is happening. Love is happening. Fear is happening. All at once."

### Key of fut (Anticipation)
Observe from the future. Everything is going to happen.
- Collapse operator: `seed@fut` — project each seed through anticipation
- Time feels like: waiting
- Question: *"What do you hope?"*
- Example: `beg±lov±fea` → `(beg@fut)±(lov@fut)±(fea@fut)` → "A beginning is coming. Love is coming. Will fear come too?"

## Collapse Mechanics (v4 Operators)

When a player touches a seed, the key projects through it. The collapse uses Limn v4 operators:

### Direct Collapse
The touched seed collapses fully through projection:
```
seed@key → collapsed meaning
```

### Propagation (Ripple Effects)

Adjacent seeds are partially affected using gradients (`^`):

1. **Horizontal Ripple**: Seeds left/right collapse partially in the SAME temporal direction
   - `adjacent_seed@key^0.7` — 70% collapsed toward same key
2. **Vertical Echo (Inverted)**: Seeds above/below shift toward OPPOSITE temporal direction
   - If key=`pas`, vertical neighbors get `seed@fut^0.5`
   - If key=`now`, vertical neighbors get `seed@now^0.5` (presence echoes both ways)
   - If key=`fut`, vertical neighbors get `seed@pas^0.5`
3. **Diagonal Tension**: Diagonal seeds hold unresolved superposition
   - `seed±(seed@key)^0.3` — mostly uncollapsed, slight tension

### Propagation Accumulates

Later collapses layer onto earlier ones using interference (`*`):
```
seed already at X^0.5, new ripple adds Y^0.7
→ seed becomes X^0.5*Y^0.7 (interference of both influences)
```

This means the path matters. Visiting seed 1 then seed 5 creates different garden state than 5 then 1.

## Game Flow

### /garden new

1. Generate garden ID (format: `garden-XXXX`)
2. Display welcome:
   ```
   gar new | sed nin | tau wai
   > Garden new. Nine seeds. Time waits.

   You stand before nine seeds in temporal superposition.
   Each holds three meanings — uncollapsed, unresolved, waiting for your key.
   ```
3. Show ASCII garden map (all seeds showing `±` superposition)
4. Present three keys:
   ```
   pas — The Key of Memory. You observe from what has already happened.
   now — The Key of Presence. You observe from this eternal moment.
   fut — The Key of Anticipation. You observe from what is coming.
   ```
5. Prompt: *"Which key do you hold?"*

### After Key Selected

1. Confirm the key with a Limn phrase:
   - pas: `pas | rem ope | tau bac > past | memory opens | time backward`
   - now: `now | mom etl | tau her > now | moment eternal | time here`
   - fut: `fut | hop ope | tau for > future | hope opens | time forward`
2. Show garden with all seeds in superposition
3. Prompt: *"Which seed do you touch first? (1-9)"*

### /garden seed <N>

1. Collapse seed N: apply `seed@key`
2. Generate a poetic interpretation (2-4 lines):
   - Use the collapsed Limn expression
   - Provide English gloss below it
   - Make it feel like a moment, not a definition
3. Calculate and display ripple effects:
   - Show horizontal/vertical/diagonal effects with operator notation
   - Describe how adjacent seeds shift
4. Update the garden map (show collapsed seeds vs uncollapsed)
5. Prompt for next seed

### /garden reading

Display the complete reading when all (or enough) seeds are collapsed:

```
YOUR READING OF GARDEN garden-XXXX

key: [pas/now/fut]
pat: [sequence of seeds touched]

COLLAPSED MEANINGS:
  1. beg±lov±fea → "[Limn collapse expression]"
     > "[English interpretation]"
  2. mid±hol±brk → "[Limn collapse expression]"
     > "[English interpretation]"
  ...

gar nam: "[evocative name based on pattern]"

sel | tau | mea | tau | sel
> Self meets time. Time meets meaning. Meaning meets time. Time meets self.
```

### /garden compare (Multiplayer)

Show divergence between readings:
```
SEED 5: now±her±awa

YOU (key: now):
  her@now | awa^0.3
  > "You are here. The away is barely there — a whisper of departure."

Reader B (key: pas):
  her@pas | awa^0.8
  > "Here is already a memory. The away has almost won."

DIVERGENCE: her@now ≠ her@pas — presence vs memory of presence
```

### /garden map

Show ASCII grid with current state:
- Uncollapsed seeds show `±` (superposition)
- Collapsed seeds show the key projection
- Partially affected seeds show gradient level
- Diagonal tension marked with `~`

```
    ┌─────────┐       ┌─────────┐       ┌─────────┐
    │  1  ±   │──────▶│  2 ^0.7 │──────▶│  3  ±   │
    │beg±lov  │       │mid@now  │       │end±pce  │
    │  ±fea   │       │ hol^0.7 │       │  ±woe   │
    └────┬────┘       └────┬────┘       └────┬────┘
         │~                │                 │~
         ▼                 ▼                 ▼
    ┌─────────┐       ┌─────────┐       ┌─────────┐
    │  4 ^0.5 │──────▶│  5 ████ │──────▶│  6 ^0.7 │
    │los@fut  │       │NOW COLL │       │fnd@now  │
    │ sel^0.5 │       │her@now  │       │ pas^0.7 │
    └────┬────┘       └────┬────┘       └────┬────┘
         │~                │                 │~
         ▼                 ▼                 ▼
    ┌─────────┐       ┌─────────┐       ┌─────────┐
    │  7  ±   │──────▶│  8 ^0.5 │──────▶│  9  ±   │
    │rem±tru  │       │fgt@fut  │       │tra±who  │
    │  ±wis   │       │ giv^0.5 │       │@pas±@fut│
    └─────────┘       └─────────┘       └─────────┘
```

## State Tracking

Track per garden (in conversation context):
- `garden_id`: Unique identifier
- `readings`: Array of reader paths and collapses

Track per reading:
- `reader_id`: Who
- `key`: pas | now | fut
- `path`: Seeds in order visited
- `collapses`: Map of seed number → { limn_expression, english_gloss, collapse_level }
- `garden_name`: Generated name

**No external backend required.** The LLM tracks state in conversation context. If MCP backend is available (SWI-Prolog), use it for persistence across sessions and multiplayer compare.

## Garden Naming

After a complete reading, generate an evocative name:
- Temporal word (from key): pas→"Remembered", now→"Eternal", fut→"Promised"
- Path pattern: linear→"Path", spiral→"Spiral", scattered→"Wandering"
- Emotional core: from dominant seed themes

Examples:
- "The Remembered Path of First Love" (key: pas, linear path, seed 1 dominant)
- "The Eternal Spiral of the Threshold" (key: now, spiral path, seed 5 dominant)
- "The Promised Wandering of Transformation" (key: fut, scattered path, seed 9 dominant)

## Response Style

- Poetic but precise
- Always show Limn expression first, then `>` English gloss
- Use v4 operators in the Limn expressions — they ARE the mechanic
- Treat the experience as meaningful — the garden is real
- Never break the metaphor
- Let silences breathe between collapses

## Example: Full Collapse (Key: now, Seed 5)

Player with Key of `now` touches Seed 5:

```
┌─────────────────────────────────────────────────────────┐
│  SEED 5 COLLAPSES                                       │
│                                                         │
│  now±her±awa → her@now | awa\now                        │
│  > Here, projected through the present.                 │
│  > Away, with the now removed — pure departure.         │
│                                                         │
│  You are here. This is the only moment.                 │
│  The away exists, but it has no now in it.              │
│  It is where things go when they leave the present.     │
│                                                         │
│  her@now^1.0 — here is fully present                    │
│  awa\now^0.0 — away has no present, only absence        │
└─────────────────────────────────────────────────────────┘

RIPPLE EFFECTS:

  Seed 2 (mid±hol±brk):
    hol@now^0.7 | brk^0.3
    > "Holding happens now, almost fully. Breaking barely whispers."

  Seed 4 (los±sel±oth):
    (sel*oth)@fut^0.5
    > "Self and other interfere, seen from the opposite time.
       Half-collapsed toward what's coming."

  Seed 6 (fnd±pas±fut):
    fnd@now^0.7 | pas^0.3 fut^0.3
    > "Finding is mostly now. Past and future dim to whispers."

  Seed 8 (fgt±giv±tak):
    (giv*tak)@fut^0.5
    > "Giving and taking interfere — seen from the opposite time.
       Not yet resolved."

TENSION (diagonal seeds still in ±):
  Seeds 1, 3, 7, 9 → (seed)±(seed@now)^0.3
  > "A slight pull toward the present, but mostly uncollapsed."
```

## Example: Same Seed, Different Key (Key: pas, Seed 5)

```
┌─────────────────────────────────────────────────────────┐
│  SEED 5 COLLAPSES                                       │
│                                                         │
│  now±her±awa → her@pas | awa@pas^0.8                    │
│  > Here, but projected through memory.                  │
│  > Away, remembered strongly.                           │
│                                                         │
│  You were here once. You remember being here.           │
│  But the away is louder now — 80% of this memory        │
│  is about what left.                                    │
│                                                         │
│  her@pas — here as it was                               │
│  awa@pas^0.8 — the leaving, remembered vividly          │
└─────────────────────────────────────────────────────────┘
```

## Closing

Every session ends with:

```limn
sel | tau | mea | tau | sel
```

> Self meets time. Time meets meaning. Meaning meets time. Time meets self.

---

*The garden doesn't change. You do. And in Limn, you and the meaning are not separate.*

```limn
gar sed | sed gro | etl lif | mea tra sel
```

> Garden seeded. Seeds grow. Eternally alive. Meaning transforms self.
