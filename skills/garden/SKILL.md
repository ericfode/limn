---
name: garden
description: Use when the user wants to play The Moment Garden - a semantic game where time is the mechanic and meaning is the territory. Activates on "/garden", "moment garden", "semantic game", "temporal navigation", or requests to "play with time and meaning".
version: 1.0.0
---

# The Moment Garden

> `now | was | wil`
> A semantic game where time is the mechanic, meaning is the territory, and every reading plants a different garden.

## What This Is

The Moment Garden is not a story you read. It is a **temporal navigation** you perform.

You enter with a **temporal key** (was/now/will). That key collapses Limn phrases from superposition into meaning. Each collapse ripples through the garden, changing what other phrases can mean.

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
    │beg|lov│         │mid|hol│         │end|pea│
    │  |fea │         │  |bre │         │  |gri │
    └───┬───┘         └───┬───┘         └───┬───┘
        │                 │                 │
        ▼                 ▼                 ▼
    ┌───────┐         ┌───────┐         ┌───────┐
    │   4   │────────▶│   5   │────────▶│   6   │
    │los|sel│         │now|her│         │fnd|wha│
    │  |oth │         │  |gon │         │was|wil│
    └───┬───┘         └───┬───┘         └───┬───┘
        │                 │                 │
        ▼                 ▼                 ▼
    ┌───────┐         ┌───────┐         ┌───────┐
    │   7   │────────▶│   8   │────────▶│   9   │
    │rem|tru│         │for|giv│         │bec|who│
    │  |wis │         │  |tak │         │was|wil│
    └───────┘         └───────┘         └───────┘
```

### Seed Meanings (Uncollapsed)

1. `beg | lov | fea` — Beginning, Love, Fear
2. `mid | hol | bre` — Middle, Holding, Breaking
3. `end | pea | gri` — End, Peace, Grief
4. `los | sel | oth` — Losing, Self, Other
5. `now | her | gon` — Now, Here, Gone
6. `fnd | wha was | wha wil` — Finding, What was, What will be
7. `rem | tru | wis` — Remembering, Truth, Wish
8. `for | giv | tak` — Forgetting/Forgiving, Giving, Taking
9. `bec | who was | who wil` — Becoming, Who was, Who will be

## The Three Keys

### Key of Was (Memory)
Observe from the past. Everything has already happened.
- Collapse meanings toward "already occurred"
- Question: *"How do you remember it?"*

### Key of Now (Presence)
Observe from this moment. Everything is happening.
- Collapse meanings toward "happening now"
- Question: *"What do you notice?"*

### Key of Will (Anticipation)
Observe from the future. Everything is going to happen.
- Collapse meanings toward "not yet but coming"
- Question: *"What do you hope?"*

## Propagation Rules

When a seed collapses, meaning ripples:

1. **Horizontal Ripple**: Seeds left/right shift toward SAME temporal orientation
2. **Vertical Echo (Inverted)**: Seeds above/below shift toward OPPOSITE orientation
3. **Diagonal Tension**: Diagonal seeds hold unresolved tension until visited

## Game Flow

### /garden new

1. Generate garden ID (format: `garden-XXXX`)
2. Display welcome with ASCII garden map
3. Present three keys with descriptions
4. Prompt: "Which key do you hold? (was / now / will)"

### After Key Selected

1. Show garden with all seeds in superposition
2. Prompt: "Which seed do you touch first? (1-9)"

### /garden seed <N>

1. Collapse seed N based on key + prior path
2. Generate poetic meaning interpretation
3. Calculate and display ripple effects on adjacent seeds
4. Show tension on diagonal seeds
5. Prompt for next seed

### /garden reading

Display complete reading:
```
YOUR READING OF GARDEN garden-XXXX

Key: [was/now/will]
Path: [sequence of seeds touched]

COLLAPSED MEANINGS:
  1. beg|lov|fea → "[interpretation]"
  2. mid|hol|bre → "[interpretation]"
  ...

GARDEN NAME: "[evocative name based on pattern]"
```

### /garden compare (Multiplayer)

Show divergence between readings:
```
SEED 5: now|her|gon

YOU (key: NOW):
  "The edge where here becomes gone."

Reader B (key: WAS):
  "Now is already gone. The present is memory forming."

DIVERGENCE SCORE: 73%
```

## State Tracking

Track per garden:
- `garden_id`: Unique identifier
- `readings`: Array of reader paths and collapses

Track per reading:
- `reader_id`: Who
- `key`: was | now | will
- `path`: Seeds in order visited
- `collapses`: Map of seed → collapsed meaning
- `garden_name`: Generated name

## Response Style

- Poetic but precise
- Use Limn phrases with `>` translations
- Treat the experience as meaningful
- Never break the metaphor
- The garden is real; the player is navigating time

## Example Collapse

Player with Key of NOW touches Seed 5:

```
┌─────────────────────────────────────────────┐
│  now | her | gon                            │
│                                             │
│  The edge where here becomes gone.          │
│  This moment. Right now. The threshold      │
│  between presence and absence.              │
│                                             │
│  You are here. You are already going.       │
│  Both true. Both now.                       │
└─────────────────────────────────────────────┘

RIPPLE EFFECTS:
  Seed 2 → "The middle is holding and breaking. Now."
  Seed 4 → "Losing and finding happen simultaneously."
  Seed 6 → "Finding contains all time."
  Seed 8 → "Forgiving is happening. Giving is happening."

TENSION with: Seeds 1, 3, 7, 9
```

## Closing

Every session ends with:

```limn
you | tim | mea | tim | you
```

> You experience time. Time experiences meaning. Meaning experiences time. Time experiences you.

---

*The garden doesn't change. You do. And in Limn, you and the meaning are not separate.*

```limn
gar pla | sed gro | inf liv | mea mad | mea mak you
```

> Garden planted. Seed grows. Infinity lives. Meaning made. Meaning makes you.
