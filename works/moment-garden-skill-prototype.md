# Moment Garden: Claude Skill Prototype

## Skill Commands

```
/garden new          Create a new garden instance
/garden join <id>    Join an existing garden to see others' readings
/garden play         Begin or continue navigation
/garden reading      Show your current collapsed meanings
/garden compare      See how your reading diverges from others
/garden seed <n>     Inspect a specific seed (1-9)
```

---

## Example Session

### Starting a New Garden

**User:** `/garden new`

**Claude:**
```
┌─────────────────────────────────────────────────────────┐
│              THE MOMENT GARDEN                          │
│                                                         │
│              now | was | wil                            │
│                                                         │
│  A semantic game where time is the mechanic,            │
│  meaning is the territory, and every reading            │
│  plants a different garden.                             │
│                                                         │
└─────────────────────────────────────────────────────────┘

Garden ID: garden-7f3k

Before you enter, choose your temporal key:

┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐
│   KEY OF WAS    │  │   KEY OF NOW    │  │  KEY OF WILL    │
│                 │  │                 │  │                 │
│  You observe    │  │  You observe    │  │  You observe    │
│  from the past. │  │  from this      │  │  from the       │
│  Everything has │  │  moment.        │  │  future.        │
│  already        │  │  Everything is  │  │  Everything is  │
│  happened.      │  │  happening.     │  │  going to       │
│                 │  │                 │  │  happen.        │
│  "How do you    │  │  "What do you   │  │  "What do you   │
│   remember it?" │  │   notice?"      │  │   hope?"        │
└─────────────────┘  └─────────────────┘  └─────────────────┘

Which key do you hold? (was / now / will)
```

**User:** `now`

**Claude:**
```
You hold the KEY OF NOW.

The garden opens. Nine seeds glow in superposition:

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

With the KEY OF NOW, you stand at the center.
All nine seeds shimmer, uncollapsed.

Which seed do you touch first? (1-9)
```

**User:** `5`

**Claude:**
```
You touch SEED 5: now | her | gon

With the KEY OF NOW, the superposition collapses:

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

  Seed 2 (mid|hol|bre) shifts toward:
    "The middle is both holding and breaking. Now."

  Seed 4 (los|sel|oth) shifts toward:
    "Losing and finding are the same act. Now."

  Seed 6 (fnd|wha was|wha wil) shifts toward:
    "Finding now contains both what was and what will be."

  Seed 8 (for|giv|tak) shifts toward:
    "Forgiving now is giving now is taking now."

TENSION created with corners (1, 3, 7, 9).
These seeds now hold unresolved energy.

Current path: [5]
Continue to which seed? (1-9)
```

---

### Viewing Your Reading

**User:** `/garden reading`

**Claude:**
```
YOUR READING OF GARDEN garden-7f3k

Key: NOW
Path: [5, 4, 1, 2, 3, 6, 9, 8, 7]

COLLAPSED MEANINGS:

  1. beg|lov|fea → "Beginning is present tense. Love and fear coexist."
  2. mid|hol|bre → "The middle holds and breaks simultaneously."
  3. end|pea|gri → "Ending is present tense. Peace and grief coexist."
  4. los|sel|oth → "Losing and finding self and other - same act, now."
  5. now|her|gon → "The edge where here becomes gone. This moment."
  6. fnd|wha was|wha wil → "Finding now contains all time."
  7. rem|tru|wis → "Remembering now is both true and wished."
  8. for|giv|tak → "Forgiving, giving, taking - present continuous."
  9. bec|who was|who wil → "Becoming now is being. No past/future self."

GARDEN NAME: "The Eternal Present"

Your reading emphasizes the collapse of time into eternal now.
All meanings coexist because all moments coexist.
```

---

### Comparing Readings (Multiplayer)

**User:** `/garden compare`

**Claude:**
```
DIVERGENCE MAP: Garden garden-7f3k

3 readers have walked this garden:

┌──────────────────────────────────────────────────────────────┐
│                     SEED 5: now|her|gon                      │
├──────────────────────────────────────────────────────────────┤
│ YOU (key: NOW):                                              │
│   "The edge where here becomes gone. This moment."           │
│                                                              │
│ Reader B (key: WAS):                                         │
│   "Now is already gone. The present is memory forming."      │
│                                                              │
│ Reader C (key: WILL):                                        │
│   "Now is not yet. The present is anticipation arriving."    │
└──────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────┐
│                     SEED 1: beg|lov|fea                      │
├──────────────────────────────────────────────────────────────┤
│ YOU (key: NOW, after touching 5):                            │
│   "Beginning is present tense. Love and fear coexist."       │
│                                                              │
│ Reader B (key: WAS, touched 1 first):                        │
│   "It began in love. The fear was already there."            │
│                                                              │
│ Reader C (key: WILL, touched 1 last):                        │
│   "It will begin. I love the possibility. I fear it too."    │
└──────────────────────────────────────────────────────────────┘

DIVERGENCE SCORE: 73%
The same garden grew three different experiences.
Same seeds. Different flowers.

╔════════════════════════════════════════════════════════════╗
║  mea shi | key shi | you shi | gar sam | gar dif          ║
║                                                            ║
║  Meaning shifts. Key shifts. You shift.                    ║
║  Garden same. Garden different.                            ║
╚════════════════════════════════════════════════════════════╝
```

---

## State Model (for Engineer)

### Garden Instance
```prolog
garden(
  id: "garden-7f3k",
  created: timestamp,
  seeds: [seed1, seed2, ..., seed9],
  readings: [reading1, reading2, ...]
).
```

### Reading Record
```prolog
reading(
  garden_id: "garden-7f3k",
  reader_id: "user-abc",
  key: now,  % was | now | will
  path: [5, 4, 1, 2, 3, 6, 9, 8, 7],
  collapses: [
    collapse(1, "Beginning is present tense..."),
    collapse(2, "The middle holds and breaks..."),
    ...
  ],
  garden_name: "The Eternal Present",
  timestamp: timestamp
).
```

### Propagation Rules
```prolog
% Horizontal ripple: collapsing seed N affects N-1 and N+1
ripple_horizontal(Seed, Key, Collapsed) :-
  adjacent_horizontal(Seed, Adjacent),
  shift_meaning(Adjacent, Key, Collapsed).

% Vertical echo (inverted): collapsing seed N creates opposite in N±3
ripple_vertical(Seed, Key, Collapsed) :-
  adjacent_vertical(Seed, Adjacent),
  invert_temporal(Key, InvertedKey),
  shift_meaning(Adjacent, InvertedKey, Collapsed).

% Diagonal tension: stored until seed is reached
tension(Seed, DiagonalSeed) :-
  diagonal(Seed, DiagonalSeed),
  not(visited(DiagonalSeed)).
```

---

## Implementation Notes

1. **LLM-Native**: The skill runs entirely in conversation. No external UI needed.

2. **Stateless Display**: Each command re-renders current state. No need for persistent connection.

3. **Multiplayer via ID**: Users share garden ID to join same instance. Readings accumulate.

4. **Divergence Calculation**: Compare collapsed meanings across readings with same seeds.

5. **Garden Naming**: LLM generates evocative name based on overall reading pattern.

---

*Prototype by Yuki. Ready for engineering review.*
