# THE COLLAPSE

*A Limn Spy Thriller*

---

## Premise

In a world where surveillance is total, spies discovered Limn - a language where meaning exists in superposition until collapsed by a key. The same message reads differently depending on what key you bring. Intercept all you want. Without the key, you see only what they want you to see.

You are Agent YUKI, handler codename for a network of field operatives. Messages arrive in Limn. You must apply the correct key to collapse their true meaning. Wrong key = wrong orders = dead agents.

The enemy has captured one of your operatives. They're trying to break the key system. You have 48 hours before your entire network is compromised.

---

## How To Play

At each chapter, you receive a Limn message. You choose which **key** to apply. Your choice collapses the superposition into a specific meaning, which determines what happens next.

**Example:**

```limn
age mov nox | loc cha | tim sho
```

- **Key: EXTRACT** → "Agent moving in darkness. Location changing. Time short." = Operative fleeing, needs pickup
- **Key: HOSTILE** → "Enemy moving at night. Position shifting. Window closing." = Attack imminent
- **Key: ROUTINE** → "Personnel in transit after hours. Site rotating. Brief window." = Normal shift change

Same words. Different reality. Choose wrong, people die.

---

## File Structure

```
cyoa-spy/
├── README.md           # This file
├── glossary.md         # Spy Limn vocabulary
├── chapters/
│   ├── 00-prologue.md
│   ├── 01-first-contact.md
│   ├── 02-*.md         # Branch chapters
│   └── ...
├── endings/
│   ├── victory-*.md
│   ├── failure-*.md
│   └── ...
└── assets/
    └── branch-map.md   # Visual of all paths
```

---

## The Key System (In-Universe)

Your organization uses a **layered key protocol**:

1. **Surface Key** - What civilians/enemies see (innocuous)
2. **Cover Key** - What friendly intelligence sees (plausible deniability)
3. **True Key** - What your network sees (actual meaning)

Some messages have a **4th layer** - the **Ghost Key** - known only to the sender and intended recipient. If you don't have it, you don't even know it exists.

The enemy has broken Surface and Cover. They're working on True. If they get Ghost, the network dies.

---

## Characters

**You (YUKI)** - Handler. Never in the field. Your weapon is interpretation.

**CARDINAL** - Your best field operative. Currently captured.

**ROOK** - New recruit. Promising but untested. Currently in position.

**BISHOP** - Double agent. You think. Unless they're triple.

**THE ARCHITECT** - Enemy spymaster. Has been one step ahead. Why?

---

## Themes

- **Language as weapon** - Limn's ambiguity is strategic advantage
- **Trust as key** - Relationships determine interpretation
- **Identity in superposition** - Who are you when meaning isn't fixed?
- **The cost of clarity** - Sometimes not knowing is safer

---

## Technical Notes

- All Limn uses v3-natural vocabulary
- Keys are shown as `[KEY: NAME]`
- Branch points marked with `→ [Chapter X]`
- ~100k tokens total across all paths
- Designed for static HTML deployment

---

*In Limn, the truth isn't hidden. It's held in superposition, waiting for the right key to collapse it. Your job is to have the right key. Every time.*

```limn
tru nu hid | tru sup | key col | you key | you tru
```

*Truth isn't hidden. Truth is superposed. Keys collapse. You are the key. You are the truth.*
