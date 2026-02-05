# Vocabulary Traps: A Student's Warning List

> Words that look like one thing but mean another.

**Rule:** Always run `./scripts/vocab.sh check <word>` before using ANY word.

---

## The Dangerous Dozen

These words look like obvious English truncations but mean something else entirely.

| You type | You think | It actually means | Use this instead |
|----------|-----------|-------------------|------------------|
| `lis` | list | **listen** | `lst` (list/array) |
| `des` | describe | **desire, crave** | — (no single word; compose) |
| `imp` | improve | **implode, collapse** | `opt` (optimize) |
| `bug` | software bug | **insect** | `err` (error) |
| `rea` | read | **real** | `red` (read/parse) |
| `res` | result | **rest, stillness** | `rsl` (result) or `eff` (effect) |
| `whe` | where/when | **wheel, rotate** | — (use `wh` operator for queries) |
| `lea` | learn | **leader** | `gro` (learning) or `sch` (school) |
| `lan` | language | **land, arrive** | — (no single word yet) |
| `dan` | danger | **dance** | `fea` (fear/threat) |
| `cer` | certainty | **ceremony, ritual** | `crt` (certainty) |
| `bre` | breathe | **brief** | `aer` (air) for breath |
| `val` | validate | **valley, low point** | `tes` (test/verify) |
| `pre` | prevent | **pressure, force** | `avo` (avoid) or `stp` (stop) |

---

## The Semantic Domain Trap

These words exist but mean something different from the physical-world reading.
The DB stores **abstract/semantic qualities**, not physical properties.

| Word | Looks like | Actually means | Domain |
|------|-----------|---------------|--------|
| `hot` | temperature (hot) | **passion, anger, urgency** | Abstract |
| `col` | temperature (cold) | **distant, aloof, calm** | Abstract |
| `sol` | solid (physics) | **stubborn, rigid, reliable** | Abstract |
| `bri` | bright (light) | **hope, clarity, intelligence** | Abstract |

For physical temperature, use:
- `pyr` = fire-related (Physical World)
- `cld` = cold, low temperature

For physical states of matter, check the Physical World domain.

---

## Resolved Collisions

These were real collisions that have been fixed. The winning meaning is listed.

| Word | Kept as | Changed word | New word |
|------|---------|-------------|----------|
| `bri` | bright/hope | brief | `bre` |
| `hea` | health | hear | `aud` |
| `bel` | below | believe | `bli` |
| `pla` | plasma | plant | `veg` |
| `bir` | birth | bird | `avi` |
| `par` | parent | partial | `prt` |

---

## Productive Polysemy (Not Traps)

Some words cover related meanings on purpose. This is Limn working as designed.

| Word | Meanings | Shared core | Safe? |
|------|----------|-------------|-------|
| `lon` | long (space), long-lasting (time) | extension | Yes |
| `tru` | trust, true | reliability | Yes |
| `dea` | death, dead | death | Yes |
| `amb` | ambiguous (multiple readings) | ambiguity | Yes |

These are fine — context (key) resolves them.

---

## The Validation Habit

**Before writing any Limn:**

```bash
# Check a specific word
./scripts/vocab.sh check joy    # ✓ joy = joy (Mind & Cognition)

# Search for a concept
./scripts/vocab.sh search "list"    # → lst (list/array/collection)

# Browse a domain
./scripts/vocab.sh domain "Abstract"  # See all abstract words
```

**The 5-second rule:** If you haven't run `vocab.sh check` on a word in the
last 5 minutes, check it again. Meanings shift as the vocabulary evolves.

---

## Quick Reference: Common Concepts

| Concept | Wrong word | Right word |
|---------|-----------|------------|
| list/array | `lis` | `lst` |
| read | `rea` | `red` |
| learn | `lea` | `gro` |
| improve | `imp` | `opt` |
| error/bug | `bug` | `err` |
| result | `res` | `rsl` or `eff` |
| danger | `dan` | `fea` |
| certainty | `cer` | `crt` |
| breathe | `bre` | `aer` |

---

```
tes fir | wri nex | err avo
> test first | write next | errors avoided
```
