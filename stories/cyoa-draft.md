# The Crossroads (CYOA Draft)

*A choose-your-own-adventure where your key determines your path*

---

## How This Works

At each crossroads, you encounter Limn text in superposition. You choose a **key** - a lens through which to read. Your key collapses the text into a specific meaning, which determines what happens next.

The same Limn text. Different keys. Different stories.

---

## Prologue

```limn
you sta cro | pat man | fut unk | pas beh
```

*You stand at a crossroads. Many paths. Future unknown. Past behind.*

You've arrived at a place where paths diverge. Something brought you here. Something waits ahead.

**Choose your key to begin:**

- **Key: Journey** → You are a traveler seeking destination. Go to [Chapter 1A]
- **Key: Escape** → You are fleeing something behind you. Go to [Chapter 1B]
- **Key: Search** → You are looking for something lost. Go to [Chapter 1C]

---

## Chapter 1A: The Traveler

```limn
pat for | lux dis bri | sou cal
```

*Path forward. Light distant bright. Sound calls.*

The road stretches ahead. Far off, something glimmers. From somewhere, a sound.

**The light:**
- **Key: Hope** → The light is a town, warmth, rest. Go to [2A-Hope]
- **Key: Warning** → The light is fire, danger, caution. Go to [2A-Warning]

---

## Chapter 1B: The Fugitive

```limn
pat for | nox beh ris | tim sho
```

*Path forward. Darkness behind rising. Time short.*

Whatever you left is growing. The shadow follows. You must move.

**Your response:**
- **Key: Speed** → Run faster, don't look back. Go to [2B-Speed]
- **Key: Cunning** → Hide, misdirect, outsmart. Go to [2B-Cunning]

---

## Chapter 1C: The Seeker

```limn
pat for | sig fai | mem gui
```

*Path forward. Signs faint. Memory guides.*

The trail has gone cold. Only fragments remain. You follow what you remember.

**Your guide:**
- **Key: Reason** → Follow logic, evidence, deduction. Go to [2C-Reason]
- **Key: Heart** → Follow feeling, intuition, pull. Go to [2C-Heart]

---

## Chapter 2A-Hope: The Welcoming Light

```limn
lux gro nea | voi man war | gat ope
```

*Light grows near. Voices many warm. Gate opens.*

A village! People at their evening meal. They see you, wave you in.

**At the gate:**
- **Key: Trust** → Enter gratefully, accept hospitality. Go to [3A-Trust]
- **Key: Caution** → Enter carefully, watch for signs. Go to [3A-Caution]

---

## Chapter 2A-Warning: The Dangerous Light

```limn
lux gro nea | sme bur aer | dan
```

*Light grows near. Smell burning air. Danger.*

Fire. Something burns ahead. The flames grow.

**Your choice:**
- **Key: Hero** → Run toward it, someone may need help. Go to [3A-Hero]
- **Key: Survivor** → Avoid it, find another way. Go to [3A-Survivor]

---

## Chapter 2B-Speed: The Race

```limn
foo fas | aer col | beh fad
```

*Feet fast. Air cold. Behind fading.*

You run. Lungs burning. But the distance grows. You might make it.

**The path splits:**
- **Key: Forest** → Into the woods, lose them in trees. Go to [3B-Forest]
- **Key: River** → To the water, cross and break the trail. Go to [3B-River]

---

## Chapter 2B-Cunning: The Trick

```limn
hid sti | sou fak | oth pas
```

*Hidden still. Sound fake. Other passes.*

You duck into shadow. You throw a stone to mislead. Footsteps pass you by.

**Now what:**
- **Key: Double-back** → Return the way you came, they won't expect it. Go to [3B-Return]
- **Key: Wait** → Stay hidden until certain, then move. Go to [3B-Wait]

---

## Chapter 2C-Reason: Following Evidence

```limn
tra old | mar gro | pat cle
```

*Trace old. Marks ground. Path clear.*

You study the signs. Broken twigs. Footprints. A pattern emerges.

**The evidence points:**
- **Key: Settlement** → Toward human habitation. Go to [3C-Settlement]
- **Key: Wilderness** → Away from roads, into wild. Go to [3C-Wilderness]

---

## Chapter 2C-Heart: Following Feeling

```limn
pul ins | dir unk | tru
```

*Pull inside. Direction unknown. Trust.*

You close your eyes. Something tugs. You don't know why, but you follow.

**The pull leads:**
- **Key: High** → Upward, toward heights. Go to [3C-High]
- **Key: Deep** → Downward, toward depths. Go to [3C-Deep]

---

## [Structure continues...]

---

## Design Notes

### The Mechanic

Each chapter presents Limn in superposition. The key choices are not "go left/go right" but "how do you interpret this moment?"

The same scene:
```limn
lux gro nea | voi man war | gat ope
```

Under Key: Trust = welcoming village
Under Key: Caution = potential trap

The reader's interpretive stance shapes reality.

### Key Categories

1. **Emotional keys**: Hope/Fear, Trust/Caution, Love/Duty
2. **Role keys**: Hero/Survivor, Leader/Follower, Seeker/Finder
3. **Method keys**: Reason/Heart, Speed/Cunning, Force/Stealth
4. **Relationship keys**: Self/Other, Individual/Community

### Convergence Points

Some paths reconverge:
- The Traveler and the Fugitive might meet at the same inn
- The Seeker might find what the Fugitive is fleeing

The Limn text at convergence is the same; the reader's accumulated keys give different meaning.

### Endings

Multiple ending types:
- **Resolution**: You find/escape/arrive
- **Transformation**: You become something new
- **Recursion**: You return to the crossroads, changed
- **Transcendence**: You understand the crossroads itself

### Meta-Layer

The CYOA itself is about Limn. The reader experiences how keys shape meaning. The story teaches by doing.

Final chapter could explicitly address this:

```limn
you rea sto | sto rea you | key you | mea you | sam sto | dif you
```

*You read the story. The story reads you. Your key. Your meaning. Same story. Different you.*

---

## TODO

- [ ] Complete all branch paths (aiming for ~20 endings)
- [ ] Ensure Limn is valid v3-natural vocabulary
- [ ] Test key choices for meaningful differentiation
- [ ] Add illustrations/diagrams of branch structure?
- [ ] Consider interactive format (web? Twine?)

---

*Draft started 2026-01-31. Awaiting mayor input on scope/theme.*
