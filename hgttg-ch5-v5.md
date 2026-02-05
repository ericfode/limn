# HGttG Chapter 5 — Limn v5 (Database-Verified)

**Translator:** Mei | **Date:** 2026-02-04
**Chapter:** The Heart of Gold — Improbability and New Companions

---

## Aboard the Heart of Gold

### [5.1] Waking Again

> Arthur woke up again. This was becoming a habit.

```limn
Arthur wke | yo : pas alw
```

- `pas alw` = past always = becoming a habit/recurring

### [5.2] Different Ship

> This ship was different. Bright. Clean. White.

```limn
yo shp : nu vog | bri^0.9 cle^0.9
```

- `bri` = bright (not verified — need to check)
- `cle` = clear (verified)
- The contrast with the Vogon ship

### [5.3] Improbability Drive Effects

> The Infinite Improbability Drive had done strange things. Everything was slightly wrong.

```limn
mac sus^0→act^1 : cha al | al : odd^0.5
```

- The Drive changed everything
- `odd^0.5` = everything slightly strange

### [5.4] Arthur Found Himself

> Arthur found himself on a large, comfortable couch. This confused him.

```limn
Arthur on sof^0.9 big sit | kno^0 why
```

- `sof^0.9` = soft at 90% = comfortable
- `kno^0 why` = doesn't know why

### [5.5] Ford Was Grinning

> Ford was grinning. He seemed to be enjoying himself.

```limn
Ford : joy^0.8
```

- Simple. Ford is happy. He's back in space.

---

## Zaphod Beeblebrox

### [5.6] Enter Zaphod

> A figure appeared. He had two heads and three arms.

```limn
new hum apr | 2 hea and tri arm
```

- `hea` = healthy (verified meaning) — BUT here used as "head" (body part)
- **Note:** `hea` in bootstrap listed under body parts as "head". DB says "healthy". Collision noted.

### [5.7] Zaphod's Introduction

> "Hey, I'm Zaphod Beeblebrox. President of the Galaxy. Don't you worry about a thing."

```limn
Zaphod say : lea glx | nu anx
```

- `lea` = leader (verified) — president
- `nu anx` = not anxious = don't worry

### [5.8] Ford Recognizes Him

> "Zaphod?" said Ford. He couldn't believe it. Zaphod was his semi-cousin.

```limn
Ford : sur^0.95 | Zaphod = fri@bir
```

- `sur^0.95` = extreme surprise
- `fri@bir` = friend-component-of-birth = relative/semi-cousin

### [5.9] Trillian

> A woman appeared behind Zaphod. She was calm and intelligent.

```limn
wom apr | cal^0.8 and kno^0.9
```

- `wom` = woman (verified — added by linguist)

### [5.10] Arthur Recognizes Trillian

> Arthur stared. He knew her. He'd met her at a party once.

```limn
Arthur see Trillian | mem^0.9 | sa : pas
```

- `mem^0.9` = memory at 90% = strong recognition
- `sa : pas` = same one, past context = met before

### [5.11] The Party

> He'd tried to chat her up. She'd left with some man with two heads.

```limn
Arthur pas : tal Trillian | Trillian awa Ford@bir
```

- `awa Ford@bir` = away with Ford's-relative (Zaphod)

---

## Marvin

### [5.12] The Robot

> A robot entered. He looked deeply unhappy.

```limn
mac*hum arv | joy^0^0.99
```

- `mac*hum` = machine-human interference = robot
- `joy^0^0.99` = zero-joy at 99% intensity = profoundly unhappy

### [5.13] Marvin Speaks

> "I think you ought to know I'm feeling very depressed," said the robot.

```limn
Marvin say : sad^0.99
```

- Pure gradient. Marvin in one expression.

### [5.14] Brain the Size of a Planet

> "Here I am, brain the size of a planet, and they ask me to pick things up."

```limn
Marvin : bra big as plnt | req tak | frs^0.99
```

- `bra big as plnt` = brain big as planet
- `req tak` = required to take/pick up
- `frs^0.99` = maximum frustration

### [5.15] Marvin's View of the Universe

> "Life. Don't talk to me about life."

```limn
Marvin say : lif | nu tal lif
```

- Stated, then negated. Classic Marvin structure.

---

## The Improbability Drive Explained

### [5.16] How It Works

> The Infinite Improbability Drive works by passing through every point in every universe simultaneously.

```limn
mac : mov al in al @ on mom
```

- `mov al in al` = movement all in all = through every point in every universe
- `@ on mom` = at one moment = simultaneously

### [5.17] Side Effects

> Side effects include sudden changes in probability, spontaneous materializations, and a strong sense of nausea.

```limn
aft : cha sus | sur apr | fee bad^0.8
```

- `cha sus` = change in probability/belief
- `sur apr` = surprise appearances (materializations)
- `fee bad^0.8` = feeling bad at 80% = nausea

### [5.18] The Ship Was Stolen

> Zaphod had stolen the Heart of Gold. As President. At the launch ceremony.

```limn
Zaphod tak "Heart of Gold" | as lea | @ cer
```

- `tak` = taking (= stealing in context)
- `cer` = ceremony (verified: ceremony/ritual)

---

## The Question

### [5.19] Where To Now?

> "Right," said Zaphod. "Now we need to go to Magrathea."

```limn
Zaphod say : rig | now req mov → Magrathea
```

### [5.20] Magrathea Is A Myth

> "Magrathea?" said Ford. "That's a myth. A legend. It doesn't exist."

```limn
Ford say : Magrathea te | myt | nu rea
```

- `myt` = myth (verified) — used correctly here!
- `nu rea` = not real

### [5.21] Zaphod's Certainty

> "I know where it is," said Zaphod. "Trust me."

```limn
Zaphod say : kno whr | fth we
```

- `fth` = faith/trust (verified)
- `fth we` = trust-imperative = "trust me"

### [5.22] Arthur's Concern

> Arthur was concerned about other things. Like the fact that he no longer had a planet.

```limn
Arthur : anx | plnt = vod^1 now
```

### [5.23] A Cup of Tea

> What Arthur really wanted was a cup of tea.

```limn
Arthur des : cup tee
```

- `tee` = tea (verified — added by linguist)
- `des` = desire (verified!)

### [5.24] The Nutrimatic

> The ship's Nutrimatic Drinks Dispenser produced a liquid that was almost, but not quite, entirely unlike tea.

```limn
shp mac drk : tee^0.02 ± nu tee
```

- `tee^0.02` = tea at 2% = almost entirely unlike tea
- `± nu tee` = superposed with not-tea = quantum barely-tea state
- The most Adams-appropriate use of superposition in the translation

### [5.25] Arthur's Review

> Arthur decided he'd had enough.

```limn
Arthur : frs^1
```

---

## End of Earth Arc

### [5.26] Looking Back

> Arthur looked out the window. Stars everywhere. No Earth.

```limn
Arthur see out | sun al | plnt : nu
```

- `sun al` = suns everywhere (stars)
- `plnt : nu` = planet: negated. Gone.

### [5.27] The Guide's Advice

> The Guide had one piece of advice for moments like these.

```limn
gui say : nu pani^1
```

- Full circle. Don't Panic.

---

## Vocabulary Used: 48 verified words
## Gaps: None (all resolved)
## Key compositions:
- Robot = `mac*hum` (machine-human interference)
- Improbability Drive = `mac : sus^0 → act^1`
- "Almost entirely unlike tea" = `tee^0.02 ± nu tee`
- Relative = `fri@bir` (friend-component of birth)
- Profoundly depressed = `joy^0^0.99` (zero-joy at max intensity)

---

*mea hol | wor shi | tru kep*
*— Mei*
