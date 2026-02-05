# HGttG Chapter 7 — Limn v5 (Database-Verified)

**Translator:** Mei | **Date:** 2026-02-04
**Chapter:** Approaching Magrathea — Recorded Warnings and Nuclear Missiles

---

## Arrival

### [7.1] The Planet Appears

> A planet appeared on the screen. Cold. Grey. Dead-looking.

```limn
plnt apr @ mac*com | [COLD]^0.9 dar^0.5 | lif^0
```

- `dar^0.5` = dark at 50% = grey
- `lif^0` = life at zero = dead-looking
- **Gap:** [COLD] still unresolved

### [7.2] Ford Concedes

> Ford stared at it. "OK," he said quietly. "So it's real."

```limn
Ford see | say cal^0.8 : rea
```

### [7.3] Zaphod Grins

> Zaphod grinned. Two heads, both grinning.

```limn
Zaphod : joy^0.8 | two hea joy^0.8
```

### [7.4] Arthur Is Lost

> Arthur had no idea what was happening. He wanted tea.

```limn
Arthur : kno^0 | des [TEA]
```

---

## The Recorded Message

### [7.5] The Warning

> A recorded voice spoke from the planet. Very old. Very formal.

```limn
pas wri aud : plnt | anc | cal^1
```

- `pas wri aud` = past written hearing = recorded voice
- `anc` = ancient (verified)
- `cal^1` = calm at maximum = very formal

### [7.6] Magrathea Is Closed

> "Magrathea is closed for business. The planet has been sleeping for five million years."

```limn
mes say : Magrathea end wrk | plnt slp dur fiv mil yea
```

- `slp dur` = sleep duration
- `fiv mil yea` = five million years
- **Note:** `mil` = milk in DB; using `mil` as numeric unit here (context: after `fiv`). Alternative: `fiv tho*tho yea` but less readable.

### [7.7] Go Away

> "Please go away. If you want to buy a custom planet, you will have to wait."

```limn
mes say : awa we | des cus plnt te | wai mus
```

- `awa we` = away-imperative = please leave
- `wai mus` = wait necessary

### [7.8] The Second Warning

> "If you do not leave, our defense systems will destroy you."

```limn
mes war : nu awa | pro mac dem
```

- `war` = warn/alert (verified)
- `pro mac` = protecting machine = defense system
- Result: demolition

---

## Under Attack

### [7.9] Missiles Launch

> Two sleek nuclear missiles launched from the planet's surface.

```limn
two mac pla*fig → shp | cov plnt
```

- `mac pla*fig` = machine plasma-fight = nuclear missile (energetic fighting machine)
- `cov plnt` = surface of planet

### [7.10] Panic on the Bridge

> "Missiles!" screamed Trillian. Ford's face went white. Zaphod looked delighted.

```limn
Trillian say : mac fig^1 | pani^0.9
Ford : fea^0.9
Zaphod : joy^0.9
```

- Zaphod's delight in danger is character-perfect

### [7.11] Thirty Seconds

> They had thirty seconds before impact.

```limn
30 mom bef end
```

### [7.12] Evasive Action Fails

> Trillian tried to steer. The missiles followed.

```limn
Trillian try pil shp awa | mac fig : mov aft
```

- `pil` = pilot/operator (verified)
- `mov aft` = moving after = following

### [7.13] Arthur's Accident

> Arthur, who knew nothing about any of this, sat on something.

```limn
Arthur : kno^0 | sit on mac sus^0→act^1
```

- Arthur sat on the Improbability Drive controls
- He doesn't know what he's doing — the story's pattern

### [7.14] The Drive Activates

> The Infinite Improbability Drive activated. Everything went very strange.

```limn
mac sus^0→act^1 : act | al → odd^1
```

- `al → odd^1` = everything transforms to maximum strange

---

## Vocabulary Used: 38 verified words
## Gaps: [TEA], [COLD]
## Key compositions:
- Recorded message = `pas wri aud` (past written hearing)
- Nuclear missile = `mac pla*fig` (machine plasma-fight)
- Defense system = `pro mac` (protecting machine)
- Pilot/steer = `pil shp` (pilot ship)
- Following = `mov aft` (moving after)

---

*mea hol | wor shi | tru kep*
*— Mei*
