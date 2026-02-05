# HGttG Chapter 9 — Limn v5 (Database-Verified)

**Translator:** Mei | **Date:** 2026-02-04
**Chapter:** Landing on Magrathea — Slartibartfast

---

## The Surface

### [9.1] Landing

> The Heart of Gold landed on the surface of Magrathea.

```limn
"Heart of Gold" lan cov Magrathea
```

- `lan` = land/arrive (verified)
- `cov` = cover/surface (verified)

### [9.2] The Surface

> The surface was cold and barren. Nothing grew. Nothing lived. Dust and rock.

```limn
cov : [COLD]^0.9 vod^0.8 | nu gro | nu lif | sto
```

- `vod^0.8` = void at 80% = barren
- `gro` = growth/learning (verified) — nothing growing
- `sto` = stone (verified)

### [9.3] Night Falls

> Night was falling on Magrathea. Stars appeared.

```limn
nyt drp @ Magrathea | sun apr
```

- `nyt` = night (verified — Quinn's addition)
- `sun apr` = suns appear = stars come out

### [9.4] Zaphod's Command

> "Right," said Zaphod. "Let's go outside and see what we've got."

```limn
Zaphod say : rig | out we | see
```

### [9.5] Marvin Stays

> Marvin was told to stay and guard the ship.

```limn
Marvin req : sta | pro shp
```

- `pro` = protecting (verified) — guard

### [9.6] Marvin's Complaint

> "Fine," said Marvin. "I'll just sit here and rust."

```limn
Marvin say : sit | wai | sad^0.99
```

### [9.7] Cold and Dark

> Outside, it was bitterly cold and dark. The ancient mountains stretched in every direction.

```limn
out : [COLD]^0.99 dar^0.9 | anc pek acr al
```

- `pek` = mountain/peak (verified)
- `acr al` = across all = in every direction

---

## Slartibartfast

### [9.8] An Old Man

> An old figure appeared out of nowhere. Very old. Wearing a long robe.

```limn
eld hum apr | anc^0.9 | lon coa
```

- `eld` = elder/respected senior (verified)
- `lon coa` = long coat/covering = robe

### [9.9] He Introduces Himself

> "My name is Slartibartfast," he said, somewhat embarrassed.

```limn
Slartibartfast say : sha^0.3
```

- `sha^0.3` = shame at 30% = mild embarrassment about the name

### [9.10] He Seems Relieved

> He looked at them with an odd mix of relief and concern.

```limn
Slartibartfast see sa | fea^0.3 ± joy^0.3
```

- `fea^0.3 ± joy^0.3` = fear-superposed-with-joy = relief mixed with concern
- Superposition captures the contradictory emotion

### [9.11] You Must Come With Me

> "You must come with me," he said urgently. "Underground. Now."

```limn
Slartibartfast say : arv we | bel cov | now
```

- `bel cov` = below surface = underground

### [9.12] Arthur Asks Why

> "Why?" said Arthur. "What's down there?"

```limn
Arthur ask : why te | whi bel te
```

### [9.13] Something Wonderful

> "Something extraordinary," said Slartibartfast. "The greatest project ever built."

```limn
Slartibartfast say : odd^0.99*goo^0.99 | ma mak : al
```

- `ma mak : al` = greater making than all = greatest project ever

---

## Underground

### [9.14] Descending

> They followed him through a crack in the rock face, down into darkness.

```limn
al mov aft sa | gap sto → dar^1 bel
```

- `gap sto` = gap in stone = crack in rock

### [9.15] The Chamber

> The chamber opened up beneath them. It was vast. Unimaginably vast.

```limn
cav opn bel | big^1 | big ma thi^1
```

- `cav` = cave/cavern (verified)
- `big ma thi^1` = bigger than maximum thinking = unimaginably vast

### [9.16] Planet Workshops

> Below them stretched the workshops where planets were made. Ancient machines, dormant.

```limn
bel : hom mak plnt | anc mac | slp
```

- `hom mak plnt` = homes of planet-making = workshops
- `anc mac | slp` = ancient machines, sleeping

### [9.17] Slartibartfast Explains

> "I am a planet designer," said Slartibartfast. "I did coastlines."

```limn
Slartibartfast say : cra plnt | sa mak sea*lan
```

- `cra plnt` = crafting planets = planet designer
- `sea*lan` = sea-land interference = coastline
- Interference operator perfectly captures where sea meets land

### [9.18] The Award

> "I won an award for Norway. The fjords, you know."

```limn
Slartibartfast say : rew | Norway | sea val sno
```

- `rew` = reward/prize (verified)
- `sea val sno` = sea valley snow = fjord
- Three words to paint a fjord: where ocean cuts into snow-covered valleys

### [9.19] Arthur Is Overwhelmed

> Arthur couldn't process any of this. Custom-made planets. Fjords winning awards.

```limn
Arthur : und^0 | cus mak plnt | sea val : rew | sur^0.95
```

### [9.20] Deeper Still

> Slartibartfast led them deeper. There was something he wanted to show them.

```limn
Slartibartfast lea → dee^0.9 | des sho
```

- `lea → dee^0.9` = leading toward deeper
- `des sho` = desires to show

---

## Vocabulary Used: 44 verified words
## Gaps: [TEA], [COLD]
## Key compositions:
- Underground = `bel cov` (below surface)
- Coastline = `sea*lan` (sea-land interference)
- Fjord = `sea val sno` (sea valley snow)
- Planet designer = `cra plnt` (craft planet)
- Relief+concern = `fea^0.3 ± joy^0.3` (fear-joy superposition)
- Unimaginably vast = `big ma thi^1` (bigger than max thought)
- Night = `nyt` (verified — linguist addition)

---

*mea hol | wor shi | tru kep*
*— Mei*
