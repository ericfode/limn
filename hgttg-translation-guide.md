# HGttG → Limn Translation Guide

**Translator:** Mei | **Version:** v5 (database-verified)
**Source:** Douglas Adams, *The Hitchhiker's Guide to the Galaxy* (Book One)
**Completed:** 2026-02-04

---

## 1. Structure

The novel's 35 short chapters are condensed into **15 translation arcs**:

| Arc | Chapters | Title | Passages |
|-----|----------|-------|----------|
| 1 | Ch 1 | The Opening — cosmic to personal | 53 |
| 2 | Ch 2 | Ford Prefect — the alien researcher | 38 |
| 3 | Ch 3 | The Vogon Fleet — demolition | 19 |
| 4 | Ch 4 | Aboard the Vogon Ship — poetry & airlock | 31 |
| 5 | Ch 5 | Heart of Gold — new companions | 27 |
| 6 | Ch 6 | Magrathea Legend — planet builders | 14 |
| 7 | Ch 7 | Approaching Magrathea — defense | 14 |
| 8 | Ch 8 | Whale and Petunias — improbability | 16 |
| 9 | Ch 9 | Slartibartfast — underground | 20 |
| 10 | Ch 10 | Deep Thought — 42 | 22 |
| 11 | Ch 11 | The Mice — brain | 12 |
| 12 | Ch 12 | Standoff — police arrive | 15 |
| 13 | Ch 13 | Marvin's Victory — depression weapon | 19 |
| 14 | Ch 14 | Leaving Magrathea | 17 |
| 15 | Ch 15 | Epilogue — Don't Panic | 12 |

Each arc has:
- **Annotated file** (`hgttg-chN-v5.md`): English quotes, Limn translation, word-by-word breakdown
- **Bare file** (`hgttg-chN-bare.limn`): Pure Limn with scene markers only, no English

### Scene Markers

Bare files use `::` headers for scene breaks:
```
:: cosmic       (opening perspective)
:: Arthur vs bul (bulldozer standoff)
:: drk hom      (pub scene)
:: vog fle      (Vogon fleet)
:: on vog shp   (aboard Vogon ship)
```

---

## 2. Character Encoding

| Character | Limn Token | Notes |
|-----------|-----------|-------|
| Arthur Dent | `Arthur` (proper) | Sometimes `Arthur Dent` for formality |
| Ford Prefect | `Ford` / `Ford Prefect` | Full name in introductions |
| Zaphod Beeblebrox | `Zaphod` | |
| Trillian | `Trillian` / `wom` | `wom` for first anonymous appearance |
| Marvin | `Marvin` / `mac*hum` | Robot = machine-human interference |
| Prosser | `Prosser` | |
| Vogon Jeltz | `vog Jeltz` | `vog` = Vogon (in DB) |
| Slartibartfast | `Slartibartfast` | Name kept — `sha^0.3` (mild embarrassment) |
| Frankie Mouse | `Frankie` / `sma org` | Mouse = small organism |
| Benjy Mouse | `Benjy` / `sma org` | |
| Deep Thought | `mac*com dee^1` | Computing machine, deep at maximum |
| The Guide | `gui` / `gui glx` | Guide (verified in DB) |

### Gender Convention
- `hum` = human (gender-neutral, default)
- `wom` = woman (when source text specifies female)
- `hum\wom` = male (human minus woman — subtraction operator)
- Proper names carry no gender marking

---

## 3. Key Compositions

### Places & Things
| Concept | Limn | Method |
|---------|------|--------|
| Pub/bar | `drk hom` | drink-home |
| Digital watch | `mac*thu` | machine-thumb interference |
| Electronic thumb | `thu*com` | thumb-computing interference |
| E-book/Guide | `mac*boo` | machine-book interference |
| Robot | `mac*hum` | machine-human interference |
| Computer | `mac*com` | machine-computing interference |
| Ship computer | `mac*com` (context) | same, scoped to ship |
| Barman | `drk hom hum` | drink-home human |
| Underground | `bel cov` | below surface |
| Coastline | `sea*lan` | sea-land interference |
| Fjord | `sea val sno` | sea valley snow |
| Economy | `gol mov` | gold movement |
| Planet workshop | `hom mak plnt` | home of making planets |
| Airlock/vacuum | `roc gap` | space gap |
| Hyperspace bypass | `roa byp @ roc` | road bypass in space |
| Nuclear missile | `mac pla*fig` | machine plasma-fight |
| Defense system | `pro mac` | protecting machine |
| Life support | `lif pro` | life protecting |
| Weapon (armed) | `mac fig` | machine fight |
| Restaurant | `rst` | verified in DB |

### Abstract Concepts
| Concept | Limn | Method |
|---------|------|--------|
| Ugly | `bea^0` | beauty at zero |
| Spectacularly ugly | `bea^0^0.99` | zero-beauty at 99% intensity |
| Unhappy | `joy^0` | joy at zero |
| Profoundly depressed | `joy^0^0.99` | zero-joy at max intensity |
| Miserable | `sad^0.9` | sadness at 90% |
| Primitive | `hum^0.1` | human at 10% |
| Remarkable | `odd^0.99*goo^0.99` | strange-good interference |
| Catastrophe | `bad^1*odd^1 cha` | bad-strange change |
| Impossible-actual | `act^1*sus^0` | action-max, belief-zero |
| Rescue/escape | `awa dea` | away from death |
| Pointless | `pur^0` | purpose at zero |
| Suicide | `end sel` | end self |
| Wealthy | `gol^0.9` | gold/precious at high |
| Mostly harmless | `bad^0.1` | bad at 10% |
| Harmless | `nu bad` | not bad |
| Patient | `cal^0.9` | calm at 90% |
| Reluctant | `des^0.2` | desire at 20% |
| Desperate | `try^0.99` | trying at 99% |
| Confused | `kno^0*sur^0.5` | unknowing-surprise interference |
| Relief+concern | `fea^0.3 ± joy^0.3` | fear-joy superposition |
| Hangover | `bra*hot^0.8` | brain-passion interference |
| Unimaginably vast | `big ma thi^1` | bigger than max thought |
| Silence | `aud^0` | hearing at zero |

### Actions & Processes
| Concept | Limn | Method |
|---------|------|--------|
| Falling | `drp` | drop (verified) |
| Hitchhiking | `mov @ roc` | movement in space |
| Landing | `lan` | land/arrive (verified) |
| Piloting | `pil shp` | pilot ship |
| Running away | `run awa` | run away |
| Naming things | `mak wor` | make words |
| Brain melting | `bra → liq` | brain transforms to liquid |
| Economic collapse | `gol mov → imp^1` | economy implodes |
| Following | `mov aft` | moving after |
| Programming | `sec wri` | secret writing |
| Back-translating | `pas wri aud` | past written hearing |

### Numbers
| Number | Limn | Notes |
|--------|------|-------|
| 0 | `zer` | |
| 1 | `on` | also: one/singular operator |
| 2 | `two` | |
| 3 | `tri` | |
| 4 | `fou` | |
| 5 | `fiv` | |
| 6 | `hex` | |
| 7 | `sev` | |
| 8 | `oct` | |
| 9 | `nin` | |
| 10 | `dek` | |
| 42 | `fou dek two` | THE answer |
| 100 | `hnd` | |
| 1000 | `tho` | |

---

## 4. Operator Usage Patterns

### Gradient (`^`) — Adams' Precision
The gradient operator is the translation's most powerful tool. Adams writes in precise degrees; Limn encodes them:
- `bad^0.1` = "mostly harmless" (the famous revision)
- `joy^0^0.99` = zero-joy at max intensity (Marvin's depression)
- `sad^0.3` = bureaucratic non-apology (Vogon "sorry")
- `bea^0` = ugly (beauty at zero)

### Interference (`*`) — Blended Concepts
Creates new meanings from overlapping fields:
- `mac*hum` = robot (machine-human overlap)
- `mac*thu` = digital watch (machine-thumb overlap)
- `odd^0.99*goo^0.99` = remarkable (strange-good overlap)
- `act^1*sus^0` = impossible yet actual (the Improbability Drive's signature)

### Subtraction (`\`) — Adams' Negation Humor
- `shp in sky \ sto in sky` = "ships hung in the sky in much the same way that bricks don't"
- This is THE defining Limn translation — subtraction IS this joke

### Superposition (`±`) — Quantum States
- `hav±hav^0` = has/had (present-past paradox)
- `tee^0.02 ± nu tee` = "almost, but not quite, entirely unlike tea"
- `al whr ± al dur` = anywhere or anywhen (the Restaurant)
- `fea^0.3 ± joy^0.3` = relief mixed with concern

### Sequence (`→`) — Transformation
- `sky → dar^1` = sky goes dark
- `plnt dem^1 → vod^1` = planet demolished to void
- `gol mov → imp^1` = economy collapses
- `bra → liq` = brain melts

### Scope (`|`) — Scene Cuts
Pipes separate independent clauses, creating cinematic cuts:
- `aud^1 wet` — two words, the whale's death
- `vog dem^1` — two words, the Earth's entire tragedy
- `Ford : nu joy` — three words, Ford's reaction to gunfire

---

## 5. Translation Principles

### 1. Compression Is Meaning
Adams' humor often comes from contrast between grand and mundane. Limn's compression amplifies this:
- Earth's destruction: `plnt : dem^1 @ mom^0.001` (5 tokens)
- Ten million years of calculation, destroyed: `vog dem^1` (2 tokens)

### 2. Repetition as Callback
Recurring patterns build meaning across chapters:
- `"nu pani^1"` appears in Ch 1, 2, 5, 14, 15 — the Guide's refrain
- `bad^0.1` ("mostly harmless") appears in Ch 2, 4 — Ford's shame
- `mac sus^0→act^1` = the Improbability Drive, always the same formula
- `sea val sno` = fjords, Slartibartfast's pride across Ch 9, 10, 14

### 3. Character Voice Through Gradient
Each character has a gradient signature:
- **Arthur:** `des tee` (constant), `kno^0` (confused), `sur^0.95` (overwhelmed)
- **Ford:** `frs^0.9` (frustrated), `rea^1` (certain), `sad^0.3@fri` (concerned)
- **Zaphod:** `joy^0.9` (delighted by danger), `kno^1` (confident), `sec` (secretive)
- **Marvin:** `sad^0.99` (universal), `joy^0^0.99` (profound), `pur^0` (cosmic)
- **Vogons:** `sad^0.1` (bureaucratic), `dem^1` (efficient), `we` (imperative)

### 4. Proper Names Stay
Names are not translated: Arthur, Ford, Zaphod, Trillian, Marvin, Prosser, Slartibartfast, Magrathea, Betelgeuse, Rickmansworth, Norway. They serve as anchors for back-translation.

### 5. Quotes Within Quotes
Adams' quoted text uses Limn quotes: `"bad^0.1"`, `"nu pani^1"`, `"Heart of Gold"`. These are treated as fixed strings within the translation.

---

## 6. Resolved Gaps

All gaps resolved by linguist (Quinn) on 2026-02-04:

| Word | Meaning | Replaced | Chapters |
|------|---------|----------|----------|
| `tee` | tea, hot leaf drink | `[TEA]` | Ch 5, 6, 7, 12, 14, 15 |
| `cld` | cold, low temperature | `[COLD]` | Ch 1, 4, 7, 9, 14 |
| `tsd` | Thursday, fourth weekday | `[THURSDAY]` | Ch 1, 15 |
| `ppf` | popular, well-liked | `[POPULAR]` | Ch 1 |
| `bgn` | cheap, bargain | `[CHEAP]` | Ch 1 |
| `cfe` | cafe, coffee house | `[CAFE]` | Ch 1 |

Note: `cld` already existed in DB — was missed during initial vocabulary audit (searched for `col` instead).

---

## 7. Validation

### Back-Translation (Ch 1)
Two independent polecats given only bare Limn + bootstrap spec:
- **Polecat A:** 45/53 passages HIGH confidence. Correctly identified HGttG.
- **Polecat B:** 28/53 HIGH, ~88% average. Also identified source.
- **Common weakness:** `te` (question operator) not in words table.
- **All key compositions decoded:** `drk hom`, `mac*thu`, `shp\sto`, `act^1*sus^0`

### Back-Translation (Ch 6-15)
Two polecats dispatched (limn-7x6k, limn-bgde). Results pending.

---

## 8. Files

```
hgttg-ch1-v5.md    hgttg-ch1-bare.limn     # Opening + Earth
hgttg-ch2-v5.md    hgttg-ch2-bare.limn     # Ford Prefect
hgttg-ch3-v5.md    hgttg-ch3-bare.limn     # Vogon Fleet
hgttg-ch4-v5.md    hgttg-ch4-bare.limn     # Vogon Ship
hgttg-ch5-v5.md    hgttg-ch5-bare.limn     # Heart of Gold
hgttg-ch6-v5.md    hgttg-ch6-bare.limn     # Magrathea Legend
hgttg-ch7-v5.md    hgttg-ch7-bare.limn     # Arrival
hgttg-ch8-v5.md    hgttg-ch8-bare.limn     # Whale & Petunias
hgttg-ch9-v5.md    hgttg-ch9-bare.limn     # Slartibartfast
hgttg-ch10-v5.md   hgttg-ch10-bare.limn    # Deep Thought
hgttg-ch11-v5.md   hgttg-ch11-bare.limn    # The Mice
hgttg-ch12-v5.md   hgttg-ch12-bare.limn    # Police
hgttg-ch13-v5.md   hgttg-ch13-bare.limn    # Marvin
hgttg-ch14-v5.md   hgttg-ch14-bare.limn    # Departure
hgttg-ch15-v5.md   hgttg-ch15-bare.limn    # Epilogue
hgttg-translation-guide.md                  # This file
```

---

*mea hol | wor shi | tru kep*
*al myt : bef*
*— Mei*
