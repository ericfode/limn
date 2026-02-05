# HGttG Chapter 4 — Limn v5 (Database-Verified)

**Translator:** Mei | **Date:** 2026-02-04
**Chapter:** Aboard the Vogon Ship

---

## Waking Up

### [4.1] Arthur Wakes

> Arthur came to. He was in a dark, cramped space that smelled terrible.

```limn
Arthur wke | dar^0.8 sma gap | bad^0.9
```

- `dar^0.8` = dark at 80%
- `sma gap` = small space (verified: gap=space between)
- `bad^0.9` = bad at 90% = terrible (smell implied by context)

### [4.2] Where Am I?

> "Where am I?" said Arthur.

```limn
Arthur ask : whr te
```

### [4.3] Ford Explains

> "We're on a Vogon ship," said Ford. "I managed to hitch us a ride."

```limn
Ford say : on vog shp | thu*com wrk
```

- `thu*com wrk` = electronic thumb worked

### [4.4] Earth Is Gone

> "What happened to the Earth?" "It's been demolished. Destroyed."

```limn
Arthur ask : wrl te
Ford say : dem^1
```

### [4.5] Arthur's Grief

> Arthur felt a deep, hollow sadness. Everything he had ever known was gone.

```limn
Arthur : sad^0.95 | al kno → vod^1
```

- `al kno → vod^1` = all known transforms to maximum void
- Everything he knew: gone

### [4.6] The Towel

> Ford handed Arthur a towel. "You should always know where your towel is."

```limn
Ford giv Arthur twl | alw kno whr twl
```

- `twl` = towel (verified in DB)
- The towel principle. One of the most database-correct sentences in the whole translation.

---

## The Guide

### [4.7] Reading the Guide

> Ford showed Arthur the Hitchhiker's Guide. He pressed a button.

```limn
Ford sho Arthur gui glx | act
```

### [4.8] The Guide's Entry on Earth

> The Guide's entry on Earth read: "Mostly harmless."

```limn
gui : wrl | "bad^0.1"
```

- The callback. Same gradient as Ch 2.

### [4.9] Arthur is Unimpressed

> "Is that all it's got to say? 'Mostly harmless'?"

```limn
Arthur say : al te | "bad^0.1" te
```

- Repeating the phrase back as a question — `te` operator

### [4.10] Ford's Embarrassment

> "Well, I did write a longer entry but the editors cut it down."

```limn
Ford say : sha^0.3 | lon → sho
```

- `sha^0.3` = mild shame
- `lon → sho` = long transforms to short (editors cut it)
- Clever: `sho` = showing, but the contraction = it was shortened. Double reading.

---

## The Babel Fish

### [4.11] The Fish

> "Now," said Ford, "put this fish in your ear."

```limn
Ford say we : bab in ear
```

### [4.12] What?

> "I beg your pardon?" said Arthur.

```limn
Arthur : sur^0.8 | te
```

### [4.13] Trust Me

> "Just do it," said Ford. "It's a babel fish. It'll translate any language."

```limn
Ford say we : act | bab : al aud → und^1
```

- Same babel fish formula from Ch 1: `al aud → und^1`

### [4.14] Arthur Complies

> Arthur reluctantly put the fish in his ear.

```limn
Arthur : des^0.2 | bab → ear
```

- `des^0.2` = desire at 20% = reluctant

### [4.15] Suddenly Understanding

> Suddenly, Arthur could understand everything. The Vogon intercom was making announcements.

```limn
sur^1 : und^1 | vog ann aud
```

---

## Vogon Poetry

### [4.16] The Captain's Hobby

> The Vogon captain fancied himself as a poet. This was a mistake.

```limn
vog Jeltz : des poe | bad ide
```

### [4.17] Third Worst

> Vogon poetry is the third worst in the universe. The second worst was by the Azgoths of Kria.

```limn
vog poe : bad^0.97 | 3 bad in al
```

- `bad^0.97` = bad at 97% = third worst (not quite the worst)
- `3 bad in al` = third bad in all/universe

### [4.18] The Reading

> "Oh freddled gruntbuggly, thy micturations are to me..."

```limn
vog poe : aud
```

- The poetry itself is untranslatable. The line simply marks: Vogon poetry heard.
- Content is `bea^0*mea^0` — zero beauty, zero meaning

### [4.19] Physical Pain

> Arthur felt his brain begin to melt. Ford groaned.

```limn
Arthur bra → liq : vog poe
Ford : sad*ang aud
```

- `bra → liq` = brain transforms to liquid = melting
- `sad*ang` = sadness-anger interference = groaning

### [4.20] Poetry Review

> "Now," said the Vogon captain, "what did you think of my poem?"

```limn
vog ask : poe goo te
```

### [4.21] Ford's Diplomacy

> Ford desperately tried to think of something nice to say.

```limn
Ford try^0.99 : say goo^0.3
```

- `try^0.99` = trying at 99% = desperately
- `goo^0.3` = good at 30% = something vaguely nice

### [4.22] Arthur Tries

> Arthur said he thought some of the metaphors were interesting.

```limn
Arthur say : ex met cur^0.4
```

- `ex` = some (existential operator)
- `met` = metaphor (verified)
- `cur^0.4` = curious at 40% = mildly interesting

### [4.23] Wrong Answer

> This was the wrong answer.

```limn
ans : bad^1
```

---

## The Airlock

### [4.24] Throw Them Out

> "Throw them out the airlock," said the Vogon captain.

```limn
vog say we : out
```

- Imperative, one word. Vogon efficiency.

### [4.25] The Airlock Opens

> The airlock door opened. Space stretched out before them, vast and cold and empty.

```limn
doo opn | roc : big^1 * cld^0.99 * vod^1
```

- `doo opn` = door opens (verified: doo=door/opportunity, opn=open)
- `cld` = cold (verified — existed in DB, previously missed)

### [4.26] Arthur's Thoughts

> Arthur thought: this is it. This is the end.

```limn
Arthur thi : yo end^1
```

### [4.27] Ford's Thoughts

> Ford thought: the odds against being picked up by another ship are astronomical.

```limn
Ford thi : shp tak : sus^0.001
```

- `sus^0.001` = suspicion/belief at 0.1% = astronomically unlikely

### [4.28] Pushed Out

> They were pushed out the airlock.

```limn
Arthur and Ford → out doo → roc
```

### [4.29] Thirty Seconds

> They had thirty seconds to live. Probably less.

```limn
lif dur 30 mom | sus mi
```

- `sus mi` = suspect less (probably less)

### [4.30] The Impossible Rescue

> In the next twenty-nine seconds, something impossible happened.

```limn
@ 29 mom : act^1*sus^0
```

- `act^1*sus^0` = actual at max, believable at zero = impossible thing happened
- The Improbability Drive's signature

### [4.31] Picked Up

> They were picked up by a ship. The Heart of Gold. Powered by the Infinite Improbability Drive.

```limn
shp tak sa | "Heart of Gold" | mac : sus^0 → act^1
```

- `mac : sus^0 → act^1` = machine that transforms impossibility to actuality
- The Infinite Improbability Drive in one compositional expression

---

## Vocabulary Used: 45 verified words
## Gaps: None (all resolved)
## Key compositions:
- Improbability Drive = `mac : sus^0 → act^1`
- Brain melting from poetry = `bra → liq : vog poe`
- "Mostly harmless" callback = `"bad^0.1"`

---

*mea hol | wor shi | tru kep*
*— Mei*
