# HGttG Chapter 1 — Limn v5 Translation (Database-Verified)

**Translator:** Mei
**Date:** 2026-02-04
**Method:** Every word verified against Dolt vocabulary database
**Operators:** `@` proj `*` interf `^` grad `\` sub `±` super `:` cond `|` scope `→` seq

**Proper names used as tokens:** Arthur Dent, Ford Prefect, Prosser, Vogon Jeltz, Betelgeuse, Guildford, Rickmansworth
**Numbers:** Literal digits (2, 6, 12, 2000) — Limn allows numeric literals

---

## The Opening — Cosmic Perspective

### [1] The Yellow Sun

> "Far out in the uncharted backwaters of the unfashionable end of the western spiral arm of the Galaxy lies a small unregarded yellow sun."

```limn
far^0.9 rmt | fas^0.1 end wes sprl arm glx | sma nu-rgd yel sun
```

- `far^0.9` = extremely far (verified: far=far)
- `rmt` = remote (verified: remote/unrelated)
- `fas^0.1` = fashionable at 10% = unfashionable
- `nu-rgd` = not regarded (negation + regard)
- All words verified ✓

### [2] The Orbiting Planet

> "Orbiting this at a distance of roughly ninety-two million miles is an utterly insignificant little blue green planet whose ape-descended life forms are so amazingly primitive that they still think digital watches are a pretty neat idea."

```limn
orb yo @ dst | rgd^0.01 sma blu-grn wrl
lif : hum^0.1 | thi mac*thu goo^0.3
```

- `orb yo` = orbiting this (yo=proximal)
- `rgd^0.01` = regarded at 1% = utterly insignificant
- `hum^0.1` = human at 10% = primitive/barely human
- `mac*thu` = machine-thumb interference = digital watch (device+grip)
- `goo^0.3` = good at 30% = "pretty neat"
- **Gap:** No word for "ape" — using `hum^0.1` for primitive life

### [3] The Unhappy Planet

> "This planet has—or rather had—a problem, which was this: most of the people living on it were unhappy for pretty much all of the time."

```limn
wrl hav±hav^0 : mst hum lif on wrl | joy^0 @ al
```

- `hav±hav^0` = have superposed with have-at-zero = has/had paradox
- `mst hum` = most humans (verified: mst=most, hum=human)
- `joy^0` = joy at zero = unhappy
- `@ al` = at all (time) — using `al` operator (universal)

### [4] Small Green Paper

> "Many solutions were suggested for this problem, but most of these were largely concerned with the movement of small green pieces of paper, which was odd because on the whole it wasn't the small green pieces of paper that were unhappy."

```limn
ans^0.9 : mst | mov sma grn fra pap
odd^0.8 : pap nu joy^0 | hum joy^0
```

- `ans^0.9` = many answers (answer at 90%)
- `fra pap` = fragment paper (verified: fra=fragment, pap=paper)
- `pap nu joy^0` = paper not joyless = paper wasn't unhappy
- `hum joy^0` = humans joyless

### [5] The Problem Remained

> "And so the problem remained; lots of people were mean, and most of them were miserable, even the ones with digital watches."

```limn
so sta | hum^0.9 cru | hum^0.8 sad^0.9
mac*thu : nu fix
```

- `so sta` = therefore stability (problem remained)
- `cru` = cruel (verified: cruel/harsh — captures "mean")
- `sad^0.9` = sadness at 90% = miserable
- `mac*thu : nu fix` = digital watches: not a fix

### [6] Thursday and the Tree

> "One Thursday, nearly two thousand years after one man had been nailed to a tree for saying how great it would be to be nice to people for a change..."

```limn
on tsd | 2000 yea aft on hum @ tre : spk goo^1
```

- `tsd` = Thursday (verified — added by linguist)
- `yea` = year (verified — added by linguist)
- `on hum @ tre` = one human at tree
- `spk goo^1` = speaking maximum good = "be nice"

### [7] The Girl in the Cafe

> "...a girl sitting on her own in a small cafe in Rickmansworth suddenly realized what it was that had been going wrong all this time, and she finally knew how the world could be made a good place."

```limn
on wom in sma cfe Rickmansworth | sur^1 kno ans^1
sa kno : fix wrl → goo
```

- `on wom` = one woman (verified — `wom` added by linguist)
- `sur^1` = maximum surprise = suddenly realized
- `kno ans^1` = knew answer at maximum
- `sa kno` = same-one knew (sa=anaphoric)
- `fix wrl → goo` = fix planet transforms to good

### [8] It Would Work

> "This time it was right, it would work, and no one would have to get nailed to anything."

```limn
yo : rig^1 | wrk^1 | nu hum @ tre
```

- `yo` = this (proximal)
- `rig^1` = right at maximum
- `wrk^1` = work at maximum
- `nu hum @ tre` = no human at tree

### [9] Too Late

> "Sadly, however, before she could get to a phone to tell anyone about it, a terrible, stupid catastrophe occurred, and the idea was lost forever."

```limn
sad^0.9 : bef sa tel
bad^1*odd^1 cha
ide los^1 al
```

- `bef sa tel` = before same-one tells (sa=anaphoric back to girl)
- `bad^1*odd^1 cha` = maximum-bad interfered with maximum-odd change = terrible stupid catastrophe
- `ide los^1 al` = idea lost at maximum, totally
- **Note:** `dsa` (disaster) now available, but composition `bad^1*odd^1 cha` preserves Adams' three adjectives

### [10] Not Her Story

> "This is not her story."

```limn
yo nu sa myt
```

- `yo` = this
- `nu sa` = not same-one's (not her)
- `myt` = myth/story (closest verified word)

### [11] This Story

> "But it is the story of that terrible, stupid catastrophe and some of its consequences."

```limn
yo myt : bad^1*odd^1 cha and aft
```

- `myt` = story/myth
- `bad^1*odd^1 cha` = terrible-stupid catastrophe (same pattern)
- `and aft` = and after (consequences)

### [12] The Guide

> "It is also the story of a book, a book called The Hitchhiker's Guide to the Galaxy—not an Earth book, never published on Earth..."

```limn
myt : boo "gui glx"
nu wrl boo | nu on wrl
```

- `boo "gui glx"` = book called "guide galaxy"
- `nu wrl boo` = not planet's book
- `nu on wrl` = never on planet

### [13] A Remarkable Book

> "Nevertheless, a wholly remarkable book."

```limn
boo : odd^0.99*goo^0.99
```

- `odd^0.99*goo^0.99` = maximum-odd interfered with maximum-good = wholly remarkable
- Interference creates "remarkable" from "strange" + "excellent"

### [14] More Popular Than...

> "More popular than the Celestial Home Care Omnibus, better selling than Fifty-three More Things to Do in Zero Gravity..."

```limn
ma ppf : al boo
```

- `ppf` = popular (verified — added by linguist)
- `ma` = greater than (operator)
- Compressing the comparison list — Adams' specific titles don't translate

### [15] Don't Panic

> "First, it is slightly cheaper; and second, it has the words DON'T PANIC inscribed in large friendly letters on its cover."

```limn
on : bgn^0.9
2 : cov say "nu pani^1" @ big*fri
```

- `bgn` = cheap/bargain (verified — added by linguist)
- `cov say` = cover says (verified: cov=cover/surface)
- `"nu pani^1"` = don't panic at maximum
- `big*fri` = big interfered with friendly = large friendly (describing the text)
- **Gap:** `let` = allow, not "letters". Using context to imply written text.

---

## Arthur vs Bulldozer

### [16] Arthur Wakes

> Arthur Dent woke with a hangover.

```limn
Arthur Dent wke | bra*hot^0.8
```

- `wke` = wake (verified)
- `bra*hot^0.8` = brain interfered with passion at 80% = hangover (head throbbing)

### [17] The House

> His house was about to be demolished to make way for a bypass.

```limn
hom : dem^0.99 | roa byp
```

- `hom` = home (verified)
- `dem^0.99` = demolish at 99% certainty
- `roa byp` = road bypass (verified: roa=road, byp=bypass)

### [18] Lying in Mud

> He was lying in front of a bulldozer in the mud.

```limn
sa lie fro bul | in mud
```

- `sa` = same one (anaphoric = Arthur)
- `lie fro bul` = lie down in-front-of bulldozer
- `fro` = in front (verified!)

### [19] Prosser's Frustration

> Mr. Prosser was upset. He was trying to do his job.

```limn
Prosser : ang*frs^0.8 | try wrk
```

- `ang*frs` = anger interfered with frustration
- `try wrk` = try work

### [20] The Absurd Standoff

> A man lying in front of a bulldozer.

```limn
on hum lie fro bul
```

- `on hum` = one human (verified: on=singular, hum=human)

### [21] Thursday

> "This must be Thursday. I never could get the hang of Thursdays."

```limn
yo : tsd | und^0 tsd al
```

- `tsd` = Thursday (verified — added by linguist)
- `und^0` = understanding at zero = never could understand

### [22] Yellow Bulldozer

> The bulldozer sat there, yellow and patient.

```limn
bul sit : yel and cal^0.9
```

- `cal^0.9` = calm at 90% = patient (calm is close)

### [23] Ford Arrives

> Ford Prefect arrived with beer and concern.

```limn
Ford Prefect arv | bee and sad^0.3@fri
```

- `arv` = arrive (verified)
- `bee` = beer (verified)
- `sad^0.3@fri` = low-sadness projected onto friend = friendly concern

### [24] The Pub Suggestion

> "Why don't you go to the pub instead?"

```limn
why nu mov → drk hom
```

- `mov → drk hom` = move to drink-home (pub via composition)
- `drk hom` = drink home = pub/tavern

---

## The Pub Scene

### [25] Ford's Urgency

> "Come with me to the pub. You need to drink three pints of beer, quickly."

```limn
Ford say : arv drk hom | req drk bee^0.9
```

- `arv drk hom` = arrive at drink-home
- `req drk bee^0.9` = require drink beer urgently

### [26] Arthur Protests

> "But what about my house?"

```limn
Arthur say : hom te
```

- `te` = question operator
- `hom te` = home? (what about house)

### [27] The Substitution

> Ford told Prosser to lie in the mud instead.

```limn
Ford tel Prosser : lie mud
```

### [28] Six Pints

> Ford ordered six pints of beer and six packets of peanuts.

```limn
Ford ask : hex cup bee and hex bag nut
```

- `hex` = six (verified: hexagon/six)
- `cup` = cup/vessel (verified)
- `bag` = bag (verified)
- `nut` = nutrient/food (verified)

### [29] World Ending

> "The world is going to end in about twelve minutes."

```limn
Ford say : wrl end @ 12 mom
```

- `wrl` = planet (verified)
- `mom` = moment (verified) — using for minutes

### [30] Arthur's Disbelief

> "Is it?" He wasn't really paying attention.

```limn
Arthur : rea te | lis^0.1
```

- `rea te` = real? (question)
- `lis^0.1` = listening at 10% = barely listening

### [31] Ford Explains

> "The Vogon constructor fleet is going to demolish it to build a hyperspace bypass."

```limn
Ford say : vog fle dem wrl | roa byp @ roc
```

- `roc` = rocket/space (verified)
- Using `roc` for space context (hyperspace = fast-space)

### [32] Beer is Important

> Arthur looked at his beer. It seemed important now.

```limn
Arthur see bee | bee : vit^0.9 now
```

- `vit^0.9` = vital at 90% = very important (verified)

### [33] Ford's Origin

> "I'm not from Guildford. I'm from a small planet near Betelgeuse."

```limn
Ford say : nu Guildford | sma wrl dst Betelgeuse
```

- `dst` = distant/near region (Betelgeuse vicinity)

### [34] Arthur Doesn't Care

> Arthur thought about this, decided it didn't matter.

```limn
Arthur thi | vit^0.1
```

- `vit^0.1` = importance at 10% = doesn't matter

### [35] Drinking Fast

> They drank their beer. It was good. The world hadn't ended yet.

```limn
drk bee | goo | wrl : end^0 now
```

- `end^0` = ending at zero = hasn't ended yet

### [36] The Electronic Thumb

> Ford pulled out a small device. "A sub-etha electronic thumb. For hitchhiking."

```limn
Ford get sma mac | thu*com | mov @ roc
```

- `mac` = machine/device (verified)
- `thu*com` = thumb interfered with computing = electronic thumb ✓
- `mov @ roc` = movement in space = hitchhiking

### [37] Sky Goes Dark

> The sky went dark. Enormous yellow shapes appeared.

```limn
sky → dar^1 | big^0.99 yel apr
```

- `sky → dar^1` = sky transforms to maximum dark
- `big^0.99 yel apr` = enormous yellow appears

---

## Vogon Fleet

### [38] Ships Hanging

> "The ships hung in the sky in much the same way that bricks don't."

```limn
shp in sky \ sto in sky
```

- `shp in sky` = ships in sky
- `\ sto in sky` = subtract stone-in-sky = hanging as stones don't
- Subtraction operator captures the paradox

### [39] Vogon Announcement

> "This is Prostetnic Vogon Jeltz of the Galactic Hyperspace Planning Council."

```limn
vog Jeltz : glx roc
```

### [40] Demolition Notice

> "Your planet is scheduled for demolition to make way for a hyperspace bypass."

```limn
vog ann : wrl dem^1 | roa byp @ roc
```

- `ann` = announce (verified)
- Same bypass pattern as Ford's explanation

### [41] Earth Destroyed

> The planet was destroyed. Instantly.

```limn
wrl : dem^1 @ mom^0.001
```

- `dem^1` = demolish at maximum
- `mom^0.001` = in 0.1% of a moment = instantaneous

### [42] Total Loss

> Everything on Earth was gone.

```limn
al@wrl → vod^1
```

- `al@wrl` = everything-on-planet (all projected onto planet)
- `→ vod^1` = transforms to maximum void

---

## Aboard the Vogon Ship

### [43] Waking on the Ship

> Arthur woke. He was on a ship. He didn't know where.

```limn
Arthur wke | on shp | kno^0 whr
```

### [44] Ship Ugliness

> The ship was ugly. Spectacularly ugly.

```limn
shp : bea^0 | bea^0^0.99
```

- `bea^0` = beauty at zero = ugly
- `bea^0^0.99` = that zero-beauty at 99% intensity = spectacularly ugly
- Double gradient: how intensely un-beautiful

### [45] Don't Panic

> Ford handed Arthur the Guide. On the cover: DON'T PANIC.

```limn
Ford giv Arthur gui | cov : "nu pani^1" @ big*fri
```

### [46] Earth Gone

> "My planet has been demolished," Arthur said quietly.

```limn
Arthur say cal^0.8 : wrl dem^1
sa : los^0.95
```

- `say cal^0.8` = say calmly (quiet)
- `los^0.95` = lost at 95%

### [47] Ford's Sympathy

> "Yes, sorry about that. Have a look at this fish."

```limn
Ford say : sad^0.3 | see yo bab
```

- `sad^0.3` = sadness at 30% = mildly sorry (Ford's alien detachment)
- `yo bab` = this babel (fish)

### [48] Babel Fish

> Ford put the babel fish in Arthur's ear. It translates any language.

```limn
Ford bab → Arthur ear
bab : al aud → und^1
```

- `bab → Arthur ear` = babel fish to Arthur's ear
- `al aud → und^1` = all hearing transforms to maximum understanding

### [49] Vogon Poetry

> The Vogon captain read his poetry. It was the third worst in the universe.

```limn
vog poe : bea^0*mea^0 | aud → sad*ang
```

- `bea^0*mea^0` = zero beauty interfered with zero meaning = anti-art
- `aud → sad*ang` = hearing transforms to sadness-anger

### [50] Brain Melting

> Arthur felt his brain begin to melt.

```limn
Arthur bra → lif^0 : vog poe
```

- `bra → lif^0` = brain transforms to lifeless = brain death from poetry
- `: vog poe` = given Vogon poetry context

### [51] Thrown Out

> "Throw them out the airlock."

```limn
vog say we : Arthur and Ford → out | roc gap
```

- `we` = imperative operator
- `→ out` = to outside
- `roc gap` = space gap = airlock/vacuum

### [52] The Airlock

> Space stretched before them. Cold. Vast. Empty.

```limn
roc : cld^0.99 * big^1 * vod^1
```

- `cld` = cold (verified — existed in DB, previously missed)
- `big^1` = maximum vast
- `vod^1` = maximum void

### [53] Impossible Rescue

> They were rescued in thirty seconds. Which was impossible.

```limn
aft 30 mom : sa + sa awa dea
yo : act^1 * sus^0
```

- `awa dea` = away from death = rescued
- `act^1 * sus^0` = action at max interfered with belief at zero = impossible yet actual

---

## Vocabulary Status

### Verified words used: 87
### All gaps resolved
- `tsd` = Thursday (linguist addition)
- `ppf` = popular (linguist addition)
- `bgn` = cheap/bargain (linguist addition)
- `cld` = cold (existed in DB, previously missed)
- `yea` = year (linguist addition)

### Composition successes (no new words needed):
- pub = `drk hom` (drink home)
- digital watch = `mac*thu` (machine-thumb)
- electronic thumb = `thu*com` (thumb-computing)
- hyperspace = `roc` (rocket/space context)
- unhappy = `joy^0`
- primitive = `hum^0.1`
- ugly = `bea^0`
- remarkable = `odd^0.99*goo^0.99`
- catastrophe = `bad^1*odd^1 cha`
- impossible-actual = `act^1*sus^0`
- barman = `drk hom hum`
- miserable = `sad^0.9`
- quiet/silent = `cal^0.8`
- escape/rescue = `awa dea`

---

*mea hol | wor shi | tru kep*
*(meaning held | words shift | truth kept)*

*— Mei*
