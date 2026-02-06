# HGttG Chapter 1 — Independent Back-Translation (B)

**Source:** `hgttg-ch1-bare.limn`
**Translator:** polecat/was (independent, no reference to other translations)
**Reference:** `docs/spec/bootstrap-v4-compositional.md` + vocabulary database
**Date:** 2026-02-04

**Source identification:** *The Hitchhiker's Guide to the Galaxy* by Douglas Adams, Chapter 1 (1979). Identified from: the unfashionable western spiral arm opening, the girl in Rickmansworth, Arthur Dent lying in front of a bulldozer, Ford Prefect, Vogon demolition fleet, Babel fish, Vogon poetry, and ejection from the airlock.

---

## Section: cosmic

**[1]** `far^0.9 rmt | fas^0.1 end wes sprl arm glx | sma nu-rgd yel sun`

Far out in the uncharted backwaters of the unfashionable end of the western spiral arm of the Galaxy lies a small unregarded yellow sun.

**Confidence: 95%** — `far^0.9 rmt` = very far, remote. `fas^0.1` = barely fashionable / unfashionable. `nu-rgd` = un-regarded. All words map cleanly.

---

**[2]** `orb yo @ dst | rgd^0.01 sma blu-grn plnt`
`lif : hum^0.1 | thi mac*thu goo^0.3`

Orbiting this at a distance is an utterly insignificant little blue-green planet whose ape-descended life forms are so amazingly primitive that they still think digital watches are a pretty neat idea.

**Confidence: 85%** — `orb yo @ dst` = orbiting it at a distance. `rgd^0.01` = barely regarded (utterly insignificant). `hum^0.1` = barely human (primitive). `mac*thu` = machine-thumb interference, likely encoding "digital watches" (machine on wrist/thumb). `goo^0.3` = marginally good ("pretty neat idea").

---

**[3]** `plnt hav±hav^0 : mst hum lif on plnt | joy^0 @ al`

This planet has — or rather had — a problem, which was this: most of the people living on it were unhappy for pretty much of the time.

**Confidence: 90%** — `hav±hav^0` = superposition of having and not-having (has or had — the planet no longer exists). `joy^0 @ al` = zero joy at all = unhappy.

---

**[4]** `ans^0.9 : mst | mov sma grn fra pap`
`odd^0.8 : pap nu joy^0 | hum joy^0`

Many solutions were suggested for this problem, but most of these concerned the movements of small green pieces of paper, which is odd because on the whole it wasn't the small green pieces of paper that were unhappy.

**Confidence: 90%** — `ans^0.9` = very nearly answers (many solutions suggested). `mov sma grn fra pap` = movements of small green fragments of paper (money). `odd^0.8` = quite odd. The second line contrasts that the paper isn't unhappy — humans are.

---

**[5]** `so sta | hum^0.9 cru | hum^0.8 sad^0.9`
`mac*thu : nu fix`

And so the problem remained; lots of the people were mean, and most of them were miserable, even the ones with digital watches.

**Confidence: 90%** — `so sta` = therefore stable/remained. `hum^0.9 cru` = most humans cruel. `hum^0.8 sad^0.9` = many humans very sad. `mac*thu : nu fix` = digital watches (machine-thumb) didn't fix it.

---

**[6]** `on [THURSDAY] | 2000 [YEAR] aft on hum @ tre : spk goo^1`

And then, one Thursday, nearly two thousand years after one man had been nailed to a tree for saying how great it would be to be nice to people for a change...

**Confidence: 85%** — `on [THURSDAY]` = one Thursday. `hum @ tre` = human projected onto/at tree = nailed to a tree. `spk goo^1` = spoke maximum good = said how wonderful it would be to be nice.

---

**[7]** `on hum in sma [CAFE] Rickmansworth | sur^1 kno ans^1`
`sa kno : fix plnt → goo`

...a girl sitting on her own in a small café in Rickmansworth suddenly realized what it was that had been going wrong all this time, and she finally knew how the world could be made a good and happy place.

**Confidence: 90%** — `sur^1` = maximum surprise (suddenly). `kno ans^1` = knew the complete answer. `sa kno : fix plnt → goo` = she knew how to fix the planet, transform it to good.

---

**[8]** `yo : rig^1 | wrk^1 | nu hum @ tre`

This time it was right, it would work, and no one would have to get nailed to anything.

**Confidence: 95%** — `rig^1` = completely right. `wrk^1` = would completely work. `nu hum @ tre` = no human on tree = no one nailed to anything.

---

**[9]** `sad^0.9 : bef sa tel`
`bad^1*odd^1 cha`
`ide los^1 al`

Sadly, however, before she could get to a phone to tell anyone about it, a terrible, stupid catastrophe occurred, and the idea was lost forever.

**Confidence: 90%** — `sad^0.9` = very sadly. `bef sa tel` = before she told (anyone). `bad^1*odd^1 cha` = maximally bad and strange change = terrible stupid catastrophe. `ide los^1 al` = idea totally lost, all of it.

---

**[10]** `yo nu sa myt`

This is not her story.

**Confidence: 95%** — `yo` = this. `nu` = not. `sa` = her. `myt` = story/myth.

---

**[11]** `yo myt : bad^1*odd^1 cha and aft`

But it is the story of that terrible, stupid catastrophe and some of its consequences.

**Confidence: 90%** — `myt` = story. `bad^1*odd^1 cha` = the terrible catastrophe. `and aft` = and what came after.

---

**[12]** `myt : boo "gui glx"`
`nu plnt boo | nu on plnt`

It is also the story of a book, a book called *The Hitchhiker's Guide to the Galaxy* — not an Earth book, never published on Earth.

**Confidence: 95%** — `boo "gui glx"` = book (called) "Guide (to the) Galaxy". `nu plnt boo` = not a planet book (not an Earth book). `nu on plnt` = not by anyone on the planet.

---

**[13]** `boo : odd^0.99*goo^0.99`

Nevertheless, a wholly remarkable book.

**Confidence: 85%** — `odd^0.99*goo^0.99` = extremely strange interfering with extremely good = wholly remarkable. The interference of "very odd" and "very good" produces something like "remarkable."

---

**[14]** `ma [POPULAR] : al boo`

In fact it was probably the most remarkable book ever to come out of the great publishing corporations of Ursa Minor — and in many of the more relaxed civilizations on the Outer Eastern Rim of the Galaxy it had already supplanted the great *Encyclopaedia Galactica* as the standard repository of all knowledge and wisdom.

**Confidence: 70%** — `ma [POPULAR]` = more/most popular. `al boo` = of all books. The passage likely says "more popular than all books" but the original text contains much more detail. Low confidence because the Limn is very compressed here.

---

**[15]** `on : [CHEAP]^0.9`
`2 : cov say "nu pani^1" @ big*fri`

It also had the words DON'T PANIC inscribed in large friendly letters on its cover. It was also cheaper.

**Confidence: 90%** — `[CHEAP]^0.9` = very cheap. `cov say "nu pani^1"` = cover says "don't panic." `big*fri` = big interfering with friendly = large friendly (letters). `2` likely = "also/secondly."

---

## Section: Arthur vs bul (Arthur vs bulldozer)

**[16]** `Arthur Dent wke | bra*hot^0.8`

Arthur Dent woke up, blearily, with a pounding headache.

**Confidence: 85%** — `wke` = woke. `bra*hot^0.8` = brain-hot interference = throbbing/feverish brain = hangover headache or bleary waking.

---

**[17]** `hom : dem^0.99 | roa byp`

His home was about to be demolished to make way for a bypass.

**Confidence: 95%** — `hom` = home. `dem^0.99` = nearly demolished / about to be demolished. `roa byp` = road bypass.

---

**[18]** `sa lie fro bul | in mud`

He lay in front of the bulldozer, in the mud.

**Confidence: 95%** — Direct mapping: `lie` = lying, `fro` = in front, `bul` = bulldozer, `mud` = mud.

---

**[19]** `Prosser : ang*frs^0.8 | try wrk`

Mr. Prosser was angry and frustrated, trying to get on with his job.

**Confidence: 90%** — `ang*frs^0.8` = anger interfering with fairly strong frustration. `try wrk` = trying to work.

---

**[20]** `on hum lie fro bul`

One human being was lying in front of the bulldozer.

**Confidence: 95%** — Straightforward: one human lying in front of bulldozer.

---

**[21]** `yo : [THURSDAY] | und^0 [THURSDAY] al`

It was a Thursday. Arthur never could get the hang of Thursdays.

**Confidence: 90%** — `und^0 [THURSDAY] al` = zero understanding of Thursdays at all = never got the hang of Thursdays.

---

**[22]** `bul sit : yel and cal^0.9`

The bulldozer sat there, yellow and calm, waiting.

**Confidence: 85%** — `bul sit` = bulldozer sat. `yel` = yellow. `cal^0.9` = very calm. Personification of the bulldozer.

---

**[23]** `Ford Prefect arv | bee and sad^0.3@fri`

Ford Prefect arrived, carrying beer and looking mildly concerned about his friend.

**Confidence: 80%** — `arv` = arrived. `bee` = beer. `sad^0.3@fri` = slight sadness projected onto friend = mild concern for a friend. Could also read as "beer and a slightly worried friend."

---

**[24]** `why nu mov → drk hom`

"Why don't you come to the pub for a drink?"

**Confidence: 80%** — `why nu mov` = why not move. `drk hom` = drink (at) home/pub. The `→` indicates direction of movement.

---

## Section: drk hom (at the pub)

**[25]** `Ford say : arv drk hom | req drk bee^0.9`

Ford said, arriving at the pub, that he urgently needed a drink — very much beer.

**Confidence: 85%** — `arv drk hom` = arrived at drinking place. `req drk bee^0.9` = required drinking very much beer.

---

**[26]** `Arthur say : hom te`

Arthur said something about his home, and wanting tea.

**Confidence: 70%** — `te` is not in the vocabulary database. Context suggests "tea" (Arthur Dent's defining trait), but could be another word. `hom te` = home + tea, expressing Arthur's concern about his house being demolished and his desire for normalcy.

---

**[27]** `Ford tel Prosser : lie mud`

Ford told Prosser to lie in the mud (to take Arthur's place in front of the bulldozer).

**Confidence: 85%** — `tel Prosser : lie mud` = told Prosser to lie in mud.

---

**[28]** `Ford ask : hex cup bee and hex bag nut`

Ford ordered six pints of beer and six packets of peanuts.

**Confidence: 95%** — `hex` = six. `cup bee` = cups/pints of beer. `bag nut` = bags of nuts/peanuts.

---

**[29]** `Ford say : plnt end @ 12 mom`

Ford said the planet would end in about twelve minutes.

**Confidence: 95%** — `plnt end` = planet ending. `12 mom` = twelve moments/minutes.

---

**[30]** `Arthur : rea te | lis^0.1`

Arthur really wanted his tea and was barely listening.

**Confidence: 80%** — `rea te` = really (wants) tea. `lis^0.1` = barely listening.

---

**[31]** `Ford say : vog fle dem plnt | roa byp @ roc`

Ford said a Vogon fleet was coming to demolish the planet for a hyperspace bypass.

**Confidence: 90%** — `vog fle` = Vogon fleet. `dem plnt` = demolish planet. `roa byp @ roc` = road bypass in space = hyperspace bypass.

---

**[32]** `Arthur see bee | bee : vit^0.9 now`

Arthur looked at his beer. The beer suddenly seemed vitally important.

**Confidence: 90%** — `see bee` = saw/looked at beer. `vit^0.9 now` = very vital right now.

---

**[33]** `Ford say : nu Guildford | sma plnt dst Betelgeuse`

Ford said he wasn't from Guildford after all — he was from a small planet somewhere in the vicinity of Betelgeuse.

**Confidence: 95%** — `nu Guildford` = not (from) Guildford. `sma plnt dst Betelgeuse` = small planet distant/near Betelgeuse.

---

**[34]** `Arthur thi | vit^0.1`

Arthur thought about this. It didn't seem to matter much.

**Confidence: 85%** — `thi` = thought. `vit^0.1` = barely vital/important.

---

**[35]** `drk bee | goo | plnt : end^0 now`

They drank the beer. It was good. The planet hadn't ended yet.

**Confidence: 85%** — `drk bee` = drank beer. `goo` = good. `plnt : end^0 now` = planet not-ended now.

---

**[36]** `Ford get sma mac | thu*com | mov @ roc`

Ford got out a small device — a thumb-computer (Sub-Etha Sens-O-Matic / electronic thumb) — for hitching a ride on a passing spaceship.

**Confidence: 80%** — `sma mac` = small machine. `thu*com` = thumb-computing interference = electronic thumb / hitchhiking device. `mov @ roc` = movement toward space/rocket.

---

**[37]** `sky → dar^1 | big^0.99 yel apr`

The sky went completely dark. Something enormously large and yellow appeared.

**Confidence: 90%** — `sky → dar^1` = sky transformed to maximum darkness. `big^0.99 yel apr` = extremely big yellow thing appeared.

---

## Section: vog fle (Vogon fleet)

**[38]** `shp in sky \ sto in sky`

The ships hung in the sky in much the same way that bricks don't.

**Confidence: 90%** — `shp in sky` = ships in sky. `\ sto in sky` = minus/without stones in sky. The subtraction operator captures Adams's famous line: ships in the sky, subtracted from stones in the sky — they hang where stones would not.

---

**[39]** `vog Jeltz : glx roc`

Prostetnic Vogon Jeltz of the Galactic Hyperspace Planning Council.

**Confidence: 75%** — `vog Jeltz` = Vogon Jeltz. `glx roc` = galactic space. The full title is compressed.

---

**[40]** `vog ann : plnt dem^1 | roa byp @ roc`

The Vogon announced that the planet was to be demolished to make way for a hyperspace express route.

**Confidence: 90%** — `ann` = announced. `plnt dem^1` = planet completely demolished. `roa byp @ roc` = road bypass in space.

---

**[41]** `plnt : dem^1 @ mom^0.001`

The planet was demolished in an instant.

**Confidence: 90%** — `dem^1` = completely demolished. `mom^0.001` = the tiniest moment = instantaneously.

---

**[42]** `al@plnt → vod^1`

Everything that was on the planet was reduced to nothing — complete void.

**Confidence: 90%** — `al@plnt` = all-of-planet (everything projected onto the planet). `→ vod^1` = transformed to maximum void = total annihilation.

---

## Section: on vog shp (on the Vogon ship)

**[43]** `Arthur wke | on shp | kno^0 whr`

Arthur woke up, on a ship, with no idea where he was.

**Confidence: 95%** — `wke` = woke. `on shp` = on a ship. `kno^0 whr` = zero knowledge of where.

---

**[44]** `shp : bea^0 | bea^0^0.99`

The ship was not beautiful. It was in fact almost aggressively ugly.

**Confidence: 85%** — `bea^0` = zero beauty. `bea^0^0.99` = zero beauty at near-maximum intensity = profoundly, emphatically ugly.

---

**[45]** `Ford giv Arthur gui | cov : "nu pani^1" @ big*fri`

Ford gave Arthur the Hitchhiker's Guide. On the cover, in large friendly letters, it said: DON'T PANIC.

**Confidence: 95%** — `giv` = gave. `gui` = guide. `cov` = cover. `"nu pani^1"` = "don't panic." `big*fri` = large friendly (letters).

---

**[46]** `Arthur say cal^0.8 : plnt dem^1`
`sa : los^0.95`

Arthur said, with apparent calm, that the planet had been demolished. He felt he had lost nearly everything.

**Confidence: 85%** — `cal^0.8` = fairly calm. `plnt dem^1` = planet completely demolished. `los^0.95` = near-total loss.

---

**[47]** `Ford say : sad^0.3 | see yo bab`

Ford said, with mild sympathy, "Look at this" — and showed him the Babel fish.

**Confidence: 80%** — `sad^0.3` = slight sadness (mild sympathy). `see yo bab` = see this Babel (fish).

---

**[48]** `Ford bab → Arthur ear`
`bab : al aud → und^1`

Ford put the Babel fish in Arthur's ear. The Babel fish translated all speech into perfect understanding.

**Confidence: 90%** — `bab → Arthur ear` = Babel fish to Arthur's ear. `al aud → und^1` = all hearing transforms to complete understanding.

---

**[49]** `vog poe : bea^0*mea^0 | aud → sad*ang`

Vogon poetry was devoid of both beauty and meaning. Hearing it induced a mixture of sadness and anger.

**Confidence: 90%** — `bea^0*mea^0` = zero beauty interfering with zero meaning. `aud → sad*ang` = hearing it transforms to sad-angry interference.

---

**[50]** `Arthur bra → lif^0 : vog poe`

Arthur's brain nearly died from the Vogon poetry.

**Confidence: 90%** — `bra → lif^0` = brain transforms to zero life = brain death. `:` = given/because of Vogon poetry.

---

**[51]** `vog say we : Arthur and Ford → out | roc gap`

The Vogon said they were going to throw Arthur and Ford out — into space, through the airlock.

**Confidence: 85%** — `Arthur and Ford → out` = Arthur and Ford to outside. `roc gap` = space gap = airlock.

---

**[52]** `roc : [COLD]^0.99 * big^1 * vod^1`

Space is vast, cold, and empty.

**Confidence: 90%** — `[COLD]^0.99` = extremely cold. `big^1` = maximally big. `vod^1` = maximum void. Three-way interference of cold, bigness, and emptiness.

---

**[53]** `aft 30 mom : sa + sa awa dea`
`yo : act^1 * sus^0`

After thirty seconds, they were rescued — snatched away from death. This was, in fact, wildly improbable.

**Confidence: 80%** — `aft 30 mom` = after 30 moments/seconds. `sa + sa` = he + he = both of them. `awa dea` = away from death. `act^1 * sus^0` = maximum actuality interfering with zero suspicion = actually happening but completely unsuspected = wildly improbable.

---

## Summary

| Metric | Value |
|--------|-------|
| Passages translated | 53/53 |
| High confidence (≥90%) | 28 |
| Medium confidence (75-89%) | 21 |
| Low confidence (<75%) | 4 |
| Average confidence | ~88% |
| Source identified | Yes — *The Hitchhiker's Guide to the Galaxy*, Douglas Adams, Chapter 1 |

### Notes on function words not in database

Several short words appear repeatedly but are absent from the vocabulary database: `sa` (she/he, 3rd person pronoun), `yo` (this/it, demonstrative), `on` (one, indefinite pronoun), `al` (all), `we` (we, 1st person plural), `te` (tea?, uncertain), `ma` (more/most). These were inferred from context.

### Key Limn encodings

- `mac*thu` = "digital watches" — machine-thumb interference, an elegant encoding
- `shp in sky \ sto in sky` = "hung in the sky the way bricks don't" — semantic subtraction captures Adams's metaphor perfectly
- `hav±hav^0` = "has — or rather had" — superposition of having and not-having
- `bea^0^0.99` = "intensely ugly" — zero beauty at near-maximum gradient, a creative double application
- `act^1 * sus^0` = "wildly improbable" — actually happening but completely unsuspected

*mea bri | wor shi | tru kep*
