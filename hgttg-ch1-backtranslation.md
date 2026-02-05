# HGttG Chapter 1 — Back-Translation from Bare Limn

> **Method:** Translated using only `hgttg-ch1-bare.limn` and `docs/spec/bootstrap-v4-compositional.md`,
> plus vocabulary database queries. No access to original English text.
>
> **Source identification:** The Hitchhiker's Guide to the Galaxy by Douglas Adams, Chapter 1.
> Identified from: proper nouns (Arthur Dent, Ford Prefect, Prosser, Vogon Jeltz, Rickmansworth,
> Guildford, Betelgeuse), the book title in the header (`gui glx` = Guide to the Galaxy),
> and narrative elements (bulldozer, babel fish, Vogon poetry, "Don't Panic").

---

## Section: Cosmic (passages 1–15)

**[1]** Very far and remote — at the unfashionable end of the western spiral arm of the Galaxy — lies a small, unregarded yellow sun.

*Confidence: HIGH* — `far^0.9 rmt` = very far, remote. `fas^0.1` = barely fashionable (unfashionable). `nu-rgd` = not-regarded (unnoticed). Clear astronomical opening.

**[2]** Orbiting this at a distance is an utterly insignificant little blue-green planet whose life forms are so barely human that they still think machine-tools are a reasonably good idea.

*Confidence: HIGH* — `rgd^0.01` = barely regarded (utterly insignificant). `hum^0.1` = barely human (primitive). `mac*thu` = machine-tools (digital watches). `goo^0.3` = moderately good ("a pretty neat idea").

**[3]** This planet has — or rather had — a problem: most of the human life on the planet was joyless about everything.

*Confidence: HIGH* — `hav±hav^0` = have-or-have-not (brilliant use of superposition for "has — or rather had"). `joy^0 @ al` = no joy at all (unhappy for pretty much all the time).

**[4]** Many answers were suggested, most involving the movement of small green fragments of paper. Oddly, the paper was not unhappy — the humans were unhappy.

*Confidence: HIGH* — `ans^0.9` = many answers (solutions). `mov sma grn fra pap` = movement of small green fragments of paper (money). `odd^0.8` = quite odd. `pap nu joy^0` = paper not joyless. `hum joy^0` = humans joyless.

**[5]** So it stayed that way — humans were very cruel, and humans were very sad. Machine-tools did not fix anything.

*Confidence: HIGH* — `sta` = stayed/stable. `cru` = cruel/mean. `sad^0.9` = very sad/miserable. `mac*thu : nu fix` = machine-tools (digital watches) didn't fix anything.

**[6]** One Thursday — two thousand years after one human associated with a tree spoke of the ultimate good —

*Confidence: MEDIUM* — `on hum @ tre : spk goo^1` = one human projected onto tree who spoke of supreme good. The man nailed to a tree for saying people should be nice to each other. `@` (projection) for the association with the tree is elegant but ambiguous — could also read "the tree-aspect of one human."

**[7]** — one human in a small café in Rickmansworth was completely surprised to know the complete answer. She knew how to fix the planet and make it good.

*Confidence: HIGH* — `sur^1` = completely surprised (suddenly realized). `kno ans^1` = knew the total answer. `fix plnt → goo` = fix the planet → good.

**[8]** It was completely right — it would have completely worked — and not like the human associated with the tree.

*Confidence: MEDIUM* — `rig^1` = completely right. `wrk^1` = would have completely worked. `nu hum @ tre` = not like the man nailed to a tree (no one would have to get nailed to anything). The negation of the tree reference is subtle.

**[9]** Very sadly, before she could tell anyone, a terribly strange change occurred, and the idea was completely lost forever.

*Confidence: HIGH* — `sad^0.9` = very sadly. `bef sa tel` = before she could tell. `bad^1*odd^1 cha` = maximally bad interfered with maximally odd change (terrible, stupid catastrophe). `ide los^1 al` = idea completely lost, all of it.

**[10]** This is not her story.

*Confidence: HIGH* — `yo nu sa myt` = this is not her myth/story.

**[11]** This is the story of the terribly strange change and what came after.

*Confidence: HIGH* — `yo myt : bad^1*odd^1 cha and aft` = this is the story of the terrible strange change and after.

**[12]** The story is also a book: "The Guide to the Galaxy." Not a planet book — not from this planet.

*Confidence: HIGH* — `boo "gui glx"` = book "Guide to the Galaxy". `nu plnt boo | nu on plnt` = not a planet book, not from this planet (not an Earth book, never published on Earth).

**[13]** The book is extremely strange and extremely good.

*Confidence: MEDIUM* — `odd^0.99*goo^0.99` = nearly maximally odd interfered with nearly maximally good. Likely "a wholly remarkable book." The interference operator creates an emergent sense of "remarkable."

**[14]** The most popular of all books.

*Confidence: MEDIUM* — `ma [POPULAR] : al boo` = most/made popular among all books. "More popular" or "the most remarkable/popular." Context suggests "the most remarkable book ever published."

**[15]** First: it is very cheap. Second: its cover says "Don't Panic" in large, friendly letters.

*Confidence: HIGH* — `[CHEAP]^0.9` = very cheap. `cov say "nu pani^1"` = cover says "Don't Panic." `big*fri` = big interfered with friendly = large friendly (letters). Classic HGttG detail.

---

## Section: Arthur vs. Bulldozer (passages 16–24)

**[16]** Arthur Dent woke up with a hot brain — a headache.

*Confidence: HIGH* — `wke` = woke. `bra*hot^0.8` = brain interfered with heat = headache/hangover.

**[17]** His home was about to be almost completely demolished for a road bypass.

*Confidence: HIGH* — `dem^0.99` = nearly completely demolished. `roa byp` = road bypass.

**[18]** He lay down in front of the bulldozer, in the mud.

*Confidence: HIGH* — `lie fro bul` = lying in front of bulldozer. `in mud` = in the mud.

**[19]** Prosser was angry and very frustrated, trying to work.

*Confidence: HIGH* — `ang*frs^0.8` = anger interfered with strong frustration. `try wrk` = trying to work.

**[20]** One human lying in front of a bulldozer.

*Confidence: HIGH* — Simple restatement. Mr. Prosser's perspective on the situation.

**[21]** It was Thursday. He never understood Thursdays at all.

*Confidence: HIGH* — `und^0 [THURSDAY] al` = understood nothing about Thursdays at all. "I never could get the hang of Thursdays."

**[22]** The bulldozer sat there: yellow and very calm.

*Confidence: HIGH* — `yel and cal^0.9` = yellow and very calm. The bulldozer described as placidly sitting.

**[23]** Ford Prefect arrived, with beer and slight sad concern for his friend.

*Confidence: HIGH* — `bee and sad^0.3@fri` = beer and the sadness-component of friendship (concern for a friend, slight worry).

**[24]** Why not move — go to the pub?

*Confidence: HIGH* — `why nu mov → drk hom` = why not move → drinking home (pub). Ford trying to get Arthur to come to the pub.

---

## Section: At the Pub (passages 25–37)

**[25]** Ford said he had arrived at the pub and needed to drink a lot of beer.

*Confidence: HIGH* — `arv drk hom` = arrived at the drinking home (pub). `req drk bee^0.9` = required drinking much beer.

**[26]** Arthur said: but my home!

*Confidence: MEDIUM* — `hom te` is ambiguous. `te` is not in the vocabulary database — possibly an emphatic particle or demonstrative. Context suggests Arthur protesting about his home being demolished. Best reading: "My home [is being demolished]!"

**[27]** Ford told Prosser to lie in the mud.

*Confidence: HIGH* — Ford convinces Prosser to take Arthur's place lying in front of the bulldozer.

**[28]** Ford asked for six cups of beer and six bags of nuts.

*Confidence: HIGH* — `hex cup bee` = six cups of beer. `hex bag nut` = six bags of nuts/peanuts. In the original: six pints of bitter and some peanuts.

**[29]** Ford said: the planet ends in twelve moments.

*Confidence: HIGH* — `plnt end @ 12 mom` = planet ends in 12 moments (minutes). Ford telling Arthur the world is about to end.

**[30]** Arthur was reacting to this, barely listening.

*Confidence: MEDIUM* — `rea te` is ambiguous (`te` again unclear). `lis^0.1` = barely listening. Arthur is skeptical and not really paying attention.

**[31]** Ford said: the Vogon fleet will demolish the planet — a road bypass through space.

*Confidence: HIGH* — `vog fle dem plnt` = Vogon fleet demolishes planet. `roa byp @ roc` = road bypass projected onto space — the hyperspace bypass, deliberately paralleling Arthur's house and the road bypass.

**[32]** Arthur saw his beer. The beer was vitally important right now.

*Confidence: HIGH* — `bee : vit^0.9 now` = beer is nearly vitally important now. Arthur suddenly appreciating his beer.

**[33]** Ford said: he was not from Guildford, but from a small planet distant from Betelgeuse.

*Confidence: HIGH* — `nu Guildford` = not from Guildford. `sma plnt dst Betelgeuse` = small planet near/distant from Betelgeuse.

**[34]** Arthur thought. He felt barely alive.

*Confidence: HIGH* — `thi` = thinking. `vit^0.1` = barely vital/alive (stunned).

**[35]** Drink the beer. It's good. The planet hasn't ended yet.

*Confidence: HIGH* — `drk bee | goo | plnt : end^0 now` = drink beer, good, planet not-ended now. A moment of pragmatic comfort.

**[36]** Ford got a small machine — an electronic thumb — for movement through space.

*Confidence: HIGH* — `sma mac` = small machine/device. `thu*com` = thumb interfered with computing = electronic thumb (hitchhiking device). `mov @ roc` = movement projected onto space = space travel/hitchhiking.

**[37]** The sky went completely dark. Enormous yellow shapes appeared.

*Confidence: HIGH* — `sky → dar^1` = sky transformed to total darkness. `big^0.99 yel apr` = nearly maximally big yellow things appeared. The Vogon ships arriving.

---

## Section: Vogon Fleet (passages 38–42)

**[38]** Ships in the sky — minus stones in the sky.

*Confidence: HIGH* — `shp in sky \ sto in sky` = ships-in-sky subtracted stones-in-sky. This is a brilliant Limn rendering of "The ships hung in the sky in much the same way that bricks don't." The subtraction operator captures the joke perfectly: ships in the sky, minus the way stones would be in the sky (i.e., they wouldn't stay up).

**[39]** Vogon Jeltz: galactic authority.

*Confidence: MEDIUM* — `glx roc` = galaxy space/rocket. Could mean "galactic headquarters" or "galactic fleet commander." Prostetnic Vogon Jeltz of the Galactic Hyperspace Planning Council.

**[40]** The Vogon announced: the planet is to be completely demolished for a road bypass through space.

*Confidence: HIGH* — Direct parallel to the road bypass at Arthur's house. `roa byp @ roc` = road bypass projected onto space = hyperspace bypass.

**[41]** The planet: completely demolished in the tiniest moment.

*Confidence: HIGH* — `dem^1 @ mom^0.001` = completely demolished in a near-zero moment (an instant). Earth is destroyed instantly.

**[42]** Everything of the planet became complete void.

*Confidence: HIGH* — `al@plnt → vod^1` = all-of-the-planet transforms to complete void. Total annihilation.

---

## Section: On the Vogon Ship (passages 43–53)

**[43]** Arthur woke up on a ship, not knowing where he was.

*Confidence: HIGH* — `wke` = woke. `on shp` = on a ship. `kno^0 whr` = knowing nothing about where.

**[44]** The ship was not beautiful. It was almost completely not-beautiful.

*Confidence: HIGH* — `bea^0` = not beautiful. `bea^0^0.99` = intensely not-beautiful (spectacularly ugly). Double-negation with gradient for emphasis.

**[45]** Ford gave Arthur the Guide. The cover said "Don't Panic" in large friendly letters.

*Confidence: HIGH* — Callback to passage [15]. `giv Arthur gui` = gave Arthur the guide. `cov : "nu pani^1" @ big*fri` = cover says "Don't Panic" in big-friendly (letters).

**[46]** Arthur said, fairly calmly: the planet has been completely demolished. He felt nearly total loss.

*Confidence: HIGH* — `cal^0.8` = fairly calm. `plnt dem^1` = planet completely demolished. `los^0.95` = near-total loss. "He said it calmly, but he felt the loss."

**[47]** Ford said, slightly sad: look at this babel-fish.

*Confidence: HIGH* — `sad^0.3` = slightly sad. `bab` = babel fish (translator creature). Ford introduces the Babel fish.

**[48]** Ford put the babel-fish into Arthur's ear. The babel-fish translates all hearing into complete understanding.

*Confidence: HIGH* — `bab → Arthur ear` = babel-fish into Arthur's ear. `al aud → und^1` = all hearing transforms to complete understanding. The Babel fish's function.

**[49]** Vogon poetry: not beautiful and not meaningful. Hearing it produced sadness and anger.

*Confidence: HIGH* — `bea^0*mea^0` = not-beautiful interfered with not-meaningful (ugly and pointless). `aud → sad*ang` = hearing transforms to sadness and anger. Vogon poetry is notoriously terrible.

**[50]** Arthur's brain nearly died from the Vogon poetry.

*Confidence: HIGH* — `bra → lif^0` = brain transforms to not-alive (brain death). `: vog poe` = from/because of Vogon poetry.

**[51]** The Vogon said: Arthur and Ford will go out through a gap in space.

*Confidence: HIGH* — `Arthur and Ford → out` = Arthur and Ford go out. `roc gap` = gap in space (airlock). They're being thrown out the airlock.

**[52]** Space: extremely cold, maximally big, and completely void.

*Confidence: HIGH* — `[COLD]^0.99 * big^1 * vod^1` = nearly max cold, max big, max void. Triple interference describing the vastness and emptiness of space.

**[53]** After thirty moments: they were alive, away from death. This was completely actual and completely unsuspected.

*Confidence: HIGH* — `sa + sa` = he + he (the two of them). `awa dea` = away from death (survived). `act^1 * sus^0` = completely actual (real) interfered with completely unsuspected (impossible) = improbable but real. They were rescued by a passing ship — an event of extraordinary improbability.

---

## Summary

**Source text identified:** The Hitchhiker's Guide to the Galaxy, Chapter 1, by Douglas Adams.

**Overall confidence:** HIGH — The narrative arc is unmistakable: the cosmic opening, the girl in Rickmansworth, Arthur vs. the bulldozer, Ford at the pub, the Vogon fleet destroying Earth, and escape on the Vogon ship.

**Notable Limn techniques observed:**
- `hav±hav^0` (superposition) for "has — or rather had" [3]
- `shp in sky \ sto in sky` (subtraction) for the bricks joke [38]
- `bea^0^0.99` (nested gradient on negation) for "spectacularly ugly" [44]
- `act^1 * sus^0` (interference) for "improbable but real" [53]
- `thu*com` (interference) for "electronic thumb" [36]
- `big*fri` (interference) for "large friendly letters" [15, 45]
- Consistent `roa byp` / `roa byp @ roc` parallelism between house demolition and planet demolition [17, 31, 40]

**Words not in vocabulary database:** `nu` (negation), `yo` (demonstrative "this/it"), `sa` (third person pronoun), `te` (unclear — possibly emphatic/demonstrative), `on` (indefinite "one/a"), `we` (unclear — possibly "will" or first person plural).
