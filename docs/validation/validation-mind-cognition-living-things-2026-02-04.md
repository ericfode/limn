# Vocabulary Validation: Mind & Cognition + Living Things

**Validator:** polecat/bef
**Date:** 2026-02-04
**Bead:** limn-nqle
**Scope:** 116 words across 2 domains

---

## Living Things Domain (58 words)

```
PASS ani | meaning confirmed
PASS ant | meaning confirmed
PASS avi | meaning confirmed
PASS ber | meaning confirmed
PASS blo | meaning confirmed — resolved collision (block→dam)
PASS bon | meaning confirmed
PASS bra | meaning confirmed
PASS bug | meaning confirmed
WARN but | issue: "butterfly"→"but" collides with English conjunction; marginal value when "bug" covers insects
PASS cel | meaning confirmed
PASS dog | meaning confirmed
WARN duc | issue: marginal core vocab value — specific waterfowl when "avi" covers birds generally
WARN eag | issue: marginal core vocab value — specific raptor when "avi" covers birds
PASS ear | meaning confirmed
PASS eye | meaning confirmed
PASS fis | meaning confirmed
WARN foo | issue: "foo" is well-known programming jargon (foo/bar); minor confusion risk in LLM contexts
PASS fox | meaning confirmed — cultural value (trickster archetype) but borderline specific
FAIL frs | problem: source "hgttg" is not natural English; "frustration" belongs to Mind & Cognition not Living Things; NOT in 1006-word master list; junk data from translation experiment
PASS fru | meaning confirmed
PASS gen | meaning confirmed
WARN goa | issue: marginal core vocab value — goat less culturally iconic than other included animals
WARN gra | issue: potential collision with proposed "gra"=gratitude (Mind & Cognition); needs single-assignment verification
WARN hai | issue: collision with proposed "hai"=hail (Weather domain); must resolve which domain owns this code
PASS han | meaning confirmed
PASS hea | meaning confirmed — resolved collision (hear→aud)
PASS hed | meaning confirmed
WARN hor | issue: borderline core vocab value; culturally significant but specific
PASS hrt | meaning confirmed
PASS hum | meaning confirmed
PASS lif | meaning confirmed
PASS lio | meaning confirmed
PASS liz | meaning confirmed
PASS mam | meaning confirmed
PASS mou | meaning confirmed
PASS ner | meaning confirmed
PASS old | meaning confirmed
WARN owl | issue: marginal core vocab value — specific bird when "avi" covers birds; domain is animal-heavy
PASS pig | meaning confirmed
WARN rab | issue: marginal core vocab value — rabbit less culturally central
PASS rep | meaning confirmed
PASS roo | meaning confirmed
PASS sed | meaning confirmed
WARN she | issue: "she" identical to English pronoun; major discernibility problem
FAIL shk | problem: source "narrative" is not natural English; "shock/surprise" belongs to Mind & Cognition not Living Things; NOT in 1006-word master list; junk data
PASS sic | meaning confirmed
PASS ski | meaning confirmed
PASS sna | meaning confirmed
WARN str | issue: known collision concern with proposed "str"=string (Music domain); all-consonant form reduces readability
FAIL sur | problem: source "narrative" is not natural English; "surprise" belongs to Mind & Cognition not Living Things; NOT in 1006-word master list; junk data
PASS tig | meaning confirmed
PASS tre | meaning confirmed
PASS veg | meaning confirmed — resolved collision (plant→veg, pla=plasma)
PASS wea | meaning confirmed
PASS wha | meaning confirmed
PASS wil | meaning confirmed
PASS wol | meaning confirmed
WARN you | issue: "you" identical to English pronoun; major discernibility problem
```

### Living Things Summary

```
pas: 40 | war: 15 | fal: 3
```

**Fails:**
- `frs`: source "hgttg", wrong domain (emotion not biology), not in master list
- `shk`: source "narrative", wrong domain (emotion not biology), not in master list
- `sur`: source "narrative", wrong domain (emotion not biology), not in master list

**Warnings:**
- `but`: English conjunction collision + marginal value
- `duc`: marginal value (avi covers birds)
- `eag`: marginal value (avi covers birds)
- `foo`: programming jargon collision
- `goa`: marginal value
- `gra`: potential collision with gratitude
- `hai`: collision with proposed hail (Weather)
- `hor`: borderline value
- `owl`: marginal value (avi covers birds)
- `rab`: marginal value
- `she`: English pronoun collision
- `str`: collision concern with string + readability
- `you`: English pronoun collision

**Structural observation:** Domain carries 18 specific animal species + 4 category words (ani, mam, rep, avi). That's 22 animal entries out of 58 (~38%). For a ~1000-word core vocabulary, this is heavily skewed. Category words + composition (e.g., "avi sma" = small bird) would provide sufficient coverage. Recommend cutting 6-8 weakest specific animals (duc, eag, owl, but, goa, rab, fox, hor) to reclaim slots.

---

## Mind & Cognition Domain (58 words)

```
PASS abd | meaning confirmed — abductive reasoning, legitimate epistemology term
PASS abl | meaning confirmed
WARN abs | issue: definition should align with canonical "abstracting" (verb) not "abstract" (adjective)
PASS aim | meaning confirmed
PASS ang | meaning confirmed — distinct from hat (acute vs sustained)
PASS anx | meaning confirmed — distinct from fea (future-oriented vs present-threat)
PASS asu | meaning confirmed
PASS aud | meaning confirmed — resolved collision (hear→aud)
PASS bli | meaning confirmed — resolved collision (believe→bli)
WARN bor | issue: UNRESOLVED COLLISION with "borrowing" (economics domain) — unrelated meanings
PASS cal | meaning confirmed
FAIL cld | problem: source "hgttg" not English; "cold" duplicates col (Physical domain); wrong domain assignment; junk data
WARN cou | issue: phonetic neighbor cow (one letter) is semantic opposite; high granularity
WARN cow | issue: not in canonical vocabulary.json; semantic overlap with fea; "cow" primarily means bovine to English speakers
PASS cre | meaning confirmed
WARN crt | issue: not in canonical vocabulary.json; overlaps with existing cer (certain) in abstract/logic
PASS cur | meaning confirmed
PASS ded | meaning confirmed — one of three classical inference forms (abd/ded/ind)
FAIL des | problem: triple collision — des already means "destination" (spatial) and "despair" (emotions) in canonical vocabulary; "desire" is NOT canonical; overlaps with wan (wanting)
FAIL dou | problem: duplicate of dub — both mean "doubt/doubting"; cannot have two words for same concept
PASS dre | meaning confirmed
FAIL dub | problem: duplicate of dou — dub not in canonical vocabulary.json; dou already holds this slot
PASS evi | meaning confirmed
PASS exc | meaning confirmed
PASS fea | meaning confirmed — distinct from anx and cow
PASS fee | meaning confirmed
PASS foc | meaning confirmed
WARN gue | issue: UNRESOLVED COLLISION with "guest" (social domain) — unrelated meanings
PASS hat | meaning confirmed
PASS hop | meaning confirmed
WARN hyp | issue: not in canonical vocabulary.json; useful concept but needs formal addition
PASS ide | meaning confirmed
PASS ima | meaning confirmed
WARN ind | issue: UNRESOLVED COLLISION with "individual" (social domain) — unrelated meanings
WARN int | issue: UNRESOLVED COLLISION with "intermittent" (temporal domain) — unrelated meanings
PASS joy | meaning confirmed
PASS kno | meaning confirmed
WARN lik | issue: not in vocabulary.json; confidence range (60-80%) overlaps with prb (70-95%)
PASS lov | meaning confirmed
WARN pra | issue: UNRESOLVED COLLISION with "praising" (communication domain) — unrelated meanings
WARN prb | issue: not in vocabulary.json; confidence range overlaps with lik
PASS pri | meaning confirmed
WARN pur | issue: UNRESOLVED COLLISION with "purple" (colors domain) — unrelated meanings
PASS puz | meaning confirmed
PASS rem | meaning confirmed
PASS sad | meaning confirmed
PASS see | meaning confirmed — note potential collision with see="seed" in vocabulary.json
PASS sha | meaning confirmed — resolved collision (share→shr)
PASS sme | meaning confirmed
WARN suf | issue: minor domain fit — suffering is body/health as much as cognition
PASS sus | meaning confirmed
PASS tas | meaning confirmed
WARN thi | issue: KNOWN UNRESOLVED collision with "thin" (Physical domain) — per collision audit v3
PASS tou | meaning confirmed
PASS und | meaning confirmed
WARN unl | issue: not in canonical vocabulary.json; part of unformalized confidence scale
PASS wan | meaning confirmed
PASS wis | meaning confirmed
```

### Mind & Cognition Summary

```
pas: 37 | war: 17 | fal: 4
```

**Fails:**
- `cld`: source "hgttg", duplicates col (cold), wrong domain (Physical not Cognition)
- `des`: triple collision — already means "destination" and "despair"; "desire" is not canonical
- `dou`: duplicate of dub — same concept, cannot have two words
- `dub`: duplicate of dou — not in canonical vocabulary.json

**Warnings (by severity):**

*Unresolved cross-domain collisions (HIGH):*
- `bor`: bored vs borrowing (economics)
- `gue`: guess vs guest (social)
- `ind`: induce vs individual (social)
- `int`: intuition vs intermittent (temporal)
- `pra`: practical vs praising (communication)
- `pur`: purpose vs purple (colors)
- `thi`: thinking vs thin (physical) — known, documented

*Semantic overlap / redundancy (MEDIUM):*
- `crt`: overlaps with cer (certain)
- `lik`: overlaps with prb (probable)
- `prb`: overlaps with lik (likely)
- `cow`: overlaps with fea (fear)
- `dou/dub`: duplicate pair (one already failed)

*Minor issues (LOW):*
- `abs`: verb/adjective form mismatch
- `cou`: phonetic neighbor of opposite (cow)
- `hyp`: not in canonical vocab (but useful)
- `suf`: domain fit question
- `unl`: not in canonical vocab

---

## Combined Summary

```
=== val sum ===
pas: 77 | war: 32 | fal: 7
> pass: 77 | warn: 32 | fail: 7
```

### All Fails

| Word | Domain | Problem |
|------|--------|---------|
| `frs` | Living Things | junk data: source "hgttg", wrong domain, not in master list |
| `shk` | Living Things | junk data: source "narrative", wrong domain, not in master list |
| `sur` | Living Things | junk data: source "narrative", wrong domain, not in master list |
| `cld` | Mind & Cognition | junk data: source "hgttg", duplicates col, wrong domain |
| `des` | Mind & Cognition | triple collision: destination/despair/desire — desire not canonical |
| `dou` | Mind & Cognition | duplicate of dub — same concept |
| `dub` | Mind & Cognition | duplicate of dou — not in canonical vocabulary |

### Critical Findings

1. **7 unresolved cross-domain collisions** found (bor, gue, ind, int, pra, pur + known thi). These need the same treatment as the 10 previously resolved collisions (bri, hea, bel, pla, bir, par, sha, blo, eas).

2. **3 junk data entries** in Living Things (frs, shk, sur) — leaked from HGttG translation or narrative experiments. All have non-English source words and wrong domain assignments. Should be removed.

3. **1 junk data entry** in Mind & Cognition (cld) — source "hgttg", duplicates existing col.

4. **1 duplicate pair** (dou/dub) — must resolve, recommend keeping dou (canonical).

5. **1 triple collision** (des) — desire is not a canonical meaning; wan covers wanting.

6. **Living Things is animal-heavy** — 22 animal entries out of 58 words (38%). Recommend pruning specific animals in favor of composition.

7. **Confidence scale** (crt/prb/lik/dub/unl) exists only in informal docs, not canonical vocabulary. If kept, needs formalization and lik/prb overlap resolved.
