# Experiment 011: Vocabulary Validation — Social + Communication + Metalinguistic + Community & Culture

**Date:** 2026-02-04
**Validator:** limn/polecats/sci
**Issue:** limn-cun6
**Scope:** 90 words across 4 domains
**Method:** Manual review against full vocabulary (1040 words) for definition correctness, meaning reasonableness, and discernibility

---

## Communication Domain (24 words)

```
PASS agr | meaning confirmed — natural truncation of "agree"
PASS ans | meaning confirmed — natural truncation of "answer"
PASS arg | meaning confirmed — natural truncation of "argue"
PASS ask | meaning confirmed — direct match
PASS cle | meaning confirmed — natural truncation of "clear"; clr (clarity, LLM domain) is domain-separated
PASS cri | meaning confirmed — natural truncation of "criticize"
PASS dis | meaning confirmed — natural truncation of "disagree"
PASS hid | meaning confirmed — natural truncation of "hide"
PASS jok | meaning confirmed — natural truncation of "joke"
PASS lis | meaning confirmed — natural truncation of "listen"
WARN mea | issue: semantic overlap with sem (semantics, Metalinguistic) — both concern linguistic meaning. Distinguished as mea=concept vs sem=field-of-study, but proximity is notable
PASS nam | meaning confirmed — direct match
PASS pub | meaning confirmed — natural truncation of "public"
PASS que | meaning confirmed — natural truncation of "question"
PASS rum | meaning confirmed — natural truncation of "rumor"
PASS say | meaning confirmed — direct match
PASS sec | meaning confirmed — natural truncation of "secret"; dense s-e-* phonetic space but all neighbors are semantically distinct
WARN sen | issue: phonetically close to sem (semantics) — both s-e-[mn] in language-related domains. Risk of confusion in speech/fast reading
PASS sho | meaning confirmed — natural truncation of "show"
PASS tal | meaning confirmed — natural truncation of "talk"
PASS tel | meaning confirmed — natural truncation of "tell"
PASS tru | meaning confirmed — direct match
WARN war | issue: source ambiguity — "war" strongly evokes warfare/conflict in English, not "warn". No separate war=warfare word exists, so no collision, but guessability is poor
PASS wor | meaning confirmed — direct match
```

## Community & Culture Domain (11 words)

```
PASS cln | meaning confirmed — natural truncation of "clan"
PASS eld | meaning confirmed — natural truncation of "elder"
PASS fes | meaning confirmed — natural truncation of "festival"
PASS gua | meaning confirmed — cultural import (guanxi), natural truncation from Mandarin term
PASS mut | meaning confirmed — natural truncation of "mutual"
PASS myt | meaning confirmed — natural truncation of "myth"
WARN nal | issue: guessability — "nal" from "national" skips more obvious "nat" (which is unused). English speakers would not guess nation from "nal"
PASS san | meaning confirmed — cultural import (sangha), natural truncation from Pali/Sanskrit
WARN trd | issue: consonant-heavy abbreviation — "trd" from "tradition" is hard to guess. tra (transformation) is taken, forcing this awkward form
PASS ubu | meaning confirmed — cultural import (ubuntu), natural truncation
PASS umm | meaning confirmed — cultural import (ummah), natural truncation from Arabic
```

## Metalinguistic Domain (20 words)

```
WARN bab | issue: source field says "scifi" instead of "babel" — metadata inconsistency. Word itself is reasonable (Babel = language/translation reference)
PASS can | meaning confirmed — direct match; function word appropriately placed in Metalinguistic domain
PASS clp | meaning confirmed — natural truncation of "collapse"; Limn-specific meaning (resolve ambiguity)
WARN gui | issue: source field says "scifi" instead of "guide" — metadata inconsistency. Meaning (guide/guidebook) fits Metalinguistic domain well
PASS hav | meaning confirmed — direct match; function word in Metalinguistic domain
PASS how | meaning confirmed — direct match; interrogative in Metalinguistic domain
PASS idi | meaning confirmed — natural truncation of "idiom"
PASS lex | meaning confirmed — natural truncation of "lexicon"
PASS met | meaning confirmed — natural truncation of "metaphor"
PASS mls | meaning confirmed — performative paradox word (claims meaninglessness while having meaning); deliberate design
FAIL mud | problem: domain misfit — "mud, wet earth" is a Physical World concept, not Metalinguistic. Source field says "hgttg" suggesting HGttG import, but the meaning is plain physical material. Should be in Physical World domain or reclassified with a metalinguistic meaning
PASS rcr | meaning confirmed — performative word (recursive, references own structure); deliberate design
PASS ref | meaning confirmed — natural truncation of "reference"
WARN sem | issue: semantic overlap with mea (meaning, Communication) — both are about linguistic meaning. sem=field-of-study vs mea=concept-itself provides distinction, but learners may confuse them
WARN slf | issue: semantic overlap with sel (self, Social) — both derive from "self". Distinguished as slf=metalinguistic self-reference (performative) vs sel=identity/personhood, but two "self" words is confusing
PASS sup | meaning confirmed — Limn-native concept (superposition); natural truncation
PASS whn | meaning confirmed — recognizable truncation of "when"
PASS who | meaning confirmed — direct match
PASS whr | meaning confirmed — recognizable truncation of "where"
PASS why | meaning confirmed — direct match
```

## Social Domain (35 words)

```
WARN ann | issue: source field says "narrative" instead of "announce" — metadata inconsistency. Also "ann" reads as a name (Ann/Anne) to English speakers, moderate source ambiguity
PASS cer | meaning confirmed — natural truncation of "ceremony"
PASS chi | meaning confirmed — natural truncation of "child"
PASS cru | meaning confirmed — natural truncation of "cruel"
WARN cul | issue: semantic overlap with cus (custom) and trd (tradition, Community) — three words for very similar concepts in the social/cultural field. cul=culture, cus=custom/practice, trd=tradition. Distinguishable but crowded
WARN cus | issue: semantic overlap with cul (culture) — see above. Additionally "cus" may evoke "cuss" (curse) in English, minor source ambiguity
PASS dut | meaning confirmed — natural truncation of "duty"
PASS ene | meaning confirmed — natural truncation of "enemy"
PASS fai | meaning confirmed — natural truncation of "fair"
PASS fam | meaning confirmed — natural truncation of "family"
PASS fol | meaning confirmed — natural truncation of "follow"
PASS fri | meaning confirmed — natural truncation of "friend"
PASS giv | meaning confirmed — natural truncation of "give"
PASS gre | meaning confirmed — natural truncation of "greedy"
PASS hel | meaning confirmed — natural truncation of "help"
WARN hon | issue: duplicate concept — hns (honesty) exists in wider vocabulary. hon=adjective, hns=noun form of same concept. In a 1000-word vocabulary, both forms may be wasteful
PASS hur | meaning confirmed — natural truncation of "hurt"
WARN kin | issue: duplicate concept — knd (kindness) exists in wider vocabulary. kin=adjective, knd=noun form of same concept. Same issue as hon/hns
WARN lea | issue: source ambiguity — "lea" could suggest leaf, lean, learn, or lead (metal). "leader" is not the most natural reading of this abbreviation
PASS los | meaning confirmed — natural truncation of "loss"
PASS loy | meaning confirmed — natural truncation of "loyal"
PASS obe | meaning confirmed — natural truncation of "obey"
PASS oth | meaning confirmed — natural truncation of "other"
PASS par | meaning confirmed — natural truncation of "parent"; known resolved collision (par=parent, prt=partial)
WARN pro | issue: source ambiguity — "pro" strongly suggests "professional" or "for/in favor of" in English, not "protecting". Truncation of "protect" is not the most natural reading
PASS pun | meaning confirmed — natural truncation of "punish"
PASS reb | meaning confirmed — natural truncation of "rebel"
PASS rew | meaning confirmed — natural truncation of "reward"
PASS rul | meaning confirmed — natural truncation of "rule"
WARN sel | issue: semantic overlap with slf (self-reference, Metalinguistic) — two words deriving from "self". sel=identity vs slf=metalinguistic reflexivity, but the distinction requires domain awareness
PASS shr | meaning confirmed — natural truncation of "share"; known resolved collision (sha=shame, shr=share)
PASS tak | meaning confirmed — natural truncation of "take"
WARN tea | issue: source ambiguity — "tea" strongly evokes the beverage in English, not "team". No Limn word for tea-the-drink exists, so no collision, but guessability suffers
PASS unf | meaning confirmed — natural truncation of "unfair"
PASS vic | meaning confirmed — natural truncation of "victory"
```

---

## val sum
> validation summary

```
=== val sum ===
pas: 71 | war: 18 | fal: 1
> pass: 71 | warn: 18 | fail: 1

fal lis:
> fail list:
- mud: domain misfit — Physical World concept placed in Metalinguistic domain with "hgttg" source tag

war lis:
> warn list:
- mea: semantic overlap with sem (semantics)
- sen: phonetically close to sem (semantics), both in language domains
- war: "war" evokes warfare, not "warn" — source ambiguity
- nal: "nal" from "national" — poor guessability, "nat" is unused
- trd: consonant-heavy "trd" from "tradition" — hard to guess
- bab: source field metadata says "scifi" instead of "babel"
- gui: source field metadata says "scifi" instead of "guide"
- sem: semantic overlap with mea (meaning)
- slf: semantic overlap with sel (self)
- ann: source field says "narrative"; "ann" reads as a name
- cul: semantic overlap with cus (custom) and trd (tradition)
- cus: semantic overlap with cul (culture); "cus" evokes "cuss"
- hon: duplicate concept — hns (honesty) also exists
- kin: duplicate concept — knd (kindness) also exists
- lea: "lea" ambiguous — could be leaf, lean, learn
- pro: "pro" evokes professional/for, not "protecting"
- sel: semantic overlap with slf (self-reference)
- tea: "tea" evokes the beverage, not "team"
```

## Recommendations

### Critical (FAIL)

1. **mud**: Move to Physical World domain, or give it a Metalinguistic meaning if the HGttG reference warrants it. Current state is a domain misclassification.

### Structural (multiple WARNs, same root cause)

2. **sel/slf overlap**: Consider whether both are needed. If the metalinguistic self-reference concept is essential (performative words are a Limn feature), document the distinction clearly. If not, merge into one word.

3. **hon/hns and kin/knd duplication**: Adjective/noun pairs for the same concept consume two vocabulary slots each. Consider keeping only the adjective form and deriving nouns compositionally (e.g., `hon ity` or `kin ity`).

4. **cul/cus/trd semantic crowding**: Three words for culture/custom/tradition is dense. `cul` and `trd` might be sufficient; `cus` overlaps both.

### Source field metadata

5. **bab, gui, ann source fields**: Source should be the English word the abbreviation derives from (babel, guide, announce), not the thematic category (scifi, narrative). This is a data quality issue.

### Source ambiguity (low priority)

6. Words where the 3-letter form's most obvious English reading differs from the Limn meaning: **war** (warfare vs warn), **pro** (professional vs protecting), **tea** (beverage vs team), **lea** (leaf vs leader). These are acceptable trade-offs in a constrained namespace but affect learnability.
