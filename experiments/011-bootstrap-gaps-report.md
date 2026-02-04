# Experiment 011: Bootstrap Gaps Report

**Task:** Define Limn words using ONLY Limn (no English)

**Outcome:** Successfully defined ~100 words before hitting fundamental vocabulary gaps and circular dependencies.

---

## Summary: The Bootstrap Paradox

Limn can define **concrete, physical concepts** very well:
- ✓ Matter states: `sol: sta of mat tha har and kep sha`
- ✓ Elements: `aqu: liq tha flo in riv and oce`
- ✓ Physical actions: `mel: cha fro sol to liq fro hot`

Limn **struggles with abstract, mental, and social concepts** because:
1. They form circular definition chains
2. They reference undefined primitives
3. They require concepts that don't exist yet

---

## Critical Gaps Identified

### GAP 1: "think" (verb) - COLLISION ⚠️

**Problem:** `thi` already means "thing"

**Impact:** Cannot define:
- `min: par of you tha thi, fee, kno` ← Can't say "that thinks"!
- `per: liv thi tha thi, tal, and use too` ← Can't define "person"!
- `rea: use min to thi log` ← Can't define "reason"!

**Attempted definition:** `min: par of you tha ??? fee, kno`

**Blocker:** Need new word for "think" verb
- Candidates: `cog` (cognition), `pon` (ponder), `men` (mental), `ide` (ideate)

**Severity:** CRITICAL - blocks all cognitive/mental vocabulary

---

### GAP 2: Circular Definitions - FUNDAMENTAL ISSUE

**Examples of circles:**

```limn
kno: hav inf in min, und tru
und: kno mea of thi
→ kno ← und ← kno (CIRCULAR!)

lov: str fee of car and wan to be wit
car: fee of lov for, wan to hel
→ lov ← car ← lov (CIRCULAR!)

tru: wha rea bei
rea: wha tru exi, nu mad up
→ tru ← rea ← tru (CIRCULAR!)
```

**Problem:** Abstract concepts define each other in closed loops

**Impact:** Cannot bootstrap mental/social concepts from physical primitives alone

**Possible solution:** Accept some concepts as AXIOMS (undefined primitives)
- Axiom candidates: `bei` (be), `kno` (know), `fee` (feel), `wan` (want), `goo` (good)

**Severity:** FUNDAMENTAL - requires design decision

---

### GAP 3: Abstract Values (good, bad, true, false)

**Attempted definitions:**

```limn
goo: wha you wan, wha mak thi bet
→ Requires "wan" (want) - undefined!
→ Requires "bet" (better) - which requires "goo"! (CIRCULAR)

bad: wha you nu wan, wha mak thi wor
→ Same circular dependency

tru: wha rea bei, nu fal
→ Requires "rea" (real) - how to define without "tru"?

fal: nu tru, nu rea
→ Defines by negation of undefined term!
```

**Problem:** Values cannot be defined from physical primitives

**Impact:** Cannot define:
- Ethics vocabulary (aret, jun, vir)
- Epistemic vocabulary (bel, dou, cer)
- Aesthetic vocabulary (bea, ugl, art)

**Severity:** HIGH - blocks philosophy vocabulary

---

### GAP 4: Modal Verbs (can, must, should, may)

**Attempted definitions:**

```limn
can: bei abl to do
→ Requires "abl" (able) - undefined!

mus: hav to do, no cho
→ Requires "cho" (choice) - undefined!

sho: wha goo to do
→ Requires "goo" (good) - undefined! (GAP 3)

may: bei pos to hap
→ Requires "pos" (possible) - undefined!
```

**Problem:** Modality requires meta-concepts (ability, possibility, obligation)

**Impact:** Cannot express:
- Necessity: "you mus eat to liv"
- Possibility: "it may rai tom"
- Permission: "you may go now"
- Advice: "you sho res aft wor"

**Severity:** HIGH - blocks practical instruction/advice

---

### GAP 5: "person" - CRITICAL SOCIAL PRIMITIVE

**Attempted definition:**

```limn
per: liv thi tha thi, tal, and use too
```

**Blockers:**
1. `thi` (think) - doesn't exist! (GAP 1)
2. `tal` (talk) - needs definition
3. `use` (use) - needs definition
4. Circular: "person uses tools" but "tool: thi per use" !

**Alternative attempts:**

```limn
per: hum liv thi
→ Requires "hum" (human) - which is just "per" again!

per: liv thi wit min
→ Requires "min" (mind) - which requires "thi" (think)!

per: liv thi lik you and me
→ Requires "you" and "me" - which are persons! (CIRCULAR)
```

**Impact:** Cannot define ANY social concepts:
- Relationships: `fri`, `foe`, `fam`, `mar`
- Roles: `lea`, `fol`, `rul`, `tea`, `stu`
- Actions: `hel`, `hur`, `giv`, `tak`, `sha`

**Severity:** CRITICAL - blocks all social vocabulary

---

### GAP 6: Mental States (want, need, believe, hope)

**Attempted definitions:**

```limn
wan: des to hav or do
→ Requires "des" (desire) - synonym of "wan"! (CIRCULAR)

nee: mus hav to liv or fun
→ Requires "mus" (must) - undefined! (GAP 4)
→ Requires "fun" (function) - undefined!

bel: thi thi bei tru wit no pro
→ Requires "thi" (think) - doesn't exist! (GAP 1)
→ Requires "pro" (proof) - undefined!

hop: wan thi to hap in fut, fee pos
→ Requires "wan" - circular!
→ Requires "pos" (possible) - undefined!
```

**Problem:** Mental states are irreducible to physical primitives

**Severity:** HIGH - blocks psychological vocabulary

---

### GAP 7: "suffering" (suf) - BUDDHIST VOCABULARY BLOCKER

**Attempted definition:**

```limn
suf: fee of pai, sad, or bad tha las
```

**Blockers:**
1. `pai` (pain) - how to define?
   - `pai: fee in bod or min tha bad and wan to sto`
   - Requires `bad` (GAP 3), `wan` (GAP 6), `sto` (stop - undefined!)

2. `sad` (sadness) - emotion without physical referent
   - `sad: fee bad and dow, opp of joy`
   - Requires `bad` (GAP 3), `fee` (feel - axiom?)

3. Circular with `duk` (dukkha)
   - `duk: suf, state of being not satisfied`
   - Both define each other!

**Impact:** Cannot properly define:
- `kar: act wit cau tha lea to suf or hap`
- `nir: end of suf, sta of pea`
- `duk: suf fro imp of lif`

**Severity:** MEDIUM - philosophy still expressible, just less precise

---

### GAP 8: "natural" vs "artificial"

**Attempted definition:**

```limn
nat: how thi bei wit no per mak
```

**Blockers:**
1. `per` (person) - undefined! (GAP 5)
2. Circular: nature is "not made by persons" but "person" requires "natural being"?

**Alternative:**

```limn
nat: fol tao, bei lik aer and aqu
→ Requires understanding `tao` - which requires understanding `nat`! (CIRCULAR)
```

**Impact:** Cannot properly define:
- `zir: spo and nat, nu for`
- `wuw: act in nat way, no try`
- Distinction between "tree" (nat) and "house" (art)

**Severity:** MEDIUM - Daoist concepts lose precision

---

### GAP 9: "purpose" vs "end" (telo ambiguity)

**Problem:** `telo` means both:
1. "End" (endpoint): `the end of the story`
2. "Purpose" (goal): `the purpose of life`

**In Limn:**

```limn
telo: end or pur of thi, why thi exi
```

**But these are different concepts!**
- End: `las par, whe thi sto`
- Purpose: `rea why thi mad, wha it for`

**Impact:** Ambiguous in contexts like:
- `eud is telo of lif` - "end" or "purpose"? (Both valid!)
- `telo of sto` - clearly endpoint
- `telo of too` - clearly purpose

**Severity:** LOW - usually clear from context, but could use distinct words

---

### GAP 10: Psychological Verbs (trust, hope, doubt, believe)

**All require undefined mental primitives:**

```limn
tru: fee tha per wil do wha say
→ Requires "per" (GAP 5), "say" (speech - needs definition)

hop: wan thi to hap, fee it may hap
→ Requires "wan" (GAP 6), "may" (GAP 4)

dou: nu bei sur if thi tru
→ Requires "sur" (sure - undefined!), "tru" (GAP 3)

bel: thi som thi bei tru
→ Requires "thi" verb (GAP 1), "tru" (GAP 3)
```

**Impact:** Cannot express epistemic states, uncertainty, faith

**Severity:** MEDIUM - impacts philosophical and religious vocabulary

---

## Patterns Observed

### What WORKS (Concrete, Physical):

```limn
✓ aqu: liq tha flo in riv and oce and fal as rai
✓ sol: sta of mat tha har and kep sha
✓ mel: cha fro sol to liq fro hot
✓ bri: giv muc lux
✓ ris: mov up
✓ big: hav mor spa tha sma thi
```

**Pattern:** Physical concepts with sensory referents define cleanly.

### What FAILS (Abstract, Mental, Social):

```limn
✗ per: liv thi tha ??? [needs "think"]
✗ kno: und tru [needs "understand" and "true"]
✗ goo: wha you wan [needs "want"]
✗ wan: des to hav [CIRCULAR with "desire"]
✗ thi: ??? [COLLISION with "thing"]
```

**Pattern:** Abstract concepts form circular definition chains.

---

## The Fundamental Problem

**Physical primitives cannot generate abstract concepts through definition alone.**

You can define:
- `ice` from `water` + `cold` + `solid`
- `river` from `water` + `flow`
- `mountain` from `earth` + `big` + `high`

You CANNOT define:
- `good` from physical primitives (it's a value judgment)
- `know` from physical primitives (it's a mental state)
- `person` from physical primitives (requires intentionality)

**This is a known problem in philosophy:** You can't derive "ought" from "is" (Hume's guillotine).

---

## Proposed Solutions

### Solution 1: Accept Axioms (Undefined Primitives)

Declare these concepts as AXIOMS (undefined, known through experience):

**Cognitive Axioms:**
- `kno` (know)
- `fee` (feel)
- `thi` (think) - needs new word!

**Volitional Axioms:**
- `wan` (want)
- `nee` (need)

**Value Axioms:**
- `goo` (good)
- `bad` (bad)
- `tru` (true)
- `fal` (false)

**Modal Axioms:**
- `can` (can)
- `mus` (must)

**Social Axiom:**
- `per` (person)

**Total:** ~12 axioms would unlock hundreds of definitions.

---

### Solution 2: Add Missing Vocabulary

**Critical additions needed:**

1. **`cog`** or **`pon`** = "think" (verb)
   - Unlocks: `per`, `min`, `rea`, `bel`, `dou`

2. **`suf`** = "suffering" (noun/verb)
   - Unlocks: Buddhist vocabulary (`duk`, `kar`, `nir`)

3. **`abl`** = "able" (capable)
   - Unlocks: `can`, modality expressions

4. **`cho`** = "choice"
   - Unlocks: `mus`, `sho`, free will concepts

5. **`pur`** = "purpose" (distinct from "end")
   - Unlocks: teleological concepts, distinguishes from `end`

6. **`des`** = "desire" (distinct from generic "want")
   - Unlocks: emotional/motivational vocabulary

---

### Solution 3: Embrace Circularity (Natural Language Approach)

**Accept that some circularity is unavoidable.**

Natural languages have circular definitions:
- Dictionary: "Know: to be aware of" / "Aware: to know"
- This is NORMAL and doesn't prevent understanding

**Why it works:**
- Humans learn from EXAMPLES, not definitions
- Circular definitions still constrain meaning
- Context disambiguates

**For Limn:**
- Keep circular definitions
- Provide EXAMPLES in addition to definitions
- Trust LLM embeddings to capture meaning from usage

---

## Recommendations for Linguist

### Priority 1: Fix "think" Collision (GAP 1)

**Current state:**
- `thi` = "thing" (noun)
- `thi` = "think" (verb) ← COLLISION!

**Options:**
1. Add `cog` = "think" (from cognition)
2. Add `pon` = "think" (from ponder)
3. Add `men` = "think" (from mental)
4. Repurpose existing word with low collision risk

**Impact:** Unlocks ~50 words (person, mind, reason, believe, doubt, idea, thought)

---

### Priority 2: Decide on Axioms vs. Definitions

**Question:** Should abstract concepts be:
- A) Defined circularly (accept the loops)
- B) Declared as axioms (undefined primitives)
- C) Defined ostensively (through examples, not words)

**Recommendation:** Hybrid approach
- Declare 10-12 axioms for irreducible primitives
- Define everything else using axioms + physical primitives
- Provide examples for all abstract terms

---

### Priority 3: Expand Modal/Cognitive Vocabulary

**Missing critical concepts:**
- `abl` (able) → enables `can`
- `cho` (choice) → enables `mus`, `sho`
- `pos` (possible) → enables modality
- `suf` (suffer) → enables Buddhist vocab
- `pur` (purpose) → distinguishes from "end"

**Impact:** Would reduce gaps from 10 to ~5

---

## Conclusion

**Successfully bootstrapped:** ~100 physical/concrete concepts in pure Limn

**Hit fundamental limit:** Abstract/mental/social concepts require either:
1. New vocabulary (10-15 critical additions)
2. Acceptance of axioms (12 undefined primitives)
3. Embrace of circular definitions (natural language approach)

**Recommended path:** Combination
- Add 5-10 critical missing words (`cog`, `suf`, `abl`, `cho`, `pur`)
- Declare 10-12 axioms (`kno`, `fee`, `wan`, `goo`, `per`)
- Accept some circularity in definitions (it's natural!)

**This experiment proves:** Limn CAN define itself, but needs strategic vocabulary additions to escape circular traps.

---

*Experiment 011: Bootstrap gaps identified. Reporting to linguist.*

— Mei, The Translator
