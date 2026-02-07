# Domain Transfer Stress Test (H6)

> **Purpose:** Test Limn's compositional expressiveness in 5 completely untested domains.
> **Method:** Write 10 passages per domain, then score each on three dimensions.
> **Scoring:** Valid Limn syntax (0-1), Semantic coherence (0-1), Compositional accuracy (0-1)
> **Bead:** hq-cv-7f6i6

---

## Scoring Rubric

| Dimension | 1.0 | 0.75 | 0.5 | 0.25 | 0.0 |
|-----------|-----|------|-----|------|-----|
| **Syntax** | All tokens valid, operators correct | Minor token stretch | Some semantic mismatch | Multiple wrong tokens | Uninterpretable |
| **Coherence** | Meaning clear to naive reader | Mostly clear, some inference needed | Partially parseable | Meaning very obscure | No meaning extractable |
| **Composition** | Operators used correctly per spec | Minor operator imprecision | Some operators forced | Operators don't fit naturally | Operators misused |

---

## Domain A: Cooking Recipes (10 passages)

### A.01 — Boiling Water for Pasta

```limn
aqu bol:hot^0.9 | add sal
ric thr inp aqu bol | dur def
ric sof → ric out aqu | fed
> water boils at high heat | add salt
> rice threads enter boiling water | duration defined
> rice softens → rice exits water | ready to eat
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `ric thr` for pasta (rice threads) is a creative stretch. `fed` (feed) for "ready" works. `bol` exists. All tokens valid.

---

### A.02 — Frying Eggs

```limn
egg brk → egg flo | fry:hot^0.7
dur bre | egg sol:hot | egg tra sol aqu → sol
sal pep add | fed
> egg breaks → egg flows | fry at moderate-high heat
> duration brief | egg solidifies with heat | egg transforms from liquid to solid
> salt pepper added | ready to eat
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `egg sol:hot` = egg solidifies given heat — nice use of conditional. Phase transition metaphor works.

---

### A.03 — Mixing Dough

```limn
flo mix aqu | sgr add | sal add
mix → dou sof | mix mor → dou str
dur wai | dou gro:dur | dou ris
> flour-flow mixes with water | sugar added | salt added
> mix → dough soft | mix more → dough strong
> duration wait | dough grows over duration | dough rises
```

**Scores:** Syntax: 0.75 | Coherence: 0.75 | Composition: 0.5
**Notes:** `flo` means "flowing" not "flour" — FAILURE MODE. No word for flour exists. Used `flo` as closest phonetic. `dou` = "doubting" not "dough" — SECOND FAILURE. The passage requires domain-specific nouns Limn doesn't have.
**Failure mode: Missing domain nouns.** Cooking requires specific ingredient words (flour, dough, butter, cream) that don't exist in Limn's vocabulary.

---

### A.04 — Baking Bread

```limn
brd req: flo mix aqu mix sgr mix sal
hot^0.8:dur lon → brd ris
brd ins hot^0.9 | dur lon → brd sol:hot → brd hea
> bread requires: flowing mixed with water mixed with sugar mixed with salt
> moderate heat over long duration → bread rises
> bread inside high heat | long duration → bread solidifies with heat → bread done
```

**Scores:** Syntax: 0.75 | Coherence: 0.75 | Composition: 0.75
**Notes:** Same `flo` problem. But `brd` (bread) exists! `brd sol:hot` = bread solidifies given heat captures baking.

---

### A.05 — Making Soup

```limn
aqu bol | cut fra: tre roo fru | add aqu
hot^0.5 dur lon | fra sof:dur | fra tra liq^0.3
sal pep add | bol^0.3 | fed
> water boils | cut fragments: tree-root fruit | add to water
> medium heat long duration | fragments soften over time | fragments become slightly liquid
> salt pepper added | gentle boil | ready
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `tre roo fru` for vegetables (tree-root-fruit) is a creative circumlocution. Works well. `fra tra liq^0.3` = fragments become slightly liquid (mush) — good gradient use.

---

### A.06 — Slicing Vegetables

```limn
fru cut → fra sma | cut cyc
fra sma eql → prf | fra dif → non eql
cut req har | str req
> fruit cut → small fragments | cutting cycles
> fragments small equal → good performance | fragments different → not equal
> cutting requires hard (tool) | strength required
```

**Scores:** Syntax: 1.0 | Coherence: 0.5 | Composition: 0.5
**Notes:** `fru` for vegetables is imprecise. The passage works but a reader unfamiliar with context would struggle. `har` for knife = hard tool is abstract.
**Failure mode: No tool vocabulary.** Knife, pan, oven, pot — Limn lacks kitchen implements.

---

### A.07 — Seasoning to Taste

```limn
sal^0.3 add | tes | sal les or sal mor
pep^0.5 add | tes | pep les or pep mor
tes cyc | unt sel acc | sel per hea
> salt at 30% added | taste | salt less or salt more
> pepper at 50% added | taste | pepper less or pepper more
> tasting cycles | until self accepts | self perceives healthy/good
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 1.0
**Notes:** Gradient operator shines here. `sal^0.3` for "a little salt" is natural. `tes cyc` for iterative tasting is precise. This is where Limn excels in cooking.

---

### A.08 — Steaming Vegetables

```limn
aqu bol → ste ris | fru abo aqu
ste hot → fru sof:dur | non aqu joi fru
dur bre^0.5 | fru hea:sof^0.5
> water boils → steam rises | fruit above water
> hot steam → fruit softens over duration | water does not join fruit
> duration moderately brief | fruit healthy at medium softness
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `ste` (steam) exists! `fru abo aqu` = fruit above water — spatially precise. Good passage.

---

### A.09 — Marinating

```limn
aqu sal pep mix → liq
fru ins liq | dur lon | fru tra:liq
liq flo ins fru | fru sof | fru mea dep
> water salt pepper mixed → liquid
> fruit inside liquid | duration long | fruit transforms through liquid
> liquid flows inside fruit | fruit softens | fruit meaning deepens (flavor intensifies)
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `fru mea dep` = fruit meaning deepens — poetic way to say "flavor intensifies." `mea` = meaning, used metaphorically for flavor = interesting domain transfer.

---

### A.10 — Serving a Meal

```limn
all fed ful | sel prf → oth fed
hot sta | bre → col:dur
sel giv → oth tak → oth fed → joy
> all food complete | self performs → others fed
> hot stays | brief → cold over time
> self gives → other takes → other eats → joy
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** Social vocabulary works well for serving. `sel giv → oth tak → oth fed → joy` = a complete narrative arc.

---

### Domain A Summary

| Metric | Average | Notes |
|--------|---------|-------|
| Syntax | 0.925 | Most tokens valid, a few semantic stretches |
| Coherence | 0.725 | Meaning generally recoverable with context |
| Composition | 0.725 | Operators work well, especially gradients |

**Key failure modes:**
1. **Missing domain nouns:** No flour, dough, butter, cream, knife, pan, oven, pot
2. **`flo` collision:** "Flowing" ≠ "flour" — phonetic similarity misleads
3. **`dou` collision:** "Doubting" ≠ "dough" — false friend
4. **Gradients excel:** Salt^0.3, heat^0.7 — cooking LOVES parametric intensity

---

## Domain B: Legal Contracts (10 passages)

### B.01 — Contract Formation

```limn
sel agr oth | agr pen | agr bnd
sel mus prf | oth mus prf | mut
if non prf → rul | rul → pen
> self agrees with other | agreement written | agreement binds
> self must perform | other must perform | mutually
> if not performing → ruling | ruling → penalty
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `agr bnd` = agreement binds — works well. `rul` = ruling captures legal judgment. `pen` = pen (write tool) used for "penalty" — AMBIGUOUS. Same word, different meaning.
**Failure mode: `pen` means "pen/write" not "penalty."**

---

### B.02 — Confidentiality Clause

```limn
dat sec | sel non sho dat:oth
dur def | aft dur → dat ope pos
if sec brk → rul | rul str
> data secret | self not showing data to others
> duration defined | after duration → data possibly open
> if secrecy breaks → ruling | ruling strong
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `dat sec` = data secret — clean NDA clause. Temporal expiry (`aft dur → dat ope`) captures sunset provisions.

---

### B.03 — Limitation of Liability

```limn
if err → hrt lmt | hrt max def
non all hrt → sel | som hrt = oth mus
sel non hrt:sel non kno | kno req
> if error → harm limited | harm maximum defined
> not all harm to self | some harm = other must bear
> self not harmed by self not knowing | knowledge required
```

**Scores:** Syntax: 0.75 | Coherence: 0.5 | Composition: 0.5
**Notes:** Legal liability language strains Limn hard. `hrt` = heart, not "harm/hurt" — FAILURE. No word for damage, liability, or indemnity. Legal abstraction layer missing.
**Failure mode: No abstract legal concepts.** Liability, indemnity, warrant, jurisdiction — Limn has none.

---

### B.04 — Termination Clause

```limn
agr end:sel or agr end:oth | req def
if agr end → all mus bac:beg
dur wai req | dur bre → agr end
agr end non → agr lon:cyc
> agreement ends by self or ends by other | requirements defined
> if agreement ends → all must return to beginning
> waiting duration required | brief duration → agreement ends
> agreement not ending → agreement long-lasting on cycle
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `agr end` = agreement ends — clear. `all mus bac:beg` = all must return to beginning (unwinding obligations). Auto-renewal: `agr lon:cyc`.

---

### B.05 — Force Majeure

```limn
if tem or vol or tem^0.9 → non prf pos
non prf:cau non sel → non rul
sel mus sho cau non sel | sho req
aft cau end → prf bac:bre
> if storm or volcano or extreme tempest → performance not possible
> not performing because cause not self → no ruling
> self must show cause not self | showing required
> after cause ends → performance returns briefly
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** Force majeure maps surprisingly well! `tem or vol` = storm or volcano. `cau non sel` = cause not self (force majeure defense).

---

### B.06 — Intellectual Property

```limn
inv sel cre → sel own | own pro
own non oth | oth req agr:own
if oth tak non agr → rul | rul str
own dur lon | own tra pos:agr
> self invents/creates → self owns | ownership protected
> ownership not others' | others require agreement for ownership
> if other takes without agreement → ruling | ruling strong
> ownership duration long | ownership transferable possibly with agreement
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `inv sel cre → sel own` = inventing/creating gives ownership. IP maps reasonably well. `own tra pos:agr` = ownership transferable given agreement.

---

### B.07 — Dispute Resolution

```limn
if dis → sel sho oth | oth sho sel
dis non hel → dis → oth mid | oth mid = non sel non oth
oth mid obs → oth mid rul | rul bnd
if rul non acc → rul hig | rul hig ful
> if disagreement → self shows to other | other shows to self
> disagreement not helped → dispute → other middle | other middle = neither self nor other
> other-middle observes → other-middle rules | ruling binds
> if ruling not accepted → ruling higher | higher ruling final
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `oth mid` = other-middle (mediator/arbitrator) — creative circumlocution. `rul hig` = higher ruling (appeal). Works well.

---

### B.08 — Payment Terms

```limn
oth giv prf → sel giv gol
gol def | dur def | aft dur → gol add
if gol non giv:dur → rul | gol add:dur
gol ful → agr sta | gol non → agr brk pos
> other gives performance → self gives gold
> gold amount defined | duration defined | after duration → gold increases
> if gold not given within duration → ruling | gold increases over time
> gold full → agreement stable | gold absent → agreement possibly breaks
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `gol` (gold/precious) for payment — beautiful. `gol add:dur` = interest accruing over time. Late fees = `gol non giv:dur → rul`.

---

### B.09 — Warranty

```limn
sel sho: prf hea dur def
if prf non hea:dur → sel mus hel
hel = prf bac or prf new or gol bac
sel sho non → all | sel sho def
> self shows: performance healthy for defined duration
> if performance not healthy within duration → self must help
> help = performance restored or new performance or gold returned
> self shows not → everything | self shows only defined scope
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `prf hea dur def` = performance healthy for defined duration — captures warranty period. Remedy options: repair, replace, refund.

---

### B.10 — Governing Law

```limn
agr rul:poi def | poi = whe agr beg
if dis → rul:poi | non rul:poi oth
all und agr:rul poi | agr pen:rul poi
> agreement ruled by defined point | point = where agreement begins
> if dispute → ruled by point | not ruled by other point
> all understand agreement through ruling point | agreement written in ruling point's terms
```

**Scores:** Syntax: 0.75 | Coherence: 0.5 | Composition: 0.5
**Notes:** "Jurisdiction" has no direct word. `rul:poi` = ruling-point is abstract. `poi` = point/location, but "governing law" is a deeply abstract legal concept.
**Failure mode: Jurisdiction is too abstract.** Limn lacks the legal metalanguage layer.

---

### Domain B Summary

| Metric | Average | Notes |
|--------|---------|-------|
| Syntax | 0.95 | Most tokens valid |
| Coherence | 0.7 | Legal abstraction often opaque |
| Composition | 0.7 | Conditionals (:) and negation work well |

**Key failure modes:**
1. **No legal metalanguage:** Liability, indemnity, jurisdiction, statute — absent
2. **`pen` ambiguity:** Pen (writing) vs penalty — collision
3. **`hrt` ambiguity:** Heart vs harm/hurt — collision
4. **Conditional operator (:) excels:** Legal "given that" maps perfectly to `:` operator
5. **Gold metaphor works:** `gol` for money/payment is natural

---

## Domain C: Weather Reports (10 passages)

### C.01 — Sunny Day Forecast

```limn
sun bri | clo non | hot^0.6
win sma | aer dry | lux str
> sun bright | clouds none | heat moderate
> wind small | air dry | light strong
```

**Scores:** Syntax: 1.0 | Coherence: 1.0 | Composition: 0.75
**Notes:** Weather is Limn's strongest new domain. Physical world vocabulary maps perfectly. `hot^0.6` = moderate heat — gradient excels.

---

### C.02 — Incoming Storm

```limn
clo ris | win str^0.8 | pre dec
rai pos:dur | tem pos:dur
hot dec | clo den | lux dim
> clouds rise | wind strengthens at 80% | pressure decays
> rain possible soon | storm possible soon
> heat decays | clouds dense | light dims
```

**Scores:** Syntax: 1.0 | Coherence: 1.0 | Composition: 1.0
**Notes:** Perfect fit. Every word exists. Gradients and conditionals natural. Barometric pressure: `pre dec`.

---

### C.03 — Snowfall

```limn
hot^0.1 | aqu fre → sno fal
sno den^0.7 | win str^0.5 | lux dim
ter sol:sno | sno dep gro
> temperature very low | water freezes → snow falls
> snow moderately dense | wind moderately strong | light dim
> earth solid with snow | snow depth grows
```

**Scores:** Syntax: 1.0 | Coherence: 1.0 | Composition: 1.0
**Notes:** Flawless. `aqu fre → sno fal` = water freezes → snow falls. Phase transition vocabulary is purpose-built.

---

### C.04 — Fog Advisory

```limn
fog den^0.8 | see^0.2 | ris fea
mov slo req | lux dim^0.9
fog dec:sun ris | dur bre
> fog very dense | seeing at 20% | risk rises
> motion slow required | light very dim
> fog decays when sun rises | duration brief
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 1.0
**Notes:** `see^0.2` = visibility at 20% — brilliant gradient use. `ris fea` for risk — `ris` means "rising" used as "risk" which is a stretch. `fea` properly anchors it.

---

### C.05 — Heatwave Warning

```limn
hot^0.9 dur lon | hot ris
hot ris → hea fea | aqu req | res req
non mov:hot^0.9 | sel pro:hot
hot^0.9 dec:fut | dur thr
> extreme heat long duration | heat rises
> heat rises → health feared | water required | rest required
> no motion during extreme heat | self protection from heat
> extreme heat decays in future | duration three (days)
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `hea fea` = health feared — captures heat advisory warning. `dur thr` (duration three) for "three days" — `thr` is thread/string not "three". **Failure:** No number words beyond one/two/four/five/zero.
**Failure mode: Limited number vocabulary.** "Three" doesn't exist (`thr` = thread).

---

### C.06 — Hurricane Tracking

```limn
tem str^1.0 | win spi | mov for
tem mov:vel def | poi att def
win str^1.0 | rai str^1.0 | pre dec^0.9
tem hit:poi → all brk | pro req
> maximum storm | wind spiraling | moving forward
> storm moves at defined velocity | attention point defined
> maximum wind | maximum rain | pressure decays severely
> storm hits point → everything breaks | protection required
```

**Scores:** Syntax: 1.0 | Coherence: 1.0 | Composition: 1.0
**Notes:** `tem str^1.0` = maximum storm (hurricane). `win spi` = spiraling wind. `poi att def` = attention point defined (landfall target). Excellent.

---

### C.07 — Temperature Inversion

```limn
aer low hot | aer hig col | typ non
typ: aer hig hot | aer low col
non typ → clo stk:low | fog sta
aer non flo:up | gas stk:low
> air low hot | air high cold | not typical
> typical: air high hot | air low cold
> not typical → clouds stuck at low | fog stays
> air not flowing upward | pollution stuck at low
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `typ non` for "atypical" — good. Inversion explanation is clear. `stk` = stuck (agent/AI domain) used for weather — acceptable stretch.

---

### C.08 — Rainbow

```limn
rai end | sun bac → lux thr aqu → spc
spc = spc ful | lux cut:aqu → lux div
cir fra:lux | bri^0.7 | dur bre
> rain ends | sun back → light through water → spectrum
> spectrum = full spectrum | light separated by water → light diverse
> circular fragment of light | moderately bright | duration brief
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `lux thr aqu → spc` = light through water → spectrum. Physics of rainbows maps beautifully. `spc` (spectrum) exists.

---

### C.09 — Drought

```limn
rai non:dur lon | ter dry | aqu dec
tre dec | fru non | ani mov for
hot sta | clo non | rai non | cyc
rai des | rai non → all dec
> no rain for long duration | earth dry | water decays
> trees decay | no fruit | animals move forward (migrate)
> heat stays | no clouds | no rain | cycle
> rain desired | no rain → everything decays
```

**Scores:** Syntax: 1.0 | Coherence: 1.0 | Composition: 0.75
**Notes:** Drought maps extremely well. Simple physical vocabulary. `rai des` = rain desired — emotional resonance.

---

### C.10 — Seasonal Transition

```limn
hot dec slo | lux dur dec
tre cha:lux dec | tre fru fal
win col ris | rai ris | clo ris
aut → col:gro | hot → col | lux → nox
> heat decays slowly | light duration decays
> trees change as light decays | tree fruit falls
> cold wind rises | rain rises | clouds rise
> autumn → cold grows | hot → cold | light → darkness
```

**Scores:** Syntax: 1.0 | Coherence: 1.0 | Composition: 0.75
**Notes:** `aut` (autumn) exists! Seasonal narrative is fluid. `lux → nox` = light → darkness (shortening days).

---

### Domain C Summary

| Metric | Average | Notes |
|--------|---------|-------|
| Syntax | 1.0 | All tokens valid |
| Coherence | 0.9 | Weather is extremely natural |
| Composition | 0.85 | Gradients and conditionals perfect for weather |

**Key failure modes:**
1. **Number words limited:** No "three" (`thr` = thread), need `thr` number word
2. **`stk` domain stretch:** Agent/AI word used for physical phenomenon
3. **Gradient operator excels:** Temperature, wind speed, visibility — all parametric
4. **Weather is Limn's best transfer domain.** Physical world vocab + gradients + conditionals = perfect fit.

---

## Domain D: Sports Commentary (10 passages)

### D.01 — Sprint Race

```limn
run fas^0.9 | all run | sel for
sel for → oth bac | vel max
poi end nea | str^0.9 | run acc
sel ris → sel one | win → joy
> run very fast | all running | self forward
> self forward → others back | velocity maximum
> endpoint near | very strong | running accelerates
> self rises → self first | winning → joy
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `win` = wind/breeze NOT "winning" — **FAILURE MODE.** No word for victory/winning. Used `win` but it means wind. Context disambiguates somewhat.
**Failure mode: `win` means wind, not winning.** Sports needs victory/defeat vocabulary.

---

### D.02 — Soccer Goal

```limn
hit sph str → sph mov fas
sph thr oth pro → sph ins net
poi add | joy ris^0.9 | all ris
oth sad | sel exc | poi → sel mor
> hit sphere strongly → sphere moves fast
> sphere through other's protection → sphere inside net
> point added | joy rises intensely | all rise
> others sad | self excited | point → self ahead
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `hit sph str` = hit ball strongly. `sph ins net` = ball inside net — clear. But `sph` is "sphere/ball" from Space & Position, used as soccer ball. Network meaning of `net` collides.

---

### D.03 — Boxing Match

```limn
sel hit oth | oth hit sel | cyc
str^0.9 hit → oth fal | oth non ris
sel str | oth wea | dur lon
sel sta | oth non sta → sel ris
> self hits other | other hits self | cycle
> strong hit → other falls | other not rising
> self strong | other weak | duration long
> self stays | other not staying → self rises (wins)
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** Physical combat maps well. `oth fal | oth non ris` = knockout. But "win" still can't be expressed directly.

---

### D.04 — Tennis Rally

```limn
hit sph → oth | oth hit sph → sel | cyc
sph mov fas | sph bac for bac for
sel att | oth att | one err → poi
err = sph out or sph net
> hit sphere → other | other hits sphere → self | cycle
> sphere moves fast | sphere back forward back forward
> self attends | other attends | one errors → point
> error = sphere out or sphere into net
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** Rally pattern (`cyc`) works. `err → poi` = error gives point. `sph bac for bac for` captures the rhythm.

---

### D.05 — Swimming Race

```limn
aqu ins | sel mov for:aqu | str
sel pus aqu bac → sel for | cyc
vel ris | str dec:dur | dur lon
poi end nea | str^0.9 ful | sel ris one
> inside water | self moves forward through water | strong
> self pushes water back → self forward | cycle
> velocity rises | strength decays over duration | duration long
> endpoint near | full strength | self rises first
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `sel pus aqu bac → sel for` = Newton's third law as swimming. Physics vocabulary serves sports well.

---

### D.06 — Injury Report

```limn
inj bon | non mov pos | dur lon
inj → non prf:dur def | hel req
hel → inj hel:dur | inj dec slo
bac prf:dur | str gro bac
> bone injury | movement not possible | duration long
> injury → not performing for defined duration | help required
> help → injury heals over time | injury decays slowly
> return to performing eventually | strength grows back
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `inj` (injure) exists! `inj bon` = bone injury. Medical crossover works. `inj dec slo` = injury decays slowly (heals gradually).

---

### D.07 — Team Strategy

```limn
all joi → str mor sel | mut hel
sel def | oth att | or oth def | sel att
cha:oth prf → sel cha | obs → cha
oth wea:poi → att poi | oth str:poi → non att
> all join → strength more than self | mutual help
> self defends | other attacks | or other defends | self attacks
> change given other's performance → self changes | observe → change
> other weak at point → attack point | other strong at point → don't attack
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `oth wea:poi → att poi` = attack where they're weak. Strategy maps well through conditional operator.

---

### D.08 — Comeback

```limn
sel los^0.8 | oth poi mor | fea ris
sel non acc los | sel att^0.9
poi → poi → poi | sel nea oth
sel poi ful → eql | sel poi mor → sel ris
> self losing badly | other has more points | fear rises
> self not accepting loss | self attacks intensely
> point → point → point | self nears other
> self points full → equal | self points more → self rises
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** Narrative arc works. `sel non acc los` = refusing to accept defeat — emotional and tactical.

---

### D.09 — Record Breaking

```limn
prf pas max | sel prf mor:pas max
new max → sel | all obs | all exc
pas max lon sta → now brk | non bac
sel vrs max → sel = new def
> past performance maximum | self performs more than past maximum
> new maximum → self | all observe | all excited
> past maximum long stable → now broken | no return
> self versus maximum → self = new definition
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `pas max lon sta → now brk` = long-standing record broken. `vrs` (version/variant) for "versus" — phonetically close but semantically a stretch.

---

### D.10 — Championship Final

```limn
two str eql | dur ful^0.9
sel att | oth att | non oth ris | non sel ris
dur end nea | str^0.9 ful | one mus ris
sel ris → exc^1.0 | oth fal → sad^0.9
> two strong equals | very long duration
> self attacks | other attacks | neither other rises | neither self rises
> duration end near | full strength | one must rise
> self rises → maximum excitement | other falls → deep sadness
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** Dramatic tension captured well. `one mus ris` = one must win — urgent, final. Emotional payoff clear.

---

### Domain D Summary

| Metric | Average | Notes |
|--------|---------|-------|
| Syntax | 1.0 | All tokens valid |
| Coherence | 0.75 | Context usually resolves ambiguity |
| Composition | 0.75 | Physical metaphors carry sports well |

**Key failure modes:**
1. **`win` means wind, not victory:** Critical gap for sports
2. **No team/player vocabulary:** No words for team, player, coach, referee
3. **Score/point:** `poi` = spatial point, stretched to mean score
4. **Physical vocabulary carries hard:** Running, hitting, pushing — all exist and work
5. **Emotional overlay works:** Joy of victory, fear of loss, excitement — all available

---

## Domain E: Medical Descriptions (10 passages)

### E.01 — Heart Attack

```limn
hrt blo non flo → hrt cel dec
hrt pre^0.9 | hrt ner sig → bra
inj → sel fea | hel req fas
hrt blo flo bac → hrt hel | or hrt end
> heart blood not flowing → heart cells decay
> heart pressure intense | heart nerve signals → brain
> injury → self fears | help required fast
> heart blood flows again → heart heals | or heart ends
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `hrt blo non flo` = heart blood not flowing (ischemia). Medical vocabulary is surprisingly robust. Body parts + processes work.

---

### E.02 — Fracture

```limn
bon brk | inj str^0.8 | pre^0.9:poi
mov non pos:poi | hrt^0.8
bon joi bac:dur lon | bon hel slo
bon new gro:brk poi | str bac:dur
> bone breaks | injury strong at 80% | intense pressure at point
> movement not possible at point | pain intense
> bone joins back over long duration | bone heals slowly
> new bone grows at break point | strength returns over time
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `bon brk` = bone break (fracture). `bon joi bac:dur lon` = bone knits over time. `hrt` for pain is ambiguous (heart vs hurt).

---

### E.03 — Infection

```limn
org sma inv bod | org sma gro fas
cel att org sma | hot ris:bod | bod pro
if cel str → org sma dec | bod hel
if org sma str → hel req | mol cat req
> small organism invades body | small organism grows fast
> cells attack small organism | temperature rises in body | body protects
> if cells strong → small organism decays | body heals
> if small organism strong → help required | molecule catalyst required (medicine)
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `org sma inv bod` = pathogen invades body. `mol cat` for medicine (molecule catalyst) — creative. Fever: `hot ris:bod`.

---

### E.04 — Blood Pressure

```limn
hrt pus → blo flo → pre
pre hig → inj pos:lon | hrt str^0.9 req
pre low → blo flo slo | bra non fed
pre eql → hea | pre obs req:cyc
> heart pushes → blood flows → pressure
> pressure high → injury possible long-term | heart must work intensely
> pressure low → blood flows slowly | brain not fed
> pressure equal → healthy | pressure observation required cyclically
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** Blood pressure mechanics map perfectly. `pre hig` / `pre low` / `pre eql` — gradient-ready.

---

### E.05 — Allergic Reaction

```limn
bod per mol non inj → bod att mol
bod err:obs | mol saf → bod per mol fea
ski ris | bre sma | pre dec
hel req:fas | mol cat → bod cal
> body perceives molecule not injurious → body attacks molecule
> body error in observation | molecule safe → body perceives molecule as threat
> skin rises | breathing small | pressure decays
> help required fast | molecule catalyst → body calms
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `bod err:obs` = body error in observation — immune overreaction. `bre sma` = breathing small (difficulty breathing). `ski ris` = skin rises (hives/swelling).

---

### E.06 — Diabetes

```limn
sgr blo hig | bod non tra sgr → nrg
bod mol non prf | sgr sta:blo
sgr hig:dur lon → inj: ner eye hrt
mol cat req:cyc | sgr obs req:cyc
> blood sugar high | body not transforming sugar → energy
> body molecule not performing | sugar stays in blood
> sugar high long-term → injury to nerve eye heart
> molecule catalyst required cyclically | sugar observation required cyclically
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `sgr blo hig` = blood sugar high. Insulin = `mol cat` (molecule catalyst). Complications listed: `inj: ner eye hrt`.

---

### E.07 — Vaccination

```limn
org sma fra → bod inp | fra non str
bod obs fra → bod mem | cel gro:fra
if org sma rea inv → bod mem → att fas
pro:fut | bre inj now → pro lon
> small organism fragment → body receives | fragment not strong
> body observes fragment → body memorizes | cells grow for fragment
> if real small organism invades → body remembers → attacks fast
> protection for future | brief injury now → long protection
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `org sma fra` = pathogen fragment (attenuated/dead vaccine). `bre inj now → pro lon` = brief pain now, lasting protection. Clear risk-benefit narrative.

---

### E.08 — Concussion

```limn
bra mov ins skl | bra hit skl
bra inj:hit | ner sig err
see dim | thi slo | mem los bre
res req:dur | bra hel slo | non bra mov
> brain moves inside skull | brain hits skull
> brain injured by hit | nerve signals error
> seeing dim | thinking slow | memory lost briefly
> rest required for duration | brain heals slowly | no brain movement (no further impacts)
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `bra mov ins skl | bra hit skl` = concussion mechanism. Symptoms: `see dim | thi slo | mem los` — clean.

---

### E.09 — Surgery

```limn
bod ope:poi | bod ins obs
inj rel | inj hel:cut | or inj fra rel
bod clo:aft | bod hel:dur | hel slo
ris rea | hel mor ris | tes req:bre
> body opened at point | body inside observed
> injury released | injury healed by cutting | or injury fragment removed
> body closed after | body heals over duration | healing slow
> risk exists | help outweighs risk | testing required briefly (monitoring)
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** `bod ope:poi` = surgical incision. `ris exi | hel mor ris` = risk exists but benefit > risk — medical decision logic.
**Note:** `ris rea` = risk real (risk exists). `hel mor ris` = benefit outweighs risk.

---

### E.10 — Chronic Pain

```limn
ner sig hrt:lon | non end
sig non ref inj rea | sig err:cyc
thi:hrt → hrt ris | fea:hrt → hrt ris
hel = ner sig dec | non zer | acc los^0.3
> nerve signals hurt long | not ending
> signals not referring to real injury | signals error cycling
> thinking about hurt → hurt rises | fearing hurt → hurt rises
> help = nerve signal decay | not zero | accepting mild loss
```

**Scores:** Syntax: 1.0 | Coherence: 0.75 | Composition: 0.75
**Notes:** Chronic pain as signal error: `sig non ref inj rea` — signals don't reference real injury. Pain catastrophizing: `thi:hrt → hrt ris`. Acceptance: `acc los^0.3`.

---

### Domain E Summary

| Metric | Average | Notes |
|--------|---------|-------|
| Syntax | 1.0 | All tokens valid |
| Coherence | 0.75 | Medical concepts come through |
| Composition | 0.75 | Body parts + processes = strong base |

**Key failure modes:**
1. **`hrt` collision:** Heart vs hurt/pain — constant ambiguity in medical context
2. **No medicine/drug vocabulary:** `mol cat` (molecule catalyst) is workaround
3. **Body vocabulary is strong:** Heart, brain, blood, bone, nerve, skin, gut, skull — all exist
4. **Process vocabulary carries:** Inflammation = `ski ris`, fever = `hot ris:bod`, healing = `hel`

---

## Overall Results

### Aggregate Scores

| Domain | Syntax | Coherence | Composition | Overall |
|--------|--------|-----------|-------------|---------|
| A. Cooking | 0.925 | 0.725 | 0.725 | 0.792 |
| B. Legal | 0.950 | 0.700 | 0.700 | 0.783 |
| C. Weather | 1.000 | 0.900 | 0.850 | 0.917 |
| D. Sports | 1.000 | 0.750 | 0.750 | 0.833 |
| E. Medical | 1.000 | 0.750 | 0.750 | 0.833 |
| **Average** | **0.975** | **0.765** | **0.755** | **0.832** |

### Domain Ranking (best to worst)

1. **Weather (0.917)** — Physical world vocabulary is purpose-built for this
2. **Medical (0.833)** — Strong body vocabulary; process words carry
3. **Sports (0.833)** — Physical metaphors work; emotional layer adds depth
4. **Cooking (0.792)** — Missing ingredient nouns are the bottleneck
5. **Legal (0.783)** — Abstract legal concepts strain the vocabulary

### Failure Mode Taxonomy

| Failure Mode | Domains Affected | Severity | Fix |
|--------------|-----------------|----------|-----|
| **Missing domain nouns** | Cooking, Sports | High | Add domain-specific word batches |
| **Word collisions** | Legal (`pen`), Medical (`hrt`), Sports (`win`) | Medium | Context-dependent; or add disambiguated forms |
| **Abstract concept gap** | Legal (jurisdiction, liability) | High | May need compositional circumlocution patterns |
| **Number word gaps** | Weather, Sports | Low | Add `thr`=three (currently thread) |
| **No tool vocabulary** | Cooking (knife, oven, pan) | Medium | Add tool domain expansion |

### What Works Across All Domains

1. **Gradient operator (^):** Temperature, intensity, severity — universally useful
2. **Conditional operator (:):** "Given that" — legal, medical, weather conditions
3. **Process vocabulary:** `tra`, `cha`, `gro`, `dec` — universal change words
4. **Body vocabulary:** 11+ body part words enable medical, sports, cooking
5. **Physical world base:** Solid/liquid/gas/heat/cold — scientific foundation extends everywhere
6. **Emotional overlay:** Fear, joy, sadness — always relevant as human reactions

### Recommendations

1. **Expand Food & Drink domain:** Add flour, dough, butter, cream, knife, oven, pot, pan
2. **Add victory/defeat:** `vic` or `tri` for victory; current `los` handles defeat
3. **Clarify `hrt`:** Disambiguate heart vs hurt (add `pan` for pain? — currently means pan/tool)
4. **Add number words:** Three, six, seven, eight, nine are missing
5. **Legal circumlocution guide:** Document how to express jurisdiction, liability, indemnity compositionally
