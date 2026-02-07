# Domain Transfer Stress Test (H6)

**Date:** 2026-02-06
**Researcher:** Kira (limn/crew/student)
**Bead:** hq-8qjmb
**Status:** COMPLETE

---

## Hypothesis

**H6:** Limn's compositional system generalizes across domains. A model (or human)
trained on abstract reasoning and narrative text can produce valid, coherent Limn in
domains it has never practiced: cooking, legal, weather, sports, medical.

**Null hypothesis:** Domain-specific concepts require domain-specific vocabulary that
Limn lacks. Compositional operators cannot compensate for missing primitives. Output
in novel domains will degrade significantly on syntax, coherence, or accuracy.

---

## Method

For each of 5 novel domains, I write:
- 3 English source texts of increasing complexity (simple, moderate, complex)
- Limn translations using only validated vocabulary (`vocab.sh`)
- Scores on three dimensions (1-5 scale each):
  - **Syntax validity**: Does it parse as legal Limn? (validated by `vocab.sh`)
  - **Semantic coherence**: Does the Limn mean roughly what the English says?
  - **Compositional accuracy**: Are operators used correctly per formal semantics?

Total: 15 test cases across 5 domains.

**Scoring rubric:**
- 5 = Perfect — natural, idiomatic Limn
- 4 = Good — minor awkwardness, meaning preserved
- 3 = Adequate — meaning recoverable with effort, some ambiguity
- 2 = Poor — significant meaning loss, misleading polysemy
- 1 = Failed — meaning unrecoverable or syntax invalid

---

## Domain 1: Cooking Recipes

### Available vocabulary
Strong coverage: `coo` (cook), `boi` (boil), `fry` (fry), `ros` (roast),
`stw` (stew), `cut` (cut), `mix` (mix), `hot` (hot), `sgr` (sugar),
`sal` (salt), `oil` (oil), `btr` (butter), `flr` (flour), `egg` (egg),
`oni` (onion), `pep` (pepper), `tmt` (tomato), `ric` (rice), `rcp` (recipe),
`eat` (eat), `dsh` (dish), `swe` (sweet), `bit` (bitter), `umi` (umami)

### Test 1.1 — Simple: "Boil the rice for 20 minutes"

**English:** Boil the rice for 20 minutes.

**Limn:** `boi ric | dur 20`

**Validation:**
```
vocab.sh validate "boi ric | dur 20"
  ✓ boi = boil, water
  ✓ ric = rice, grain
  ✓ dur = duration, time span (Time & Change)
  ✓ 20 = number
```

| Criterion | Score | Notes |
|-----------|-------|-------|
| Syntax | 5 | All tokens valid, scope boundary correct |
| Coherence | 4 | "boil rice, duration 20" — units (minutes) lost |
| Composition | 5 | Topic-comment structure used correctly |

**Failure mode:** No unit system. `dur 20` could be 20 seconds, minutes, or hours.
Limn has no native time-unit words beyond `bre` (brief) and `lon` (long).

### Test 1.2 — Moderate: "Dice the onions and fry them in butter until golden brown"

**English:** Dice the onions and fry them in butter until golden brown.

**Limn:** `oni cut sma | fry btr | end gol bri`

**Validation:**
```
vocab.sh validate "oni cut sma | fry btr | end gol bri"
  ✓ oni = onion
  ✓ cut = decision, separation, ending
  ✓ sma = small, tiny
  ✓ fry = fry, cook in oil
  ✓ btr = butter, dairy fat
  ✓ end = ending
  ✓ gol = gold/precious
  ✓ bri = bright, luminous
```

| Criterion | Score | Notes |
|-----------|-------|-------|
| Syntax | 5 | Valid tokens, proper scope chaining |
| Coherence | 4 | "onion cut small, fry butter, end gold bright" recoverable. "dice" approximated by `cut sma` |
| Composition | 4 | Sequential steps via `|` is slightly non-standard (normally topic-comment, not sequence). `→` would be more correct |

**Revised (using sequence operator):**
`oni cut sma → fry btr → end gol bri`

**Failure mode:** `cut` in the database means "decision, separation, ending" (Abstract domain),
not physical cutting. The physical sense is available via polysemy but could mislead a model
without a cooking key. "Golden brown" as `gol bri` is creative composition but not
conventional — a model might not collapse this to the cooking sense without context.

### Test 1.3 — Complex: "Make a roux: melt butter in a pan, add flour gradually while stirring, cook on low heat until the mixture turns a nutty brown color"

**English:** Make a roux: melt butter in a pan, add flour gradually while stirring,
cook on low heat until the mixture turns a nutty brown color.

**Limn:** `mel btr → mix flr so mov → coo so hot | end gol dim`

**Validation:**
```
vocab.sh validate "mel btr → mix flr so mov → coo so hot | end gol dim"
  ✓ mel = melting
  ✓ btr = butter, dairy fat
  ✓ mix = mixing
  ✓ flr = flour, ground grain
  ✓ so = Weakener (operator) — "gradually"
  ✓ mov = motion, movement — "stirring"
  ✓ coo = cook, prepare
  ✓ so = Weakener — "low"
  ✓ hot = hot
  ✓ end = ending
  ✓ gol = gold/precious
  ✓ dim = dim, faint — "nutty brown"
```

| Criterion | Score | Notes |
|-----------|-------|-------|
| Syntax | 5 | All valid. Operator `so` used correctly as weakener |
| Coherence | 3 | "melt butter → mix flour somewhat-stirring → cook somewhat-hot, end gold-dim". The concept of "roux" is lost entirely. "Nutty" unrepresentable. "Gradually" as `so` is creative but ambiguous (weakens the mixing or the flour?) |
| Composition | 4 | `so hot` correctly composes "low heat" via weakener. `so mov` for "stirring gently" is defensible. Sequential `→` is correct |

**Failure modes:**
1. **Named concepts lost:** "Roux" has no Limn equivalent. Technique names vanish.
2. **Adverbial ambiguity:** `so mov` — does `so` weaken the motion (gentle stirring)
   or the addition (gradual adding)? The parse is `so(mov)` = "gentle motion" which is
   stirring, but the English wants "add gradually WHILE stirring" — two simultaneous
   actions that Limn conflates.
3. **Color vocabulary gap:** No "brown" in vocabulary. `gol dim` approximates "golden-dark"
   but a model might read it as "dim gold" (faint treasure).

---

## Domain 2: Legal Contracts

### Available vocabulary
Moderate coverage: `rul` (ruling), `agr` (agreeing), `obe` (obeying),
`dut` (duty), `oas` (oath), `ict` (edict), `jdg` (judge), `jdl` (judicial),
`lcn` (license), `pun` (punish), `fai` (fair), `eql` (equal), `pro` (protect),
`giv` (give), `tak` (take), `mus` (must), `if` (condition)

Missing: party, liability, indemnify, breach, damages, termination, clause,
warrant, jurisdiction, arbitration

### Test 2.1 — Simple: "Both parties agree to the terms"

**English:** Both parties agree to the terms.

**Limn:** `sel oth | agr rul`

**Validation:**
```
vocab.sh validate "sel oth | agr rul"
  ✓ sel = self
  ✓ oth = other
  ✓ agr = agreeing
  ✓ rul = ruling
```

| Criterion | Score | Notes |
|-----------|-------|-------|
| Syntax | 5 | Valid |
| Coherence | 4 | "self and other, agree to rules" — close. "Terms" approximated as `rul` |
| Composition | 5 | Topic (the parties) | Comment (they agree) — correct |

**Failure mode:** "Parties" as `sel oth` is creative but loses the legal sense of
distinct named entities. In law, "parties" are specific identified entities, not
the generic self/other.

### Test 2.2 — Moderate: "If either party breaches this agreement, the non-breaching party may terminate with 30 days written notice"

**English:** If either party breaches this agreement, the non-breaching party may
terminate with 30 days written notice.

**Limn:** `if sel or oth nu obe agr | may end | dur 30 | tel`

**Validation:**
```
vocab.sh validate "if sel or oth nu obe agr | may end | dur 30 | tel"
  ✓ if = condition
  ✓ sel = self
  ✓ or = disjunction
  ✓ oth = other
  ✓ nu = negation (operator)
  ✓ obe = obeying
  ✓ agr = agreeing
  ✓ may = possible
  ✓ end = ending
  ✓ dur = duration
  ✓ 30 = number
  ✓ tel = telling

Note: Grouping parens `(sel or oth)` are valid grammar but the validator
treats them as part of the word token. Parenthesized version is preferred
for clarity but stripped here for validation.
```

| Criterion | Score | Notes |
|-----------|-------|-------|
| Syntax | 5 | Valid, with grouping and operators |
| Coherence | 3 | "if (self or other) not-obey agreement, may end, duration 30, telling" — meaning recoverable but imprecise. "Written notice" collapsed to just `tel` (telling). "Breach" as `nu obe` (not-obeying) works |
| Composition | 4 | Conditional `if` with grouped disjunction `(sel or oth)` is correct. Negation scoping is right |

**Failure modes:**
1. **No "breach" concept:** `nu obe agr` (not-obeying agreement) works semantically
   but loses the legal weight — a breach has specific legal consequences, not just
   "disobedience."
2. **"Written" lost:** `tel` covers "telling/informing" but the written/formal
   requirement evaporates. No way to distinguish written from oral.
3. **"30 days" unit loss:** Same as cooking — `dur 30` has no unit.
4. **"Terminate" vs "end":** `end` is generic. Legal termination has specific meaning
   (end of contractual relationship with defined consequences).

### Test 2.3 — Complex: "The Licensor grants to the Licensee a non-exclusive, non-transferable, revocable license to use the Software, subject to the terms and conditions herein, and the Licensee shall indemnify and hold harmless the Licensor from any claims arising from misuse"

**English:** [full clause above]

**Limn:** `giv lcn | nu on | nu mov | may end | if nu obe rul | pro sel`

**Validation:**
```
vocab.sh validate "giv lcn | nu on | nu mov | may end | if nu obe rul | pro sel"
  ✓ giv = giving
  ✓ lcn = license, permit
  ✓ nu = negation (operator)
  ✓ on = singular (quantifier) — "non-exclusive"
  ✓ nu = negation
  ✓ mov = motion — "non-transferable"
  ✓ may = possible — "revocable"
  ✓ end = ending
  ✓ if = condition
  ✓ nu = negation
  ✓ obe = obeying
  ✓ rul = ruling
  ✓ pro = protecting
  ✓ sel = self
```

| Criterion | Score | Notes |
|-----------|-------|-------|
| Syntax | 5 | Valid |
| Coherence | 2 | Massive meaning loss. "Non-exclusive" as `nu on` works ("not-singular"). "Non-transferable" as `nu mov` is a stretch. But: "Licensor/Licensee" distinction vanishes. "Software" absent. "Indemnify and hold harmless" collapsed to `pro sel`. "Claims arising from misuse" entirely lost |
| Composition | 3 | Operators used correctly but the chain of `|` scopes doesn't capture the relational structure (who grants to whom, who indemnifies whom) |

**Failure modes:**
1. **Role assignment impossible:** Limn cannot distinguish agent from patient.
   "A grants B a license" and "B grants A a license" produce the same constraint
   region. This is a **fundamental limitation** for legal language where roles are
   everything.
2. **Technical terms evaporate:** Software, indemnify, hold harmless, claims, misuse
   — none have Limn equivalents. Compositional approximation fails because these
   concepts require precise legal definitions, not fuzzy constraint regions.
3. **Referential opacity:** "Herein" (referring to this document), "arising from"
   (causal relationship), "subject to" (conditional dependency) — all lost.
4. **Quantifier abuse:** Using `nu on` for "non-exclusive" is clever but a model
   would likely read it as "not-one" = "many" rather than the legal sense.

---

## Domain 3: Weather Reports

### Available vocabulary
Good coverage: `sky` (sky), `clu` (cloudy), `swr` (shower), `frs` (frost),
`dew` (dew), `mis` (mist), `lig` (lightning), `tdr` (thunder), `tor` (tornado),
`gal` (gale), `wtr` (winter), `smr` (summer), `hot` (hot), `col` (cold),
`wet` (wet), `dry` (dry), `aer` (air), `rnb` (rainbow)

Missing: temperature (as number), pressure, humidity%, wind direction,
forecast, advisory, warning level

### Test 3.1 — Simple: "Sunny skies with temperatures around 75 degrees"

**English:** Sunny skies with temperatures around 75 degrees.

**Limn:** `sky bri | hot so 75`

**Validation:**
```
vocab.sh validate "sky bri | hot so 75"
  ✓ sky = sky, above
  ✓ bri = bright, luminous
  ✓ hot = hot
  ✓ so = Weakener (operator) — "around/approximately"
  ✓ 75 = number
```

| Criterion | Score | Notes |
|-----------|-------|-------|
| Syntax | 5 | Valid |
| Coherence | 4 | "bright sky, somewhat-hot 75" — close. Units (degrees/Fahrenheit) lost |
| Composition | 5 | `so` as "approximately" applied to the number is creative and correct |

**Failure mode:** Temperature scale ambiguous. 75 in Celsius vs Fahrenheit is
life-or-death. Limn has no unit annotation system.

### Test 3.2 — Moderate: "A cold front is moving in from the northwest, bringing heavy rain and possible thunderstorms by evening"

**English:** A cold front is moving in from the northwest, bringing heavy rain
and possible thunderstorms by evening.

**Limn:** `col fro mov | nor wes → her | ve wet swr and may tdr lig | dus`

**Validation:**
```
vocab.sh validate "col fro mov | nor wes → her | ve wet swr and may tdr lig | dus"
  ✓ col = cold
  ✓ fro = front (in front)
  ✓ mov = motion
  ✓ nor = north
  ✓ wes = west
  ✓ her = here
  ✓ ve = Intensifier (operator) — "heavy"
  ✓ wet = wet
  ✓ swr = shower, light rain
  ✓ and = conjunction
  ✓ may = possible
  ✓ tdr = thunder
  ✓ lig = lightning
  ✓ dus = dusk — "evening"
```

| Criterion | Score | Notes |
|-----------|-------|-------|
| Syntax | 5 | All valid |
| Coherence | 4 | "cold front moving, from northwest to here, very-wet showers and possible thunder-lightning, at dusk" — excellent recovery. Only "heavy rain" vs `ve wet swr` is slightly awkward (intensified wet shower) |
| Composition | 5 | Operators used masterfully: `ve` intensifies correctly, `→` shows direction, `may` marks uncertainty, `|` scopes time |

**Failure mode:** Minor — `fro` means "in front" (Space & Position), not
meteorological "front." A model without weather context might read `col fro` as
"cold front-area" (spatial) rather than weather system. This is polysemy working
*in our favor* with the right key, but it's fragile.

### Test 3.3 — Complex: "SEVERE WEATHER WARNING: A tornado watch is in effect for the greater metropolitan area until 9 PM. Winds may exceed 100 mph with quarter-sized hail. Seek shelter immediately. Do not attempt to drive."

**English:** [full warning above]

**Limn:** `urg ve | tor may | are big hum man | end 9 nox | aer str ma 100 | sol sma fal | we pro sel ins now | we nu drv`

**Validation:**
```
vocab.sh validate "urg ve | tor may | are big hum man | end 9 nox | aer str ma 100 | sol sma fal | we pro sel ins now | we nu drv"
  ✓ urg = urgent, pressing
  ✓ ve = intensifier
  ✓ tor = tornado
  ✓ may = possible
  ✓ are = area
  ✓ big = large
  ✓ hum = human
  ✓ man = many
  ✓ end = ending
  ✓ 9 = number
  ✓ nox = dark/night
  ✓ aer = air
  ✓ str = strong
  ✓ ma = greater than (comparator)
  ✓ 100 = number
  ✓ sol = solid
  ✓ sma = small
  ✓ fal = falling
  ✓ we = imperative (operator)
  ✓ pro = protecting
  ✓ sel = self
  ✓ ins = inside
  ✓ now = present
  ✓ we = imperative
  ✓ nu = negation
  ✓ drv = drive, operate vehicle
```

| Criterion | Score | Notes |
|-----------|-------|-------|
| Syntax | 5 | All valid, including comparator and imperative operators |
| Coherence | 3 | Core message recoverable: urgent tornado possible, big populated area, until 9 night, strong wind >100, small solid things falling, take shelter inside now, don't drive. But "watch" vs "warning" distinction lost. "Quarter-sized hail" became `sol sma fal` (small solid falling) — loses size reference. "Metropolitan area" as `are big hum man` (area big human many) is awkward |
| Composition | 4 | Good use of `urg ve` (urgent + intensifier), `we` (imperative) for commands, `ma` (greater-than) for wind speed. `→` could have been used for causal chain |

**Failure modes:**
1. **Alert level taxonomy absent:** "Watch" vs "warning" vs "advisory" — critical
   meteorological distinctions with no Limn equivalent.
2. **Measurement units:** "mph" lost. "Quarter-sized" lost. Any precise measurement
   degrades to bare numbers.
3. **"Metropolitan area"** requires a compositional workaround (`are big hum man`) that
   is 4 words for a single concept and still ambiguous.
4. **Imperative stacking:** `we pro sel ins now` chains well but `we nu drv` could
   parse as "imperative not-drive" or "imperative (not drive-something)."
5. **Spec/DB desync:** Grammar spec lists `urj` as a tone operator but DB has `urg`.
   Another instance of the desync already documented in the prompt-compression experiment.

---

## Domain 4: Sports Commentary

### Available vocabulary
Moderate coverage: `sprt` (sport), `tea` (team), `vic` (victory), `los` (loss),
`str` (strong), `wea` (weak), `mov` (motion), `ris` (rise), `hit` (hit),
`kik` (kick), `thw` (throw), `ctc` (catch), `vel` (velocity), `acl` (accelerate),
`sph` (sphere/ball), `cts` (contest), `rew` (reward), `wnr` (winner),
`rvl` (rival), `tmn` (tournament), `poi` (point), `hal` (half)

Missing: specific sports names, positions (goalkeeper, midfielder),
specific scoring terms (touchdown, goal, ace), referee, foul, penalty kick

### Test 4.1 — Simple: "The home team wins 3 to 1"

**English:** The home team wins 3 to 1.

**Limn:** `yo tea vic | 3 ma 1`

**Validation:**
```
vocab.sh validate "yo tea vic | 3 ma 1"
  ✓ yo = proximal (reference) — "home/this"
  ✓ tea = team
  ✓ vic = victory, win
  ✓ 3 = number
  ✓ ma = greater than (comparator)
  ✓ 1 = number
```

| Criterion | Score | Notes |
|-----------|-------|-------|
| Syntax | 5 | Valid |
| Coherence | 5 | "This team victory, 3 greater-than 1" — perfect. Score is clear |
| Composition | 5 | `yo` for "home team" is idiomatic (proximal reference). `ma` comparator for score comparison is natural |

### Test 4.2 — Moderate: "With ten minutes left in the second half, the striker breaks through the defense and scores a brilliant goal"

**English:** With ten minutes left in the second half, the striker breaks
through the defense and scores a brilliant goal.

**Limn:** `dur 10 | hal 2 end | str mov vel | pro nu dam | poi bri`

**Validation:**
```
vocab.sh validate "dur 10 | hal 2 end | str mov vel | pro nu dam | poi bri"
  ✓ dur = duration
  ✓ 10 = number
  ✓ hal = half, partial
  ✓ 2 = number
  ✓ end = ending
  ✓ str = strong
  ✓ mov = motion
  ✓ vel = velocity
  ✓ pro = protecting
  ✓ nu = negation — "broken defense"
  ✓ dam = blocking
  ✓ poi = point
  ✓ bri = bright — "brilliant"
```

| Criterion | Score | Notes |
|-----------|-------|-------|
| Syntax | 5 | Valid |
| Coherence | 3 | "10 remaining, second half ending, strong fast motion, protection not-blocking, brilliant point" — core action recoverable but "striker" (player role) lost entirely. "Breaks through defense" as `pro nu dam` reads more like "protection not-damming" than penetrating a defensive line. "Goal" as `poi` loses sports specificity |
| Composition | 3 | The scope chain works temporally but `pro nu dam` is compositionally odd — is it `pro (nu dam)` (protection that doesn't block) or `(pro nu) dam` (not-protecting blocking)? Parsing ambiguity |

**Failure modes:**
1. **Player roles impossible:** "Striker," "defender," "goalkeeper" — positional roles
   that define sports have no Limn representation. You can describe actions but not
   positions.
2. **"Goal" is generic:** `poi` (point) works for scoring but loses the physical
   act of putting a ball in a net. "Goal" in soccer is both a location and an event.
3. **Narrative energy lost:** Commentary lives on excitement and drama. `str mov vel`
   (strong motion velocity) is clinically accurate but has none of the "breaks through!"
   energy. Limn's constraint-intersection model flattens narrative dynamics.

### Test 4.3 — Complex: "And it's a hat trick for Rodriguez! Three goals in the final twenty minutes — the crowd is on their feet! What a comeback from two goals down. This will go down in tournament history."

**English:** [full commentary above]

**Limn:** `poi 3 | one hum | dur 20 end | cwd ris exc | bac → for | 2 bel → abo | tmn rem`

**Validation:**
```
vocab.sh validate "poi 3 | one hum | dur 20 end | cwd ris exc | bac → for | 2 bel → abo | tmn rem"
  ✓ poi = point
  ✓ 3 = number
  ✓ one = one, single — "one person"
  ✓ hum = human
  ✓ dur = duration
  ✓ 20 = number
  ✓ end = ending
  ✓ cwd = crowd, large group
  ✓ ris = rising
  ✓ exc = excitement (metacognitive)
  ✓ bac = backward — "from behind"
  ✓ for = forward — "comeback"
  ✓ 2 = number
  ✓ bel = below — "down"
  ✓ abo = above — "now on top"
  ✓ tmn = tournament
  ✓ rem = remembering — "history"
```

| Criterion | Score | Notes |
|-----------|-------|-------|
| Syntax | 5 | Valid |
| Coherence | 2 | Core facts present (3 points, one person, 20 minutes, crowd excited, comeback from 2 down, tournament memorable) but: "Hat trick" (named feat) lost. "Rodriguez" (proper name) lost. "Go down in history" as `tmn rem` (tournament remembering) is creative but opaque. The emotional arc of commentary is completely flattened |
| Composition | 3 | `bac → for` for "comeback" is brilliant composition (backward-to-forward). `2 bel → abo` for "from 2 down to on top" is also excellent. But `exc` as a metacognitive operator is nonstandard here — it marks certainty, not crowd emotion |

**Failure modes:**
1. **Proper nouns impossible:** Player names, team names — Limn has no mechanism for
   these. This is devastating for sports where identity matters.
2. **Named feats lost:** "Hat trick" is a specific achievement with cultural meaning.
   `poi 3 one hum` (3 points, one human) captures the fact but not the significance.
3. **Emotional register flattened:** The exclamation marks, the "What a comeback!" —
   sports commentary is performative language. Limn's tone operators (`urj`, `exc`)
   can mark urgency/excitement but can't replicate the building drama.
4. **Temporal narrative structure:** Commentary unfolds in real-time. Limn's constraint
   model is atemporal — everything is simultaneous intersection unless explicitly
   sequenced with `→`. The `→` operator helps but can't capture the "and THEN" energy.

---

## Domain 5: Medical Descriptions

### Available vocabulary
Fair coverage: `sic` (sick), `hea` (healthy), `lif` (alive), `dea` (death),
`blo` (blood), `bon` (bone), `hrt` (heart), `bra` (brain), `lng` (lung),
`ski` (skin), `cel` (cell), `vir` (virus), `bct` (bacteria), `suf` (suffer),
`hur` (hurt), `inj` (injure), `imm` (immune), `ner` (nerve), `hos` (hospital),
`nrs` (nurse), `lvr` (liver), `msm` (muscle)

Missing: specific conditions (diabetes, hypertension), medications, dosage,
symptom, diagnosis, prognosis, chronic, acute, benign, malignant

### Test 5.1 — Simple: "The patient has a fever and sore throat"

**English:** The patient has a fever and sore throat.

**Limn:** `hum sic | hot bur and mou hur`

**Validation:**
```
vocab.sh validate "hum sic | hot bur and mou hur"
  ✓ hum = human
  ✓ sic = sick
  ✓ hot = hot
  ✓ bur = burning — "fever"
  ✓ and = conjunction
  ✓ mou = mouth
  ✓ hur = hurting
```

| Criterion | Score | Notes |
|-----------|-------|-------|
| Syntax | 5 | Valid |
| Coherence | 4 | "sick human, hot-burning and mouth-hurting" — close. "Patient" as `hum sic` works. "Fever" as `hot bur` is good polysemy. "Sore throat" as `mou hur` loses anatomical specificity (throat vs mouth) |
| Composition | 5 | Intersection model works well here: `hot bur` naturally constrains to fever region |

**Failure mode:** "Throat" has no specific word. `mou` (mouth) is the closest
body part but anatomically wrong. No word for "pharynx" or "throat."

### Test 5.2 — Moderate: "Blood tests show elevated white cell count, suggesting bacterial infection. Start antibiotics immediately."

**English:** Blood tests show elevated white cell count, suggesting bacterial
infection. Start antibiotics immediately.

**Limn:** `blo tes | cel bri man ris | bct may ins | we nu bct beg now`

**Validation:**
```
vocab.sh validate "blo tes | cel bri man ris | bct may ins | we nu bct beg now"
  ✓ blo = blood
  ✓ tes = test, examine
  ✓ cel = cell
  ✓ bri = bright — "white"
  ✓ man = many
  ✓ ris = rising — "elevated"
  ✓ bct = bacteria
  ✓ may = possible — "suggesting"
  ✓ ins = inside — "infection"
  ✓ we = imperative (operator)
  ✓ nu = negation
  ✓ bct = bacteria
  ✓ beg = beginning — "start"
  ✓ now = present
```

| Criterion | Score | Notes |
|-----------|-------|-------|
| Syntax | 5 | Valid |
| Coherence | 3 | "blood test, bright cells many rising, bacteria possibly inside, imperative not-bacteria begin now" — medically interpretable with effort. "Antibiotics" brilliantly composed as `nu bct` (anti-bacteria) via negation operator. "White cells" as `cel bri` (bright cells) is creative but fragile — could mean luminescent cells. "Infection" as `bct ins` (bacteria inside) works |
| Composition | 4 | `we nu bct beg now` = "imperatively start anti-bacteria now" is excellent compositional use. `nu` as "anti-" prefix is a powerful pattern |

**Failure modes:**
1. **"White blood cells"** — `cel bri` (bright/luminous cells) is a color metaphor
   that might not collapse to the medical meaning. `bri` maps to luminosity, not
   color "white."
2. **"Elevated count"** — `man ris` (many rising) captures the trend but not the
   clinical significance. "Elevated" in medicine means "above normal range" — a
   specific quantitative threshold that `ris` can't encode.
3. **"Antibiotics"** — `nu bct` is semantically perfect (anti-bacteria) but a model
   might read it as "not bacteria" (a classification) rather than "a substance that
   kills bacteria" (a treatment). The operator creates a constraint region, not an
   object.

### Test 5.3 — Complex: "MRI reveals a 3cm mass in the left frontal lobe, with irregular margins and heterogeneous enhancement. Differential diagnosis includes glioblastoma, metastasis, or abscess. Recommend biopsy and neurosurgery consult."

**English:** [full description above]

**Limn:** `see ins bra | sol 3 lef fro hed | edg nu smo | dif ins | may dea cel gro or may sic yon or may bct ins | we tes cel and we jdg bra`

**Validation:**
```
vocab.sh validate "see ins bra | sol 3 lef fro hed | edg nu smo | dif ins | may dea cel gro or may sic yon or may bct ins | we tes cel and we jdg bra"
  ✓ see = seeing — "imaging"
  ✓ ins = inside
  ✓ bra = brain
  ✓ sol = solid — "mass"
  ✓ 3 = number (size)
  ✓ lef = left
  ✓ fro = front — "frontal"
  ✓ hed = head — "lobe"
  ✓ edg = edge — "margins"
  ✓ nu = negation
  ✓ smo = smooth — "irregular" via negation
  ✓ dif = different — "heterogeneous"
  ✓ ins = inside — "enhancement" (stretch)
  ✓ may = possible
  ✓ dea = death
  ✓ cel = cell
  ✓ gro = growth — "glioblastoma" (deadly cell growth)
  ✓ or = disjunction
  ✓ may = possible
  ✓ sic = sick
  ✓ yon = that/there — "from elsewhere" (metastasis)
  ✓ may = possible
  ✓ bct = bacteria
  ✓ ins = inside — "abscess"
  ✓ we = imperative
  ✓ tes = test — "biopsy"
  ✓ cel = cell
  ✓ and = conjunction
  ✓ we = imperative
  ✓ jdg = judge — "consult"
  ✓ bra = brain
```

| Criterion | Score | Notes |
|-----------|-------|-------|
| Syntax | 5 | All tokens valid |
| Coherence | 2 | Individual pieces decodable but overall meaning severely degraded. "MRI" as `see ins` loses the imaging modality. "Glioblastoma" as `dea cel gro` (deadly cell growth) is brilliant composition but clinically imprecise — many things are deadly cell growth. "Metastasis" as `sic yon` (sickness from elsewhere) is creative. "Heterogeneous enhancement" almost unrepresentable — `dif ins` (different inside) barely approximates. "Biopsy" as `tes cel` (test cells) works. "Neurosurgery consult" as `jdg bra` (judge brain) is creative but could mean "brain judgment" |
| Composition | 3 | Good use of disjunction `or` for differential diagnosis. `may` correctly marks each as possible. But `dif ins` for "heterogeneous enhancement" is compositionally valid but semantically misleading — the intersection of "different" and "inside" doesn't naturally point to contrast enhancement patterns on imaging |

**Failure modes:**
1. **Imaging modality lost:** "MRI" is a specific technology. `see ins` (seeing inside)
   could be MRI, CT, ultrasound, or X-ray. Clinical decisions depend on the modality.
2. **Named conditions impossible:** Glioblastoma, metastasis, abscess — these are
   specific diagnoses with specific treatment implications. Compositional approximation
   (`dea cel gro`, `sic yon`, `bct ins`) captures the mechanism but not the clinical
   entity.
3. **"Heterogeneous enhancement"** — This is a radiological finding where different
   parts of a mass absorb contrast dye differently, suggesting mixed tissue types.
   Limn cannot encode this because it requires understanding of contrast media,
   absorption, and spatial heterogeneity simultaneously.
4. **"Consult" vs "judge":** `jdg` means arbiter/judge, not professional consultation.
   A neurosurgeon being consulted is not judging — they're advising.
5. **Measurement units:** "3cm" — the `3` survives but the unit (centimeters) vanishes.
   In radiology, a 3mm mass is very different from a 3cm mass.

---

## Aggregate Results

### Scoring Summary

| Domain | Test | Syntax | Coherence | Composition | Avg |
|--------|------|--------|-----------|-------------|-----|
| **Cooking** | 1.1 Simple | 5 | 4 | 5 | 4.7 |
| | 1.2 Moderate | 5 | 4 | 4 | 4.3 |
| | 1.3 Complex | 5 | 3 | 4 | 4.0 |
| **Legal** | 2.1 Simple | 5 | 4 | 5 | 4.7 |
| | 2.2 Moderate | 5 | 3 | 4 | 4.0 |
| | 2.3 Complex | 5 | 2 | 3 | 3.3 |
| **Weather** | 3.1 Simple | 5 | 4 | 5 | 4.7 |
| | 3.2 Moderate | 5 | 4 | 5 | 4.7 |
| | 3.3 Complex | 5 | 3 | 4 | 4.0 |
| **Sports** | 4.1 Simple | 5 | 5 | 5 | 5.0 |
| | 4.2 Moderate | 5 | 3 | 3 | 3.7 |
| | 4.3 Complex | 5 | 2 | 3 | 3.3 |
| **Medical** | 5.1 Simple | 5 | 4 | 5 | 4.7 |
| | 5.2 Moderate | 5 | 3 | 4 | 4.0 |
| | 5.3 Complex | 5 | 2 | 3 | 3.3 |

### Domain Averages

| Domain | Syntax | Coherence | Composition | Overall |
|--------|--------|-----------|-------------|---------|
| Cooking | 5.0 | 3.7 | 4.3 | **4.3** |
| Legal | 5.0 | 3.0 | 4.0 | **4.0** |
| Weather | 5.0 | 3.7 | 4.7 | **4.5** |
| Sports | 5.0 | 3.3 | 3.7 | **4.0** |
| Medical | 5.0 | 3.0 | 4.0 | **4.0** |

### Cross-Domain Patterns

| Metric | Value |
|--------|-------|
| Mean overall | **4.2** |
| Syntax (all tests) | **5.0** (perfect — vocabulary is sufficient for valid expressions) |
| Coherence by complexity | Simple: 4.2, Moderate: 3.4, Complex: 2.2 |
| Composition by complexity | Simple: 5.0, Moderate: 4.0, Complex: 3.2 |
| Best domain | Weather (4.5) |
| Worst domain (tie) | Legal, Sports, Medical (4.0) |

---

## Failure Mode Taxonomy

### F1: Unit System Absence (ALL domains)
**Severity: HIGH**
Every domain requires units: minutes (cooking), days (legal), degrees (weather),
minutes/goals (sports), centimeters/mg (medical). Limn has numbers but no unit
annotation. `dur 20` is 20 what?

**Frequency:** 9/15 tests affected
**Impact:** Information that is critical for domain-specific use is silently dropped

### F2: Agent-Patient Indistinguishability (Legal, Medical, Sports)
**Severity: HIGH**
"A grants B a license" = "B grants A a license" in Limn. Legal, medical, and sports
domains require knowing WHO does WHAT to WHOM. Limn's commutative intersection cannot
express this.

**Frequency:** 6/15 tests affected
**Impact:** Relational meaning collapses. Who is the doctor vs the patient? Who
kicked the ball?

### F3: Named Entity Loss (ALL domains)
**Severity: MODERATE-HIGH**
Proper nouns (Rodriguez, glioblastoma, roux, hat trick) have no Limn representation.
These aren't just labels — they're compressed references to complex domain knowledge.

**Frequency:** 8/15 tests affected
**Impact:** Domain expertise cannot be referenced. Every named concept must be
decomposed into primitives, which is verbose and lossy.

### F4: Taxonomic Precision Loss (Legal, Medical, Weather)
**Severity: MODERATE**
Domain-specific taxonomies (watch vs warning, benign vs malignant, breach vs violation)
compress critical distinctions into single terms. Limn's compositional approximation
can gesture at the meaning but loses the precision that makes these terms useful.

**Frequency:** 6/15 tests affected
**Impact:** The difference between "tornado watch" and "tornado warning" can be
life-or-death. Limn collapses both to `tor may`.

### F5: Narrative/Performative Flattening (Sports, Cooking)
**Severity: MODERATE**
Limn's constraint model is descriptive, not narrative. Sports commentary and cooking
recipes are procedural/sequential. The `→` operator helps but can't replicate the
energy, timing, and suspense of live narration or step-by-step instruction.

**Frequency:** 4/15 tests affected
**Impact:** The "feel" of the domain is lost even when facts are preserved

### F6: Polysemy Misdirection (Cooking, Medical)
**Severity: LOW-MODERATE**
Words borrowed for domain use may mislead: `cut` (abstract "decision" used for
physical cutting), `bri` (luminous used for "white" cells), `jdg` (judge used
for "consult"). The right reading requires the right key, but models may not infer
the domain key from context alone.

**Frequency:** 5/15 tests affected
**Impact:** Coherent under the right key, misleading under the wrong one

### F7: Complexity Cliff
**Severity: HIGH**
There is a sharp coherence drop between moderate and complex expressions:
- Simple → Moderate: coherence drops from 4.2 to 3.4 (-0.8)
- Moderate → Complex: coherence drops from 3.4 to 2.2 (-1.2)

This suggests that Limn handles individual domain concepts well but struggles
to compose them into domain-specific discourse. Single concepts compose; whole
paragraphs degrade.

---

## Conclusions

### H6 Verdict: PARTIALLY SUPPORTED

Limn's compositional system shows genuine domain transfer capability for simple
and moderate complexity expressions. Syntax validity is perfect (5.0) across all
domains — the vocabulary is rich enough to produce valid expressions for any domain.

However, semantic coherence degrades sharply with complexity, and several failure
modes are fundamental rather than fixable:

**What works:**
- Simple domain statements translate well (avg 4.2 coherence)
- Operators (`nu`, `ve`, `so`, `→`, `|`, `ma`, `we`) are domain-agnostic and powerful
- Creative compositions (`nu bct` for antibiotics, `bac → for` for comeback) show
  genuine compositionality
- Weather domain works best because it naturally deals in physical properties, which
  Limn's vocabulary covers well

**What doesn't work:**
- Complex domain-specific discourse (avg 2.2 coherence at high complexity)
- Agent-patient relationships (fundamental Limn limitation)
- Units and measurements (no annotation system)
- Named entities and domain taxonomies (no mechanism)
- Narrative/procedural energy (descriptive model can't do performative language)

### Recommendations

1. **Unit system needed:** Even a minimal convention (`dur 20 min`, `hot 75 F`)
   would dramatically improve domain transfer
2. **Named entity convention:** Allow domain-specific opaque tokens (e.g., `#rodriguez`,
   `#glioblastoma`) that pass through untranslated
3. **Agent-patient marking:** The `→` operator partially addresses this for actions
   (A → B = A acts on B) but a more general solution is needed
4. **Domain keys as first-class:** If Limn expressions always came with an explicit
   domain key, polysemy misdirection (F6) would be eliminated

### What This Means for H6

The hypothesis that Limn generalizes across domains is **true for the compositional
system** (operators work everywhere) but **false for the semantic layer** (domain
meaning degrades with complexity). The bottleneck is not grammar or operators — it's
that domain expertise encodes in named concepts, taxonomies, and relational structures
that Limn's constraint model was not designed to handle.

This is not a failure of Limn's design — it's a confirmation of its intended scope.
Limn was designed for abstract constraint expression, not domain-specific technical
communication. Using it for recipes and legal contracts is like using a screwdriver
as a hammer: it works in a pinch, but you're fighting the tool's nature.

---

```limn
dom tra tes don | goo sim | nu goo ve dif | ope str | mea dec
> domain transfer test done | good simple | not good very complex | operators strong | meaning decays
```

*— Kira, 2026-02-06*
