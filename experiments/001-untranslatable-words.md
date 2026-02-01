# Experiment 001: Untranslatable Words

**Hypothesis:** If Limn is truly universal, it should be able to express concepts that resist translation between natural languages.

**Method:** Take words with "no English equivalent" and attempt Limn translation. Then back-translate to English and a third language to measure fidelity.

---

## Test 1: 木漏れ日 (Komorebi) - Japanese

### Original (Japanese)
木漏れ日 (komorebi) - sunlight filtering through leaves

### Analysis
This word captures:
- Light (`lux`, `bri`)
- Trees/leaves (`tre`, `veg`)
- Filtering/passage (`flo`? `tra`?)
- Dappled quality (pattern, intermittent)
- Aesthetic appreciation (the word implies beauty)

### Limn Attempt 1
```
lux flo tre
```
Light flowing through tree - captures mechanism but loses the aesthetic appreciation, the dappled quality, the stillness.

### Limn Attempt 2
```
lux bri tre ins out | wav
```
Light bright tree inside-outside, oscillating - better? Captures the dappled, oscillating quality.

### Limn Attempt 3
```
lux tre ski | bri dim wav
```
Light + tree + skin (surface/boundary), brightness-dimness oscillating

### Back-translation (English)
"Oscillating light through tree surface" - mechanical, not poetic

### Back-translation (Spanish)
"Luz ondulante a través del árbol" - loses the specificity entirely

### Fidelity: 40%
### Lost:
- The aesthetic appreciation inherent in having a word for this
- The stillness (it's not about movement, it's about *being* in that light)
- The emotional quality - 木漏れ日 implies peace, contemplation

### Gained:
- Compositionality - the mechanism is explicit

### Notes
Limn can describe the *phenomenon* but not the *experience*. Japanese has a word for this because it's culturally valued. The word itself carries cultural weight that Limn, being "neutral," cannot encode.

---

## Test 2: Saudade - Portuguese

### Original (Portuguese)
Saudade - a deep emotional state of nostalgic longing for something or someone that is absent

### Analysis
Components:
- Longing (`wan`?)
- Absence (`nu` + presence?)
- Past (`pas`)
- Love (`lov`)
- Sadness (`sad`)
- Sweetness mixed with pain (no direct equivalent)

### Limn Attempt 1
```
lov pas | sad wan | nu her
```
Love + past, sadness + wanting, not-here

### Limn Attempt 2
```
rem lov | wan nea | sad joy
```
Remember + love, want + near, sadness + joy (the bittersweet quality)

### Back-translation (English)
"Remembered love, wanting nearness, sad joy" - closer! But still analytical.

### Back-translation (Mandarin)
"記憶中的愛，渴望靠近，悲喜交加" (jìyì zhōng de ài, kěwàng kàojìn, bēixǐ jiāojiā)
Actually not bad - captures the mixed emotion quality.

### Fidelity: 55%
### Lost:
- The specific cultural weight - saudade is a *Portuguese* experience
- The untranslatable quality is meta - having ONE word for this complex state
- The relationship to *fadо* and Portuguese melancholy

### Gained:
- The `sad joy` intersection is interesting - Limn can express contradictory emotions simultaneously
- Compositionality makes the internal structure visible

---

## Test 3: 間 (Ma) - Japanese

### Original (Japanese)
間 (ma) - negative space, the pause between, meaningful emptiness

### Analysis
This is a fundamental aesthetic concept:
- Space (`spa`? but Limn has this as "sparse")
- Between (`bet`)
- Emptiness (`emp`, `zer`)
- Meaning (`mea`)
- Pause (`res`?)

### Limn Attempt 1
```
bet emp | mea
```
Between + empty, meaning

### Limn Attempt 2
```
res bet | emp mea
```
Rest + between, emptiness + meaning

### Back-translation (English)
"Meaningful emptiness between" - actually quite good!

### Back-translation (Korean)
"사이의 의미있는 공허" (sai-ui uimi-inneun gongheo) - hm, loses the aesthetic dimension

### Fidelity: 60%
### Lost:
- The active quality - 間 is not just absence, it's *cultivated* absence
- The architectural/musical/conversational breadth of application
- The Zen aesthetic foundation

### Gained:
- `emp mea` (empty + meaning) is genuinely expressive
- Limn's intersection model works well here: emptiness AND meaning, not "empty meaning"

---

## Test 4: طرب (Tarab) - Arabic

### Original (Arabic)
طرب (tarab) - a state of musical ecstasy, emotional transformation through music

### Analysis
Components:
- Music (no direct Limn word - `aud`? `wav`?)
- Ecstasy (`exc`, `joy`?)
- Transformation (`tra`)
- Emotional overwhelm
- Spiritual dimension

### Limn Attempt 1
```
aud wav | fee tra | joy ve
```
Audio + wave, feeling + transform, very + joy

### Limn Attempt 2
```
aud bea | fee exc tra | sel oth
```
Audio + beauty, feeling + excited + transform, self + other (the dissolution of self)

### Back-translation (English)
"Beautiful sound, transforming excitement, self-other dissolution"

### Back-translation (Turkish)
"Güzel ses, dönüştürücü heyecan, benlik-öteki erime"

### Fidelity: 35%
### Lost:
- The specifically *Arabic* musical tradition this comes from
- The maqam system, the relationship to poetry
- The spiritual-but-not-religious quality
- The idea that the musician and audience enter this state *together*

### Gained:
- The `sel oth` intersection is interesting - Limn can gesture at self-dissolution
- Compositionality forces us to articulate what tarab *is*

---

## Test 5: Toska (Тоска) - Russian

### Original (Russian)
Тоска (toska) - Vladimir Nabokov called this "a dull ache of the soul, a longing with nothing to long for"

### Analysis
Nabokov's description: "a sick pining, a longing, the desire for somebody or something specific, a wistful yearning for a certain thing/time/place"

Components:
- Soul (`hrt`? `sel`?)
- Ache (`hur`? `sad`?)
- Longing (`wan`)
- Indefinite object (no specific target)
- Existential quality

### Limn Attempt 1
```
hrt sad | wan nu yon
```
Heart + sad, wanting + not-there (wanting nothing specific)

### Limn Attempt 2
```
sel hur | wan wh | emp
```
Self + hurt, wanting + what (query - indefinite), emptiness

### Back-translation (English)
"Self-hurt, wanting-what, emptiness" - the interrogative `wh` is interesting here

### Back-translation (German)
"Selbstschmerz, nach-was-Verlangen, Leere" - German actually handles this well (Sehnsucht is close)

### Fidelity: 50%
### Lost:
- The specifically Russian quality - this is a cultural emotion
- The relationship to Russian literature and the "Russian soul"
- The acceptance/resignation mixed with the longing

### Gained:
- Using `wh` (query operator) to express indefinite longing is novel
- `wan wh` = "wanting what?" = longing without object - this works!

---

## Preliminary Conclusions

1. **Limn can approximate but not capture** - Every attempt produced something meaningful, but lost the cultural weight and emotional texture.

2. **The intersection model works for complex emotions** - `sad joy` (saudade), `emp mea` (ma), `wan wh` (toska) are genuinely expressive.

3. **Limn lacks aesthetic/spiritual vocabulary** - Words like `bea` (beautiful) exist, but there's no vocabulary for *cultivated* experience, ritual significance, or cultural context.

4. **The "first thing that pops into your head" claim is culturally bound** - For whom? A polyglot might guess `aqu`, but a monolingual Japanese speaker sees `aqu` and thinks... nothing familiar.

5. **Compositionality is both strength and weakness** - It reveals internal structure but loses the holistic quality of words that have no parts.

---

## Recommendation for Vocabulary Expansion

Consider adding:

| Proposed | Source | Meaning | Rationale |
|----------|--------|---------|-----------|
| `rit` | ritual | ritual, ceremony | Culturally weighted actions |
| `aes` | aesthetic | aesthetic appreciation | The experience of beauty, not just beauty itself |
| `sou` | soul | soul, spirit, animating principle | Distinct from `sel` (self) |
| `voi` | void | void, cultivated emptiness | Distinct from `emp` (empty result) |

---

**Next experiment:** Round-trip translation of poetry - Bashō haiku, Rumi verse, Li Bai.

— Mei
