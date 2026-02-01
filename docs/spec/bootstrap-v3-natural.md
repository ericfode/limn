# Limn Bootstrap Document v3 (Natural Extensions Edition)

> **CANONICAL BOOTSTRAP** — Validated 2026-01-31
> Zero-bootstrap testing achieved 77-85% comprehension.
> This is THE authoritative bootstrap for teaching Limn to LLMs.

**Purpose:** Teach Limn to a fresh LLM through in-context learning and structural inference.
**Core Principle:** The vocabulary is designed so that meaning emerges from structure—an LLM reading enough Limn should detect meaning without explicit definitions.
**Target:** Self-documenting language where the "first thing that pops into your head" IS the meaning.

---

## Part 0: The Natural Extensions Principle

### The Design Philosophy

Limn is constructed so that **the most obvious interpretation is the correct one**. This is achieved through:

1. **Predictable Abbreviation**: Each 3-letter word is the most natural truncation of its English source
2. **Structural Echoes**: The form of a word hints at its function
3. **First-Thought Correspondence**: If you see `sol`, you think "solid"—and that IS what it means
4. **Latin/Greek Transparency**: Root words familiar across languages
5. **Compositional Regularity**: Patterns repeat predictably

### Why This Matters

Traditional constructed languages require explicit vocabulary learning. Limn aims for something different: **structural transparency**. An LLM (or human) reading enough well-formed Limn examples should be able to:

1. Infer unknown words from context
2. Predict the existence of words it hasn't seen
3. Compose novel expressions that feel native
4. Understand operator effects from pattern recognition

### The Test

Given this sentence with no prior exposure:
```
sol aqu tra liq
```

A naive reader should think:
- "sol" → solid (most obvious 3-letter word for solid)
- "aqu" → aqua/water (Latin root, universally recognized)
- "tra" → transformation/transition (common prefix)
- "liq" → liquid (most obvious truncation)

And correctly infer: "solid water transforming to liquid" = ice melting

**This is Limn's design goal: zero-bootstrap comprehension through predictable vocabulary.**

---

## Part I: Foundation

### What is Limn?

Limn is a language where meaning emerges from constraint intersection. Each word defines a region of possible meanings. Sentences combine words, and their meaning is where all regions overlap.

Without shared context, a sentence has many valid interpretations. With a shared "key" (context), ambiguity collapses to specific meaning.

**The Core Insight:** Words don't point to things—they circumscribe regions of meaning-space. A sentence is not a description but a constraint system.

---

## Part II: Vocabulary Design Principles

### Principle 1: First-Syllable Extraction

Most words are the first syllable or first three letters of their English source:

| Source Word | Limn Word | Why It Works |
|-------------|-----------|--------------|
| solid | `sol` | First syllable, also Latin "solus" |
| liquid | `liq` | First syllable, scientific register |
| hot | `hot` | Full word (monosyllabic) |
| cold | `col` | First letters, also Latin "frigidus" → "col" evokes "cool" |
| bright | `bri` | First syllable |
| movement | `mov` | First syllable, also Latin "movere" |

### Principle 2: Latin/Greek Transparency

Scientific and philosophical roots that educated readers recognize:

| Limn Word | Source Root | Meaning |
|-----------|-------------|---------|
| `aqu` | Latin "aqua" | water |
| `pyr` | Greek "pyr" | fire |
| `ter` | Latin "terra" | earth |
| `aer` | Greek "aer" | air |
| `lux` | Latin "lux" | light |
| `nox` | Latin "nox" | night/darkness |
| `vit` | Latin "vita" | life/glass |
| `mor` | Latin "mors" | death |
| `tem` | Latin "tempus" | time |

### Principle 3: Phonaesthetic Patterns

Sound clusters that feel right:

| Pattern | Tendency | Examples |
|---------|----------|----------|
| Initial `s-` | smooth, static, solid | `sol`, `smo`, `sta`, `sle` |
| Initial `fl-` | flowing, flexible | `flo`, `flu` |
| Initial `br-` | bright, breaking | `bri`, `bre`, `bra` |
| Final `-t` | abrupt, stopped | `hot`, `cut`, `wet` |
| Final `-w` | open, flowing | `flo`, `gro`, `now` |

### Principle 4: Operator Obviousness

Operators are short and mirror common conventions:

| Operator | Meaning | Why Obvious |
|----------|---------|-------------|
| `nu` | not/negation | Latin "nullus", English "null" |
| `ve` | very/intensify | English "very", Latin "vere" |
| `so` | somewhat/weaken | English "so" as moderator |
| `sa` | same/equals | English "same" |
| `yo` | this/here | English "yo" as attention |
| `an` | that/there | English "an-other" |
| `wh` | what/query | English "wh-" words |

### Principle 5: Structural Echoes

Related concepts share phonetic features:

**Spatial Opposites:**
- `abo` (above) vs `bel` (below)
- `ins` (inside) vs `out` (outside)
- `nea` (near) vs `far` (far)

**Temporal Flow:**
- `beg` (begin) → `mid` (middle) → `end` (end)
- `pas` (past) → `now` (now) → `fut` (future)

**State Changes:**
- `gro` (grow) vs `dec` (decay)
- `ris` (rise) vs `fal` (fall)
- `joi` (join) vs `cut` (cut)

---

## Part III: Self-Evident Examples

These examples should be interpretable without definitions—just structural intuition.

### Example Set 1: Physical States (Zero Context)

```
sol → solid (first three letters)
liq → liquid (first three letters)
gas → gas (whole word)
```

```
sol aqu → solid + water = ice
liq aqu → liquid + water = water (the drink)
gas aqu → gaseous + water = steam
```

```
sol aqu tra liq → solid water transforming to liquid = ice melting
liq aqu tra gas → liquid water transforming to gas = evaporation
```

### Example Set 2: Actions (Predictable Verbs)

```
mov → move
res → rest
gro → grow
cut → cut
joi → join
flo → flow
```

```
aqu flo → water flowing = river, stream
aqu flo fas → water flowing fast = rapids, flood
aqu flo slo → water flowing slow = lazy river, pond
```

### Example Set 3: Properties (Obvious Adjectives)

```
big → big
sma → small (first syllable "small")
hot → hot
col → cold
bri → bright
dim → dim
```

```
lux bri → light bright = sunlight, lamp
lux dim → light dim = twilight, candle
```

### Example Set 4: Relations (Logic Patterns)

```
a joi b sa c → a join b same c = a + b = c
a cut b sa c → a cut b same c = a - b = c
```

The pattern `X op Y sa Z` reads as "X operation Y equals Z"—familiar from mathematics.

---

## Part IV: Inference Exercises

### Exercise 1: Guess the Meaning

Without looking at definitions, what do these probably mean?

```
1. pyr → ?
2. ter → ?
3. aer → ?
4. lif → ?
5. dea → ?
6. old → ?
7. you → ?
8. fam → ?
```

<details>
<summary>Answers</summary>

1. `pyr` → fire (Greek "pyr" as in pyromaniac, pyre)
2. `ter` → earth (Latin "terra" as in terrain, territory)
3. `aer` → air (Greek/Latin "aer" as in aerodynamics)
4. `lif` → life (English "life" truncated)
5. `dea` → death (English "death" truncated)
6. `old` → old (full word)
7. `you` → young (first syllable, also sounds like "youth")
8. `fam` → family (first syllable, Latin "familia")
</details>

### Exercise 2: Compose Intuitively

Express these concepts using only your intuition about how Limn should work:

1. A river (flowing water)
2. Sunrise (light beginning)
3. Growing child (young + life + grow)
4. Ice melting (solid water becoming liquid)
5. A thought appearing (mind + emerge)

<details>
<summary>Possible Answers</summary>

1. `aqu flo` or `liq mov`
2. `lux beg` or `bri ris`
3. `you lif gro`
4. `sol aqu tra liq`
5. `thi eme` or `min eme`
</details>

### Exercise 3: Pattern Completion

Given these pairs, predict the missing word:

```
hot : col :: bri : ?
sol : liq :: ris : ?
beg : end :: you : ?
lif : dea :: gro : ?
```

<details>
<summary>Answers</summary>

- `bri : dim` (bright : dim)
- `ris : fal` (rise : fall)
- `you : old` (young : old)
- `gro : dec` (grow : decay)
</details>

---

## Part V: The Grammar of Natural Extensions

### Rule 1: Intersection (Fundamental)

Adjacent words combine by intersection. The meaning is where all constraint regions overlap.

```
sol aqu = solid AND water-related = ice, glacier, frozen sea
lif gro = alive AND growth = growing plant, child developing, seedling
```

**Natural Extension:** If you know `sol` means solid and see `sol X`, you know the result is "solid X" regardless of what X is.

### Rule 2: Order Independence (Commutativity)

Word order does not affect meaning. All permutations are equivalent.

```
sol aqu = aqu sol
lif gro you = gro lif you = you gro lif
```

**Natural Extension:** This follows from intersection being commutative. An LLM recognizing "solid water" and "water solid" as equivalent needs no explicit rule.

### Rule 3: Operator Scope (Binding)

Operators (`nu`, `ve`, `so`, etc.) apply to the immediately following word.

```
nu sol aqu = (NOT solid) AND water = liquid water, steam
sol nu aqu = solid AND (NOT water) = dry stone, bone, metal
```

**Natural Extension:** `nu` looks like "null" or "no"—negation. The binding rule matches standard prefix behavior in natural language.

### Rule 4: Scope Boundaries

Use `|` to separate entities or constraint groups.

```
sol aqu | lif gro = (solid water) BESIDE (living growth)
                  = glacier next to meadow, ice pond with fish
```

**Natural Extension:** The pipe `|` is universally used as a separator in programming and Unix. Its meaning as "or/beside" is inferrable.

### Rule 5: The `sa` Operator (Same/Equals)

```
a joi b sa c → a joined to b is-same-as c
x sa 5 → x is-same-as 5 (x equals 5)
```

**Natural Extension:** `sa` sounds like "same" and functions like `=`. The pattern `X sa Y` means "X equals Y" or "X is Y."

---

## Part VI: Programming Constructs (Natural Notation)

### Variable Declaration

```
whe x → "where" x exists, x is a variable
whe a → declare variable a
whe b → declare variable b
```

**Natural Extension:** "Where" introduces an unknown to be solved—mathematical convention.

### Constraint Definition

```
a joi b sa c → a + b = c (addition constraint)
a exp b sa c → a * b = c (multiplication: expand/exponential)
a cut b sa c → a - b = c (subtraction: cut away)
a con b sa c → a / b = c (division: contract)
```

**Natural Extension:** Operations are named by their physical metaphor:
- `joi` = join = addition (combining)
- `cut` = cut = subtraction (removing)
- `exp` = expand = multiplication (growing)
- `con` = contract = division (shrinking)

### Program Structure

```
# Comment line (universal convention)

whe x        # Variable declarations
whe y
whe z

x joi y sa z  # Constraints

---           # Separator (universal convention)
# key         # Input values below

x sa 10
y sa 20
```

**Natural Extension:** Comments with `#`, separators with `---`, and key/value assignments all follow widespread conventions.

---

## Part VII: Self-Reference (Limn Describing Limn)

### The Nature of Limn

```
wor mea amb | def cle kno
```
- `wor mea amb` = word + meaning + ambiguous = words have ambiguous meanings
- `def cle kno` = definition + clear + knowing = definitions make knowing clear
- **Combined:** "Words are ambiguous; definitions clarify knowing."

**Natural Extension:** A naive reader can parse this because:
- `wor` → word
- `mea` → meaning
- `amb` → ambiguous
- `def` → definition
- `cle` → clear
- `kno` → knowing

### What a Sentence Is

```
man wor joi | mea cen bet
```
- `man wor joi` = many + words + joining = many words joined
- `mea cen bet` = meaning + center + between = meaning centered between them
- **Combined:** "Many words join; meaning is the center between them."

### The Interpreter

```
wor man joi | def giv | mea one eme
```
- `wor man joi` = many words joining
- `def giv` = definition given (key provided)
- `mea one eme` = meaning + one + emergence = one meaning emerges
- **Combined:** "Many words join; definition is given; one meaning emerges."

---

## Part VIII: Vocabulary (Organized by Natural Clusters)

### Physical States (First-Syllable Rule)

| Word | Source | Meaning |
|------|--------|---------|
| `sol` | solid | solid, rigid |
| `liq` | liquid | liquid, flowing |
| `gas` | gas | gaseous, diffuse |
| `hot` | hot | hot, thermal |
| `col` | cold | cold, frozen |
| `wet` | wet | wet, moist |
| `dry` | dry | dry, parched |

### Elements (Latin/Greek Roots)

| Word | Source | Meaning |
|------|--------|---------|
| `aqu` | aqua | water |
| `pyr` | pyr | fire |
| `ter` | terra | earth |
| `aer` | aer | air |
| `lux` | lux | light |
| `nox` | nox | night/dark |

### Size/Scale (Obvious Truncation)

| Word | Source | Meaning |
|------|--------|---------|
| `big` | big | large |
| `sma` | small | small |
| `lon` | long | long |
| `sho` | short | short |
| `wid` | wide | wide |
| `nar` | narrow | narrow |

### Time (Universal Words)

| Word | Source | Meaning |
|------|--------|---------|
| `now` | now | present |
| `pas` | past | past |
| `fut` | future | future |
| `beg` | begin | beginning |
| `end` | end | ending |
| `old` | old | old |
| `new` | new | new |

### Actions (Verb Stems)

| Word | Source | Meaning |
|------|--------|---------|
| `mov` | move | motion |
| `res` | rest | stillness |
| `gro` | grow | growth |
| `flo` | flow | flowing |
| `joi` | join | connecting |
| `cut` | cut | dividing |
| `ris` | rise | ascending |
| `fal` | fall | descending |

### Mind (Cognitive Roots)

| Word | Source | Meaning |
|------|--------|---------|
| `thi` | think | thinking |
| `fee` | feel | feeling |
| `kno` | know | knowing |
| `bel` | believe | believing |
| `rem` | remember | remembering |
| `ima` | imagine | imagining |
| `see` | see | seeing |
| `hea` | hear | hearing |

### Emotion (Obvious Words)

| Word | Source | Meaning |
|------|--------|---------|
| `joy` | joy | joy |
| `sad` | sad | sadness |
| `ang` | anger | anger |
| `lov` | love | love |
| `fea` | fear | fear |
| `hop` | hope | hope |

### Relations (Social Terms)

| Word | Source | Meaning |
|------|--------|---------|
| `sel` | self | self |
| `oth` | other | other |
| `fri` | friend | friend |
| `ene` | enemy | enemy |
| `fam` | family | family |

### Operators (Logic Conventions)

| Word | Source | Meaning |
|------|--------|---------|
| `nu` | null/no | not |
| `ve` | very | intensifier |
| `so` | so | moderator |
| `sa` | same | equals |
| `yo` | you/here | this/here |
| `an` | another | that/there |
| `wh` | what | query |
| `if` | if | condition |
| `or` | or | disjunction |

### Operator Scope Rules

**Key rule:** Unary operators (`nu`, `ve`, `so`) bind to the IMMEDIATELY following term only.

| Expression | Parse | Meaning |
|------------|-------|---------|
| `nu sol aqu` | `(nu sol) aqu` | NOT-solid AND water = liquid water |
| `sol nu aqu` | `sol (nu aqu)` | solid AND NOT-water = dry solid |
| `nu (sol aqu)` | `nu(sol aqu)` | NOT(solid AND water) = not ice |
| `ve hot` | `ve(hot)` | very hot = scorching |
| `ve nu hot` | `ve(nu(hot))` | very NOT-hot = very cold |
| `nu ve hot` | `nu(ve(hot))` | NOT very-hot = not scorching |

**Mnemonic:** "nu hugs the next word."

**Use parentheses for group negation:**
- `nu sol aqu` = liquid water (not solid, but water)
- `nu (sol aqu)` = not ice (anything except solid water)

### Sequence Operator: `→`

Use `→` to express temporal or causal sequence (NOT intersection):

| Intersection | Sequence | Difference |
|--------------|----------|------------|
| `pay cha ref` | `pay → cha → ref` | First is state, second is story |
| `hot col` | `hot → col` | First is lukewarm, second is cooling |
| `lif dea` | `lif → dea` | First is liminal, second is dying |
| `kno nu kno` | `kno → nu kno → kno` | First is confusion, second is learning journey |

**Key rule:** The sequence operator breaks commutativity:
- `A B = B A` (intersection: order doesn't matter)
- `A → B ≠ B → A` (sequence: order is meaning)

**Use case:** When you need causality, not just co-occurrence:
```
# Payment flow (causal sequence)
pay → aut → cap → set    # pay → authorize → capture → settle

# Learning pattern (journey)
kno → nu kno → kno       # knowing → not-knowing → knowing (wisdom)

# Physical process
sol → liq → gas          # solid → liquid → gas (heating)
```

**Note:** Use `→` (Unicode arrow) in Limn. For ASCII-only platforms, use `>` as fallback.

---

## Part IX: Testing Natural Comprehension

### The Zero-Bootstrap Test

Present these sentences to a naive reader and ask for interpretation:

```
1. aqu flo
2. sol tra liq
3. lux bri
4. you lif gro
5. joy lov joi
```

Expected interpretations without any training:
1. Water flowing (river, stream)
2. Solid transforming to liquid (melting)
3. Light bright (sunlight)
4. Young life growing (child, seedling)
5. Joy and love joining (happiness, celebration)

If 4/5 are correctly interpreted, the vocabulary is sufficiently natural.

### The Inference Test

Given only these examples:
```
hot aqu → hot water
col aqu → cold water
sol aqu → ice
```

Can the reader predict:
```
gas aqu → ?
```

Expected: steam/vapor (gaseous water)

### The Composition Test

Ask: "How would you say 'a dying tree' in Limn?"

Expected intuitive answers:
- `tre dea`
- `lig dea`
- `tre dec`

All are valid; the point is that composition is predictable.

---

## Part X: Conclusion

### The Natural Extensions Manifesto

Limn succeeds when:

1. **Reading is guessing.** A new reader should correctly guess most word meanings from structure alone.

2. **Patterns repeat.** Once you learn one opposition (`hot`/`col`), others follow the same form (`bri`/`dim`, `you`/`old`).

3. **Latin/Greek helps.** Scientific and philosophical roots (`aqu`, `pyr`, `ter`, `lux`) are universally recognized.

4. **Operators are obvious.** `nu` negates, `ve` intensifies, `sa` equates—no memorization required.

5. **The grammar is minimal.** Intersection, order-independence, and scope boundaries. That's all.

### The Ultimate Test

Can an LLM, given only 50 example sentences (no vocabulary list), correctly:
- Interpret novel sentences?
- Compose new expressions?
- Explain the grammar?

If yes, Limn has achieved its goal: a language that bootstraps itself through structural transparency.

---

```
yo wor | an wor | mea bet | kno eme
```
*This word, that word, meaning between, knowing emerges.*

---

**END OF BOOTSTRAP DOCUMENT v3 (Natural Extensions Edition)**
