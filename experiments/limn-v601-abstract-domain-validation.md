# Validation: Abstract Domain Vocabulary (131 words)

**Bead:** limn-v601
**Validator:** limn/polecats/now
**Date:** 2026-02-04

## Validation Results

```
PASS age | meaning confirmed — "agent" from "agent", natural truncation
PASS all | meaning confirmed — "all, every" from "all", full word
PASS alp | meaning confirmed — "alpha, first" from "alpha", natural truncation
PASS alt | meaning confirmed — "alternatively, or else" from "alternative", natural truncation
PASS amb | meaning confirmed — "many meanings" from "ambiguous", natural truncation
WARN anc | issue: semantic overlap with `old` (ancient vs old both denote age)
PASS and | meaning confirmed — conjunction from "and", full word
PASS any | meaning confirmed — "any, free choice quantifier" from "any", full word
PASS as | meaning confirmed — "as, in the manner of" from "as", full word
PASS ate | meaning confirmed — "make, cause to be" suffix from "-ate"
PASS att | meaning confirmed — "attention" from "attend", natural truncation
PASS avg | meaning confirmed — "average" from "average", abbreviation
PASS bad | meaning confirmed — "bad" from "bad", full word
PASS bea | meaning confirmed — "beautiful" from "beautiful", natural truncation
PASS bec | meaning confirmed — "because" from "because", natural truncation
WARN bou | issue: semantic overlap with `lim` (boundary region) and `edg` (edge, boundary) — three boundary words
WARN bri | issue: source field lists "sun, lamp, star" (physical examples) instead of English source "bright"
PASS cau | meaning confirmed — "cause, reason why" from "cause", natural truncation
PASS cho | meaning confirmed — "choice, option" from "choice", natural truncation
PASS cla | meaning confirmed — "conflicting constraints" from "clash", natural truncation
WARN cnt | issue: semantic overlap with `cla` — "contradicts/opposes" vs "conflicting constraints" cover same territory
WARN col | issue: source field lists "ice, winter, shade" (physical examples) instead of English source "cold"
WARN cot | issue: semantic overlap with `may` and `pos` — contingent/possible/may form a cluster of near-synonyms
PASS ctx | meaning confirmed — "context" from "context", abbreviation
WARN cut | issue: source field lists "blade, knife" (physical examples) instead of English source "cut"
WARN dar | issue: semantic overlap with `nox` — "dark, ignorance" vs "darkness-related" cover similar ground
PASS dee | meaning confirmed — "deep, profound, thorough" from "deep", natural truncation
PASS def | meaning confirmed — "definition, specify" from "define", natural truncation
PASS det | meaning confirmed — "single meaning" from "determined", natural truncation
PASS dif | meaning confirmed — "different" from "different", natural truncation
WARN div | issue: semantic overlap with `dif` — "diverse, varied" vs "different, distinct" are near-synonyms
PASS doo | meaning confirmed — "door, opportunity" from "door", natural truncation
WARN due | issue: semantic overlap with `bec` — "due to, because of" vs "because" are near-synonyms
PASS eac | meaning confirmed — "each, distributive quantifier" from "each", distinct from `eve` (universal)
PASS eff | meaning confirmed — "effect, result, consequence" from "effect", natural truncation
WARN els | issue: semantic overlap with `alt` — "else, otherwise, alternative" vs "alternatively, or else"
PASS emb | meaning confirmed — "embedding" from "embed", natural truncation
WARN emp | issue: semantic overlap with `nil`, `nul`, `zer` — four words in the nothing/zero/empty space
WARN ena | issue: semantic overlap with `let` — "enable, make possible, allow" vs "let, allow, permission"
PASS eps | meaning confirmed — "epsilon, small" from "epsilon", distinct mathematical concept from `sma`
PASS eta | meaning confirmed — "eta, efficiency" from "eta", full word
PASS eve | meaning confirmed — "every, universal distributive" from "every", distinct from `eac` (distributive)
WARN ext | issue: collision with operator `ext` (metacognitive) — word and operator share same form
PASS fak | meaning confirmed — "fake" from "fake", full word
WARN fal | issue: source "falling object" is misleading for abstract meaning "failure, decline, sunset"
PASS fat | meaning confirmed — "fate, destiny" from "fate", full word
PASS few | meaning confirmed — "few, some" from "few", full word
WARN fir | issue: semantic overlap with `alp` — "first, initial" vs "alpha, first" are near-synonyms
PASS fra | meaning confirmed — "fragment, piece" from "fragment", natural truncation
FAIL frn | problem: source listed as "hgttg" (Hitchhiker's Guide reference, not an etymology); also duplicates `fro` ("in front, forward")
PASS ful | meaning confirmed — "full, complete" from "full", full word
PASS gam | meaning confirmed — "gamma, third" from "gamma", natural truncation
PASS goo | meaning confirmed — "good" from "good", full word
WARN gro | issue: source field lists "plant growth, tumor" (physical examples) instead of English source "grow"
PASS gvn | meaning confirmed — "given that, assuming, premise" from "given", distinct from `if` (condition)
PASS hal | meaning confirmed — "half, partial" from "half", distinct from `prt` (epistemic partial)
WARN hot | issue: source field lists "fire, fever, sun" (physical examples) instead of English source "hot"
PASS if | meaning confirmed — "condition" from "if", full word
FAIL iff | problem: meaning says "if-then, conditional" but source is "if-and-only-if" — these are different logical concepts (implication vs biconditional); meaning contradicts source
WARN ify | issue: semantic overlap with `ate` and `ize` — three suffixes all meaning "make/cause"
WARN ina | issue: semantic overlap with `wro` — "inaccurate, imprecise" vs "wrong, incorrect, mistaken"
PASS inf | meaning confirmed — "inference" from "infer", natural truncation
PASS ism | meaning confirmed — "doctrine, belief system" suffix from "-ism"
PASS ity | meaning confirmed — "quality, state, condition" suffix from "-ity"
WARN ize | issue: semantic overlap with `ate` and `ify` — three suffixes all meaning "make/cause"
FAIL kee | problem: duplicate with `kei` — both derive from "key" with overlapping meanings ("cipher key, unlock" vs "key, solution, opener")
FAIL kei | problem: duplicate with `kee` — both derive from "key" with overlapping meanings ("key, solution, opener" vs "cipher key, unlock")
PASS lad | meaning confirmed — "ladder, ascent" from "ladder", distinct object from `ris` (action)
PASS lam | meaning confirmed — "lambda, function" from "lambda", natural truncation
PASS lat | meaning confirmed — "latent" from "latent", natural truncation
PASS les | meaning confirmed — "less" from "less", full word
PASS let | meaning confirmed — "let, allow, permission" from "let", full word
PASS lim | meaning confirmed — "boundary region" from "liminal", natural truncation
WARN liq | issue: source field lists "water, oil, blood" (physical examples) instead of English source "liquid"
PASS man | meaning confirmed — "many, numerous" from "many", natural truncation
PASS max | meaning confirmed — "maximum" from "maximum", natural truncation
PASS may | meaning confirmed — "possible" from "may", full word
WARN mig | issue: semantic overlap with `may` — "might, weak possibility" vs "possible"
PASS min | meaning confirmed — "minimum, least" from "min", abbreviation
PASS mod | meaning confirmed — "model" from "model", natural truncation
PASS mor | meaning confirmed — "more" from "more", natural truncation
FAIL mul | problem: duplicate concept with `man` — both mean "many" ("many, multiple" vs "many, numerous")
PASS mus | meaning confirmed — "necessary" from "must", natural truncation
PASS nan | meaning confirmed — "NaN, undefined" from "nan", distinct technical concept (undefined computation result vs null/absence)
WARN neg | issue: semantic overlap with `not` — "negate, deny, opposite of" vs "negation"
PASS new | meaning confirmed — "new" from "new", full word
WARN nil | issue: semantic overlap with `zer`, `nul`, `emp` — four words in nothing/zero/empty space
PASS non | meaning confirmed — "none, no, negative quantifier" from "none", natural truncation
PASS not | meaning confirmed — "negation" from "not", full word
WARN nov | issue: semantic overlap with `new` — "novel, new" vs "new"
WARN nul | issue: semantic overlap with `nil`, `zer`, `emp` — four words in nothing/zero/empty space
PASS ome | meaning confirmed — "omega, last" from "omega", natural truncation
PASS one | meaning confirmed — "one, single" from "one", full word
PASS or | meaning confirmed — "disjunction" from "or", full word
FAIL ore | problem: duplicate with `or` — both mean disjunction from same source "or"
PASS ous | meaning confirmed — "full of, characterized by" suffix from "-ous"
PASS phi | meaning confirmed — "phi, golden" from "phi", full word
WARN poo | issue: semantic overlap with `bad` and `pov` — "poor, low quality" overlaps both
WARN pos | issue: semantic overlap with `may` — "possible, could be, possibility" vs "possible"
WARN pov | issue: semantic overlap with `poo` — "poverty, poor" vs "poor, low quality"
PASS prm | meaning confirmed — "prompt" from "prompt", abbreviation
PASS prt | meaning confirmed — "some determined" from "partial", abbreviation
PASS psi | meaning confirmed — "psi, mind" from "psi", full word
PASS qui | meaning confirmed — "quite, moderately, fairly" from "quite", natural truncation
PASS rea | meaning confirmed — "real" from "real", natural truncation
PASS rho | meaning confirmed — "rho, density" from "rho", full word
PASS roa | meaning confirmed — "road, way forward" from "road", natural truncation
PASS saf | meaning confirmed — "safe, secure" from "safe", natural truncation
PASS sam | meaning confirmed — "same" from "same", natural truncation
WARN sev | issue: arbitrary number — why is "seven" in vocabulary when two through six are absent?
PASS sig | meaning confirmed — "sigma, sum" from "sigma", natural truncation
PASS sim | meaning confirmed — "easy, simple" from "simple", natural truncation
WARN so | issue: collision with operator `so` (weakener/somewhat) — word "therefore" and operator share same form
WARN sol | issue: source field lists "rock, ice, bone" (physical examples) instead of English source "solid"
PASS som | meaning confirmed — "some, existential quantifier" from "some", natural truncation
PASS tau | meaning confirmed — "tau, time" from "tau", full word
PASS tha | meaning confirmed — "than, compared to" from "than", natural truncation
WARN the | issue: highly confusable with English definite article "the" — source "theta" is technically correct but practically problematic for English speakers
PASS tok | meaning confirmed — "token" from "token", natural truncation
WARN typ | issue: semantic overlap with `avg` — "typical, normal" vs "average, middle, typical, normal"
PASS ugl | meaning confirmed — "ugly" from "ugly", natural truncation
WARN unc | issue: semantic overlap with `dou` — "uncertain, unsure" vs "doubting, uncertainty"
PASS uni | meaning confirmed — "unique" from "unique", natural truncation
WARN vag | issue: semantic overlap with `amb` — "vague, unclear" vs "ambiguous, many-meaning, unclear"
WARN val | issue: source ambiguity — `val` could be read as "value", "valid", or "valley"; truncation is ambiguous
PASS vit | meaning confirmed — "vital, essential, critically important" from "vital"
PASS wei | meaning confirmed — "weight" from "weight", natural truncation
PASS whi | meaning confirmed — "which, interrogative selector" from "which", natural truncation
PASS wou | meaning confirmed — "would, counterfactual conditional" from "would", natural truncation
PASS wro | meaning confirmed — "wrong, incorrect, mistaken" from "wrong", natural truncation
PASS zer | meaning confirmed — "zero, none" from "zero", natural truncation
```

## Notes on Systematic Patterns

### Polysemy Source Field Pattern
Seven words (bri, col, cut, fal, gro, hot, liq, sol) list physical manifestations in the source field rather than the English source word. This is consistent — these are intentional polysemy entries showing abstract meanings, with the source column documenting the physical→abstract metaphor chain. The pattern is systematic but inconsistent with how other words use the source column (English etymology).

### Semantic Cluster: Nothing/Zero/Empty
Four words occupy the nothing/empty space: `zer` (zero/number), `nil` (nothing), `nul` (null/void), `emp` (empty set). While these have distinct technical meanings in computer science, four words for a ~1000-word vocabulary is dense. Consider whether all four are needed.

### Semantic Cluster: Possibility/Modality
Four words express possibility: `may` (possible), `mig` (might/weak possibility), `pos` (possible/capability), `cot` (contingent). These have graduated modal strength, but `pos` and `may` are near-duplicates.

### Suffix Redundancy
Three suffixes mean "make/cause": `ate` (-ate), `ify` (-ify), `ize` (-ize). In English these serve different registers and collocations, but for a constrained vocabulary this may be excessive.

=== val sum ===
pas: 87 | war: 38 | fal: 6
> pass: 87 | warn: 38 | fail: 6

fal lis:
> fail list:
- frn: source "hgttg" is nonsensical (not an etymology); also duplicates `fro` ("in front")
- iff: meaning says "if-then, conditional" but source says "if-and-only-if" — these are different logical concepts
- kee: duplicate with `kei` — both from "key" with overlapping meanings
- kei: duplicate with `kee` — both from "key" with overlapping meanings
- mul: duplicate concept with `man` — both mean "many"
- ore: duplicate with `or` — both mean disjunction from same source

war lis:
> warn list:
- anc: semantic overlap with `old`
- bou: semantic overlap with `lim` and `edg` (three boundary words)
- bri: source field shows physical examples, not English source "bright"
- cnt: semantic overlap with `cla`
- col: source field shows physical examples, not English source "cold"
- cot: semantic overlap with `may`/`pos`
- cut: source field shows physical examples, not English source "cut"
- dar: semantic overlap with `nox`
- div: semantic overlap with `dif`
- due: semantic overlap with `bec`
- els: semantic overlap with `alt`
- emp: semantic overlap with `nil`, `nul`, `zer`
- ena: semantic overlap with `let`
- ext: collision with operator `ext` (metacognitive)
- fal: source "falling object" misleading for abstract meaning
- fir: semantic overlap with `alp`
- gro: source field shows physical examples, not English source "grow"
- hot: source field shows physical examples, not English source "hot"
- ify: semantic overlap with `ate` and `ize`
- ina: semantic overlap with `wro`
- ize: semantic overlap with `ate` and `ify`
- liq: source field shows physical examples, not English source "liquid"
- mig: semantic overlap with `may`
- neg: semantic overlap with `not`
- nil: semantic overlap with `zer`, `nul`, `emp`
- nov: semantic overlap with `new`
- nul: semantic overlap with `nil`, `zer`, `emp`
- poo: semantic overlap with `bad` and `pov`
- pos: semantic overlap with `may`
- pov: semantic overlap with `poo`
- sev: arbitrary number selection — seven without two through six
- so: collision with operator `so` (weakener)
- sol: source field shows physical examples, not English source "solid"
- the: confusable with English definite article "the"
- typ: semantic overlap with `avg`
- unc: semantic overlap with `dou`
- vag: semantic overlap with `amb`
- val: source ambiguity — could be "value", "valid", or "valley"
