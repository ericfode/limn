# Limn Word Derivation Algorithm (LWDA)

**Author:** Dr. Maren Solvik
**Date:** 2026-01-31
**Status:** Active Development
**Purpose:** Systematic method for creating optimal Limn words

---

## 1. Design Principles

Unlike Lojban's algorithmic approach (weighted averages from 6 source languages), Limn prioritizes:

1. **Zero-Bootstrap Learnability** - Word should be guessable
2. **Phonaesthetic Alignment** - Sound should match meaning
3. **Collision Avoidance** - No duplicate CVC patterns
4. **Semantic Density** - One word, tight constraint region

---

## 2. The LWDA Pipeline

```
CONCEPT → [Source Selection] → [Form Generation] → [Collision Check] → [Phonaesthetic Score] → WORD
```

### Step 1: Source Selection (Priority Order)

| Priority | Source | When to Use | Example |
|----------|--------|-------------|---------|
| 1 | English whole-word | Word ≤3 letters | hot, gas, big |
| 2 | English truncation | Common word, clear first syllable | mov←move, flo←flow |
| 3 | Latin/Greek root | Technical/scientific | lux←lux, pyr←pyr |
| 4 | Phonaesthetic creation | Abstract concepts | (sound-symbolic) |
| 5 | Systematic derivation | When all else collides | (algorithmic) |

### Step 2: Form Generation Rules

**For English source:**
```
IF len(word) <= 3 AND is_CVC:
    USE word (hot, big, sad)
ELIF first_syllable is CVC:
    USE first_syllable (mov, flo, bri)
ELIF first_syllable is CV:
    ADD consonant from word (mo+v → mov)
ELSE:
    TRUNCATE to CVC pattern
```

**For Latin/Greek:**
```
USE root form if CVC (lux, pyr, nox)
ELSE truncate to CVC
```

### Step 3: Collision Check

```bash
./scripts/vocab.sh check <proposed_word>
```

If collision detected:
1. Try alternative truncation (different consonant)
2. Try synonym source
3. Try phonaesthetic variant
4. Use systematic suffix (-a, -i, -o variants)

### Step 4: Phonaesthetic Scoring

Score 0-10 based on English sound-meaning associations:

| Sound Pattern | Meaning Association | Score Boost |
|---------------|---------------------|-------------|
| /gl-/ | light, shine | +2 if light-related |
| /fl-/ | flow, movement | +2 if motion-related |
| /sl-/ | slippery, negative | +2 if negative/smooth |
| /sm-/ | smooth, small | +2 if smooth/small |
| /br-/ | break, sharp | +2 if sharp/broken |
| /sn-/ | nose, contempt | +2 if smell/negative |
| /-ash/ | sudden impact | +2 if impact-related |
| /-ump/ | heavy, round | +2 if heavy/round |
| short /i/ | small, quick | +1 if small/fast |
| long /o/ | large, round | +1 if large/slow |

**Minimum score for acceptance: 5**

---

## 3. Domain Expansion Targets

| Domain | Current | Target | Gap | Priority |
|--------|---------|--------|-----|----------|
| Physical World | 50 | 80 | 30 | HIGH |
| Space & Position | 28 | 40 | 12 | MEDIUM |
| Time & Change | 23 | 35 | 12 | MEDIUM |
| Living Things | 28 | 50 | 22 | HIGH |
| Mind & Cognition | 26 | 45 | 19 | HIGH |
| Communication | 14 | 30 | 16 | MEDIUM |
| Social | 18 | 35 | 17 | MEDIUM |
| Abstract | 52 | 70 | 18 | LOW |
| Technology | 0 | 30 | 30 | HIGH |
| Nature | 0 | 25 | 25 | MEDIUM |
| Arts | 0 | 20 | 20 | LOW |

**Total: 239 → 460 words (+221)**

---

## 4. Systematic Derivation (Fallback)

When natural sources collide, use systematic patterns:

### 4.1 Vowel Rotation
```
Base: XaY (taken)
Try:  XeY, XiY, XoY, XuY
```

### 4.2 Consonant Substitution
```
Voiced ↔ Unvoiced: b↔p, d↔t, g↔k, v↔f, z↔s
Nasal variants: m↔n
Liquid variants: l↔r
```

### 4.3 Semantic Clustering
Related concepts share initial consonant:
```
Temperature: hot, hea (heat), hum (humid)
Water: wat, wav, wak (wake/splash)
Light: lux, lum, lit
```

---

## 5. Implementation

**Note:** Limn uses Prolog exclusively (engineer-approved). Implementation in Prolog forthcoming.

The derivation algorithm can be implemented as a Prolog predicate:

```prolog
% derive_word(+Concept, -Word, -Score)
% Generates optimal Limn word for concept
derive_word(Concept, Word, Score) :-
    get_source_word(Concept, Source),
    generate_cvc(Source, Form),
    check_and_resolve_collision(Form, Concept, ResolvedForm),
    phonaesthetic_score(ResolvedForm, Concept, Score),
    (Score >= 5 -> Word = ResolvedForm ; find_better_form(Concept, Word)).
```

---

## 6. Quality Gates

Before adding any word:

- [ ] Collision check passed (`vocab.sh check`)
- [ ] Phonaesthetic score ≥ 5
- [ ] Zero-bootstrap test: Can naive reader guess meaning?
- [ ] Domain assignment clear
- [ ] No offensive associations in major languages

---

## 7. Batch Expansion Process

```bash
# 1. Identify concept domain
# (manual brainstorming of needed concepts)

# 2. Derive candidate words using algorithm
# (apply LWDA pipeline: source selection → CVC generation → collision check → scoring)

# 3. Review and score
# (manual review of candidates)

# 4. Add approved words
./scripts/vocab.sh add <word> <source> <meaning> <domain_id> "<examples>"

# 5. Commit to Dolt
cd data/vocabulary && dolt add . && dolt commit -m "Add N words to domain X"
```

---

*wor der = sys cre | zer boo | col avo*
*(word derivation = systematic creation | zero bootstrap | collision avoidance)*
