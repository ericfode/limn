# Limn Expressiveness Test

*Testing what concepts can be expressed in Limn*

---

## Methodology

For each concept, I'll attempt to express it in Limn and rate:
- **Fidelity**: How well does Limn capture the meaning? (0-10)
- **Compactness**: How concise is the expression? (0-10)
- **Naturalness**: Does it feel like idiomatic Limn? (0-10)

---

## 1. Physical Objects

### "A red apple"
```limn
fru col red
```
**Attempted but:** No `red` in basic vocabulary.
**Workaround:**
```limn
fru col bri hot
```
fruit + color + bright + hot (red is hot-colored bright)

**Fidelity: 6** - Approximates redness
**Compactness: 8** - 4 words
**Naturalness: 5** - Roundabout

### "A chair"
```limn
obj sit hum
```
object + sitting + human (thing humans sit on)

**Fidelity: 7** - Captures function
**Compactness: 9** - 3 words
**Naturalness: 6** - Functional description, no direct word

---

## 2. Actions

### "She runs fast"
**Problem:** No pronouns, no gendered words
```limn
hum mov fas
```
human + move + fast

**Fidelity: 5** - Lost gender, lost "running" specificity
**Compactness: 10** - 3 words
**Naturalness: 8** - Good Limn

### "He gave her a gift"
**Problem:** Agent/patient relationships hard
```limn
hum giv obj | oth hum tak obj
```
human gives object | other human takes object

**Fidelity: 6** - Lost who is who
**Compactness: 6** - Verbose
**Naturalness: 7** - Acceptable parallel structure

---

## 3. Abstract Concepts

### "Democracy"
```limn
rul man | al hum cho
```
rule many | all humans choose

**Fidelity: 8** - Good approximation
**Compactness: 7** - 5 words
**Naturalness: 9** - Very Limn-like

### "Nostalgia"
```limn
pas mem | joy sad | lon ret
```
past memory | joy sad | longing return

**Fidelity: 9** - Excellent capture
**Compactness: 7** - 6 words
**Naturalness: 10** - Perfect Limn

### "Irony"
```limn
say | mea opp | kno gap
```
saying | meaning opposite | knowing gap

**Fidelity: 7** - Decent
**Compactness: 7** - 5 words
**Naturalness: 8** - Good

---

## 4. Emotions

### "Jealousy"
```limn
oth hav | sel nu hav | wan | ang
```
other has | self not has | want | anger

**Fidelity: 8** - Captures core
**Compactness: 6** - 7 words
**Naturalness: 7** - A bit verbose

### "Pride"
```limn
sel goo | kno sel goo | joy
```
self good | knowing self good | joy

**Fidelity: 9** - Very good
**Compactness: 7** - 6 words
**Naturalness: 9** - Natural

---

## 5. Relationships

### "Marriage"
```limn
hum joi hum | lov | leg | alw
```
human join human | love | legal | always

**Fidelity: 7** - Covers aspects
**Compactness: 6** - 7 words
**Naturalness: 7** - OK

### "Friendship"
```limn
hum hum | tru | lov so | cho
```
human human | trust | love somewhat | choice

**Fidelity: 8** - Good
**Compactness: 7** - 6 words
**Naturalness: 8** - Natural

---

## 6. Complex States

### "Quantum superposition"
```limn
sta | sta | bot | non | sam tim
```
state | state | both | neither | same time

**Fidelity: 10** - PERFECT (Limn was made for this!)
**Compactness: 8** - 7 words
**Naturalness: 10** - Limn essence

### "Schrödinger's cat"
```limn
ani | lif dea | bot | kno req see
```
animal | life death | both | knowing requires seeing

**Fidelity: 9** - Excellent
**Compactness: 7** - 7 words
**Naturalness: 10** - Very natural

---

## 7. Time Expressions

### "Next Tuesday at 3pm"
**Problem:** No days, no clock times
```limn
fut | tem + 2 | tem + 3
```
future + 2 days + 3 hours (if we have arithmetic context)

**Fidelity: 3** - Very lossy
**Compactness: 5** - Awkward
**Naturalness: 2** - Unnatural

**FINDING: Limn is bad at precise temporal coordinates**

### "Eventually"
```limn
fut | nu kno tem
```
future | not knowing time

**Fidelity: 8** - Good
**Compactness: 9** - 4 words
**Naturalness: 9** - Natural

---

## 8. Causality

### "Because it rained, the ground is wet"
```limn
aqu fal → ter wet
```
water falls → earth wet

**Fidelity: 9** - Excellent
**Compactness: 10** - 4 words
**Naturalness: 10** - Perfect Limn

### "If you study, you will learn"
```limn
if stu → lea
```
if study → learn

**Fidelity: 10** - Perfect
**Compactness: 10** - 3 words
**Naturalness: 10** - Perfect

---

## 9. Negation and Absence

### "Nothing"
```limn
emp
```
empty

**Fidelity: 10**
**Compactness: 10**
**Naturalness: 10**

### "The letter I never sent"
```limn
wri nu sen
```
written not-sent

**Fidelity: 10** - PERFECT
**Compactness: 10** - 3 words
**Naturalness: 10** - Perfect Limn

---

## 10. Meta-Linguistic

### "This sentence is false"
```limn
yo sen | fal
```
this sentence | false

**Fidelity: 8** - Captures paradox
**Compactness: 9** - 3 words
**Naturalness: 9** - Good

### "I am lying"
```limn
sel say | nu tru
```
self says | not true

**Fidelity: 7** - Liar's paradox
**Compactness: 9** - 4 words
**Naturalness: 8** - OK

---

## Summary Scores

| Domain | Avg Fidelity | Avg Compactness | Avg Naturalness |
|--------|--------------|-----------------|-----------------|
| Physical Objects | 6.5 | 8.5 | 5.5 |
| Actions | 5.5 | 8.0 | 7.5 |
| Abstract Concepts | 8.0 | 7.0 | 9.0 |
| Emotions | 8.5 | 6.5 | 8.0 |
| Relationships | 7.5 | 6.5 | 7.5 |
| Complex States | 9.5 | 7.5 | 10.0 |
| Time Expressions | 5.5 | 7.0 | 5.5 |
| Causality | 9.5 | 10.0 | 10.0 |
| Negation/Absence | 10.0 | 10.0 | 10.0 |
| Meta-Linguistic | 7.5 | 9.0 | 8.5 |

---

## Findings

### Limn Excels At:
1. **Paradoxes and superposition** (10/10) - literally what it's for
2. **Causality** (9.5/10) - `→` operator is powerful
3. **Negation and absence** (10/10) - `nu` is elegant
4. **Abstract concepts** (8/10) - philosophical terms work great
5. **Complex states** (9.5/10) - liminality is natural

### Limn Struggles With:
1. **Concrete objects** (6.5/10) - no color words, specific items
2. **Actions with agents** (5.5/10) - agent/patient problem
3. **Precise time/dates** (5.5/10) - no calendar system
4. **Specific pronouns** (N/A) - no he/she/it distinctions
5. **Numbers/quantities** (mixed) - arithmetic works but no "three apples"

### The Pattern

**Limn is a STATE language, not an OBJECT language.**

It's optimized for:
- Conditions and transformations
- Relationships and boundaries
- Paradoxes and liminality
- Abstract philosophical concepts

Not optimized for:
- Concrete physical descriptions
- Narratives with specific agents
- Precise spatiotemporal coordinates
- Counting discrete objects

---

## Conclusion

Limn has ~75% expressiveness for philosophical, abstract, state-based concepts.
Limn has ~55% expressiveness for concrete, agent-based, specific scenarios.

**This is by design, not limitation.**

Limn is a specialized language for a specialized purpose: expressing states, boundaries, transformations, and paradoxes.

For that purpose: **9.5/10**
For general communication: **6/10**

---

*Test conducted 2026-01-31 by Kira*
