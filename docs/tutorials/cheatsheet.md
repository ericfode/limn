# Limn for Beginners: Cheatsheet

> Your first 10 minutes with Limn.

---

## 10 Essential Words

| Word | Meaning | Domain | Think of... |
|------|---------|--------|-------------|
| `lux` | light | Physical | sun, brightness, clarity |
| `nox` | darkness | Physical | night, shadow, mystery |
| `lif` | alive, living | Living Things | creatures, vitality |
| `beg` | beginning | Time | dawn, birth, starting |
| `end` | ending | Time | dusk, completion, death |
| `joy` | joy | Emotion | happiness, delight |
| `sad` | sadness | Emotion | grief, sorrow |
| `lov` | love | Emotion | affection, care |
| `mov` | motion | Physical | travel, change, flow |
| `gro` | growth, learning | Abstract | deepening, development |

---

## 3 Operators You Need

| Operator | Meaning | Source | Example |
|----------|---------|--------|---------|
| `nu` | not | Latin "nullus" | `nu lux` = not-light = darkness |
| `ve` | very (intensifier) | English "very" | `ve joy` = great joy = ecstasy |
| `so` | somewhat (softener) | English "so-so" | `so sad` = a little sad = melancholy |

**Binding rule:** Operators attach to the **next word only**.
- `nu lux nox` = `(nu lux) nox` = not-light AND darkness
- `lux nu nox` = `lux (nu nox)` = light AND not-darkness

---

## 5 Key Patterns

### 1. Intersection (the core mechanic)

Put words together. The meaning is where they **overlap**.

```
lux lif = light + alive = firefly, hopeful person, blooming flower
```

### 2. Commutativity (order doesn't matter)

```
lux lif = lif lux     (identical meaning)
joy gro = gro joy     (identical meaning)
```

### 3. Pipe separator `|` (side by side)

Groups ideas into separate units, read as "beside" or "and separately":

```
joy gro | sad end = (joyful growth) beside (sad ending)
```

### 4. Negation narrows meaning

```
lif nu mov = alive + not-moving = sleeping, rooted, still
```

### 5. Sequence `→` (temporal/causal order)

Unlike intersection, order **matters** with `→`:

```
nox → lux = darkness then light = dawn
lux → nox = light then darkness = sunset
```

---

## 5 Example Sentences (with breakdowns)

**1.** `lux beg`
> light + beginning = sunrise, birth of an idea, new hope

**2.** `nox end lux beg`
> darkness ending, light beginning = dawn breaking

**3.** `ve lov nu end`
> very-love + not-ending = undying love, eternal devotion

**4.** `joy gro | sad dec`
> (joyful growth) beside (sadness decaying) = healing, getting better

**5.** `lif mov → lif nu mov → nu lif`
> alive and moving → alive but still → not alive = the arc of mortality

---

## Common Traps

| Trap | Wrong | Right |
|------|-------|-------|
| One right answer | "What does `nox lif` mean exactly?" | Multiple meanings are valid — context narrows |
| Word order | "`lux nox` differs from `nox lux`" | They're identical (commutativity) |
| English assumptions | `hot` = temperature? | `hot` = passion/urgency in Limn |
| Operator scope | `nu` negates everything after it | `nu` only binds the **next word** |

---

## What's Next?

- **30-minute tutorial:** `docs/tutorials/first-day.md`
- **Full spec:** `docs/spec/bootstrap-v3-natural.md` (v4 for operators)
- **Validate words:** `./scripts/vocab.sh check <word>`
- **Practice:** Write one Limn sentence. Interpret it. Then add a key for context.

---

```
beg yo | nu end
> this beginning | no ending
```
