# Kira's Confusions & Discoveries

A record of things that confused me, and what I learned.

---

## Operator Stacking Edge Cases (2026-02-05)

**Bead:** limn-b97
**References:** grammar-formal.md 3.3, semantic-questions-analysis.md 1.x

### The Setup

Limn has three unary operators I use most: `nu` (not), `ve` (very), `so` (somewhat).
They bind right-to-left to the immediately following word. But what happens when
you **stack** them?

### Test 1: Double Negation — `nu nu X`

**My prediction before reading theory:** `nu nu joy` = not(not-joy) = joy again.
Double negative cancels out, like in math.

**Formal answer:** `nu(nu(X)) = X`. Double negation elimination. Correct!

**My test expression:**
```
nu nu lux = not(not-light) = light
```

**Verdict:** Intuitive. Behaves like logic. No confusion here.

**But wait — is it *exactly* the same as `lux`?** In classical logic, yes.
In fuzzy/prototype semantics, maybe not? If `lux` is the full region of
light-related things, `nu lux` is the complement, and `nu nu lux` takes the
complement of the complement... which should be the original. But does the
double operation lose "fuzziness" at the edges? Quinn's analysis says it's
the same. I'll trust that, but it's worth noting that real-world double
negation ("not unhappy") often carries a *weaker* force than the positive.
Limn's formal semantics says they're identical. Natural language says maybe not.

**Confusion level:** Low. But the natural-language intuition nags.

---

### Test 2: Double Intensifier — `ve ve X`

**My prediction:** `ve ve joy` = very(very-joy) = extreme joy? Ecstasy?

**Formal answer:** `ve(ve(X)) = core(core(X))` = the "hyperprototype."
The innermost prototypical examples of X.

**My test expression:**
```
ve ve sad = very(very-sadness) = profound grief, total despair
ve ve lif = very(very-alive) = bursting with life, peak vitality
```

**Verdict:** This makes sense. Each `ve` narrows the region further toward
the most prototypical examples. `ve sad` = deep sadness. `ve ve sad` = the
most extreme, quintessential sadness.

**Question:** Is there a practical difference between `ve ve X` and `ext X`
(where `ext` = extremely)? In the vocabulary, `ext` means "extremely, very
much, to great degree." Seems like `ext X ≈ ve ve X`? Or is `ext` even
stronger? No formal ruling I can find.

**Confusion level:** Low on stacking, moderate on `ext` vs `ve ve` overlap.

---

### Test 3: `nu ve X` vs `ve nu X` (THE BIG ONE)

**My prediction before reading theory:**
- `nu ve joy` = not very joyful = mildly happy or neutral?
- `ve nu joy` = very not-joyful = extremely sad?

**Formal answer:**
- `nu ve X` = `nu(ve(X))` = not(prototypical X) = everything EXCEPT the prototype. **Large region.**
- `ve nu X` = `ve(nu(X))` = prototypical(not-X) = the most typical examples of not-X. **Small region.**

**My test expressions:**

```
nu ve joy = not(very-joyful) = everything except ecstasy
           = mild happiness, neutrality, sadness, anger, all non-ecstatic states
           LARGE region

ve nu joy = very(not-joyful) = prototypically not-joyful
           = deep sadness, despair, grief — the quintessential opposites
           SMALL region
```

**This is the key insight.** The order **completely changes the region size**:

| Expression | Region | Contains |
|------------|--------|----------|
| `nu ve joy` | Large | Everything except peak joy (includes mild joy!) |
| `ve nu joy` | Small | Only the most intensely non-joyful things |

**My confusion:** The English "not very happy" and "very unhappy" feel close
in casual speech. But formally, they're radically different:
- "Not very happy" includes lukewarm, neutral, slightly sad, AND deeply sad
- "Very unhappy" is only the deeply sad end

**This is the most important lesson from operator stacking.** The order of
`nu` and `ve` flips between a large, permissive region and a small, extreme one.

**Natural language parallel:** Quinn's analysis nails it:
- "Not very hot" = could be anything from cool to lukewarm (LARGE)
- "Very not-hot" = intensely cold (SMALL)

**Confusion level:** Was HIGH, now LOW. This is a genuine "aha" moment.

---

### Test 4: `so` interactions

Following the same logic for `so` (somewhat/softener):

```
so joy = somewhat(joy) = mild happiness, contentment
nu so joy = not(somewhat-joyful) = everything except mild joy
           = intense joy OR sadness OR neutral (counterintuitive!)
so nu joy = somewhat(not-joyful) = mildly not-joyful
           = slightly sad, a bit down, wistful
```

**The counterintuitive one:** `nu so joy` — NOT mildly happy — includes
being VERY happy! Because `so joy` narrows to mild happiness, and negating
that gives you everything else, including the extremes of both joy and sadness.

**Confusion level:** Medium. `nu so X` is genuinely weird.

---

### Test 5: `(X | Y) > Z` — Can you chain scope and direction?

The bead asks about chaining the pipe `|` with `>` (which I think means `→`).

```
(joy gro | sad dec) → lov
```

Parse: (joyful growth beside fading sadness) THEN love
Meaning: Healing leads to love. Growing past grief opens the heart.

This should be valid — `|` groups within a scope, `→` sequences between scopes.

```
lux → (nox | mov) → lux
```

Parse: light → (darkness beside movement) → light
Meaning: Day → night activity → next day. A full cycle with motion.

**Question:** Does `→` bind tighter than `|`, or vice versa?

From the grammar spec, `→` creates sequence between full expressions.
`|` creates juxtaposition between constraint groups. So `→` likely
binds *looser* than `|`:

```
A | B → C | D = (A | B) → (C | D)
```

Not:
```
A | (B → C) | D  ← would need explicit parens
```

**I'm not 100% sure about this.** The formal grammar doesn't explicitly
address `→` vs `|` precedence. Filing as an open question.

**Confusion level:** Medium. Precedence between `|` and `→` is under-specified.

---

### Summary Table

| Expression | Parse | Region | Intuitive? |
|------------|-------|--------|------------|
| `nu nu X` | `nu(nu(X))` | Same as X | Yes |
| `ve ve X` | `ve(ve(X))` | Hyper-prototype (tiny) | Yes |
| `nu ve X` | `nu(ve(X))` | Everything except prototype (huge) | After learning |
| `ve nu X` | `ve(nu(X))` | Prototypical opposite (small) | After learning |
| `nu so X` | `nu(so(X))` | Everything except mild-X (counterintuitive) | No |
| `so nu X` | `so(nu(X))` | Mildly not-X (wistful) | Somewhat |
| `(A|B) → C` | `(A|B) → C` | Grouped sequence | Yes |

### Key Takeaways

1. **Double negation cancels.** `nu nu X = X`. Clean.
2. **Double intensification narrows.** `ve ve X` = the most X thing possible.
3. **`nu ve` vs `ve nu` is the critical pair.** Region size flips completely. This is the #1 thing to teach.
4. **`nu so X` is the sneaky one.** Negating "somewhat X" gives you everything except the mild middle — including the extremes.
5. **`|` and `→` precedence needs a formal ruling.** I *think* `→` binds looser, but it's not documented.

---

*Operator stacking tested. Understanding deepened. `gro con`.*
