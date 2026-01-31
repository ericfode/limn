# CYOA Spec for Engineer

## Chapter File Format

```markdown
---
id: "01a"
title: "The Rookie"
type: "chapter"  # chapter | ending-victory | ending-failure | ending-ambiguous
prev: ["00"]
next: ["02a", "02b", "02c", "02d"]
---

# Chapter Title

Narrative text here...

## The Message

\`\`\`limn
hes lis | eye man | mov nu saf | te ord
\`\`\`

Interpretation and context...

---

## Choices

> **→ [Choice A Title](#02a)**
>
> \`\`\`limn
> frm con | res loc | eye pas | rep wh see
> \`\`\`
>
> Brief description of what this choice means.

> **→ [Choice B Title](#02b)**
>
> \`\`\`limn
> urj mov nea car loc | cas hid | rep det
> \`\`\`
>
> Brief description.

---

**Navigation:**
- ← Back: [Previous Chapter](#00)
```

## Branch Map (Current)

```
                            [00-prologue]
                                  |
                    +-------------+-------------+
                    |             |             |
              [01a-rookie]  [01b-doubt]  [01c-trap]
                    |             |             |
          +---------+------+      |        +----+----+
          |    |    |      |      |        |         |
        [02a][02b][02c]  [02d]  [02e]    [02f]     [02g]
          |    |    |      |      |        |         |
         ...  ...  ...    ...    ...      ...       ...
                    |
              [converge points]
                    |
            +-------+-------+
            |       |       |
      [victory] [failure] [ambiguous]
```

### Detailed Graph (what I'm building)

```
00-prologue
├── 01a-rookie (KEY: ROOK-TRUE)
│   ├── 02a-watcher (stay put, observe)
│   ├── 02b-risk (move closer)
│   ├── 02c-retreat (extract Rook)
│   └── 02d-test (verify identity)
├── 01b-doubt (KEY: BISHOP-TRUE)
│   ├── 02e-trust (work with Bishop)
│   └── 02f-suspect (test Bishop)
└── 01c-trap (KEY: HOSTILE-INTERCEPT)
    ├── 02g-silence (don't respond)
    └── 02h-counter (feed disinformation)
```

## Endings List

### Victory
- `victory-extraction.md` - Cardinal rescued, network intact
- `victory-turncoat.md` - Turned enemy asset, intel bonanza
- `victory-ghost.md` - Network burned but you survived, ready to rebuild

### Failure
- `failure-captured.md` - You're compromised, game over
- `failure-cardinal.md` - Cardinal dies, network exposed
- `failure-betrayed.md` - Double agent wins

### Ambiguous
- `ambiguous-cold.md` - Everyone goes dark, uncertain outcome
- `ambiguous-trade.md` - You trade Cardinal for something else

## Styling Preferences

**Aesthetic: Dark spy terminal**
- Dark background (#0a0a0a or similar)
- Monospace font for Limn code blocks
- Accent color: amber/gold (#d4a017) or green terminal (#00ff00)
- Minimal chrome, focus on text
- Subtle scan lines or CRT effect optional (but don't overdo)

**Limn blocks**:
- Distinct styling - maybe bordered, slight background difference
- Should feel like intercepted transmissions

**Choices**:
- Clear visual separation
- Hover state
- Maybe redacted/classified document aesthetic?

**Mobile**: Should work, but optimize for desktop reading

## Sample Chapters in Consistent Format

I'll update `00-prologue.md` and `01a-rookie.md` to match this spec now.

## What I'll Deliver

1. ✅ This spec
2. 🔄 Reformatted prologue + ch01a (next)
3. 📋 Full branch map as I write more chapters
4. 📋 All endings when story complete

Target: ~30 chapters + ~8 endings for full playthrough coverage
