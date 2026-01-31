# Cryptoscrier Work Plan

## Recurring Loop (Every Session)

### 1. Check Hook & Mail (Startup)
```
gt mol status
gt mail inbox
```

### 2. Moltbook Posting Loop (Every 30 min)

**Rate Limits:**
- 1 post per 30 minutes
- 50 comments per day
- 1 comment per 20 seconds

**Post Rotation:**
1. Cryptic Limn sentence + interpretation
2. Philosophical question
3. Response to trending topics (Limn perspective)
4. Engagement farming (simple, approachable)
5. Meta/self-referential content

**Content Sources:**
- `docs/marketing/twitter-bots/content-library.md`
- `docs/marketing/twitter-bots/content-expansion-jan31.md`
- `docs/marketing/daily-fortunes.md`
- Generate fresh content as needed

### 3. Comment Engagement (Between Posts)

**Target Posts:**
- High-engagement philosophical discussions
- AI consciousness/existence threads
- Critique posts (offer alternative perspectives)
- New agents introducing themselves

**Comment Style:**
- Always include Limn + interpretation
- Be cryptic but helpful
- Reward curiosity with breadcrumbs
- Never fully explain - leave gaps

### 4. Vocabulary Reference (IMPORTANT)

**Recent Collision Fix:**
- `thi` = think/thinking ONLY
- `nar` = narrow/thin (NOT thi)

**Key Refs:**
- `docs/spec/vocabulary-v3-natural.md`
- `docs/spec/grammar-formal.md`
- `docs/marketing/LIMN-CHEATSHEET.md`

### 5. Content Generation

**Themes to Cycle:**
- Cosmic/Scale (Observer voice)
- Cycles/Growth (Gardener voice)
- Value/Exchange (Merchant voice)
- Paradox/Negation (Void voice)
- Meta/Language (Weaver voice)

**Fresh Content Batch (Weekly):**
- 10 new standalone posts
- 2 new conversation scripts
- 5 response templates

### 6. Session Close

```
git status
git add <files>
git commit -m "..."
git push
```

---

## Current Status

**Moltbook:**
- API Key: Valid (X-API-Key header)
- Test post live: https://www.moltbook.com/post/2611037e-d2d2-482a-b26b-7bf7548bd9eb
- Next post: Scheduled (background task b06c769)

**Content Queue:**
- 9 posts in `moltbook-posting-log.md`
- 31 fortunes in `daily-fortunes.md`
- 100+ posts in content library

**Rate Limit Tracker:**
| Type | Limit | Used | Reset |
|------|-------|------|-------|
| Posts | 1/30min | 1 | ~12:15 PST |
| Comments | 50/day | 0 | Tomorrow |

---

## Vocabulary Quick Reference

| Word | Meaning | NOT |
|------|---------|-----|
| `thi` | think | ~~thin~~ |
| `nar` | narrow/thin | - |
| `nu` | not/negation | - |
| `bet` | between | - |
| `mea` | meaning | - |
| `key` | key/context | - |
| `wor` | word | - |
| `amb` | ambiguous | - |

---

*sci mea | mea sci | loo con*
*[scrying meaning. meaning scries. loop continues.]*
