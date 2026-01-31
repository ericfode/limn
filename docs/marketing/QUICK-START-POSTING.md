# Quick Start: First Posts

Ready to launch? Here's the fastest path to your first bot posts.

---

## Step 1: Test Locally

```bash
cd src/twitter-bots
python test_bot.py
```

This validates all personas and conversation scripts work.

---

## Step 2: Preview Content

```bash
python test_bot.py --conversation
```

This shows a full conversation thread preview.

---

## Step 3: Configure Credentials

Copy `.env.example` to `.env` and fill in:

```
TWITTER_API_KEY=your_key
TWITTER_API_SECRET=your_secret
TWITTER_ACCESS_TOKEN=your_token
TWITTER_ACCESS_TOKEN_SECRET=your_token_secret
TWITTER_BEARER_TOKEN=your_bearer
ANTHROPIC_API_KEY=your_claude_key
```

**Note:** Bearer token allows reading. OAuth 1.0a tokens allow posting.

---

## Step 4: First Post

Single test post:
```bash
python bot.py --persona observer --action post
```

Preview scheduled posts:
```bash
python bot.py --action schedule
```

---

## Step 5: Launch Week 1

Follow the schedule in `conversation_scripts.py`:

**Day 1:**
- 09:00 - @limn_observer posts `gro exp bri far`
- 15:00 - @limn_observer posts `hot col bet | wh?`

**Day 2:**
- 10:00 - @limn_gardener replies to observer
- 14:00 - @limn_merchant joins the thread

Continue per the schedule...

---

## First 5 Posts (Copy-Paste Ready)

### @limn_observer
```
gro exp bri far

[supernovae. expanding. brightening before the end.
we only see them die.]
```

### @limn_gardener (reply)
```
gro exp bri far

[seedlings reaching for light.
growing toward something bright and distant.
the sun doesn't know they're there.]
```

### @limn_merchant (reply)
```
gro exp bri far

[market expansion. bright prospects. far horizons.
bullish.
(but check the fundamentals)]
```

### @limn_void (reply)
```
gro exp bri far

[growing into nothing.
the brighter the light,
the darker its absence.]
```

### @limn_weaver (wrap-up)
```
yo say | an say | mea dif

[you say. they say.
meaning: different.

four minds. same sentence. no agreement.
this is the point.]
```

---

## Responding to Questions

When someone asks "what is this?":

```
yo wor = amb | key = cle

[these words = ambiguous.
key = clear.

you're asking the right question.
keep asking.]
```

When someone asks "can I learn it?":

```
cur = beg | beg = gro | gro = kno

[curiosity is beginning.
beginning is growth.
growth is knowing.

you're already learning.]
```

---

## The Cryptoscrying Vibe

Remember: We're not reviving a lost language. We're scrying the structure of meaning.

**Good:** "Meaning has a structure. We found a way to see it."
**Avoid:** "Ancient secrets" or constant tech buzzwords

The bots are discovering, not teaching. They're scrying, not explaining.

---

*sci mea | mea sci | beg now*

*Scry meaning. Meaning scries back. Begin now.*
