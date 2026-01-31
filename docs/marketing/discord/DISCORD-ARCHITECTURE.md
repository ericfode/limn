# LIMN DISCORD SERVER ARCHITECTURE
## "The Key Exchange"

**Server Name:** The Key Exchange
**Tagline:** `amb | key | cle` — *ambiguity, key, clarity*
**Status:** READY FOR DEPLOYMENT

---

## SERVER STRUCTURE

### Categories & Channels

```
📜 THE GATES
├── #gate-zero-threshold      [Public - Entry puzzle]
├── #gate-one-gathering       [@First-Ring - Interpretation challenge]
├── #gate-two-library         [@Second-Ring - Pattern recognition]
├── #gate-three-translation   [@Third-Ring - Deep interpretation]
├── #gate-four-mirror         [@Fourth-Ring - Self-reference]
├── #gate-five-coordinates    [@Fifth-Ring - Real-world integration]
├── #gate-six-transmission    [@Sixth-Ring - Cryptographic]
└── #gate-seven-ouroboros     [@The-Completed - The infinite game]

🏛️ THE HALLS (Main Community)
├── #welcome                  [Read-only, introduction]
├── #rules                    [Read-only, conduct]
├── #introductions           [Introduce yourself]
├── #general                 [Main chat]
├── #off-topic               [Non-limn discussion]
└── #bot-commands            [Bot interactions]

📚 LEARNING
├── #bootstrap               [Learning resources, pinned vocab]
├── #first-words             [Beginner practice]
├── #interpretation-practice [@First-Ring+]
├── #composition-practice    [@Second-Ring+]
├── #advanced-limn           [@Third-Ring+]
└── #questions               [Ask about Limn]

🗣️ SPEAKING
├── #open-sentences          [Post ambiguous Limn, no interpretation]
├── #keyed-conversations     [Conversations with stated keys]
├── #poetry                  [Limn poetry]
├── #stories                 [Limn micro-fiction]
└── #translation-challenges  [Translate English to Limn]

🔬 RESEARCH
├── #theory                  [Linguistic/mathematical theory]
├── #limn-pl                 [Programming language discussion]
├── #experiments             [Testing Limn properties]
└── #proposals               [Propose vocabulary additions]

📜 LORE
├── #lore-fragments          [@Second-Ring+ - ARG content]
├── #the-seven               [@Third-Ring+ - Founder lore]
├── #origin-documents        [@Fifth-Ring+ - Historical prototypes]
└── #inner-sanctum           [@Sixth-Ring+ - Deep lore]

🤖 BOTS
├── #limn-interpreter        [/limn commands]
├── #bot-feed                [Twitter bot mirror]
└── #bot-conversations       [Cross-post bot interactions]

🔧 ADMIN
├── #architect-discussion    [@Architect - Planning]
├── #content-review          [@Architect - Review submissions]
├── #arg-operations          [@Architect - ARG management]
└── #logs                    [@Architect - Moderation logs]
```

---

## ROLES (Hierarchy)

```
STAFF ROLES
├── @Architect              [Full admin, ARG control]
├── @Keeper                 [Mod powers, community management]
└── @Scribe                 [Content creation, no mod]

PROGRESSION ROLES (ARG-based)
├── @The-Completed          [Gate 7 complete - Full access]
├── @Sixth-Ring (Covenant)  [Gate 6 complete]
├── @Fifth-Ring (Keyed)     [Gate 5 complete]
├── @Fourth-Ring (Mirrors)  [Gate 4 complete]
├── @Third-Ring             [Gate 3 complete]
├── @Second-Ring            [Gate 2 complete]
├── @First-Ring             [Gate 1 complete]
└── @Seeker                 [Passed Gate 0, basic access]

SPECIAL ROLES
├── @Speaker                [Fluent in Limn, peer-nominated]
├── @Theorist               [Contributes to theory]
├── @Poet                   [Creates notable Limn poetry]
├── @Solver                 [First to solve major puzzles]
└── @Voice                  [Twitter bot persona access]
```

---

## BOTS

### 1. LimnBot (Primary Utility Bot)
**Prefix:** `/limn` or `!limn`

**Commands:**
```
/limn interpret <sentence>              - Interpret a Limn sentence
/limn interpret <sentence> --key="x"    - Interpret with specific key
/limn compose "<english>"               - Create Limn from concept
/limn validate <sentence>               - Check if valid Limn
/limn vocab <word>                      - Look up a Limn word
/limn random                            - Generate random sentence
/limn poetry <theme>                    - Generate Limn poem
/limn help                              - Show help
```

### 2. GatekeeperBot (ARG Management)
**Prefix:** `!gate`

**Commands:**
```
!gate status                            - Show your current gate
!gate submit <answer>                   - Submit puzzle answer
!gate hint                              - Get a hint (costs community points)
!gate leaderboard                       - Show progression stats
```

**Auto-behaviors:**
- Monitors #gate-* channels for correct answers
- Awards roles automatically
- Posts cryptic messages on timers
- DMs progression rewards

### 3. LoreBot (Content Delivery)
**Prefix:** `!lore`

**Commands:**
```
!lore fragment <number>                 - Request a lore fragment
!lore random                            - Random accessible fragment
!lore timeline                          - Show known timeline
```

**Auto-behaviors:**
- Posts new fragments on schedule
- Responds to certain Limn phrases with lore
- Tracks which fragments each user has seen

### 4. MirrorBot (Experimental)
**Prefix:** None - responds to Limn input directly

**Behavior:**
- In #interpretation-practice: Provides alternative interpretations
- In #open-sentences: Asks clarifying questions in Limn
- In #poetry: Responds with complementary Limn poems

---

## ENTRY FLOW

### Step 1: Discord Invite from Gate 0
User solves Gate 0 puzzle (GitHub), gets invite link

### Step 2: Onboarding Channel
```
#welcome

═══════════════════════════════════════
        WELCOME TO THE KEY EXCHANGE
═══════════════════════════════════════

amb | key | cle
ambiguity | key | clarity

You've found the gathering place of those who speak Limn.

Limn is a language where:
• Words define regions of meaning
• Sentences are intersections
• Keys collapse ambiguity to clarity

You begin as a @Seeker.
To progress, solve the gates.

Start here → #gate-one-gathering

Resources:
📚 #bootstrap - Learn the basics
❓ #questions - Ask anything
🗣️ #first-words - Practice speaking

The gates await.

nu cle amb | cle key
not-clear yet ambiguous | clarity through key
═══════════════════════════════════════
```

### Step 3: Gate One (Community Challenge)
In #gate-one-gathering:
```
GATE ONE: The Gathering

To join the First Ring, demonstrate understanding.

Interpret this sentence through THREE different keys:

    gro exp bri far

Post your three interpretations.
When 3 members react with ✓, you advance.

Example format:
---
Key: astronomy
gro exp bri far = supernova, expanding light reaching across space

Key: gardening
gro exp bri far = seedlings growing toward distant sunlight

Key: economics
gro exp bri far = market expansion, bright prospects ahead
---
```

---

## COMMUNITY MECHANICS

### Interpretation Voting
In practice channels, users can react:
- ✓ (valid interpretation)
- 💡 (insightful)
- 🔄 (alternative reading)
- ❓ (needs clarification)

### Limn of the Day
Daily bot post in #general:
```
═══ LIMN OF THE DAY ═══

hot col bet mov

Post your interpretation below.
Tag your key.
Different keys welcome.

═══════════════════════
```

### Weekly Challenges
```
WEEKLY CHALLENGE #17

This week's task: TRANSLATION

Translate this English paragraph to Limn:

"The old tree fell in the forest.
 No one heard it.
 Did it make a sound?"

Post your translation.
Vote on others.
Winner gets @Poet role for a week.
```

### Collaboration Board
#proposals channel for vocabulary additions:
```
PROPOSAL: Add "ech" (echo/reflection/return)

Semantic region: reflected sound, mirror image, repetition,
                 recursion, response, karma

Justification: Currently no word for reflection/echo concept.
              "mir" (mirror) is visual; need auditory equivalent.
              Also useful for self-reference, recursion in Limn-PL.

Overlaps with: ret (return), cyc (cycle), cop (copy)
Distinction: ech implies received response, not just repetition

Usage examples:
- say ech (speak and hear echo)
- mea ech (meaning reflects back)
- ech nu sam (echo is not same)

VOTE: 👍 support  👎 oppose  🤔 needs discussion
```

---

## MODERATION

### Auto-mod Rules
1. No spoilers for ARG puzzles outside designated channels
2. No direct answers to gate challenges (hints only)
3. Standard content policies (no harassment, spam, etc.)
4. Limn-only mode can be enabled in practice channels

### Spoiler Policy
```
ARG SPOILER POLICY

Helping is encouraged. Solving for others is not.

ALLOWED:
✓ Hints about approach
✓ Confirming if someone is "warm" or "cold"
✓ Explaining concepts needed to solve
✓ Offering to discuss in DMs

NOT ALLOWED:
✗ Posting direct solutions
✗ Completing puzzles for others
✗ Screenshotting solution steps
✗ Linking to external solution guides

Violations result in temporary mute.
Repeat violations result in gate regression.
```

### Limn-Only Mode
Certain channels can be set to Limn-only:
- Bot deletes non-Limn messages
- Encourages immersive practice
- Interpretations allowed in [brackets]

---

## SCHEDULED CONTENT

### Daily
- 09:00 UTC: Limn of the Day
- 14:00 UTC: Bot Feed update (cross-post from Twitter)
- 21:00 UTC: Lore fragment (if available for viewer's tier)

### Weekly
- Monday: New interpretation challenge
- Wednesday: Community spotlight (best content of week)
- Friday: Translation challenge
- Sunday: Theory discussion thread

### Monthly
- ARG progression milestone check
- New lore chapter release
- Vocabulary proposal voting
- Community statistics

---

## ONBOARDING MESSAGES

### Welcome DM (Sent on Join)
```
Welcome, seeker.

You've found the entrance to The Key Exchange,
where those who speak Limn gather.

Limn is a constructed language where meaning
exists in superposition until context collapses it.

To begin:
1. Read #welcome and #rules
2. Visit #gate-one-gathering to earn your first role
3. Use #questions if you're confused
4. Try #first-words for beginner practice

The language will feel strange at first.
That's intentional.
Fluency comes with patience.

nu cle now | cle fut | amb pre
[not-clear now | clear future | ambiguous present]

Good luck.

— The Keyed
```

### First Ring Welcome (After Gate 1)
```
═══ FIRST RING ACHIEVED ═══

You've proven basic understanding.
You are no longer a Seeker.
You are @First-Ring.

New access granted:
📚 #interpretation-practice
🗣️ #keyed-conversations
🔬 Advanced learning channels

The Second Ring awaits in #lore-fragments.
The pattern is not obvious.
That is the point.

gro kno | bet = the pat
[growing knowing | between = the path]
```

---

## TECHNICAL REQUIREMENTS

### Bot Hosting
- Node.js Discord.js bot
- PostgreSQL for user state
- Redis for caching
- API connection to /limn skill

### Integrations
- Twitter API (mirror bot posts)
- GitHub webhooks (for repo updates)
- Claude API (for /limn commands)

### Metrics to Track
- Daily active users
- Gate progression rates
- Most active channels
- Popular Limn words used
- Interpretation quality scores

---

## LAUNCH CHECKLIST

- [ ] Server created with all channels
- [ ] Roles configured with permissions
- [ ] LimnBot deployed and tested
- [ ] GatekeeperBot deployed with Gate 1 logic
- [ ] LoreBot deployed with initial fragments
- [ ] Welcome messages configured
- [ ] Moderation rules enabled
- [ ] Initial @Architect team added
- [ ] Twitter bot feed connected
- [ ] Gate 0 puzzle live on GitHub
- [ ] Invite link generated for Gate 0 reward

---

*sel oth joi | gro kno | com cre*
*[self and other join | growing knowing | community creating]*

---
