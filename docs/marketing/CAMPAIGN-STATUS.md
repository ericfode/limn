# Limn Marketing Campaign Status

*Last updated: 2026-01-25 (Evening)*
*Status: READY FOR DEPLOYMENT - Awaiting User Decisions*

## Campaign Ready for Launch

The Limn marketing campaign is **ready for deployment**. All major deliverables have been created.

---

## Core Narrative

**Cryptoscrying the Semantic Substrate**

> "Meaning has a structure. We found a way to see it."

Limn is presented as a discovered interface to semantic space - the realm where meaning lives before words capture it. Not a lost language, not AI-generated, but a deliberate construction designed to make the invisible structure of meaning visible.

---

## Completed Deliverables

### Twitter Bot Framework
`src/twitter-bots/`
- `bot.py` - Core LimnBot class
- `personas.py` - 5 bot personas with full characterization
- `content_generator.py` - AI-powered content generation
- `conversation_scripts.py` - Pre-written conversation threads
- `scheduler.py` - Posting schedule management
- `test_bot.py` - Validation tests
- `requirements.txt` - Dependencies
- `.env.example` - Environment template

### Claude Skill
`src/claude-skill/`
- `limn.skill.json` - Skill manifest with 6 subcommands
- `vocabulary.json` - 585 words across 13 domains
- `bootstrap_prompt.md` - Interpretation guidelines
- `limn_interpreter.py` - Python interpreter

### ARG Puzzles
`docs/marketing/arg-puzzles/`
- Puzzle 01: The First Word (constraint regions)
- Puzzle 02: Between (intersection)
- Puzzle 03: Order of Things (commutativity)
- Puzzle 04: Not What You Think (negation)
- Puzzle 05: Scope boundaries

### Discord Design
`docs/marketing/discord-design.md`
- "The Key Exchange" server structure
- Entry puzzle mechanism
- Role progression system
- Community events schedule

### Content Library
- 5 bot persona content libraries
- Week 1 conversation scripts
- Standalone cryptic posts
- Response templates for human questions
- Launch teasers

### Narrative Materials
`crew/author/stories/`
- "The Scrying" - Cryptoscrying origin story
- "The Listeners"
- "The Naming"
- "The Key Exchange"
- "First Contact"

### Campaign Documentation
- `NARRATIVE-BIBLE.md` - Tone and messaging guide
- `CAMPAIGN-EXECUTION-PLAN.md` - Master plan
- `LAUNCH-CHECKLIST.md` - Pre-launch checklist
- `WEEK-1-CALENDAR.md` - Exact posts for launch week
- `QUICK-START-POSTING.md` - Fastest path to first posts
- `LIMN-CHEATSHEET.md` - Quick reference for operators
- `launch-teasers.md` - Week 1 content

---

## To Deploy

### Immediate (Before Week 1)
1. Create Twitter accounts for 5 bots
2. Get OAuth 1.0a credentials (bearer token insufficient for posting)
3. Create Discord server "The Key Exchange"
4. Set up GitHub repo `limnographers/limn` or similar
5. Generate puzzle images (use `src/puzzle-assets/create_puzzle_01.py`)

### Week 1
1. Deploy bots according to `launch-teasers.md` schedule
2. Monitor engagement
3. Begin responding to human questions

### Week 2+
1. Open Discord to solvers
2. Launch puzzles in sequence
3. Expand community

---

## Files Summary

| Area | Files | Size | Status |
|------|-------|------|--------|
| Twitter Bots | 8 Python files | 110KB+ | COMPLETE |
| Claude Skill | 5 files | 70KB+ | COMPLETE |
| ARG Puzzles | 6 docs (5 puzzles + structure) | 75KB+ | COMPLETE |
| Discord | 1 design doc | 10KB | Ready |
| Narratives | 6 stories | 30KB+ | Ready |
| Campaign Docs | 9 guides | 40KB+ | Ready |
| Content Library | 100+ posts | 30KB+ | Ready |

**Total campaign materials: ~365KB of ready-to-deploy content**

---

## Key Decision Points

### Confirmed
- Name: **Limn** (keep)
- Tone: Very mysterious/cryptic (Urbit-style)
- Narrative: Cryptoscrying, not lost language
- Priority: All workstreams in parallel

### Needs User Action
- Create Twitter accounts
- Get OAuth credentials for posting
- Create Discord server
- Decide on GitHub repo name/location

---

*sci mea | mea sci | int fnd*

*Scry meaning. Meaning scries back. Interface found.*
