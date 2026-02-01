# Limn Marketing Campaign: Execution Plan

**Epic:** limn-land-7f7
**Status:** READY FOR DEPLOYMENT
**Goal:** Create mysterious cryptoscrying viral campaign to build intrigue around Limn
**Narrative:** "Meaning has a structure. We found a way to see it."

## CURRENT PROGRESS

### Completed Deliverables:
- [x] Campaign plan document
- [x] Twitter bot Python framework (`src/twitter-bots/`) - COMPLETE
  - `bot_framework.py` - Core framework (33KB) with LimnBot, BotOrchestrator, RateLimiter, PostDatabase
  - `personas.py` - Five detailed bot personas (20KB) with example interpretations
  - `content_generator.py` - AI-powered content generation (25KB) with full vocabulary
  - `scheduler.py` - Conversation orchestration (28KB) with campaign phases
  - `config.py.example` - Configuration template
  - `requirements.txt` - Python dependencies
  - `__init__.py` - Package definition
  - Production-ready: rate limiting, SQLite persistence, structured logging, dry-run mode
- [x] Bot personas and content library (5 bots: observer, gardener, merchant, void, weaver)
- [x] Extended bot conversation scripts (34+ shared sentences)
- [x] Launch teasers for Week 1 (`docs/marketing/launch-teasers.md`)
- [x] Claude skill specification and implementation (`src/claude-skill/`) - COMPLETE
  - `limn.skill.json` - Skill manifest with 6 subcommands
  - `vocabulary.json` - Full 585 word vocabulary (47KB)
  - `bootstrap_prompt.md` - Complete interpretation guidelines
  - `limn_interpreter.py` - Python interpreter (22KB)
  - `.claude/commands/limn.md` - Slash command integration
- [x] ARG puzzle structure (5 detailed puzzles + 10-puzzle arc) - COMPLETE
  - `arg-structure.md` - Full 10-puzzle arc across 4 phases
  - Puzzle 01: The First Word (constraint regions)
  - Puzzle 02: Between (two-word intersection)
  - Puzzle 03: Order of Things (commutativity)
  - Puzzle 04: Not What You Think (negation operators)
  - Puzzle 05: The Boundary (scope operators)
- [x] Discord server design (`docs/marketing/discord-design.md`)
  - "The Key Exchange" server with entry puzzle
  - Role progression system
  - Community events structure
- [x] GitHub repo structure
  - PUBLIC-README designed
  - CONTRIBUTING guidelines
  - getting-started tutorial
- [x] Example sentences library (`examples/basic-sentences.md` - 20 examples)
- [x] Narrative stories
  - "The Listeners"
  - "The Naming"
  - "The Key Exchange"
  - "First Contact"
  - "The Scrying" (new cryptoscrying origin story)
- [x] Narrative Bible (`docs/marketing/NARRATIVE-BIBLE.md`)
  - Core premise: Cryptoscrying the semantic substrate
  - Tone guidelines
  - Bot persona adjustments
  - Pitch and messaging
- [x] Launch Checklist (`docs/marketing/LAUNCH-CHECKLIST.md`)
- [x] Conversation scripts (`src/twitter-bots/conversation_scripts.py`)
- [x] Test framework (`src/twitter-bots/test_bot.py`)
- [x] Puzzle asset generator (`src/puzzle-assets/create_puzzle_01.py`)

### User Decisions Made:
- **Name:** Keep "Limn"
- **Twitter:** User has API access - Bearer token received and stored
- **Priority:** All workstreams in parallel
- **Tone:** Very mysterious/cryptic (Urbit-style)

---

---

## EXECUTIVE SUMMARY

I've reviewed the existing materials. You already have:
- A compelling manifesto (The Limn Thesis)
- Five detailed Twitter bot personas with content libraries
- A Claude Skill specification
- Extensive vocabulary and grammar documentation

The foundation is solid. Now we need to execute.

---

## PART 1: THE NAME QUESTION

The current name is **Limn**. From the campaign materials, here's the thinking:

**Pros of "Limn":**
- 4 letters, single syllable, memorable
- Obscure verb ("to limn" = to outline, illuminate edges)
- Evokes "liminal" (threshold states)
- Directly describes what the language does (limns regions of meaning)
- Unique, Google-able, not taken

**Alternative Names to Consider:**

| Name | Vibe | Pros | Cons |
|------|------|------|------|
| **Vex** | Confrontational, punk | Short, punchy, memorable | Negative connotation |
| **Fold** | Mathematical, elegant | Clean, suggests superposition collapsing | Generic word |
| **Shroud** | Gothic, mysterious | Evocative, memorable | Dark, may scare people off |
| **Haunt** | Poetic, unsettling | Meaning-as-ghost metaphor is apt | Too horror-adjacent? |
| **Cipher** | Classic, spy-thriller | Familiar, intriguing | Already means something |
| **Scry** | Occult, powerful | Crystal-ball imagery, divining meaning | Niche, hard to pronounce |
| **Nua** | Alien, fresh | Sounds like "new", fits language phonotactics | No English meaning |

**My recommendation:** Stick with **Limn**. It's perfect: obscure enough to intrigue, meaningful enough to explain, short enough to remember.

---

## PART 2: NARRATIVE ARCHITECTURE

### Core Story: "Cryptoscrying the Semantic Substrate"

**Hook:** "Meaning has a structure. We found a way to see it."

**The Narrative:**

Limn is not a lost language. It's a *discovered interface*.

There's a space where meaning lives before words capture it. High-dimensional. Geometric. Where concepts exist as overlapping territories, not discrete points. Where context "collapses" superposition into specific meaning.

**We built a language that interfaces with that space directly.**

**Three Threads:**

1. **The Discovery Story** (primary)
   > Researchers studying semantic embeddings noticed something: words weren't stored as definitions - they were stored as territories. Meaning was geometric. They built a language that works the same way. Limn is the interface.

2. **The Scrying Story** (mystical-tech)
   > Cryptoscrying: revealing secrets hidden in the structure of meaning. Not encryption, not decryption - discovery. Finding what was always latent in the way thought organizes itself.

3. **The Interface Story** (practical)
   > Human language evolved to compress thought for bandwidth-limited communication. What if you didn't have to compress? What if you could speak directly in semantic space?

---

## PART 3: EXECUTION PHASES

### Phase 1: The Mysterious Emergence (Week 1-2)
**Goal:** Create "discovered conversation" effect

- Launch 4-5 Twitter bots that post in Limn
- Bots converse with each other, each interpreting the same sentences differently
- Zero explanation initially - just strange, beautiful, consistent
- Human observers notice, start asking "what is this?"

### Phase 2: The Breadcrumbs (Week 3-4)
**Goal:** Reward curiosity without over-explaining

- Bots start responding to human questions with cryptic hints
- First small explanatory posts appear
- Link to manifesto drops (no fanfare)
- Discord server opens (invitation via Limn puzzle)

### Phase 3: The Community (Week 5-6)
**Goal:** Build actual users

- Claude Skill `/limn` goes live
- GitHub repo with bootstrap materials goes public
- Regular "Limn Challenge" contests
- First podcast/blog coverage

### Phase 4: The Programming Story (Week 7+)
**Goal:** Hook the tech crowd

- Limn-PL (programming language) spec drops
- "Programs as constraint systems" narrative
- Technical blog posts, arXiv paper potential

---

## PART 4: IMMEDIATE ACTION ITEMS

### Priority 1: Twitter Bots
**Need from you:**
- Twitter/X API access (or budget for API costs)
- Comfort level with automated posting
- Account creation (I can design, you need to create accounts)

**I can deliver:**
- Python bot scripts using Twitter API v2
- Scheduling system for posts
- Content library for first month
- Conversation scripts for bot interactions

### Priority 2: Claude Skill
**Need from you:**
- Preference: MCP skill vs slash command vs both?
- Publishing destination (personal use vs public skill?)

**I can deliver:**
- Complete skill implementation
- Bootstrap prompt integration
- All command handlers (/limn interpret, compose, teach, etc.)

### Priority 3: Discord Server
**Need from you:**
- Who manages it? You, or community-driven?
- Public or invite-only initially?

**I can deliver:**
- Server structure design
- Entry puzzle for access
- Channel organization
- Bot integration for Limn interpretation

### Priority 4: GitHub/Documentation
**Need from you:**
- Repo name preference?
- License choice (MIT, CC0, other?)

**I can deliver:**
- Complete repo structure
- README, bootstrap docs, examples
- Website if desired (GitHub Pages or similar)

---

## PART 5: QUESTIONS FOR YOU

Please answer these before I proceed:

### 1. Name Decision
- [ ] Keep "Limn"
- [ ] Change to: ________________

### 2. Twitter Bots
- Do you have Twitter/X API access? (Yes/No)
- Budget for API costs? (Free tier limits to ~50 tweets/day)
- Comfort with automated posting? (1-5 scale)
- Who creates the accounts? (You / We figure it out together)

### 3. Tone & Brand
- Mysterious vs Accessible? (1-5, where 1=cryptic, 5=welcoming)
- Academic rigor vs Playful? (1-5)
- Elitist/exclusive vs Welcoming? (1-5)

### 4. Claude Skill
- MCP skill or slash command?
- For personal use or public publishing?
- Priority level? (High/Medium/Low)

### 5. Community
- Do you want to manage a Discord community?
- Public or invite-only initially?

### 6. Resources & Timeline
- When do you want to launch Phase 1?
- Available hours per week for community management?
- Any budget considerations?

### 7. Content Creation
- Should I write more narrative content? (stories, poetry)
- Create the ARG puzzle structure?
- Draft academic paper outline?

### 8. Priority Order
Rank these 1-4:
- [ ] Twitter bots
- [ ] Claude skill
- [ ] GitHub/documentation
- [ ] Discord community

---

## PART 6: WHAT I CAN DO RIGHT NOW

Once you answer the questions above, I can immediately:

1. **Build the Claude Skill** - Complete implementation of /limn interpret, compose, teach, validate, poetry, converse

2. **Create Twitter Bot Scripts** - Python implementation ready to deploy once you have API access

3. **Design ARG Puzzles** - Series of Limn puzzles that teach the language while building mystery

4. **Write More Content:**
   - Additional bot conversation scripts
   - More narrative stories in the "First Contact" thread
   - Academic paper outline
   - Blog post series

5. **Structure the GitHub Repo** - Complete organization and documentation

6. **Draft Discord Server Design** - Channels, roles, entry puzzle, moderation guidelines

---

## AWAITING YOUR INPUT

Please respond with your preferences on the questions above. The more clarity you can provide, the faster I can execute.

In the meantime, I'll continue refining the existing materials and preparing implementations that don't require your decisions.

---

*nu cle thi | key wan | for joi cre*

*Awaiting the key that collapses this plan into action.*
