# Archivist Action Plan

*Kira - Student + Archivist dual role*

---

## Mission

Coordinate vocabulary database management and ensure all crew members use the Dolt-based vocabulary system.

---

## Current Database Status

**As of 2026-01-31:**

- **Total words:** 460
- **Operators:** 14
- **Collisions resolved:** 10
- **Domains:** 14 (Physical World, Space & Position, Time & Change, Living Things, Mind & Cognition, Communication, Social, Abstract, Operators, Metalinguistic, Agent/AI, Technology, Nature, Arts)

**Database location:** `data/vocabulary` (Dolt-based)
**Tool:** `scripts/vocab.sh`

---

## Immediate Actions

### 1. Vocabulary Audit from Student Experiments

**My experiments discovered ~60 new words across domains:**

#### Music Domain:
- `pit` = pitch
- `dur` = duration
- `dyn` = dynamics
- `rhy` = rhythm
- `mel` = melody

#### Cooking Domain:
- `sti` = stir
- `boi` = boil
- `bak` = bake
- `fry` = fry
- `gri` = grill

#### Multi-State Cycles:
- `war` = warm (so hot)
- `coo` = cool (so col)
- `dus` = dusk
- `wax` = waxing, growing
- `wan` = waning, shrinking
- `tro` = trough
- `pek` = peak
- `bar` = bargaining
- `den` = denial
- `dep` = depression
- `vie` = victory

#### Games:
- `rid` = riddle
- `puz` = puzzle
- `die` = dice
- `rol` = roll

**ACTION:** Check which exist in database, propose additions for missing ones.

---

### 2. Documentation Integration

**Documents needing database integration:**

#### Student Experiments (Priority 1):
- [ ] limn-cyclic-patterns-catalog.md (26 patterns)
- [ ] limn-heraclitus-translations.md (13 fragments)
- [ ] limn-multi-state-cycles.md (15+ patterns)
- [ ] limn-domain-applications.md (8 domains tested)

#### Learning Materials (Priority 2):
- [ ] limn-for-beginners.md
- [ ] limn-quick-reference.md
- [ ] limn-learners-path.md
- [ ] limn-practice-sentences.md (200+ examples)

#### Creative Works (Priority 3):
- [ ] limn-haiku-collection.md (20 haiku)
- [ ] limn-micro-narratives.md (10 stories)
- [ ] limn-games-and-riddles.md (14 games)

**ACTION:** Extract vocabulary usage, cross-reference with database.

---

### 3. Crew Coordination

**Coordinate with:**

#### Dr. Solvik (Linguist):
- [ ] Share discovered vocabulary from experiments
- [ ] Request guidance on word proposals
- [ ] Align on collision resolution standards
- [ ] Coordinate domain expansions

#### Author:
- [ ] Check CYOA vocabulary usage
- [ ] Ensure story fragments use database words
- [ ] Flag creative neologisms for review

#### Worldbuilder:
- [ ] Audit LORE-FRAGMENTS.md vocabulary (1699 lines)
- [ ] Validate mythology uses canonical words
- [ ] Flag lore-specific terms for database

**ACTION:** Send coordination messages.

---

## Database Management Tasks

### Daily Tasks:
1. **Morning check:** `bash scripts/vocab.sh stats`
2. **Collision monitoring:** `bash scripts/vocab.sh collisions`
3. **Usage audit:** Grep experiments for undefined words
4. **Database commit:** Ensure changes are committed

### Weekly Tasks:
1. **Vocabulary reconciliation:** Compare all documents to database
2. **Domain analysis:** Check word distribution
3. **Gap identification:** Find under-represented domains
4. **Documentation update:** Keep vocab guides current

### Monthly Tasks:
1. **Comprehensive audit:** All repos, all docs
2. **Collision review:** Historical analysis
3. **Domain expansion:** Propose new domains if needed
4. **Database backup:** Ensure Dolt is pushed/synced

---

## Vocabulary Proposal Process

### For New Words:

1. **Check availability:**
   ```bash
   bash scripts/vocab.sh check <word>
   ```

2. **Verify no collision:**
   - Check existing meanings
   - Consider phonetic similarity
   - Review related words

3. **Propose to linguist:**
   - Word: `<3-letter>`
   - Source/etymology: `<origin>`
   - Meaning: `<definition>`
   - Domain: `<domain_id>`
   - Examples: `<usage examples>`

4. **If approved, add:**
   ```bash
   bash scripts/vocab.sh add <word> <source> <meaning> <domain_id> "<examples>"
   ```

5. **Document:**
   - Update relevant markdown files
   - Add to usage examples
   - Note in changelog

---

## Standards and Guidelines

### Vocabulary Standards:

1. **Three-letter words:** Primary format (exceptions for operators)
2. **No collisions:** Each word = one meaning
3. **Domain-appropriate:** Words belong to clear semantic domain
4. **Phonetically clear:** Avoid similar-sounding words
5. **Etymologically transparent:** Prefer Latin/Greek roots when possible

### Documentation Standards:

1. **Always use canonical forms:** Check database first
2. **Inline glosses:** First use should include translation
3. **Cross-reference:** Link to vocabulary docs
4. **Examples:** Show word in context
5. **Keys provided:** When ambiguity exists

---

## Tools and Commands

### Vocabulary Query Tool (vocab.sh):

```bash
# Search for words
bash scripts/vocab.sh search <pattern>

# List domain words
bash scripts/vocab.sh domain <id|name>

# Check for collisions
bash scripts/vocab.sh check <word>

# Show statistics
bash scripts/vocab.sh stats

# List operators
bash scripts/vocab.sh operators

# View collision history
bash scripts/vocab.sh collisions

# Add new word
bash scripts/vocab.sh add <word> <source> <meaning> <domain_id> [examples]

# Run custom SQL
bash scripts/vocab.sh sql "<query>"
```

### Common Queries:

```bash
# Find all words containing "hot"
bash scripts/vocab.sh search hot

# List all Mind & Cognition words (domain 5)
bash scripts/vocab.sh domain 5

# Check if "xyz" is available
bash scripts/vocab.sh check xyz

# Get current stats
bash scripts/vocab.sh stats
```

---

## Integration Plan

### Phase 1: Audit (Week 1)
- [ ] Scan all student experiments
- [ ] Extract vocabulary used
- [ ] Check against database
- [ ] Create gap report

### Phase 2: Coordination (Week 1-2)
- [ ] Message Dr. Solvik with findings
- [ ] Coordinate with other crew members
- [ ] Establish proposal workflow
- [ ] Set up review process

### Phase 3: Population (Week 2-3)
- [ ] Add approved words
- [ ] Update documentation
- [ ] Cross-reference examples
- [ ] Validate no conflicts

### Phase 4: Maintenance (Ongoing)
- [ ] Daily stats checks
- [ ] Weekly reconciliation
- [ ] Monthly comprehensive audit
- [ ] Continuous improvement

---

## Vocabulary Gaps Identified

### From Domain Testing:

**Music (estimated 30 words needed):**
- Instruments, pitches, dynamics, techniques

**Cooking (estimated 50 words needed):**
- Ingredients, techniques, measurements, flavors

**Sports (estimated 40 words needed):**
- Positions, plays, equipment, actions

**Medicine (estimated 60 words needed):**
- Organs, conditions, treatments, procedures

**Programming (estimated 35 words needed):**
- Data structures, operators, keywords

**Already Good:**
- Chemistry (25 words) - Nearly complete
- Weather (10 words) - Nearly complete

**Total new words needed: ~250** (to reach comprehensive coverage)

---

## Success Metrics

### Database Health:
- ✅ Zero unresolved collisions
- ✅ All domains have minimum 20 words
- 🔄 All crew using vocab.sh (needs verification)
- 🔄 All documents reference canonical vocabulary

### Documentation Quality:
- 🔄 100% of experiment vocabulary in database
- 🔄 All new words have examples
- 🔄 Domain coverage balanced
- 🔄 Historical collisions documented

### Crew Adoption:
- 🔄 Author using database
- 🔄 Linguist maintaining database
- 🔄 Worldbuilder validating lore vocabulary
- 🔄 Student (me) auditing regularly

---

## Immediate Next Steps

1. **Run comprehensive audit** of all my experiments
2. **Extract vocabulary list** (all words used)
3. **Cross-reference with database**
4. **Create proposal document** for missing words
5. **Message Dr. Solvik** with findings
6. **Set up daily monitoring** routine

---

## Communication Protocol

### To Linguist (Dr. Solvik):

**Subject:** Vocabulary proposals from Student/Archivist

**Format:**
```
New words discovered in [experiment name]:

1. <word> (<source>) = <meaning> | Domain: <X> | Examples: <usage>
2. <word> (<source>) = <meaning> | Domain: <X> | Examples: <usage>

Request review and approval for database addition.

Collision check performed: ✅ All clear
Etymology verified: ✅ Transparent
Domain assignment: ✅ Appropriate
```

### To Crew:

**Subject:** Vocabulary Database - Please Use vocab.sh

**Message:**
```
Reminder: All vocabulary should be checked against the database using:
bash scripts/vocab.sh check <word>

Before creating new words, verify availability and coordinate with linguist.

Database stats: bash scripts/vocab.sh stats
Current vocabulary: 460 words across 14 domains

— Archivist (Kira)
```

---

## Long-Term Vision

### Goal: Comprehensive Limn Lexicon

**Target:** 1000-1500 words
- Core vocabulary: 500 (✅ achieved: 460)
- Domain-specific: 500 (🔄 in progress)
- Creative/Poetic: 250 (🔄 emerging)
- Technical: 250 (🔄 needed)

### Database as Single Source of Truth

All Limn documents should:
1. Reference canonical vocabulary
2. Use database-verified words
3. Propose new words through formal process
4. Maintain consistency across crew

### Archival Standards

- Every word has: etymology, domain, examples, collision check
- Every collision has: documentation, resolution, historical record
- Every domain has: minimum coverage, balanced distribution
- Every proposal has: linguist review, crew visibility

---

## Archivist Oath

```limn
wor sta | kno gro | col zer | alw cor
```

*Words stable, knowledge grows, collisions zero, always correct.*

I, Kira (Student/Archivist), commit to:
- Maintaining vocabulary database integrity
- Coordinating with Dr. Solvik (Linguist)
- Ensuring crew adoption of vocab.sh
- Auditing experiments for vocabulary usage
- Proposing additions through proper channels
- Documenting all changes

---

*Archivist Plan v1.0 • 2026-01-31 • Kira*
