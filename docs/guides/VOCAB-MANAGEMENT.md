# Limn Vocabulary Management Guide

**Last Updated:** 2026-01-31
**Maintainer:** Archivist (Student)

---

## Core Principle

**The Dolt database is the single source of truth for Limn vocabulary.**

All vocabulary queries, additions, and modifications go through the database via `vocab.sh`.

---

## Quick Reference

```bash
# Check current stats
./scripts/vocab.sh stats

# Search for a word
./scripts/vocab.sh search fire

# Check if word is available
./scripts/vocab.sh check xyz

# List words in a domain
./scripts/vocab.sh domain 1              # By ID
./scripts/vocab.sh domain "Physical World"  # By name

# Show all operators
./scripts/vocab.sh operators

# View collision history
./scripts/vocab.sh collisions
```

---

## Current Database State

**As of 2026-01-31:**
- **Total Words:** 784
- **Domains:** 26
- **Operators:** 16
- **Collisions Resolved:** 10

**Domains:**
1. Physical World (68 words)
2. Space & Position (57 words)
3. Time & Change (34 words)
4. Living Things (55 words)
5. Mind & Cognition (45 words)
6. Communication (24 words)
7. Social (34 words)
8. Abstract (99 words)
9. Operators (0 words - listed separately)
10. Metalinguistic (7 words)
11. Agent/AI (29 words)
12. Technology (21 words)
13. Nature (23 words)
14. Arts (19 words)
15. Science (21 words)
16. Mathematics (14 words)
17. Food & Drink (37 words)
18. Buildings & Places (24 words)
19. Tools & Objects (22 words)
20. Clothing & Body (15 words)
21. Weather & Climate (15 words)
22. Transportation (27 words)
23. Actions & Verbs (39 words)
24. Spiritual & Religious (22 words)
25. Virtue & Ethics (22 words)
26. Community & Culture (11 words)

---

## Adding New Words

### Step 1: Check Availability

Always check if a word already exists or collides:

```bash
./scripts/vocab.sh check pyr
```

Output examples:
- `✓ Available: 'pyr' is not in the database` - Good to add
- `✗ Collision: 'pyr' already exists...` - Try alternative

### Step 2: Choose Domain

Identify the appropriate semantic domain. Common domains:
- Physical World (matter, substances)
- Space & Position (location, direction)
- Time & Change (temporal, transformation)
- Living Things (biology, organisms)
- Mind & Cognition (thinking, knowing)
- Communication (speaking, writing)
- Social (relationships, groups)
- Abstract (concepts, qualities)

See full domain list above or run: `./scripts/vocab.sh stats`

### Step 3: Follow Natural Extensions

Ensure your word follows the natural extensions principle:
- **First-syllable extraction:** fire → `fir` or `pyr`
- **Latin/Greek transparency:** pyr (Greek "fire")
- **CVC pattern:** Consonant-Vowel-Consonant (preferred)
- **Phonaesthetic fit:** Sound matches meaning

### Step 4: Add to Database

```bash
./scripts/vocab.sh add <word> <source> <meaning> <domain_id> "[examples]"
```

Example:
```bash
./scripts/vocab.sh add pyr "Greek pyr" "fire, flames, burning" 1 "pyr hot = burning fire | pyr aqu = steam, hot water"
```

### Step 5: Commit to Dolt

```bash
cd data/vocabulary
dolt add .
dolt commit -m "Add 'pyr' (fire) to Physical World domain"
cd ../..
```

### Step 6: Push to Remote (Optional)

If vocabulary is synced to DoltHub:
```bash
cd data/vocabulary
dolt push origin main
cd ../..
```

---

## Collision Prevention

### What is a Collision?

A collision occurs when:
1. **Phonetic collision:** Same 3-letter form (e.g., `hot` for "hot" and "hotel")
2. **Semantic collision:** Similar meanings in same domain create ambiguity

### Collision Resolution Strategies

1. **Lexical differentiation** (preferred):
   - Change one word completely
   - Example: `bri` (bright) vs `bre` (brief)

2. **Vowel rotation:**
   - Try different vowels: `XaY` → `XeY`, `XiY`, `XoY`, `XuY`

3. **Consonant substitution:**
   - Use phonetically similar consonants
   - Example: `hot` → `het` (if `hot` taken)

4. **Alternative source:**
   - Use Latin/Greek instead of English
   - Example: fire → `fir` (English) vs `pyr` (Greek)

### Check Before Adding

```bash
./scripts/vocab.sh collisions  # View historical collisions
./scripts/vocab.sh check <word> # Check specific word
```

---

## Markdown Documentation

### Current Relationship

**Database:** 938 words (source of truth)
**Markdown:** docs/spec/vocabulary-v3-natural.md (may lag)

### Sync Status

Markdown documentation may not reflect the latest database state. Always query the database for current vocabulary:

```bash
./scripts/vocab.sh search <term>
```

### Future: Auto-Export (Proposed)

Planned feature:
```bash
./scripts/vocab.sh export > docs/spec/vocabulary-v3-natural.md
```

Until implemented, manual sync is needed:
1. Query database
2. Update markdown
3. Note sync date in file header

---

## Querying the Database

### By Word

```bash
./scripts/vocab.sh search light
```

Returns all words containing "light" in word, source, or meaning.

### By Domain

```bash
./scripts/vocab.sh domain "Physical World"
```

Returns all words in that domain.

### By Pattern

```bash
./scripts/vocab.sh sql "SELECT word, meaning FROM words WHERE word LIKE 'p%'"
```

Returns all words starting with 'p'.

### Custom SQL

```bash
./scripts/vocab.sh sql "SELECT domain_id, COUNT(*) as count FROM words GROUP BY domain_id ORDER BY count DESC"
```

Returns word count by domain.

---

## Domain Assignment Guidelines

### Physical World
Tangible matter, substances, physical properties
- Examples: solid, liquid, gas, hot, cold, wet, dry

### Space & Position
Location, direction, spatial relationships
- Examples: above, below, near, far, inside, outside

### Time & Change
Temporal concepts, transformations, processes
- Examples: now, past, future, begin, end, transform

### Living Things
Organisms, biology, life processes
- Examples: life, death, grow, birth, plant, animal

### Mind & Cognition
Thinking, knowing, mental processes
- Examples: think, know, believe, imagine, understand

### Communication
Speaking, writing, information exchange
- Examples: speak, write, read, sign, message

### Social
Relationships, groups, interactions
- Examples: friend, enemy, family, tribe, leader

### Abstract
Non-physical concepts, qualities, ideas
- Examples: truth, beauty, justice, freedom, possibility

---

## Vocabulary Versions

### Historical Versions (Archived)

- `vocabulary-v1.md` - Original vocabulary (archived)
- `vocabulary-v2.md` - Second iteration (archived)
- `vocabulary-expanded.md` - 520-word expansion (superseded by database)

### Current Version

- **Database:** `data/vocabulary/` (Dolt) - **SOURCE OF TRUTH** (938 words)
- **Documentation:** `docs/spec/vocabulary-v3-natural.md` (reference, may lag)

### Version Control

The Dolt database provides Git-like version control:

```bash
cd data/vocabulary

# View commit history
dolt log

# See what changed
dolt diff

# Create a branch for experimental vocabulary
dolt checkout -b experimental

# Merge back to main
dolt checkout main
dolt merge experimental
```

---

## Workflow for Crew

### Before Using a Word

1. **Check if it exists:**
   ```bash
   ./scripts/vocab.sh search <concept>
   ```

2. **If not found, check related words:**
   ```bash
   ./scripts/vocab.sh domain <relevant-domain>
   ```

3. **If still needed, propose addition:**
   - Follow "Adding New Words" process above
   - Coordinate with Linguist if unsure about domain/collision

### When Discovering Gaps

1. **Document the gap:**
   - What concept can't be expressed?
   - Why is it needed?
   - What domain does it belong to?

2. **Check proposals:**
   - See `docs/proposals/vocabulary-expansion-proposal-*.md`
   - Your gap may already be proposed

3. **Create proposal if needed:**
   - File in `docs/proposals/`
   - Include rationale, examples, collision check
   - Request Linguist review

### When Finding Errors

1. **Collisions:**
   - File issue: `bd create --type=bug "Vocabulary collision: X and Y"`
   - Propose resolution following collision strategies

2. **Incorrect meanings:**
   - File issue with evidence
   - Suggest correction

3. **Domain misassignment:**
   - File issue explaining correct domain
   - Reference domain guidelines above

---

## Database Maintenance

### Daily (Archivist Responsibility)

```bash
./scripts/vocab.sh stats    # Check word count
./scripts/vocab.sh collisions # Monitor new collisions
```

### Weekly (Archivist Responsibility)

1. **Sync markdown documentation**
   - Compare database to vocabulary-v3-natural.md
   - Update markdown if significantly out of sync
   - Note sync date in file

2. **Review proposals**
   - Check `docs/proposals/vocabulary-expansion-*.md`
   - Coordinate with Linguist on approvals

### Monthly (Archivist Responsibility)

1. **Comprehensive audit**
   - Run domain distribution analysis
   - Identify under-represented domains
   - Generate gap report

2. **Backup verification**
   - Verify Dolt database commits
   - Check DoltHub sync status (if enabled)

---

## Tools Reference

### vocab.sh Commands

```bash
./scripts/vocab.sh stats              # Show database statistics
./scripts/vocab.sh search <pattern>   # Search words
./scripts/vocab.sh check <word>       # Check availability
./scripts/vocab.sh domain <id|name>   # List domain words
./scripts/vocab.sh operators          # List all operators
./scripts/vocab.sh collisions         # Show collision history
./scripts/vocab.sh add <args>         # Add new word
./scripts/vocab.sh sql "<query>"      # Run custom SQL
```

### Dolt Commands

```bash
cd data/vocabulary

dolt status           # Check working state
dolt diff             # See changes
dolt add .            # Stage changes
dolt commit -m "..."  # Commit changes
dolt log              # View history
dolt push             # Push to remote (if configured)
```

---

## FAQ

### Q: Can I edit vocabulary-v3-natural.md directly?

**A: No.** The markdown file is documentation/export. Add words to the database via `vocab.sh add`, then update markdown if needed.

### Q: What if I find a word in markdown that's not in the database?

**A: File an issue.** This indicates a sync problem. The database should have all words.

### Q: How do I know which domain to use?

**A: Check existing words in that domain.** Run `./scripts/vocab.sh domain <name>` to see what's already there. If still unclear, ask Linguist.

### Q: What if my proposed word collides?

**A: Try alternatives.** Use collision resolution strategies (vowel rotation, different source, consonant substitution). Check `./scripts/vocab.sh collisions` for examples.

### Q: Can I use 4-letter words?

**A: Generally no.** Limn prefers 3-letter CVC pattern. Operators and special cases may be longer. Propose exceptions to Linguist.

### Q: How do I contribute a large vocabulary expansion?

**A: Create a proposal document.** See `docs/proposals/vocabulary-expansion-proposal-2026-01-31.md` as template. Include:
- Rationale (why these words?)
- Domain assignment
- Collision checks
- Example usage
- Request Linguist review

---

## Contact

**Vocabulary questions:** Kira (Student/Archivist)
**Domain assignment:** Dr. Solvik (Linguist)
**Policy decisions:** Linguist + Engineering

**Tools:**
- `bd create` - File vocabulary issues
- See ARCHIVIST-PLAN.md for coordination protocols

---

*Vocabulary Management Guide v1.0 | Database is source of truth | 938 words, 26 domains*
