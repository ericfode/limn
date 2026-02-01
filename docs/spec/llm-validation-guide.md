# LLM Validation Guide for Limn

**Purpose:** Help LLMs write valid, natural Limn
**Audience:** LLMs processing or generating Limn text

---

## Quick Validation Checklist

Before outputting Limn, verify:

1. **Words exist** - Check against vocabulary database
2. **Operators used correctly** - Right type, right position
3. **Grammar respected** - Precedence, associativity
4. **Semantics sensible** - No unintentional contradictions

---

## Database Access

**DoltHub:** https://www.dolthub.com/repositories/ericfode/limn

```sql
-- Check if word exists
SELECT word, meaning, domain_id FROM words WHERE word = 'sol';

-- Check operator
SELECT word, op_type, precedence FROM operators WHERE word = 'nu';

-- Search by meaning
SELECT word, meaning FROM words WHERE meaning LIKE '%light%';
```

---

## Strictness Levels

### Permissive (Learning/Creative)
- Unknown words: **Allowed** (flag for human review)
- Grammar errors: **Warn only**
- Use for: Poetry, experimentation, learning

### Standard (General Use)
- Unknown words: **Warn** (suggest alternatives)
- Grammar errors: **Error**
- Use for: Communication, documentation

### Strict (Formal)
- Unknown words: **Error**
- Grammar errors: **Error**
- Use for: Specifications, code, official docs

**Recommendation:** Default to Standard. Language evolves through use.

---

## Common Errors

### 1. Word Doesn't Exist
```
BAD:  blu sky bri
GOOD: Check if 'blu' exists. If not, use existing color word or flag.
```

**Fix:** Query database, suggest nearest match, or mark as neologism.

### 2. Operator Misuse
```
BAD:  hot nu col (nu in wrong position)
GOOD: nu hot col (nu modifies next term)
```

**Fix:** Unary operators precede their argument.

### 3. Unintentional Contradiction
```
WARN: hot col (contradiction - liminal zone)
```

**Note:** Contradictions ARE valid in Limn (they denote liminal zones). Only warn if context suggests error.

### 4. Ambiguous Quantifier Scope
```
AMBIGUOUS: al hum lov (all humans love? or humans love all?)
CLEARER:   al hum | lov (topic: all humans, comment: love)
           hum | al lov (topic: humans, comment: love all)
```

**Fix:** Use scope operator `|` to disambiguate.

---

## Operator Reference

| Operator | Type | Position | Example | Meaning |
|----------|------|----------|---------|---------|
| nu | unary | prefix | nu hot | not hot |
| ve | unary | prefix | ve bri | very bright |
| so | unary | prefix | so hot | somewhat hot |
| te | unary | prefix | te hum | is human? |
| we | unary | prefix | we mov | move! (imperative) |
| al | quantifier | prefix | al hum | all humans |
| ex | quantifier | prefix | ex sol | some solid |
| on | quantifier | prefix | on hum | exactly one human |
| yo | reference | prefix | yo aqu | this water |
| an | reference | prefix | an pek | that mountain |
| sa | reference | prefix | sa riv | same river |
| eq | comparator | infix | X eq Y | X equals Y |
| ma | comparator | infix | X ma Y | X greater than Y |
| mi | comparator | infix | X mi Y | X less than Y |
| seq/→ | sequence | infix | hot → col | hot leads to cold |
| sco/\| | scope | infix | A \| B | topic A, comment B |

---

## Generating New Limn

When an LLM needs a word that doesn't exist:

1. **Check database first** - It might exist with different meaning
2. **Follow LWDA** - CVC pattern, phonaesthetic alignment
3. **Flag for human review** - Don't add to official vocabulary
4. **Use circumlocution** - Describe with existing words

Example:
```
NEED: word for "purple"
CHECK: Does 'pur' exist? No.
OPTION 1: Flag 'pur' as proposed neologism
OPTION 2: Use 'red blu lim' (red-blue liminal = purple)
```

---

## Validation API (Future)

```
POST /validate
{
  "text": "nu ve bri lux",
  "strictness": "standard"
}

Response:
{
  "valid": true,
  "words": [
    {"word": "nu", "type": "operator", "valid": true},
    {"word": "ve", "type": "operator", "valid": true},
    {"word": "bri", "type": "word", "valid": true, "meaning": "bright"},
    {"word": "lux", "type": "word", "valid": true, "meaning": "light"}
  ],
  "parse": "nu(ve(bri ∩ lux))",
  "warnings": []
}
```

---

## Best Practices for LLMs

1. **Query before generating** - Check words exist
2. **Use operators sparingly** - Natural Limn is mostly content words
3. **Embrace ambiguity** - It's a feature, not a bug
4. **Respect commutativity** - `A B = B A` (except with operators)
5. **Flag uncertainty** - Mark generated words as provisional

---

*val gui = llm hel | wri cor lim | err avo*
*(validation guide = LLM help | write correct Limn | error avoidance)*
