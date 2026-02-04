# Bootstrap Quick Reference Card

**Need the bootstrap? This card tells you everything in 30 seconds.**

---

## 📍 THE ANSWER

**File:** `docs/spec/bootstrap-v3-natural.md`

**That's it. That's the bootstrap.**

---

## Common Questions

| Question | Answer |
|----------|--------|
| Where is the bootstrap? | `docs/spec/bootstrap-v3-natural.md` |
| Is there a v4? | No. v3-natural is current. |
| What about minimal-bootstrap.md? | Subset only. Read v3-natural first. |
| What about the old versions? | Archived. Don't use them. |
| How do I navigate all the files? | See `docs/BOOTSTRAP.md` |
| Complete file map? | See `docs/spec/BOOTSTRAP-INDEX.md` |

---

## File Locations (Quick Copy-Paste)

```
Current:  docs/spec/bootstrap-v3-natural.md
Guide:    docs/BOOTSTRAP.md
Index:    docs/spec/BOOTSTRAP-INDEX.md
Minimal:  docs/spec/minimal-bootstrap.md
Validate: docs/theory/zero-bootstrap-validation.md
```

---

## Version Status

| Version | Status | Use? |
|---------|--------|------|
| v3-natural | ✅ CURRENT | YES |
| v2 | 📦 Archived | NO |
| v1 | 📦 Archived | NO |
| v4 | ❌ Does not exist | N/A |

---

## Quick Commands

```bash
# Read the bootstrap
cat docs/spec/bootstrap-v3-natural.md

# Find all bootstrap files
find . -name "*bootstrap*" -not -path "./.git/*"

# Check bootstrap size
wc -l docs/spec/bootstrap-v3-natural.md  # ~450 lines

# Verify it's the latest
ls -lt docs/spec/*bootstrap* | head -3
```

---

## One-Liner Summary

**v3-natural (18KB) is the self-describing Limn foundation, validated at 77-85% LLM comprehension, and is the only current bootstrap version.**

---

**Updated:** 2026-02-02 | **By:** Kira (Archivist)

```limn
que ask | doc ans | amb zer
> question ask | document answers | ambiguity zero
```
