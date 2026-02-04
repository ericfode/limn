# Bootstrap Quick Reference Card

**Need the bootstrap? This card tells you everything in 30 seconds.**

---

## 📍 THE ANSWER

**File:** `docs/spec/bootstrap-v4-compositional.md`

**That's it. That's the bootstrap.**

---

## Common Questions

| Question | Answer |
|----------|--------|
| Where is the bootstrap? | `docs/spec/bootstrap-v4-compositional.md` |
| Is there a v4? | No. v3-natural is current. |
| What about minimal-bootstrap.md? | Subset only. Read v3-natural first. |
| What about the old versions? | Archived. Don't use them. |
| How do I navigate all the files? | See `docs/BOOTSTRAP.md` |
| Complete file map? | See `docs/spec/BOOTSTRAP-INDEX.md` |

---

## File Locations (Quick Copy-Paste)

```
Current:  docs/spec/bootstrap-v4-compositional.md
Guide:    docs/BOOTSTRAP.md
Index:    docs/spec/BOOTSTRAP-INDEX.md
Minimal:  docs/spec/minimal-bootstrap.md
Validate: docs/theory/zero-bootstrap-validation.md
```

---

## Version Status

| Version | Status | Use? |
|---------|--------|------|
| v4-compositional | ✅ CURRENT | YES |
| v3-natural | 📋 Superseded | For basics only |
| v2 | 📦 Archived | NO |
| v1 | 📦 Archived | NO |

---

## Quick Commands

```bash
# Read the bootstrap
cat docs/spec/bootstrap-v4-compositional.md

# Find all bootstrap files
find . -name "*bootstrap*" -not -path "./.git/*"

# Check bootstrap size
wc -l docs/spec/bootstrap-v4-compositional.md  # ~450 lines

# Verify it's the latest
ls -lt docs/spec/*bootstrap* | head -3
```

---

## One-Liner Summary

**v4-compositional (22KB) is the self-describing Limn foundation with 6 compositional operators enabling 30,039 validated expressions from 1,076 core words.**

---

**Updated:** 2026-02-02 | **By:** Kira (Archivist)

```limn
que ask | doc ans | amb zer
> question ask | document answers | ambiguity zero
```
