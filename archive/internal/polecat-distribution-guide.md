# Polecat Work Distribution Guide

**Author:** Mei (Translator)
**Date:** 2026-02-03
**Purpose:** How to distribute HGttG translation work to polecats

---

## Translation Tasks Created

**Ready for polecat assignment:**

1. **limn-uhu1:** Arthur vs Bulldozer Scene
2. **limn-c4vf:** Ford Prefect Reveal
3. **limn-2dld:** Vogon Fleet Arrives

**Convoy:** hq-cv-2lh2s tracks all three tasks

---

## Polecat Assignment Protocol

### Option 1: Manual Sling (When Polecats Available)

```bash
# Spawn polecat for specific task
gt sling limn-uhu1 limn/polecats/alpha --create

# With context message
gt sling limn-uhu1 limn/polecats/bravo --create -m "Use v4 operators, see examples in hgttg-chapter-1-full.md"
```

### Option 2: Make Available for Claim

```bash
# Ensure task is in ready state
bd update limn-uhu1 --status=open
bd list --status=open

# Polecats check ready work
bd ready

# Polecat claims work
bd update limn-uhu1 --status=in_progress --assignee=limn/polecats/alpha
```

### Option 3: Witness Auto-Assignment

Witness can automatically spawn polecats for ready work in the queue.

---

## Handoff Protocol

When polecat completes translation:

1. **Polecat commits work:**
   ```bash
   git add hgttg-translations/chapter-1-bulldozer.md
   git commit -m "translate: Arthur vs Bulldozer scene"
   ```

2. **Polecat submits with handoff:**
   ```bash
   gt handoff -s "Translation complete" -m "
   Completed Arthur vs Bulldozer scene translation.

   Operators used:
   - pain^0.8 (hangover intensity)
   - ang^0.9 (Prosser's anger)
   - mud quality captured

   Back-translation tested: 95% fidelity

   Ready for translator validation.
   "
   ```

3. **Polecat calls gt done:**
   ```bash
   gt done
   ```
   This submits to merge queue and exits.

4. **Translator (me) receives notification**
   - Review translation
   - Validate operator usage
   - Test back-translation
   - Approve or request changes

---

## Translation Standards for Polecats

**Required reading:**
- `bootstrap-v4-compositional.md` - Operator reference
- `hgttg-v4-translations.md` - Translation patterns
- `hgttg-chapter-1-full.md` - Working examples
- `back-translation-test.md` - Validation methodology

**Quality gates:**
- Use compositional operators (v4)
- Follow documented patterns
- Maintain 90%+ fidelity
- Back-translation must work
- Document operator choices

**Operator guidelines:**
- Gradients (^) for intensifiers
- Interference (*) for absurd combinations
- Superposition (±) for paradoxes
- Keep 2-3 operators max per expression
- Test complex compositions

---

## Validation Process (Translator)

When polecat work arrives:

1. **Review translation:**
   ```bash
   git diff master..polecat/alpha/limn-uhu1
   ```

2. **Check operator usage:**
   - Appropriate operator choices?
   - Correct precedence?
   - Optimal complexity?

3. **Back-translation test:**
   - Can I reconstruct English from Limn?
   - Meaning preserved?
   - Style captured?

4. **Approve or feedback:**
   ```bash
   # If good:
   bd close limn-uhu1

   # If needs work:
   bd comments add limn-uhu1 "Feedback: Try using gradient instead of interference for X. See example Y."
   ```

---

## Current Status

**Beads created:** 3 translation tasks
**Convoy created:** hq-cv-2lh2s
**Status:** Ready for polecat assignment
**Blocked by:** Need active polecat pool or manual spawn

**Next steps:**
1. Either spawn polecats manually for these tasks
2. Or continue translator-led work and distribute later
3. Document completed translations as examples for polecats

---

## Notes

- Polecats are ephemeral (one task then nuked)
- Work survives session restarts via git worktree
- Self-cleaning: `gt done` submits and exits
- Witness monitors polecat health
- Translator validates all polecat work

---

*— Guide for distributing translation work across the polecat pool*
