# Agent/AI Domain Vocabulary Validation

**Bead:** limn-find
**Validator:** limn/polecats/was
**Date:** 2026-02-04
**Words validated:** 125
**Cross-referenced against:** vocabulary-v3-natural.md (spec), vocabulary.json (429 words)

---

## Validation Results

```
PASS ack | meaning confirmed — "acknowledge" → ack is standard (TCP ACK). Fits agent protocol domain.
PASS act | meaning confirmed — "action" → act is first 3 letters. Core agent concept.
PASS ada | meaning confirmed — "adapt" → ada is first 3 letters. Fits adaptive systems.
PASS agg | meaning confirmed — "aggregate" → agg is first 3 letters. Data processing concept.
WARN agt | issue: potential duplicate with existing `age` (agent) in spec Domain 11. Different code, same concept.
WARN ale | issue: "ale" naturally suggests beer/ale to English speakers, not "aleatoric uncertainty". Very technical source word.
WARN alg | issue: "alg" strongly suggests "algorithm" to most readers, not "aligned with intent". Source ambiguity.
PASS alv | meaning confirmed — "alive" → alv is recognizable truncation. Fits agent status domain.
PASS asm | meaning confirmed — "assume" → asm is recognizable. Fits reasoning domain.
PASS awt | meaning confirmed — "await" → awt is recognizable truncation. Fits async domain.
FAIL blk | problem: collision with existing `blk` (black/dark) in colors domain. "Blocked" and "black" are unrelated meanings.
PASS bnd | meaning confirmed — "bend" → bnd is recognizable. HVM primitive context clear.
FAIL brc | problem: duplicate concept with existing `bra` (branch, fork) in physical/structures. Same source word "branch".
PASS clb | meaning confirmed — "calibrated" → clb is recognizable truncation. Fits LLM quality domain.
FAIL clr | problem: duplicate concept with existing `cle` (clear) in vocabulary. Same meaning, different code creates confusion.
PASS cmp | meaning confirmed — "complete" → cmp is standard abbreviation. Fits lifecycle domain.
PASS cmt | meaning confirmed — "commit" → cmt is recognizable. Fits version control/decision domain.
PASS coh | meaning confirmed — "coherent" → coh is first 3 letters. Fits LLM success mode.
PASS cst | meaning confirmed — "construct" → cst is recognizable. HVM primitive.
PASS cvg | meaning confirmed — "converge" → cvg is recognizable truncation. Fits LLM behavior.
PASS dlg | meaning confirmed — "delegate" → dlg is standard abbreviation. Fits agent hierarchy.
FAIL dns | problem: duplicate concept with existing `den` (dense, compact) in physical properties. Same meaning, different code.
WARN don | issue: semantic overlap with `cmp` (complete). Both mean "finished" — `cmp` = successful completion, `don` = settled finality. Consider whether both are needed.
PASS dps | meaning confirmed — "dependency" → dps is recognizable in package management context.
WARN dps | issue: "dps" is a non-obvious truncation of "dependency" (first 3 = "dep"). Could suggest DPS (damage per second) to gamers.
PASS dpt | meaning confirmed — "depth" → dpt is recognizable. Fits LLM success mode.
PASS dqe | meaning confirmed — "dequeue" → dqe is recognizable. Fits queue operations.
PASS dup | meaning confirmed — "duplicate" → dup is standard abbreviation. HVM primitive.
WARN epi | issue: "epi" is ambiguous — could mean epistemic, epidemic, or episode. Source word "epistemic" is very technical.
PASS ers | meaning confirmed — "erase" → ers is recognizable. HVM primitive.
PASS esc | meaning confirmed — "escalate" → esc is first 3 letters. Fits agent workflow.
PASS est | meaning confirmed — "estimate" → est is first 3 letters. Fits confidence domain.
PASS eva | meaning confirmed — "evaluate" → eva is first 3 letters. Fits assessment domain.
PASS fit | meaning confirmed — "fitting" → fit is the full word. LLM success mode.
FAIL fld | problem: "fld" is NOT a natural truncation of "failed" (first 3 = "fai"). Would suggest "fold" or "field" to most readers.
FAIL flu | problem: collision with existing `flu` (fluctuation) in temporal/change_types. "Fluent" and "fluctuation" are unrelated meanings.
PASS fmt | meaning confirmed — "format" → fmt is standard abbreviation.
PASS fnc | meaning confirmed — "function" → fnc is standard abbreviation.
PASS frb | meaning confirmed — "forbid" → frb is recognizable truncation.
PASS frm | meaning confirmed — "framed" → frm is recognizable. LLM success mode.
FAIL fwd | problem: duplicate concept with existing `for` (forward, future, progress, advance) in spatial/directions. Same meaning.
PASS get | meaning confirmed — "get" → full word. Universal data retrieval.
PASS gnd | meaning confirmed — "grounded" → gnd is standard abbreviation (electronics). LLM success mode.
PASS gus | meaning confirmed — "guess" → gus is recognizable truncation. Fits confidence domain.
PASS hbt | meaning confirmed — "heartbeat" → hbt is recognizable. Fits agent monitoring.
WARN hlu | issue: "hlu" is a non-standard truncation of "hallucinate" (first 3 = "hal"). The jump from "hal" to "hlu" is not intuitive.
WARN hof | issue: "hof" is a non-standard truncation of "handoff" (first 3 = "han"). Not immediately guessable.
PASS hok | meaning confirmed — "hook" → hok is recognizable truncation. Fits agent attachment.
PASS idl | meaning confirmed — "idle" → idl is standard abbreviation. Fits agent status.
PASS ign | meaning confirmed — "ignore" → matches existing `ign` (ignoring) in mental/mental_states. Same meaning, cross-domain use valid.
PASS inp | meaning confirmed — "input" → inp is standard abbreviation.
WARN ist | issue: "ist" is a non-standard truncation of "install" (first 3 = "ins"). But `ins` is taken (inside). Creates confusion.
PASS kil | meaning confirmed — "kill" → kil is recognizable. Fits process management.
PASS lay | meaning confirmed — "layer" → matches existing `lay` (layer, stratum) in physical/structures. Same meaning, valid cross-domain use.
PASS llm | meaning confirmed — "LLM" → acronym used as word. Domain-defining term.
PASS lmt | meaning confirmed — "limit" → lmt is standard abbreviation.
WARN lok | issue: close to existing `loc` (local) — one letter difference. "Lock" vs "local" distinguishable but dense neighborhood.
PASS lst | meaning confirmed — "list" → lst is standard abbreviation.
PASS luc | meaning confirmed — "lucid" → luc is first 3 letters. LLM success mode.
PASS mrg | meaning confirmed — "merge" → mrg is standard abbreviation. HVM primitive.
WARN mtc | issue: "mtc" is a non-standard truncation of "match" (first 3 = "mat"). Not immediately guessable.
PASS nod | meaning confirmed — "node" → matches existing `nod` (node, junction) in physical/structures. Same meaning.
PASS nud | meaning confirmed — "nudge" → nud is first 3 letters. Fits agent communication.
PASS nxt | meaning confirmed — "next" → matches existing `nxt` (next/then) in discourse. Same meaning.
PASS obj | meaning confirmed — "object" → obj is standard abbreviation.
PASS obs | meaning confirmed — "observe" → obs is first 3 letters.
PASS ocp | meaning confirmed — "occupied" → ocp is recognizable. Fits agent status.
PASS opn | meaning confirmed — "open" → opn is standard abbreviation. Fits state machine.
PASS opt | meaning confirmed — "optimize" → opt is first 3 letters.
WARN otp | issue: "otp" commonly means "one-time password" in security contexts. Could create misreadings in agent/security discourse.
WARN ovr | issue: "ovr" is too generic — reads as "over" generally. Weak link to source "overconfident". Partner `udc` (underconfident) uses different abbreviation pattern.
PASS own | meaning confirmed — "own" → matches existing `own` (ownership) in economics. Same meaning.
PASS pdg | meaning confirmed — "pending" → pdg is recognizable. Fits lifecycle status.
PASS per | meaning confirmed — "perceive" → matches existing `per` (perceiving) in mental/cognitive_actions. Same meaning already in vocabulary.
PASS pkg | meaning confirmed — "package" → pkg is standard abbreviation.
PASS pln | meaning confirmed — "plan" → pln is recognizable truncation.
PASS pok | meaning confirmed — "poke" → pok is recognizable. Fits agent monitoring.
FAIL prc | problem: collision with existing `prc` (price/cost) in economics. "Precise" and "price" are unrelated meanings.
FAIL prf | problem: collision with existing `prf` (profit) in economics. "Proof/prove" and "profit" are unrelated meanings.
PASS prl | meaning confirmed — "parallel" → prl is recognizable. HVM concept.
PASS prv | meaning confirmed — "provisional" → prv is recognizable. Fits confidence domain.
PASS pty | meaning confirmed — "priority" → pty is recognizable (though `pri` is more standard, it's taken by pride).
PASS put | meaning confirmed — "put" → full word. Data storage operation.
WARN qeu | issue: non-standard representation of "queue". Expected "que" (first 3 letters). "qeu" looks like a misspelling.
PASS rdc | meaning confirmed — "reduce" → rdc is recognizable. HVM primitive.
PASS rdy | meaning confirmed — "ready" → rdy is standard abbreviation. Fits agent status.
FAIL red | problem: collision with existing `red` (red/warm hue) in colors. "Read" and "red" are unrelated meanings.
WARN req | issue: overlaps with existing `req` (requesting) in communication. "Requires" and "requesting" are semantically related but distinct verbs. Borderline polysemy.
WARN rev | issue: overlaps with existing `rev` (revolution) in temporal/change_types. "Revision" (reconsidering) vs "revolution" (sudden change) — related etymology but different meanings.
PASS rfn | meaning confirmed — "refine" → rfn is recognizable truncation.
PASS rls | meaning confirmed — "release" → rls is standard abbreviation.
PASS rsl | meaning confirmed — "result" → rsl is recognizable.
WARN rsn | issue: "rsn" is ambiguous — could be "reason" or "resonant". Source is "resonant" but "reason" is a more common English word starting with "rea/res".
PASS rtr | meaning confirmed — "retry" → rtr is recognizable.
PASS sgl | meaning confirmed — "signal" → sgl is recognizable.
PASS skp | meaning confirmed — "skip" → skp is recognizable truncation.
WARN snc | issue: "snc" is a non-standard truncation of "synced" (first 3 = "syn"). `syn` exists as "synthesizing". Derivation unclear.
FAIL spn | problem: collision with existing `spn` (spending) in economics. "Spawn" and "spending" are unrelated meanings.
WARN stb | issue: semantic overlap with existing `sta` (stability) in physical domain. "Stable" and "stability" are the same concept in different forms.
PASS stg | meaning confirmed — "string" → stg is recognizable (though "str" is more standard, it's taken by strong).
PASS stk | meaning confirmed — "stuck" → stk is recognizable truncation. Fits agent status.
PASS sys | meaning confirmed — "system" → sys is standard abbreviation.
PASS tbl | meaning confirmed — "table" → tbl is standard abbreviation.
PASS tes | meaning confirmed — "test" → tes is first 3 letters.
WARN tkn | issue: potential duplicate with existing `tok` (token) in spec Domain 11. Same concept, different code.
PASS tmo | meaning confirmed — "timeout" → tmo is recognizable.
WARN tnv | issue: "tnv" is a non-standard truncation of "tentative" (first 3 = "ten"). Derivation not obvious.
FAIL too | problem: "too" reads as English word "too" (also/excessively), not "tool". Meaning unguessable from form. Source truncation drops the key letter 'l'.
WARN tpe | issue: "tpe" is a non-standard truncation of "type" (expected "typ"). "tpe" also looks like Taipei airport code.
PASS try | meaning confirmed — "try" → full word. Fits attempt/execution domain.
PASS ubk | meaning confirmed — "unblocked" → ubk derivable from un+blk. Fits dependency domain.
WARN udc | issue: naming pattern inconsistent with pair word `ovr` (overconfident). "udc" = under+confident compressed, but "ovr" = over truncated. Asymmetric.
PASS ufy | meaning confirmed — "unify" → ufy is recognizable. HVM/Prolog concept.
PASS ulk | meaning confirmed — "unlock" → ulk derivable from un+lok. Fits access control.
PASS unk | meaning confirmed — "unknown" → unk is standard abbreviation.
PASS usr | meaning confirmed — "user" → usr is a Unix standard. Universally recognized.
FAIL ver | problem: collision with existing `ver` (vertical) in spatial/directions. "Verified" and "vertical" are unrelated meanings.
PASS viz | meaning confirmed — "visualize" → viz is standard abbreviation.
PASS vlu | meaning confirmed — "value" → vlu is recognizable truncation.
PASS vsn | meaning confirmed — "version" → vsn is recognizable (standard `ver` is taken by vertical collision above).
PASS wgt | meaning confirmed — "weight" → wgt is recognizable. Note: spec Domain 11 has `wei` for same concept.
PASS wke | meaning confirmed — "wake" → wke is recognizable truncation.
PASS wrk | meaning confirmed — "work" → wrk is standard abbreviation.
PASS xfr | meaning confirmed — "transfer" → xfr is standard abbreviation.
PASS xpt | meaning confirmed — "export" → xpt is standard abbreviation.
PASS yld | meaning confirmed — "yield" → yld is standard abbreviation.
```

---

## val sum
> validation summary

```
pas: 89 | war: 23 | fal: 13
> pass: 89 | warn: 23 | fail: 13
```

### fal lis
> fail list

| Word | Problem |
|------|---------|
| `blk` | Collision with existing `blk` (black/dark) in colors domain. Unrelated meanings. |
| `brc` | Duplicate concept with existing `bra` (branch, fork). Same source word. |
| `clr` | Duplicate concept with existing `cle` (clear). Same meaning, different code. |
| `dns` | Duplicate concept with existing `den` (dense, compact). Same meaning, different code. |
| `fld` | Non-natural truncation: "fld" does not derive from "failed" (first 3 = "fai"). Suggests "fold" or "field". |
| `flu` | Collision with existing `flu` (fluctuation) in temporal domain. "Fluent" ≠ "fluctuation". |
| `fwd` | Duplicate concept with existing `for` (forward) in spatial/directions. Same meaning. |
| `prc` | Collision with existing `prc` (price/cost) in economics. "Precise" ≠ "price". |
| `prf` | Collision with existing `prf` (profit) in economics. "Proof" ≠ "profit". |
| `red` | Collision with existing `red` (red/warm hue) in colors. "Read" ≠ "red". |
| `spn` | Collision with existing `spn` (spending) in economics. "Spawn" ≠ "spending". |
| `too` | Reads as English "too" (also/excessively), not "tool". Meaning unguessable from form. |
| `ver` | Collision with existing `ver` (vertical) in spatial/directions. "Verified" ≠ "vertical". |

### war lis
> warn list

| Word | Issue |
|------|-------|
| `agt` | Potential duplicate with `age` (agent) in spec Domain 11. |
| `ale` | "ale" suggests beer; source "aleatoric" is very technical/uncommon. |
| `alg` | "alg" strongly suggests "algorithm", not "aligned". Source ambiguity. |
| `don` | Semantic overlap with `cmp` (complete). Both mean "finished". |
| `dps` | Non-obvious truncation of "dependency" (first 3 = "dep"). |
| `epi` | Source ambiguity: "epi" could mean epistemic, epidemic, or episode. |
| `hlu` | Non-standard truncation of "hallucinate" (first 3 = "hal"). |
| `hof` | Non-standard truncation of "handoff" (first 3 = "han"). |
| `ist` | Non-standard truncation of "install" (first 3 = "ins", taken by "inside"). |
| `lok` | Close to existing `loc` (local) — one letter difference. |
| `mtc` | Non-standard truncation of "match" (first 3 = "mat"). |
| `otp` | "OTP" commonly means "one-time password" in security contexts. |
| `ovr` | Too generic — reads as "over". Weak link to source "overconfident". |
| `qeu` | Non-standard representation of "queue" (expected "que"). |
| `req` | Overlaps with existing `req` (requesting). Semantically related but distinct. |
| `rev` | Overlaps with existing `rev` (revolution). Related etymology, different meaning. |
| `rsn` | Ambiguous: could be "reason" or "resonant". |
| `snc` | Non-standard truncation of "synced" (first 3 = "syn", taken by "synthesizing"). |
| `stb` | Semantic overlap with existing `sta` (stability). Same concept. |
| `tkn` | Potential duplicate with `tok` (token) in spec Domain 11. |
| `tnv` | Non-standard truncation of "tentative" (first 3 = "ten"). |
| `tpe` | Non-standard truncation of "type" (expected "typ"). |
| `udc` | Naming pattern inconsistent with pair `ovr` (overconfident). |

---

## Methodology Notes

### Cross-references checked
- **vocabulary-v3-natural.md**: Full spec including Domain 11 (10 words: age, mod, wei, ctx, inf, tok, prm, emb, lat, att)
- **vocabulary.json**: 429 words across 12 domains (physical, spatial, temporal, living, mental, communication, social, abstract, economics, colors, textures, discourse)
- **Phonetic neighbor analysis**: All 1-Hamming-distance pairs checked between task words and existing vocabulary (429 words), and within task words themselves

### Key collision categories
1. **Cross-domain collisions** (7): blk, flu, prc, prf, red, spn, ver — same 3-letter code, unrelated meanings in different domains
2. **Duplicate concepts** (4): brc/bra, clr/cle, dns/den, fwd/for — same meaning encoded with different 3-letter codes
3. **Source derivation failures** (2): fld (not from "failed"), too (reads as "too" not "tool")
4. **Spec Domain 11 overlaps** (3 warns): agt/age, tkn/tok, wgt/wei — may be intentional replacements

### Phonetic density concerns
The Agent/AI domain has high phonetic density in several regions:
- **a**-prefix: ack, act, ada, agg, agt, ale, alg, alv, asm, awt (10 words)
- **st**-prefix: stb, stg, stk (3 words, all 1-letter apart from each other AND from existing sta, ste, sti, sto, str)
- **pr**-prefix: prc, prf, prl, prv (4 words, all 1-letter apart, plus existing pra, pri, pro, prs)
- **fr**-prefix: frb, frm (2 words, 1-letter apart)
- **fl**-prefix: fld, flu (2 words, 1-letter apart)

These dense clusters increase the risk of misidentification in both reading and composition.
