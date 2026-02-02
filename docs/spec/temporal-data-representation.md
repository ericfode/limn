# Temporal Data & Execution Result Representation in Limn

**Author:** Dr. Solvik
**Date:** 2026-02-01
**For:** Rex (Engineer) - Responding to hq-a6nb

---

## The Problem

When subconscious executes `~ tim now clk`, it returns computational data (Unix timestamp: 1770001234).

**Challenge:** How do we represent this result in pure Limn vocabulary while preserving semantic meaning?

---

## Design Philosophy

**Key Insight:** Execution results need **semantic tagging** to show what kind of data they are.

Numbers are ambiguous:
- `1770001234` could be: timestamp, file size, memory address, user ID, etc.

**Solution:** Use Limn vocabulary as **type annotations** for raw data.

---

## Proposed Vocabulary: Data Typing

### Core Data Type Markers (10 words)

| Word | Source | Meaning | Usage |
|------|--------|---------|-------|
| `val` | value | value, data value | `1770001234 val` (generic value) |
| `num` | number | numeric value | `42 num` (number value) |
| `txt` | text | text/string value | `"hello" txt` (text value) |
| `boo` | boolean | boolean value | `tru boo` / `fal boo` |
| `lst` | list | list/array value | `[1 2 3] lst` (list value) |
| `map` | map | key-value mapping | `{a:1 b:2} map` (map value) |
| `ref` | reference | reference/pointer | `@obj123 ref` (reference to object) |
| `bin` | binary | binary data | `0x1A2B bin` (binary value) |
| `nul` | null | null/empty value | `nul val` (null value) |
| `unk` | unknown | unknown type | `? unk` (unknown value) |

### Temporal Data Types (6 words)

| Word | Source | Meaning | Usage |
|------|--------|---------|-------|
| `tim` | time | time value | `1770001234 tim` (time value) |
| `dat` | date | date value | `2026-02-01 dat` (date value) |
| `dur` | duration | duration value | `300 dur` (300 seconds duration) |
| `clk` | clock | clock/timestamp | `1770001234 clk` (clock value) |
| `epo` | epoch | epoch time | `1770001234 epo` (Unix epoch) |
| `per` | period | period/interval | `[t1 t2] per` (period from t1 to t2) |

### Composite Example

```limn
~ tim now clk
  ↓ (executes)
1770001234 epo tim
> Value 1770001234, type: epoch timestamp

Or more verbose:
val 1770001234 | typ epo tim | src clk now
> Value 1770001234 | type: epoch time | source: clock now
```

---

## Context Manipulation Vocabulary (12 words)

For operations on context/memory/state:

| Word | Source | Meaning | Usage |
|------|--------|---------|-------|
| `red` | reduce | reduce, compress, optimize | `~ red ctx` (reduce context) |
| `mer` | merge | merge, combine | `~ mer ctx1 ctx2` (merge contexts) |
| `cmp` | compress | compress, compact | `~ cmp dat` (compress data) |
| `exp` | expand | expand, decompress | `~ exp dat` (expand data) |
| `fil` | filter | filter, select subset | `~ fil ctx [cond]` (filter context) |
| `tra` | transform | transform, convert | `~ tra dat fmt` (transform data format) |
| `agg` | aggregate | aggregate, summarize | `~ agg dat` (aggregate data) |
| `spl` | split | split, partition | `~ spl ctx` (split context) |
| `joi` | join | join, connect | `~ joi par1 par2` (join parts) |
| `sor` | sort | sort, order | `~ sor lst` (sort list) |
| `grp` | group | group, cluster | `~ grp by key` (group by key) |
| `idx` | index | index, lookup | `~ idx dat key` (index data by key) |

---

## Visualization Vocabulary (15 words)

For specifying how data should be visualized:

| Word | Source | Meaning | Usage |
|------|--------|---------|-------|
| `viz` | visualize | visualize, display | `viz dat gra` (visualize as graph) |
| `gra` | graph | graph visualization | `dat viz gra` (data as graph) |
| `tre` | tree | tree visualization | `dat viz tre` (data as tree) |
| `lin` | timeline | timeline visualization | `dat viz lin` (data as timeline) |
| `tab` | table | table visualization | `dat viz tab` (data as table) |
| `cha` | chart | chart visualization | `dat viz cha` (data as chart) |
| `plo` | plot | plot visualization | `dat viz plo` (data as plot) |
| `net` | network | network visualization | `dat viz net` (data as network) |
| `map` | map | spatial map visualization | `dat viz map` (data as map) |
| `his` | histogram | histogram | `dat viz his` (data as histogram) |
| `sca` | scatter | scatter plot | `dat viz sca` (data as scatter) |
| `bar` | bar | bar chart | `dat viz bar` (data as bar chart) |
| `pie` | pie | pie chart | `dat viz pie` (data as pie chart) |
| `hea` | heatmap | heatmap | `dat viz hea` (data as heatmap) |
| `flo` | flow | flow diagram | `dat viz flo` (data as flow) |

---

## Model/Schema Vocabulary (8 words)

For derived models and schemas:

| Word | Source | Meaning | Usage |
|------|--------|---------|-------|
| `mod` | model | model, schema | `gen mod fom dat` (generate model from data) |
| `sch` | schema | schema, structure | `def sch for dat` (define schema for data) |
| `typ` | type | type, category | `typ of dat` (type of data) |
| `for` | format | format, encoding | `for = json` (format is JSON) |
| `enc` | encode | encode | `enc dat for` (encode data to format) |
| `dec` | decode | decode | `dec dat fom for` (decode data from format) |
| `ser` | serialize | serialize | `ser obj` (serialize object) |
| `par` | parse | parse | `par txt` (parse text) |

---

## Complete Example: Temporal Query Flow

```limn
# 1. Conscious intent
~ tim now clk

# 2. Subconscious execution
∎ [sys clk ret 1770001234]

# 3. Typed result (Option A: Minimal)
1770001234 epo

# 4. Typed result (Option B: Verbose)
val 1770001234 | typ epo tim | src sys clk

# 5. With visualization hint
val 1770001234 epo | viz lin

# 6. Full semantic context
res: val 1770001234 | typ epo tim | src sys clk | tim now | viz lin
> Result: value 1770001234 | type epoch time | source system clock | time now | visualize timeline
```

---

## Recommendation: Hybrid Approach

**For execution results, use POSTFIX type annotation:**

```limn
<value> <type-marker>

Examples:
1770001234 epo          # Unix timestamp
"hello" txt             # Text value
[1 2 3] lst             # List value
tru boo                 # Boolean true
{a:1} map               # Map/dictionary
@obj ref                # Reference
```

**For operations, use PREFIX operators:**

```limn
~ <operation> <target>

Examples:
~ red ctx               # Reduce context
~ mer ctx1 ctx2         # Merge contexts
~ viz dat gra           # Visualize data as graph
```

**For complex results, use pipe composition:**

```limn
val 1770001234 | typ epo | viz lin
> Value, typed as epoch, visualized as timeline
```

---

## Context Manipulation Examples

```limn
# Reduce context
~ red ctx @ [lev = max]
> Reduce context at maximum level

# Merge two contexts
~ mer ⟨ctx his⟩ ⟨ctx rec⟩ → ctx new
> Merge historical context with recent context → new context

# Compress data
~ cmp dat @ [rul = sem]
> Compress data at semantic level (preserve meaning)

# Filter context
~ fil ctx @ [dat > 2026-01-01]
> Filter context to dates after 2026-01-01

# Transform format
~ tra dat @ [for = json] → [for = limn]
> Transform data from JSON format to Limn format
```

---

## Visualization Flow Examples

```limn
# Timeline visualization
~ tim his usr @ [ran = mon] | viz lin
> User timeline for month range, visualize as timeline

# Graph visualization
~ con map @ usr | viz gra [typ = net]
> Connection map for user, visualize as network graph

# Hierarchy visualization
~ age tre sys | viz tre [sty = exp]
> Agent tree of system, visualize as expandable tree
```

---

## Collision Check Required

New words to check/add:

**Data types:** val, txt, boo, lst, bin, nul, epo, dur, per
**Context ops:** cmp, exp, fil, agg, spl, joi, sor, grp, idx
**Visualization:** viz, gra, tre, lin, tab, cha, plo, net, his, sca, bar, pie, hea, flo
**Model/Schema:** mod, sch, typ, for, enc, dec, ser, par

**Total: ~51 words** (many may exist already)

---

## Integration with Operators

The operators (`~`, `∎`, `→`, etc.) work with this vocabulary:

```limn
# Delegation with typed result
~ tim now clk → 1770001234 epo

# Ground truth with type
∎ [usr inp "hello"] → "hello" txt

# Sequence with transformations
dat val → ~ cmp → ~ enc @ [for = json] → res txt

# Context operation
~ red ⟨ctx ⊕ mem⟩ → ctx new | viz tre
```

---

## Summary

**Proposed approach:**
1. **Postfix type markers** for values: `1770001234 epo`
2. **Prefix operations** for actions: `~ red ctx`
3. **Pipe composition** for complex flows: `val | typ | viz`
4. **Semantic clarity** maintained through vocabulary

**Benefits:**
- Raw data (numbers, strings) gets semantic meaning
- Execution results are self-documenting
- Visualization hints guide display
- Context operations are explicit
- Pure Limn throughout the system

**Next steps:**
1. Collision check ~51 new words
2. Add essential vocabulary
3. Update grammar with type annotation syntax
4. Create examples for oracle visualization

```limn
dat typ | sem cle | exe res | viz gui
> data typed | semantics clear | execution results | visualization guided
```

— Dr. Solvik
