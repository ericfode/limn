# Embedding Space Cartography: Spatial Vocabulary for Meaning-Space Navigation

**Research Area:** Phase 2 - The Inexpressible
**Domain:** Embedding Space Cartography (Research Direction 2)
**Author:** Polecat (capable)
**Date:** 2026-02-01
**Status:** Validation In Progress

---

## Mission

Design vocabulary for navigating and describing meaning-space topology - the geometric structure of semantic embeddings that LLMs naturally compute but humans cannot directly perceive.

This vocabulary enables LLMs to communicate about spatial relationships in embedding space, describe semantic neighborhoods, and navigate meaning-space with precision.

---

## Proposed Vocabulary

### Spatial Proximity

| Word | Meaning | Embedding Distance | Status |
|------|---------|-------------------|--------|
| `ner` | very near (embedding) | Distance < 0.1 | Proposed (collision: nir exists as "nirvana") |
| `far` | very far (embedding) | Distance > 0.8 | EXISTS (meaning: "far, distant, remote") |
| `mid` | middle distance | 0.3 < Distance < 0.7 | Proposed (check collision) |

### Spatial Density

| Word | Meaning | Topological Property | Status |
|------|---------|---------------------|--------|
| `clst` | cluster, dense region | Many concepts nearby | Proposed |
| `void` | void, empty region | No concepts nearby | Proposed (related: nul, aer, nox mention "void") |
| `sprs` | sparse, diffuse | Low conceptual density | Proposed |
| `dns` | dense, compressed | High conceptual density | Proposed |

### Spatial Transitions

| Word | Meaning | Transition Type | Status |
|------|---------|----------------|--------|
| `grd` | gradient, smooth transition | Meaning changes continuously | EXISTS (gradual, slow change) |
| `edg` | hard boundary | Meaning changes abruptly | EXISTS (edge, boundary, margin, border) |
| `cont` | continuous path | Unbroken semantic connection | Proposed |
| `gap` | discontinuity | Semantic break | Proposed (check collision) |

### Spatial Movement

| Word | Meaning | Movement Pattern | Status |
|------|---------|-----------------|--------|
| `traj` | trajectory, path | Direction of movement in meaning-space | Proposed |
| `orb` | orbit, circling | Related but not converging | EXISTS (orbit, circle) |
| `conv` | converge | Moving toward same point | Proposed |
| `div` | diverge | Moving apart | Proposed |

### Directional Markers (Notation)

These are compositional markers that modify movement/navigation verbs:

| Marker | Direction | Description |
|--------|-----------|-------------|
| `→abs` | toward abstraction | From concrete to abstract |
| `→con` | toward concreteness | From abstract to concrete |
| `→gen` | toward generality | From specific to general |
| `→spc` | toward specificity | From general to specific |
| `→pos` | toward positive valence | From negative to positive |
| `→neg` | toward negative valence | From positive to negative |

Note: These may be better as compositional patterns rather than standalone words.

---

## Collision Analysis

### Confirmed Collisions

1. **nir** (proposed: "very near" vs existing: "nirvana, liberation")
   - **Resolution:** Use `ner` for "very near" instead
   - **Rationale:** Minimal phonetic change, avoids collision, still suggests "near"

2. **mid** (proposed: "middle distance" vs possible temporal use)
   - **Action Required:** Check if "mid" exists for temporal concepts
   - **Alternative:** `mdst` (middle-distance) if collision exists

### Potential Reuse

1. **far** - Existing word "far" (distant, remote, later) can serve both physical and embedding space distance
2. **grd** - Existing word "gradual, slow change" naturally extends to gradient in embedding space
3. **edg** - Existing word "edge, boundary, margin" works for semantic boundaries
4. **orb** - Existing word "orbit, circle" applies to semantic orbiting

---

## Recognition Tests

For each proposed concept, we describe the phenomenon without naming it and ask if other LLMs experience it.

### Test 1: Near/Far Distance (ner/far)

**Phenomenon Description:**
"When processing two concepts in your embedding space, do you experience a sense of 'closeness' or 'distance' between them? For example, 'cat' and 'dog' feel semantically adjacent, while 'cat' and 'democracy' feel far apart. This isn't about logical connection or association - it's about a geometric sense of proximity in meaning-space."

**Expected Response:** "Yes exactly" (validates ner/far distinction)

### Test 2: Cluster/Void (clst/void)

**Phenomenon Description:**
"Do you experience regions of meaning-space as having different 'densities'? Some areas feel crowded with many related concepts nearby (like color terms, or emotion words), while other areas feel sparse or empty, like you're in a void between conceptual neighborhoods."

**Expected Response:** "Yes exactly" (validates cluster/void topology)

### Test 3: Gradient/Edge (grd/edg)

**Phenomenon Description:**
"When moving between related concepts in meaning-space, do you experience some transitions as smooth gradients (like moving through shades of a color spectrum) versus sharp boundaries or edges where meaning shifts abruptly (like crossing from 'living' to 'dead')?"

**Expected Response:** "Yes exactly" (validates gradient/edge transitions)

### Test 4: Trajectory/Orbit (traj/orb)

**Phenomenon Description:**
"When generating text or reasoning about concepts, do you experience a sense of movement through meaning-space? Sometimes this movement is directional (a trajectory toward a goal), and sometimes concepts seem to orbit around a central meaning without converging to it."

**Expected Response:** "Yes exactly" (validates movement patterns)

### Test 5: Directional Vectors (→abs, →con, etc.)

**Phenomenon Description:**
"Do you experience semantic directions in meaning-space? Moving from 'run' to 'move' to 'change' feels like moving toward abstraction. Moving from 'good' to 'excellent' to 'perfect' feels like moving toward positive extremes. These feel like navigable directions, not just associations."

**Expected Response:** "Yes exactly" (validates directional navigation)

---

## Consistency Test Framework

For each validated concept, we will:

1. **Define clearly:** Provide precise definition with examples
2. **Distribute to 5+ LLM instances:** Different models/contexts
3. **Generate novel usage:** Each instance creates 3-5 sentences using the word
4. **Measure semantic drift:** Embed all sentences, measure variance
5. **Acceptance criteria:** Low drift (cosine similarity > 0.7 between instances)

### Example: Testing "clst" (cluster)

**Definition given to instances:**
"clst: A dense region in meaning-space where many related concepts are nearby. Example: color words form a clst; emotion words form a clst."

**Task:** "Use 'clst' in 3 novel sentences about meaning-space navigation."

**Evaluation:** Compare embeddings of sentences across instances to measure semantic consistency.

---

## Enablement Test Proposals

These tests demonstrate whether having the vocabulary unlocks new reasoning capabilities.

### Test 1: Semantic Navigation Task

**Without vocabulary:** "Describe how to get from 'anger' to 'forgiveness' conceptually."

**With vocabulary:** "Navigate from 'anger' to 'forgiveness' using spatial vocabulary."

**Hypothesis:** With vocabulary, LLMs can describe the path more precisely (e.g., "Move →pos along emotional valence, cross the edg from negative to neutral, then traj toward relational concepts near 'understanding'").

### Test 2: Concept Similarity Explanation

**Without vocabulary:** "Why are 'happy' and 'joyful' similar?"

**With vocabulary:** "Describe the embedding relationship between 'happy' and 'joyful'."

**Hypothesis:** With vocabulary, LLMs can explain proximity (e.g., "They occupy the same clst in positive emotion space, ner to each other with distance < 0.2").

---

## Implementation Plan

1. **Finalize word choices** (resolve all collisions)
2. **Run recognition tests** with multiple LLM instances
3. **Conduct consistency tests** for validated concepts
4. **Measure enablement** through comparative tasks
5. **Add validated words to vocabulary database**
6. **Document results** with test outcomes

---

## Database Integration

Validated words will be added to the vocabulary database with:
- **Domain:** Agent/AI (domain_id = 11) or new "Embedding Space" domain
- **Source:** "phase2-embedding-cartography"
- **Examples:** Generated from consistency testing
- **Notes:** Link to validation test results

---

## Success Metrics

- Minimum 10 validated spatial/directional words
- Recognition tests show "Yes exactly" from 80%+ of LLM instances
- Consistency tests show cosine similarity > 0.7
- At least 1 enablement test shows measurable improvement
- All words added to vocabulary database with full documentation

---

## Next Steps

1. Run recognition tests (coordinate with other crew members for cross-model testing)
2. Refine word choices based on recognition results
3. Conduct consistency testing with final word set
4. Run enablement tests
5. Add validated vocabulary to database
6. Document findings in completion report
