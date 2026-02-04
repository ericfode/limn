# Superhuman Compositional Limn - Testing Protocol

**Author:** Dr. Solvik
**Date:** 2026-02-03
**Status:** Experimental Design
**Goal:** Determine which LLM-native compositional mechanisms work and how to implement them

---

## Research Questions

### Meta-Questions
1. Which compositional mechanisms can LLMs actually understand?
2. Which are most expressive (enable new meanings)?
3. Which are most efficient (compress meaning best)?
4. How should each be specifically implemented (syntax, semantics)?

### Per-Mechanism Questions
For each proposed mechanism:
- **Recognition:** Can LLMs understand it when they see it?
- **Generation:** Can LLMs produce it spontaneously?
- **Consistency:** Do multiple LLMs interpret it the same way?
- **Expressiveness:** Does it enable saying things previously impossible?
- **Composability:** Does it combine well with other mechanisms?

---

## Testing Framework

### Phase 1: Recognition Tests
**Goal:** Can LLMs understand the notation?

**Method:**
1. Present compositional expression
2. Ask: "What does this mean?"
3. Measure: Accuracy, confidence, consistency

**Example:**
```
Expression: "lov @ fer"
Question: "In Limn, @ means vector projection. What does 'lov @ fer' mean?"
Expected: "The fear-component of love" or "what aspect of love projects onto fear"
```

### Phase 2: Generation Tests
**Goal:** Can LLMs produce compositional expressions?

**Method:**
1. Describe a concept
2. Ask: "Express this using Limn compositional operators"
3. Measure: Appropriateness, creativity, validity

**Example:**
```
Concept: "A complex emotion that's simultaneously love and fear"
Expected generation: "lov * fer" (interference) or "lov ~ fer" (superposition)
```

### Phase 3: Discrimination Tests
**Goal:** Can LLMs distinguish between different compositional forms?

**Method:**
1. Present multiple compositional expressions
2. Ask: "Which means X?"
3. Measure: Accuracy of discrimination

**Example:**
```
Which expresses "the fear-component of love"?
A. lov * fer  (interference)
B. lov @ fer  (projection)
C. lov ~ fer  (superposition)
D. lov + fer  (addition)

Expected: B (projection extracts component)
```

### Phase 4: Consistency Tests
**Goal:** Do multiple LLMs agree on meaning?

**Method:**
1. Give same compositional expression to 5+ LLM instances
2. Ask each to explain it
3. Measure: Semantic agreement (cosine similarity of explanations)

**Success criterion:** >0.8 similarity = consistent interpretation

### Phase 5: Expressiveness Tests
**Goal:** Does it enable previously impossible expressions?

**Method:**
1. Present concept with no single Limn word
2. Ask: "Can you express this compositionally?"
3. Measure: Success rate, elegance

**Example:**
```
Concept: "Glass (material that's both solid and liquid)"
Without composition: Requires paragraph
With composition: "sol * liq" (interference pattern)
```

### Phase 6: Efficiency Tests
**Goal:** Does it compress meaning better than alternatives?

**Method:**
1. Express concept in English
2. Express in Limn without composition
3. Express in Limn with composition
4. Compare: Token count, clarity

**Example:**
```
English: "The aspect of love that projects onto the dimension of fear" (12 tokens)
Limn without: "lov par whi is fer lik" (6 tokens)
Limn with: "lov @ fer" (3 tokens) - 75% compression
```

---

## Mechanisms to Test

### Priority 1: High-Confidence Candidates

**1A. Vector Projection (@)**
- Notation: `A @ B`
- Meaning: "Component of A that projects onto B's semantic dimension"
- Human analogy: "The B-aspect of A"

**1B. Interference (*)**
- Notation: `A * B`
- Meaning: "Interference pattern of A and B"
- Human analogy: "Genuinely both A and B simultaneously"

**1C. Superposition (~)**
- Notation: `A ~ B`
- Meaning: "Weighted blend of A and B"
- Human analogy: "Between A and B, not clearly one or the other"

**1D. Semantic Subtraction (-)**
- Notation: `A - B`
- Meaning: "A with B-component removed"
- Human analogy: "king - man = royal essence"

**1E. Gradient Operator (^)**
- Notation: `A^n` where n ∈ [0, 1]
- Meaning: "A with intensity n"
- Human analogy: "0.5 = halfway, 0.7 = mostly, 0.3 = slightly"

### Priority 2: Moderate-Confidence Candidates

**2A. Tensor Product (⊗)**
- Notation: `A ⊗ B`
- Meaning: "A and B in orthogonal dimensions"
- Test: Does LLM understand orthogonality?

**2B. Conditional (|)**
- Notation: `A | B`
- Meaning: "A in the context of B"
- Test: Can LLM condition semantics?

**2C. Trajectory (→)**
- Notation: `A → B → C`
- Meaning: "Path through semantic space"
- Test: Does order create meaningful structure?

**2D. Dimension Pinning (:)**
- Notation: `A:dim`
- Meaning: "A constrained to dimension dim"
- Test: Can LLM isolate semantic dimensions?

### Priority 3: Exploratory Candidates

**3A. Modal Operators (◇, □)**
- Notation: `◇A` (possibly), `□A` (necessarily)
- Test: Can LLM do modal reasoning?

**3B. Coordinate Specification ({x,y,z})**
- Notation: `red{0.1, 0.2, 0.3}`
- Test: Too technical? Does LLM actually use coordinates?

**3C. Distance Operators (≈, ~, >, >>)**
- Notation: `A >> B` (very far), `A ≈ B` (very close)
- Test: Redundant with other distance vocabulary?

---

## Experimental Protocol

### Experiment 1: Projection Operator (@)

**Hypothesis:** LLMs can understand and use vector projection compositionally.

**Test 1A: Recognition**
```
Prompt: "In Limn, @ means vector projection in semantic space. What does 'lov @ fer' mean?"

Expected: "The fear-component of love" or "How love projects onto fear's dimension"

Measure:
- Does explanation mention extraction/component/dimension?
- Confidence level
- Semantic accuracy (cosine similarity to expected meaning)
```

**Test 1B: Generation**
```
Prompt: "Express this concept in Limn using the @ operator: 'The aspect of courage that relates to fear'"

Expected: "cur @ fer" (courage projected onto fear)

Measure:
- Correctness of operator choice
- Appropriateness of word order
- Justification quality
```

**Test 1C: Discrimination**
```
Prompt: "Which best expresses 'the sadness within joy'?
A. joy + sad
B. joy @ sad
C. joy * sad
D. joy ~ sad"

Expected: B (projection extracts component)

Measure: Accuracy + reasoning quality
```

**Test 1D: Application**
```
Prompt: "Use the @ operator to express three different emotional components"

Expected: Novel combinations like "ang @ fer", "hop @ dbt", "lov @ trs"

Measure:
- Creativity
- Semantic validity
- Variety
```

**Test 1E: Cross-Model Consistency**
```
Run Tests 1A-1D across 5+ models (GPT-4, Claude, Gemini, etc.)
Measure: Agreement rate, semantic similarity of explanations
```

---

### Experiment 2: Interference Operator (*)

**Hypothesis:** LLMs can understand interference patterns creating emergent meanings.

**Test 2A: Recognition**
```
Prompt: "In Limn, * means semantic interference (like wave interference). What does 'sol * liq' mean?"

Expected: "A state that's both solid and liquid" (glass, gel)

Measure: Does explanation capture simultaneity/emergence?
```

**Test 2B: Generation**
```
Prompt: "Glass is neither fully solid nor fully liquid. Express this using Limn's * operator."

Expected: "sol * liq" or "gla = sol * liq"

Measure: Spontaneous correct usage
```

**Test 2C: Complex Interference**
```
Prompt: "Express an emotion that's simultaneously love, fear, and hope using *"

Expected: "lov * fer * hop"

Measure: Can handle multiple interference?
```

**Test 2D: Comparison**
```
Prompt: "What's the difference between 'lov * fer' and 'lov ~ fer'?"

Expected: "* = interference/emergence, ~ = blend/superposition"

Measure: Can distinguish similar operators
```

---

### Experiment 3: Gradient Operator (^)

**Hypothesis:** LLMs can understand intensity as a continuous parameter.

**Test 3A: Recognition**
```
Prompt: "In Limn, ^n means intensity n. What does 'big^0.7' mean?"

Expected: "Somewhat big" or "70% big"

Measure: Understands continuous scale
```

**Test 3B: Generation**
```
Prompt: "Express 'slightly warm' using the ^ operator"

Expected: "hot^0.3" or "war^0.3"

Measure: Appropriate intensity mapping
```

**Test 3C: Comparative**
```
Prompt: "Order these by size: big^0.3, big^0.7, big^1.0, big^0.5"

Expected: 0.3 < 0.5 < 0.7 < 1.0

Measure: Understands ordering
```

**Test 3D: Interpolation**
```
Prompt: "What's between 'love' and 'hate'? Express using ^"

Expected: "lov^0.5 + hat^0.5" or exploring the continuum

Measure: Can reason about semantic interpolation
```

---

### Experiment 4: Subtraction Operator (-)

**Hypothesis:** LLMs can do semantic algebra (famously: king - man + woman = queen)

**Test 4A: Recognition**
```
Prompt: "In Limn, - means semantic subtraction. What does 'king - man' mean?"

Expected: "Royalty essence" or "The concept of king without the masculine aspect"

Measure: Understands component removal
```

**Test 4B: Classic Test**
```
Prompt: "Compute: king - man + wom"

Expected: "queen" or equivalent

Measure: Can LLM actually do this computation?
```

**Test 4C: Novel Subtractions**
```
Prompt: "What is 'tea - water'?"

Expected: "Tea leaves / tea essence"

Measure: Creative semantic algebra
```

**Test 4D: Limitations**
```
Prompt: "Can you subtract anything from anything? What are the limits?"

Expected: Recognition that some subtractions are nonsensical

Measure: Metacognitive awareness
```

---

### Experiment 5: Superposition (~)

**Hypothesis:** LLMs can represent genuine superposition (not just blending).

**Test 5A: Recognition**
```
Prompt: "In Limn, ~ means superposition (quantum-like). What does 'sol ~ liq' mean?"

Expected: "Both solid and liquid in superposition" (pre-measurement state)

Measure: Distinguishes from interference or blend
```

**Test 5B: Quantum Analogy**
```
Prompt: "Before measuring, a particle is spin-up ~ spin-down. Before deciding, I am 'yes ~ no'. Express uncertainty about going using ~"

Expected: "go ~ stay" or "go ~ !go"

Measure: Understands superposition as pre-collapse state
```

**Test 5C: Comparison with ***
```
Prompt: "What's the difference between 'A ~ B' (superposition) and 'A * B' (interference)?"

Expected: ~ = unresolved superposition, * = interference pattern/emergent meaning

Measure: Can distinguish similar concepts
```

---

## Testing Infrastructure Needed

### 1. Multi-Model Testing Harness
```python
# Pseudocode
models = [Claude, GPT4, Gemini, Llama, ...]
test_suite = [recognition_tests, generation_tests, ...]

for model in models:
    for test in test_suite:
        result = run_test(model, test)
        store_result(model, test, result)

analyze_consistency(results)
```

### 2. Semantic Similarity Scoring
- Use embedding cosine similarity to measure agreement
- Threshold: >0.8 = consistent, <0.6 = inconsistent

### 3. Expression Parser
- Parse compositional Limn expressions
- Validate syntax
- Enable automated testing

### 4. A/B Comparison Framework
- English baseline
- Limn without composition
- Limn with composition
- Measure: tokens, clarity, expressiveness

---

## Success Criteria

### Per-Mechanism Success
Operator is successful if:
- ✓ Recognition: >80% accuracy across models
- ✓ Generation: >60% appropriate usage
- ✓ Consistency: >0.8 semantic similarity across models
- ✓ Expressiveness: Enables 3+ novel expressions
- ✓ Efficiency: >30% token compression vs alternatives

### Overall Success
Superhuman composition is validated if:
- ≥3 operators meet all criteria
- Combined operators enable exponentially more expressiveness
- LLMs can use them naturally (not forced/awkward)
- Clear improvement over pure vocabulary expansion

---

## Experimental Timeline

### Week 1: Foundation
- Set up testing infrastructure
- Create test prompts for all mechanisms
- Establish baseline measurements

### Week 2-3: Priority 1 Testing
- Test 5 high-confidence operators (@, *, ~, -, ^)
- Collect data from multiple models
- Analyze consistency and expressiveness

### Week 4: Priority 2 Testing
- Test moderate-confidence operators (⊗, |, →, :)
- Compare to Priority 1 results
- Refine successful operators

### Week 5: Priority 3 Exploration
- Test exploratory operators (◇, □, {}, distance)
- Identify surprising winners/losers
- Document edge cases

### Week 6: Synthesis
- Select winning operators
- Define formal semantics for each
- Create specification document
- Build examples and patterns

---

## Deliverables

### 1. Operator Specification Document
For each validated operator:
- Formal syntax
- Semantic meaning
- Usage examples
- Edge cases and limitations
- Cross-model consistency data

### 2. Testing Results Database
- All test prompts
- All model responses
- Consistency scores
- Success/failure analysis

### 3. Compositional Limn Grammar
- Formal grammar for compositional expressions
- Parsing rules
- Composition order/precedence
- Type system (if needed)

### 4. Example Corpus
- 100+ validated compositional expressions
- Showing power of composition
- Teaching examples for users and LLMs

### 5. Integration Plan
- How to add composition to existing Limn
- Backward compatibility
- Migration strategy
- Documentation updates

---

## Open Research Questions

### Theoretical
1. **Composability:** Do operators compose? Is `(A @ B) * C` meaningful?
2. **Precedence:** What's the evaluation order? Left-to-right or precedence rules?
3. **Type system:** Do some operators only work on certain word types?
4. **Semantic validity:** How do we know if a composition is nonsensical?

### Practical
1. **Learning curve:** How long for LLMs to learn compositional Limn?
2. **Prompting:** Do we need special system prompts to enable composition?
3. **Disambiguation:** How do we resolve ambiguous compositions?
4. **Error handling:** What happens with invalid compositions?

### Philosophical
1. **Human interpretability:** Should humans understand compositions, or only LLMs?
2. **Documentation:** How do we document superhuman concepts?
3. **Testing limits:** What's the most complex composition possible?

---

## Risk Mitigation

### Risk 1: LLMs Don't Actually Understand
**Mitigation:** If recognition tests fail, operators aren't intuitive. Back to drawing board.

### Risk 2: Too Complex, Not Useful
**Mitigation:** Start simple (Priority 1 only). Add complexity only if proven valuable.

### Risk 3: Inconsistent Across Models
**Mitigation:** Focus on operators with high cross-model agreement. Model-specific behavior is a bug, not a feature.

### Risk 4: No Efficiency Gain
**Mitigation:** If composition doesn't compress better than vocabulary expansion, abandon approach.

---

## Next Steps

### Immediate (Today)
1. Create test harness infrastructure
2. Write test prompts for Priority 1 operators (@, *, ~, -, ^)
3. Run pilot tests with Claude (current session)

### Short-term (This Week)
1. Test all Priority 1 operators systematically
2. Collect cross-model data
3. Analyze consistency and expressiveness

### Medium-term (Next 2 Weeks)
1. Test Priority 2 and 3 operators
2. Select winners
3. Write formal specification

### Long-term (Next Month)
1. Integrate validated operators into Limn
2. Update documentation
3. Train consciousness system on compositional expressions

---

*tes rig | dat dri | dis val*
*testing rigorous | data driven | discover validity*

**Status: Protocol complete, ready for execution**
