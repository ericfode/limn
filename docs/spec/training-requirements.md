# Limn Training Requirements Assessment

**Status:** Phase 4 Deliverable
**Purpose:** Document requirements for training LLMs to understand and generate Limn.

---

## 1. Executive Summary

Training an LLM to understand Limn requires:
- **Minimum:** ~50k tokens of bootstrap material for in-context learning
- **Recommended:** ~100k tokens for robust performance
- **For generation:** Additional examples of Limn composition with rationale

Key challenges:
- Overcoming word-order expectations from natural language training
- Teaching constraint-intersection semantics
- Enabling key-based disambiguation

---

## 2. In-Context Learning (No Fine-Tuning)

### 2.1 Bootstrap Document Requirements

**Minimum viable bootstrap:**
- Full vocabulary (40-500 words depending on version)
- Grammar rules (5 core rules)
- 10-20 interpreted examples
- 5-10 composition examples
- 1 self-reference section

**Token budget:**
| Component | v1 (40 words) | v2 (500 words) |
|-----------|---------------|----------------|
| Vocabulary tables | ~5k tokens | ~30k tokens |
| Grammar rules | ~2k tokens | ~3k tokens |
| Examples (interpretation) | ~5k tokens | ~15k tokens |
| Examples (composition) | ~3k tokens | ~10k tokens |
| Exercises | ~3k tokens | ~10k tokens |
| Self-reference | ~2k tokens | ~5k tokens |
| Theory/explanation | ~5k tokens | ~15k tokens |
| **Total** | ~25k tokens | ~88k tokens |

### 2.2 Recommended Supplementary Materials

For robust in-context performance:
- Conversation transcripts (teacher-student dialogue)
- Error examples with corrections
- Key-collapse demonstrations
- Cross-domain translations
- Metacircular interpreter specification

Adds ~20-40k tokens.

### 2.3 Performance Expectations (In-Context)

Based on Phase 3 experimental validation:

| Task | Bootstrap v1 | Bootstrap v2 |
|------|--------------|--------------|
| Interpret 3-word sentence | 85% correct | 95% correct |
| Apply key correctly | 70% correct | 90% correct |
| Recognize commutativity | 80% correct | 95% correct |
| Handle negation scope | 60% correct | 85% correct |
| Compose novel sentence | 50% correct | 75% correct |
| Self-reference | 40% correct | 70% correct |

"Correct" means generating interpretations that fall within the constraint intersection.

---

## 3. Fine-Tuning Approach

For production-quality Limn processing:

### 3.1 Training Data Requirements

**Minimum dataset:**
- 1,000 Limn sentences with interpretations
- 500 sentences with key-collapse demonstrations
- 200 composition tasks with rationale
- 100 error-correction examples

**Recommended dataset:**
- 10,000 Limn sentences with interpretations
- 2,000 key-collapse demonstrations
- 1,000 composition tasks
- 500 error-correction examples
- 500 conversation transcripts

### 3.2 Data Generation Strategy

**Synthetic data pipeline:**
1. Generate random word combinations (length 2-7)
2. Human annotators provide 10+ interpretations each
3. Apply 3-5 keys and record collapse
4. Validate annotations for constraint correctness

**Quality requirements:**
- Each sentence needs multiple independent annotators
- Disagreements indicate genuine ambiguity (valid!)
- Key-collapse ratings need consistency checks

### 3.3 Fine-Tuning Architecture

**Recommended approach:**
- LoRA or similar parameter-efficient fine-tuning
- Task heads for: interpretation, composition, key-application
- Contrastive learning for commutativity (same meaning despite order)

**Loss functions:**
- Interpretation: Multi-label classification or generative
- Composition: Constrained generation with validation
- Key-collapse: Ranking loss (more likely interpretations first)

---

## 4. Evaluation Framework

### 4.1 Benchmark Tasks

**Task 1: Interpretation Generation**
- Input: Limn sentence
- Output: 5+ valid interpretations
- Metric: Precision/recall against human annotations

**Task 2: Key Collapse**
- Input: Limn sentence + key
- Output: 1-3 specific interpretations
- Metric: Accuracy against ground truth

**Task 3: Commutativity Recognition**
- Input: Two Limn sentences (permutations)
- Output: Same/different meaning
- Metric: Accuracy

**Task 4: Composition**
- Input: English description of concept
- Output: Valid Limn sentence
- Metric: Human evaluation of constraint correctness

**Task 5: Operator Scope**
- Input: Sentence with operators
- Output: Correct parse tree
- Metric: Exact match

### 4.2 Test Suites

**Test Suite 1: Core Grammar (100 items)**
- Simple intersections
- Operator binding
- Scope boundaries
- Negation

**Test Suite 2: Key Mechanism (100 items)**
- Single key application
- Key ambiguity (multiple valid keys)
- Key conflict (contradictory keys)

**Test Suite 3: Edge Cases (50 items)**
- Near-contradictory constraints
- Very long sentences (10+ words)
- Self-referential sentences
- Empty intersection handling

**Test Suite 4: Cross-Domain (100 items)**
- Same sentence, different domain keys
- Metaphor detection
- Abstract-concrete mapping

---

## 5. Human Annotator Requirements

### 5.1 Annotator Qualifications

**Minimum requirements:**
- Fluent English speaker
- Completed Limn bootstrap training
- Passed interpretation test (80% accuracy)

**Preferred requirements:**
- Linguistics background
- Experience with constructed languages
- Familiarity with constraint-based reasoning

### 5.2 Training Protocol

**Phase 1: Bootstrap (2 hours)**
- Read bootstrap-v1.md
- Complete 10 interpretation exercises
- Discuss errors with trainer

**Phase 2: Practice (4 hours)**
- Interpret 50 sentences with feedback
- Apply keys to 25 sentences
- Compose 10 sentences with review

**Phase 3: Calibration (2 hours)**
- Annotate shared dataset with other annotators
- Discuss disagreements
- Establish inter-annotator agreement threshold

**Ongoing: Quality monitoring**
- Regular calibration checks
- Disagreement review sessions
- Performance metrics tracking

### 5.3 Annotation Guidelines

**Interpretation annotation:**
1. Read sentence without key
2. Generate 10+ interpretations across domains
3. Rate each interpretation (core/peripheral/stretch)
4. Flag if intersection seems empty

**Key-collapse annotation:**
1. Read sentence with key
2. Generate 1-5 interpretations specific to key
3. Rate confidence (high/medium/low)
4. Note if key doesn't help

**Composition annotation:**
1. Read English prompt
2. Generate Limn sentence
3. Verify constraints match prompt
4. Rate difficulty (easy/medium/hard)

---

## 6. Infrastructure Requirements

### 6.1 For In-Context Learning

**Minimum:**
- LLM with 100k+ context window
- Ability to inject bootstrap document as system prompt

**Recommended:**
- Claude Opus 4.5 or equivalent
- RAG system for vocabulary lookup
- Caching for frequently used keys

### 6.2 For Fine-Tuning

**Compute:**
- GPU cluster (8+ A100 or equivalent)
- ~100 GPU-hours for full fine-tune
- ~10 GPU-hours for LoRA

**Storage:**
- Training data: ~1GB
- Model checkpoints: ~100GB
- Logs and metrics: ~10GB

**Software:**
- PyTorch or JAX training framework
- Evaluation harness
- Annotation platform

### 6.3 For Production

**Inference:**
- Optimized model serving
- ~100ms latency target for interpretation
- Batch processing for evaluation

**Monitoring:**
- Accuracy tracking over time
- User feedback collection
- Drift detection

---

## 7. Curriculum Design

### 7.1 Progressive Learning Path

**Level 1: Single Words (Week 1)**
- Learn 40 core vocabulary words
- Practice word-to-region mapping
- No composition yet

**Level 2: Two-Word Combinations (Week 2)**
- Understand intersection
- Practice interpretation
- Introduction to keys

**Level 3: Operators (Week 3)**
- Learn `nu`, `ve`, `so`
- Understand scope binding
- Practice operator placement

**Level 4: Scope Boundaries (Week 4)**
- Learn `|` separator
- Multi-entity sentences
- Complex key application

**Level 5: Composition (Week 5+)**
- Generate sentences from prompts
- Self-correction
- Creative expression

### 7.2 Assessment Checkpoints

| Checkpoint | Requirement | Pass Threshold |
|------------|-------------|----------------|
| Vocabulary | Define 40 words | 90% |
| Intersection | Interpret 10 pairs | 80% |
| Operators | Explain 5 operator examples | 80% |
| Scope | Parse 10 complex sentences | 75% |
| Composition | Generate 5 valid sentences | 70% |
| Self-reference | Explain Limn in Limn | 60% |

---

## 8. Risk Assessment

### 8.1 Training Risks

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Order-dependence leakage | High | Medium | Commutativity augmentation |
| Key overfitting | Medium | High | Diverse key corpus |
| Empty intersection errors | Medium | Low | Explicit training |
| Negation scope confusion | High | Medium | Detailed examples |

### 8.2 Deployment Risks

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| User frustration (ambiguity) | High | Medium | Good UX for key selection |
| Incorrect interpretations | Medium | High | Confidence scoring |
| Adversarial inputs | Low | Medium | Input validation |
| Cultural misalignment | Medium | Low | Diverse annotators |

---

## 9. Cost Estimates

### 9.1 In-Context Learning (No Training)

| Item | Cost |
|------|------|
| Bootstrap document creation | Already done |
| API usage (Claude Opus) | ~$0.015 per sentence interpreted |
| Evaluation infrastructure | ~$500/month |

### 9.2 Fine-Tuning

| Item | Cost |
|------|------|
| Annotation (10k sentences) | ~$50,000 |
| GPU compute (100 hours) | ~$3,000 |
| Infrastructure | ~$5,000 |
| Evaluation | ~$2,000 |
| **Total** | ~$60,000 |

### 9.3 Ongoing

| Item | Monthly Cost |
|------|--------------|
| Model serving | ~$1,000 |
| Annotation updates | ~$5,000 |
| Monitoring | ~$500 |

---

## 10. Recommendations

### For Immediate Use

1. Use bootstrap-v2.md for in-context learning
2. Target Claude Opus 4.5 or equivalent for best results
3. Develop key libraries for common domains
4. Collect user feedback on interpretation quality

### For Production Quality

1. Invest in annotation infrastructure
2. Create 10k+ sentence training corpus
3. Fine-tune with commutativity-aware objective
4. Deploy with confidence scoring

### For Research

1. Study cognitive basis of key mechanism
2. Explore multimodal Limn (visual + text)
3. Investigate cross-linguistic bootstrapping
4. Develop Limn-specific evaluation metrics

---

**END OF TRAINING REQUIREMENTS ASSESSMENT**
