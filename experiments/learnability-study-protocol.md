# Limn Human Learnability Study Protocol

**Version:** 1.0
**Status:** Ready for IRB Review
**Purpose:** Empirically test whether humans can learn Limn from the bootstrap document.

---

## 1. Study Overview

### 1.1 Research Questions

1. **RQ1:** Can participants learn to interpret novel Limn sentences after bootstrap exposure?
2. **RQ2:** Can participants produce valid Limn sentences for English prompts?
3. **RQ3:** Can participants correctly apply keys to disambiguate sentences?
4. **RQ4:** Does background (math/CS vs linguistics vs control) predict performance?
5. **RQ5:** What cognitive processes do participants report using?

### 1.2 Hypotheses

- **H1:** Participants with math/CS backgrounds will outperform linguistics students on interpretation tasks (geometric reasoning advantage)
- **H2:** Linguistics students will outperform on production tasks (language production experience)
- **H3:** All groups will struggle with operator scope initially
- **H4:** Key application will correlate with domain expertise (biologists better with biology keys)

---

## 2. Participant Selection

### 2.1 Target Groups

| Group | N | Background | Recruitment |
|-------|---|------------|-------------|
| A | 20 | Mathematics/CS students | University math/CS departments |
| B | 20 | Linguistics students | University linguistics departments |
| C | 20 | General population | Prolific/MTurk with screening |

### 2.2 Inclusion Criteria

- Age 18-45
- Native English speaker or high proficiency (CEFR C1+)
- No prior exposure to Limn or similar constraint languages
- Normal or corrected-to-normal vision
- Able to commit 2-hour session

### 2.3 Exclusion Criteria

- Prior exposure to Lojban, Ithkuil, or formal semantics courses
- Cognitive impairments affecting reasoning
- Incomplete bootstrap exposure phase

### 2.4 Screening Questionnaire

1. What is your primary field of study/work?
2. Rate your comfort with mathematics (1-5)
3. Rate your comfort with learning new languages (1-5)
4. Have you studied formal logic? (Y/N, if Y: how much?)
5. Have you studied any constructed languages? (Y/N, if Y: which?)
6. Describe your experience with puzzles or pattern recognition (brief)

---

## 3. Study Design

### 3.1 Session Structure (120 minutes total)

| Phase | Duration | Activity |
|-------|----------|----------|
| 1 | 5 min | Consent and screening verification |
| 2 | 30 min | Bootstrap document exposure (self-paced reading) |
| 3 | 5 min | Break |
| 4 | 25 min | Comprehension test (interpretation) |
| 5 | 5 min | Break |
| 6 | 20 min | Production test (composition) |
| 7 | 5 min | Break |
| 8 | 15 min | Key application test |
| 9 | 10 min | Exit interview and debrief |

### 3.2 Bootstrap Exposure Phase

Participants receive:
- bootstrap-v1.md (40-word vocabulary)
- Self-paced reading with no time limit (within 30 min)
- Allowed to take notes
- No external references permitted

**Attention check:** Embedded questions during reading to verify engagement.

---

## 4. Test Materials

### 4.1 Comprehension Test (25 items)

#### Section A: Two-Word Sentences (10 items)
Generate at least 5 interpretations for each:

1. `ko su` (solid + above)
2. `vi ta` (alive + beginning)
3. `mu du` (dark + ongoing)
4. `he ra` (hot + linear)
5. `lu fi` (bright + ending)
6. `fu bi` (unknown + connecting)
7. `wo ka` (waiting + sudden)
8. `ga ma` (positive + increasing)
9. `zo le` (negative + decreasing)
10. `si ri` (known + cyclic)

**Scoring:**
- 1 point per valid interpretation (within constraint region)
- 0.5 points for borderline interpretations
- 0 points for invalid (outside constraint region)
- Max 10 points per item

#### Section B: Three-Word Sentences (10 items)
Generate at least 5 interpretations:

1. `vi ta fi` (alive + beginning + ending)
2. `ko mu du` (solid + dark + ongoing)
3. `lu ra he` (bright + linear + hot)
4. `wo bi sa` (waiting + connecting + same)
5. `nu vi du` (NOT alive + ongoing)
6. `fu ke ta` (unknown + surprising + beginning)
7. `ga ma ri` (positive + increasing + cyclic)
8. `mi bi no` (dispersed + connecting + different)
9. `su ra fi` (above + linear + ending)
10. `nu mu vi` (NOT dark + alive)

**Scoring:** Same as Section A

#### Section C: Complex Sentences (5 items)
Generate at least 3 interpretations:

1. `yo lu | an mu` (this-bright | that-dark)
2. `nu ko su | vi ta` (NOT-solid + above | alive + beginning)
3. `ve he ra du` (VERY-hot + linear + ongoing)
4. `mu vi ka ta | lu wo fi` (dark + alive + sudden + beginning | bright + waiting + ending)
5. `so ga bi | ve zo no` (WEAKLY-positive + connecting | VERY-negative + different)

**Scoring:** Same as Section A

### 4.2 Production Test (20 items)

Create a Limn sentence for each English concept:

#### Section A: Simple Concepts (10 items)
1. A mountain
2. A river flowing
3. Something dying
4. A cold night
5. A growing child
6. A sudden flash
7. Something hidden waiting
8. Two different things connected
9. A repeating pattern
10. Something positive ending

#### Section B: Complex Concepts (10 items)
1. A storm approaching at dawn
2. Life emerging from darkness
3. The known becoming unknown
4. Heat spreading outward continuously
5. Something solid breaking into many pieces
6. A cycle of growth and decline
7. Light and darkness separated but connected
8. An unexpected positive change
9. Something ancient still alive
10. The center of something hidden

**Scoring:**
- 2 points: Correct constraint intersection
- 1 point: Partially correct (some constraints wrong)
- 0 points: Invalid or unrelated

### 4.3 Key Application Test (15 items)

For each sentence, apply the given key and provide 1-2 specific interpretations:

| # | Sentence | Key |
|---|----------|-----|
| 1 | `vi ta fi` | "insects" |
| 2 | `vi ta fi` | "relationships" |
| 3 | `ko su du` | "architecture" |
| 4 | `ko su du` | "geology" |
| 5 | `mu vi ka` | "horror" |
| 6 | `mu vi ka` | "gardening" |
| 7 | `ra du he` | "physics" |
| 8 | `ra du he` | "emotions" |
| 9 | `lu fi | mu ta` | "astronomy" |
| 10 | `lu fi | mu ta` | "psychology" |
| 11 | `nu vi ko du` | "technology" |
| 12 | `nu vi ko du` | "philosophy" |
| 13 | `ga ma ri` | "economics" |
| 14 | `ga ma ri` | "biology" |
| 15 | `yo fu | an si` | "learning" |

**Scoring:**
- 2 points: Interpretation correctly matches key domain
- 1 point: Interpretation partially matches key
- 0 points: Interpretation ignores key or is invalid

---

## 5. Exit Interview Protocol

### 5.1 Semi-Structured Questions

1. How would you describe the experience of learning Limn?
2. What was the easiest aspect? The hardest?
3. What strategies did you use to interpret sentences?
4. Did you visualize anything when thinking about sentences? If so, what?
5. How did you approach composition (creating sentences)?
6. Did the key mechanism feel intuitive or confusing?
7. Would you describe Limn as more like math or more like language?
8. Any other thoughts about the experience?

### 5.2 Rating Scales

Rate each (1-7 scale):
- Overall difficulty of learning Limn
- Similarity to learning a natural language
- Similarity to learning mathematics
- Usefulness of the bootstrap document
- Confidence in your interpretation abilities
- Confidence in your composition abilities
- Interest in learning more about Limn

---

## 6. Analysis Plan

### 6.1 Primary Analyses

| Hypothesis | Test | Variables |
|------------|------|-----------|
| H1 | ANOVA | Group (A/B/C) x Comprehension score |
| H2 | ANOVA | Group x Production score |
| H3 | Item analysis | Operator items vs non-operator items |
| H4 | Correlation | Domain expertise x Key application score |

### 6.2 Secondary Analyses

- Learning curves across test sections
- Error pattern analysis (common mistakes)
- Qualitative coding of interview responses
- Correlation between self-reported strategies and performance

### 6.3 Exploratory Analyses

- Relationship between math comfort and comprehension
- Effect of note-taking during bootstrap phase
- Time-on-task correlations

---

## 7. Ethical Considerations

### 7.1 Consent Process

- Written informed consent before participation
- Right to withdraw at any time without penalty
- Explanation of data use and anonymization

### 7.2 Data Protection

- All data anonymized with participant codes
- No PII collected beyond demographics
- Data stored on encrypted university servers
- Retention: 5 years post-publication

### 7.3 Compensation

- $25 Amazon gift card for full participation
- Prorated ($15) for early withdrawal
- Optional: Extra credit for student participants

### 7.4 Risk Assessment

- **Minimal risk:** Cognitive fatigue possible
- **Mitigation:** Scheduled breaks, self-paced exposure
- **Debrief:** Full explanation of study purpose and Limn

---

## 8. Materials Checklist

### 8.1 Before Session
- [ ] Consent forms printed
- [ ] Screening questionnaire ready
- [ ] Bootstrap document (paper or tablet)
- [ ] Note paper and pens
- [ ] Test booklets (A, B, C)
- [ ] Timer/stopwatch
- [ ] Audio recorder (for interview)

### 8.2 Digital Materials
- [ ] Online test platform (if applicable)
- [ ] Backup paper tests
- [ ] Scoring rubrics
- [ ] Interview recording software

### 8.3 Post-Session
- [ ] Debriefing script
- [ ] Compensation tracking
- [ ] Data entry forms

---

## 9. Scoring Rubrics

### 9.1 Interpretation Validity Rubric

| Score | Criteria |
|-------|----------|
| 1.0 | Interpretation clearly within all constraint regions |
| 0.5 | Interpretation within most constraints, borderline on 1 |
| 0.0 | Interpretation violates 2+ constraints or is unrelated |

**Examples for `ko su` (solid + above):**
- "Mountain peak" = 1.0 (solid, above)
- "Airplane" = 0.5 (arguably solid, definitely above, but temporary)
- "River" = 0.0 (not solid)

### 9.2 Production Validity Rubric

| Score | Criteria |
|-------|----------|
| 2.0 | All relevant constraints captured, valid word choices |
| 1.0 | Some constraints captured, minor errors |
| 0.0 | Major constraints missing or invalid words used |

**Example for "A growing child":**
- `vi ma ta` = 2.0 (alive + increasing + beginning)
- `vi ma` = 1.0 (alive + increasing, missing "child" aspect)
- `ko du` = 0.0 (solid + ongoing, not child-related)

### 9.3 Key Application Rubric

| Score | Criteria |
|-------|----------|
| 2.0 | Interpretation clearly within key domain |
| 1.0 | Interpretation partially within key domain |
| 0.0 | Interpretation ignores key or uses wrong domain |

---

## 10. Timeline

| Week | Activity |
|------|----------|
| 1-2 | IRB submission and approval |
| 3-4 | Participant recruitment |
| 5-8 | Data collection (20 sessions/week) |
| 9-10 | Data entry and cleaning |
| 11-12 | Analysis |
| 13-14 | Write-up |
| 15 | Internal review |
| 16+ | Submission |

---

## Appendix A: Sample Consent Form

[Standard institutional consent form template - customize per institution]

## Appendix B: Full Bootstrap Document

[Include bootstrap-v1.md as appendix]

## Appendix C: Answer Keys

[Detailed rubrics for each test item - for experimenter use only]

---

**END OF PROTOCOL**
