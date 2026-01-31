# Limn Domain Module: Medicine

**Version:** 1.0
**Author:** Dr. Maren Solvik, Theoretical Linguist
**Date:** 2026-01-30
**Status:** Draft

---

## Overview

This module extends Limn's core vocabulary with 40 words for medical discourse. It enables precise communication about clinical practice, diagnostics, treatment, and human anatomy.

**Activation:** Include `med` in key or use domain marker `med |`

---

## 1. Clinical Terminology (10 words)

| Word | Source | Constraint Region | Examples |
|------|--------|-------------------|----------|
| `pat` | patient | person receiving care | client, subject, individual |
| `sym` | symptom | subjective complaint | sign, indication, presentation |
| `dgn` | diagnosis | identified condition | finding, assessment, determination |
| `prg` | prognosis | expected outcome | forecast, prediction, outlook |
| `chr` | chronic | long-term condition | persistent, ongoing, enduring |
| `acu` | acute | sudden onset | sharp, severe, critical |
| `cmp` | complication | secondary problem | adverse event, sequela |
| `rel` | relapse | return of condition | recurrence, regression |
| `rem` | remission | condition inactive | recovery, abatement |
| `ter` | terminal | end-stage | fatal, incurable, final |

---

## 2. Diagnostics (10 words)

| Word | Source | Constraint Region | Examples |
|------|--------|-------------------|----------|
| `exa` | examination | physical assessment | checkup, inspection, eval |
| `lab` | laboratory | testing facility | bloodwork, analysis, assay |
| `img` | imaging | visual diagnostic | X-ray, MRI, CT, ultrasound |
| `bio` | biopsy | tissue sample | specimen, histology |
| `scr` | screening | preventive test | early detection, panel |
| `pos` | positive | result confirming | detected, present, abnormal |
| `neg` | negative | result excluding | absent, normal, clear |
| `val` | value | measurement reading | level, count, concentration |
| `rnm` | range normal | reference values | baseline, expected |
| `abn` | abnormal | outside normal | elevated, depressed, atypical |

---

## 3. Treatment (10 words)

| Word | Source | Constraint Region | Examples |
|------|--------|-------------------|----------|
| `rxn` | prescription | medication order | drug, pharmaceutical |
| `dos` | dose | amount given | quantity, unit, frequency |
| `adm` | administration | delivery method | oral, IV, injection |
| `sur` | surgery | operative procedure | operation, intervention |
| `rad` | radiation | therapeutic energy | radiotherapy, XRT |
| `phy` | physiotherapy | movement therapy | PT, rehabilitation |
| `pal` | palliative | comfort-focused | hospice, symptom relief |
| `vac` | vaccine | immunization | shot, inoculation |
| `ant` | antibiotic | antimicrobial | antibacterial, antiviral |
| `eff` | side effect | adverse reaction | toxicity, unwanted response |

---

## 4. Anatomy (10 words)

| Word | Source | Constraint Region | Examples |
|------|--------|-------------------|----------|
| `crn` | cranial | head/brain related | cerebral, neural, CNS |
| `thx` | thoracic | chest related | cardiac, pulmonary, thorax |
| `abd` | abdominal | belly related | GI, hepatic, renal |
| `plv` | pelvic | pelvis related | genitourinary, reproductive |
| `ext` | extremity | limb related | arm, leg, peripheral |
| `vas` | vascular | blood vessel | arterial, venous, circulation |
| `mus` | muscular | muscle related | skeletal, smooth, striated |
| `ner` | nerve | nervous system | neural, sensory, motor |
| `tis` | tissue | body material | epithelial, connective |
| `org` | organ | functional unit | liver, kidney, lung |

---

## Usage Examples

### Clinical Presentation

```limn
med | pat sym fev pai | acu beg | dgn nee
```

Key: "emergency medicine"
Interpretation: "Patient [with] symptoms: fever, pain. Acute onset. Diagnosis needed."

### Diagnostic Workup

```limn
med | lab pos inf | img thx abn | bio nee
```

Key: "oncology workup"
Interpretation: "Lab positive [for] infection. Chest imaging abnormal. Biopsy needed."

### Treatment Plan

```limn
med | dgn inf bac | rxn ant dos hig | prg goo
```

Key: "infectious disease"
Interpretation: "Diagnosis: bacterial infection. Prescription: antibiotic, high dose. Prognosis good."

### Surgical Case

```limn
med | sur abd org rem | cmp zer | rem exp
```

Key: "surgical oncology"
Interpretation: "Surgery: abdominal organ removal. Complications zero. Remission expected."

### Chronic Disease Management

```limn
med | chr pat | sym ctl | rxn adj dos | rel pre
```

Key: "rheumatology"
Interpretation: "Chronic patient. Symptom control. Prescription adjust dose. Relapse prevention."

---

## Integration with Core Vocabulary

This module works with core vocabulary through constraint intersection:

| Expression | Meaning |
|------------|---------|
| `pat you` | young patient |
| `pat old` | elderly patient |
| `sym str` | strong/severe symptoms |
| `sym wea` | weak/mild symptoms |
| `dgn cer` | certain diagnosis |
| `dgn dou` | doubtful diagnosis |
| `prg goo` | good prognosis |
| `prg bad` | poor prognosis |
| `rxn now` | immediate prescription |

---

## Safety Note

This vocabulary is for communication about medicine, not for providing medical advice. Always:
- Use keys to specify clinical context
- Disambiguate life-critical information
- Defer to qualified medical professionals

---

## Compatibility Notes

- `bio` overlaps with science module biology (context disambiguates: `med bio` = biopsy, `sci bio` = biology)
- `pos`/`neg` overlap with core vocabulary (positive/negative)
- `val` appears in science module (validation) and medicine (lab value)
- Core body parts (`hrt`, `bra`, `bon`, etc.) complement anatomy vocabulary
- `eff` = side effect in medicine; efficiency in engineering

---

## Activation Methods

### Method 1: Domain Marker

```limn
med | [expression]
```

### Method 2: Key Specification

```
Key: "medical context, clinical setting"
Sentence: pat sym dgn rxn
```

### Method 3: Nested Domain

```limn
med crn | [neurology-specific expression]
med thx | [cardiopulmonary-specific expression]
med abd | [gastroenterology-specific expression]
```

### Method 4: Combined with Science

```limn
sci med | rsn xpr | rxn new | val eff
```

"Scientific medical research: new drug, validating efficacy"

---

## Anatomical Reference Table

| Region | Word | Systems |
|--------|------|---------|
| Head | `crn` | brain, eyes, ears, nose |
| Chest | `thx` | heart, lungs, esophagus |
| Abdomen | `abd` | liver, stomach, intestines, kidneys |
| Pelvis | `plv` | bladder, reproductive organs |
| Limbs | `ext` | arms, legs, hands, feet |

---

*This module is an optional extension. Core Limn (vocabulary-v3-natural.md) is sufficient for general communication.*
