# Limn Domain Module: Engineering

**Version:** 1.0
**Author:** Dr. Maren Solvik, Theoretical Linguist
**Date:** 2026-01-30
**Status:** Draft

---

## Overview

This module extends Limn's core vocabulary with 30 words for engineering discourse. It enables precise communication about systems design, component specification, manufacturing processes, and quality assurance.

**Activation:** Include `eng` in key or use domain marker `eng |`

---

## 1. Systems (10 words)

| Word | Source | Constraint Region | Examples |
|------|--------|-------------------|----------|
| `sys` | system | integrated whole | architecture, platform, network |
| `mod` | module | independent unit | component, package, subsystem |
| `int` | interface | connection point | API, port, boundary |
| `inp` | input | entering data/signal | feed, source, stimulus |
| `oup` | output | exiting data/signal | result, product, response |
| `arc` | architecture | structural design | blueprint, framework, schema |
| `dep` | dependency | requirement link | prerequisite, blocker, need |
| `red` | redundancy | backup, duplicate | failover, spare, resilience |
| `lat` | latency | delay, time lag | response time, wait, lag |
| `thr` | throughput | processing rate | bandwidth, capacity, volume |

---

## 2. Components (10 words)

| Word | Source | Constraint Region | Examples |
|------|--------|-------------------|----------|
| `cmp` | component | building block | part, element, unit |
| `sen` | sensor | input device | detector, probe, meter |
| `act` | actuator | output device | motor, valve, effector |
| `prc` | processor | computing unit | CPU, controller, logic |
| `mem` | memory | storage | buffer, cache, register |
| `bus` | bus | data pathway | channel, link, connection |
| `pwr` | power | energy supply | voltage, current, battery |
| `cir` | circuit | electronic path | board, chip, trace |
| `mch` | machine | mechanical system | device, apparatus, tool |
| `rob` | robot | autonomous machine | automaton, bot, agent |

---

## 3. Processes (5 words)

| Word | Source | Constraint Region | Examples |
|------|--------|-------------------|----------|
| `dsn` | design | planning phase | specification, blueprint |
| `prt` | prototype | early version | mockup, proof-of-concept |
| `mfg` | manufacture | production | fabrication, assembly |
| `tst` | test | verification | QA, validation, check |
| `dpl` | deploy | release, install | launch, rollout, ship |

---

## 4. Quality (5 words)

| Word | Source | Constraint Region | Examples |
|------|--------|-------------------|----------|
| `rel` | reliability | consistent function | uptime, MTBF, stable |
| `eff` | efficiency | resource ratio | performance, yield, ROI |
| `tol` | tolerance | acceptable range | margin, variance, spec |
| `flt` | fault | failure, defect | bug, error, breakdown |
| `fix` | fix | repair, patch | correction, remedy, workaround |

---

## Usage Examples

### Describing a System Architecture

```limn
eng | sys arc mod dep | int inp oup | rel eff
```

Key: "software architecture"
Interpretation: "System architecture [with] modular dependencies. Interfaces [for] input/output. Reliable [and] efficient."

### Component Specification

```limn
eng | sen tem inp | prc dat | oup act hea
```

Key: "HVAC control"
Interpretation: "Temperature sensor input. Processor [analyzes] data. Output [to] heating actuator."

### Manufacturing Process

```limn
eng | dsn spe | prt tst | mfg tol | dpl rel
```

Key: "product development"
Interpretation: "Design specification. Prototype [and] test. Manufacture [within] tolerance. Deploy reliably."

### Fault Analysis

```limn
eng | sys flt lat | cmp mem ful | fix red act
```

Key: "system debugging"
Interpretation: "System fault [causing] latency. Component memory full. Fix [by] activating redundancy."

---

## Integration with Core Vocabulary

This module works with core vocabulary through constraint intersection:

| Expression | Meaning |
|------------|---------|
| `sys big` | large system |
| `mod sma` | small module |
| `flt nu fix` | unfixed fault |
| `tst fai` | failed test |
| `dpl new` | new deployment |
| `rel str` | strong reliability |
| `eff hig` | high efficiency |

---

## Compatibility Notes

- `int` in engineering = interface; in core = intensity (context disambiguates)
- `act` = actuator; overlaps with core action vocabulary
- `pwr` = power; overlaps with core physical properties
- `mem` = memory; similar to core cognition vocabulary
- Domain markers enable precise disambiguation

---

## Activation Methods

### Method 1: Domain Marker

```limn
eng | [expression]
```

### Method 2: Key Specification

```
Key: "engineering context, systems design"
Sentence: sys arc mod int
```

### Method 3: Nested Domain

```limn
eng rob | [robotics-specific expression]
eng cir | [electronics-specific expression]
```

### Method 4: Combined with Science

```limn
sci eng | xpr sys tst | dat val
```

"Scientific engineering experiment: system test, data validation"

---

*This module is an optional extension. Core Limn (vocabulary-v3-natural.md) is sufficient for general communication.*
