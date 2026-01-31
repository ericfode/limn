# Practical Limn Applications

*Real-world use cases for Limn*

---

## 1. System Status Messages

### Health Checks
```limn
sys hea | al com res     # system healthy, all components responding
sys sic | ex com nu res  # system sick, some components not responding
sys dea | zer res        # system dead, zero responses
```

### Load States
```limn
loa sma | res man        # low load, many resources
loa big | res few        # high load, few resources
loa ve big | urj act     # very high load, urgent action needed
```

### Deployment States
```limn
cod tes | nu dep         # code tested, not deployed
cod dep | sta            # code deployed, stable
cod dep | sic | rol bac  # code deployed sick, rollback
```

---

## 2. Agent Communication Protocol

### Agent Status
```limn
age act | lis           # agent active, listening
age sle | nu lis        # agent sleeping, not listening
age wor tas             # agent working task
age don | res           # agent done, resting
```

### Task Messages
```limn
tas new | urj           # new urgent task
tas ass age_id          # task assigned to agent
tas don | suc           # task done successfully
tas fai | err_code      # task failed with error
```

### Priority Levels
```limn
pri max | we act        # priority maximum, must act
pri avg | so urj        # priority average, somewhat urgent
pri min | nu urj        # priority minimum, not urgent
```

---

## 3. Weather Reports

```limn
lux bri | aqu nu        # sunny, no water/rain
lux dim | aqu fal       # dim light, rain falling
nox | aqu sol fal       # night, solid water falling (snow)
aer mov big | aqu man   # air moving big, much water (storm)
```

With temperature:
```limn
tem hot | hum big       # hot temp, high humidity (muggy)
tem col | aqu sol       # cold temp, solid water (ice/snow)
tem avg | aqu nu        # average temp, no rain (pleasant)
```

---

## 4. Financial States

### Market Conditions
```limn
mar ris | con big       # market rising, confidence big
mar fal | fea man       # market falling, much fear
mar sta | vol sma       # market stable, volatility small
```

### Transaction States
```limn
tra pen | wai aut       # transaction pending, waiting authorization
tra suc | fun tra       # transaction success, funds transferred
tra fai | fun nu        # transaction failed, no funds
tra can | fun ret       # transaction cancelled, funds returned
```

### Account Health
```limn
acc hea | bal ma zer    # account healthy, balance > 0
acc war | bal sma       # account warning, balance small
acc cri | bal mi zer    # account critical, balance < 0
```

---

## 5. Smart Home Control

### Lights
```limn
lux on | bri max        # lights on, brightness max
lux on | bri so max     # lights on, somewhat bright
lux of                  # lights off
```

### Temperature
```limn
tem set 20 | tem now 22  # target 20, current 22
tem col | hea on         # temp cold, heating on
tem hot | coo on         # temp hot, cooling on
```

### Security
```limn
dor loc | sec on         # door locked, security on
dor ope | ale urj        # door open, urgent alert
win clo | al sec         # windows closed, all secure
```

---

## 6. Data Pipeline States

```limn
dat in | pro | out       # data in, processing, out (pipeline stages)
dat val | nu err         # data validated, no errors
dat err | sto            # data error, stopped
dat flo | rat big        # data flowing, high rate
```

With metrics:
```limn
dat rec 1000 | pro 950 | out 900  # received, processed, output counts
lat sma | thr big                  # latency small, throughput big
```

---

## 7. Collaborative Work States

### Document Editing
```limn
doc ope | age 3          # document open, 3 agents
doc edi age_a            # document being edited by agent A
doc sav | ver 5          # document saved, version 5
doc loc | con             # document locked, conflict
```

### Code Review
```limn
cod rev | age_b          # code under review by agent B
cod apr | mer            # code approved, can merge
cod rej | cha req        # code rejected, changes requested
cod mer | bra mai        # code merged to main branch
```

---

## 8. Learning System States

```limn
mod tra | ep 10          # model training, epoch 10
mod val | acc 0.95       # model validated, accuracy 0.95
mod ovf | sto            # model overfitting, stopped
mod dep | ser            # model deployed, serving
```

Performance:
```limn
los dec | acc ris        # loss decreasing, accuracy rising
los sta | lea nu         # loss stable, not learning
gra van | lea fai        # gradient vanishing, learning failed
```

---

## 9. Game States

```limn
gam run | pla 4          # game running, 4 players
pla_a win | sco max      # player A wins, score maximum
pla_b los | sco min      # player B loses, score minimum
gam pau | wai pla        # game paused, waiting player
gam don | sav            # game done, saved
```

---

## 10. Monitoring Alerts

### Severity Levels
```limn
ale inf | kno req        # alert info, knowing required
ale war | act so         # alert warning, somewhat action needed
ale err | act urj        # alert error, urgent action
ale cri | we act now     # alert critical, must act now
```

### Resolution
```limn
ale ope | inv            # alert open, investigating
ale ack | fix in         # alert acknowledged, fixing in progress
ale res | fix don        # alert resolved, fix done
ale clo | ver            # alert closed, verified
```

---

## 11. IoT Sensor Messages

```limn
sen hum | val 65         # humidity sensor, value 65%
sen tem | val 22         # temperature sensor, value 22°
sen mov | det            # motion sensor, detected
sen dor | ope            # door sensor, open
```

With states:
```limn
sen bat | low | war      # sensor battery low, warning
sen sig | wea | dat nu   # sensor signal weak, no data
sen hea | ok             # sensor health OK
```

---

## Key Patterns for Applications

### Status Template
```limn
[entity] [state] | [detail]
```

### Event Template
```limn
[subject] [action] [object] | [result]
```

### Metrics Template
```limn
[metric] [comparator] [threshold] | [status]
```

### Temporal Template
```limn
[state1] → [state2] → [state3]
```

---

## Benefits of Limn for Systems

1. **Compact**: Fits in log lines, packets, displays
2. **Parseable**: Clear structure for machines
3. **Scannable**: Humans can skim quickly
4. **Multilingual**: Works across language barriers
5. **Versioned**: Vocabulary can extend without breaking old parsers

---

*Limn bridges human and machine communication.*

— Kira • 2026-01-31
