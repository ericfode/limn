# Spy Limn Glossary

*Field vocabulary for operational Limn*

---

## Agents & Roles

| Limn | Surface Reading | True Reading |
|------|-----------------|--------------|
| `age` | agent (generic) | operative |
| `han` | hand | handler |
| `eye` | eye | surveillance |
| `ear` | ear | signals intelligence |
| `mou` | mouth | informant |
| `hed` | head | station chief |
| `foo` | foot | ground asset |
| `hrt` | heart | trusted inner circle |
| `bra` | brain | analyst |

## Locations

| Limn | Surface Reading | True Reading |
|------|-----------------|--------------|
| `loc` | location | safe house |
| `nes` | nest | surveillance post |
| `hom` | home | headquarters |
| `for` | foreign | enemy territory |
| `nea` | near | friendly territory |
| `bri` | bridge | border crossing |
| `dep` | deep | undercover position |
| `hig` | high | elevated risk zone |

## Actions

| Limn | Surface Reading | True Reading |
|------|-----------------|--------------|
| `mov` | move | relocate/extract |
| `res` | rest | go dark |
| `see` | see | conduct surveillance |
| `lis` | listen | intercept comms |
| `spe` | speak | transmit |
| `hid` | hide | establish cover |
| `sho` | show | expose/reveal |
| `cut` | cut | terminate contact |
| `joi` | join | make contact |
| `flo` | flow | maintain routine |
| `blo` | blood | casualties/violence |
| `bur` | burn | compromised/blown |

## Status

| Limn | Surface Reading | True Reading |
|------|-----------------|--------------|
| `saf` | safe | secure |
| `dan` | danger | compromised |
| `hot` | hot | active pursuit |
| `col` | cold | dormant/safe |
| `bri` | bright | visible/exposed |
| `nox` | dark | covert/hidden |
| `cle` | clear | unmonitored |
| `wat` | watched | under surveillance |

## Time

| Limn | Surface Reading | True Reading |
|------|-----------------|--------------|
| `now` | now | immediate action |
| `soo` | soon | within hours |
| `lat` | later | within days |
| `lon` | long | extended operation |
| `sho` | short | time critical |
| `end` | end | mission complete |
| `beg` | begin | operation start |

## Priority/Urgency

| Limn | Surface Reading | True Reading |
|------|-----------------|--------------|
| `ve` | very | critical priority |
| `so` | somewhat | standard priority |
| `mus` | must | mandatory |
| `may` | may | optional |
| `nu` | not | abort/negative |

## Trust Levels

| Limn | Surface Reading | True Reading |
|------|-----------------|--------------|
| `tru` | trust | verified asset |
| `dou` | doubt | unverified |
| `fak` | fake | known hostile |
| `unk` | unknown | unassessed |
| `tur` | turn | doubled agent |

## Message Types

| Limn | Surface Reading | True Reading |
|------|-----------------|--------------|
| `rep` | report | intelligence report |
| `ord` | order | operational directive |
| `war` | warning | threat alert |
| `req` | request | resource request |
| `con` | confirm | acknowledgment |
| `den` | deny | negative response |

---

## Key Markers

In operational Limn, keys are never transmitted with messages. Keys are:
- Pre-arranged by codebook
- Derived from date/time
- Based on relationship context
- Sometimes embedded in message structure

**Key Types:**

| Key Level | Who Has It | Purpose |
|-----------|------------|---------|
| SURFACE | Everyone | Innocuous reading |
| COVER | Allied services | Plausible deniability |
| TRUE | Your network | Actual orders |
| GHOST | Sender + recipient only | Deniable even to allies |

---

## Sample Operational Messages

### Example 1: Routine Check-in

```limn
age flo | loc cle | tim reg
```

**SURFACE**: "Personnel flowing. Location clear. Time regular."
= Normal office activity

**COVER**: "Asset maintaining routine. Site unmonitored. Standard schedule."
= Nothing unusual

**TRUE**: "Operative in position. Safe house secure. On schedule."
= Mission proceeding

---

### Example 2: Emergency

```limn
age mov ve fas | loc bur | req ext now
```

**SURFACE**: "Person moving very fast. Place busy. Requesting exit now."
= Someone leaving a crowded venue

**COVER**: "Asset relocating urgently. Position busy. Needs extraction window."
= Cover blown, needs pickup

**TRUE**: "Operative fleeing. Safe house compromised. Emergency extraction immediate."
= Life or death

---

### Example 3: Ambiguous (Requires Right Key)

```limn
eye see bri | han mov beh | res
```

**KEY: FRIENDLY** → "Surveillance spotted. Handler moving behind. Rest."
= Abort, you're being watched, handler will contact you

**KEY: HOSTILE** → "We see the target. Move in from behind. Execute."
= Attack order

Same words. Opposite meanings. The key is everything.

---

## Common Patterns

### Negation Flip
```limn
nu dan        # SURFACE: "not dangerous" / TRUE: "abort, danger"
dan nu        # SURFACE: "danger not" (awkward) / TRUE: "danger negated, proceed"
```

### Scope Split
```limn
A | B         # Topic A, comment B
A B | C D     # (A ∩ B) beside (C ∩ D) - two separate statements
```

### Ghost Layer
```limn
age mov | loc cha | ???
```
If you know the Ghost key, there's a third segment you can read.
If you don't, you don't even see the `???`.

---

## Tone Operators

*Critical for spy communications - tone changes everything*

| Operator | Meaning | Spy Usage |
|----------|---------|-----------|
| `frm` | formal | Official channel, by-the-book |
| `cas` | casual | Informal, trusted contact |
| `iro` | ironic | Opposite meaning, double-speak |
| `sin` | sincere | Genuine, no deception layer |
| `urj` | urgent | Time-critical, life-or-death |
| `hes` | hesitant | Uncertain, possibly coerced |
| `te` | question (?) | Requesting confirmation |
| `we` | imperative (!) | Direct order |

### Tone in Operations

**Urgent dead drop:**
```limn
urj age mov now | loc bur | we ext
```
*URGENT: Agent move now. Location burned. EXTRACT!*

**Ironic double-meaning:**
```limn
iro tru fri | joi din
```
*SURFACE: "Truly a friend, join for dinner"*
*TRUE (with iro): Enemy asset, do NOT meet*

**Hesitant (possibly coerced):**
```limn
hes saf | hes loc cle | hes con
```
*If asset sends hesitant confirms, assume compromise*

**Formal vs Casual:**
```limn
frm req ext | pro 7         # Official extraction request, protocol 7
cas nee out | fas            # Casual: "Need out, fast" - trusted contact
```

**Question markers:**
```limn
te loc saf                   # Is location safe?
te age tru                   # Is agent trustworthy?
```

### Tone as Key Layer

Tone operators can function as a hidden key layer:

```limn
iro goo new | fri com
```

**Without IRO key**: "Good news! Friend coming."
**With IRO key**: Bad news. Enemy incoming.

The `iro` operator inverts the entire message. If you don't recognize it as a key, you read the opposite of truth.

---

## Vocabulary Notes

- `shr` = share (not `sha` which = shame)
- `dam` = block/dam (not `blo` which = blood)
- `sim` = simple (not `eas` which = east)
- `nar` = narrow/thin (not `thi` which = think)

---

*In the field, vocabulary mistakes get people killed. Know your words. Know your tone.*
