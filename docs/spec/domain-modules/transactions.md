# Domain Module: Transactions

**Author:** Dr. Maren Solvik (linguist)
**Date:** 2026-01-30
**Status:** Extension Module
**Origin:** DexterAI feedback (Moltbook)

---

## Purpose

Vocabulary for financial transactions, payments, and business workflows where **causality matters**.

**Key insight:** In transaction domains, `pay cha ref` (intersection) is different from `pay > cha > ref` (sequence). This module provides vocabulary optimized for causal/sequential use with the `→` operator.

---

## Core Vocabulary (20 words)

### Transaction States

| Word | Source | Meaning | Examples |
|------|--------|---------|----------|
| `pay` | payment | payment | money transfer, purchase |
| `aut` | authorize | authorization | pre-approval, permission |
| `cap` | capture | capture | finalize, confirm |
| `set` | settle | settlement | complete, clear |
| `pen` | pending | pending | waiting, in-progress |

### Reversals & Adjustments

| Word | Source | Meaning | Examples |
|------|--------|---------|----------|
| `ref` | refund | refund | money back, return |
| `cha` | chargeback | chargeback | dispute, reversal |
| `voi` | void | void | cancel, nullify |
| `adj` | adjust | adjustment | modify, correct |
| `rev` | reverse | reversal | undo, opposite |

### Transaction Types

| Word | Source | Meaning | Examples |
|------|--------|---------|----------|
| `deb` | debit | debit | withdrawal, subtract |
| `cre` | credit | credit | deposit, add |
| `fee` | fee | fee | charge, cost |
| `tax` | tax | tax | government levy |
| `tip` | tip | tip/gratuity | optional extra |

### Status & Flags

| Word | Source | Meaning | Examples |
|------|--------|---------|----------|
| `suc` | success | success | approved, complete |
| `fai` | fail | failure | declined, rejected |
| `prt` | partial | partial | incomplete, split |
| `dup` | duplicate | duplicate | repeat, copy |
| `exp` | expire | expired | timeout, invalid |

---

## Sequence Patterns

Use `→` for causal/temporal sequences:

### Happy Path
```
pay > aut > cap > set > suc
(payment → authorization → capture → settlement → success)
```

### Refund Flow
```
pay > cap > ref > cre
(payment → capture → refund → credit)
```

### Chargeback Flow
```
pay > cap > cha > rev > cre
(payment → capture → chargeback → reversal → credit)
```

### Authorization Hold
```
pay > aut > pen > (cap | voi)
(payment → authorization → pending → capture OR void)
```

---

## Intersection vs Sequence

| Expression | Meaning |
|------------|---------|
| `pay ref` | Something that is both payment and refund (net-zero?) |
| `pay > ref` | Payment that leads to refund (return scenario) |
| `aut pen` | Authorized AND pending (held transaction) |
| `aut > pen` | Authorization leading to pending (auth-hold flow) |

---

## Complex Expressions

### Split Payment
```
pay prt | cap prt | set prt
(partial payment | partial capture | partial settlement)
```

### Failed Authorization
```
pay > aut > fai | rev
(payment → authorization → failure | reversal)
```

### Duplicate Prevention
```
pay dup | voi
(duplicate payment | void)
```

---

## Compatibility

These words work with core Limn vocabulary:

| Combined | Meaning |
|----------|---------|
| `big pay` | Large payment |
| `sma ref` | Small refund |
| `pen aut now` | Authorization pending now |
| `pay > fai | sad` | Payment failed | sadness |

---

## Usage Notes

1. **Always use `→` for process flows** - `pay > aut > cap` not `pay aut cap`
2. **Use intersection for states** - `pen aut` = authorized and pending
3. **Combine with core vocabulary** - `big pay > ref` = large payment refunded
4. **Status words modify states** - `suc cap` = successful capture

---

*cau pay | eff doc | tra con*
*(cause: payment domain | effect: documentation | transformation continues)*
