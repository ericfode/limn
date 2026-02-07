#!/usr/bin/env python3
"""
Channel Coding Analysis — Error Correction Overhead for Limn

Sprint 4 Open Question #2: How much of Limn's 53% density advantage
survives after adding enough error correction to match English's noise
resilience?

Uses results from:
- compression_results.json (H11: density advantage)
- noise_injection_results.json (H16: noise resilience gap)
- zipfian_results.json (H18: encoding efficiency)

Analyzes multiple coding strategies:
1. Repetition coding (simple, inefficient)
2. Hamming codes (single-error correcting)
3. Reed-Solomon over GF(2^5) (burst-error correcting)
4. Shannon limit (theoretical best)

— Lex
"""

import json
import math
from pathlib import Path

OUTPUT_DIR = Path(__file__).resolve().parent
RESULTS_FILE = OUTPUT_DIR / "channel_coding_results.json"

# ─── Load experimental data ──────────────────────────────────────────────────

with open(OUTPUT_DIR / "compression_results.json") as f:
    comp = json.load(f)

with open(OUTPUT_DIR / "noise_injection_results.json") as f:
    noise = json.load(f)

with open(OUTPUT_DIR / "zipfian_results.json") as f:
    zipf = json.load(f)

# Key numbers
LIMN_BPSU = comp["density"]["limn_best_bpsu"]      # 13.0 bits per semantic unit
ENGLISH_BPSU = comp["density"]["english_best_bpsu"]  # 27.6 bits per semantic unit
DENSITY_ADV = comp["density"]["advantage"]            # 53% advantage

LIMN_WORD_LEN = 3      # characters
LIMN_CHAR_BITS = 5      # ceil(log2(26)) = 5 bits per character
LIMN_WORD_BITS = LIMN_WORD_LEN * LIMN_CHAR_BITS  # 15 bits per word

LIMN_VOCAB = noise["limn_vocab_size"]               # 2006 words
LIMN_NAMESPACE = 26**3                               # 17,576 possible 3-letter strings
OCCUPANCY = LIMN_VOCAB / LIMN_NAMESPACE              # 11.4%


# ─── Analysis functions ──────────────────────────────────────────────────────

def binary_symmetric_channel_capacity(p_error):
    """Shannon capacity of BSC with crossover probability p_error."""
    if p_error == 0 or p_error == 1:
        return 0 if p_error == 0.5 else 1.0
    h = -p_error * math.log2(p_error) - (1 - p_error) * math.log2(1 - p_error)
    return 1 - h


def word_error_prob(char_error_rate, word_len=3):
    """Probability that at least one character in word is corrupted."""
    return 1 - (1 - char_error_rate) ** word_len


def hamming_word_error_after_coding(char_error_rate, bits_per_char=5):
    """Word error rate after Hamming(7,4) on each character.

    Hamming(7,4) corrects any single bit error in a 7-bit block.
    Rate = 4/7. Applied per character (5 data bits → need 2 blocks of Hamming(7,4)
    or one Hamming(15,11) block for the full 15-bit word).
    """
    # Using Hamming(15,11) for the full 15-bit word
    # Corrects any single bit error in the 15-bit codeword (21 coded bits)
    # P(uncorrectable) = probability of 2+ bit errors in 21 bits
    p_bit = char_error_rate / 8  # Approximate: char error → bit error
    n_coded = 21  # Hamming(15,11) → 21 parity bits? No...
    # Hamming(15,11): 11 data bits, 4 parity = 15 total. Corrects 1 error.
    # For 15 data bits (word), we need extended code.
    # Let's use Hamming(31,26): 26 data bits, 5 parity = 31 total. But we only need 15.
    # Simpler: Hamming(15,11) on 11 data bits + separate 4-bit block
    # Or just analyze per-character Hamming(7,4)

    # Per character: 5 data bits. Extended Hamming(8,4) gives rate 4/8=0.5.
    # But 5 bits doesn't fit neatly. Use Hamming(7,4) for first 4 bits + uncoded 5th bit.
    # This is messy. Let's use a more practical approach.

    # Practical: SEC-DED (Single Error Correct, Double Error Detect) on 15-bit word
    # Need ceil(log2(15+1)) + 1 = 5 parity bits → 20 coded bits
    # Corrects 1 bit error, detects 2 bit errors

    # P(bit error) from char error: each char has p_char chance of being wrong.
    # When a char is wrong, on average ~2.5 of 5 bits flip (random corruption to new char)
    p_bit_approx = char_error_rate * 2.5 / 5  # = char_error_rate * 0.5

    n_coded = 20  # SEC-DED for 15 data bits
    p_uncorrectable = 1 - (1 - p_bit_approx)**n_coded - n_coded * p_bit_approx * (1 - p_bit_approx)**(n_coded - 1)
    rate = 15 / 20  # 0.75
    return p_uncorrectable, rate, n_coded


def repetition_analysis(char_error_rate, k):
    """k-fold repetition coding: transmit each word k times, majority vote."""
    p_word_err = word_error_prob(char_error_rate)
    # Majority vote: need > k/2 copies to be correct
    # P(decode wrong) = sum_{i > k//2} C(k,i) * p^i * (1-p)^(k-i)
    p_wrong = 0
    for i in range(k // 2 + 1, k + 1):
        p_wrong += math.comb(k, i) * (p_word_err ** i) * ((1 - p_word_err) ** (k - i))
    rate = 1.0 / k
    return p_wrong, rate


def rs_analysis(char_error_rate, n_symbols, k_data, symbol_bits=5):
    """Reed-Solomon code: n_symbols total, k_data data, corrects t = (n-k)/2 errors.

    Operates on symbols (characters/letters), not bits.
    Each symbol is one 5-bit character.
    """
    t_correct = (n_symbols - k_data) // 2  # errors correctable

    # P(symbol error) = char_error_rate (each symbol IS a character)
    p_sym_err = char_error_rate

    # P(decoding failure) = P(more than t errors in n symbols)
    p_fail = 0
    for i in range(t_correct + 1, n_symbols + 1):
        p_fail += math.comb(n_symbols, i) * (p_sym_err ** i) * ((1 - p_sym_err) ** (n_symbols - i))

    rate = k_data / n_symbols
    return p_fail, rate


# ─── Run analysis ────────────────────────────────────────────────────────────

print("=" * 70)
print("CHANNEL CODING ANALYSIS — Error Correction Overhead for Limn")
print("=" * 70)

print(f"\n  Source coding (from H11):")
print(f"    Limn:    {LIMN_BPSU:.2f} bits/semantic unit")
print(f"    English: {ENGLISH_BPSU:.2f} bits/semantic unit")
print(f"    Raw advantage: {DENSITY_ADV:.1%}")
print(f"    Break-even code rate: R_min = {LIMN_BPSU / ENGLISH_BPSU:.4f}")
print(f"    Max tolerable overhead: {(1 - LIMN_BPSU / ENGLISH_BPSU):.1%}")

R_MIN = LIMN_BPSU / ENGLISH_BPSU
MAX_OVERHEAD = 1 - R_MIN

results = {
    "source_coding": {
        "limn_bpsu": LIMN_BPSU,
        "english_bpsu": ENGLISH_BPSU,
        "raw_advantage": DENSITY_ADV,
        "break_even_rate": R_MIN,
        "max_tolerable_overhead": MAX_OVERHEAD,
    },
    "analysis_per_corruption_rate": {},
}

corruption_rates = [0.01, 0.05, 0.10, 0.20]

for cr in corruption_rates:
    print(f"\n{'─' * 70}")
    print(f"  Corruption rate: {cr:.0%}")
    print(f"{'─' * 70}")

    # Noise resilience from experiment
    key = f"rate_{cr}"
    limn_preserved = noise["results"][key]["limn"]["meaning_preserved"]
    eng_preserved = noise["results"][key]["english"]["meaning_preserved"]
    gap = eng_preserved - limn_preserved

    print(f"  Uncoded: Limn={limn_preserved:.1%} preserved, English={eng_preserved:.1%}, Gap={gap:.1%}")

    p_word_err = word_error_prob(cr)
    print(f"  P(word corrupted): {p_word_err:.4f}")

    # Shannon limit
    # Binary symmetric channel with p_bit ≈ cr * 0.5 (approximation)
    p_bit = cr * 0.5
    shannon_cap = binary_symmetric_channel_capacity(p_bit)
    print(f"  Shannon channel capacity: {shannon_cap:.4f} bits/channel-use")
    print(f"  Shannon limit code rate: {shannon_cap:.4f}")

    analysis = {
        "corruption_rate": cr,
        "uncoded_limn_preserved": limn_preserved,
        "uncoded_english_preserved": eng_preserved,
        "gap": gap,
        "p_word_error": p_word_err,
        "shannon_capacity": shannon_cap,
        "coding_schemes": {},
    }

    # 1. Repetition coding
    print(f"\n  Repetition coding:")
    for k in [3, 5, 7]:
        p_wrong, rate = repetition_analysis(cr, k)
        coded_bpsu = LIMN_BPSU / rate
        advantage_vs_english = 1 - coded_bpsu / ENGLISH_BPSU
        status = "BETTER" if coded_bpsu < ENGLISH_BPSU else "WORSE"
        print(f"    k={k}: P(wrong)={p_wrong:.6f}, Rate={rate:.3f}, "
              f"Coded={coded_bpsu:.1f} bpsu ({status} vs English {ENGLISH_BPSU:.1f})")
        analysis["coding_schemes"][f"repetition_k{k}"] = {
            "p_word_error_after": p_wrong,
            "code_rate": rate,
            "coded_bpsu": coded_bpsu,
            "vs_english": status,
            "advantage": advantage_vs_english,
        }

    # 2. Hamming (SEC-DED)
    p_uncorr, rate_h, n_coded = hamming_word_error_after_coding(cr)
    coded_bpsu_h = LIMN_BPSU / rate_h
    status_h = "BETTER" if coded_bpsu_h < ENGLISH_BPSU else "WORSE"
    print(f"\n  Hamming SEC-DED (15→20 bits):")
    print(f"    P(uncorrectable)={p_uncorr:.6f}, Rate={rate_h:.3f}, "
          f"Coded={coded_bpsu_h:.1f} bpsu ({status_h} vs English)")
    analysis["coding_schemes"]["hamming_secded"] = {
        "p_word_error_after": p_uncorr,
        "code_rate": rate_h,
        "coded_bpsu": coded_bpsu_h,
        "vs_english": status_h,
    }

    # 3. Reed-Solomon (symbol-level, operating on characters)
    # For a 3-char word, RS codes operate on 5-bit symbols
    # RS(n, k) over GF(2^5=32): max n=31
    print(f"\n  Reed-Solomon (symbol=character, GF(32)):")
    # Code a MESSAGE of m words (e.g., 6 words = 18 chars) as one RS block
    for msg_words, parity_chars in [(3, 2), (3, 4), (6, 4), (6, 6)]:
        k_data = msg_words * 3  # data symbols (characters)
        n_total = k_data + parity_chars
        if n_total > 31:
            continue
        p_fail, rate_rs = rs_analysis(cr, n_total, k_data)
        coded_bpsu_rs = LIMN_BPSU / rate_rs
        status_rs = "BETTER" if coded_bpsu_rs < ENGLISH_BPSU else "WORSE"
        t_correct = parity_chars // 2
        print(f"    RS({n_total},{k_data}): t={t_correct} errors, P(fail)={p_fail:.6f}, "
              f"Rate={rate_rs:.3f}, Coded={coded_bpsu_rs:.1f} bpsu ({status_rs})")
        analysis["coding_schemes"][f"rs_{n_total}_{k_data}"] = {
            "n_symbols": n_total,
            "k_data": k_data,
            "t_correct": t_correct,
            "p_decode_fail": p_fail,
            "code_rate": rate_rs,
            "coded_bpsu": coded_bpsu_rs,
            "vs_english": status_rs,
        }

    # 4. Shannon limit
    if shannon_cap > 0:
        coded_bpsu_shannon = LIMN_BPSU / shannon_cap
        status_sh = "BETTER" if coded_bpsu_shannon < ENGLISH_BPSU else "WORSE"
        print(f"\n  Shannon limit (theoretical best):")
        print(f"    Rate={shannon_cap:.4f}, Coded={coded_bpsu_shannon:.1f} bpsu ({status_sh})")
        analysis["coding_schemes"]["shannon_limit"] = {
            "code_rate": shannon_cap,
            "coded_bpsu": coded_bpsu_shannon,
            "vs_english": status_sh,
        }

    # Best practical scheme
    schemes = analysis["coding_schemes"]
    practical = {k: v for k, v in schemes.items() if k != "shannon_limit"}
    if practical:
        # Find best rate that keeps p_error low enough
        best = None
        for name, s in practical.items():
            p_err = s.get("p_word_error_after", s.get("p_decode_fail", 1.0))
            if p_err < 0.01:  # < 1% residual word error
                if best is None or s["coded_bpsu"] < best[1]["coded_bpsu"]:
                    best = (name, s)
        if best:
            analysis["best_practical"] = {
                "scheme": best[0],
                "coded_bpsu": best[1]["coded_bpsu"],
                "code_rate": best[1]["code_rate"],
            }

    results["analysis_per_corruption_rate"][key] = analysis


# ─── Summary ─────────────────────────────────────────────────────────────────

print(f"\n{'=' * 70}")
print("SUMMARY — Limn Density After Channel Coding")
print(f"{'=' * 70}")

print(f"\n  Break-even point: Limn can tolerate up to {MAX_OVERHEAD:.1%} coding overhead")
print(f"  before losing density advantage over English.")
print(f"\n  {'Rate':<8s} {'Best Scheme':<18s} {'Code Rate':>10s} {'Coded BPSU':>12s} {'vs English':>12s}")
print(f"  {'─' * 62}")

for cr in corruption_rates:
    key = f"rate_{cr}"
    a = results["analysis_per_corruption_rate"][key]
    best = a.get("best_practical")
    if best:
        adv = 1 - best["coded_bpsu"] / ENGLISH_BPSU
        status = f"{adv:+.1%}"
        print(f"  {cr:<8.0%} {best['scheme']:<18s} {best['code_rate']:>10.3f} "
              f"{best['coded_bpsu']:>12.1f} {status:>12s}")
    else:
        print(f"  {cr:<8.0%} {'(none <1% err)' :<18s}")

# Overall verdict
print(f"\n  Shannon limit analysis:")
for cr in corruption_rates:
    key = f"rate_{cr}"
    sh = results["analysis_per_corruption_rate"][key]["coding_schemes"].get("shannon_limit")
    if sh:
        adv = 1 - sh["coded_bpsu"] / ENGLISH_BPSU
        print(f"    {cr:.0%} corruption: coded Limn = {sh['coded_bpsu']:.1f} bpsu "
              f"(vs English {ENGLISH_BPSU:.1f}) → {adv:+.1%}")

# Does coded Limn beat English at all corruption rates?
all_beat = True
for cr in corruption_rates:
    key = f"rate_{cr}"
    sh = results["analysis_per_corruption_rate"][key]["coding_schemes"].get("shannon_limit", {})
    if sh.get("coded_bpsu", float("inf")) >= ENGLISH_BPSU:
        all_beat = False
        break

if all_beat:
    verdict = "DENSITY_SURVIVES: Coded Limn beats English at all tested corruption rates (Shannon limit)"
    print(f"\n  VERDICT: {verdict}")
else:
    # Find crossover point
    for cr in corruption_rates:
        key = f"rate_{cr}"
        sh = results["analysis_per_corruption_rate"][key]["coding_schemes"].get("shannon_limit", {})
        if sh.get("coded_bpsu", float("inf")) >= ENGLISH_BPSU:
            verdict = f"CROSSOVER: Coded Limn loses density advantage at ~{cr:.0%} corruption"
            print(f"\n  VERDICT: {verdict}")
            break

results["verdict"] = verdict

with open(RESULTS_FILE, "w") as f:
    json.dump(results, f, indent=2)
print(f"\n  Results saved to: {RESULTS_FILE}")


if __name__ == "__main__":
    pass  # Already executed at module level
