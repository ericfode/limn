#!/usr/bin/env python3
"""
Information-Theoretic Analysis: Limn vs English
================================================

Tests H11: Does Limn's information density exceed natural language?

Measurements:
1. Shannon entropy (unigram, bigram, trigram) — character and word level
2. Compression ratios (gzip, zlib)
3. Character efficiency (chars per sentence, chars per semantic unit)
4. Token efficiency (tokens per concept)
5. Redundancy analysis

Data: 329 parallel HGttG pairs (English ↔ Limn)

Usage:
    python entropy_analysis.py
"""

import gzip
import json
import math
import zlib
from collections import Counter, defaultdict
from pathlib import Path

PAIRS_FILE = Path("/home/eric/src/limntown/limn/crew/translator/hgttg-training-pairs.jsonl")
OUTPUT_DIR = Path(__file__).resolve().parent


def load_pairs() -> list:
    """Load parallel English-Limn pairs."""
    pairs = []
    with open(PAIRS_FILE) as f:
        for line in f:
            d = json.loads(line)
            pairs.append({
                "id": d["id"],
                "english": d["english"].strip('"'),
                "limn": d["limn"],
            })
    return pairs


# ---------------------------------------------------------------------------
# Shannon Entropy
# ---------------------------------------------------------------------------
def char_entropy(text: str) -> float:
    """Compute Shannon entropy in bits per character."""
    counts = Counter(text.lower())
    total = sum(counts.values())
    if total == 0:
        return 0.0
    entropy = 0.0
    for count in counts.values():
        p = count / total
        if p > 0:
            entropy -= p * math.log2(p)
    return entropy


def ngram_entropy(tokens: list, n: int) -> float:
    """Compute n-gram entropy in bits per n-gram."""
    ngrams = [tuple(tokens[i:i+n]) for i in range(len(tokens) - n + 1)]
    counts = Counter(ngrams)
    total = sum(counts.values())
    if total == 0:
        return 0.0
    entropy = 0.0
    for count in counts.values():
        p = count / total
        if p > 0:
            entropy -= p * math.log2(p)
    return entropy


def word_entropy(text: str) -> float:
    """Compute word-level Shannon entropy."""
    words = text.lower().split()
    return ngram_entropy(words, 1)


def conditional_entropy(tokens: list, n: int) -> float:
    """Compute H(X_n | X_1...X_{n-1}) — entropy rate estimate."""
    if n <= 1:
        return ngram_entropy(tokens, 1)
    h_n = ngram_entropy(tokens, n)
    h_nm1 = ngram_entropy(tokens, n - 1)
    return h_n - h_nm1


# ---------------------------------------------------------------------------
# Compression
# ---------------------------------------------------------------------------
def compression_ratio(text: str) -> dict:
    """Compute compression ratios with multiple algorithms."""
    raw = text.encode("utf-8")
    raw_size = len(raw)

    gz = gzip.compress(raw, compresslevel=9)
    zl = zlib.compress(raw, level=9)

    return {
        "raw_bytes": raw_size,
        "gzip_bytes": len(gz),
        "zlib_bytes": len(zl),
        "gzip_ratio": len(gz) / max(1, raw_size),
        "zlib_ratio": len(zl) / max(1, raw_size),
    }


# ---------------------------------------------------------------------------
# Efficiency metrics
# ---------------------------------------------------------------------------
def count_semantic_units(english: str) -> int:
    """Rough count of semantic units in English text.

    Counts content words (nouns, verbs, adjectives, adverbs) — approximated
    by excluding stopwords and short function words.
    """
    stopwords = {
        "the", "a", "an", "is", "are", "was", "were", "be", "been", "being",
        "have", "has", "had", "do", "does", "did", "will", "would", "could",
        "should", "may", "might", "can", "shall", "must", "need",
        "i", "you", "he", "she", "it", "we", "they", "me", "him", "her",
        "us", "them", "my", "your", "his", "its", "our", "their",
        "this", "that", "these", "those", "what", "which", "who", "whom",
        "and", "but", "or", "nor", "for", "yet", "because", "if",
        "then", "else", "while", "although", "though", "after", "before",
        "in", "on", "at", "to", "from", "by", "with", "about", "between",
        "of", "up", "out", "off", "over", "under", "into", "upon",
        "not", "no", "so", "than", "too", "very", "also", "just",
    }
    words = [w.lower().strip(".,!?;:'\"()-") for w in english.split()]
    content = [w for w in words if w and w not in stopwords and len(w) > 1]
    return max(1, len(content))


def count_limn_tokens(limn: str) -> int:
    """Count meaningful tokens in Limn (words + operators)."""
    import re
    # Words: 2-4 letter sequences
    words = re.findall(r'\b[a-zA-Z]{2,4}\b', limn)
    # Operators
    ops = re.findall(r'[@*^\\±:\|→∎~∿]', limn)
    # Numbers (gradient values)
    nums = re.findall(r'\d+\.?\d*', limn)
    return len(words) + len(ops) + len(nums)


# ---------------------------------------------------------------------------
# Main analysis
# ---------------------------------------------------------------------------
def main():
    pairs = load_pairs()
    print(f"Loaded {len(pairs)} parallel pairs")

    # Concatenate all text for corpus-level analysis
    all_english = "\n".join(p["english"] for p in pairs)
    all_limn = "\n".join(p["limn"] for p in pairs)

    print()
    print("=" * 70)
    print("  H11: Information Density — Limn vs English")
    print("  329 parallel HGttG translations")
    print("=" * 70)

    # --- 1. Basic statistics ---
    print("\n[1] Basic Statistics")
    print(f"  {'Metric':<35s} {'English':>10s} {'Limn':>10s} {'Ratio':>8s}")
    print(f"  {'─' * 65}")

    en_chars = len(all_english)
    lm_chars = len(all_limn)
    print(f"  {'Total characters':<35s} {en_chars:>10d} {lm_chars:>10d} {lm_chars/en_chars:>8.3f}")

    en_words = all_english.split()
    lm_words = all_limn.split()
    print(f"  {'Total words/tokens':<35s} {len(en_words):>10d} {len(lm_words):>10d} {len(lm_words)/len(en_words):>8.3f}")

    en_unique = len(set(w.lower() for w in en_words))
    lm_unique = len(set(w.lower() for w in lm_words))
    print(f"  {'Unique words/tokens':<35s} {en_unique:>10d} {lm_unique:>10d} {lm_unique/en_unique:>8.3f}")

    en_avg_len = sum(len(w) for w in en_words) / len(en_words)
    lm_avg_len = sum(len(w) for w in lm_words) / len(lm_words)
    print(f"  {'Avg word length (chars)':<35s} {en_avg_len:>10.2f} {lm_avg_len:>10.2f} {lm_avg_len/en_avg_len:>8.3f}")

    # --- 2. Shannon entropy ---
    print("\n[2] Shannon Entropy (bits)")
    print(f"  {'Metric':<35s} {'English':>10s} {'Limn':>10s} {'Ratio':>8s}")
    print(f"  {'─' * 65}")

    en_char_h = char_entropy(all_english)
    lm_char_h = char_entropy(all_limn)
    print(f"  {'Character entropy (H₁)':<35s} {en_char_h:>10.4f} {lm_char_h:>10.4f} {lm_char_h/en_char_h:>8.3f}")

    en_word_h = word_entropy(all_english)
    lm_word_h = word_entropy(all_limn)
    print(f"  {'Word entropy (H₁)':<35s} {en_word_h:>10.4f} {lm_word_h:>10.4f} {lm_word_h/en_word_h:>8.3f}")

    en_chars_list = list(all_english.lower())
    lm_chars_list = list(all_limn.lower())

    en_bigram_h = ngram_entropy(en_chars_list, 2)
    lm_bigram_h = ngram_entropy(lm_chars_list, 2)
    print(f"  {'Character bigram entropy (H₂)':<35s} {en_bigram_h:>10.4f} {lm_bigram_h:>10.4f} {lm_bigram_h/en_bigram_h:>8.3f}")

    en_cond_h = conditional_entropy(en_chars_list, 2)
    lm_cond_h = conditional_entropy(lm_chars_list, 2)
    print(f"  {'Conditional entropy H(X₂|X₁)':<35s} {en_cond_h:>10.4f} {lm_cond_h:>10.4f} {lm_cond_h/en_cond_h:>8.3f}")

    en_trigram_h = ngram_entropy(en_chars_list, 3)
    lm_trigram_h = ngram_entropy(lm_chars_list, 3)
    print(f"  {'Character trigram entropy (H₃)':<35s} {en_trigram_h:>10.4f} {lm_trigram_h:>10.4f} {lm_trigram_h/en_trigram_h:>8.3f}")

    # Entropy rate estimate
    en_rate = conditional_entropy(en_chars_list, 3)
    lm_rate = conditional_entropy(lm_chars_list, 3)
    print(f"  {'Entropy rate estimate H(X₃|X₁₂)':<35s} {en_rate:>10.4f} {lm_rate:>10.4f} {lm_rate/en_rate:>8.3f}")

    # --- 3. Compression ---
    print("\n[3] Compression Analysis")
    print(f"  {'Metric':<35s} {'English':>10s} {'Limn':>10s} {'Ratio':>8s}")
    print(f"  {'─' * 65}")

    en_comp = compression_ratio(all_english)
    lm_comp = compression_ratio(all_limn)

    print(f"  {'Raw size (bytes)':<35s} {en_comp['raw_bytes']:>10d} {lm_comp['raw_bytes']:>10d} {lm_comp['raw_bytes']/en_comp['raw_bytes']:>8.3f}")
    print(f"  {'gzip compressed (bytes)':<35s} {en_comp['gzip_bytes']:>10d} {lm_comp['gzip_bytes']:>10d} {lm_comp['gzip_bytes']/en_comp['gzip_bytes']:>8.3f}")
    print(f"  {'zlib compressed (bytes)':<35s} {en_comp['zlib_bytes']:>10d} {lm_comp['zlib_bytes']:>10d} {lm_comp['zlib_bytes']/en_comp['zlib_bytes']:>8.3f}")
    print(f"  {'gzip ratio (compressed/raw)':<35s} {en_comp['gzip_ratio']:>10.4f} {lm_comp['gzip_ratio']:>10.4f} {lm_comp['gzip_ratio']/en_comp['gzip_ratio']:>8.3f}")
    print(f"  {'zlib ratio (compressed/raw)':<35s} {en_comp['zlib_ratio']:>10.4f} {lm_comp['zlib_ratio']:>10.4f} {lm_comp['zlib_ratio']/en_comp['zlib_ratio']:>8.3f}")

    # --- 4. Efficiency per sentence ---
    print("\n[4] Per-Sentence Efficiency")
    print(f"  {'Metric':<35s} {'English':>10s} {'Limn':>10s} {'Ratio':>8s}")
    print(f"  {'─' * 65}")

    en_sent_chars = [len(p["english"]) for p in pairs]
    lm_sent_chars = [len(p["limn"]) for p in pairs]
    en_avg_sc = sum(en_sent_chars) / len(en_sent_chars)
    lm_avg_sc = sum(lm_sent_chars) / len(lm_sent_chars)
    print(f"  {'Avg chars/sentence':<35s} {en_avg_sc:>10.1f} {lm_avg_sc:>10.1f} {lm_avg_sc/en_avg_sc:>8.3f}")

    en_sent_words = [len(p["english"].split()) for p in pairs]
    lm_sent_words = [len(p["limn"].split()) for p in pairs]
    en_avg_sw = sum(en_sent_words) / len(en_sent_words)
    lm_avg_sw = sum(lm_sent_words) / len(lm_sent_words)
    print(f"  {'Avg words/sentence':<35s} {en_avg_sw:>10.1f} {lm_avg_sw:>10.1f} {lm_avg_sw/en_avg_sw:>8.3f}")

    # --- 5. Information density: bits per semantic unit ---
    print("\n[5] Information Density (the key metric)")
    print(f"  {'Metric':<35s} {'English':>10s} {'Limn':>10s} {'Ratio':>8s}")
    print(f"  {'─' * 65}")

    # Compute per-pair metrics
    en_chars_per_sem = []
    lm_chars_per_sem = []
    en_bytes_per_sem = []
    lm_bytes_per_sem = []

    for p in pairs:
        sem_units = count_semantic_units(p["english"])
        en_chars_per_sem.append(len(p["english"]) / sem_units)
        lm_chars_per_sem.append(len(p["limn"]) / sem_units)
        en_bytes_per_sem.append(len(p["english"].encode("utf-8")) / sem_units)
        lm_bytes_per_sem.append(len(p["limn"].encode("utf-8")) / sem_units)

    en_avg_cps = sum(en_chars_per_sem) / len(en_chars_per_sem)
    lm_avg_cps = sum(lm_chars_per_sem) / len(lm_chars_per_sem)
    print(f"  {'Chars per semantic unit':<35s} {en_avg_cps:>10.2f} {lm_avg_cps:>10.2f} {lm_avg_cps/en_avg_cps:>8.3f}")

    en_avg_bps = sum(en_bytes_per_sem) / len(en_bytes_per_sem)
    lm_avg_bps = sum(lm_bytes_per_sem) / len(lm_bytes_per_sem)
    print(f"  {'Bytes per semantic unit':<35s} {en_avg_bps:>10.2f} {lm_avg_bps:>10.2f} {lm_avg_bps/en_avg_bps:>8.3f}")

    # Compressed bits per semantic unit (true information content)
    total_sem = sum(count_semantic_units(p["english"]) for p in pairs)
    en_comp_bits_per_sem = en_comp["gzip_bytes"] * 8 / total_sem
    lm_comp_bits_per_sem = lm_comp["gzip_bytes"] * 8 / total_sem
    print(f"  {'Compressed bits per sem. unit':<35s} {en_comp_bits_per_sem:>10.2f} {lm_comp_bits_per_sem:>10.2f} {lm_comp_bits_per_sem/en_comp_bits_per_sem:>8.3f}")

    # Limn tokens per English semantic unit
    lm_tok_per_sem = []
    for p in pairs:
        sem_units = count_semantic_units(p["english"])
        lm_tok = count_limn_tokens(p["limn"])
        lm_tok_per_sem.append(lm_tok / sem_units)
    lm_avg_tps = sum(lm_tok_per_sem) / len(lm_tok_per_sem)
    print(f"  {'Limn tokens per semantic unit':<35s} {'—':>10s} {lm_avg_tps:>10.2f} {'—':>8s}")

    # --- 6. Redundancy analysis ---
    print("\n[6] Redundancy Analysis")
    print(f"  {'Metric':<35s} {'English':>10s} {'Limn':>10s}")
    print(f"  {'─' * 57}")

    # Type-token ratio (higher = more diverse = less redundant)
    en_ttr = en_unique / len(en_words)
    lm_ttr = lm_unique / len(lm_words)
    print(f"  {'Type-token ratio':<35s} {en_ttr:>10.4f} {lm_ttr:>10.4f}")

    # Compression entropy (bits per raw character after compression)
    en_comp_entropy = en_comp["gzip_bytes"] * 8 / en_chars
    lm_comp_entropy = lm_comp["gzip_bytes"] * 8 / lm_chars
    print(f"  {'Compression entropy (bits/char)':<35s} {en_comp_entropy:>10.4f} {lm_comp_entropy:>10.4f}")

    # Redundancy = 1 - (compressed / theoretical max)
    en_max_entropy = math.log2(len(set(all_english.lower())))
    lm_max_entropy = math.log2(len(set(all_limn.lower())))
    en_redundancy = 1 - (en_char_h / en_max_entropy)
    lm_redundancy = 1 - (lm_char_h / lm_max_entropy)
    print(f"  {'Character redundancy':<35s} {en_redundancy:>10.4f} {lm_redundancy:>10.4f}")
    print(f"  {'Alphabet size':<35s} {len(set(all_english.lower())):>10d} {len(set(all_limn.lower())):>10d}")

    # --- 7. Distribution analysis ---
    print("\n[7] Character Distribution")
    en_char_counts = Counter(all_english.lower())
    lm_char_counts = Counter(all_limn.lower())
    en_top = en_char_counts.most_common(10)
    lm_top = lm_char_counts.most_common(10)
    print(f"  English top 10:  {''.join(f'{c}:{n/en_chars:.3f} ' for c,n in en_top)}")
    print(f"  Limn top 10:     {''.join(f'{c}:{n/lm_chars:.3f} ' for c,n in lm_top)}")

    # --- Summary ---
    print()
    print("=" * 70)
    print("  SUMMARY")
    print("=" * 70)
    print()

    compression_advantage = 1 - lm_comp["gzip_bytes"] / en_comp["gzip_bytes"]
    char_advantage = 1 - lm_chars / en_chars
    density_advantage = 1 - lm_avg_cps / en_avg_cps

    print(f"  Limn is {char_advantage:.1%} shorter in raw characters")
    print(f"  Limn is {compression_advantage:.1%} smaller when compressed")
    print(f"  Limn uses {density_advantage:.1%} fewer chars per semantic unit")
    print()
    print(f"  Character entropy:    English {en_char_h:.4f} vs Limn {lm_char_h:.4f} bits/char")
    print(f"  Compression entropy:  English {en_comp_entropy:.4f} vs Limn {lm_comp_entropy:.4f} bits/char")
    print()

    if lm_comp_bits_per_sem < en_comp_bits_per_sem:
        print(f"  H11 EVIDENCE: Limn carries the same meaning in fewer compressed bits")
        print(f"  ({lm_comp_bits_per_sem:.1f} vs {en_comp_bits_per_sem:.1f} bits per semantic unit)")
        print(f"  Information density advantage: {1 - lm_comp_bits_per_sem/en_comp_bits_per_sem:.1%}")
    else:
        print(f"  H11 EVIDENCE: English is MORE information-dense when compressed")
        print(f"  ({en_comp_bits_per_sem:.1f} vs {lm_comp_bits_per_sem:.1f} bits per semantic unit)")
        print(f"  Limn LOSES {lm_comp_bits_per_sem/en_comp_bits_per_sem - 1:.1%} in compressed density")

    print()
    print("  CAVEAT: This analysis measures ENCODING efficiency, not SEMANTIC")
    print("  precision. Limn may be shorter but lossier. Round-trip fidelity")
    print("  testing (Limn → English → Limn) is needed to measure information")
    print("  PRESERVATION, not just information COMPRESSION.")

    # Save results
    results = {
        "pairs_count": len(pairs),
        "basic_stats": {
            "english_chars": en_chars, "limn_chars": lm_chars,
            "english_words": len(en_words), "limn_words": len(lm_words),
            "english_unique": en_unique, "limn_unique": lm_unique,
        },
        "entropy": {
            "char_h1": {"english": en_char_h, "limn": lm_char_h},
            "word_h1": {"english": en_word_h, "limn": lm_word_h},
            "char_h2": {"english": en_bigram_h, "limn": lm_bigram_h},
            "cond_h2": {"english": en_cond_h, "limn": lm_cond_h},
            "entropy_rate": {"english": en_rate, "limn": lm_rate},
        },
        "compression": {
            "english": en_comp, "limn": lm_comp,
        },
        "density": {
            "chars_per_semantic_unit": {"english": en_avg_cps, "limn": lm_avg_cps},
            "bytes_per_semantic_unit": {"english": en_avg_bps, "limn": lm_avg_bps},
            "compressed_bits_per_semantic_unit": {
                "english": en_comp_bits_per_sem, "limn": lm_comp_bits_per_sem
            },
        },
        "redundancy": {
            "type_token_ratio": {"english": en_ttr, "limn": lm_ttr},
            "compression_entropy": {"english": en_comp_entropy, "limn": lm_comp_entropy},
            "char_redundancy": {"english": en_redundancy, "limn": lm_redundancy},
        },
    }

    output_file = OUTPUT_DIR / "entropy_results.json"
    with open(output_file, "w") as f:
        json.dump(results, f, indent=2)
    print(f"\n  Results saved to: {output_file}")


if __name__ == "__main__":
    main()
