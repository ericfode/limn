#!/usr/bin/env python3
"""
Compression Benchmark: Limn vs English (v5 Sprint 3.2)

Extends the entropy analysis with:
1. Multiple compressors (gzip, bz2, lzma) at varying levels
2. Per-sentence compression analysis (not just corpus-level)
3. Statistical significance testing (paired t-test)
4. Normalized metrics: compressed bits per semantic unit

The key question: Is Limn's 2:1 raw character advantage preserved,
amplified, or diminished after optimal compression? If preserved,
Limn genuinely carries more meaning per bit. If diminished, Limn's
shorter form comes from redundancy, not information density.

— Lex
"""

import bz2
import gzip
import json
import lzma
import math
import statistics
from collections import Counter
from pathlib import Path

PAIRS_FILE = Path("/home/eric/src/limntown/limn/crew/translator/hgttg-training-pairs.jsonl")
OUTPUT_DIR = Path(__file__).resolve().parent


def load_pairs() -> list:
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


def count_semantic_units(english: str) -> int:
    """Count content words in English (excluding stopwords)."""
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


def compress_all(data: bytes) -> dict:
    """Compress with all available algorithms, return sizes."""
    results = {"raw_bytes": len(data)}

    # gzip (LZ77 + Huffman)
    results["gzip"] = len(gzip.compress(data, compresslevel=9))

    # bz2 (Burrows-Wheeler Transform)
    results["bz2"] = len(bz2.compress(data, compresslevel=9))

    # lzma/xz (LZMA2 — best general-purpose compressor)
    results["lzma"] = len(lzma.compress(data, preset=9))

    return results


def paired_ttest(a: list, b: list) -> tuple:
    """Paired t-test. Returns (t_statistic, p_value, effect_size)."""
    n = len(a)
    diffs = [a[i] - b[i] for i in range(n)]
    mean_d = statistics.mean(diffs)
    std_d = statistics.stdev(diffs) if n > 1 else 0
    if std_d == 0:
        return float('inf'), 0.0, float('inf')
    t = mean_d / (std_d / math.sqrt(n))
    # Two-tailed p-value approximation (good enough for n=329)
    # Using normal approximation since n >> 30
    from math import erfc
    p = erfc(abs(t) / math.sqrt(2))
    effect = mean_d / std_d  # Cohen's d
    return t, p, effect


def main():
    pairs = load_pairs()
    print(f"Compression Benchmark — Limn vs English")
    print(f"{'=' * 70}")
    print(f"  {len(pairs)} parallel HGttG pairs")

    # =========================================================================
    # Part 1: Corpus-level compression
    # =========================================================================
    all_english = "\n".join(p["english"] for p in pairs)
    all_limn = "\n".join(p["limn"] for p in pairs)

    en_bytes = all_english.encode("utf-8")
    lm_bytes = all_limn.encode("utf-8")

    en_comp = compress_all(en_bytes)
    lm_comp = compress_all(lm_bytes)

    print(f"\n[1] Corpus-Level Compression")
    print(f"  {'Algorithm':<15s} {'English':>10s} {'Limn':>10s} {'Ratio':>8s} {'Limn saves':>12s}")
    print(f"  {'─' * 57}")

    for alg in ["raw_bytes", "gzip", "bz2", "lzma"]:
        en_size = en_comp[alg]
        lm_size = lm_comp[alg]
        ratio = lm_size / en_size
        savings = 1 - ratio
        label = alg if alg != "raw_bytes" else "raw (UTF-8)"
        print(f"  {label:<15s} {en_size:>10d} {lm_size:>10d} {ratio:>8.3f} {savings:>11.1%}")

    # Best compressor
    best_alg = min(["gzip", "bz2", "lzma"], key=lambda a: lm_comp[a])
    print(f"\n  Best compressor for Limn: {best_alg} ({lm_comp[best_alg]} bytes)")
    best_en_alg = min(["gzip", "bz2", "lzma"], key=lambda a: en_comp[a])
    print(f"  Best compressor for English: {best_en_alg} ({en_comp[best_en_alg]} bytes)")

    # =========================================================================
    # Part 2: Compressed bits per semantic unit
    # =========================================================================
    total_sem = sum(count_semantic_units(p["english"]) for p in pairs)
    print(f"\n[2] Compressed Bits per Semantic Unit")
    print(f"  Total semantic units: {total_sem}")
    print(f"  {'Algorithm':<15s} {'English':>10s} {'Limn':>10s} {'Ratio':>8s}")
    print(f"  {'─' * 45}")

    for alg in ["gzip", "bz2", "lzma"]:
        en_bpsu = en_comp[alg] * 8 / total_sem
        lm_bpsu = lm_comp[alg] * 8 / total_sem
        ratio = lm_bpsu / en_bpsu
        print(f"  {alg:<15s} {en_bpsu:>10.2f} {lm_bpsu:>10.2f} {ratio:>8.3f}")

    # =========================================================================
    # Part 3: Per-sentence compression analysis
    # =========================================================================
    print(f"\n[3] Per-Sentence Compression Analysis")

    en_per_sent = []
    lm_per_sent = []
    en_bpsu_per_sent = []
    lm_bpsu_per_sent = []
    ratios_per_sent = []

    for p in pairs:
        en_raw = p["english"].encode("utf-8")
        lm_raw = p["limn"].encode("utf-8")

        # Use gzip for per-sentence (most widely used)
        en_gz = len(gzip.compress(en_raw, compresslevel=9))
        lm_gz = len(gzip.compress(lm_raw, compresslevel=9))

        en_per_sent.append(en_gz)
        lm_per_sent.append(lm_gz)

        sem = count_semantic_units(p["english"])
        en_bpsu_per_sent.append(en_gz * 8 / sem)
        lm_bpsu_per_sent.append(lm_gz * 8 / sem)
        ratios_per_sent.append(lm_gz / max(1, en_gz))

    en_mean = statistics.mean(en_per_sent)
    lm_mean = statistics.mean(lm_per_sent)
    en_bpsu_mean = statistics.mean(en_bpsu_per_sent)
    lm_bpsu_mean = statistics.mean(lm_bpsu_per_sent)
    ratio_mean = statistics.mean(ratios_per_sent)

    print(f"  {'Metric':<40s} {'English':>10s} {'Limn':>10s}")
    print(f"  {'─' * 62}")
    print(f"  {'Mean gzip bytes/sentence':<40s} {en_mean:>10.1f} {lm_mean:>10.1f}")
    print(f"  {'Median gzip bytes/sentence':<40s} {statistics.median(en_per_sent):>10.1f} {statistics.median(lm_per_sent):>10.1f}")
    print(f"  {'Std gzip bytes/sentence':<40s} {statistics.stdev(en_per_sent):>10.1f} {statistics.stdev(lm_per_sent):>10.1f}")
    print(f"  {'Mean gzip bits/semantic unit':<40s} {en_bpsu_mean:>10.1f} {lm_bpsu_mean:>10.1f}")
    print(f"  {'Mean Limn/English ratio':<40s} {'':>10s} {ratio_mean:>10.3f}")

    # Note: per-sentence compression is biased because gzip has ~20 byte
    # header overhead per call, which penalizes shorter texts more
    print(f"\n  NOTE: Per-sentence gzip includes ~20 byte header overhead,")
    print(f"  which penalizes shorter Limn sentences more than longer English.")
    print(f"  Corpus-level compression is the fairer comparison.")

    # =========================================================================
    # Part 4: Statistical significance
    # =========================================================================
    print(f"\n[4] Statistical Significance (paired t-test on gzip bits/semantic unit)")

    t_stat, p_val, cohens_d = paired_ttest(en_bpsu_per_sent, lm_bpsu_per_sent)
    print(f"  t-statistic: {t_stat:.4f}")
    print(f"  p-value:     {p_val:.2e}")
    print(f"  Cohen's d:   {cohens_d:.4f}")
    print(f"  n:           {len(pairs)}")

    if p_val < 0.001:
        print(f"  SIGNIFICANT (p < 0.001): Limn compresses to fewer bits per semantic unit")
    elif p_val < 0.05:
        print(f"  SIGNIFICANT (p < 0.05): Limn compresses to fewer bits per semantic unit")
    else:
        print(f"  NOT SIGNIFICANT (p = {p_val:.4f})")

    if abs(cohens_d) > 0.8:
        effect_label = "LARGE"
    elif abs(cohens_d) > 0.5:
        effect_label = "MEDIUM"
    elif abs(cohens_d) > 0.2:
        effect_label = "SMALL"
    else:
        effect_label = "NEGLIGIBLE"
    print(f"  Effect size: {effect_label}")

    # =========================================================================
    # Part 5: Distribution of per-sentence ratios
    # =========================================================================
    print(f"\n[5] Distribution of Per-Sentence Compression Ratios (Limn/English)")

    # Percentiles
    sorted_ratios = sorted(ratios_per_sent)
    n = len(sorted_ratios)
    p10 = sorted_ratios[int(n * 0.10)]
    p25 = sorted_ratios[int(n * 0.25)]
    p50 = sorted_ratios[int(n * 0.50)]
    p75 = sorted_ratios[int(n * 0.75)]
    p90 = sorted_ratios[int(n * 0.90)]

    print(f"  p10:    {p10:.3f}")
    print(f"  p25:    {p25:.3f}")
    print(f"  Median: {p50:.3f}")
    print(f"  p75:    {p75:.3f}")
    print(f"  p90:    {p90:.3f}")

    # How many sentences have Limn smaller?
    limn_wins = sum(1 for r in ratios_per_sent if r < 1.0)
    print(f"\n  Limn smaller: {limn_wins}/{n} ({limn_wins/n:.1%})")
    print(f"  English smaller: {n - limn_wins}/{n} ({(n - limn_wins)/n:.1%})")

    # Worst cases (where English compresses better)
    worst = sorted(zip(ratios_per_sent, pairs), key=lambda x: -x[0])[:5]
    print(f"\n  Worst 5 ratios (Limn relatively largest):")
    for ratio, p in worst:
        print(f"    {ratio:.3f}  [{p['id']}] {p['limn'][:50]}")

    # Best cases
    best = sorted(zip(ratios_per_sent, pairs), key=lambda x: x[0])[:5]
    print(f"\n  Best 5 ratios (Limn relatively smallest):")
    for ratio, p in best:
        print(f"    {ratio:.3f}  [{p['id']}] {p['limn'][:50]}")

    # =========================================================================
    # Summary
    # =========================================================================
    print(f"\n{'=' * 70}")
    print(f"  SUMMARY")
    print(f"{'=' * 70}")

    corpus_savings = 1 - lm_comp["gzip"] / en_comp["gzip"]
    best_savings = 1 - lm_comp[best_alg] / en_comp[best_en_alg]

    print(f"\n  Raw character advantage:      {1 - len(lm_bytes)/len(en_bytes):.1%}")
    print(f"  gzip compression advantage:   {corpus_savings:.1%}")
    print(f"  Best compressor advantage:    {best_savings:.1%} ({best_alg} vs {best_en_alg})")
    print(f"  Per-sentence advantage:       {1 - lm_mean/en_mean:.1%} (gzip, mean)")
    print(f"  Statistical significance:     p = {p_val:.2e}, Cohen's d = {cohens_d:.2f}")
    print(f"  Limn wins per-sentence:       {limn_wins}/{n} ({limn_wins/n:.1%})")

    lm_best_bpsu = lm_comp[best_alg] * 8 / total_sem
    en_best_bpsu = en_comp[best_en_alg] * 8 / total_sem
    print(f"\n  Compressed bits per semantic unit:")
    print(f"    English ({best_en_alg}): {en_best_bpsu:.1f}")
    print(f"    Limn ({best_alg}):    {lm_best_bpsu:.1f}")
    print(f"    Density advantage:  {1 - lm_best_bpsu/en_best_bpsu:.1%}")

    if lm_best_bpsu < en_best_bpsu:
        print(f"\n  H11 SUPPORTED: Limn is genuinely denser, not just shorter.")
        print(f"  Even after optimal compression removes redundancy,")
        print(f"  Limn uses {1 - lm_best_bpsu/en_best_bpsu:.0%} fewer bits per semantic unit.")
    else:
        print(f"\n  H11 CHALLENGED: Limn's raw advantage disappears under compression.")
        print(f"  The shorter form comes from redundancy, not information density.")

    # Save results
    results = {
        "pairs_count": len(pairs),
        "total_semantic_units": total_sem,
        "corpus_compression": {
            "english": en_comp,
            "limn": lm_comp,
        },
        "per_sentence": {
            "mean_gzip_english": en_mean,
            "mean_gzip_limn": lm_mean,
            "mean_ratio": ratio_mean,
            "limn_wins": limn_wins,
            "total": n,
        },
        "significance": {
            "t_statistic": t_stat,
            "p_value": p_val,
            "cohens_d": cohens_d,
            "effect_size": effect_label,
        },
        "density": {
            "english_best_bpsu": en_best_bpsu,
            "limn_best_bpsu": lm_best_bpsu,
            "advantage": 1 - lm_best_bpsu / en_best_bpsu,
        },
    }

    output_file = OUTPUT_DIR / "compression_results.json"
    with open(output_file, "w") as f:
        json.dump(results, f, indent=2)
    print(f"\n  Results saved to: {output_file}")


if __name__ == "__main__":
    main()
