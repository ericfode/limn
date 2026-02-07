#!/usr/bin/env python3
"""
Zipfian Efficiency Analysis: H18 — Fixed CVC Tax

All Limn words are exactly 3 characters. Natural languages use shorter
words for frequent meanings (Piantadosi et al. 2011 — word length
correlates with surprisal-in-context across 10 languages).

This experiment computes:
1. The frequency distribution of Limn words in the HGttG corpus
2. The optimal variable-length code (Huffman) for that distribution
3. The Shannon entropy lower bound
4. The gap between Limn's fixed 3-char = log2(26)*3 = 14.1 bits/word
   and the optimal variable-length code
5. The "CVC tax" — how many extra bits Limn wastes per word

Key insight: If Limn words follow a Zipfian distribution (they should —
natural text does), then high-frequency words like 'hum', 'wrl', 'kno'
could be encoded in 1-2 chars, saving significant bandwidth. The CVC
constraint prevents this optimization.

— Lex
"""

import json
import math
import re
import statistics
import subprocess
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


def extract_limn_words(text: str) -> list:
    """Extract Limn content words from text (excluding operators, numbers, markers)."""
    tokens = re.split(r'[\s|:\n]+', text)
    words = []
    for t in tokens:
        clean = re.sub(r'[@*^±].*', '', t)
        clean = re.sub(r'[^a-zA-Z\-]', '', clean)
        if clean.startswith('nu-'):
            words.append('nu')
            clean = clean[3:]
        parts = clean.split('-')
        for p in parts:
            p = p.strip().lower()
            if p and p.isalpha() and 2 <= len(p) <= 4:
                words.append(p)
    return words


def extract_english_words(text: str) -> list:
    """Extract English content words (including function words)."""
    tokens = text.split()
    words = []
    for t in tokens:
        clean = t.strip('.,!?;:\'"()-…""').lower()
        if clean and clean.isalpha() and len(clean) >= 1:
            words.append(clean)
    return words


def shannon_entropy(freq: Counter) -> float:
    """Shannon entropy in bits."""
    total = sum(freq.values())
    if total == 0:
        return 0
    h = 0
    for count in freq.values():
        if count > 0:
            p = count / total
            h -= p * math.log2(p)
    return h


def huffman_average_length(freq: Counter) -> float:
    """Compute average Huffman code length (in bits) for given frequency distribution.
    Uses the standard heap-based algorithm."""
    if len(freq) <= 1:
        return 1.0

    import heapq
    total = sum(freq.values())

    # Build Huffman tree
    # Each node is (weight, unique_id, symbol_or_None)
    heap = [(count / total, i, sym) for i, (sym, count) in enumerate(freq.items())]
    heapq.heapify(heap)
    uid = len(heap)

    while len(heap) > 1:
        w1, _, s1 = heapq.heappop(heap)
        w2, _, s2 = heapq.heappop(heap)
        heapq.heappush(heap, (w1 + w2, uid, None))
        uid += 1

    # Recompute: assign lengths by rebuilding
    # Simpler approach: compute directly from probabilities
    # Huffman average length = sum(p_i * l_i) where l_i is code length
    # We can compute this by simulation

    # Actually, let's do the proper tree traversal
    import heapq

    # Rebuild with tree structure
    class Node:
        def __init__(self, sym=None, weight=0, left=None, right=None):
            self.sym = sym
            self.weight = weight
            self.left = left
            self.right = right

        def __lt__(self, other):
            return self.weight < other.weight

    nodes = [Node(sym=sym, weight=count / total) for sym, count in freq.items()]
    heapq.heapify(nodes)

    while len(nodes) > 1:
        left = heapq.heappop(nodes)
        right = heapq.heappop(nodes)
        parent = Node(weight=left.weight + right.weight, left=left, right=right)
        heapq.heappush(nodes, parent)

    root = nodes[0]

    # Traverse to get code lengths
    lengths = {}

    def traverse(node, depth):
        if node.sym is not None:
            lengths[node.sym] = depth
        if node.left:
            traverse(node.left, depth + 1)
        if node.right:
            traverse(node.right, depth + 1)

    traverse(root, 0)

    # Handle edge case: single symbol gets length 1
    if len(lengths) == 1:
        for sym in lengths:
            lengths[sym] = 1

    # Average length
    avg = sum((count / total) * lengths[sym] for sym, count in freq.items())
    return avg


def zipf_analysis(freq: Counter, label: str):
    """Analyze how well the distribution follows Zipf's law."""
    sorted_items = freq.most_common()
    n = len(sorted_items)
    if n < 10:
        return None

    # Compute log(rank) vs log(frequency)
    log_ranks = [math.log(i + 1) for i in range(n)]
    log_freqs = [math.log(count) for _, count in sorted_items]

    # Linear regression: log(f) = -alpha * log(r) + c
    mean_lr = statistics.mean(log_ranks)
    mean_lf = statistics.mean(log_freqs)
    num = sum((log_ranks[i] - mean_lr) * (log_freqs[i] - mean_lf) for i in range(n))
    den = sum((log_ranks[i] - mean_lr) ** 2 for i in range(n))
    alpha = -num / den if den > 0 else 0

    return {
        "alpha": alpha,
        "top_10": [(sym, count) for sym, count in sorted_items[:10]],
        "unique_words": n,
        "total_tokens": sum(freq.values()),
    }


def main():
    print("Zipfian Efficiency Analysis — H18: Fixed CVC Tax")
    print("=" * 70)

    pairs = load_pairs()
    print(f"  Loaded {len(pairs)} parallel pairs")

    # Extract words
    limn_words = []
    english_words = []
    for p in pairs:
        limn_words.extend(extract_limn_words(p["limn"]))
        english_words.extend(extract_english_words(p["english"]))

    limn_freq = Counter(limn_words)
    english_freq = Counter(english_words)

    print(f"  Limn: {len(limn_words)} tokens, {len(limn_freq)} unique words")
    print(f"  English: {len(english_words)} tokens, {len(english_freq)} unique words")

    # =========================================================================
    # Part 1: Frequency distributions
    # =========================================================================
    print(f"\n[1] Word Frequency Distributions")
    print(f"\n  Top 20 Limn words:")
    for word, count in limn_freq.most_common(20):
        pct = count / len(limn_words) * 100
        print(f"    {word:>5s}  {count:>4d}  ({pct:>5.1f}%)")

    print(f"\n  Top 20 English words:")
    for word, count in english_freq.most_common(20):
        pct = count / len(english_words) * 100
        print(f"    {word:>10s}  {count:>4d}  ({pct:>5.1f}%)")

    # =========================================================================
    # Part 2: Zipf analysis
    # =========================================================================
    print(f"\n[2] Zipf's Law Fit")

    limn_zipf = zipf_analysis(limn_freq, "Limn")
    english_zipf = zipf_analysis(english_freq, "English")

    if limn_zipf:
        print(f"  Limn:    alpha = {limn_zipf['alpha']:.3f} (Zipf's law: alpha ≈ 1.0)")
    if english_zipf:
        print(f"  English: alpha = {english_zipf['alpha']:.3f}")

    # =========================================================================
    # Part 3: Encoding efficiency
    # =========================================================================
    print(f"\n[3] Encoding Efficiency Comparison")

    # Limn: fixed 3-char words
    # Each char from 26-letter alphabet = log2(26) ≈ 4.7 bits
    # Fixed 3-char = 3 * log2(26) = 14.1 bits per word
    limn_fixed_bits = 3 * math.log2(26)

    # Shannon entropy (lower bound for any code)
    limn_entropy = shannon_entropy(limn_freq)
    english_entropy = shannon_entropy(english_freq)

    # Huffman code (achievable, within 1 bit of entropy)
    limn_huffman = huffman_average_length(limn_freq)
    english_huffman = huffman_average_length(english_freq)

    # English: variable-length words, average length
    en_avg_len = statistics.mean([len(w) for w in english_words])
    en_fixed_bits = en_avg_len * math.log2(26)

    print(f"\n  {'Metric':<40s} {'English':>10s} {'Limn':>10s}")
    print(f"  {'─' * 62}")
    print(f"  {'Shannon entropy (bits/word)':<40s} {english_entropy:>10.2f} {limn_entropy:>10.2f}")
    print(f"  {'Huffman avg code length (bits/word)':<40s} {english_huffman:>10.2f} {limn_huffman:>10.2f}")
    print(f"  {'Actual avg encoding (bits/word)':<40s} {en_fixed_bits:>10.2f} {limn_fixed_bits:>10.2f}")
    print(f"  {'Encoding overhead vs entropy':<40s} {en_fixed_bits - english_entropy:>10.2f} {limn_fixed_bits - limn_entropy:>10.2f}")
    print(f"  {'Encoding overhead vs Huffman':<40s} {en_fixed_bits - english_huffman:>10.2f} {limn_fixed_bits - limn_huffman:>10.2f}")

    # =========================================================================
    # Part 4: The CVC Tax
    # =========================================================================
    print(f"\n[4] The CVC Tax")

    # Bits wasted per word
    cvc_tax_vs_entropy = limn_fixed_bits - limn_entropy
    cvc_tax_vs_huffman = limn_fixed_bits - limn_huffman

    # Total bits wasted over corpus
    total_wasted_entropy = cvc_tax_vs_entropy * len(limn_words)
    total_wasted_huffman = cvc_tax_vs_huffman * len(limn_words)

    # What fraction of total encoding is wasted?
    total_fixed = limn_fixed_bits * len(limn_words)
    waste_frac_entropy = cvc_tax_vs_entropy / limn_fixed_bits
    waste_frac_huffman = cvc_tax_vs_huffman / limn_fixed_bits

    print(f"\n  Limn fixed encoding: {limn_fixed_bits:.2f} bits/word (3 chars × {math.log2(26):.2f} bits/char)")
    print(f"  Shannon entropy:     {limn_entropy:.2f} bits/word")
    print(f"  Huffman code:        {limn_huffman:.2f} bits/word")
    print(f"\n  CVC tax vs entropy:  {cvc_tax_vs_entropy:.2f} bits/word ({waste_frac_entropy:.1%} overhead)")
    print(f"  CVC tax vs Huffman:  {cvc_tax_vs_huffman:.2f} bits/word ({waste_frac_huffman:.1%} overhead)")
    print(f"\n  Over corpus ({len(limn_words)} tokens):")
    print(f"    Total fixed encoding: {total_fixed:.0f} bits ({total_fixed/8:.0f} bytes)")
    print(f"    Wasted vs entropy:    {total_wasted_entropy:.0f} bits ({total_wasted_entropy/8:.0f} bytes)")
    print(f"    Wasted vs Huffman:    {total_wasted_huffman:.0f} bits ({total_wasted_huffman/8:.0f} bytes)")

    # =========================================================================
    # Part 5: Optimal variable-length Limn
    # =========================================================================
    print(f"\n[5] What Optimal Variable-Length Limn Would Look Like")

    # If we could assign shorter codes to frequent words...
    top_words = limn_freq.most_common(30)
    total = sum(limn_freq.values())

    # Compute actual Huffman code lengths
    import heapq

    class Node:
        def __init__(self, sym=None, weight=0, left=None, right=None):
            self.sym = sym
            self.weight = weight
            self.left = left
            self.right = right

        def __lt__(self, other):
            return self.weight < other.weight

    nodes = [Node(sym=sym, weight=count / total) for sym, count in limn_freq.items()]
    heapq.heapify(nodes)

    while len(nodes) > 1:
        left = heapq.heappop(nodes)
        right = heapq.heappop(nodes)
        parent = Node(weight=left.weight + right.weight, left=left, right=right)
        heapq.heappush(nodes, parent)

    root = nodes[0]
    code_lengths = {}

    def traverse(node, depth):
        if node.sym is not None:
            code_lengths[node.sym] = max(1, depth)
        if node.left:
            traverse(node.left, depth + 1)
        if node.right:
            traverse(node.right, depth + 1)

    traverse(root, 0)

    print(f"\n  {'Word':>6s}  {'Count':>5s}  {'Freq':>6s}  {'Fixed':>6s}  {'Huffman':>7s}  {'Savings':>8s}")
    print(f"  {'─' * 46}")

    for word, count in top_words:
        freq = count / total
        fixed = 3  # chars
        huff_bits = code_lengths.get(word, 10)
        # Convert to equivalent chars: bits / log2(26)
        huff_chars = huff_bits / math.log2(26)
        savings = (1 - huff_chars / fixed) * 100
        print(f"  {word:>6s}  {count:>5d}  {freq:>5.1%}  {fixed:>4d}ch  {huff_chars:>5.1f}ch  {savings:>+6.1f}%")

    # =========================================================================
    # Part 6: Comparison with English
    # =========================================================================
    print(f"\n[6] English Variable-Length Efficiency")

    en_entropy_overhead = en_fixed_bits - english_entropy
    en_huffman_overhead = en_fixed_bits - english_huffman

    print(f"  English avg encoding: {en_fixed_bits:.2f} bits/word ({en_avg_len:.1f} chars × {math.log2(26):.2f} bits/char)")
    print(f"  English entropy:      {english_entropy:.2f} bits/word")
    print(f"  English Huffman:      {english_huffman:.2f} bits/word")
    print(f"  English overhead:     {en_entropy_overhead:.2f} bits/word ({en_entropy_overhead/en_fixed_bits:.1%})")

    print(f"\n  Key comparison:")
    print(f"    Limn encoding efficiency:    {limn_entropy/limn_fixed_bits:.1%} (entropy/fixed)")
    print(f"    English encoding efficiency: {english_entropy/en_fixed_bits:.1%} (entropy/fixed)")

    if limn_entropy / limn_fixed_bits < english_entropy / en_fixed_bits:
        print(f"\n    Limn is LESS efficient — wastes more of its encoding capacity")
        print(f"    Despite being denser per character, the fixed-width constraint")
        print(f"    prevents adaptation to usage frequency")
    else:
        print(f"\n    Limn is MORE efficient — better utilizes its encoding capacity")

    # =========================================================================
    # Part 7: The machine communication question
    # =========================================================================
    print(f"\n[7] Implication for Machine Communication")

    # Compute: what if Limn used variable-length (1-5 char) words?
    # Top 8 words (2^3 = 8): 1-char codes → 3 bits each
    # Next 56 words (2^6 - 2^3): 2-char codes → 6 bits each (but need prefix-free)
    # Actually, just use Huffman bits directly

    huffman_total = sum(code_lengths.get(w, 10) * count for w, count in limn_freq.items())
    fixed_total = limn_fixed_bits * len(limn_words)
    savings = 1 - huffman_total / fixed_total

    print(f"  Total corpus encoding (fixed 3-char): {fixed_total:.0f} bits")
    print(f"  Total corpus encoding (Huffman):      {huffman_total:.0f} bits")
    print(f"  Savings from variable-length:         {savings:.1%}")
    print(f"\n  For machine-to-machine communication, variable-length tokens")
    print(f"  would save {savings:.0%} of bandwidth with zero semantic loss.")
    print(f"  The CVC constraint is purely a human-learnability concession.")

    # =========================================================================
    # Summary
    # =========================================================================
    print(f"\n{'=' * 70}")
    print(f"  SUMMARY")
    print(f"{'=' * 70}")

    print(f"\n  CVC Tax:           {cvc_tax_vs_huffman:.2f} bits/word ({waste_frac_huffman:.1%} of encoding)")
    print(f"  Corpus waste:      {total_wasted_huffman:.0f} bits ({total_wasted_huffman/8:.0f} bytes)")
    print(f"  Variable savings:  {savings:.1%} of total bandwidth")

    if waste_frac_huffman > 0.30:
        print(f"\n  H18 STRONGLY SUPPORTED: Fixed CVC wastes >{waste_frac_huffman:.0%} of encoding capacity.")
        print(f"  For machine communication, variable-length tokens are strictly superior.")
        h18_result = "STRONGLY SUPPORTED"
    elif waste_frac_huffman > 0.15:
        print(f"\n  H18 SUPPORTED: Fixed CVC wastes {waste_frac_huffman:.0%} of encoding capacity.")
        h18_result = "SUPPORTED"
    else:
        print(f"\n  H18 CHALLENGED: CVC overhead is modest ({waste_frac_huffman:.0%}).")
        h18_result = "CHALLENGED"

    # Save results
    results = {
        "experiment": "zipfian_efficiency_h18",
        "pairs_count": len(pairs),
        "limn_tokens": len(limn_words),
        "limn_unique": len(limn_freq),
        "english_tokens": len(english_words),
        "english_unique": len(english_freq),
        "encoding": {
            "limn_fixed_bits_per_word": limn_fixed_bits,
            "limn_entropy_bits_per_word": limn_entropy,
            "limn_huffman_bits_per_word": limn_huffman,
            "english_fixed_bits_per_word": en_fixed_bits,
            "english_entropy_bits_per_word": english_entropy,
            "english_huffman_bits_per_word": english_huffman,
        },
        "cvc_tax": {
            "bits_per_word_vs_entropy": cvc_tax_vs_entropy,
            "bits_per_word_vs_huffman": cvc_tax_vs_huffman,
            "overhead_fraction": waste_frac_huffman,
            "total_wasted_bits": total_wasted_huffman,
            "variable_length_savings": savings,
        },
        "zipf": {
            "limn_alpha": limn_zipf["alpha"] if limn_zipf else None,
            "english_alpha": english_zipf["alpha"] if english_zipf else None,
        },
        "h18_verdict": h18_result,
    }

    output_file = OUTPUT_DIR / "zipfian_results.json"
    with open(output_file, "w") as f:
        json.dump(results, f, indent=2)
    print(f"\n  Results saved to: {output_file}")


if __name__ == "__main__":
    main()
