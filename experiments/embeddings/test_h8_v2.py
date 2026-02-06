#!/usr/bin/env python3
"""
H8 Test v2: Embedder generalization with proper controls.

v1 showed suspiciously perfect scores (0.97+). This version adds:
1. Fixed HGttG path (the REAL out-of-domain test)
2. Negative controls (mismatched pairs — should score LOW)
3. Random string controls (should score near 0)
4. Semantic-only descriptions (no keyword overlap with Limn words)

If negatives also score high, the embedding space has collapsed.

— Lex
"""

import json
import os
import sys
import random
import numpy as np

from sentence_transformers import SentenceTransformer

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
EMBEDDER_PATH = os.path.join(SCRIPT_DIR, "limn-embedder")
HGTTG_PATH = "/home/eric/src/limntown/limn/crew/translator/hgttg-training-pairs.jsonl"


def load_hgttg_pairs(n=30):
    pairs = []
    with open(HGTTG_PATH, 'r') as f:
        for line in f:
            obj = json.loads(line.strip())
            en = obj.get("english", "")
            limn = obj.get("limn", "")
            if en and limn and len(en) < 300:
                pairs.append({"limn": limn, "english": en})
            if len(pairs) >= n:
                break
    return pairs


# Positive pairs: standard Limn CVC → English
POSITIVE_VOCAB = [
    {"limn": "lov", "english": "love"},
    {"limn": "fea", "english": "fear"},
    {"limn": "joy", "english": "joy"},
    {"limn": "kno", "english": "knowledge"},
    {"limn": "gro", "english": "growth"},
    {"limn": "tim", "english": "time"},
    {"limn": "spa", "english": "space"},
    {"limn": "lif", "english": "life"},
    {"limn": "dep", "english": "depth"},
    {"limn": "hop", "english": "hope"},
    {"limn": "wis", "english": "wisdom"},
    {"limn": "tru", "english": "truth"},
    {"limn": "sad", "english": "sadness"},
    {"limn": "str", "english": "strength"},
    {"limn": "lig", "english": "light"},
    {"limn": "dar", "english": "darkness"},
    {"limn": "cha", "english": "change"},
    {"limn": "flo", "english": "flow"},
    {"limn": "pat", "english": "pattern"},
    {"limn": "for", "english": "form"},
]

# Negative controls: MISMATCHED pairs (should score LOW)
NEGATIVE_VOCAB = [
    {"limn": "lov", "english": "darkness"},
    {"limn": "fea", "english": "happiness"},
    {"limn": "joy", "english": "knowledge"},
    {"limn": "kno", "english": "sadness"},
    {"limn": "gro", "english": "time"},
    {"limn": "tim", "english": "fear"},
    {"limn": "spa", "english": "growth"},
    {"limn": "lif", "english": "pattern"},
    {"limn": "dep", "english": "light"},
    {"limn": "hop", "english": "form"},
    {"limn": "wis", "english": "space"},
    {"limn": "tru", "english": "depth"},
    {"limn": "sad", "english": "hope"},
    {"limn": "str", "english": "truth"},
    {"limn": "lig", "english": "love"},
    {"limn": "dar", "english": "strength"},
    {"limn": "cha", "english": "wisdom"},
    {"limn": "flo", "english": "change"},
    {"limn": "pat", "english": "flow"},
    {"limn": "for", "english": "life"},
]

# Semantic descriptions WITHOUT keyword overlap
# (e.g., "lov" described as "romantic attachment" not "love")
SEMANTIC_PAIRS = [
    {"limn": "lov", "english": "romantic attachment and deep affection"},
    {"limn": "fea", "english": "anxiety and dread of danger"},
    {"limn": "joy", "english": "elation and delight"},
    {"limn": "kno", "english": "understanding and awareness of facts"},
    {"limn": "gro", "english": "development and maturation"},
    {"limn": "tim", "english": "duration and temporal passage"},
    {"limn": "spa", "english": "physical extent and volume"},
    {"limn": "lif", "english": "biological existence and vitality"},
    {"limn": "dep", "english": "profundity and vertical extent"},
    {"limn": "hop", "english": "optimism and expectation of good"},
    {"limn": "wis", "english": "sagacity and sound judgment"},
    {"limn": "tru", "english": "verity and factual accuracy"},
    {"limn": "sad", "english": "melancholy and sorrow"},
    {"limn": "str", "english": "power and physical might"},
    {"limn": "lig", "english": "illumination and radiance"},
]

# Random gibberish (should score near 0)
RANDOM_PAIRS = [
    {"limn": "xvz", "english": "knowledge"},
    {"limn": "qwp", "english": "love"},
    {"limn": "bnm", "english": "truth"},
    {"limn": "rtf", "english": "fear"},
    {"limn": "jkl", "english": "hope"},
]


def compute_sims(model, pairs):
    if not pairs:
        return []
    limn_embs = model.encode([p["limn"] for p in pairs], normalize_embeddings=True)
    eng_embs = model.encode([p["english"] for p in pairs], normalize_embeddings=True)
    return [float(np.dot(limn_embs[i], eng_embs[i])) for i in range(len(pairs))]


def report(name, pairs, ft_sims, base_sims):
    print(f"\n{'='*70}")
    print(f"  {name}")
    print(f"{'='*70}")
    print(f"  {'Limn':<30s} {'English':<30s} {'FT':>6s} {'Base':>6s}")
    print(f"  {'-'*72}")
    for p, fs, bs in zip(pairs, ft_sims, base_sims):
        l = p["limn"][:28]
        e = p["english"][:28]
        print(f"  {l:<30s} {e:<30s} {fs:6.3f} {bs:6.3f}")
    ft_mean = np.mean(ft_sims)
    base_mean = np.mean(base_sims)
    print(f"\n  FT Mean: {ft_mean:.4f}  Base Mean: {base_mean:.4f}  Delta: {ft_mean - base_mean:+.4f}")
    return ft_mean, base_mean


def main():
    print("H8 TEST v2: Embedder Generalization with Controls")
    print("=" * 70)

    print("\nLoading models...")
    ft = SentenceTransformer(EMBEDDER_PATH)
    base = SentenceTransformer("sentence-transformers/all-MiniLM-L6-v2")

    print("Loading HGttG pairs...")
    hgttg = load_hgttg_pairs(30)
    print(f"  Loaded {len(hgttg)} pairs")

    results = {}

    # Test 1: Positive vocab
    ft_s = compute_sims(ft, POSITIVE_VOCAB)
    base_s = compute_sims(base, POSITIVE_VOCAB)
    ft_m, base_m = report("POSITIVE: CVC word → correct English", POSITIVE_VOCAB, ft_s, base_s)
    results["positive_vocab"] = {"ft": ft_m, "base": base_m}

    # Test 2: Negative vocab (CRITICAL CONTROL)
    ft_s = compute_sims(ft, NEGATIVE_VOCAB)
    base_s = compute_sims(base, NEGATIVE_VOCAB)
    ft_m, base_m = report("NEGATIVE: CVC word → WRONG English (should be LOW)", NEGATIVE_VOCAB, ft_s, base_s)
    results["negative_vocab"] = {"ft": ft_m, "base": base_m}

    # Test 3: Semantic descriptions (no keyword overlap)
    ft_s = compute_sims(ft, SEMANTIC_PAIRS)
    base_s = compute_sims(base, SEMANTIC_PAIRS)
    ft_m, base_m = report("SEMANTIC: CVC word → description (no keyword overlap)", SEMANTIC_PAIRS, ft_s, base_s)
    results["semantic"] = {"ft": ft_m, "base": base_m}

    # Test 4: Random strings
    ft_s = compute_sims(ft, RANDOM_PAIRS)
    base_s = compute_sims(base, RANDOM_PAIRS)
    ft_m, base_m = report("RANDOM: gibberish → English (should be near 0)", RANDOM_PAIRS, ft_s, base_s)
    results["random"] = {"ft": ft_m, "base": base_m}

    # Test 5: HGttG (the real test)
    ft_s = compute_sims(ft, hgttg)
    base_s = compute_sims(base, hgttg)
    ft_m, base_m = report("HGttG: real Limn translations (TRUE out-of-domain)", hgttg, ft_s, base_s)
    results["hgttg"] = {"ft": ft_m, "base": base_m}

    # Analysis
    print(f"\n\n{'#'*70}")
    print(f"  ANALYSIS")
    print(f"{'#'*70}")

    pos = results["positive_vocab"]["ft"]
    neg = results["negative_vocab"]["ft"]
    sem = results["semantic"]["ft"]
    rnd = results["random"]["ft"]
    hg = results["hgttg"]["ft"]

    print(f"\n  Positive (correct matches):     {pos:.4f}")
    print(f"  Negative (wrong matches):       {neg:.4f}")
    print(f"  Semantic (no keyword overlap):  {sem:.4f}")
    print(f"  Random (gibberish):             {rnd:.4f}")
    print(f"  HGttG (real translations):      {hg:.4f}")

    discrimination = pos - neg
    print(f"\n  Discrimination (pos - neg):     {discrimination:.4f}")

    if discrimination < 0.1:
        print(f"\n  CRITICAL: Embedding space has COLLAPSED!")
        print(f"  The model scores {neg:.3f} on WRONG pairs vs {pos:.3f} on correct.")
        print(f"  It cannot distinguish correct from incorrect mappings.")
        verdict = "COLLAPSED"
    elif neg > 0.7:
        print(f"\n  WARNING: High negative scores ({neg:.3f}) suggest partial collapse.")
        print(f"  Some discrimination exists but model is unreliable.")
        verdict = "PARTIAL_COLLAPSE"
    elif hg < 0.3:
        print(f"\n  FALSIFIED: Embedder does NOT generalize to real Limn.")
        print(f"  HGttG score ({hg:.3f}) is below 0.3 threshold.")
        verdict = "FALSIFIED"
    elif hg < 0.5:
        print(f"\n  WEAK: Minimal transfer to real Limn translations.")
        verdict = "WEAK"
    elif sem < 0.3:
        print(f"\n  KEYWORD-DEPENDENT: High scores on direct matches ({pos:.3f})")
        print(f"  but low on semantic descriptions ({sem:.3f}).")
        print(f"  The model matches surface forms, not meaning.")
        verdict = "KEYWORD_ONLY"
    else:
        print(f"\n  CONFIRMED: Embedder shows genuine generalization.")
        verdict = "CONFIRMED"

    print(f"\n  H8 VERDICT: {verdict}")

    # Save
    output = {
        "test": "H8 v2 — Embedder generalization with controls",
        "verdict": verdict,
        "discrimination": float(discrimination),
        "results": {k: {kk: float(vv) for kk, vv in v.items()} for k, v in results.items()},
    }
    out_path = os.path.join(SCRIPT_DIR, "h8_v2_results.json")
    with open(out_path, 'w') as f:
        json.dump(output, f, indent=2)
    print(f"\n  Saved to: {out_path}")


if __name__ == "__main__":
    main()
