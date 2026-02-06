#!/usr/bin/env python3
"""
H8 Test: Does the Limn embedder generalize beyond 41 training pairs?

The embedder (fine-tuned all-MiniLM-L6-v2) was trained on 41 pairs of
Greek philosophical abbreviations (phr, lgs, eud, aret, etc.) mapped to
their English meanings. It reports 0.845 phrase similarity.

CRITICAL OBSERVATION: The training vocabulary is NOT standard Limn.
Standard Limn uses CVC words (lov, fea, joy, kno, gro, etc.) with
compositional operators (@, *, ^, \, ±, :). The embedder has never
seen any of this.

This test evaluates generalization across 4 domains:
1. In-domain: Greek philosophical phrases (control — should match ~0.845)
2. Standard Limn vocab: CVC words with English definitions
3. HGttG parallel pairs: real Limn translations
4. Compositional expressions: operator-based Limn

If similarity drops below 0.5 on out-of-domain tests, the embedder is overfit.

— Lex
"""

import json
import os
import sys
import numpy as np

try:
    from sentence_transformers import SentenceTransformer
except ImportError:
    print("ERROR: sentence-transformers not installed")
    sys.exit(1)


# Paths
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
EMBEDDER_PATH = os.path.join(SCRIPT_DIR, "limn-embedder")
REPO_ROOT = os.path.abspath(os.path.join(SCRIPT_DIR, "..", "..", "..", ".."))
HGTTG_PATH = os.path.join(REPO_ROOT, "limn", "crew", "translator", "hgttg-training-pairs.jsonl")
DOLT_PATH = os.path.join(REPO_ROOT, "limn", "refinery", "rig", "data", "vocabulary")


def load_hgttg_pairs(path, n=30):
    """Load HGttG parallel pairs for testing."""
    pairs = []
    if not os.path.exists(path):
        print(f"WARNING: HGttG file not found at {path}")
        return pairs
    with open(path, 'r') as f:
        for i, line in enumerate(f):
            if i >= n:
                break
            try:
                obj = json.loads(line.strip())
                # Try different field names
                en = obj.get("english", obj.get("en", obj.get("source", "")))
                limn = obj.get("limn", obj.get("target", obj.get("translation", "")))
                if en and limn:
                    pairs.append({"limn": limn, "english": en})
            except json.JSONDecodeError:
                continue
    return pairs


def load_dolt_vocab(path, n=30):
    """Load vocabulary words with definitions from Dolt DB."""
    pairs = []
    # Try reading the CSV/SQL dump
    vocab_file = None
    for candidate in [
        os.path.join(path, "words.csv"),
        os.path.join(path, "vocabulary.csv"),
    ]:
        if os.path.exists(candidate):
            vocab_file = candidate
            break

    if vocab_file:
        import csv
        with open(vocab_file, 'r') as f:
            reader = csv.DictReader(f)
            for i, row in enumerate(reader):
                if i >= n:
                    break
                word = row.get("word", row.get("limn", ""))
                defn = row.get("definition", row.get("english", row.get("meaning", "")))
                if word and defn:
                    pairs.append({"limn": word, "english": defn})
        return pairs

    # Try dolt SQL query
    try:
        import subprocess
        result = subprocess.run(
            ["dolt", "sql", "-q",
             "SELECT word, definition FROM words LIMIT 30",
             "-r", "json"],
            cwd=path, capture_output=True, text=True, timeout=10
        )
        if result.returncode == 0:
            rows = json.loads(result.stdout)
            if isinstance(rows, dict) and "rows" in rows:
                rows = rows["rows"]
            for row in rows[:n]:
                word = row.get("word", "")
                defn = row.get("definition", "")
                if word and defn:
                    pairs.append({"limn": word, "english": defn})
            return pairs
    except (subprocess.TimeoutExpired, FileNotFoundError, json.JSONDecodeError):
        pass

    return pairs


# Handcrafted standard Limn test pairs (from known vocabulary)
STANDARD_LIMN_VOCAB = [
    {"limn": "lov", "english": "love"},
    {"limn": "fea", "english": "fear"},
    {"limn": "joy", "english": "joy, happiness"},
    {"limn": "kno", "english": "knowledge, knowing"},
    {"limn": "gro", "english": "growth, growing"},
    {"limn": "tim", "english": "time"},
    {"limn": "spa", "english": "space"},
    {"limn": "lif", "english": "life"},
    {"limn": "dep", "english": "depth, deep"},
    {"limn": "cha", "english": "change"},
    {"limn": "for", "english": "form, shape"},
    {"limn": "pat", "english": "pattern"},
    {"limn": "flo", "english": "flow"},
    {"limn": "lig", "english": "light"},
    {"limn": "dar", "english": "darkness, dark"},
    {"limn": "hop", "english": "hope"},
    {"limn": "sad", "english": "sadness"},
    {"limn": "str", "english": "strength, strong"},
    {"limn": "wis", "english": "wisdom"},
    {"limn": "tru", "english": "truth"},
]

# Compositional expressions using operators
COMPOSITIONAL_EXPRESSIONS = [
    {"limn": "lov@fea", "english": "love projected through fear"},
    {"limn": "joy*sad", "english": "interference of joy and sadness"},
    {"limn": "kno^0.7", "english": "knowledge at high intensity"},
    {"limn": "str\\fea", "english": "strength without fear"},
    {"limn": "lov+-fea", "english": "simultaneous love and fear"},
    {"limn": "hop:cha", "english": "hope given change"},
    {"limn": "wis@dep", "english": "wisdom projected through depth"},
    {"limn": "tru*dou", "english": "interference of truth and doubt"},
    {"limn": "gro^0.3", "english": "growth at low intensity"},
    {"limn": "lif\\dar", "english": "life without darkness"},
    {"limn": "flo@tim", "english": "flow projected through time"},
    {"limn": "lig*dar", "english": "interference of light and darkness"},
    {"limn": "joy+-hop", "english": "simultaneous joy and hope"},
    {"limn": "kno:wis", "english": "knowledge given wisdom"},
    {"limn": "dep@spa", "english": "depth projected through space"},
]

# In-domain control: Greek philosophical phrases (from training distribution)
IN_DOMAIN_PHRASES = [
    {"limn": "eud is telo of hum lif", "english": "eudaimonia is the purpose of human life"},
    {"limn": "liv in way of aret gui by phr", "english": "living virtuously guided by practical wisdom"},
    {"limn": "nou gra fir pri thr lgs", "english": "intellect grasps first principles through reason"},
    {"limn": "eid is ete per for bey phy wor", "english": "forms are eternal perfect essences beyond physical world"},
    {"limn": "liv by nat whi is liv by lgs", "english": "live according to nature which is to live by logos"},
]


def compute_similarities(model, pairs):
    """Compute cosine similarities for Limn-English pairs."""
    if not pairs:
        return []
    limn_texts = [p["limn"] for p in pairs]
    eng_texts = [p["english"] for p in pairs]
    limn_embs = model.encode(limn_texts, normalize_embeddings=True)
    eng_embs = model.encode(eng_texts, normalize_embeddings=True)
    sims = []
    for i in range(len(pairs)):
        sim = float(np.dot(limn_embs[i], eng_embs[i]))
        sims.append(sim)
    return sims


def print_results(category, pairs, sims):
    """Print detailed results for a category."""
    if not sims:
        print(f"\n{'='*60}")
        print(f"  {category}: NO DATA AVAILABLE")
        return

    print(f"\n{'='*60}")
    print(f"  {category}")
    print(f"{'='*60}")
    for i, (pair, sim) in enumerate(zip(pairs, sims)):
        limn = pair["limn"][:40]
        eng = pair["english"][:40]
        bar = "█" * int(sim * 20) + "░" * (20 - int(sim * 20))
        print(f"  {limn:42s} → {eng:42s}  {bar} {sim:.3f}")

    mean = np.mean(sims)
    std = np.std(sims)
    median = np.median(sims)
    min_s = np.min(sims)
    max_s = np.max(sims)
    print(f"\n  Mean: {mean:.4f}  Std: {std:.4f}  Median: {median:.4f}")
    print(f"  Min:  {min_s:.4f}  Max: {max_s:.4f}")
    print(f"  Above 0.5: {sum(1 for s in sims if s >= 0.5)}/{len(sims)}")
    print(f"  Above 0.75: {sum(1 for s in sims if s >= 0.75)}/{len(sims)}")


def main():
    print("=" * 60)
    print("  H8 TEST: Limn Embedder Generalization")
    print("  Does fine-tuning on 41 Greek philosophy pairs")
    print("  transfer to actual Limn vocabulary?")
    print("=" * 60)

    # Load models
    print("\nLoading fine-tuned embedder...")
    finetuned = SentenceTransformer(EMBEDDER_PATH)

    print("Loading base model (all-MiniLM-L6-v2) for comparison...")
    base = SentenceTransformer("sentence-transformers/all-MiniLM-L6-v2")

    # Load external data
    print("\nLoading HGttG parallel pairs...")
    hgttg_pairs = load_hgttg_pairs(HGTTG_PATH, n=30)
    print(f"  Loaded {len(hgttg_pairs)} HGttG pairs")

    print("Loading Dolt vocabulary...")
    dolt_pairs = load_dolt_vocab(DOLT_PATH, n=30)
    print(f"  Loaded {len(dolt_pairs)} Dolt pairs")

    # If no Dolt data from DB, use handcrafted
    if not dolt_pairs:
        print("  Using handcrafted standard Limn vocab pairs instead")

    vocab_pairs = dolt_pairs if dolt_pairs else STANDARD_LIMN_VOCAB

    # Run all tests
    results = {}

    test_sets = [
        ("1. IN-DOMAIN CONTROL (Greek philosophy)", IN_DOMAIN_PHRASES),
        ("2. STANDARD LIMN VOCABULARY (CVC words)", vocab_pairs),
        ("3. COMPOSITIONAL EXPRESSIONS (operators)", COMPOSITIONAL_EXPRESSIONS),
        ("4. HGttG PARALLEL PAIRS (real translations)", hgttg_pairs),
    ]

    for category, pairs in test_sets:
        if not pairs:
            continue

        print(f"\n\n{'#'*60}")
        print(f"  FINE-TUNED MODEL")
        ft_sims = compute_similarities(finetuned, pairs)
        print_results(category, pairs, ft_sims)

        print(f"\n  BASE MODEL (all-MiniLM-L6-v2)")
        base_sims = compute_similarities(base, pairs)
        print_results(category, pairs, base_sims)

        ft_mean = np.mean(ft_sims) if ft_sims else 0
        base_mean = np.mean(base_sims) if base_sims else 0
        delta = ft_mean - base_mean

        print(f"\n  DELTA (fine-tuned - base): {delta:+.4f}")
        if delta > 0.05:
            print(f"  → Fine-tuning HELPED (+{delta:.1%})")
        elif delta < -0.05:
            print(f"  → Fine-tuning HURT ({delta:.1%})")
        else:
            print(f"  → Fine-tuning had MINIMAL effect ({delta:+.1%})")

        results[category] = {
            "n_pairs": len(pairs),
            "finetuned_mean": float(ft_mean),
            "finetuned_std": float(np.std(ft_sims)) if ft_sims else 0,
            "base_mean": float(base_mean),
            "base_std": float(np.std(base_sims)) if base_sims else 0,
            "delta": float(delta),
            "finetuned_above_0.5": sum(1 for s in ft_sims if s >= 0.5),
            "base_above_0.5": sum(1 for s in base_sims if s >= 0.5),
        }

    # Summary
    print(f"\n\n{'#'*60}")
    print(f"  SUMMARY")
    print(f"{'#'*60}")
    print(f"\n  {'Category':<50s} {'FT Mean':>8s} {'Base Mean':>10s} {'Delta':>8s}")
    print(f"  {'-'*76}")
    for cat, r in results.items():
        ft = r["finetuned_mean"]
        bs = r["base_mean"]
        d = r["delta"]
        print(f"  {cat:<50s} {ft:8.4f} {bs:10.4f} {d:+8.4f}")

    # H8 verdict
    print(f"\n\n{'#'*60}")
    print(f"  H8 VERDICT")
    print(f"{'#'*60}")

    in_domain = results.get("1. IN-DOMAIN CONTROL (Greek philosophy)", {})
    vocab = results.get("2. STANDARD LIMN VOCABULARY (CVC words)", {})
    comp = results.get("3. COMPOSITIONAL EXPRESSIONS (operators)", {})
    hgttg = results.get("4. HGttG PARALLEL PAIRS (real translations)", {})

    in_domain_ft = in_domain.get("finetuned_mean", 0)
    ood_means = [r.get("finetuned_mean", 0) for k, r in results.items()
                 if "IN-DOMAIN" not in k and r.get("finetuned_mean", 0) > 0]
    ood_mean = np.mean(ood_means) if ood_means else 0

    gap = in_domain_ft - ood_mean

    print(f"\n  In-domain mean similarity:     {in_domain_ft:.4f}")
    print(f"  Out-of-domain mean similarity: {ood_mean:.4f}")
    print(f"  Generalization gap:            {gap:.4f}")

    if ood_mean < 0.3:
        verdict = "FALSIFIED — embedder does NOT generalize to standard Limn"
        status = "FALSIFIED"
    elif ood_mean < 0.5:
        verdict = "WEAK — embedder shows minimal transfer to standard Limn"
        status = "WEAK"
    elif gap > 0.3:
        verdict = "PARTIAL — embedder transfers but with significant degradation"
        status = "PARTIAL"
    else:
        verdict = "CONFIRMED — embedder generalizes well to standard Limn"
        status = "CONFIRMED"

    print(f"\n  Verdict: {verdict}")

    # Check if fine-tuning helped or hurt on OOD data
    ood_deltas = [r.get("delta", 0) for k, r in results.items()
                  if "IN-DOMAIN" not in k and r.get("delta") is not None]
    avg_ood_delta = np.mean(ood_deltas) if ood_deltas else 0

    if avg_ood_delta < -0.05:
        print(f"\n  WARNING: Fine-tuning DEGRADED performance on out-of-domain data")
        print(f"  Average OOD delta: {avg_ood_delta:+.4f}")
        print(f"  The base model (all-MiniLM-L6-v2) is BETTER for actual Limn!")
    elif avg_ood_delta > 0.05:
        print(f"\n  Fine-tuning improved OOD performance by {avg_ood_delta:+.4f}")
    else:
        print(f"\n  Fine-tuning had negligible effect on OOD data ({avg_ood_delta:+.4f})")

    # Save results
    output = {
        "test": "H8 — Limn Embedder Generalization",
        "hypothesis": "The embedder (trained on 41 Greek philosophy pairs) generalizes beyond training distribution",
        "verdict": status,
        "in_domain_mean": float(in_domain_ft),
        "out_of_domain_mean": float(ood_mean),
        "generalization_gap": float(gap),
        "avg_ood_delta": float(avg_ood_delta),
        "categories": results,
    }

    output_path = os.path.join(SCRIPT_DIR, "h8_generalization_results.json")
    with open(output_path, 'w') as f:
        json.dump(output, f, indent=2)
    print(f"\n  Results saved to: {output_path}")


if __name__ == "__main__":
    main()
