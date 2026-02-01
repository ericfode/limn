#!/usr/bin/env python3
"""
Experiment 005: Contradiction Test
Do contradictions score lower than valid compositions?

Hypothesis: hot col, sol gas, etc. should show lower compositional
similarity than valid compositions (0.5-0.7 vs 0.88)

This tests if embeddings capture Limn's semantic constraints.
"""

import numpy as np
from sentence_transformers import SentenceTransformer
from scipy.spatial.distance import cosine
from scipy.stats import ttest_ind

# Contradiction test cases
CONTRADICTIONS = [
    {'a': 'hot', 'b': 'col', 'combo': 'hot col', 'expected': 'impossible (hot+cold)'},
    {'a': 'sol', 'b': 'gas', 'combo': 'sol gas', 'expected': 'impossible (solid+gas)'},
    {'a': 'big', 'b': 'sma', 'combo': 'big sma', 'expected': 'impossible (big+small)'},
    {'a': 'bri', 'b': 'dim', 'combo': 'bri dim', 'expected': 'impossible (bright+dim)'},
    {'a': 'joy', 'b': 'sad', 'combo': 'joy sad', 'expected': 'impossible (joy+sad)'},
]

# Valid compositions for comparison (from previous test)
VALID_CASES = [
    {'a': 'sol', 'b': 'aqu', 'combo': 'sol aqu', 'expected': 'ice'},
    {'a': 'liq', 'b': 'aqu', 'combo': 'liq aqu', 'expected': 'water'},
    {'a': 'gas', 'b': 'aqu', 'combo': 'gas aqu', 'expected': 'steam'},
    {'a': 'hot', 'b': 'dry', 'combo': 'hot dry', 'expected': 'desert'},
    {'a': 'col', 'b': 'wet', 'combo': 'col wet', 'expected': 'rain'},
    {'a': 'joy', 'b': 'lov', 'combo': 'joy lov', 'expected': 'happiness'},
    {'a': 'sad', 'b': 'wan', 'combo': 'sad wan', 'expected': 'longing'},
    {'a': 'big', 'b': 'str', 'combo': 'big str', 'expected': 'powerful'},
    {'a': 'old', 'b': 'kno', 'combo': 'old kno', 'expected': 'wisdom'},
]

def cosine_similarity(a, b):
    return 1 - cosine(a, b)

def test_contradictions(model):
    print("\n" + "="*70)
    print("CONTRADICTION TEST")
    print("="*70)
    print("\nHypothesis: Contradictions score LOWER than valid compositions")
    print("Valid baseline: 0.88")
    print("Contradiction prediction: 0.5-0.7\n")

    # Test contradictions
    print("TESTING CONTRADICTIONS:")
    print("-" * 70)

    contradiction_scores = []

    for i, case in enumerate(CONTRADICTIONS, 1):
        emb_a = model.encode(case['a'])
        emb_b = model.encode(case['b'])
        emb_combo = model.encode(case['combo'])

        composed = emb_a + emb_b
        composed = composed / np.linalg.norm(composed)

        similarity = cosine_similarity(composed, emb_combo)
        contradiction_scores.append(similarity)

        print(f"[{i}/5] {case['a']:3s} + {case['b']:3s} → {case['combo']:8s}")
        print(f"      Similarity: {similarity:.4f}")
        print(f"      Expected: {case['expected']}")
        print()

    # Test valid compositions for comparison
    print("\n" + "="*70)
    print("VALID COMPOSITIONS (BASELINE):")
    print("-" * 70)

    valid_scores = []

    for i, case in enumerate(VALID_CASES, 1):
        emb_a = model.encode(case['a'])
        emb_b = model.encode(case['b'])
        emb_combo = model.encode(case['combo'])

        composed = emb_a + emb_b
        composed = composed / np.linalg.norm(composed)

        similarity = cosine_similarity(composed, emb_combo)
        valid_scores.append(similarity)

        print(f"[{i}/9] {case['a']:3s} + {case['b']:3s} → {case['combo']:8s} = {similarity:.4f} ({case['expected']})")

    # Statistical comparison
    print("\n" + "="*70)
    print("STATISTICAL COMPARISON")
    print("="*70)

    contra_mean = np.mean(contradiction_scores)
    contra_std = np.std(contradiction_scores)
    valid_mean = np.mean(valid_scores)
    valid_std = np.std(valid_scores)

    print(f"\nContradictions:  Mean = {contra_mean:.4f}, Std = {contra_std:.4f}")
    print(f"Valid:           Mean = {valid_mean:.4f}, Std = {valid_std:.4f}")
    print(f"Difference:      Δ = {valid_mean - contra_mean:.4f}")

    # t-test
    t_stat, p_value = ttest_ind(valid_scores, contradiction_scores)
    print(f"\nt-test: t = {t_stat:.4f}, p = {p_value:.6f}")

    # Interpretation
    print("\n" + "="*70)
    print("INTERPRETATION")
    print("="*70)

    if contra_mean < 0.7 and valid_mean > 0.8:
        if p_value < 0.01:
            print("\n✓✓✓ HYPOTHESIS STRONGLY CONFIRMED ✓✓✓")
            print(f"  Contradictions ({contra_mean:.2f}) << Valid ({valid_mean:.2f})")
            print(f"  Difference: {valid_mean - contra_mean:.2f} (p < 0.01)")
            print("  Embeddings DO capture Limn's semantic constraints!")
            print("  Contradictory combinations are detectably different.")
        else:
            print("\n✓ HYPOTHESIS CONFIRMED (not statistically significant)")
            print(f"  Contradictions ({contra_mean:.2f}) < Valid ({valid_mean:.2f})")
            print(f"  But p = {p_value:.4f} (p > 0.01)")
    elif contra_mean < valid_mean:
        print(f"\n⚠ WEAK SUPPORT")
        print(f"  Contradictions ({contra_mean:.2f}) < Valid ({valid_mean:.2f})")
        print(f"  But difference is small ({valid_mean - contra_mean:.2f})")
    else:
        print(f"\n✗ HYPOTHESIS REJECTED")
        print(f"  Contradictions ({contra_mean:.2f}) >= Valid ({valid_mean:.2f})")
        print("  Embeddings do NOT distinguish contradictions")

    # Effect size (Cohen's d)
    pooled_std = np.sqrt(((len(valid_scores)-1)*valid_std**2 + (len(contradiction_scores)-1)*contra_std**2) /
                         (len(valid_scores) + len(contradiction_scores) - 2))
    cohens_d = (valid_mean - contra_mean) / pooled_std

    print(f"\nEffect size (Cohen's d): {cohens_d:.4f}")
    if abs(cohens_d) > 0.8:
        print("  → LARGE effect")
    elif abs(cohens_d) > 0.5:
        print("  → MEDIUM effect")
    elif abs(cohens_d) > 0.2:
        print("  → SMALL effect")
    else:
        print("  → NEGLIGIBLE effect")

    # Distribution comparison
    print("\n" + "="*70)
    print("DISTRIBUTION VISUALIZATION")
    print("="*70)

    print("\nValid compositions:")
    print(f"  Range: {min(valid_scores):.4f} - {max(valid_scores):.4f}")
    print(f"  Mean:  {'█' * int(valid_mean * 50)} {valid_mean:.4f}")

    print("\nContradictions:")
    print(f"  Range: {min(contradiction_scores):.4f} - {max(contradiction_scores):.4f}")
    print(f"  Mean:  {'█' * int(contra_mean * 50)} {contra_mean:.4f}")

    print("\n0.0                    0.5                    1.0")
    print("|" + "-" * 23 + "|" + "-" * 23 + "|")

    return {
        'contradiction_scores': [float(x) for x in contradiction_scores],
        'valid_scores': [float(x) for x in valid_scores],
        'contradiction_mean': float(contra_mean),
        'valid_mean': float(valid_mean),
        'difference': float(valid_mean - contra_mean),
        'p_value': float(p_value),
        'cohens_d': float(cohens_d)
    }

if __name__ == "__main__":
    print("Loading sentence-transformers model...")
    model = SentenceTransformer('all-MiniLM-L6-v2')
    print("Model loaded!\n")

    results = test_contradictions(model)

    # Save results
    import json
    with open('contradiction-test-results.json', 'w') as f:
        json.dump(results, f, indent=2)

    print(f"\n✓ Results saved to contradiction-test-results.json")
