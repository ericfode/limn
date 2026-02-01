#!/usr/bin/env python3
"""
Experiment 005: Operator Consistency Test
Do operators (nu, ve, so) show consistent vector transformations?

Tests:
- nu (negation): Does nu hot ≈ col (cold)?
- ve (intensifier): Does ve hot show consistent amplification?
- so (weakener): Does so hot show consistent reduction?
"""

import numpy as np
from sentence_transformers import SentenceTransformer
from scipy.spatial.distance import cosine

def cosine_similarity(a, b):
    return 1 - cosine(a, b)

# Test cases for operators
NEGATION_TESTS = [
    {'word': 'hot', 'negated': 'nu hot', 'antonym': 'col', 'antonym_word': 'cold'},
    {'word': 'big', 'negated': 'nu big', 'antonym': 'sma', 'antonym_word': 'small'},
    {'word': 'bri', 'negated': 'nu bri', 'antonym': 'dim', 'antonym_word': 'dim'},
    {'word': 'joy', 'negated': 'nu joy', 'antonym': 'sad', 'antonym_word': 'sad'},
    {'word': 'up', 'negated': 'nu up', 'antonym': 'dow', 'antonym_word': 'down'},
]

INTENSIFIER_TESTS = [
    {'base': 'hot', 'intensified': 've hot', 'english_base': 'hot', 'english_intense': 'very hot'},
    {'base': 'big', 'intensified': 've big', 'english_base': 'big', 'english_intense': 'very big'},
    {'base': 'bri', 'intensified': 've bri', 'english_base': 'bright', 'english_intense': 'very bright'},
]

WEAKENER_TESTS = [
    {'base': 'hot', 'weakened': 'so hot', 'english_base': 'hot', 'english_weak': 'somewhat hot'},
    {'base': 'big', 'weakened': 'so big', 'english_base': 'big', 'english_weak': 'somewhat big'},
]

def test_negation(model):
    print("\n" + "="*70)
    print("NEGATION OPERATOR TEST (nu)")
    print("="*70)
    print("\nHypothesis: nu X should be similar to antonym(X)")
    print("Example: nu hot ≈ col (cold)\n")

    results = []

    for i, test in enumerate(NEGATION_TESTS, 1):
        # Get embeddings
        emb_word = model.encode(test['word'])
        emb_negated = model.encode(test['negated'])
        emb_antonym = model.encode(test['antonym'])

        # Test 1: Is nu X similar to antonym?
        sim_negated_antonym = cosine_similarity(emb_negated, emb_antonym)

        # Test 2: Is nu X dissimilar to X?
        sim_negated_original = cosine_similarity(emb_negated, emb_word)

        # Test 3: Vector arithmetic check
        # If nu is negation, emb(nu X) should be far from emb(X)
        # and close to emb(antonym)

        results.append({
            'word': test['word'],
            'negated': test['negated'],
            'antonym': test['antonym'],
            'sim_to_antonym': sim_negated_antonym,
            'sim_to_original': sim_negated_original
        })

        print(f"[{i}/{len(NEGATION_TESTS)}] {test['negated']:8s} vs {test['antonym']:3s} ({test['antonym_word']})")
        print(f"      Similarity to antonym:  {sim_negated_antonym:.4f}")
        print(f"      Similarity to original: {sim_negated_original:.4f}")

        if sim_negated_antonym > 0.7:
            print(f"      → ✓ Strong negation effect")
        elif sim_negated_antonym > 0.5:
            print(f"      → ⚠ Moderate negation effect")
        else:
            print(f"      → ✗ Weak negation effect")
        print()

    # Summary
    mean_antonym_sim = np.mean([r['sim_to_antonym'] for r in results])
    mean_original_sim = np.mean([r['sim_to_original'] for r in results])

    print("\n" + "-"*70)
    print("SUMMARY:")
    print(f"  Mean similarity (nu X, antonym):  {mean_antonym_sim:.4f}")
    print(f"  Mean similarity (nu X, X):        {mean_original_sim:.4f}")

    if mean_antonym_sim > 0.7 and mean_original_sim < mean_antonym_sim:
        print("\n  ✓ NEGATION OPERATOR WORKS")
        print("    nu X consistently maps toward antonym(X)")
    elif mean_antonym_sim > 0.5:
        print("\n  ⚠ WEAK NEGATION EFFECT")
        print("    nu X shows some association with antonym, but weak")
    else:
        print("\n  ✗ NEGATION OPERATOR FAILS")
        print("    nu X does not map to antonym(X)")

    return results

def test_intensifier(model):
    print("\n" + "="*70)
    print("INTENSIFIER OPERATOR TEST (ve)")
    print("="*70)
    print("\nHypothesis: ve X should amplify meaning")
    print("Limn 've hot' should behave like English 'very hot'\n")

    results = []

    for i, test in enumerate(INTENSIFIER_TESTS, 1):
        # Limn
        emb_base = model.encode(test['base'])
        emb_intensified = model.encode(test['intensified'])

        # English
        emb_eng_base = model.encode(test['english_base'])
        emb_eng_intense = model.encode(test['english_intense'])

        # Similarity tests
        sim_limn = cosine_similarity(emb_intensified, emb_base)
        sim_english = cosine_similarity(emb_eng_intense, emb_eng_base)

        # Cross-language comparison
        sim_limn_to_eng_intense = cosine_similarity(emb_intensified, emb_eng_intense)

        results.append({
            'base': test['base'],
            'intensified': test['intensified'],
            'limn_similarity': sim_limn,
            'english_similarity': sim_english,
            'cross_similarity': sim_limn_to_eng_intense
        })

        print(f"[{i}/{len(INTENSIFIER_TESTS)}] {test['intensified']:8s} vs {test['base']:3s}")
        print(f"      Limn similarity:    {sim_limn:.4f}")
        print(f"      English similarity: {sim_english:.4f}")
        print(f"      Cross-language:     {sim_limn_to_eng_intense:.4f}")
        print()

    mean_limn = np.mean([r['limn_similarity'] for r in results])
    mean_english = np.mean([r['english_similarity'] for r in results])

    print("\n" + "-"*70)
    print("SUMMARY:")
    print(f"  Limn 've X' similarity:    {mean_limn:.4f}")
    print(f"  English 'very X' similarity: {mean_english:.4f}")

    if abs(mean_limn - mean_english) < 0.1:
        print("\n  ✓ INTENSIFIER BEHAVES LIKE ENGLISH 'very'")
    else:
        print("\n  ⚠ INTENSIFIER DIFFERS FROM ENGLISH 'very'")

    return results

def test_weakener(model):
    print("\n" + "="*70)
    print("WEAKENER OPERATOR TEST (so)")
    print("="*70)
    print("\nHypothesis: so X should weaken meaning")
    print("Limn 'so hot' should behave like English 'somewhat hot'\n")

    results = []

    for i, test in enumerate(WEAKENER_TESTS, 1):
        # Limn
        emb_base = model.encode(test['base'])
        emb_weakened = model.encode(test['weakened'])

        # English
        emb_eng_base = model.encode(test['english_base'])
        emb_eng_weak = model.encode(test['english_weak'])

        sim_limn = cosine_similarity(emb_weakened, emb_base)
        sim_english = cosine_similarity(emb_eng_weak, emb_eng_base)

        results.append({
            'base': test['base'],
            'weakened': test['weakened'],
            'limn_similarity': sim_limn,
            'english_similarity': sim_english
        })

        print(f"[{i}/{len(WEAKENER_TESTS)}] {test['weakened']:8s} vs {test['base']:3s}")
        print(f"      Limn similarity:    {sim_limn:.4f}")
        print(f"      English similarity: {sim_english:.4f}")
        print()

    mean_limn = np.mean([r['limn_similarity'] for r in results])
    mean_english = np.mean([r['english_similarity'] for r in results])

    print("\n" + "-"*70)
    print("SUMMARY:")
    print(f"  Limn 'so X' similarity:    {mean_limn:.4f}")
    print(f"  English 'somewhat X' similarity: {mean_english:.4f}")

    if abs(mean_limn - mean_english) < 0.1:
        print("\n  ✓ WEAKENER BEHAVES LIKE ENGLISH 'somewhat'")
    else:
        print("\n  ⚠ WEAKENER DIFFERS FROM ENGLISH 'somewhat'")

    return results

if __name__ == "__main__":
    print("Loading sentence-transformers model...")
    model = SentenceTransformer('all-MiniLM-L6-v2')
    print("Model loaded!\n")

    # Run all tests
    negation_results = test_negation(model)
    intensifier_results = test_intensifier(model)
    weakener_results = test_weakener(model)

    # Save results
    import json
    results = {
        'negation': negation_results,
        'intensifier': intensifier_results,
        'weakener': weakener_results
    }

    with open('operator-test-results.json', 'w') as f:
        json.dump(results, f, indent=2)

    print("\n✓ Results saved to operator-test-results.json")
