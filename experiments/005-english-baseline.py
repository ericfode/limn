#!/usr/bin/env python3
"""
Experiment 005: English Baseline Comparison
Test if English shows same compositionality as Limn (0.88)

If English << 0.88, proves Limn's compositional advantage is REAL
"""

import numpy as np
from sentence_transformers import SentenceTransformer
from scipy.spatial.distance import cosine

# Test cases: Limn vs English
TEST_CASES = [
    # Physical compositions
    {
        'limn_a': 'sol', 'limn_b': 'aqu', 'limn_result': 'sol aqu',
        'eng_a': 'solid', 'eng_b': 'water', 'eng_result': 'ice'
    },
    {
        'limn_a': 'liq', 'limn_b': 'aqu', 'limn_result': 'liq aqu',
        'eng_a': 'liquid', 'eng_b': 'water', 'eng_result': 'water'
    },
    {
        'limn_a': 'gas', 'limn_b': 'aqu', 'limn_result': 'gas aqu',
        'eng_a': 'gas', 'eng_b': 'water', 'eng_result': 'steam'
    },
    {
        'limn_a': 'hot', 'limn_b': 'dry', 'limn_result': 'hot dry',
        'eng_a': 'hot', 'eng_b': 'dry', 'eng_result': 'desert'
    },
    {
        'limn_a': 'col', 'limn_b': 'wet', 'limn_result': 'col wet',
        'eng_a': 'cold', 'eng_b': 'wet', 'eng_result': 'rain'
    },
    {
        'limn_a': 'hot', 'limn_b': 'wet', 'limn_result': 'hot wet',
        'eng_a': 'hot', 'eng_b': 'wet', 'eng_result': 'humid'
    },
    {
        'limn_a': 'bri', 'limn_b': 'lux', 'limn_result': 'bri lux',
        'eng_a': 'bright', 'eng_b': 'light', 'eng_result': 'sunlight'
    },

    # Emotional
    {
        'limn_a': 'joy', 'limn_b': 'lov', 'limn_result': 'joy lov',
        'eng_a': 'joy', 'eng_b': 'love', 'eng_result': 'happiness'
    },
    {
        'limn_a': 'sad', 'limn_b': 'wan', 'limn_result': 'sad wan',
        'eng_a': 'sad', 'eng_b': 'want', 'eng_result': 'longing'
    },

    # Properties
    {
        'limn_a': 'big', 'limn_b': 'str', 'limn_result': 'big str',
        'eng_a': 'big', 'eng_b': 'strong', 'eng_result': 'powerful'
    },
    {
        'limn_a': 'old', 'limn_b': 'kno', 'limn_result': 'old kno',
        'eng_a': 'old', 'eng_b': 'knowing', 'eng_result': 'wisdom'
    },
]

def cosine_similarity(a, b):
    return 1 - cosine(a, b)

def test_compositionality(model):
    print("\n" + "="*70)
    print("ENGLISH BASELINE COMPARISON")
    print("="*70)
    print("\nCRITICAL TEST: Does English show same 0.88 compositionality as Limn?")
    print("If English << 0.88, Limn's advantage is VALIDATED\n")

    limn_scores = []
    english_scores = []

    for i, case in enumerate(TEST_CASES, 1):
        print(f"[{i}/{len(TEST_CASES)}]")

        # LIMN TEST
        limn_a = model.encode(case['limn_a'])
        limn_b = model.encode(case['limn_b'])
        limn_result = model.encode(case['limn_result'])

        limn_composed = limn_a + limn_b
        limn_composed = limn_composed / np.linalg.norm(limn_composed)

        limn_sim = cosine_similarity(limn_composed, limn_result)
        limn_scores.append(limn_sim)

        # ENGLISH TEST
        eng_a = model.encode(case['eng_a'])
        eng_b = model.encode(case['eng_b'])
        eng_result = model.encode(case['eng_result'])

        eng_composed = eng_a + eng_b
        eng_composed = eng_composed / np.linalg.norm(eng_composed)

        eng_sim = cosine_similarity(eng_composed, eng_result)
        english_scores.append(eng_sim)

        # Print comparison
        print(f"  Limn:    {case['limn_a']:4s} + {case['limn_b']:4s} → {case['limn_result']:8s} = {limn_sim:.4f}")
        print(f"  English: {case['eng_a']:8s} + {case['eng_b']:8s} → {case['eng_result']:10s} = {eng_sim:.4f}")

        delta = limn_sim - eng_sim
        if delta > 0.1:
            print(f"  → LIMN WINS by {delta:.4f} ✓")
        elif delta < -0.1:
            print(f"  → ENGLISH WINS by {-delta:.4f}")
        else:
            print(f"  → TIE (within 0.1)")
        print()

    # Summary
    limn_mean = np.mean(limn_scores)
    eng_mean = np.mean(english_scores)

    print("\n" + "="*70)
    print("FINAL RESULTS")
    print("="*70)
    print(f"\nLimn compositionality:    {limn_mean:.4f}")
    print(f"English compositionality: {eng_mean:.4f}")
    print(f"Difference:               {limn_mean - eng_mean:.4f}")

    print("\n" + "="*70)
    print("INTERPRETATION")
    print("="*70)

    if eng_mean < 0.5 and limn_mean > 0.8:
        print("\n✓✓✓ LIMN'S ADVANTAGE VALIDATED ✓✓✓")
        print(f"  Limn ({limn_mean:.2f}) >> English ({eng_mean:.2f})")
        print("  Limn is demonstrably more compositional for LLMs!")
        print("  This validates LLM-native language claim.")
    elif limn_mean > eng_mean + 0.2:
        print(f"\n✓ LIMN ADVANTAGE CONFIRMED")
        print(f"  Limn ({limn_mean:.2f}) > English ({eng_mean:.2f}) by {limn_mean - eng_mean:.2f}")
        print("  Significant compositional improvement")
    elif limn_mean > eng_mean:
        print(f"\n⚠ LIMN SLIGHTLY BETTER")
        print(f"  Limn ({limn_mean:.2f}) > English ({eng_mean:.2f}) by {limn_mean - eng_mean:.2f}")
        print("  Modest advantage")
    else:
        print(f"\n✗ NO ADVANTAGE")
        print(f"  English ({eng_mean:.2f}) >= Limn ({limn_mean:.2f})")
        print("  No compositional benefit demonstrated")

    return {
        'limn_mean': float(limn_mean),
        'english_mean': float(eng_mean),
        'limn_scores': [float(x) for x in limn_scores],
        'english_scores': [float(x) for x in english_scores]
    }

if __name__ == "__main__":
    print("Loading sentence-transformers model...")
    model = SentenceTransformer('all-MiniLM-L6-v2')
    print("Model loaded!\n")

    results = test_compositionality(model)

    # Save results
    import json
    with open('english-baseline-results.json', 'w') as f:
        json.dump(results, f, indent=2)

    print(f"\n✓ Results saved to english-baseline-results.json")
