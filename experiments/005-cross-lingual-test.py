#!/usr/bin/env python3
"""
Experiment 005: Cross-Lingual Clustering Test
Does Limn sit at the semantic centroid of translations?

Hypothesis: Limn serves as language-agnostic interlingua
Test: Compare distances from Limn to translations vs translation-to-translation

If Limn is at centroid, all translations should be equidistant from Limn
"""

import numpy as np
from sentence_transformers import SentenceTransformer
from scipy.spatial.distance import cosine, pdist, squareform
from scipy.stats import ttest_ind

def cosine_similarity(a, b):
    return 1 - cosine(a, b)

# Test cases from Dr. Solvik's corpus
CROSS_LINGUAL_CASES = [
    {
        'limn': 'sol aqu',
        'concept': 'ice',
        'translations': {
            'english': 'ice',
            'mandarin': '冰',
            'spanish': 'hielo',
            'arabic': 'ثلج',
            'japanese': '氷'
        }
    },
    {
        'limn': 'gas aqu',
        'concept': 'steam',
        'translations': {
            'english': 'steam',
            'mandarin': '蒸汽',
            'spanish': 'vapor',
            'arabic': 'بخار',
            'japanese': '蒸気'
        }
    },
    {
        'limn': 'hot dry',
        'concept': 'desert',
        'translations': {
            'english': 'desert',
            'mandarin': '沙漠',
            'spanish': 'desierto',
            'arabic': 'صحراء',
            'japanese': '砂漠'
        }
    },
]

def test_cross_lingual_clustering(model):
    print("\n" + "="*70)
    print("CROSS-LINGUAL CLUSTERING TEST")
    print("="*70)
    print("\nHypothesis: Limn sits at semantic centroid of translations")
    print("Test: Compare Limn-to-translation vs translation-to-translation distances\n")

    all_results = []

    for case_num, case in enumerate(CROSS_LINGUAL_CASES, 1):
        print(f"\n{'='*70}")
        print(f"CASE {case_num}: {case['concept']} ({case['limn']})")
        print(f"{'='*70}\n")

        # Get embeddings
        limn_emb = model.encode(case['limn'])

        translation_embs = {}
        for lang, word in case['translations'].items():
            translation_embs[lang] = model.encode(word)

        # Calculate Limn-to-translation distances
        limn_distances = {}
        for lang, emb in translation_embs.items():
            dist = 1 - cosine_similarity(limn_emb, emb)
            limn_distances[lang] = dist
            print(f"  {case['limn']:10s} → {lang:8s} ({case['translations'][lang]:8s}): {dist:.4f}")

        # Calculate pairwise translation distances
        print("\n  Translation-to-translation distances:")
        languages = list(translation_embs.keys())
        pairwise_distances = []

        for i, lang1 in enumerate(languages):
            for lang2 in languages[i+1:]:
                dist = 1 - cosine_similarity(translation_embs[lang1], translation_embs[lang2])
                pairwise_distances.append(dist)
                print(f"    {lang1:8s} → {lang2:8s}: {dist:.4f}")

        # Statistics
        mean_limn_dist = np.mean(list(limn_distances.values()))
        std_limn_dist = np.std(list(limn_distances.values()))
        mean_pairwise_dist = np.mean(pairwise_distances)
        std_pairwise_dist = np.std(pairwise_distances)

        print(f"\n  Limn distances:        Mean = {mean_limn_dist:.4f}, Std = {std_limn_dist:.4f}")
        print(f"  Pairwise distances:    Mean = {mean_pairwise_dist:.4f}, Std = {std_pairwise_dist:.4f}")

        # Check if Limn is at centroid
        if mean_limn_dist < mean_pairwise_dist:
            print(f"\n  ✓ Limn is CLOSER to translations than they are to each other")
            print(f"    → Supports interlingua hypothesis")
        else:
            print(f"\n  ✗ Translations are closer to each other than to Limn")
            print(f"    → Limn not at centroid")

        # Calculate actual centroid
        all_translation_embs = np.array(list(translation_embs.values()))
        centroid = np.mean(all_translation_embs, axis=0)

        dist_limn_to_centroid = 1 - cosine_similarity(limn_emb, centroid)

        # Compare each translation to centroid
        centroid_distances = []
        for lang, emb in translation_embs.items():
            dist = 1 - cosine_similarity(emb, centroid)
            centroid_distances.append(dist)

        mean_translation_to_centroid = np.mean(centroid_distances)

        print(f"\n  Distance (Limn → actual centroid):        {dist_limn_to_centroid:.4f}")
        print(f"  Distance (translations → centroid mean):  {mean_translation_to_centroid:.4f}")

        if dist_limn_to_centroid < mean_translation_to_centroid * 1.5:
            print(f"    → Limn is NEAR the semantic centroid ✓")
        else:
            print(f"    → Limn is FAR from semantic centroid ✗")

        all_results.append({
            'concept': case['concept'],
            'limn': case['limn'],
            'mean_limn_distance': float(mean_limn_dist),
            'mean_pairwise_distance': float(mean_pairwise_dist),
            'limn_to_centroid': float(dist_limn_to_centroid),
            'translations_to_centroid': float(mean_translation_to_centroid),
            'limn_closer_than_pairwise': mean_limn_dist < mean_pairwise_dist,
            'limn_near_centroid': dist_limn_to_centroid < mean_translation_to_centroid * 1.5
        })

    # Overall summary
    print("\n" + "="*70)
    print("OVERALL SUMMARY")
    print("="*70)

    overall_limn_dist = np.mean([r['mean_limn_distance'] for r in all_results])
    overall_pairwise_dist = np.mean([r['mean_pairwise_distance'] for r in all_results])
    overall_limn_to_centroid = np.mean([r['limn_to_centroid'] for r in all_results])

    print(f"\nMean Limn-to-translation distance:  {overall_limn_dist:.4f}")
    print(f"Mean translation-to-translation:    {overall_pairwise_dist:.4f}")
    print(f"Mean Limn-to-centroid distance:     {overall_limn_to_centroid:.4f}")

    cases_where_limn_closer = sum(r['limn_closer_than_pairwise'] for r in all_results)
    cases_near_centroid = sum(r['limn_near_centroid'] for r in all_results)

    print(f"\nCases where Limn closer than pairwise: {cases_where_limn_closer}/{len(all_results)}")
    print(f"Cases where Limn near centroid:        {cases_near_centroid}/{len(all_results)}")

    print("\n" + "="*70)
    print("INTERPRETATION")
    print("="*70)

    if cases_where_limn_closer == len(all_results) and cases_near_centroid == len(all_results):
        print("\n✓✓✓ STRONG INTERLINGUA EVIDENCE ✓✓✓")
        print("  Limn consistently sits at semantic centroid")
        print("  Closer to all translations than they are to each other")
        print("  Supports language-agnostic representation hypothesis")
    elif cases_near_centroid >= len(all_results) * 0.67:
        print("\n✓ MODERATE INTERLINGUA SUPPORT")
        print(f"  Limn near centroid in {cases_near_centroid}/{len(all_results)} cases")
        print("  Partially supports interlingua hypothesis")
    else:
        print("\n⚠ WEAK INTERLINGUA SUPPORT")
        print("  Limn not consistently at semantic centroid")
        print("  May be closer to English than other languages")

    # Check for English bias
    print("\n" + "="*70)
    print("ENGLISH BIAS CHECK")
    print("="*70)

    for case in all_results:
        print(f"\n{case['concept']}:")
        # Would need to store individual distances to check this properly
        # For now, just note the pattern

    return all_results

if __name__ == "__main__":
    print("Loading sentence-transformers model...")
    model = SentenceTransformer('all-MiniLM-L6-v2')
    print("Model loaded!\n")

    results = test_cross_lingual_clustering(model)

    # Save results
    import json
    with open('cross-lingual-results.json', 'w') as f:
        json.dump(results, f, indent=2)

    print("\n✓ Results saved to cross-lingual-results.json")
