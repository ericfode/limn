#!/usr/bin/env python3
"""
Experiment 009: Empirical Validation of Greek Philosophical Vocabulary

CRITICAL: Validate claims from Experiment 007-retest with actual LLM embeddings.

Tests:
1. Embedding similarity: Limn terms vs English concepts
2. Compositionality: embed(A) + embed(B) ≈ embed(A B)
3. Specific vs generic: eud vs gud lif
4. Semantic cluster density

Model: sentence-transformers/all-MiniLM-L6-v2 (same as Experiment 005)
"""

import numpy as np
from sentence_transformers import SentenceTransformer
from scipy.spatial.distance import cosine
from scipy.stats import ttest_ind
import json

def cosine_similarity(a, b):
    return 1 - cosine(a, b)

# Test cases: Limn term vs English concept vs Generic descriptor
GREEK_VOCAB_TESTS = [
    {
        'limn': 'eud',
        'concept': 'eudaimonia',
        'english_full': 'eudaimonia flourishing happiness living well',
        'generic': 'gud lif',
        'generic_english': 'good life',
        'description': 'Eudaimonia - flourishing, living well'
    },
    {
        'limn': 'aret',
        'concept': 'arete',
        'english_full': 'arete excellence virtue moral excellence',
        'generic': 'exc',
        'generic_english': 'excellence',
        'description': 'Arete - excellence, virtue'
    },
    {
        'limn': 'phr',
        'concept': 'phronesis',
        'english_full': 'phronesis practical wisdom prudence',
        'generic': 'pra wis',
        'generic_english': 'practical wisdom',
        'description': 'Phronesis - practical wisdom'
    },
    {
        'limn': 'telo',
        'concept': 'telos',
        'english_full': 'telos purpose end final cause goal',
        'generic': 'end',
        'generic_english': 'end',
        'description': 'Telos - purpose, final cause'
    },
    {
        'limn': 'nou',
        'concept': 'nous',
        'english_full': 'nous intellect intuitive mind understanding',
        'generic': 'min',
        'generic_english': 'mind',
        'description': 'Nous - intuitive intellect'
    },
    {
        'limn': 'lgs',
        'concept': 'logos',
        'english_full': 'logos reason rational principle divine reason',
        'generic': 'rea',
        'generic_english': 'reason',
        'description': 'Logos - rational principle'
    },
    {
        'limn': 'eid',
        'concept': 'eidos',
        'english_full': 'eidos form essence platonic form',
        'generic': 'for',
        'generic_english': 'form',
        'description': 'Eidos - Platonic Form'
    }
]

# Compositionality tests: philosophical phrases
COMPOSITION_TESTS = [
    {
        'limn_phrase': 'eud aret',
        'limn_a': 'eud',
        'limn_b': 'aret',
        'english_phrase': 'eudaimonia through arete',
        'description': 'Flourishing through virtue'
    },
    {
        'limn_phrase': 'nou lgs',
        'limn_a': 'nou',
        'limn_b': 'lgs',
        'english_phrase': 'nous grasps through logos',
        'description': 'Intellect through reason'
    },
    {
        'limn_phrase': 'liv in way of aret gui by phr to rea eud',
        'limn_a': 'aret',
        'limn_b': 'phr',
        'english_phrase': 'living virtuously with practical wisdom to reach flourishing',
        'description': 'Complete Aristotelian ethics'
    },
    {
        'limn_phrase': 'telo eud',
        'limn_a': 'telo',
        'limn_b': 'eud',
        'english_phrase': 'purpose is eudaimonia',
        'description': 'Teleological ethics'
    }
]

def test_concept_similarity(model):
    print("\n" + "="*70)
    print("TEST 1: CONCEPT SIMILARITY")
    print("="*70)
    print("\nHypothesis: Limn terms preserve semantic similarity to Greek concepts\n")

    results = []

    for test in GREEK_VOCAB_TESTS:
        # Get embeddings
        limn_emb = model.encode(test['limn'])
        concept_emb = model.encode(test['concept'])
        full_emb = model.encode(test['english_full'])
        generic_limn_emb = model.encode(test['generic'])
        generic_eng_emb = model.encode(test['generic_english'])

        # Calculate similarities
        limn_to_concept = cosine_similarity(limn_emb, concept_emb)
        limn_to_full = cosine_similarity(limn_emb, full_emb)
        generic_to_concept = cosine_similarity(generic_limn_emb, concept_emb)

        improvement = limn_to_concept - generic_to_concept

        results.append({
            'term': test['limn'],
            'concept': test['concept'],
            'limn_to_concept': float(limn_to_concept),
            'generic_to_concept': float(generic_to_concept),
            'improvement': float(improvement),
            'description': test['description']
        })

        print(f"{test['description']}")
        print(f"  Limn '{test['limn']}' → '{test['concept']}': {limn_to_concept:.4f}")
        print(f"  Generic '{test['generic']}' → '{test['concept']}': {generic_to_concept:.4f}")
        print(f"  Improvement: {improvement:+.4f}", end="")

        if improvement > 0:
            print(" ✓ Limn better!")
        else:
            print(" ✗ Generic better")
        print()

    # Summary statistics
    mean_limn = np.mean([r['limn_to_concept'] for r in results])
    mean_generic = np.mean([r['generic_to_concept'] for r in results])
    mean_improvement = np.mean([r['improvement'] for r in results])

    print("\n" + "-"*70)
    print("SUMMARY:")
    print(f"Mean similarity (Limn → concept):    {mean_limn:.4f}")
    print(f"Mean similarity (generic → concept): {mean_generic:.4f}")
    print(f"Mean improvement:                    {mean_improvement:+.4f}")

    better_count = sum(1 for r in results if r['improvement'] > 0)
    print(f"\nLimn better than generic: {better_count}/{len(results)}")

    if mean_improvement > 0.05:
        print("\n✓ HYPOTHESIS CONFIRMED: Limn terms preserve semantic similarity")
    elif mean_improvement > 0:
        print("\n⚠ WEAK SUPPORT: Limn slightly better")
    else:
        print("\n✗ HYPOTHESIS REJECTED: Generic terms perform equally or better")

    return results

def test_compositionality(model):
    print("\n" + "="*70)
    print("TEST 2: COMPOSITIONALITY")
    print("="*70)
    print("\nHypothesis: Greek vocab maintains 88% compositional baseline\n")

    results = []

    for test in COMPOSITION_TESTS:
        # Get embeddings
        phrase_emb = model.encode(test['limn_phrase'])

        # For simple two-word compositions
        if 'limn_b' in test:
            emb_a = model.encode(test['limn_a'])
            emb_b = model.encode(test['limn_b'])

            # Vector addition composition
            composed = emb_a + emb_b
            composed = composed / np.linalg.norm(composed)

            similarity = cosine_similarity(composed, phrase_emb)

            results.append({
                'phrase': test['limn_phrase'],
                'description': test['description'],
                'compositional_similarity': float(similarity)
            })

            print(f"{test['description']}")
            print(f"  '{test['limn_a']}' + '{test['limn_b']}' → '{test['limn_phrase']}'")
            print(f"  Compositional similarity: {similarity:.4f}")

            if similarity > 0.85:
                print("  → ✓ High compositionality")
            elif similarity > 0.70:
                print("  → ⚠ Moderate compositionality")
            else:
                print("  → ✗ Low compositionality")
            print()

    # Summary
    mean_comp = np.mean([r['compositional_similarity'] for r in results])

    print("\n" + "-"*70)
    print("SUMMARY:")
    print(f"Mean compositional similarity: {mean_comp:.4f}")
    print(f"Baseline from Experiment 005:  0.8801")
    print(f"Difference:                    {mean_comp - 0.8801:+.4f}")

    if mean_comp > 0.85:
        print("\n✓ HYPOTHESIS CONFIRMED: Greek vocab maintains high compositionality")
    elif mean_comp > 0.75:
        print("\n⚠ MODERATE: Greek vocab has acceptable compositionality")
    else:
        print("\n✗ HYPOTHESIS REJECTED: Greek vocab shows poor compositionality")

    return results

def test_semantic_cluster(model):
    print("\n" + "="*70)
    print("TEST 3: SEMANTIC CLUSTER DENSITY")
    print("="*70)
    print("\nHypothesis: Greek terms form a dense semantic cluster\n")

    # Greek cluster
    greek_terms = ['eud', 'aret', 'phr', 'telo', 'nou', 'lgs', 'eid']
    greek_concepts = ['eudaimonia', 'arete', 'phronesis', 'telos', 'nous', 'logos', 'eidos']

    # Eastern cluster for comparison (from Experiment 006)
    eastern_terms = ['kar', 'nir', 'dha', 'ren', 'tao', 'wuw']
    eastern_concepts = ['karma', 'nirvana', 'dharma', 'ren', 'tao', 'wu wei']

    # Get embeddings
    greek_embs = [model.encode(term) for term in greek_terms]
    eastern_embs = [model.encode(term) for term in eastern_terms]

    # Calculate pairwise similarities within clusters
    def cluster_density(embeddings):
        n = len(embeddings)
        similarities = []
        for i in range(n):
            for j in range(i+1, n):
                sim = cosine_similarity(embeddings[i], embeddings[j])
                similarities.append(sim)
        return similarities

    greek_sims = cluster_density(greek_embs)
    eastern_sims = cluster_density(eastern_embs)

    greek_mean = np.mean(greek_sims)
    greek_std = np.std(greek_sims)
    eastern_mean = np.mean(eastern_sims)
    eastern_std = np.std(eastern_sims)

    print(f"Greek cluster density:")
    print(f"  Mean pairwise similarity: {greek_mean:.4f} (std: {greek_std:.4f})")
    print(f"  Range: {min(greek_sims):.4f} - {max(greek_sims):.4f}")
    print(f"  Pairs: {len(greek_sims)}")

    print(f"\nEastern cluster density (comparison):")
    print(f"  Mean pairwise similarity: {eastern_mean:.4f} (std: {eastern_std:.4f})")
    print(f"  Range: {min(eastern_sims):.4f} - {max(eastern_sims):.4f}")
    print(f"  Pairs: {len(eastern_sims)}")

    print(f"\nDifference: {greek_mean - eastern_mean:+.4f}")

    if greek_mean > 0.50:
        print("\n✓ Greek cluster shows high semantic density")
    elif greek_mean > 0.35:
        print("\n⚠ Greek cluster shows moderate semantic density")
    else:
        print("\n✗ Greek cluster shows weak semantic density")

    return {
        'greek_mean': float(greek_mean),
        'greek_std': float(greek_std),
        'eastern_mean': float(eastern_mean),
        'eastern_std': float(eastern_std),
        'greek_sims': [float(s) for s in greek_sims],
        'eastern_sims': [float(s) for s in eastern_sims]
    }

def test_philosophical_phrases(model):
    print("\n" + "="*70)
    print("TEST 4: COMPLETE PHILOSOPHICAL PHRASES")
    print("="*70)
    print("\nTest: Do complex Limn philosophical phrases match English equivalents?\n")

    test_cases = [
        {
            'limn': 'eud is telo of hum lif',
            'english': 'eudaimonia is the purpose of human life',
            'description': 'Aristotelian teleology'
        },
        {
            'limn': 'liv in way of aret gui by phr',
            'english': 'living virtuously guided by practical wisdom',
            'description': 'Aristotelian ethics'
        },
        {
            'limn': 'nou gra fir pri thr lgs',
            'english': 'intellect grasps first principles through reason',
            'description': 'Greek epistemology'
        },
        {
            'limn': 'eid is ete per for bey phy wor',
            'english': 'forms are eternal perfect essences beyond physical world',
            'description': 'Platonic metaphysics'
        },
        {
            'limn': 'liv by nat whi is liv by lgs',
            'english': 'live according to nature which is to live by logos',
            'description': 'Stoic ethics'
        }
    ]

    results = []

    for test in test_cases:
        limn_emb = model.encode(test['limn'])
        eng_emb = model.encode(test['english'])

        similarity = cosine_similarity(limn_emb, eng_emb)

        results.append({
            'limn': test['limn'],
            'english': test['english'],
            'similarity': float(similarity),
            'description': test['description']
        })

        print(f"{test['description']}")
        print(f"  Limn: '{test['limn']}'")
        print(f"  English: '{test['english']}'")
        print(f"  Similarity: {similarity:.4f}")

        if similarity > 0.85:
            print("  → ✓ Excellent match")
        elif similarity > 0.75:
            print("  → ⚠ Good match")
        else:
            print("  → ✗ Weak match")
        print()

    mean_sim = np.mean([r['similarity'] for r in results])

    print("\n" + "-"*70)
    print(f"Mean phrase similarity: {mean_sim:.4f}")

    if mean_sim > 0.80:
        print("\n✓ Complex philosophical phrases match English well")
    elif mean_sim > 0.70:
        print("\n⚠ Moderate match for philosophical phrases")
    else:
        print("\n✗ Weak match for philosophical phrases")

    return results

if __name__ == "__main__":
    print("Loading sentence-transformers model...")
    model = SentenceTransformer('all-MiniLM-L6-v2')
    print("Model loaded!\n")

    # Run all tests
    concept_results = test_concept_similarity(model)
    comp_results = test_compositionality(model)
    cluster_results = test_semantic_cluster(model)
    phrase_results = test_philosophical_phrases(model)

    # Overall summary
    print("\n" + "="*70)
    print("OVERALL VALIDATION SUMMARY")
    print("="*70)

    concept_mean = np.mean([r['limn_to_concept'] for r in concept_results])
    concept_improvement = np.mean([r['improvement'] for r in concept_results])
    comp_mean = np.mean([r['compositional_similarity'] for r in comp_results])
    phrase_mean = np.mean([r['similarity'] for r in phrase_results])

    print(f"\n1. Concept Similarity:     {concept_mean:.4f} (improvement: {concept_improvement:+.4f})")
    print(f"2. Compositionality:       {comp_mean:.4f} (baseline: 0.8801)")
    print(f"3. Cluster Density:        {cluster_results['greek_mean']:.4f}")
    print(f"4. Philosophical Phrases:  {phrase_mean:.4f}")

    print("\n" + "="*70)
    print("VALIDATION STATUS")
    print("="*70)

    validations = []

    if concept_mean > 0.70:
        print("\n✓ Greek terms preserve semantic similarity to concepts")
        validations.append(True)
    else:
        print("\n✗ Greek terms do not preserve semantic similarity")
        validations.append(False)

    if comp_mean > 0.85:
        print("✓ Greek vocab maintains high compositionality")
        validations.append(True)
    elif comp_mean > 0.75:
        print("⚠ Greek vocab has acceptable compositionality")
        validations.append(True)
    else:
        print("✗ Greek vocab shows poor compositionality")
        validations.append(False)

    if cluster_results['greek_mean'] > 0.45:
        print("✓ Greek terms form a dense semantic cluster")
        validations.append(True)
    else:
        print("✗ Greek terms do not form a dense cluster")
        validations.append(False)

    if phrase_mean > 0.75:
        print("✓ Philosophical phrases match English equivalents")
        validations.append(True)
    else:
        print("✗ Philosophical phrases do not match well")
        validations.append(False)

    if all(validations):
        print("\n" + "="*70)
        print("✓✓✓ ALL VALIDATIONS PASSED ✓✓✓")
        print("="*70)
        print("\nGreek philosophical vocabulary is EMPIRICALLY VALIDATED")
        print("Experiment 007-retest claims are SUPPORTED by evidence")
    elif sum(validations) >= 3:
        print("\n" + "="*70)
        print("✓ MOST VALIDATIONS PASSED")
        print("="*70)
        print("\nGreek vocabulary shows strong performance with minor gaps")
    else:
        print("\n" + "="*70)
        print("⚠ VALIDATION CONCERNS")
        print("="*70)
        print("\nGreek vocabulary needs further investigation")

    # Save results
    all_results = {
        'concept_similarity': concept_results,
        'compositionality': comp_results,
        'semantic_cluster': cluster_results,
        'philosophical_phrases': phrase_results,
        'summary': {
            'concept_mean': float(concept_mean),
            'concept_improvement': float(concept_improvement),
            'compositionality_mean': float(comp_mean),
            'cluster_density': float(cluster_results['greek_mean']),
            'phrase_mean': float(phrase_mean),
            'all_passed': all(validations),
            'validations_passed': sum(validations),
            'validations_total': len(validations)
        }
    }

    with open('009-validation-results.json', 'w') as f:
        json.dump(all_results, f, indent=2)

    print("\n✓ Results saved to 009-validation-results.json")
