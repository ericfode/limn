#!/usr/bin/env python3
"""
Experiment 010: Validate Eastern Philosophy Vocabulary with Both Embedders

Tests Buddhist/Daoist/Confucian terms using:
1. Generic sentence-transformers/all-MiniLM-L6-v2 (baseline)
2. Limn-native fine-tuned embedder (from Experiment 009)

Compares performance to determine if:
- Eastern vocab had same compositional failure as Greek vocab
- Limn-native embedder generalizes across vocabulary domains
"""

import numpy as np
from sentence_transformers import SentenceTransformer
from sklearn.metrics.pairwise import cosine_similarity
import json
from pathlib import Path

def cosine_sim(a, b):
    """Compute cosine similarity between two vectors."""
    return cosine_similarity(a.reshape(1, -1), b.reshape(1, -1))[0][0]

def test_concept_similarity(model, model_name):
    """Test if Eastern Limn terms preserve semantic similarity to their concepts."""

    tests = [
        # Buddhist terms
        {"limn": "kar", "concept": "karma", "description": "Karma - action and consequence"},
        {"limn": "dha", "concept": "dharma", "description": "Dharma - cosmic law, truth"},
        {"limn": "sam", "concept": "samsara", "description": "Samsara - cycle of rebirth"},
        {"limn": "nir", "concept": "nirvana", "description": "Nirvana - liberation from suffering"},
        {"limn": "bod", "concept": "bodhi", "description": "Bodhi - awakening, enlightenment"},
        {"limn": "san", "concept": "sangha", "description": "Sangha - spiritual community"},
        {"limn": "duk", "concept": "dukkha", "description": "Dukkha - suffering, unsatisfactoriness"},

        # Daoist terms
        {"limn": "tao", "concept": "tao", "description": "Tao - the Way, cosmic principle"},
        {"limn": "wuw", "concept": "wu wei", "description": "Wu wei - effortless action"},
        {"limn": "zir", "concept": "ziran", "description": "Ziran - spontaneity, naturalness"},
        {"limn": "qig", "concept": "qi", "description": "Qi - vital energy, life force"},

        # Confucian terms
        {"limn": "ren", "concept": "ren", "description": "Ren - benevolence, humaneness"},
        {"limn": "li", "concept": "li", "description": "Li - ritual propriety, etiquette"},
        {"limn": "xin", "concept": "xin", "description": "Xin - trustworthiness, faithfulness"},
        {"limn": "jun", "concept": "junzi", "description": "Junzi - exemplary person, gentleman"},
    ]

    results = []

    for test in tests:
        limn_emb = model.encode(test['limn'])
        concept_emb = model.encode(test['concept'])
        generic_emb = model.encode("xyz")

        limn_to_concept = cosine_sim(limn_emb, concept_emb)
        generic_to_concept = cosine_sim(generic_emb, concept_emb)
        improvement = limn_to_concept - generic_to_concept

        results.append({
            "term": test['limn'],
            "concept": test['concept'],
            "limn_to_concept": float(limn_to_concept),
            "generic_to_concept": float(generic_to_concept),
            "improvement": float(improvement),
            "description": test['description']
        })

    mean_similarity = np.mean([r['limn_to_concept'] for r in results])
    mean_improvement = np.mean([r['improvement'] for r in results])

    print(f"\n{model_name} - Concept Similarity Test")
    print(f"{'='*70}")
    for r in results:
        status = "✓" if r['improvement'] > 0 else "✗"
        print(f"{status} {r['term']:6} → {r['concept']:12} sim={r['limn_to_concept']:.3f} (Δ{r['improvement']:+.3f})")
    print(f"\nMean similarity: {mean_similarity:.4f}")
    print(f"Mean improvement over generic: {mean_improvement:+.4f}")

    return results, mean_similarity, mean_improvement

def test_compositionality(model, model_name):
    """Test if Eastern vocab maintains compositional semantics."""

    tests = [
        {"phrase": "kar dha", "description": "Karma and dharma together"},
        {"phrase": "tao wuw", "description": "The Way through effortless action"},
        {"phrase": "ren li", "description": "Benevolence through ritual"},
        {"phrase": "sam nir", "description": "From samsara to nirvana"},
        {"phrase": "bod dha", "description": "Enlightenment and truth"},
        {"phrase": "zir qig", "description": "Spontaneous vital energy"},
    ]

    results = []

    for test in tests:
        parts = test['phrase'].split()
        phrase_emb = model.encode(test['phrase'])

        # Compute compositional embedding (mean of parts)
        part_embs = [model.encode(p) for p in parts]
        composed = np.mean(part_embs, axis=0)
        composed = composed / np.linalg.norm(composed)

        similarity = cosine_sim(composed, phrase_emb)

        results.append({
            "phrase": test['phrase'],
            "description": test['description'],
            "compositional_similarity": float(similarity)
        })

    mean_comp = np.mean([r['compositional_similarity'] for r in results])

    print(f"\n{model_name} - Compositionality Test")
    print(f"{'='*70}")
    for r in results:
        status = "✓" if r['compositional_similarity'] > 0.75 else "✗"
        print(f"{status} {r['phrase']:20} comp={r['compositional_similarity']:.3f} - {r['description']}")
    print(f"\nMean compositionality: {mean_comp:.4f}")

    return results, mean_comp

def test_philosophical_phrases(model, model_name):
    """Test complex Eastern philosophy phrases in Limn vs English."""

    tests = [
        {
            "limn": "kar is law of cau and eff in all act",
            "english": "karma is the law of cause and effect in all actions",
            "description": "Buddhist causation"
        },
        {
            "limn": "to rea nir one mus fol eig pat",
            "english": "to reach nirvana one must follow the eightfold path",
            "description": "Buddhist soteriology"
        },
        {
            "limn": "tao tha can be spo is not ete tao",
            "english": "the tao that can be spoken is not the eternal tao",
            "description": "Daoist ineffability"
        },
        {
            "limn": "wuw is act in har wit nat wit for",
            "english": "wu wei is acting in harmony with nature without forcing",
            "description": "Daoist ethics"
        },
        {
            "limn": "ren is ess of bei hum, lov for oth",
            "english": "ren is the essence of being human, love for others",
            "description": "Confucian virtue"
        },
        {
            "limn": "jun cul sel thr li and stu of cla",
            "english": "the junzi cultivates self through ritual and study of classics",
            "description": "Confucian self-cultivation"
        },
        {
            "limn": "all suf com fro att to imp thi",
            "english": "all suffering comes from attachment to impermanent things",
            "description": "Buddhist diagnosis"
        },
        {
            "limn": "yin and yan are opp for tha cre bal",
            "english": "yin and yang are opposing forces that create balance",
            "description": "Daoist cosmology"
        },
    ]

    results = []

    for test in tests:
        limn_emb = model.encode(test['limn'])
        eng_emb = model.encode(test['english'])

        similarity = cosine_sim(limn_emb, eng_emb)

        results.append({
            "limn": test['limn'],
            "english": test['english'],
            "similarity": float(similarity),
            "description": test['description']
        })

    mean_sim = np.mean([r['similarity'] for r in results])
    pass_count = sum(1 for r in results if r['similarity'] > 0.75)

    print(f"\n{model_name} - Philosophical Phrases Test")
    print(f"{'='*70}")
    for r in results:
        status = "✓" if r['similarity'] > 0.75 else "✗"
        print(f"{status} sim={r['similarity']:.3f} - {r['description']}")
        print(f"  Limn: {r['limn']}")
        print(f"  Eng:  {r['english']}")
        print()

    print(f"Mean similarity: {mean_sim:.4f}")
    print(f"Passing threshold (>0.75): {pass_count}/{len(results)} ({100*pass_count/len(results):.1f}%)")

    return results, mean_sim, pass_count, len(results)

def test_semantic_cluster(model, model_name):
    """Test if Eastern terms cluster more tightly than random Western terms."""

    eastern_terms = [
        "kar", "dha", "sam", "nir", "bod", "san", "duk",  # Buddhist
        "tao", "wuw", "zir", "qig",  # Daoist
        "ren", "li", "xin", "jun"  # Confucian
    ]

    western_terms = [
        "cat", "dog", "car", "house", "tree", "book", "phone",
        "table", "chair", "door", "window", "road", "water",
        "fire", "stone", "metal", "wood"
    ]

    eastern_embs = model.encode(eastern_terms)
    western_embs = model.encode(western_terms)

    # Compute pairwise similarities within each cluster
    eastern_sims = []
    for i in range(len(eastern_embs)):
        for j in range(i+1, len(eastern_embs)):
            sim = cosine_sim(eastern_embs[i], eastern_embs[j])
            eastern_sims.append(sim)

    western_sims = []
    for i in range(len(western_embs)):
        for j in range(i+1, len(western_embs)):
            sim = cosine_sim(western_embs[i], western_embs[j])
            western_sims.append(sim)

    eastern_mean = np.mean(eastern_sims)
    eastern_std = np.std(eastern_sims)
    western_mean = np.mean(western_sims)
    western_std = np.std(western_sims)

    print(f"\n{model_name} - Semantic Cluster Test")
    print(f"{'='*70}")
    print(f"Eastern philosophy terms:")
    print(f"  Mean pairwise similarity: {eastern_mean:.4f} ± {eastern_std:.4f}")
    print(f"  ({len(eastern_sims)} pairs)")
    print(f"\nRandom Western terms:")
    print(f"  Mean pairwise similarity: {western_mean:.4f} ± {western_std:.4f}")
    print(f"  ({len(western_sims)} pairs)")

    if eastern_mean > western_mean:
        print(f"\n✓ Eastern terms cluster more tightly (+{eastern_mean - western_mean:.4f})")
    else:
        print(f"\n✗ Eastern terms less cohesive ({eastern_mean - western_mean:.4f})")

    return {
        "eastern_mean": float(eastern_mean),
        "eastern_std": float(eastern_std),
        "western_mean": float(western_mean),
        "western_std": float(western_std),
        "eastern_sims": [float(s) for s in eastern_sims],
        "western_sims": [float(s) for s in western_sims]
    }

def run_validation(model_path, model_name):
    """Run all validation tests on a model."""

    print(f"\n{'='*70}")
    print(f"TESTING: {model_name}")
    print(f"Model: {model_path}")
    print(f"{'='*70}")

    model = SentenceTransformer(model_path)

    # Run all tests
    concept_results, concept_mean, concept_improvement = test_concept_similarity(model, model_name)
    comp_results, comp_mean = test_compositionality(model, model_name)
    phrase_results, phrase_mean, phrase_pass, phrase_total = test_philosophical_phrases(model, model_name)
    cluster_results = test_semantic_cluster(model, model_name)

    # Determine pass/fail for each validation
    validations = {
        "concept_similarity": concept_improvement > 0.20,  # Significant improvement
        "compositionality": comp_mean > 0.75,  # Good composition
        "phrase_similarity": phrase_mean > 0.75,  # Good phrase alignment
        "cluster_density": cluster_results['eastern_mean'] > cluster_results['western_mean']
    }

    validations_passed = sum(validations.values())
    validations_total = len(validations)

    summary = {
        "model": model_name,
        "concept_mean": concept_mean,
        "concept_improvement": concept_improvement,
        "compositionality_mean": comp_mean,
        "phrase_mean": phrase_mean,
        "phrase_pass_rate": phrase_pass / phrase_total,
        "cluster_density": cluster_results['eastern_mean'],
        "all_passed": all(validations.values()),
        "validations_passed": validations_passed,
        "validations_total": validations_total
    }

    print(f"\n{model_name} - SUMMARY")
    print(f"{'='*70}")
    print(f"Concept similarity:     {concept_mean:.4f} (improvement: {concept_improvement:+.4f})")
    print(f"Compositionality:       {comp_mean:.4f}")
    print(f"Phrase similarity:      {phrase_mean:.4f}")
    print(f"Phrase pass rate:       {phrase_pass}/{phrase_total} ({100*phrase_pass/phrase_total:.1f}%)")
    print(f"Cluster density:        {cluster_results['eastern_mean']:.4f}")
    print(f"\nValidations: {validations_passed}/{validations_total} passed")

    if all(validations.values()):
        print("✓ ALL VALIDATIONS PASSED")
    else:
        print("✗ Some validations failed:")
        for name, passed in validations.items():
            if not passed:
                print(f"  ✗ {name}")

    return {
        "concept_similarity": concept_results,
        "compositionality": comp_results,
        "philosophical_phrases": phrase_results,
        "semantic_cluster": cluster_results,
        "summary": summary
    }

def main():
    """Compare generic and Limn-native embedders on Eastern vocabulary."""

    # Find the Limn-native embedder (created by Experiment 009)
    limn_embedder_path = Path(__file__).parent / "embeddings" / "limn-embedder"

    if not limn_embedder_path.exists():
        # Check in parent experiments directory
        limn_embedder_path = Path(__file__).parent.parent / "experiments" / "embeddings" / "limn-embedder"

    # Test with both models
    print("\n" + "="*70)
    print("EXPERIMENT 010: EASTERN VOCABULARY VALIDATION")
    print("="*70)
    print("\nComparing:")
    print("1. Generic sentence-transformers (baseline)")
    print("2. Limn-native fine-tuned embedder")
    print()

    # Test generic model
    generic_results = run_validation(
        'sentence-transformers/all-MiniLM-L6-v2',
        "GENERIC EMBEDDER"
    )

    # Test Limn-native model
    if limn_embedder_path.exists():
        limn_results = run_validation(
            str(limn_embedder_path),
            "LIMN-NATIVE EMBEDDER"
        )
    else:
        print(f"\n⚠️  Limn-native embedder not found at {limn_embedder_path}")
        print("Skipping Limn-native tests. Run from experiments/ directory or train embedder first.")
        limn_results = None

    # Comparison
    if limn_results:
        print(f"\n{'='*70}")
        print("COMPARISON: Generic vs Limn-Native on Eastern Vocabulary")
        print(f"{'='*70}")

        print(f"\n{'Metric':<25} {'Generic':>15} {'Limn-Native':>15} {'Improvement':>15}")
        print("-" * 70)

        metrics = [
            ("Concept similarity", "concept_mean"),
            ("Compositionality", "compositionality_mean"),
            ("Phrase similarity", "phrase_mean"),
            ("Phrase pass rate", "phrase_pass_rate"),
            ("Cluster density", "cluster_density"),
        ]

        for label, key in metrics:
            gen_val = generic_results['summary'][key]
            limn_val = limn_results['summary'][key]
            improvement = limn_val - gen_val

            print(f"{label:<25} {gen_val:>15.4f} {limn_val:>15.4f} {improvement:>+15.4f}")

        print("\n" + "="*70)

        # Overall verdict
        gen_passed = generic_results['summary']['validations_passed']
        limn_passed = limn_results['summary']['validations_passed']
        total = generic_results['summary']['validations_total']

        print(f"\nGeneric embedder:     {gen_passed}/{total} validations passed")
        print(f"Limn-native embedder: {limn_passed}/{total} validations passed")

        if limn_passed > gen_passed:
            print(f"\n✓ Limn-native embedder BETTER on Eastern vocabulary (+{limn_passed - gen_passed} validations)")
        elif limn_passed == gen_passed:
            print(f"\n= Limn-native embedder EQUAL to generic on Eastern vocabulary")
        else:
            print(f"\n✗ Limn-native embedder WORSE on Eastern vocabulary ({limn_passed - gen_passed} validations)")

    # Save results (convert numpy types to Python types for JSON)
    def convert_to_python_types(obj):
        """Recursively convert numpy types to Python types."""
        if isinstance(obj, dict):
            return {k: convert_to_python_types(v) for k, v in obj.items()}
        elif isinstance(obj, list):
            return [convert_to_python_types(item) for item in obj]
        elif isinstance(obj, np.integer):
            return int(obj)
        elif isinstance(obj, np.floating):
            return float(obj)
        elif isinstance(obj, np.ndarray):
            return obj.tolist()
        return obj

    output = {
        "generic": convert_to_python_types(generic_results),
        "limn_native": convert_to_python_types(limn_results) if limn_results else None
    }

    output_path = Path(__file__).parent / "010-validation-results.json"
    with open(output_path, 'w') as f:
        json.dump(output, f, indent=2)

    print(f"\n📊 Results saved to {output_path}")
    print("\n" + "="*70)

if __name__ == "__main__":
    main()
