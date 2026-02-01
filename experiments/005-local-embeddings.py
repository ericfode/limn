#!/usr/bin/env python3
"""
Experiment 005: Embedding Compositionality - LOCAL IMPLEMENTATION
Uses sentence-transformers for local embeddings (no API needed)

Usage:
    python3 005-local-embeddings.py --test 1  # Linear compositionality
    python3 005-local-embeddings.py --test all  # All tests
"""

import numpy as np
from typing import List, Dict
import json
from dataclasses import dataclass
import argparse

# Local embeddings
try:
    from sentence_transformers import SentenceTransformer
    ST_AVAILABLE = True
except ImportError:
    ST_AVAILABLE = False
    print("Warning: sentence-transformers not available")

from scipy.spatial.distance import cosine


@dataclass
class CompositionTest:
    """A test case for compositional semantics"""
    word_a: str
    word_b: str
    composition: str
    expected_meaning: str
    test_type: str


class LocalEmbeddingCompositionality:
    """Test suite using local sentence-transformers"""

    def __init__(self, model_name: str = "all-MiniLM-L6-v2"):
        """Initialize with local embedding model"""
        if not ST_AVAILABLE:
            raise ImportError("Install: pip install sentence-transformers")

        print(f"Loading model: {model_name}...")
        self.model = SentenceTransformer(model_name)
        print(f"Model loaded. Embedding dimension: {self.model.get_sentence_embedding_dimension()}")

        self.embeddings_cache = {}
        self.results = {}

    def get_embedding(self, text: str) -> np.ndarray:
        """Get embedding for text, with caching"""
        if text in self.embeddings_cache:
            return self.embeddings_cache[text]

        embedding = self.model.encode(text, convert_to_numpy=True)
        self.embeddings_cache[text] = embedding
        return embedding

    def cosine_similarity(self, vec_a: np.ndarray, vec_b: np.ndarray) -> float:
        """Compute cosine similarity"""
        return 1 - cosine(vec_a, vec_b)

    def compose_addition(self, emb_a: np.ndarray, emb_b: np.ndarray) -> np.ndarray:
        """Composition via vector addition"""
        return emb_a + emb_b

    def compose_min(self, emb_a: np.ndarray, emb_b: np.ndarray) -> np.ndarray:
        """Composition via element-wise minimum"""
        return np.minimum(emb_a, emb_b)

    def compose_hadamard(self, emb_a: np.ndarray, emb_b: np.ndarray) -> np.ndarray:
        """Composition via Hadamard product"""
        return emb_a * emb_b

    def test_linear_compositionality(self, test_cases: List[CompositionTest]) -> Dict:
        """
        Test 1: Linear Compositionality
        Compare: addition, min, Hadamard
        """
        print("\n" + "="*70)
        print("TEST 1: LINEAR COMPOSITIONALITY")
        print("="*70)
        print("\nDr. Solvik's Prediction: min or Hadamard > addition\n")

        results = {
            'addition': [],
            'min': [],
            'hadamard': [],
            'cases': []
        }

        valid_cases = [t for t in test_cases if t.test_type == 'valid']
        print(f"Testing {len(valid_cases)} valid compositions...\n")

        for i, test in enumerate(valid_cases, 1):
            print(f"[{i}/{len(valid_cases)}] {test.word_a} + {test.word_b} → {test.composition}")
            print(f"     Expected: {test.expected_meaning}")

            # Get embeddings
            emb_a = self.get_embedding(test.word_a)
            emb_b = self.get_embedding(test.word_b)
            emb_composition = self.get_embedding(test.composition)

            # Three composition operations
            composed_add = self.compose_addition(emb_a, emb_b)
            composed_min = self.compose_min(emb_a, emb_b)
            composed_had = self.compose_hadamard(emb_a, emb_b)

            # Normalize
            composed_add = composed_add / np.linalg.norm(composed_add)
            composed_min = composed_min / np.linalg.norm(composed_min)
            composed_had = composed_had / np.linalg.norm(composed_had)

            # Compute similarities
            sim_add = self.cosine_similarity(composed_add, emb_composition)
            sim_min = self.cosine_similarity(composed_min, emb_composition)
            sim_had = self.cosine_similarity(composed_had, emb_composition)

            results['addition'].append(sim_add)
            results['min'].append(sim_min)
            results['hadamard'].append(sim_had)
            results['cases'].append({
                'composition': test.composition,
                'addition': float(sim_add),
                'min': float(sim_min),
                'hadamard': float(sim_had)
            })

            # Determine winner
            scores = [('Add', sim_add), ('Min', sim_min), ('Had', sim_had)]
            winner = max(scores, key=lambda x: x[1])

            print(f"     Addition:  {sim_add:.4f}")
            print(f"     Min:       {sim_min:.4f} {'✓' if sim_min == winner[1] else ''}")
            print(f"     Hadamard:  {sim_had:.4f} {'✓' if sim_had == winner[1] else ''}")
            print(f"     → WINNER: {winner[0]} ({winner[1]:.4f})\n")

        # Summary statistics
        print("\n" + "="*70)
        print("SUMMARY STATISTICS")
        print("="*70)

        mean_add = np.mean(results['addition'])
        mean_min = np.mean(results['min'])
        mean_had = np.mean(results['hadamard'])

        std_add = np.std(results['addition'])
        std_min = np.std(results['min'])
        std_had = np.std(results['hadamard'])

        print(f"\nAddition:  Mean={mean_add:.4f}, Std={std_add:.4f}")
        print(f"Min:       Mean={mean_min:.4f}, Std={std_min:.4f}")
        print(f"Hadamard:  Mean={mean_had:.4f}, Std={std_had:.4f}")

        # Winner counts
        winners = {'Add': 0, 'Min': 0, 'Had': 0}
        for case in results['cases']:
            scores = [('Add', case['addition']), ('Min', case['min']), ('Had', case['hadamard'])]
            winner = max(scores, key=lambda x: x[1])[0]
            winners[winner] += 1

        print(f"\nWin counts:")
        print(f"  Addition:  {winners['Add']}/{len(valid_cases)} ({100*winners['Add']/len(valid_cases):.1f}%)")
        print(f"  Min:       {winners['Min']}/{len(valid_cases)} ({100*winners['Min']/len(valid_cases):.1f}%)")
        print(f"  Hadamard:  {winners['Had']}/{len(valid_cases)} ({100*winners['Had']/len(valid_cases):.1f}%)")

        # Test Dr. Solvik's prediction
        print("\n" + "="*70)
        print("DR. SOLVIK'S PREDICTION")
        print("="*70)

        if mean_min > mean_add or mean_had > mean_add:
            print("\n✓ PREDICTION CONFIRMED: min or Hadamard outperforms addition")
            if mean_min > mean_had:
                print(f"  → MIN is best ({mean_min:.4f} > {mean_had:.4f})")
                print("  → Constraint INTERSECTION validated!")
            else:
                print(f"  → HADAMARD is best ({mean_had:.4f} > {mean_min:.4f})")
                print("  → Feature BINDING validated!")
        else:
            print("\n✗ PREDICTION NOT CONFIRMED: addition performs best")
            print(f"  → Standard compositional semantics ({mean_add:.4f})")

        self.results['test1'] = results
        return results

    def test_commutativity(self, test_cases: List[CompositionTest]) -> Dict:
        """
        Test 3: Commutativity
        Does embed(A B) = embed(B A)?
        """
        print("\n" + "="*70)
        print("TEST 3: COMMUTATIVITY")
        print("="*70)
        print("\nHypothesis: Limn is commutative, so embed(A B) ≈ embed(B A)\n")

        results = {'similarities': [], 'pairs': []}

        for test in test_cases:
            if test.test_type == 'contradiction':
                continue

            # Create reversed composition
            composition_ab = f"{test.word_a} {test.word_b}"
            composition_ba = f"{test.word_b} {test.word_a}"

            emb_ab = self.get_embedding(composition_ab)
            emb_ba = self.get_embedding(composition_ba)

            similarity = self.cosine_similarity(emb_ab, emb_ba)
            results['similarities'].append(similarity)
            results['pairs'].append({
                'forward': composition_ab,
                'reverse': composition_ba,
                'similarity': float(similarity)
            })

            print(f"{composition_ab:15s} ↔ {composition_ba:15s} : {similarity:.4f}")

        mean_sim = np.mean(results['similarities'])
        std_sim = np.std(results['similarities'])

        print("\n" + "="*70)
        print(f"Mean commutativity: {mean_sim:.4f} ± {std_sim:.4f}")
        print("="*70)

        if mean_sim > 0.95:
            print("\n✓ HIGH commutativity confirmed (>0.95)")
            print("  → Limn embeddings are order-independent")
        elif mean_sim > 0.80:
            print("\n⚠ MODERATE commutativity (0.80-0.95)")
            print("  → Some word order effects present")
        else:
            print("\n✗ LOW commutativity (<0.80)")
            print("  → Significant word order effects")

        self.results['test3'] = results
        return results

    def save_results(self, output_file: str = "results-local.json"):
        """Save all results to JSON"""
        with open(output_file, 'w') as f:
            json.dump(self.results, f, indent=2)
        print(f"\n✓ Saved results to {output_file}")


def load_test_corpus(corpus_file: str) -> List[CompositionTest]:
    """Load test corpus from file"""
    if corpus_file.endswith('.json'):
        with open(corpus_file, 'r') as f:
            data = json.load(f)

        # Handle both my format and Dr. Solvik's format
        if 'test_cases' in data:  # Dr. Solvik's format
            return []  # TODO: Parse Dr. Solvik's format
        else:  # My format
            return [CompositionTest(**case) for case in data]

    return []


def get_builtin_test_cases() -> List[CompositionTest]:
    """Built-in starter corpus"""
    return [
        # Physical compositions
        CompositionTest("sol", "aqu", "sol aqu", "ice", "valid"),
        CompositionTest("liq", "aqu", "liq aqu", "water", "valid"),
        CompositionTest("gas", "aqu", "gas aqu", "steam", "valid"),
        CompositionTest("hot", "dry", "hot dry", "desert", "valid"),
        CompositionTest("col", "wet", "col wet", "rain", "valid"),

        # Emotional
        CompositionTest("joy", "lov", "joy lov", "happiness", "valid"),
        CompositionTest("sad", "wan", "sad wan", "longing", "valid"),

        # Properties
        CompositionTest("big", "str", "big str", "powerful", "valid"),
        CompositionTest("sma", "bri", "sma bri", "star", "valid"),
        CompositionTest("old", "kno", "old kno", "wisdom", "valid"),

        # Contradictions
        CompositionTest("hot", "col", "hot col", "contradiction", "contradiction"),
        CompositionTest("sol", "gas", "sol gas", "contradiction", "contradiction"),
    ]


def main():
    parser = argparse.ArgumentParser(description='Run Limn embedding experiments locally')
    parser.add_argument('--test', type=str, default='1',
                       help='Test number (1, 3, or "all")')
    parser.add_argument('--model', type=str, default='all-MiniLM-L6-v2',
                       help='Sentence-transformers model name')
    parser.add_argument('--corpus', type=str, default='test_corpus.json',
                       help='Path to test corpus')
    parser.add_argument('--output', type=str, default='results-local.json',
                       help='Output file')

    args = parser.parse_args()

    # Initialize
    print("\n" + "="*70)
    print("EXPERIMENT 005: EMBEDDING COMPOSITIONALITY (LOCAL)")
    print("="*70)

    exp = LocalEmbeddingCompositionality(model_name=args.model)

    # Load test cases
    try:
        test_cases = load_test_corpus(args.corpus)
        if not test_cases:
            print(f"Using built-in test cases (corpus file not found or empty)")
            test_cases = get_builtin_test_cases()
    except:
        print(f"Using built-in test cases")
        test_cases = get_builtin_test_cases()

    print(f"\nLoaded {len(test_cases)} test cases")

    # Run tests
    if args.test == '1' or args.test == 'all':
        exp.test_linear_compositionality(test_cases)

    if args.test == '3' or args.test == 'all':
        exp.test_commutativity(test_cases)

    # Save results
    exp.save_results(args.output)

    print("\n" + "="*70)
    print("EXPERIMENT COMPLETE")
    print("="*70)


if __name__ == "__main__":
    main()
