#!/usr/bin/env python3
"""
Experiment 005: Embedding Compositionality - Implementation
Test if Limn's constraint intersection maps to geometric operations in embedding space.

Usage:
    python 005-implementation.py --test 1  # Linear compositionality
    python 005-implementation.py --test all  # All tests

Requirements:
    pip install openai numpy scipy scikit-learn matplotlib seaborn
"""

import os
import numpy as np
from typing import List, Dict, Tuple
import json
from dataclasses import dataclass
import argparse

# Embedding API imports
try:
    from openai import OpenAI
    OPENAI_AVAILABLE = True
except ImportError:
    OPENAI_AVAILABLE = False
    print("Warning: OpenAI not available. Install with: pip install openai")

# Analysis imports
from scipy.spatial.distance import cosine
from sklearn.manifold import TSNE
import matplotlib.pyplot as plt
import seaborn as sns


@dataclass
class CompositionTest:
    """A test case for compositional semantics"""
    word_a: str
    word_b: str
    composition: str
    expected_meaning: str
    test_type: str  # 'valid', 'contradiction', 'cross-lingual'


class EmbeddingCompositionality:
    """Test suite for Limn embedding compositionality"""

    def __init__(self, api_key: str = None):
        """Initialize with OpenAI API key"""
        self.api_key = api_key or os.getenv('OPENAI_API_KEY')
        if OPENAI_AVAILABLE and self.api_key:
            self.client = OpenAI(api_key=self.api_key)
        else:
            self.client = None

        self.embeddings_cache = {}
        self.results = {}

    def get_embedding(self, text: str, model: str = "text-embedding-3-large") -> np.ndarray:
        """Get embedding for a text string, with caching"""
        cache_key = f"{model}:{text}"

        if cache_key in self.embeddings_cache:
            return self.embeddings_cache[cache_key]

        if not self.client:
            raise ValueError("OpenAI client not initialized. Set OPENAI_API_KEY environment variable.")

        response = self.client.embeddings.create(
            input=text,
            model=model
        )

        embedding = np.array(response.data[0].embedding)
        self.embeddings_cache[cache_key] = embedding
        return embedding

    def cosine_similarity(self, vec_a: np.ndarray, vec_b: np.ndarray) -> float:
        """Compute cosine similarity between two vectors"""
        return 1 - cosine(vec_a, vec_b)

    def compose_addition(self, emb_a: np.ndarray, emb_b: np.ndarray) -> np.ndarray:
        """Composition via vector addition"""
        return emb_a + emb_b

    def compose_min(self, emb_a: np.ndarray, emb_b: np.ndarray) -> np.ndarray:
        """Composition via element-wise minimum (intersection-like)"""
        return np.minimum(emb_a, emb_b)

    def compose_hadamard(self, emb_a: np.ndarray, emb_b: np.ndarray) -> np.ndarray:
        """Composition via Hadamard product (element-wise multiplication)"""
        return emb_a * emb_b

    def test_linear_compositionality(self, test_cases: List[CompositionTest]) -> Dict:
        """
        Test 1: Linear Compositionality
        Does embed(A B) ≈ f(embed(A), embed(B))?

        Tests three composition functions:
        - Addition: embed(A) + embed(B)
        - Element-wise min: min(embed(A), embed(B))
        - Hadamard product: embed(A) ⊙ embed(B)
        """
        print("\n=== Test 1: Linear Compositionality ===\n")

        results = {
            'addition': [],
            'min': [],
            'hadamard': []
        }

        for test in test_cases:
            if test.test_type == 'contradiction':
                continue  # Skip contradictions for linearity test

            print(f"Testing: {test.word_a} + {test.word_b} → {test.composition} ({test.expected_meaning})")

            # Get embeddings
            emb_a = self.get_embedding(test.word_a)
            emb_b = self.get_embedding(test.word_b)
            emb_composition = self.get_embedding(test.composition)

            # Test three composition operations
            composed_add = self.compose_addition(emb_a, emb_b)
            composed_min = self.compose_min(emb_a, emb_b)
            composed_had = self.compose_hadamard(emb_a, emb_b)

            # Normalize (important for comparison)
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

            print(f"  Addition:  {sim_add:.4f}")
            print(f"  Min:       {sim_min:.4f}")
            print(f"  Hadamard:  {sim_had:.4f}")
            print(f"  Winner:    {max([('Add', sim_add), ('Min', sim_min), ('Had', sim_had)], key=lambda x: x[1])[0]}")
            print()

        # Summary statistics
        print("\n=== Summary Statistics ===")
        print(f"Addition:  Mean={np.mean(results['addition']):.4f}, Std={np.std(results['addition']):.4f}")
        print(f"Min:       Mean={np.mean(results['min']):.4f}, Std={np.std(results['min']):.4f}")
        print(f"Hadamard:  Mean={np.mean(results['hadamard']):.4f}, Std={np.std(results['hadamard']):.4f}")

        # Dr. Solvik's prediction: min or Hadamard > addition
        if np.mean(results['min']) > np.mean(results['addition']) or \
           np.mean(results['hadamard']) > np.mean(results['addition']):
            print("\n✓ Dr. Solvik's prediction CONFIRMED: min or Hadamard outperforms addition")
        else:
            print("\n✗ Dr. Solvik's prediction NOT confirmed: addition performs best")

        self.results['test1'] = results
        return results

    def test_operator_consistency(self, test_cases: List[CompositionTest]) -> Dict:
        """
        Test 2: Operator Consistency
        Does 'nu' consistently negate vectors?
        """
        print("\n=== Test 2: Operator Consistency ===\n")
        # Implementation TODO
        pass

    def test_commutativity(self, test_cases: List[CompositionTest]) -> Dict:
        """
        Test 3: Commutativity
        Does embed(A B) = embed(B A)?
        """
        print("\n=== Test 3: Commutativity ===\n")

        results = {'similarities': []}

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

            print(f"{composition_ab} ↔ {composition_ba}: {similarity:.4f}")

        mean_sim = np.mean(results['similarities'])
        print(f"\nMean commutativity: {mean_sim:.4f}")

        if mean_sim > 0.95:
            print("✓ HIGH commutativity confirmed")
        elif mean_sim > 0.80:
            print("⚠ MODERATE commutativity")
        else:
            print("✗ LOW commutativity - Limn shows word order effects")

        self.results['test3'] = results
        return results

    def visualize_embedding_space(self, test_cases: List[CompositionTest], output_file: str = "embedding_space.png"):
        """Visualize embeddings in 2D using t-SNE"""
        print("\n=== Visualizing Embedding Space ===\n")

        # Collect all embeddings
        embeddings = []
        labels = []

        for test in test_cases[:20]:  # Limit for clarity
            emb_a = self.get_embedding(test.word_a)
            emb_b = self.get_embedding(test.word_b)
            emb_comp = self.get_embedding(test.composition)

            embeddings.extend([emb_a, emb_b, emb_comp])
            labels.extend([test.word_a, test.word_b, f"{test.composition}\n({test.expected_meaning})"])

        # t-SNE projection
        embeddings = np.array(embeddings)
        tsne = TSNE(n_components=2, random_state=42, perplexity=min(30, len(embeddings) - 1))
        embeddings_2d = tsne.fit_transform(embeddings)

        # Plot
        plt.figure(figsize=(15, 10))

        # Color by whether it's a single word or composition
        colors = ['blue' if ' ' not in label else 'red' for label in labels]

        plt.scatter(embeddings_2d[:, 0], embeddings_2d[:, 1], c=colors, alpha=0.6)

        for i, label in enumerate(labels):
            plt.annotate(label, (embeddings_2d[i, 0], embeddings_2d[i, 1]),
                        fontsize=8, alpha=0.7)

        plt.title("Limn Embedding Space (t-SNE projection)")
        plt.xlabel("Dimension 1")
        plt.ylabel("Dimension 2")
        plt.savefig(output_file, dpi=300, bbox_inches='tight')
        print(f"Saved visualization to {output_file}")

    def save_results(self, output_file: str = "results.json"):
        """Save all results to JSON"""
        with open(output_file, 'w') as f:
            # Convert numpy values to Python floats
            serializable_results = {}
            for test_name, test_results in self.results.items():
                serializable_results[test_name] = {}
                for key, values in test_results.items():
                    if isinstance(values, list):
                        serializable_results[test_name][key] = [float(v) for v in values]
                    else:
                        serializable_results[test_name][key] = values

            json.dump(serializable_results, f, indent=2)
        print(f"\nSaved results to {output_file}")


def load_test_corpus(corpus_file: str) -> List[CompositionTest]:
    """Load test corpus from file"""
    if not os.path.exists(corpus_file):
        print(f"Warning: Test corpus {corpus_file} not found. Using built-in test cases.")
        return get_builtin_test_cases()

    with open(corpus_file, 'r') as f:
        data = json.load(f)

    return [CompositionTest(**case) for case in data]


def get_builtin_test_cases() -> List[CompositionTest]:
    """Built-in test cases for initial testing"""
    return [
        # Physical compositions
        CompositionTest("sol", "aqu", "sol aqu", "ice", "valid"),
        CompositionTest("liq", "aqu", "liq aqu", "water", "valid"),
        CompositionTest("gas", "aqu", "gas aqu", "steam", "valid"),

        # Property combinations
        CompositionTest("hot", "dry", "hot dry", "desert", "valid"),
        CompositionTest("col", "wet", "col wet", "rain", "valid"),

        # Abstract compositions
        CompositionTest("joy", "lov", "joy lov", "happiness", "valid"),
        CompositionTest("sad", "wan", "sad wan", "longing", "valid"),

        # Contradictions
        CompositionTest("hot", "col", "hot col", "contradiction", "contradiction"),
        CompositionTest("sol", "gas", "sol gas", "contradiction", "contradiction"),
    ]


def main():
    parser = argparse.ArgumentParser(description='Run Limn embedding compositionality experiments')
    parser.add_argument('--test', type=str, default='1',
                       help='Test number to run (1, 2, 3, or "all")')
    parser.add_argument('--corpus', type=str, default='test_corpus.json',
                       help='Path to test corpus JSON file')
    parser.add_argument('--output', type=str, default='results.json',
                       help='Output file for results')

    args = parser.parse_args()

    # Initialize experiment
    exp = EmbeddingCompositionality()

    # Load test corpus
    test_cases = load_test_corpus(args.corpus)
    print(f"Loaded {len(test_cases)} test cases")

    # Run tests
    if args.test == '1' or args.test == 'all':
        exp.test_linear_compositionality(test_cases)

    if args.test == '3' or args.test == 'all':
        exp.test_commutativity(test_cases)

    if args.test == 'all':
        exp.visualize_embedding_space(test_cases)

    # Save results
    exp.save_results(args.output)


if __name__ == "__main__":
    main()
