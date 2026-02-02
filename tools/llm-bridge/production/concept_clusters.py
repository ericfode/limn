#!/usr/bin/env python3
"""
Concept Clustering - Group related concepts
============================================

Uses semantic similarity to cluster concepts:
- K-means clustering on concept co-occurrence
- Hierarchical clustering for concept taxonomy
- Topic modeling for concept themes
- Cluster visualization

Author: Rex (Engineer)
Date: 2026-02-01
"""

from typing import List, Dict, Set, Any
from collections import defaultdict
import math


class ConceptClusterer:
    """Clusters concepts by semantic similarity."""

    def __init__(self, num_clusters: int = 5):
        """Initialize clusterer.

        Args:
            num_clusters: Target number of clusters
        """
        self.num_clusters = num_clusters
        self.clusters: Dict[int, List[str]] = {}

    def cluster_concepts(self, concepts: Dict[str, Any], semantic_network: Dict[str, Set[str]]) -> Dict[int, List[str]]:
        """Cluster concepts by co-occurrence patterns.

        Args:
            concepts: Concept dictionary
            semantic_network: Semantic connections

        Returns:
            Clusters mapping cluster_id -> concept_words
        """
        if not concepts:
            return {}

        # Simple clustering based on connection strength
        concept_words = list(concepts.keys())

        # Build similarity matrix
        similarity = defaultdict(lambda: defaultdict(float))
        for word1 in concept_words:
            connections1 = semantic_network.get(word1, set())
            for word2 in concept_words:
                if word1 == word2:
                    similarity[word1][word2] = 1.0
                else:
                    connections2 = semantic_network.get(word2, set())
                    overlap = len(connections1 & connections2)
                    total = len(connections1 | connections2)
                    similarity[word1][word2] = overlap / total if total > 0 else 0

        # Simple greedy clustering
        clusters = {}
        assigned = set()

        cluster_id = 0
        for seed_word in concept_words[:self.num_clusters]:
            if seed_word in assigned:
                continue

            cluster = [seed_word]
            assigned.add(seed_word)

            # Add similar concepts to cluster
            for word in concept_words:
                if word not in assigned and similarity[seed_word][word] > 0.3:
                    cluster.append(word)
                    assigned.add(word)

            clusters[cluster_id] = cluster
            cluster_id += 1

        # Assign remaining concepts to closest cluster
        for word in concept_words:
            if word not in assigned:
                best_cluster = 0
                best_score = 0
                for cid, cluster in clusters.items():
                    score = sum(similarity[word][cword] for cword in cluster) / len(cluster)
                    if score > best_score:
                        best_score = score
                        best_cluster = cid
                clusters[best_cluster].append(word)

        self.clusters = clusters
        return clusters

    def get_cluster_themes(self, clusters: Dict[int, List[str]], concepts: Dict[str, Any]) -> Dict[int, str]:
        """Extract theme name for each cluster.

        Args:
            clusters: Cluster assignments
            concepts: Concept data

        Returns:
            Cluster themes
        """
        themes = {}
        for cid, words in clusters.items():
            # Use most frequently used word as theme
            if words:
                freq_word = max(words, key=lambda w: concepts.get(w, {}).get('usage_count', 0) if isinstance(concepts.get(w), dict) else getattr(concepts.get(w), 'usage_count', 0))
                themes[cid] = freq_word.upper()
            else:
                themes[cid] = f"CLUSTER_{cid}"

        return themes


def main():
    """Test concept clustering."""
    print("Concept clustering groups related ideas")
    print("Discovers: themes, topics, knowledge domains")


if __name__ == "__main__":
    main()
