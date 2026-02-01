#!/usr/bin/env python3
"""
Limn Semantic Search using ChromaDB and Limn-native embedder.

Build semantic search infrastructure for Limn vocabulary:
- Load all vocabulary from Dolt database
- Embed definitions using Limn-native embedder
- Store in ChromaDB for vector search
- Provide query interface
- Compute coherence scores
"""

import os
import sys
import json
import subprocess
from pathlib import Path
from typing import List, Dict, Tuple
import numpy as np

import chromadb
from chromadb.config import Settings
from sentence_transformers import SentenceTransformer


class LimnSemanticSearch:
    """Semantic search engine for Limn vocabulary."""

    def __init__(self, vocab_db_path: str = None, embedder_path: str = None):
        """
        Initialize semantic search engine.

        Args:
            vocab_db_path: Path to Dolt vocabulary database
            embedder_path: Path to Limn-native embedder model
        """
        # Set default paths relative to project root
        project_root = Path(__file__).parent.parent

        if vocab_db_path is None:
            vocab_db_path = project_root / "data" / "vocabulary"
        if embedder_path is None:
            embedder_path = project_root / "experiments" / "embeddings" / "limn-embedder"

        self.vocab_db_path = Path(vocab_db_path)
        self.embedder_path = Path(embedder_path)

        # Initialize ChromaDB
        self.chroma_client = chromadb.PersistentClient(
            path=str(project_root / "data" / "chroma_db"),
            settings=Settings(anonymized_telemetry=False)
        )

        # Get or create collection without default embedding function
        # We provide our own embeddings via the Limn embedder
        try:
            self.collection = self.chroma_client.get_collection(name="limn_vocabulary")
        except:
            # Create new collection - delete if exists to reset
            try:
                self.chroma_client.delete_collection(name="limn_vocabulary")
            except:
                pass
            self.collection = self.chroma_client.create_collection(
                name="limn_vocabulary",
                metadata={"description": "Limn bootstrap vocabulary embeddings"}
            )

        # Load Limn embedder
        print(f"Loading Limn embedder from {self.embedder_path}...")
        self.embedder = SentenceTransformer(str(self.embedder_path))
        print("✓ Embedder loaded")

    def load_vocabulary(self) -> List[Dict]:
        """Load all vocabulary from Dolt database."""
        print(f"Loading vocabulary from {self.vocab_db_path}...")

        # Query Dolt database
        cmd = [
            "dolt", "sql", "-q",
            "SELECT word, source, meaning, examples, domain_id FROM words ORDER BY word",
            "-r", "json"
        ]

        result = subprocess.run(
            cmd,
            cwd=self.vocab_db_path,
            capture_output=True,
            text=True
        )

        if result.returncode != 0:
            raise RuntimeError(f"Failed to query Dolt database: {result.stderr}")

        # Parse JSON output (Dolt wraps results in {"rows": [...]} )
        data = json.loads(result.stdout)
        rows = data.get('rows', [])

        print(f"✓ Loaded {len(rows)} vocabulary entries")
        return rows

    def embed_vocabulary(self, vocab: List[Dict]) -> Tuple[List[str], np.ndarray]:
        """
        Embed all vocabulary using Limn embedder.

        Args:
            vocab: List of vocabulary entries

        Returns:
            Tuple of (word_ids, embeddings)
        """
        print("Embedding vocabulary...")

        # Prepare texts to embed: use meaning alone for better semantic space
        texts = []
        word_ids = []

        for entry in vocab:
            word = entry['word']
            meaning = entry['meaning']

            # Embed just the meaning to get semantic differentiation
            # The Limn embedder was trained on simple Limn-English pairs
            texts.append(meaning)
            word_ids.append(word)

        # Embed all texts using Limn embedder
        embeddings = self.embedder.encode(
            texts,
            batch_size=32,
            show_progress_bar=True,
            convert_to_numpy=True
        )

        print(f"✓ Created {len(embeddings)} embeddings (384-dim)")
        return word_ids, embeddings

    def store_in_chromadb(self, vocab: List[Dict], word_ids: List[str], embeddings: np.ndarray):
        """Store vocabulary and embeddings in ChromaDB."""
        print("Storing in ChromaDB...")

        # Prepare data for ChromaDB
        documents = []
        metadatas = []
        ids = []

        for i, entry in enumerate(vocab):
            # Document text (for retrieval)
            doc = f"{entry['word']} ({entry['source']}): {entry['meaning']}"
            if entry.get('examples'):
                doc += f"\nExamples: {entry['examples']}"

            documents.append(doc)

            # Metadata
            metadatas.append({
                "word": entry['word'],
                "source": entry['source'],
                "meaning": entry['meaning'],
                "domain_id": entry.get('domain_id', 0)
            })

            # ID
            ids.append(word_ids[i])

        # Store in ChromaDB (upsert to handle re-runs)
        self.collection.upsert(
            ids=ids,
            embeddings=embeddings.tolist(),
            documents=documents,
            metadatas=metadatas
        )

        print(f"✓ Stored {len(ids)} entries in ChromaDB")

    def compute_coherence_scores(self, embeddings: np.ndarray) -> Dict[str, float]:
        """
        Compute coherence metrics for the vocabulary embeddings.

        Args:
            embeddings: Vocabulary embeddings matrix

        Returns:
            Dictionary of coherence metrics
        """
        print("Computing coherence scores...")

        # Normalize embeddings for cosine similarity
        norms = np.linalg.norm(embeddings, axis=1, keepdims=True)
        normalized = embeddings / norms

        # Compute pairwise similarities
        similarities = normalized @ normalized.T

        # Remove diagonal (self-similarity)
        n = len(embeddings)
        mask = ~np.eye(n, dtype=bool)
        similarities_no_diag = similarities[mask].reshape(n, n-1)

        # Compute metrics
        mean_similarity = float(np.mean(similarities_no_diag))
        std_similarity = float(np.std(similarities_no_diag))

        # Top-k average similarities (most similar neighbors)
        k = min(10, n-1)
        top_k_sims = np.sort(similarities_no_diag, axis=1)[:, -k:]
        mean_top_k = float(np.mean(top_k_sims))

        metrics = {
            "mean_similarity": mean_similarity,
            "std_similarity": std_similarity,
            "mean_top_10_similarity": mean_top_k,
            "vocabulary_size": n,
            "embedding_dim": embeddings.shape[1]
        }

        print("✓ Coherence metrics:")
        for key, value in metrics.items():
            if isinstance(value, float):
                print(f"  {key}: {value:.4f}")
            else:
                print(f"  {key}: {value}")

        return metrics

    def query(self, query_text: str, n_results: int = 10) -> Dict:
        """
        Query the semantic search index.

        Args:
            query_text: Query string (Limn or English)
            n_results: Number of results to return

        Returns:
            Query results with words and similarities
        """
        # Embed query
        query_embedding = self.embedder.encode(query_text, convert_to_numpy=True)

        # Debug: print embedding stats
        print(f"Debug: Query embedding shape: {query_embedding.shape}")
        print(f"Debug: Query embedding norm: {np.linalg.norm(query_embedding):.4f}")

        # Search ChromaDB
        results = self.collection.query(
            query_embeddings=[query_embedding.tolist()],
            n_results=n_results
        )

        return results

    def build_index(self):
        """Build the complete semantic search index."""
        print("=" * 80)
        print("BUILDING LIMN SEMANTIC SEARCH INDEX")
        print("=" * 80)
        print()

        # Load vocabulary
        vocab = self.load_vocabulary()

        # Embed vocabulary
        word_ids, embeddings = self.embed_vocabulary(vocab)

        # Store in ChromaDB
        self.store_in_chromadb(vocab, word_ids, embeddings)

        # Compute coherence
        metrics = self.compute_coherence_scores(embeddings)

        # Save metrics
        metrics_path = Path(__file__).parent.parent / "data" / "coherence_metrics.json"
        with open(metrics_path, 'w') as f:
            json.dump(metrics, f, indent=2)
        print(f"\n✓ Saved coherence metrics to {metrics_path}")

        print("\n" + "=" * 80)
        print("INDEX BUILD COMPLETE")
        print("=" * 80)
        print(f"Collection: {self.collection.name}")
        print(f"Entries: {self.collection.count()}")
        print()

        return metrics


def main():
    """Main entry point."""
    import argparse

    parser = argparse.ArgumentParser(description="Limn Semantic Search")
    parser.add_argument('--build', action='store_true', help='Build the search index')
    parser.add_argument('--query', type=str, help='Query the index')
    parser.add_argument('-n', '--n-results', type=int, default=10, help='Number of results')

    args = parser.parse_args()

    search = LimnSemanticSearch()

    if args.build:
        search.build_index()

    if args.query:
        results = search.query(args.query, n_results=args.n_results)

        print(f"\nQuery: {args.query}")
        print("-" * 80)

        for i, (word_id, document, metadata, distance) in enumerate(zip(
            results['ids'][0],
            results['documents'][0],
            results['metadatas'][0],
            results['distances'][0]
        )):
            similarity = 1 - distance  # Convert distance to similarity
            print(f"\n{i+1}. {metadata['word']} (similarity: {similarity:.3f})")
            print(f"   {metadata['meaning']}")


if __name__ == '__main__':
    main()
