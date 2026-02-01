# Limn Semantic Search

Semantic search infrastructure for Limn vocabulary using ChromaDB and sentence embeddings.

## Overview

This system enables semantic search over the complete Limn vocabulary (902 words) using:
- **ChromaDB**: Vector database for efficient similarity search
- **Sentence Transformers**: Pre-trained embeddings (all-MiniLM-L6-v2)
- **384-dimensional embeddings**: Compact yet expressive semantic representations

## Quick Start

### Build the Index

```bash
# Activate virtual environment
source .venv/bin/activate

# Build semantic search index from Dolt vocabulary database
python tools/semantic_search.py --build
```

This will:
1. Load all 902 vocabulary entries from `data/vocabulary/` (Dolt)
2. Embed English meanings using sentence-transformers
3. Store embeddings in ChromaDB at `data/chroma_db/`
4. Compute coherence metrics saved to `data/coherence_metrics.json`

### Query the Index

```bash
# Search for words related to "wisdom"
python tools/semantic_search.py --query "wisdom" -n 10

# Search for "enlightenment"
python tools/semantic_search.py --query "enlightenment" -n 5

# Search for "karma"
python tools/semantic_search.py --query "karma" -n 10
```

## Example Queries

```bash
$ python tools/semantic_search.py --query "wisdom" -n 5

Query: wisdom
--------------------------------------------------------------------------------

1. wis (similarity: 0.822)
   wisdom, insight

2. pru (similarity: 0.595)
   prudence, wisdom in action

3. hns (similarity: 0.545)
   honesty, truthfulness

4. bri (similarity: 0.486)
   hope, clarity, intelligence

5. hur (similarity: 0.465)
   hurting
```

```bash
$ python tools/semantic_search.py --query "enlightenment" -n 5

Query: enlightenment
--------------------------------------------------------------------------------

1. bod (similarity: 0.738)
   awakening, enlightenment (Buddhist)

2. zen (similarity: 0.561)
   zen, enlightened calm

3. wis (similarity: 0.532)
   wisdom, insight

4. gnr (similarity: 0.479)
   generosity, liberality

5. reb (similarity: 0.469)
   rebelling
```

## Coherence Metrics

The index computes semantic coherence metrics:

```json
{
  "mean_similarity": 0.1626,
  "std_similarity": 0.0908,
  "mean_top_10_similarity": 0.4519,
  "vocabulary_size": 902,
  "embedding_dim": 384
}
```

**Interpretation:**
- `mean_similarity`: Average cosine similarity between all word pairs (0.1626 = diverse semantic space)
- `std_similarity`: Standard deviation of similarities (0.0908 = good variance)
- `mean_top_10_similarity`: Average similarity of each word's 10 nearest neighbors (0.4519 = related but not identical)

A mean similarity around 0.15-0.20 indicates a healthy semantic space with good differentiation.

## Architecture

### Embedding Strategy

**Vocabulary Embeddings:**
- Input: English meanings from Dolt database
- Model: `sentence-transformers/all-MiniLM-L6-v2` (base model)
- Why: Provides good semantic differentiation across diverse meanings

**Query Embeddings:**
- Input: English query text
- Model: `sentence-transformers/all-MiniLM-L6-v2` (same as vocabulary)
- Future: Will support Limn queries using the Limn-native embedder

### Search Algorithm

1. Query text → Embed using base model → 384-dim vector
2. Compute cosine similarity with all stored embeddings
3. Rank by similarity (highest first)
4. Return top-k results with metadata

## Files

- `tools/semantic_search.py` - Main semantic search implementation
- `data/chroma_db/` - ChromaDB persistent storage (902 embeddings)
- `data/coherence_metrics.json` - Vocabulary coherence statistics
- `data/vocabulary/` - Dolt vocabulary database (source data)

## Dependencies

```bash
# Install in virtual environment
uv pip install chromadb sentence-transformers
```

Versions used:
- chromadb==1.4.1
- sentence-transformers==5.2.2
- torch==2.10.0

## Performance

- **Index build time**: ~1-2 seconds (902 words)
- **Query time**: <0.1 seconds (in-memory similarity computation)
- **Storage**: ~350KB (embeddings) + SQLite metadata

## Future Enhancements

- [ ] Support Limn word queries using Limn-native embedder
- [x] Coherence score computation
- [ ] Dolt database sync for versioning
- [ ] Web interface for interactive search
- [ ] Multi-language query support

## Usage from Python

```python
from tools.semantic_search import LimnSemanticSearch

# Initialize
search = LimnSemanticSearch()

# Build index
search.build_index()

# Query
results = search.query("wisdom", n_results=5)

for word_id, metadata, similarity in zip(
    results['ids'][0],
    results['metadatas'][0],
    results['similarities'][0]
):
    print(f"{metadata['word']}: {metadata['meaning']} ({similarity:.3f})")
```

---

Built with ChromaDB + Sentence Transformers for semantic search over Limn vocabulary.
