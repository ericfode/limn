#!/usr/bin/env python3
"""
Representation Geometry Analysis (v5 Sprint 3.3)

Analyzes the geometric structure of Limn embeddings from the v2 embedder:
1. Do words in the same semantic domain cluster together?
2. Do operator combinations form predictable geometric patterns?
3. Is embed(A@B) ≈ f(embed(A), embed(B))? (Compositional structure)
4. What does the embedding space look like? (t-SNE visualization)

If the embedder learned compositional structure, operator combinations
should form regular geometric patterns. If it memorized pairs, the
geometry will be random.

— Lex
"""

import json
import os
import sys
from collections import defaultdict
from pathlib import Path

import matplotlib
matplotlib.use('Agg')  # Non-interactive backend
import matplotlib.pyplot as plt
import numpy as np
from sklearn.manifold import TSNE
from sklearn.metrics import silhouette_score
from sklearn.cluster import KMeans

# Paths
SCRIPT_DIR = Path(__file__).resolve().parent
EMBEDDER_PATH = SCRIPT_DIR.parent / "embeddings" / "limn-embedder-v2"
VOCAB_JSON = SCRIPT_DIR.parent.parent / "src" / "claude-skill" / "vocabulary.json"
PAIRS_FILE = Path("/home/eric/src/limntown/limn/crew/translator/hgttg-training-pairs.jsonl")
OUTPUT_DIR = SCRIPT_DIR


def load_embedder():
    from sentence_transformers import SentenceTransformer
    return SentenceTransformer(str(EMBEDDER_PATH))


def load_domain_mapping() -> dict:
    """Load word -> domain mapping from vocabulary.json."""
    with open(VOCAB_JSON) as f:
        v = json.load(f)

    word_domain = {}
    for domain_id, domain_data in v['domains'].items():
        domain_name = domain_data.get('name', domain_id)
        categories = domain_data.get('categories', {})
        for cat_name, cat_data in categories.items():
            if isinstance(cat_data, dict):
                for word, info in cat_data.items():
                    if isinstance(info, dict) and 'region' in info:
                        word_domain[word] = domain_name
    return word_domain


def load_pairs():
    pairs = []
    with open(PAIRS_FILE) as f:
        for line in f:
            d = json.loads(line)
            pairs.append({
                "id": d["id"],
                "english": d["english"].strip('"'),
                "limn": d["limn"],
            })
    return pairs


def analyze_domain_clustering(embedder, word_domain):
    """Test whether words in the same domain cluster in embedding space."""
    print("\n[1] Domain Clustering Analysis")
    print("=" * 70)

    # Get words that have domain labels
    words = sorted(word_domain.keys())
    domains = [word_domain[w] for w in words]

    # Encode all words
    print(f"  Encoding {len(words)} words...")
    embeddings = embedder.encode(words, normalize_embeddings=True, show_progress_bar=False)

    # Compute within-domain vs between-domain similarity
    domain_set = sorted(set(domains))
    print(f"  {len(domain_set)} domains")

    # Group words by domain
    domain_indices = defaultdict(list)
    for i, d in enumerate(domains):
        domain_indices[d].append(i)

    # Within-domain similarity
    within_sims = []
    between_sims = []

    for domain, indices in domain_indices.items():
        if len(indices) < 2:
            continue
        # Within-domain pairs
        for i in range(len(indices)):
            for j in range(i + 1, len(indices)):
                sim = float(np.dot(embeddings[indices[i]], embeddings[indices[j]]))
                within_sims.append(sim)

    # Between-domain pairs (sample to avoid O(n^2) explosion)
    rng = np.random.RandomState(42)
    n_between = min(len(within_sims) * 2, 5000)
    for _ in range(n_between):
        d1, d2 = rng.choice(domain_set, 2, replace=False)
        i = rng.choice(domain_indices[d1])
        j = rng.choice(domain_indices[d2])
        sim = float(np.dot(embeddings[i], embeddings[j]))
        between_sims.append(sim)

    within_mean = np.mean(within_sims)
    between_mean = np.mean(between_sims)
    disc = within_mean - between_mean

    print(f"\n  Within-domain similarity:  {within_mean:.4f} (n={len(within_sims)})")
    print(f"  Between-domain similarity: {between_mean:.4f} (n={len(between_sims)})")
    print(f"  Discrimination:            {disc:.4f}")

    # Silhouette score (requires numeric labels)
    domain_to_id = {d: i for i, d in enumerate(domain_set)}
    labels = [domain_to_id[d] for d in domains]
    sil = silhouette_score(embeddings, labels)
    print(f"  Silhouette score:          {sil:.4f}")

    if sil > 0.1:
        print(f"  STRUCTURE: Domains form meaningful clusters (silhouette > 0.1)")
    elif sil > 0.0:
        print(f"  WEAK: Slight clustering tendency (silhouette > 0)")
    else:
        print(f"  NONE: No domain-based clustering detected")

    # Per-domain cohesion
    print(f"\n  Per-Domain Cohesion (within-domain mean similarity):")
    domain_cohesion = {}
    for domain, indices in sorted(domain_indices.items()):
        if len(indices) < 3:
            continue
        sims = []
        for i in range(len(indices)):
            for j in range(i + 1, len(indices)):
                sims.append(float(np.dot(embeddings[indices[i]], embeddings[indices[j]])))
        cohesion = np.mean(sims)
        domain_cohesion[domain] = cohesion
        n_words = len(indices)
        print(f"    {domain:<30s} {cohesion:>8.4f}  ({n_words} words)")

    return words, embeddings, domains, domain_set, {
        "within_mean": within_mean,
        "between_mean": between_mean,
        "discrimination": disc,
        "silhouette": sil,
        "domain_cohesion": domain_cohesion,
    }


def analyze_operator_geometry(embedder):
    """Test whether operators create predictable geometric transformations."""
    print("\n[2] Operator Geometry Analysis")
    print("=" * 70)

    # Test words — core vocabulary that the embedder should know
    words = ["lov", "fea", "joy", "sad", "kno", "hop", "str", "wis",
             "tru", "lig", "dar", "gro", "cha", "dep", "lif", "dea"]

    # Operators with their semantic descriptions
    operators = {
        "@": "projected through",
        "*": "interfering with",
        "\\": "minus",
        ":": "conditional on",
    }

    # Get base word embeddings
    base_embs = embedder.encode(words, normalize_embeddings=True, show_progress_bar=False)
    word_to_emb = {w: base_embs[i] for i, w in enumerate(words)}

    results = {}

    for op_sym, op_desc in operators.items():
        print(f"\n  Operator: {op_sym} ({op_desc})")

        # Generate A{op}B expressions for all pairs
        expressions = []
        a_embs_list = []
        b_embs_list = []

        for i, a in enumerate(words[:8]):
            for j, b in enumerate(words[8:]):
                expr = f"{a}{op_sym}{b}"
                expressions.append(expr)
                a_embs_list.append(word_to_emb[a])
                b_embs_list.append(word_to_emb[b])

        # Encode operator expressions
        expr_embs = embedder.encode(expressions, normalize_embeddings=True,
                                     show_progress_bar=False)
        a_embs = np.array(a_embs_list)
        b_embs = np.array(b_embs_list)

        # Test: Is embed(A{op}B) predictable from embed(A) and embed(B)?

        # Test 1: Linear combination - embed(A@B) ≈ α*embed(A) + β*embed(B)
        # Solve least squares: expr_embs ≈ X @ [α, β]
        X = np.column_stack([
            a_embs.reshape(len(expressions), -1),
            b_embs.reshape(len(expressions), -1)
        ])
        # Use pseudoinverse to find best linear map
        # expr_embs ≈ a_embs * W_a + b_embs * W_b
        # Simplified: compute correlation between actual and predicted
        pred_add = (a_embs + b_embs)
        pred_add = pred_add / np.linalg.norm(pred_add, axis=1, keepdims=True)

        # Cosine similarity between actual and additive prediction
        add_sims = np.sum(expr_embs * pred_add, axis=1)
        add_mean = float(np.mean(add_sims))

        # Test 2: Difference consistency - is (A@B - A@C) ≈ (D@B - D@C)?
        # This tests if the operator transformation is consistent
        diff_consistency = []
        for i in range(min(4, len(words[:8]))):
            for j in range(min(4, len(words[8:]))):
                for k in range(j + 1, min(4, len(words[8:]))):
                    idx_ij = i * len(words[8:]) + j
                    idx_ik = i * len(words[8:]) + k
                    diff_i = expr_embs[idx_ij] - expr_embs[idx_ik]

                    for i2 in range(i + 1, min(4, len(words[:8]))):
                        idx_i2j = i2 * len(words[8:]) + j
                        idx_i2k = i2 * len(words[8:]) + k
                        diff_i2 = expr_embs[idx_i2j] - expr_embs[idx_i2k]

                        # Cosine of difference vectors
                        norm_di = np.linalg.norm(diff_i)
                        norm_di2 = np.linalg.norm(diff_i2)
                        if norm_di > 0 and norm_di2 > 0:
                            cos = float(np.dot(diff_i, diff_i2) / (norm_di * norm_di2))
                            diff_consistency.append(cos)

        diff_mean = float(np.mean(diff_consistency)) if diff_consistency else 0.0

        # Test 3: Are all A@B closer to A than to random?
        a_sim = float(np.mean(np.sum(expr_embs * a_embs, axis=1)))
        b_sim = float(np.mean(np.sum(expr_embs * b_embs, axis=1)))

        print(f"    Additive prediction similarity:  {add_mean:.4f}")
        print(f"    Difference consistency:          {diff_mean:.4f}")
        print(f"    Similarity to A operand:         {a_sim:.4f}")
        print(f"    Similarity to B operand:         {b_sim:.4f}")

        results[op_sym] = {
            "additive_prediction": add_mean,
            "difference_consistency": diff_mean,
            "a_similarity": a_sim,
            "b_similarity": b_sim,
        }

    # Summary
    print(f"\n  Operator Geometry Summary:")
    print(f"  {'Op':<5s} {'Additive':>10s} {'Consistency':>12s} {'sim(A)':>8s} {'sim(B)':>8s}")
    print(f"  {'─' * 45}")
    for op, r in results.items():
        print(f"  {op:<5s} {r['additive_prediction']:>10.4f} {r['difference_consistency']:>12.4f} {r['a_similarity']:>8.4f} {r['b_similarity']:>8.4f}")

    return results


def analyze_compositionality(embedder):
    """Test specific compositional claims about operators."""
    print("\n[3] Compositionality Tests")
    print("=" * 70)

    tests = [
        # (expression, description, should_be_closer_to, should_be_farther_from)
        ("lov@fea", "love through fear", "fea", "joy"),
        ("joy*sad", "joy interfering with sad", "sad", "hop"),
        ("kno\\dou", "knowledge minus doubt", "kno", "fea"),
        ("str:fea", "strength conditional on fear", "str", "wis"),
    ]

    all_texts = []
    for expr, desc, close, far in tests:
        all_texts.extend([expr, desc, close, far])

    embs = embedder.encode(all_texts, normalize_embeddings=True, show_progress_bar=False)

    print(f"  {'Expression':<15s} {'Description':<30s} {'sim(desc)':>10s} {'sim(close)':>10s} {'sim(far)':>10s} {'Pass':>5s}")
    print(f"  {'─' * 75}")

    passed = 0
    total = 0
    idx = 0
    for expr, desc, close, far in tests:
        e_expr = embs[idx]
        e_desc = embs[idx + 1]
        e_close = embs[idx + 2]
        e_far = embs[idx + 3]
        idx += 4

        sim_desc = float(np.dot(e_expr, e_desc))
        sim_close = float(np.dot(e_expr, e_close))
        sim_far = float(np.dot(e_expr, e_far))

        # Pass if expression is closer to expected close word than far word
        is_pass = sim_close > sim_far
        total += 1
        if is_pass:
            passed += 1
        status = "Y" if is_pass else "N"

        print(f"  {expr:<15s} {desc:<30s} {sim_desc:>10.4f} {sim_close:>10.4f} {sim_far:>10.4f} {status:>5s}")

    print(f"\n  Compositionality score: {passed}/{total} ({passed/total:.0%})")

    return {"passed": passed, "total": total}


def visualize_tsne(words, embeddings, domains, domain_set, output_path):
    """Create t-SNE visualization of word embeddings colored by domain."""
    print("\n[4] t-SNE Visualization")
    print("=" * 70)

    # Assign colors to domains
    cmap = plt.cm.get_cmap('tab20', len(domain_set))
    domain_colors = {d: cmap(i) for i, d in enumerate(domain_set)}

    # Run t-SNE
    print(f"  Running t-SNE on {len(embeddings)} embeddings...")
    tsne = TSNE(n_components=2, random_state=42, perplexity=min(30, len(words) - 1))
    coords = tsne.fit_transform(embeddings)

    # Plot
    fig, ax = plt.subplots(1, 1, figsize=(16, 12))

    for domain in domain_set:
        mask = [i for i, d in enumerate(domains) if d == domain]
        if not mask:
            continue
        ax.scatter(
            coords[mask, 0], coords[mask, 1],
            c=[domain_colors[domain]],
            label=f"{domain} ({len(mask)})",
            alpha=0.7, s=20
        )

    # Label a subset of interesting words
    label_words = {"lov", "fea", "joy", "sad", "kno", "hop", "str", "wis",
                   "tru", "lig", "dar", "gro", "lif", "dea", "mov", "res",
                   "hum", "ani", "sol", "liq", "gas", "hot", "col", "big",
                   "red", "blu", "grn", "yel", "goo", "bad", "new", "old"}
    for i, w in enumerate(words):
        if w in label_words:
            ax.annotate(w, (coords[i, 0], coords[i, 1]),
                       fontsize=7, alpha=0.8,
                       xytext=(3, 3), textcoords='offset points')

    ax.set_title("Limn Vocabulary Embedding Space (t-SNE, v2 Embedder)")
    ax.legend(bbox_to_anchor=(1.05, 1), loc='upper left', fontsize=8)
    plt.tight_layout()
    plt.savefig(str(output_path), dpi=150, bbox_inches='tight')
    print(f"  Saved to: {output_path}")
    plt.close()


def nearest_neighbors_analysis(words, embeddings, word_domain, n=5):
    """Show nearest neighbors for key words — do they make semantic sense?"""
    print("\n[5] Nearest Neighbor Analysis")
    print("=" * 70)

    probe_words = ["lov", "fea", "kno", "lif", "lig", "mov", "red", "buy"]

    # Similarity matrix
    sim_matrix = embeddings @ embeddings.T

    word_to_idx = {w: i for i, w in enumerate(words)}

    for pw in probe_words:
        if pw not in word_to_idx:
            continue
        idx = word_to_idx[pw]
        sims = sim_matrix[idx]
        top_indices = np.argsort(-sims)[1:n+1]  # Exclude self

        domain = word_domain.get(pw, "?")
        print(f"\n  {pw} [{domain}]:")
        for ti in top_indices:
            neighbor = words[ti]
            neighbor_domain = word_domain.get(neighbor, "?")
            same = "+" if neighbor_domain == domain else "-"
            print(f"    {same} {neighbor:<6s} ({sims[ti]:.4f}) [{neighbor_domain}]")


def main():
    print("Representation Geometry Analysis — Limn Embedder v2")
    print("=" * 70)

    embedder = load_embedder()
    word_domain = load_domain_mapping()
    print(f"  {len(word_domain)} words with domain labels")

    # 1. Domain clustering
    words, embeddings, domains, domain_set, cluster_results = \
        analyze_domain_clustering(embedder, word_domain)

    # 2. Operator geometry
    operator_results = analyze_operator_geometry(embedder)

    # 3. Compositionality
    comp_results = analyze_compositionality(embedder)

    # 4. t-SNE visualization
    tsne_path = OUTPUT_DIR / "embedding_tsne.png"
    visualize_tsne(words, embeddings, domains, domain_set, tsne_path)

    # 5. Nearest neighbors
    nearest_neighbors_analysis(words, embeddings, word_domain)

    # =========================================================================
    # Summary
    # =========================================================================
    print(f"\n{'=' * 70}")
    print(f"  SUMMARY")
    print(f"{'=' * 70}")

    print(f"\n  Domain Clustering:")
    print(f"    Within-domain sim:  {cluster_results['within_mean']:.4f}")
    print(f"    Between-domain sim: {cluster_results['between_mean']:.4f}")
    print(f"    Silhouette score:   {cluster_results['silhouette']:.4f}")

    print(f"\n  Operator Geometry:")
    for op, r in operator_results.items():
        print(f"    {op}: additive={r['additive_prediction']:.3f}, consistency={r['difference_consistency']:.3f}")

    print(f"\n  Compositionality: {comp_results['passed']}/{comp_results['total']}")

    # Assess overall
    has_structure = cluster_results['silhouette'] > 0.05
    has_composition = comp_results['passed'] > comp_results['total'] * 0.5
    has_operator_geom = np.mean([r['additive_prediction'] for r in operator_results.values()]) > 0.3

    if has_structure and has_composition:
        print(f"\n  FINDING: Embedder has learned BOTH domain structure and compositional patterns")
    elif has_structure:
        print(f"\n  FINDING: Embedder has domain structure but NO compositional geometry")
    elif has_composition:
        print(f"\n  FINDING: Embedder has compositional patterns but NO domain clustering")
    else:
        print(f"\n  FINDING: Embedder has NEITHER domain structure NOR compositional geometry")
        print(f"  This is expected for an embedder trained on 329 pairs with BPE tokenizer.")
        print(f"  A custom Limn tokenizer (Phase 2.1) should improve structure significantly.")

    # Save results
    results = {
        "clustering": {
            "within_domain_sim": cluster_results['within_mean'],
            "between_domain_sim": cluster_results['between_mean'],
            "discrimination": cluster_results['discrimination'],
            "silhouette": cluster_results['silhouette'],
            "domain_cohesion": {k: float(v) for k, v in cluster_results['domain_cohesion'].items()},
        },
        "operator_geometry": {k: {k2: float(v2) for k2, v2 in v.items()} for k, v in operator_results.items()},
        "compositionality": comp_results,
    }

    output_file = OUTPUT_DIR / "geometry_results.json"
    with open(output_file, "w") as f:
        json.dump(results, f, indent=2)
    print(f"\n  Results saved to: {output_file}")
    print(f"  t-SNE plot saved to: {tsne_path}")


if __name__ == "__main__":
    main()
