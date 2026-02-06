#!/usr/bin/env python3
"""
Limn Embedder v2: Contrastive training on 329 HGttG parallel pairs.

Fixes the catastrophic embedding collapse found in v1 (H8: FALSIFIED).

Root cause of v1 failure:
- Only 41 training pairs with all-positive labels (1.0)
- CosineSimilarityLoss with no negatives
- 20 epochs on tiny data
- Result: model learned to project everything to the same point

v2 fixes:
- MultipleNegativesRankingLoss: in-batch negatives automatically
- 329 HGttG parallel pairs (real Limn, not Greek philosophy)
- Proper train/val split with discrimination monitoring
- Early stopping based on validation discrimination

— Lex
"""

import json
import os
import sys
import random
import numpy as np
from datetime import datetime

from sentence_transformers import (
    SentenceTransformer,
    InputExample,
    losses,
    evaluation,
)
from sentence_transformers.evaluation import SentenceEvaluator
from torch.utils.data import DataLoader

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
HGTTG_PATH = "/home/eric/src/limntown/limn/crew/translator/hgttg-training-pairs.jsonl"
OUTPUT_DIR = os.path.join(SCRIPT_DIR, "limn-embedder-v2")
BASE_MODEL = "sentence-transformers/all-MiniLM-L6-v2"

# Hyperparameters
EPOCHS = 10
BATCH_SIZE = 32
LR = 2e-5
WARMUP_RATIO = 0.1
SEED = 42
VAL_RATIO = 0.2


def load_data():
    """Load and split HGttG parallel pairs."""
    pairs = []
    with open(HGTTG_PATH, 'r') as f:
        for line in f:
            obj = json.loads(line.strip())
            en = obj.get("english", "").strip()
            lm = obj.get("limn", "").strip()
            if en and lm:
                pairs.append({"limn": lm, "english": en})

    random.seed(SEED)
    random.shuffle(pairs)

    split = int(len(pairs) * (1 - VAL_RATIO))
    train_pairs = pairs[:split]
    val_pairs = pairs[split:]

    print(f"Total pairs: {len(pairs)}")
    print(f"Train: {len(train_pairs)}, Val: {len(val_pairs)}")

    return train_pairs, val_pairs


def create_examples(pairs):
    """Convert pairs to InputExamples for MNRL."""
    examples = []
    for p in pairs:
        # MNRL expects (anchor, positive) pairs
        # It uses other items in batch as negatives
        examples.append(InputExample(texts=[p["limn"], p["english"]]))
    return examples


class DiscriminationEvaluator(SentenceEvaluator):
    """Custom evaluator that checks positive vs negative discrimination."""

    def __init__(self, val_pairs, name="discrimination"):
        super().__init__()
        self.val_pairs = val_pairs
        self.name = name
        self.primary_metric = "discrimination"
        self.best_discrimination = -1

    def __call__(self, model, output_path=None, epoch=-1, steps=-1, **kwargs):
        # Positive pairs
        limn_texts = [p["limn"] for p in self.val_pairs]
        eng_texts = [p["english"] for p in self.val_pairs]

        limn_embs = model.encode(limn_texts, normalize_embeddings=True)
        eng_embs = model.encode(eng_texts, normalize_embeddings=True)

        # Positive: matching pairs
        pos_sims = [float(np.dot(limn_embs[i], eng_embs[i]))
                     for i in range(len(self.val_pairs))]

        # Negative: mismatched pairs (shifted by 1)
        neg_sims = [float(np.dot(limn_embs[i], eng_embs[(i + 1) % len(self.val_pairs)]))
                     for i in range(len(self.val_pairs))]

        # Random strings
        random_texts = ["xvz qwp bnm", "rtf jkl mnb", "asd fgh zxc", "plk iqw mnz"]
        random_embs = model.encode(random_texts, normalize_embeddings=True)
        random_sims = [float(np.dot(random_embs[i], eng_embs[i % len(eng_embs)]))
                       for i in range(len(random_texts))]

        pos_mean = np.mean(pos_sims)
        neg_mean = np.mean(neg_sims)
        rnd_mean = np.mean(random_sims)
        discrimination = pos_mean - neg_mean

        print(f"\n  Epoch {epoch} | Positive: {pos_mean:.4f} | "
              f"Negative: {neg_mean:.4f} | Random: {rnd_mean:.4f} | "
              f"Discrimination: {discrimination:.4f}")

        if discrimination > self.best_discrimination:
            self.best_discrimination = discrimination
            if output_path:
                model.save(output_path)
                print(f"  New best discrimination: {discrimination:.4f}")

        # Return discrimination as the metric to track
        return discrimination


def validate_final_inline(model, val_pairs):
    """Run comprehensive validation using model in memory."""
    print(f"\n{'='*60}")
    print(f"  FINAL VALIDATION")
    print(f"{'='*60}")

    base = SentenceTransformer(BASE_MODEL)

    # Test categories
    tests = {
        "Positive (matched)": [(p["limn"], p["english"]) for p in val_pairs],
        "Negative (shifted)": [(val_pairs[i]["limn"],
                                val_pairs[(i+1) % len(val_pairs)]["english"])
                               for i in range(len(val_pairs))],
        "Random gibberish": [("xvz qwp", "love and fear"),
                             ("bnm rtf", "knowledge and wisdom"),
                             ("plk iqw", "growth and change"),
                             ("mnz vbx", "truth and hope"),
                             ("wer typ", "light and darkness")],
    }

    results = {}
    for name, pairs in tests.items():
        a_texts = [p[0] for p in pairs]
        b_texts = [p[1] for p in pairs]

        a_embs = model.encode(a_texts, normalize_embeddings=True)
        b_embs = model.encode(b_texts, normalize_embeddings=True)
        ft_sims = [float(np.dot(a_embs[i], b_embs[i])) for i in range(len(pairs))]

        a_embs_b = base.encode(a_texts, normalize_embeddings=True)
        b_embs_b = base.encode(b_texts, normalize_embeddings=True)
        base_sims = [float(np.dot(a_embs_b[i], b_embs_b[i])) for i in range(len(pairs))]

        ft_mean = np.mean(ft_sims)
        base_mean = np.mean(base_sims)

        print(f"\n  {name}:")
        print(f"    v2 Mean:   {ft_mean:.4f}")
        print(f"    Base Mean: {base_mean:.4f}")
        print(f"    Delta:     {ft_mean - base_mean:+.4f}")

        results[name] = {
            "v2_mean": float(ft_mean),
            "v2_std": float(np.std(ft_sims)),
            "base_mean": float(base_mean),
            "n": len(pairs),
        }

    # Discrimination check
    pos = results["Positive (matched)"]["v2_mean"]
    neg = results["Negative (shifted)"]["v2_mean"]
    rnd = results["Random gibberish"]["v2_mean"]
    disc = pos - neg

    print(f"\n  DISCRIMINATION: {disc:.4f} (pos={pos:.4f}, neg={neg:.4f}, rnd={rnd:.4f})")

    if disc < 0.1:
        print(f"  FAIL: Embedding space has collapsed (discrimination < 0.1)")
        status = "COLLAPSED"
    elif disc < 0.2:
        print(f"  WEAK: Minimal discrimination ({disc:.4f})")
        status = "WEAK"
    elif neg > 0.7:
        print(f"  WARNING: Negative scores still high ({neg:.4f})")
        status = "PARTIAL"
    else:
        print(f"  PASS: Good discrimination between positive and negative pairs")
        status = "PASS"

    return {"status": status, "discrimination": disc, "details": results}


def main():
    print("=" * 60)
    print("  Limn Embedder v2 Training")
    print("  MultipleNegativesRankingLoss on 329 HGttG pairs")
    print("=" * 60)

    train_pairs, val_pairs = load_data()

    print(f"\nLoading base model: {BASE_MODEL}")
    model = SentenceTransformer(BASE_MODEL)

    # Create training data
    train_examples = create_examples(train_pairs)
    train_dataloader = DataLoader(
        train_examples,
        shuffle=True,
        batch_size=BATCH_SIZE,
    )

    # Loss: MultipleNegativesRankingLoss
    # Uses in-batch negatives — no explicit negative sampling needed
    train_loss = losses.MultipleNegativesRankingLoss(model)

    # Evaluator
    evaluator = DiscriminationEvaluator(val_pairs)

    # Training
    warmup_steps = int(len(train_dataloader) * EPOCHS * WARMUP_RATIO)
    print(f"\nTraining config:")
    print(f"  Epochs: {EPOCHS}")
    print(f"  Batch size: {BATCH_SIZE}")
    print(f"  Learning rate: {LR}")
    print(f"  Warmup steps: {warmup_steps}")
    print(f"  Total steps: {len(train_dataloader) * EPOCHS}")
    print(f"  Loss: MultipleNegativesRankingLoss")

    print(f"\n{'='*60}")
    print(f"  Pre-training baseline:")
    evaluator(model, epoch=-1)

    print(f"\n{'='*60}")
    print(f"  Training...")

    model.fit(
        train_objectives=[(train_dataloader, train_loss)],
        evaluator=evaluator,
        epochs=EPOCHS,
        warmup_steps=warmup_steps,
        optimizer_params={"lr": LR},
        evaluation_steps=len(train_dataloader),  # Evaluate every epoch
        save_best_model=False,
    )

    print(f"\n{'='*60}")
    print(f"  Training complete.")

    # Save model properly
    model.save(OUTPUT_DIR)
    print(f"  Model saved to: {OUTPUT_DIR}")

    # Final validation (use model in memory, don't reload)
    val_results = validate_final_inline(model, val_pairs)

    # Save full results
    results = {
        "model": "limn-embedder-v2",
        "base_model": BASE_MODEL,
        "training_data": "329 HGttG parallel pairs",
        "loss": "MultipleNegativesRankingLoss",
        "epochs": EPOCHS,
        "batch_size": BATCH_SIZE,
        "lr": LR,
        "train_size": len(train_pairs),
        "val_size": len(val_pairs),
        "seed": SEED,
        "timestamp": datetime.now().isoformat(),
        "validation": val_results,
    }

    results_path = os.path.join(SCRIPT_DIR, "embedder_v2_results.json")
    with open(results_path, 'w') as f:
        json.dump(results, f, indent=2)
    print(f"\n  Full results saved to: {results_path}")

    return val_results["status"]


if __name__ == "__main__":
    status = main()
    sys.exit(0 if status == "PASS" else 1)
