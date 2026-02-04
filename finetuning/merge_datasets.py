#!/usr/bin/env python3
"""
Merge and Split Datasets
=========================

Combine corpus-based and synthetic datasets, then split into train/validation.

Author: Polecat now
Date: 2026-02-02
"""

import json
import random
from pathlib import Path
from typing import List, Dict


def load_jsonl(file_path: str) -> List[Dict]:
    """Load JSONL file.

    Args:
        file_path: Path to JSONL file

    Returns:
        List of examples
    """
    examples = []
    with open(file_path, 'r', encoding='utf-8') as f:
        for line in f:
            examples.append(json.loads(line))
    return examples


def save_jsonl(examples: List[Dict], file_path: str):
    """Save examples to JSONL.

    Args:
        examples: Examples to save
        file_path: Output file path
    """
    with open(file_path, 'w', encoding='utf-8') as f:
        for example in examples:
            f.write(json.dumps(example, ensure_ascii=False) + '\n')


def main():
    """Merge datasets and create splits."""
    base_dir = Path(__file__).parent

    # Load datasets
    print("Loading datasets...")
    corpus_dataset = load_jsonl(str(base_dir / "limn_finetune_dataset.jsonl"))
    synthetic_dataset = load_jsonl(str(base_dir / "limn_synthetic_dataset.jsonl"))

    print(f"Corpus dataset: {len(corpus_dataset)} examples")
    print(f"Synthetic dataset: {len(synthetic_dataset)} examples")

    # Combine
    all_examples = corpus_dataset + synthetic_dataset
    print(f"Total examples: {len(all_examples)}")

    # Shuffle
    random.seed(42)  # Reproducible shuffle
    random.shuffle(all_examples)

    # Split into train/validation (90/10)
    split_idx = int(len(all_examples) * 0.9)
    train_examples = all_examples[:split_idx]
    val_examples = all_examples[split_idx:]

    print(f"\nSplit:")
    print(f"  Train: {len(train_examples)} examples")
    print(f"  Validation: {len(val_examples)} examples")

    # Save splits
    save_jsonl(train_examples, str(base_dir / "limn_train.jsonl"))
    save_jsonl(val_examples, str(base_dir / "limn_val.jsonl"))

    # Save combined
    save_jsonl(all_examples, str(base_dir / "limn_combined.jsonl"))

    print("\nSaved:")
    print(f"  Train: limn_train.jsonl")
    print(f"  Validation: limn_val.jsonl")
    print(f"  Combined: limn_combined.jsonl")

    # Statistics
    print("\n=== Dataset Statistics ===")

    # Type distribution in training set
    type_counts = {}
    for ex in train_examples:
        if 'metadata' in ex and 'type' in ex['metadata']:
            ex_type = ex['metadata']['type']
            type_counts[ex_type] = type_counts.get(ex_type, 0) + 1

    print("\nTraining set distribution:")
    for ex_type, count in sorted(type_counts.items(), key=lambda x: x[1], reverse=True):
        pct = count / len(train_examples) * 100
        print(f"  {ex_type}: {count} ({pct:.1f}%)")

    # Average lengths
    avg_input_len = sum(len(ex['input'].split()) for ex in train_examples) / len(train_examples)
    avg_output_len = sum(len(ex['output'].split()) for ex in train_examples) / len(train_examples)

    print(f"\nAverage lengths:")
    print(f"  Input: {avg_input_len:.1f} words")
    print(f"  Output: {avg_output_len:.1f} words")


if __name__ == "__main__":
    main()
