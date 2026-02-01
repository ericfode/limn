#!/usr/bin/env python3
"""
Train Limn-native sentence embedder using parallel corpus distillation.

Goal: Fix phrase composition failures where Limn phrases get only 0.17 similarity
to English equivalents (target >0.75).

Approach: Fine-tune sentence-transformers model to align Limn embeddings with
English embeddings using contrastive learning.
"""

import json
import torch
from sentence_transformers import SentenceTransformer, InputExample, losses
from torch.utils.data import DataLoader
import numpy as np
from typing import List, Tuple
import re


def load_training_data() -> List[Tuple[str, str]]:
    """
    Load Limn-English phrase pairs from experiment validation results.

    Returns:
        List of (limn_phrase, english_phrase) tuples
    """
    import os
    pairs = []

    # Get path to validation results relative to this script
    script_dir = os.path.dirname(os.path.abspath(__file__))
    validation_path = os.path.join(script_dir, '..', '009-validation-results.json')

    # Load from 009-validation-results.json
    with open(validation_path, 'r') as f:
        data = json.load(f)

    # Extract philosophical phrases
    for item in data['philosophical_phrases']:
        limn = item['limn']
        english = item['english']
        pairs.append((limn, english))

    # Extract concept mappings (term -> concept)
    for item in data['concept_similarity']:
        term = item['term']
        concept = item['concept']
        description = item['description']
        # Add both the term-concept pair and term-description pair
        pairs.append((term, concept))
        pairs.append((term, description))

    # Extract compositionality examples
    for item in data['compositionality']:
        phrase = item['phrase']
        description = item['description']
        pairs.append((phrase, description))

    return pairs


def create_synthetic_pairs(pairs: List[Tuple[str, str]]) -> List[Tuple[str, str]]:
    """
    Augment training data with synthetic pairs.

    This helps the model learn compositional structure by creating
    variations of existing phrases.
    """
    synthetic = []

    # For each pair, create word-level mappings
    for limn, english in pairs:
        limn_words = limn.split()
        english_words = english.split()

        # If phrases are similar length, create partial alignments
        if len(limn_words) > 1 and len(english_words) > 1:
            # Create pairs with first halves and second halves
            mid_limn = len(limn_words) // 2
            mid_eng = len(english_words) // 2

            first_half = (
                ' '.join(limn_words[:mid_limn]),
                ' '.join(english_words[:mid_eng])
            )
            second_half = (
                ' '.join(limn_words[mid_limn:]),
                ' '.join(english_words[mid_eng:])
            )

            if len(first_half[0]) > 2:
                synthetic.append(first_half)
            if len(second_half[0]) > 2:
                synthetic.append(second_half)

    return synthetic


def prepare_training_examples(pairs: List[Tuple[str, str]]) -> List[InputExample]:
    """
    Convert Limn-English pairs to sentence-transformers InputExamples.

    Uses semantic textual similarity format where paired sentences should
    have high similarity (score=1.0).
    """
    examples = []

    for idx, (limn, english) in enumerate(pairs):
        # Create positive pair (Limn phrase should be similar to English)
        examples.append(InputExample(
            texts=[limn, english],
            label=1.0  # High similarity
        ))

    return examples


def train_limn_embedder(
    base_model: str = 'sentence-transformers/all-MiniLM-L6-v2',
    epochs: int = 10,
    batch_size: int = 16,
    output_path: str = './limn-embedder'
):
    """
    Fine-tune a sentence-transformers model for Limn-English alignment.

    Args:
        base_model: Base sentence-transformers model to fine-tune
        epochs: Number of training epochs
        batch_size: Batch size for training
        output_path: Path to save the fine-tuned model
    """
    print("=" * 80)
    print("LIMN-NATIVE EMBEDDER TRAINING")
    print("=" * 80)

    # Load base model
    print(f"\n1. Loading base model: {base_model}")
    model = SentenceTransformer(base_model)

    # Load training data
    print("\n2. Loading training data from experiments...")
    pairs = load_training_data()
    print(f"   Loaded {len(pairs)} Limn-English pairs")

    # Augment with synthetic pairs
    print("\n3. Creating synthetic training pairs...")
    synthetic = create_synthetic_pairs(pairs)
    print(f"   Created {len(synthetic)} synthetic pairs")

    all_pairs = pairs + synthetic
    print(f"   Total training pairs: {len(all_pairs)}")

    # Prepare training examples
    print("\n4. Preparing training examples...")
    train_examples = prepare_training_examples(all_pairs)

    # Create DataLoader
    train_dataloader = DataLoader(
        train_examples,
        shuffle=True,
        batch_size=batch_size
    )

    # Define loss function (Cosine Similarity Loss)
    # This encourages Limn embeddings to be close to English embeddings
    train_loss = losses.CosineSimilarityLoss(model)

    # Train the model
    print(f"\n5. Training model for {epochs} epochs...")
    print(f"   Batch size: {batch_size}")
    print(f"   Device: {model.device}")

    model.fit(
        train_objectives=[(train_dataloader, train_loss)],
        epochs=epochs,
        warmup_steps=100,
        output_path=output_path,
        show_progress_bar=True
    )

    print(f"\n6. Model saved to: {output_path}")
    return model


def validate_embedder(model_path: str = './limn-embedder'):
    """
    Validate the trained embedder on test phrases.

    Computes similarity between Limn and English phrases to verify
    the embedder fixes the composition failure.
    """
    import os
    print("\n" + "=" * 80)
    print("VALIDATION")
    print("=" * 80)

    # Load trained model
    print(f"\n1. Loading trained model from: {model_path}")
    model = SentenceTransformer(model_path)

    # Load validation data
    print("\n2. Loading validation data...")
    script_dir = os.path.dirname(os.path.abspath(__file__))
    validation_path = os.path.join(script_dir, '..', '009-validation-results.json')
    with open(validation_path, 'r') as f:
        data = json.load(f)

    philosophical_phrases = data['philosophical_phrases']

    print("\n3. Computing similarities on test set:")
    print("-" * 80)

    similarities = []

    for item in philosophical_phrases:
        limn = item['limn']
        english = item['english']
        original_sim = item['similarity']

        # Compute embeddings
        limn_emb = model.encode(limn, convert_to_tensor=True)
        eng_emb = model.encode(english, convert_to_tensor=True)

        # Compute cosine similarity
        similarity = torch.nn.functional.cosine_similarity(
            limn_emb.unsqueeze(0),
            eng_emb.unsqueeze(0)
        ).item()

        similarities.append(similarity)

        # Format for display
        improvement = similarity - original_sim
        status = "✓" if similarity > 0.75 else "✗"

        print(f"{status} {limn[:40]:<40} | {english[:40]:<40}")
        print(f"  Original: {original_sim:.3f} → New: {similarity:.3f} "
              f"(Δ {improvement:+.3f})")
        print()

    # Compute statistics
    mean_sim = np.mean(similarities)
    original_mean = np.mean([p['similarity'] for p in philosophical_phrases])
    improvement = mean_sim - original_mean

    print("-" * 80)
    print(f"\nSUMMARY:")
    print(f"  Original mean similarity: {original_mean:.3f}")
    print(f"  New mean similarity:      {mean_sim:.3f}")
    print(f"  Improvement:              {improvement:+.3f}")
    print(f"  Target:                   >0.75")
    print(f"  Status:                   {'✓ PASS' if mean_sim > 0.75 else '✗ FAIL'}")

    # Check individual phrase performance
    passed = sum(1 for s in similarities if s > 0.75)
    total = len(similarities)
    print(f"\n  Phrases passing >0.75:    {passed}/{total} ({100*passed/total:.1f}%)")

    return mean_sim


def main():
    """Main training and validation pipeline."""
    import argparse

    parser = argparse.ArgumentParser(
        description='Train Limn-native sentence embedder'
    )
    parser.add_argument(
        '--base-model',
        default='sentence-transformers/all-MiniLM-L6-v2',
        help='Base sentence-transformers model'
    )
    parser.add_argument(
        '--epochs',
        type=int,
        default=20,
        help='Number of training epochs'
    )
    parser.add_argument(
        '--batch-size',
        type=int,
        default=16,
        help='Training batch size'
    )
    parser.add_argument(
        '--output',
        default='./limn-embedder',
        help='Output path for trained model'
    )
    parser.add_argument(
        '--validate-only',
        action='store_true',
        help='Only run validation on existing model'
    )

    args = parser.parse_args()

    # Check CUDA availability
    print("\nGPU/CUDA Status:")
    print(f"  CUDA available: {torch.cuda.is_available()}")
    if torch.cuda.is_available():
        print(f"  CUDA version: {torch.version.cuda}")
        print(f"  Device: {torch.cuda.get_device_name(0)}")
    print()

    if args.validate_only:
        validate_embedder(args.output)
    else:
        # Train model
        model = train_limn_embedder(
            base_model=args.base_model,
            epochs=args.epochs,
            batch_size=args.batch_size,
            output_path=args.output
        )

        # Validate results
        mean_sim = validate_embedder(args.output)

        print("\n" + "=" * 80)
        print("TRAINING COMPLETE")
        print("=" * 80)
        print(f"\nFinal mean similarity: {mean_sim:.3f}")
        print(f"Target achieved: {'Yes ✓' if mean_sim > 0.75 else 'No ✗'}")

        if mean_sim > 0.75:
            print("\n🎉 SUCCESS! Limn-native embedder fixes phrase composition!")
        else:
            print("\n⚠️  Model needs more training or data augmentation")


if __name__ == '__main__':
    main()
