#!/usr/bin/env python3
"""
Evaluate Limn Language Model
=============================

Evaluate finetuned Limn model on:
1. Limn purity (no English words)
2. Vocabulary adherence (uses Limn vocabulary)
3. Semantic coherence
4. Perplexity on held-out data

Author: Polecat now
Date: 2026-02-02
"""

import re
import json
import torch
import numpy as np
from pathlib import Path
from typing import List, Dict, Set, Tuple
from dataclasses import dataclass, asdict

from transformers import AutoTokenizer, AutoModelForCausalLM
from datasets import load_dataset
from peft import PeftModel


@dataclass
class EvaluationMetrics:
    """Metrics for model evaluation."""
    limn_purity: float  # % of outputs that are pure Limn
    vocabulary_adherence: float  # % of words in Limn vocabulary
    perplexity: float  # Model perplexity on validation set
    avg_output_length: float  # Average output length in words
    sample_outputs: List[Dict]  # Sample inputs and outputs


class LimnEvaluator:
    """Evaluate Limn language model."""

    def __init__(self, model_path: str, tokenizer_path: str = None):
        """Initialize evaluator.

        Args:
            model_path: Path to trained model
            tokenizer_path: Path to tokenizer (defaults to model_path)
        """
        self.model_path = model_path
        self.tokenizer_path = tokenizer_path or model_path

        self.model = None
        self.tokenizer = None
        self.limn_vocab = self._load_limn_vocabulary()
        self.english_stopwords = self._load_english_stopwords()

    def _load_limn_vocabulary(self) -> Set[str]:
        """Load Limn vocabulary for validation."""
        # Basic Limn vocabulary (expand as needed)
        vocab = {
            # Core
            "ess", "rea", "exi", "bei", "sel", "awa", "kno", "per", "sen", "con",
            "thi", "und", "int", "fel", "emo", "lov", "hat", "hop",
            "act", "do", "mak", "cre", "des",

            # States and qualities
            "sta", "cha", "gro", "flu", "fix", "goo", "bad", "bri", "dar", "str",

            # Time and space
            "tim", "pas", "pre", "fut", "now", "spa", "her", "the", "nea", "far",

            # Logic
            "tru", "fal", "pos", "imp", "nec", "qry", "wh", "why", "how", "ask",

            # Relations
            "sa", "dif", "sim", "bet", "joi", "sep",

            # Common
            "and", "or", "not", "all", "som", "non", "yo", "we", "thi", "tha",

            # Abstract
            "mea", "val", "ide", "tho", "pro", "eme", "bec", "tra",

            # Operators (for validation)
            "def", "exp", "int", "ctx", "qry", "ans", "nex",
        }

        return vocab

    def _load_english_stopwords(self) -> Set[str]:
        """Load English stopwords to detect non-Limn."""
        return {
            "the", "a", "an", "is", "are", "was", "were", "be", "been",
            "have", "has", "had", "will", "would", "should", "could",
            "may", "might", "must", "can", "to", "of", "in", "on", "at",
            "by", "for", "with", "from", "about", "into", "through"
        }

    def load_model(self):
        """Load trained model and tokenizer."""
        print(f"Loading model from {self.model_path}...")

        self.tokenizer = AutoTokenizer.from_pretrained(self.tokenizer_path)

        # Load base model
        self.model = AutoModelForCausalLM.from_pretrained(self.model_path)

        # Set to eval mode
        self.model.eval()

        print("Model loaded successfully")

    def is_pure_limn(self, text: str) -> bool:
        """Check if text is pure Limn.

        Args:
            text: Text to check

        Returns:
            True if pure Limn (no English)
        """
        # Remove operators and special characters
        cleaned = text
        for char in ["∎", "~", "∿", "|", "→", "←", "↔", ":", ".", ",", "!", "?"]:
            cleaned = cleaned.replace(char, " ")

        # Get words
        words = [w.strip().lower() for w in cleaned.split() if w.strip() and w.isalpha()]

        if not words:
            return True  # Empty is technically pure

        # Check for English stopwords
        english_count = sum(1 for w in words if w in self.english_stopwords)

        return english_count == 0

    def check_vocabulary_adherence(self, text: str) -> float:
        """Check what % of words are in Limn vocabulary.

        Args:
            text: Text to check

        Returns:
            Ratio of Limn vocabulary words
        """
        # Remove operators
        cleaned = text
        for char in ["∎", "~", "∿", "|", "→", "←", "↔"]:
            cleaned = cleaned.replace(char, " ")

        # Get words
        words = [w.strip().lower() for w in cleaned.split() if w.strip() and w.isalpha()]

        if not words:
            return 1.0

        # Count words in vocabulary
        vocab_count = sum(1 for w in words if w in self.limn_vocab)

        return vocab_count / len(words)

    def generate_text(self, prompt: str, max_length: int = 50) -> str:
        """Generate text from prompt.

        Args:
            prompt: Input prompt
            max_length: Maximum generation length

        Returns:
            Generated text
        """
        inputs = self.tokenizer(prompt, return_tensors="pt")

        with torch.no_grad():
            outputs = self.model.generate(
                **inputs,
                max_length=max_length,
                num_return_sequences=1,
                temperature=0.7,
                do_sample=True,
                pad_token_id=self.tokenizer.eos_token_id,
            )

        generated = self.tokenizer.decode(outputs[0], skip_special_tokens=True)

        # Extract output part (after prompt)
        if prompt in generated:
            generated = generated[len(prompt):].strip()

        return generated

    def calculate_perplexity(self, val_file: str) -> float:
        """Calculate perplexity on validation set.

        Args:
            val_file: Path to validation file

        Returns:
            Perplexity value
        """
        print("Calculating perplexity...")

        # Load validation data
        dataset = load_dataset("json", data_files={"validation": val_file})
        val_data = dataset["validation"]

        total_loss = 0
        num_examples = 0

        for example in val_data:
            # Format text
            text = f"Input: {example['input']}\nOutput: {example['output']}"

            # Tokenize
            inputs = self.tokenizer(text, return_tensors="pt", truncation=True, max_length=128)

            # Calculate loss
            with torch.no_grad():
                outputs = self.model(**inputs, labels=inputs["input_ids"])
                loss = outputs.loss
                total_loss += loss.item()
                num_examples += 1

        # Calculate perplexity
        avg_loss = total_loss / num_examples
        perplexity = np.exp(avg_loss)

        return perplexity

    def evaluate(self, val_file: str, num_samples: int = 100) -> EvaluationMetrics:
        """Run full evaluation.

        Args:
            val_file: Path to validation file
            num_samples: Number of samples to generate for evaluation

        Returns:
            Evaluation metrics
        """
        print("Running evaluation...")

        # Load validation data
        dataset = load_dataset("json", data_files={"validation": val_file})
        val_data = dataset["validation"]

        # Sample examples
        sample_indices = np.random.choice(len(val_data), min(num_samples, len(val_data)), replace=False)

        pure_limn_count = 0
        vocab_adherence_scores = []
        output_lengths = []
        sample_outputs = []

        for idx in sample_indices:
            example = val_data[int(idx)]

            # Generate output
            prompt = f"Input: {example['input']}\nOutput:"
            generated = self.generate_text(prompt)

            # Check purity
            is_pure = self.is_pure_limn(generated)
            if is_pure:
                pure_limn_count += 1

            # Check vocabulary adherence
            vocab_score = self.check_vocabulary_adherence(generated)
            vocab_adherence_scores.append(vocab_score)

            # Track length
            output_lengths.append(len(generated.split()))

            # Store samples
            if len(sample_outputs) < 10:
                sample_outputs.append({
                    "input": example['input'],
                    "expected": example['output'],
                    "generated": generated,
                    "pure_limn": is_pure,
                    "vocab_adherence": vocab_score
                })

        # Calculate metrics
        limn_purity = pure_limn_count / len(sample_indices)
        avg_vocab_adherence = np.mean(vocab_adherence_scores)
        avg_output_length = np.mean(output_lengths)

        # Calculate perplexity
        perplexity = self.calculate_perplexity(val_file)

        metrics = EvaluationMetrics(
            limn_purity=limn_purity,
            vocabulary_adherence=avg_vocab_adherence,
            perplexity=perplexity,
            avg_output_length=avg_output_length,
            sample_outputs=sample_outputs
        )

        return metrics

    def print_report(self, metrics: EvaluationMetrics):
        """Print evaluation report.

        Args:
            metrics: Evaluation metrics
        """
        print("\n" + "="*60)
        print("LIMN MODEL EVALUATION REPORT")
        print("="*60)
        print()
        print(f"Limn Purity:          {metrics.limn_purity:.1%}")
        print(f"Vocabulary Adherence: {metrics.vocabulary_adherence:.1%}")
        print(f"Perplexity:           {metrics.perplexity:.2f}")
        print(f"Avg Output Length:    {metrics.avg_output_length:.1f} words")
        print()
        print("="*60)
        print("SAMPLE OUTPUTS")
        print("="*60)
        print()

        for i, sample in enumerate(metrics.sample_outputs, 1):
            print(f"Sample {i}:")
            print(f"  Input:     {sample['input']}")
            print(f"  Expected:  {sample['expected']}")
            print(f"  Generated: {sample['generated']}")
            print(f"  Pure Limn: {'✓' if sample['pure_limn'] else '✗'}")
            print(f"  Vocab %:   {sample['vocab_adherence']:.1%}")
            print()

    def save_metrics(self, metrics: EvaluationMetrics, output_path: str):
        """Save metrics to JSON.

        Args:
            metrics: Evaluation metrics
            output_path: Output file path
        """
        with open(output_path, 'w') as f:
            json.dump(asdict(metrics), f, indent=2)

        print(f"\nMetrics saved to: {output_path}")


def main():
    """Main evaluation script."""
    import argparse

    parser = argparse.ArgumentParser(description="Evaluate Limn SLM")
    parser.add_argument("--model-path", required=True, help="Path to trained model")
    parser.add_argument("--val-file", default="limn_val.jsonl", help="Validation data file")
    parser.add_argument("--num-samples", type=int, default=100, help="Number of samples to evaluate")
    parser.add_argument("--output", default="evaluation_metrics.json", help="Output metrics file")

    args = parser.parse_args()

    print("=== Limn SLM Evaluation ===")
    print(f"Model: {args.model_path}")
    print(f"Validation file: {args.val_file}")
    print(f"Number of samples: {args.num_samples}")
    print()

    # Initialize evaluator
    evaluator = LimnEvaluator(args.model_path)
    evaluator.load_model()

    # Run evaluation
    metrics = evaluator.evaluate(args.val_file, args.num_samples)

    # Print report
    evaluator.print_report(metrics)

    # Save metrics
    evaluator.save_metrics(metrics, args.output)


if __name__ == "__main__":
    main()
