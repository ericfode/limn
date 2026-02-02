#!/usr/bin/env python3
"""
Format Limn Training Dataset
=============================

Transform extracted Limn corpus and synthetic data into training format.

Target format:
{
  "input": "instruction or query in Limn",
  "output": "expected response in Limn"
}

Author: Polecat now
Date: 2026-02-02
"""

import json
import random
import re
from pathlib import Path
from typing import List, Dict, Tuple, Set
from dataclasses import dataclass


@dataclass
class TrainingExample:
    """A single training example."""
    input: str
    output: str
    metadata: Dict = None


class DatasetFormatter:
    """Format Limn expressions into training data."""

    def __init__(self):
        """Initialize formatter."""
        self.limn_operators = ['∎', '~', '∿', '|', '→', '←', '↔', 'sa']
        self.english_words = self._load_english_stopwords()

    def _load_english_stopwords(self) -> Set[str]:
        """Load common English words to filter out."""
        # Common English words that should not appear in pure Limn
        return {
            'the', 'a', 'an', 'and', 'or', 'but', 'is', 'are', 'was', 'were',
            'be', 'been', 'being', 'have', 'has', 'had', 'do', 'does', 'did',
            'will', 'would', 'should', 'could', 'may', 'might', 'must',
            'can', 'to', 'of', 'in', 'on', 'at', 'by', 'for', 'with', 'from',
            'this', 'that', 'these', 'those', 'it', 'its', 'they', 'their',
            'what', 'when', 'where', 'why', 'how', 'which', 'who', 'whom'
        }

    def is_pure_limn(self, text: str) -> bool:
        """Check if text is pure Limn (no English).

        Args:
            text: Text to check

        Returns:
            True if pure Limn
        """
        # Remove operators and special chars
        cleaned = text
        for op in self.limn_operators:
            cleaned = cleaned.replace(op, ' ')

        # Get words
        words = [w.strip().lower() for w in cleaned.split() if w.strip()]

        if not words:
            return True  # Empty is technically pure

        # Check for English words
        english_count = sum(1 for w in words if w in self.english_words)

        # Also check for long words (Limn words are typically 2-3 letters)
        long_words = sum(1 for w in words if len(w) > 4)

        # Pure Limn should have no English stopwords and few long words
        return english_count == 0 and long_words < len(words) * 0.2

    def create_interpretation_pair(self, limn_expr: str, context: str = "") -> TrainingExample:
        """Create an interpretation task.

        Args:
            limn_expr: Limn expression
            context: Optional context

        Returns:
            Training example
        """
        # Input: ask to interpret the expression
        input_variants = [
            f"int {limn_expr}",  # interpret [expression]
            f"mea {limn_expr}",  # meaning [expression]
            f"qry mea {limn_expr}",  # query meaning [expression]
        ]

        input_text = random.choice(input_variants)

        # Output: the expression itself (self-supervised learning)
        # Or a paraphrase/elaboration
        output_text = limn_expr

        return TrainingExample(
            input=input_text,
            output=output_text,
            metadata={"type": "interpretation", "context": context}
        )

    def create_completion_pair(self, limn_expr: str) -> List[TrainingExample]:
        """Create completion tasks from an expression.

        Args:
            limn_expr: Limn expression

        Returns:
            List of training examples
        """
        words = limn_expr.split()

        if len(words) < 3:
            return []  # Too short for meaningful completion

        examples = []

        # Create prefix completion tasks
        for i in range(1, len(words)):
            prefix = ' '.join(words[:i])
            completion = ' '.join(words[i:])

            example = TrainingExample(
                input=prefix,
                output=completion,
                metadata={"type": "completion"}
            )
            examples.append(example)

        return examples

    def create_query_response_pair(self, limn_expr: str) -> TrainingExample:
        """Create query-response pair.

        Args:
            limn_expr: Limn expression

        Returns:
            Training example
        """
        # If expression has operators, split on them
        if '|' in limn_expr:
            parts = limn_expr.split('|')
            if len(parts) >= 2:
                query = parts[0].strip()
                response = parts[1].strip()

                return TrainingExample(
                    input=query,
                    output=response,
                    metadata={"type": "query-response"}
                )

        # For expressions with arrows
        if '→' in limn_expr:
            parts = limn_expr.split('→')
            if len(parts) >= 2:
                premise = parts[0].strip()
                conclusion = parts[1].strip()

                return TrainingExample(
                    input=premise,
                    output=conclusion,
                    metadata={"type": "implication"}
                )

        return None

    def create_context_examples(self, expressions: List[str]) -> List[TrainingExample]:
        """Create examples with context from multiple expressions.

        Args:
            expressions: List of related expressions

        Returns:
            Training examples
        """
        examples = []

        # Create sequences
        for i in range(len(expressions) - 1):
            context = expressions[i]
            next_expr = expressions[i + 1]

            example = TrainingExample(
                input=f"ctx {context} | qry nex",  # context [...] | query next
                output=next_expr,
                metadata={"type": "context-prediction"}
            )
            examples.append(example)

        return examples

    def format_corpus(self, corpus_path: str) -> List[TrainingExample]:
        """Format raw corpus into training examples.

        Args:
            corpus_path: Path to raw corpus JSON

        Returns:
            List of training examples
        """
        print(f"Loading corpus from {corpus_path}...")

        with open(corpus_path, 'r') as f:
            data = json.load(f)

        expressions = data['expressions']
        print(f"Loaded {len(expressions)} expressions")

        training_examples = []

        # Group by source file for context
        by_file = {}
        for expr in expressions:
            file_path = expr['source_file']
            if file_path not in by_file:
                by_file[file_path] = []
            by_file[file_path].append(expr)

        # Process each file's expressions
        for file_path, exprs in by_file.items():
            for expr_data in exprs:
                content = expr_data['content']
                context = expr_data.get('context', '')

                # Skip if not pure Limn
                if not self.is_pure_limn(content):
                    continue

                # Create interpretation pair
                interp = self.create_interpretation_pair(content, context)
                training_examples.append(interp)

                # Create completion pairs
                completions = self.create_completion_pair(content)
                training_examples.extend(completions)

                # Create query-response if applicable
                qr = self.create_query_response_pair(content)
                if qr:
                    training_examples.append(qr)

            # Create context examples from sequences
            file_contents = [e['content'] for e in exprs if self.is_pure_limn(e['content'])]
            if len(file_contents) > 1:
                context_ex = self.create_context_examples(file_contents)
                training_examples.extend(context_ex)

        print(f"Generated {len(training_examples)} training examples")

        return training_examples

    def save_dataset(self, examples: List[TrainingExample], output_path: str):
        """Save training examples in JSONL format.

        Args:
            examples: Training examples
            output_path: Output file path
        """
        print(f"Saving dataset to {output_path}...")

        with open(output_path, 'w', encoding='utf-8') as f:
            for example in examples:
                data = {
                    "input": example.input,
                    "output": example.output
                }
                if example.metadata:
                    data["metadata"] = example.metadata

                f.write(json.dumps(data, ensure_ascii=False) + '\n')

        print(f"Saved {len(examples)} examples")

    def get_statistics(self, examples: List[TrainingExample]) -> Dict:
        """Get dataset statistics.

        Args:
            examples: Training examples

        Returns:
            Statistics dictionary
        """
        stats = {
            "total_examples": len(examples),
            "by_type": {},
            "avg_input_length": 0,
            "avg_output_length": 0,
            "limn_purity": 0
        }

        # Count by type
        for ex in examples:
            if ex.metadata and 'type' in ex.metadata:
                ex_type = ex.metadata['type']
                stats["by_type"][ex_type] = stats["by_type"].get(ex_type, 0) + 1

        # Calculate averages
        total_input_len = sum(len(ex.input.split()) for ex in examples)
        total_output_len = sum(len(ex.output.split()) for ex in examples)

        stats["avg_input_length"] = total_input_len / len(examples) if examples else 0
        stats["avg_output_length"] = total_output_len / len(examples) if examples else 0

        # Check purity
        pure_count = sum(1 for ex in examples if self.is_pure_limn(ex.input) and self.is_pure_limn(ex.output))
        stats["limn_purity"] = pure_count / len(examples) if examples else 0

        return stats


def main():
    """Main formatting process."""
    formatter = DatasetFormatter()

    # Format the extracted corpus
    corpus_path = Path(__file__).parent / "raw_corpus.json"
    examples = formatter.format_corpus(str(corpus_path))

    # Save formatted dataset
    output_path = Path(__file__).parent / "limn_finetune_dataset.jsonl"
    formatter.save_dataset(examples, str(output_path))

    # Print statistics
    stats = formatter.get_statistics(examples)
    print("\n=== Dataset Statistics ===")
    print(f"Total examples: {stats['total_examples']}")
    print(f"Average input length: {stats['avg_input_length']:.1f} words")
    print(f"Average output length: {stats['avg_output_length']:.1f} words")
    print(f"Limn purity: {stats['limn_purity']:.1%}")
    print("\nBy type:")
    for ex_type, count in sorted(stats['by_type'].items()):
        print(f"  {ex_type}: {count}")


if __name__ == "__main__":
    main()
