#!/usr/bin/env python3
"""
Extract Limn Corpus from Repository
====================================

This script extracts all Limn expressions from:
1. .limn and .lmn files
2. Documentation and specs
3. Example files

Filters out comments and English text to create a pure Limn corpus.

Author: Polecat now
Date: 2026-02-02
"""

import os
import re
import json
from pathlib import Path
from typing import List, Dict, Set
from dataclasses import dataclass, asdict


@dataclass
class LimnExpression:
    """A single Limn expression with metadata."""
    content: str
    source_file: str
    line_number: int
    context: str = ""  # Optional context from comments


class LimnCorpusExtractor:
    """Extract and process Limn expressions from the repository."""

    def __init__(self, repo_root: str):
        """Initialize extractor.

        Args:
            repo_root: Root directory of the repository
        """
        self.repo_root = Path(repo_root)
        self.expressions: List[LimnExpression] = []
        self.limn_vocab: Set[str] = self._load_limn_vocabulary()

    def _load_limn_vocabulary(self) -> Set[str]:
        """Load Limn vocabulary from the database or docs."""
        # For now, use a basic pattern-based approach
        # TODO: Load from Dolt database for complete validation
        vocab = set()

        # Try to load from vocabulary docs
        vocab_file = self.repo_root / "docs" / "spec" / "vocabulary-v3-natural.md"
        if vocab_file.exists():
            try:
                with open(vocab_file, 'r') as f:
                    content = f.read()
                    # Extract Limn words (simple pattern: 2-3 letter words)
                    words = re.findall(r'\b[a-z]{2,3}\b', content)
                    vocab.update(words)
            except Exception as e:
                print(f"Warning: Could not load vocabulary: {e}")

        return vocab

    def is_limn_expression(self, line: str) -> bool:
        """Check if a line contains Limn (not just English comments).

        Args:
            line: Line to check

        Returns:
            True if line appears to be Limn
        """
        # Remove comments
        line = re.sub(r'#.*$', '', line).strip()

        if not line:
            return False

        # Limn has specific operators
        limn_operators = ['∎', '~', '∿', '|', '→', '←', '↔']
        if any(op in line for op in limn_operators):
            return True

        # Check for patterns of short words (Limn vocab is mostly 2-3 letters)
        words = line.split()
        if not words:
            return False

        # If most words are short and lowercase, likely Limn
        short_words = [w for w in words if len(w) <= 4 and w.islower() and w.isalpha()]
        ratio = len(short_words) / len(words)

        return ratio > 0.6  # At least 60% short words

    def extract_from_file(self, file_path: Path) -> List[LimnExpression]:
        """Extract Limn expressions from a single file.

        Args:
            file_path: Path to file

        Returns:
            List of extracted expressions
        """
        expressions = []

        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                lines = f.readlines()

            context = ""
            for i, line in enumerate(lines, 1):
                # Extract context from comments
                if line.strip().startswith('#'):
                    context = line.strip().lstrip('#').strip()
                    continue

                # Check if line has Limn
                if self.is_limn_expression(line):
                    # Clean the expression
                    clean_line = re.sub(r'#.*$', '', line).strip()

                    if clean_line:
                        expr = LimnExpression(
                            content=clean_line,
                            source_file=str(file_path.relative_to(self.repo_root)),
                            line_number=i,
                            context=context
                        )
                        expressions.append(expr)
                        context = ""  # Reset context after use

        except Exception as e:
            print(f"Error reading {file_path}: {e}")

        return expressions

    def extract_from_markdown(self, file_path: Path) -> List[LimnExpression]:
        """Extract Limn expressions from markdown code blocks.

        Args:
            file_path: Path to markdown file

        Returns:
            List of extracted expressions
        """
        expressions = []

        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()

            # Find code blocks (both ``` and backtick-quoted)
            # Look for Limn in code blocks
            in_code_block = False
            code_lines = []
            line_num = 0

            for line in content.split('\n'):
                line_num += 1

                if line.strip().startswith('```'):
                    if in_code_block:
                        # End of code block - process accumulated lines
                        for code_line in code_lines:
                            if self.is_limn_expression(code_line):
                                expr = LimnExpression(
                                    content=code_line.strip(),
                                    source_file=str(file_path.relative_to(self.repo_root)),
                                    line_number=line_num,
                                    context=""
                                )
                                expressions.append(expr)
                        code_lines = []
                        in_code_block = False
                    else:
                        in_code_block = True
                elif in_code_block:
                    code_lines.append(line)

            # Also look for inline backtick Limn (e.g., `sel awa ∎`)
            inline_limn = re.findall(r'`([^`]+)`', content)
            for expr in inline_limn:
                if self.is_limn_expression(expr):
                    expressions.append(LimnExpression(
                        content=expr.strip(),
                        source_file=str(file_path.relative_to(self.repo_root)),
                        line_number=0,  # Line number not available for inline
                        context="inline"
                    ))

        except Exception as e:
            print(f"Error reading markdown {file_path}: {e}")

        return expressions

    def extract_all(self) -> List[LimnExpression]:
        """Extract all Limn expressions from the repository.

        Returns:
            List of all extracted expressions
        """
        print("Extracting Limn corpus...")

        # Find all .limn and .lmn files
        limn_files = list(self.repo_root.glob('**/*.limn'))
        limn_files.extend(self.repo_root.glob('**/*.lmn'))

        print(f"Found {len(limn_files)} .limn/.lmn files")

        for file_path in limn_files:
            if '.git' in str(file_path) or 'node_modules' in str(file_path):
                continue
            exprs = self.extract_from_file(file_path)
            self.expressions.extend(exprs)
            print(f"  {file_path.name}: {len(exprs)} expressions")

        # Find markdown files in docs/ and examples/
        md_paths = [
            self.repo_root / 'docs',
            self.repo_root / 'experiments',
        ]

        md_files = []
        for path in md_paths:
            if path.exists():
                md_files.extend(path.glob('**/*.md'))

        print(f"\nFound {len(md_files)} markdown files")

        for file_path in md_files:
            exprs = self.extract_from_markdown(file_path)
            if exprs:
                self.expressions.extend(exprs)
                print(f"  {file_path.name}: {len(exprs)} expressions")

        # Deduplicate expressions
        unique_exprs = {}
        for expr in self.expressions:
            key = expr.content.lower().strip()
            if key not in unique_exprs:
                unique_exprs[key] = expr

        self.expressions = list(unique_exprs.values())

        print(f"\nTotal unique expressions: {len(self.expressions)}")

        return self.expressions

    def save_corpus(self, output_path: str):
        """Save extracted corpus to JSON file.

        Args:
            output_path: Path to output JSON file
        """
        data = {
            "metadata": {
                "source": "limn repository",
                "extractor": "extract_corpus.py",
                "total_expressions": len(self.expressions)
            },
            "expressions": [asdict(expr) for expr in self.expressions]
        }

        with open(output_path, 'w', encoding='utf-8') as f:
            json.dump(data, f, indent=2, ensure_ascii=False)

        print(f"\nCorpus saved to: {output_path}")


def main():
    """Main extraction process."""
    import sys

    # Get repo root (parent of finetuning directory)
    repo_root = Path(__file__).parent.parent

    extractor = LimnCorpusExtractor(repo_root)
    expressions = extractor.extract_all()

    # Save to JSON
    output_path = Path(__file__).parent / "raw_corpus.json"
    extractor.save_corpus(output_path)

    # Print some statistics
    print("\n=== Corpus Statistics ===")
    print(f"Total expressions: {len(expressions)}")

    # Sample expressions
    print("\n=== Sample Expressions ===")
    for expr in expressions[:10]:
        print(f"{expr.content}")
        if expr.context:
            print(f"  Context: {expr.context}")
        print()


if __name__ == "__main__":
    main()
