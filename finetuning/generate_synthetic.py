#!/usr/bin/env python3
"""
Generate Synthetic Limn Training Data
======================================

Generate synthetic Limn expressions using:
1. Template-based generation with vocabulary
2. Compositional combinations
3. Reasoning chain generation

Author: Polecat now
Date: 2026-02-02
"""

import json
import random
import itertools
from pathlib import Path
from typing import List, Dict, Tuple, Set
from dataclasses import dataclass, asdict


@dataclass
class SyntheticExample:
    """A synthetic training example."""
    input: str
    output: str
    metadata: Dict


class SyntheticGenerator:
    """Generate synthetic Limn training data."""

    def __init__(self):
        """Initialize generator."""
        # Load Limn vocabulary
        self.vocabulary = self._load_vocabulary()
        self.operators = ['∎', '~', '∿', '|', '→', '←', '↔']

        # Templates for different types of expressions
        self.templates = self._init_templates()

    def _load_vocabulary(self) -> Dict[str, List[str]]:
        """Load Limn vocabulary organized by semantic domain."""
        # Core vocabulary categories (from bootstrap and specs)
        vocab = {
            # Core concepts
            "existence": ["ess", "rea", "exi", "bei", "sel"],
            "awareness": ["awa", "kno", "per", "sen", "con"],
            "thinking": ["thi", "rea", "und", "int", "con"],
            "feeling": ["fel", "emo", "lov", "hat", "hop"],
            "action": ["act", "do", "mak", "cre", "des"],

            # States
            "states": ["sta", "cha", "gro", "flu", "fix"],
            "qualities": ["goo", "bad", "bri", "dar", "str"],
            "time": ["tim", "pas", "pre", "fut", "now"],
            "space": ["spa", "her", "the", "nea", "far"],

            # Logic and reasoning
            "logic": ["tru", "fal", "pos", "imp", "nec"],
            "query": ["qry", "wh", "why", "how", "ask"],
            "relation": ["sa", "dif", "sim", "bet", "con"],

            # Operators
            "grounding": ["∎"],
            "oracle": ["~"],
            "temporal": ["∿"],
            "composition": ["|"],
            "implication": ["→", "←", "↔"],

            # Common words
            "common": ["and", "or", "not", "all", "som", "non"],

            # Objects and entities
            "entities": ["thi", "tha", "yo", "we", "obj"],
            "nature": ["sun", "moo", "sta", "tre", "riv"],
            "life": ["lif", "dea", "bir", "gro", "end"],

            # Abstract
            "abstract": ["mea", "val", "ide", "con", "tho"],
            "process": ["pro", "cha", "tra", "bec", "eme"],
        }

        return vocab

    def _init_templates(self) -> List[Dict]:
        """Initialize templates for generating expressions."""
        return [
            # Basic grounding
            {
                "pattern": "{word} ∎ {word}",
                "type": "grounding",
                "categories": ["existence", "awareness", "states"],
            },
            # Oracle queries
            {
                "pattern": "~ qry {word}",
                "type": "oracle_query",
                "categories": ["query", "abstract"],
            },
            # Temporal
            {
                "pattern": "∿ {word} {word}",
                "type": "temporal",
                "categories": ["time", "process", "states"],
            },
            # Composition
            {
                "pattern": "{word} {word} | {word} {word}",
                "type": "composition",
                "categories": ["existence", "awareness", "thinking"],
            },
            # Implication
            {
                "pattern": "{word} → {word}",
                "type": "implication",
                "categories": ["logic", "relation", "process"],
            },
            # Complex reasoning
            {
                "pattern": "{word} ∎ {word} | qry {word}",
                "type": "grounded_query",
                "categories": ["existence", "query", "thinking"],
            },
            # Metacognitive
            {
                "pattern": "thi {word} | mea {word}",
                "type": "thought_meaning",
                "categories": ["thinking", "abstract"],
            },
        ]

    def get_words_from_categories(self, categories: List[str], count: int = 1) -> List[str]:
        """Get random words from vocabulary categories.

        Args:
            categories: List of category names
            count: Number of words to get

        Returns:
            List of words
        """
        words = []
        for _ in range(count):
            category = random.choice(categories)
            if category in self.vocabulary:
                words.append(random.choice(self.vocabulary[category]))
        return words

    def generate_from_template(self, template: Dict, count: int = 1) -> List[SyntheticExample]:
        """Generate examples from a template.

        Args:
            template: Template dictionary
            count: Number of examples to generate

        Returns:
            List of synthetic examples
        """
        examples = []
        pattern = template["pattern"]
        categories = template["categories"]
        ex_type = template["type"]

        for _ in range(count):
            # Count placeholders
            placeholder_count = pattern.count("{word}")

            # Get words
            words = self.get_words_from_categories(categories, placeholder_count)

            # Fill template
            expr = pattern
            for word in words:
                expr = expr.replace("{word}", word, 1)

            # Create training pair based on type
            if ex_type in ["grounding", "temporal", "composition"]:
                # Input: expression itself, Output: elaborated form
                example = SyntheticExample(
                    input=expr,
                    output=expr,  # Self-supervised
                    metadata={"type": ex_type, "template": True}
                )
            elif ex_type == "oracle_query":
                # Input: query, Output: expression about query
                parts = expr.split("qry ")
                if len(parts) == 2:
                    query_word = parts[1].strip()
                    example = SyntheticExample(
                        input=expr,
                        output=f"{query_word} ∎ ans",  # query grounds answer
                        metadata={"type": ex_type, "template": True}
                    )
                else:
                    continue
            elif ex_type == "implication":
                # Input: premise, Output: conclusion
                parts = expr.split("→")
                if len(parts) == 2:
                    example = SyntheticExample(
                        input=parts[0].strip(),
                        output=parts[1].strip(),
                        metadata={"type": ex_type, "template": True}
                    )
                else:
                    continue
            else:
                # Default: self-supervised
                example = SyntheticExample(
                    input=expr,
                    output=expr,
                    metadata={"type": ex_type, "template": True}
                )

            examples.append(example)

        return examples

    def generate_reasoning_chains(self, count: int = 100) -> List[SyntheticExample]:
        """Generate reasoning chain examples.

        Args:
            count: Number of chains to generate

        Returns:
            List of examples
        """
        examples = []

        reasoning_templates = [
            # Grounding → Understanding
            "{a} ∎ {b} | {b} → und | und gro",
            # Query → Answer → Knowledge
            "qry {a} | {a} ∎ {b} | kno {b}",
            # Thinking → Meaning → Truth
            "thi {a} | mea {b} | {b} → tru",
            # Awareness → Consciousness → Self
            "awa {a} | con {a} | sel awa",
            # Process → Change → State
            "pro {a} | cha {b} | sta {b}",
        ]

        for _ in range(count):
            template = random.choice(reasoning_templates)

            # Get words
            a_word = random.choice(self.get_words_from_categories(["existence", "awareness", "thinking"], 1))
            b_word = random.choice(self.get_words_from_categories(["abstract", "states", "qualities"], 1))

            chain = template.replace("{a}", a_word).replace("{b}", b_word)

            # Split chain into steps
            steps = chain.split("|")
            if len(steps) >= 2:
                # Create examples for each step
                for i in range(len(steps) - 1):
                    example = SyntheticExample(
                        input=steps[i].strip(),
                        output=steps[i+1].strip(),
                        metadata={"type": "reasoning_chain", "chain_length": len(steps)}
                    )
                    examples.append(example)

        return examples

    def generate_vocabulary_pairs(self) -> List[SyntheticExample]:
        """Generate examples pairing related vocabulary words.

        Returns:
            List of examples
        """
        examples = []

        # Create pairs within categories
        for category, words in self.vocabulary.items():
            if len(words) < 2:
                continue

            # Create various combinations
            for word1, word2 in itertools.combinations(words, 2):
                # Similarity pairs
                examples.append(SyntheticExample(
                    input=f"{word1} sim {word2}",
                    output=f"{word1} sa {word2}",
                    metadata={"type": "vocabulary_relation", "relation": "similarity"}
                ))

                # Composition pairs
                examples.append(SyntheticExample(
                    input=f"{word1} | {word2}",
                    output=f"{word1} joi {word2}",
                    metadata={"type": "vocabulary_relation", "relation": "composition"}
                ))

                # Limit pairs per category
                if len([e for e in examples if e.metadata.get("relation") == "similarity"]) > 50:
                    break

        return examples

    def generate_instruction_following(self, count: int = 500) -> List[SyntheticExample]:
        """Generate instruction-following examples.

        Args:
            count: Number to generate

        Returns:
            List of examples
        """
        examples = []

        instruction_templates = [
            # Define X
            ("def {word}", "{word} ∎ mea"),
            # Explain X
            ("exp {word}", "{word} → und"),
            # Query X
            ("qry {word}", "~ qry {word} ess"),
            # Interpret X
            ("int {word}", "{word} ∎ int"),
            # Think about X
            ("thi {word}", "thi {word} | mea eme"),
            # Create X
            ("cre {word}", "{word} ∎ cre"),
        ]

        for _ in range(count):
            template_input, template_output = random.choice(instruction_templates)

            # Get word
            word = random.choice(self.get_words_from_categories(
                ["existence", "awareness", "thinking", "abstract"], 1
            ))

            input_text = template_input.replace("{word}", word)
            output_text = template_output.replace("{word}", word)

            examples.append(SyntheticExample(
                input=input_text,
                output=output_text,
                metadata={"type": "instruction_following"}
            ))

        return examples

    def generate_all(self, target_count: int = 50000) -> List[SyntheticExample]:
        """Generate synthetic data up to target count.

        Args:
            target_count: Target number of examples

        Returns:
            List of all synthetic examples
        """
        print(f"Generating synthetic data (target: {target_count})...")

        all_examples = []

        # Generate from templates
        print("Generating from templates...")
        template_examples_per = target_count // (len(self.templates) * 3)
        for template in self.templates:
            examples = self.generate_from_template(template, template_examples_per)
            all_examples.extend(examples)
            print(f"  {template['type']}: {len(examples)} examples")

        # Generate reasoning chains
        print("Generating reasoning chains...")
        chains = self.generate_reasoning_chains(target_count // 10)
        all_examples.extend(chains)
        print(f"  Reasoning chains: {len(chains)} examples")

        # Generate vocabulary pairs
        print("Generating vocabulary pairs...")
        vocab_pairs = self.generate_vocabulary_pairs()
        all_examples.extend(vocab_pairs)
        print(f"  Vocabulary pairs: {len(vocab_pairs)} examples")

        # Generate instruction following
        print("Generating instruction following...")
        instructions = self.generate_instruction_following(target_count // 20)
        all_examples.extend(instructions)
        print(f"  Instructions: {len(instructions)} examples")

        # Shuffle
        random.shuffle(all_examples)

        # Limit to target
        if len(all_examples) > target_count:
            all_examples = all_examples[:target_count]

        print(f"\nTotal generated: {len(all_examples)} examples")

        return all_examples

    def save_synthetic(self, examples: List[SyntheticExample], output_path: str):
        """Save synthetic examples to JSONL.

        Args:
            examples: Synthetic examples
            output_path: Output file path
        """
        print(f"Saving to {output_path}...")

        with open(output_path, 'w', encoding='utf-8') as f:
            for example in examples:
                data = {
                    "input": example.input,
                    "output": example.output,
                    "metadata": example.metadata
                }
                f.write(json.dumps(data, ensure_ascii=False) + '\n')

        print(f"Saved {len(examples)} synthetic examples")


def main():
    """Generate synthetic dataset."""
    generator = SyntheticGenerator()

    # Generate synthetic data
    synthetic_examples = generator.generate_all(target_count=40000)

    # Save
    output_path = Path(__file__).parent / "limn_synthetic_dataset.jsonl"
    generator.save_synthetic(synthetic_examples, str(output_path))

    # Statistics
    print("\n=== Synthetic Dataset Statistics ===")
    print(f"Total examples: {len(synthetic_examples)}")

    # Count by type
    type_counts = {}
    for ex in synthetic_examples:
        ex_type = ex.metadata.get("type", "unknown")
        type_counts[ex_type] = type_counts.get(ex_type, 0) + 1

    print("\nBy type:")
    for ex_type, count in sorted(type_counts.items()):
        print(f"  {ex_type}: {count}")


if __name__ == "__main__":
    main()
