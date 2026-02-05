#!/usr/bin/env python3
"""
Prepare v3 training data for Limn SLM.

Adds to existing v2 data:
- 262 compositional expressions from Dolt
- 80 operator contrast pairs from Quinn
- 49 limn-in-limn definitions

Usage:
    python prepare_v3_data.py
"""

import json
import re
import subprocess
from pathlib import Path

DATA_DIR = Path(__file__).resolve().parent / "data"
LINGUIST_DATA = Path("/home/eric/src/limntown/limn/crew/linguist/data")
VOCAB_DB = Path("/home/eric/src/limntown/limn/refinery/rig/data/vocabulary")

SYSTEM_PROMPT = """You are a Limn language model. Limn is a constructed language where meaning emerges from constraint intersection. Every core word is exactly 3 letters (CVC: Consonant-Vowel-Consonant). Sentences use pipe | to separate constraint groups, and → for temporal/causal sequence.

Six compositional operators combine words:
@ projection: A@B = B-aspect of A (lov@fea = fear-aspect of love)
* interference: A*B = emergent blend (sol*liq = gel)
^ gradient: A^N = intensity 0.0-1.0 (big^0.7 = fairly big)
\\ subtraction: A\\B = A without B (lov\\fea = love without fear)
± superposition: A±B = quantum both/and (joy±fea = joyful-fear)
: conditional: A:B = A given B (lov:fth = love given faith)

You think, speak, and reason in Limn. Respond in Limn."""


def make_example(user_content: str, assistant_content: str) -> dict:
    """Create a training example in chat format."""
    return {
        "messages": [
            {"role": "system", "content": SYSTEM_PROMPT},
            {"role": "user", "content": user_content},
            {"role": "assistant", "content": assistant_content},
        ]
    }


def load_compositional_expressions() -> list:
    """Load 262 compositional expressions from Dolt."""
    result = subprocess.run(
        ["dolt", "sql", "-q",
         "SELECT expression, meaning, source FROM compositional_expressions",
         "-r", "json"],
        cwd=VOCAB_DB,
        capture_output=True,
        text=True
    )

    if result.returncode != 0:
        print(f"Error querying Dolt: {result.stderr}")
        return []

    rows = json.loads(result.stdout)["rows"]
    examples = []

    for row in rows:
        expr = row["expression"]
        meaning = row["meaning"]

        # Multiple question formats
        examples.append(make_example(
            f"mea: {expr}",
            f"{expr} = {meaning}"
        ))

        # Reverse: meaning to expression
        examples.append(make_example(
            f"lim: {meaning}",
            f"{meaning} → {expr}"
        ))

    print(f"  Loaded {len(examples)} examples from compositional_expressions")
    return examples


def load_operator_contrast_pairs() -> list:
    """Load 80 operator contrast pairs from Quinn."""
    contrast_file = LINGUIST_DATA / "operator_contrast_pairs.jsonl"

    if not contrast_file.exists():
        print(f"  Warning: {contrast_file} not found")
        return []

    examples = []
    with open(contrast_file) as f:
        for line in f:
            pair = json.loads(line)
            q = pair["q"]
            a = pair["a"]
            examples.append(make_example(q, a))

    print(f"  Loaded {len(examples)} operator contrast pairs")
    return examples


def load_limn_in_limn_definitions() -> list:
    """Load limn-in-limn definitions from markdown."""
    defs_file = LINGUIST_DATA / "limn-in-limn-definitions.md"

    if not defs_file.exists():
        print(f"  Warning: {defs_file} not found")
        return []

    examples = []
    content = defs_file.read_text()

    # Pattern: **word** (english)\n`word = definition`
    pattern = r'\*\*([a-z]{3})\*\*\s*\([^)]+\)\s*\n`([^`]+)`'
    matches = re.findall(pattern, content)

    for word, definition in matches:
        # Definition format: "word = limn definition"
        if "=" in definition:
            _, limn_def = definition.split("=", 1)
            limn_def = limn_def.strip()

            # Multiple formats
            examples.append(make_example(
                f"def: {word}",
                f"{word} = {limn_def}"
            ))

            examples.append(make_example(
                f"mea: {word}",
                limn_def
            ))

    print(f"  Loaded {len(examples)} limn-in-limn definitions")
    return examples


def main():
    print("=== Preparing v3 Training Data ===\n")

    # Load existing v2 data
    v2_train = DATA_DIR / "train.jsonl"
    v2_eval = DATA_DIR / "eval.jsonl"

    with open(v2_train) as f:
        train_examples = [json.loads(line) for line in f]
    print(f"Loaded {len(train_examples)} v2 training examples")

    with open(v2_eval) as f:
        eval_examples = [json.loads(line) for line in f]
    print(f"Loaded {len(eval_examples)} v2 eval examples")

    # Add new data
    print("\nAdding new data sources:")

    comp_expr = load_compositional_expressions()
    contrast_pairs = load_operator_contrast_pairs()
    limn_defs = load_limn_in_limn_definitions()

    # Combine
    all_new = comp_expr + contrast_pairs + limn_defs
    print(f"\nTotal new examples: {len(all_new)}")

    # Split 90/10 for train/eval
    import random
    random.seed(42)
    random.shuffle(all_new)

    split_idx = int(len(all_new) * 0.9)
    new_train = all_new[:split_idx]
    new_eval = all_new[split_idx:]

    # Combine with existing
    train_examples.extend(new_train)
    eval_examples.extend(new_eval)

    print(f"\nv3 totals:")
    print(f"  Training: {len(train_examples)} examples")
    print(f"  Eval: {len(eval_examples)} examples")

    # Save v3 data
    v3_train = DATA_DIR / "train_v3.jsonl"
    v3_eval = DATA_DIR / "eval_v3.jsonl"

    with open(v3_train, "w") as f:
        for ex in train_examples:
            f.write(json.dumps(ex) + "\n")

    with open(v3_eval, "w") as f:
        for ex in eval_examples:
            f.write(json.dumps(ex) + "\n")

    print(f"\nSaved to:")
    print(f"  {v3_train}")
    print(f"  {v3_eval}")

    # Also create symlinks for train.py to use
    print("\nTo use v3 data, rename or symlink:")
    print(f"  mv {DATA_DIR}/train.jsonl {DATA_DIR}/train_v2.jsonl")
    print(f"  mv {DATA_DIR}/eval.jsonl {DATA_DIR}/eval_v2.jsonl")
    print(f"  ln -s train_v3.jsonl {DATA_DIR}/train.jsonl")
    print(f"  ln -s eval_v3.jsonl {DATA_DIR}/eval.jsonl")


if __name__ == "__main__":
    main()
