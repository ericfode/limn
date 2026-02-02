"""
Vocabulary loader for Limn tokenizer.

Loads Limn vocabulary from Dolt database and exports to JSON.
"""

import json
import os
import subprocess
from typing import List, Dict


def load_words_from_dolt(dolt_db_path: str = '../data/vocabulary') -> List[str]:
    """
    Load all Limn words from Dolt database.

    Args:
        dolt_db_path: Path to Dolt database directory

    Returns:
        List of Limn words (sorted alphabetically)
    """
    if not os.path.exists(dolt_db_path):
        raise FileNotFoundError(f"Dolt database not found at {dolt_db_path}")

    try:
        # Use dolt SQL to query words
        result = subprocess.run(
            ['dolt', 'sql', '-q', 'SELECT word FROM words ORDER BY word ASC', '-r', 'csv'],
            cwd=dolt_db_path,
            capture_output=True,
            text=True,
            check=True
        )

        # Parse CSV output (skip header)
        lines = result.stdout.strip().split('\n')
        words = [line.strip() for line in lines[1:] if line.strip()]

        return sorted(words)

    except subprocess.CalledProcessError as e:
        print(f"Error querying Dolt: {e}")
        print(f"stderr: {e.stderr}")
        raise


def load_words_from_script() -> List[str]:
    """
    Fallback: Load words using vocab.sh script.

    Returns:
        List of Limn words
    """
    try:
        # Try using vocab.sh words command
        result = subprocess.run(
            ['../scripts/vocab.sh', 'words'],
            capture_output=True,
            text=True,
            check=True
        )

        words = [line.strip() for line in result.stdout.split('\n') if line.strip()]
        return sorted(words)

    except (subprocess.CalledProcessError, FileNotFoundError) as e:
        print(f"Error loading words from script: {e}")
        return []


def build_vocab_json(
    output_path: str = './tokenizer/vocab.json',
    dolt_db_path: str = '../data/vocabulary'
) -> Dict[str, int]:
    """
    Build complete vocabulary JSON file for tokenizer.

    Includes special tokens, Limn words, and operators.

    Args:
        output_path: Where to save vocab.json
        dolt_db_path: Path to Dolt database

    Returns:
        Complete vocab dictionary
    """
    # Special tokens (IDs 0-4)
    vocab = {
        "<pad>": 0,
        "<bos>": 1,
        "<eos>": 2,
        "<unk>": 3,
        "<key>": 4,
    }

    # Load Limn words
    print("Loading Limn words from Dolt...")
    try:
        words = load_words_from_dolt(dolt_db_path)
    except Exception as e:
        print(f"Failed to load from Dolt: {e}")
        print("Trying vocab.sh script...")
        words = load_words_from_script()

    if not words:
        print("Warning: No words loaded! Using minimal vocabulary.")
        words = []

    print(f"Loaded {len(words)} words")

    # Assign token IDs to words (starting at 5)
    token_id = 5
    for word in words:
        vocab[word] = token_id
        token_id += 1

    # Operators (IDs 1005+)
    operators = {
        '|': 1005,   # OR/superposition
        '.': 1006,   # AND/intersection
        '->': 1007,  # IMPLICATION
        '~': 1008,   # NOT
        '?': 1009,   # QUESTION
        '!': 1010,   # EMPHASIS
        '=': 1011,   # DEFINITION
        ':': 1012,   # KEY_SEP
        '(': 1013,   # LEFT_PAREN
        ')': 1014,   # RIGHT_PAREN
        '[': 1015,   # LEFT_BRACKET
        ']': 1016,   # RIGHT_BRACKET
        '{': 1017,   # LEFT_BRACE
        '}': 1018,   # RIGHT_BRACE
        '^': 1019,   # FOCUS
        '*': 1020,   # UNIVERSAL
        '@': 1021,   # CONTEXT
        '#': 1022,   # META
        'al': 1023,  # ALL quantifier
        'ex': 1024,  # EXISTS quantifier
        'on': 1025,  # ONE quantifier
        'no': 1026,  # NONE quantifier
        'wh': 1027,  # WHICH quantifier
    }

    vocab.update(operators)

    # Save to JSON
    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    with open(output_path, 'w') as f:
        json.dump(vocab, f, indent=2)

    print(f"Vocabulary saved to {output_path}")
    print(f"Total tokens: {len(vocab)}")

    return vocab


if __name__ == '__main__':
    import argparse

    parser = argparse.ArgumentParser(description='Export Limn vocabulary to JSON')
    parser.add_argument('--output', type=str, default='./tokenizer/vocab.json',
                       help='Output path for vocab.json')
    parser.add_argument('--dolt-db', type=str, default='../data/vocabulary',
                       help='Path to Dolt database')

    args = parser.parse_args()

    vocab = build_vocab_json(args.output, args.dolt_db)

    print("\nVocabulary stats:")
    print(f"  Special tokens: 5")
    print(f"  Limn words: {len([k for k in vocab.keys() if k not in ['<pad>', '<bos>', '<eos>', '<unk>', '<key>'] and vocab[k] < 1005])}")
    print(f"  Operators: {len([k for k in vocab.keys() if vocab[k] >= 1005])}")
    print(f"  Total: {len(vocab)}")
