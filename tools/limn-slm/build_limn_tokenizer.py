#!/usr/bin/env python3
"""
Build a custom Limn tokenizer where every vocabulary word = 1 token.

v5 Phase 2.1: Addresses H3 (BPE fragments Limn) and H4 (enables
grammar-constrained decoding).

Token classes:
  - 2005 Limn vocabulary words from Dolt DB
  - 6 compositional operators: @ * ^ \\ ± :
  - Structural markers: | → = ! . + - \\n
  - Gradient values: 0.0, 0.1, ..., 1.0 (quantized)
  - Digits: 0-9 (for non-quantized values)
  - Special: <pad> <bos> <eos> <unk> <sep> <space>
  - Proper noun character fallback: individual letters A-Z

Output: limn_tokenizer/ directory with HuggingFace-compatible files.

— Lex
"""

import json
import os
import subprocess
import sys

from tokenizers import Tokenizer, Regex, pre_tokenizers, models, trainers
from tokenizers.processors import TemplateProcessing
from transformers import PreTrainedTokenizerFast

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
OUTPUT_DIR = os.path.join(SCRIPT_DIR, "limn_tokenizer")
DOLT_PATH = "/home/eric/src/limntown/limn/refinery/rig/data/vocabulary"


def get_dolt_vocabulary():
    """Extract all vocabulary words from Dolt DB."""
    try:
        result = subprocess.run(
            ["dolt", "sql", "-q", "SELECT word FROM words ORDER BY word", "-r", "csv"],
            cwd=DOLT_PATH, capture_output=True, text=True, timeout=10
        )
        if result.returncode == 0:
            words = [w.strip() for w in result.stdout.strip().split('\n')[1:] if w.strip()]
            return words
    except (subprocess.TimeoutExpired, FileNotFoundError):
        pass

    # Fallback: try to read from exported file
    fallback = os.path.join(SCRIPT_DIR, "data", "v5", "held_out_words.json")
    if os.path.exists(fallback):
        with open(fallback) as f:
            data = json.load(f)
            if "all_words" in data:
                return data["all_words"]

    print("ERROR: Cannot access Dolt vocabulary. Run from limn repo root.")
    sys.exit(1)


def build_tokenizer():
    """Build a word-level Limn tokenizer."""
    print("Building Limn Tokenizer")
    print("=" * 60)

    # Get vocabulary
    print("Loading Dolt vocabulary...")
    vocab_words = get_dolt_vocabulary()
    print(f"  {len(vocab_words)} vocabulary words")

    # Define token classes
    special_tokens = ["<pad>", "<unk>", "<bos>", "<eos>", "<sep>", "<space>"]

    operators = ["@", "*", "^", "\\", "±", ":"]

    structural = ["|", "→", "=", "!", ".", "+", "-", "\n", '"']

    # Gradient values at 0.01 resolution (for ^0.7, ^0.99 etc.)
    gradients = [f"{i/100:.2f}" for i in range(101)]  # 0.00 to 1.00
    # Also add single-decimal forms (0.0, 0.1, ..., 1.0)
    gradients += [f"{i/10:.1f}" for i in range(11)]

    # Individual digits for numeric values
    digits = [str(d) for d in range(10)]

    # Upper-case letters for proper nouns (character-level fallback)
    uppercase = [chr(c) for c in range(ord('A'), ord('Z') + 1)]

    # HGttG-specific words not in Dolt (common function words)
    # These appear frequently in translations but haven't been added to vocab DB
    hgttg_extra = [
        "nu", "al", "sa", "te", "on", "we", "yo", "of", "ma",
        "pani", "ex", "sprl", "mi", "eq", "bil",
    ]

    # Build vocabulary mapping
    vocab = {}
    idx = 0

    # Special tokens first
    for tok in special_tokens:
        vocab[tok] = idx
        idx += 1

    # Operators
    for tok in operators:
        vocab[tok] = idx
        idx += 1

    # Structural markers
    for tok in structural:
        if tok not in vocab:
            vocab[tok] = idx
            idx += 1

    # Gradient values
    for tok in gradients:
        vocab[tok] = idx
        idx += 1

    # Digits
    for tok in digits:
        if tok not in vocab:
            vocab[tok] = idx
            idx += 1

    # Uppercase letters
    for tok in uppercase:
        vocab[tok] = idx
        idx += 1

    # HGttG extra words
    for word in hgttg_extra:
        if word not in vocab:
            vocab[word] = idx
            idx += 1

    # Vocabulary words (the main content)
    for word in vocab_words:
        if word not in vocab:
            vocab[word] = idx
            idx += 1

    print(f"  Total tokens: {idx}")
    print(f"    Special: {len(special_tokens)}")
    print(f"    Operators: {len(operators)}")
    print(f"    Structural: {len(structural)}")
    print(f"    Gradients: {len(set(gradients))}")
    print(f"    Digits: {len(digits)}")
    print(f"    Uppercase: {len(uppercase)}")
    print(f"    HGttG extra: {len(hgttg_extra)}")
    print(f"    Vocabulary: {len(vocab_words)}")

    # Create tokenizer using WordLevel model
    tokenizer = Tokenizer(models.WordLevel(vocab=vocab, unk_token="<unk>"))

    # Pre-tokenizer: split on whitespace and special characters
    # This ensures "lov@fea" becomes ["lov", "@", "fea"]
    # and "kno^0.7" becomes ["kno", "^", "0.7"]
    # Note: Must use Regex() object, not raw string (raw string is literal match)
    tokenizer.pre_tokenizer = pre_tokenizers.Sequence([
        pre_tokenizers.WhitespaceSplit(),
        pre_tokenizers.Split(
            Regex(r'[@*\^\\±:|→=!\+\-\n"]'),
            behavior="isolated",
        ),
    ])

    # Post-processor: add BOS/EOS
    tokenizer.post_processor = TemplateProcessing(
        single="<bos> $A <eos>",
        pair="<bos> $A <sep> $B:1 <eos>",
        special_tokens=[
            ("<bos>", vocab["<bos>"]),
            ("<eos>", vocab["<eos>"]),
            ("<sep>", vocab["<sep>"]),
        ],
    )

    # Save raw tokenizer
    os.makedirs(OUTPUT_DIR, exist_ok=True)
    tokenizer_path = os.path.join(OUTPUT_DIR, "tokenizer.json")
    tokenizer.save(tokenizer_path)

    # Wrap as HuggingFace PreTrainedTokenizerFast
    hf_tokenizer = PreTrainedTokenizerFast(
        tokenizer_object=tokenizer,
        unk_token="<unk>",
        pad_token="<pad>",
        bos_token="<bos>",
        eos_token="<eos>",
        sep_token="<sep>",
    )

    # Save HuggingFace format
    hf_tokenizer.save_pretrained(OUTPUT_DIR)

    print(f"\n  Tokenizer saved to: {OUTPUT_DIR}")

    return hf_tokenizer, vocab


def test_tokenizer(tokenizer):
    """Test the tokenizer on various Limn inputs."""
    print(f"\n{'='*60}")
    print(f"  TOKENIZER TESTS")
    print(f"{'='*60}")

    test_cases = [
        # Single words
        ("lov", "single word"),
        ("fea", "single word"),
        ("kno", "single word"),

        # Operators
        ("lov@fea", "projection operator"),
        ("joy*sad", "interference operator"),
        ("kno^0.7", "gradient operator"),
        ("str\\fea", "subtraction operator"),
        ("lov±fea", "superposition operator"),
        ("hop:cha", "conditional operator"),

        # Constraint groups
        ("lov@fea | kno^0.7 | hop:cha", "pipe-separated groups"),

        # HGttG example
        ("far^0.9 rmt | fas^0.1 end wes sprl arm glx | sma nu-rgd yel sun", "HGttG ch1.1"),

        # With arrow
        ("lov@fea → hop gro", "arrow expression"),

        # Proper nouns
        ("Arthur Dent wke | bra*hot^0.9", "with proper nouns"),

        # Complex
        ("wrl hav±hav^0 : mst hum lif on wrl | joy^0 @ al", "HGttG ch1.3"),
    ]

    all_passed = True
    for text, desc in test_cases:
        encoded = tokenizer.encode(text)
        decoded = tokenizer.decode(encoded, skip_special_tokens=True)
        tokens = tokenizer.convert_ids_to_tokens(encoded)

        # Check roundtrip
        # Note: spaces won't be perfectly preserved with word-level tokenizer
        clean_text = text.strip()
        clean_decoded = decoded.strip()

        print(f"\n  [{desc}]")
        print(f"    Input:   {text}")
        print(f"    Tokens:  {tokens}")
        print(f"    IDs:     {encoded}")
        print(f"    Decoded: {decoded}")

        # Check no <unk> tokens (except for proper nouns)
        unk_count = tokens.count("<unk>")
        if unk_count > 0:
            # Check if unks are only for proper nouns (uppercase)
            unk_positions = [i for i, t in enumerate(tokens) if t == "<unk>"]
            print(f"    WARNING: {unk_count} <unk> tokens at positions {unk_positions}")

    # Vocabulary coverage test
    print(f"\n{'='*60}")
    print(f"  VOCABULARY COVERAGE")
    print(f"{'='*60}")

    # Test all operators tokenize as single tokens
    operators = ["@", "*", "^", "\\", "±", ":"]
    for op in operators:
        tok_id = tokenizer.convert_tokens_to_ids(op)
        is_unk = tok_id == tokenizer.unk_token_id
        status = "FAIL" if is_unk else "OK"
        print(f"  Operator {op:3s}: token_id={tok_id:5d}  [{status}]")

    # Test sample vocabulary words
    sample_words = ["lov", "fea", "joy", "kno", "gro", "sun", "spa", "lif",
                    "dep", "hop", "wis", "tru", "sad", "str", "lig", "dar"]
    for word in sample_words:
        tok_id = tokenizer.convert_tokens_to_ids(word)
        is_unk = tok_id == tokenizer.unk_token_id
        status = "FAIL" if is_unk else "OK"
        print(f"  Word {word:5s}: token_id={tok_id:5d}  [{status}]")

    # Summary stats
    print(f"\n  Vocab size: {tokenizer.vocab_size}")
    print(f"  Pad token:  {tokenizer.pad_token} (id={tokenizer.pad_token_id})")
    print(f"  UNK token:  {tokenizer.unk_token} (id={tokenizer.unk_token_id})")
    print(f"  BOS token:  {tokenizer.bos_token} (id={tokenizer.bos_token_id})")
    print(f"  EOS token:  {tokenizer.eos_token} (id={tokenizer.eos_token_id})")


def main():
    tokenizer, vocab = build_tokenizer()
    test_tokenizer(tokenizer)

    # Save vocab mapping for reference
    vocab_path = os.path.join(OUTPUT_DIR, "vocab_mapping.json")
    with open(vocab_path, 'w') as f:
        json.dump({
            "total_tokens": len(vocab),
            "special_tokens": ["<pad>", "<unk>", "<bos>", "<eos>", "<sep>", "<space>"],
            "operators": ["@", "*", "^", "\\", "±", ":"],
            "structural": ["|", "→", "=", "!", ".", "+", "-"],
            "gradient_values": [f"{i/10:.1f}" for i in range(11)],
        }, f, indent=2)

    print(f"\n  Vocab mapping saved to: {vocab_path}")
    print(f"\n  Done.")


if __name__ == "__main__":
    main()
