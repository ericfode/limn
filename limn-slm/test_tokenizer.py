"""
Test suite for Limn tokenizer.

Validates vocabulary coverage, operator handling, and round-trip consistency.
"""

import sys
from tokenizer import LimnTokenizer


def test_special_tokens():
    """Test special token IDs are correct."""
    print("Testing special tokens...")
    tokenizer = LimnTokenizer()

    assert tokenizer.pad_token_id == 0
    assert tokenizer.bos_token_id == 1
    assert tokenizer.eos_token_id == 2
    assert tokenizer.unk_token_id == 3
    assert tokenizer.key_token_id == 4

    print("  ✓ Special token IDs correct")


def test_encoding():
    """Test basic encoding."""
    print("\nTesting encoding...")
    tokenizer = LimnTokenizer()

    # Simple text
    text = "sol liq gas"
    tokens = tokenizer.encode(text, add_special_tokens=False)

    assert len(tokens) == 3, f"Expected 3 tokens, got {len(tokens)}"
    assert all(t != tokenizer.unk_token_id for t in tokens), "Unexpected UNK token"

    print(f"  Input: '{text}'")
    print(f"  Tokens: {tokens}")
    print("  ✓ Basic encoding works")


def test_decoding():
    """Test encoding -> decoding round-trip."""
    print("\nTesting round-trip...")
    tokenizer = LimnTokenizer()

    test_cases = [
        "sol liq gas",
        "aqu pyr ter aer",
        "big sma lon sho",
    ]

    for text in test_cases:
        tokens = tokenizer.encode(text, add_special_tokens=False)
        decoded = tokenizer.decode(tokens)

        assert decoded == text, f"Round-trip failed: '{text}' -> '{decoded}'"

    print(f"  Tested {len(test_cases)} cases")
    print("  ✓ Round-trip consistency verified")


def test_special_token_handling():
    """Test special tokens in encoding/decoding."""
    print("\nTesting special token handling...")
    tokenizer = LimnTokenizer()

    text = "sol liq"

    # With special tokens
    tokens_with = tokenizer.encode(text, add_special_tokens=True)
    assert tokens_with[0] == tokenizer.bos_token_id
    assert tokens_with[-1] == tokenizer.eos_token_id

    # Decoding should skip them by default
    decoded = tokenizer.decode(tokens_with, skip_special_tokens=True)
    assert decoded == text

    # Decoding without skipping
    decoded_all = tokenizer.decode(tokens_with, skip_special_tokens=False)
    assert tokenizer.BOS_TOKEN in decoded_all
    assert tokenizer.EOS_TOKEN in decoded_all

    print("  ✓ Special token handling correct")


def test_operators():
    """Test operator tokenization."""
    print("\nTesting operators...")
    tokenizer = LimnTokenizer()

    test_cases = [
        ("sol | liq", 3),  # word OR word
        ("aqu -> pyr", 3),  # word IMPLIES word
        ("~ nox", 2),  # NOT word
        ("al mov", 2),  # ALL movement
    ]

    for text, expected_count in test_cases:
        tokens = tokenizer.encode(text, add_special_tokens=False)
        actual_count = len(tokens)

        assert actual_count == expected_count, \
            f"Expected {expected_count} tokens for '{text}', got {actual_count}"

        # Verify round-trip
        decoded = tokenizer.decode(tokens)
        assert set(text.split()) == set(decoded.split()), \
            f"Operator round-trip failed: '{text}' -> '{decoded}'"

    print(f"  Tested {len(test_cases)} operator cases")
    print("  ✓ Operator handling correct")


def test_key_encoding():
    """Test encoding with key."""
    print("\nTesting key encoding...")
    tokenizer = LimnTokenizer()

    key = "wh mea"
    text = "sol | liq"

    tokens = tokenizer.encode_with_key(text, key)

    # Should have: <bos> [key] <key> [text] <eos>
    assert tokens[0] == tokenizer.bos_token_id
    assert tokenizer.key_token_id in tokens
    assert tokens[-1] == tokenizer.eos_token_id

    # Key separator should be in middle
    key_sep_idx = tokens.index(tokenizer.key_token_id)
    assert 0 < key_sep_idx < len(tokens) - 1

    print(f"  Key: '{key}'")
    print(f"  Text: '{text}'")
    print(f"  Tokens: {tokens}")
    print(f"  Key separator at index: {key_sep_idx}")
    print("  ✓ Key encoding correct")


def test_unknown_words():
    """Test handling of unknown words."""
    print("\nTesting unknown word handling...")
    tokenizer = LimnTokenizer()

    # Made-up words not in Limn vocabulary
    text = "xyzabc qwerty"
    tokens = tokenizer.encode(text, add_special_tokens=False)

    # Should be UNK tokens
    assert all(t == tokenizer.unk_token_id for t in tokens), \
        "Unknown words should map to UNK"

    print("  ✓ Unknown words map to UNK")


def test_vocab_size():
    """Test vocabulary size is reasonable."""
    print("\nTesting vocabulary size...")
    tokenizer = LimnTokenizer()

    vocab_size = tokenizer.vocab_size

    # Should be ~1130 (1000 words + 23 ops + 5 special + some reserved)
    assert 1000 <= vocab_size <= 1200, \
        f"Unexpected vocab size: {vocab_size}"

    print(f"  Vocabulary size: {vocab_size}")
    print("  ✓ Vocab size reasonable")


def test_padding_truncation():
    """Test padding and truncation."""
    print("\nTesting padding and truncation...")
    tokenizer = LimnTokenizer()

    text = "sol liq gas pyr ter"

    # Test padding
    tokens_padded = tokenizer.encode(
        text,
        add_special_tokens=True,
        max_length=20,
        padding=True
    )
    assert len(tokens_padded) == 20
    assert tokens_padded[-1] == tokenizer.pad_token_id

    # Test truncation
    tokens_truncated = tokenizer.encode(
        text,
        add_special_tokens=True,
        max_length=5,
        truncation=True
    )
    assert len(tokens_truncated) == 5

    print("  ✓ Padding and truncation work")


def run_all_tests():
    """Run all tokenizer tests."""
    print("=" * 60)
    print("Limn Tokenizer Test Suite")
    print("=" * 60)

    try:
        test_special_tokens()
        test_encoding()
        test_decoding()
        test_special_token_handling()
        test_operators()
        test_key_encoding()
        test_unknown_words()
        test_vocab_size()
        test_padding_truncation()

        print("\n" + "=" * 60)
        print("✅ All tests passed!")
        print("=" * 60)
        return True

    except AssertionError as e:
        print("\n" + "=" * 60)
        print(f"❌ Test failed: {e}")
        print("=" * 60)
        return False
    except Exception as e:
        print("\n" + "=" * 60)
        print(f"❌ Error during testing: {e}")
        print("=" * 60)
        import traceback
        traceback.print_exc()
        return False


if __name__ == '__main__':
    success = run_all_tests()
    sys.exit(0 if success else 1)
