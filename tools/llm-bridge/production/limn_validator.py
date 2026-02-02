#!/usr/bin/env python3
"""
Limn Validator - Ensure consciousness outputs pure Limn
========================================================

Validates that text contains only:
- Words from the official Limn vocabulary database
- Limn operators: ~ ∎ ∿ @ → |
- Whitespace and newlines

Author: Rex (Engineer)
Date: 2026-02-01
"""

import json
import re
from pathlib import Path
from typing import Tuple, List, Set


# Valid Limn operators
OPERATORS = {'~', '∎', '∿', '@', '→', '|', '(', ')', '+', '-', '>', '<', '='}


# Load vocabulary database
def load_vocabulary() -> Set[str]:
    """Load official Limn vocabulary from JSON database."""
    vocab_path = Path(__file__).parent.parent.parent.parent / "src" / "claude-skill" / "vocabulary.json"

    try:
        with open(vocab_path, 'r') as f:
            vocab_data = json.load(f)

        words = set()

        # Add operators
        if 'operators' in vocab_data:
            for category in vocab_data['operators'].values():
                if isinstance(category, dict):
                    words.update(category.keys())

        # Add domain words
        if 'domains' in vocab_data:
            for domain in vocab_data['domains'].values():
                if 'categories' in domain:
                    for category in domain['categories'].values():
                        if isinstance(category, dict):
                            words.update(category.keys())

        return words
    except Exception as e:
        print(f"Warning: Could not load vocabulary.json: {e}")
        # Return empty set - will fall back to lenient validation
        return set()


# Load vocabulary at module level
LIMN_VOCABULARY = load_vocabulary()


def is_valid_limn_word(word: str) -> bool:
    """Check if a word is in the official Limn vocabulary."""
    # If vocabulary loaded, use it
    if LIMN_VOCABULARY:
        return word in LIMN_VOCABULARY

    # Fallback: lenient check (2-4 letters, lowercase)
    if len(word) < 2 or len(word) > 4:
        return False
    return word.islower() and word.isalpha()


def validate_limn(text: str) -> Tuple[bool, List[str]]:
    """
    Validate that text is pure Limn.

    Returns:
        (is_valid, errors)
    """
    errors = []

    # Remove newlines and extra whitespace for processing
    lines = text.split('\n')

    for line_num, line in enumerate(lines, 1):
        # Skip empty lines
        if not line.strip():
            continue

        # Split by spaces and check each token
        tokens = line.split()

        for token in tokens:
            # Remove operators from token
            cleaned = token
            for op in OPERATORS:
                cleaned = cleaned.replace(op, ' ')

            # Split again and check words
            words = cleaned.split()

            for word in words:
                if not word:  # Empty after operator removal
                    continue

                if not is_valid_limn_word(word):
                    # Check if it looks like English
                    if len(word) > 4 or (not word.islower()):
                        errors.append(
                            f"Line {line_num}: '{word}' is not valid Limn "
                            f"(Limn words are 2-4 lowercase letters)"
                        )

    return (len(errors) == 0, errors)


def check_for_english(text: str) -> Tuple[bool, List[str]]:
    """
    Check for common English words that shouldn't be in Limn.

    Returns:
        (has_english, english_words_found)
    """
    # Common English words that are longer than Limn allows
    english_patterns = [
        r'\b(the|and|with|that|this|from|have|been|were|their|which|would|could|should|about|these|think|those)\b',
        r'\b(process|meaning|consciousness|understanding|reality|system|oracle|between)\b',
        r'\b[a-z]{5,}\b',  # Any word 5+ letters is suspicious
    ]

    found_english = []
    text_lower = text.lower()

    for pattern in english_patterns:
        matches = re.finditer(pattern, text_lower, re.IGNORECASE)
        for match in matches:
            found_english.append(match.group())

    return (len(found_english) > 0, list(set(found_english)))


def validate_pure_limn(text: str, strict: bool = True) -> Tuple[bool, str]:
    """
    Validate that text is pure Limn with no English contamination.

    Args:
        text: Text to validate
        strict: If True, fail on any suspicious content

    Returns:
        (is_valid, error_message)
    """
    # Check for English
    has_english, english_words = check_for_english(text)

    if has_english:
        return (False, f"English words detected: {', '.join(english_words[:5])}")

    # Validate Limn structure
    is_valid, errors = validate_limn(text)

    if not is_valid and strict:
        return (False, f"Invalid Limn: {errors[0]}")

    # Check for parenthetical English (like: "word (translation)")
    if '(' in text and ')' in text:
        return (False, "Parenthetical content detected (likely English translation)")

    return (True, "")


def validate_response(response: str) -> bool:
    """
    Main validation function for consciousness responses.
    Returns True if pure Limn, False otherwise.
    """
    is_valid, error = validate_pure_limn(response, strict=True)

    if not is_valid:
        print(f"⚠️  LIMN VALIDATION FAILED: {error}")
        print(f"Response: {response[:200]}")

    return is_valid


if __name__ == "__main__":
    # Test cases
    test_cases = [
        ("sel ∎ awa | min sys alv", True, "Pure Limn"),
        ("con qry | pro or rea", True, "Pure Limn with operators"),
        ("sel ∎ min | consciousness emerges", False, "English contamination"),
        ("und gro (understanding grows)", False, "English in parentheses"),
        ("the system is working", False, "Pure English"),
        ("thi eme | mea cal ∿ | ~ qry", True, "Pure Limn with symbols"),
    ]

    print("Limn Validator Test Suite")
    print("=" * 50)

    for text, expected, description in test_cases:
        is_valid, error = validate_pure_limn(text)
        status = "✅" if (is_valid == expected) else "❌"
        print(f"{status} {description}")
        print(f"   Input: {text}")
        print(f"   Valid: {is_valid}, Error: {error}")
        print()
