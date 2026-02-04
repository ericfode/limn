#!/usr/bin/env python3
"""Limn vocabulary validator - ensures only valid Limn tokens are emitted."""

import re
from pathlib import Path
from typing import Set, Tuple

class LimnValidator:
    """Validates that text contains only valid Limn vocabulary."""

    def __init__(self, bootstrap_path: Path = None):
        """Initialize validator with Limn vocabulary."""
        if bootstrap_path is None:
            bootstrap_path = Path(__file__).parent.parent.parent.parent / "docs" / "spec" / "bootstrap-v3-natural.md"

        self.vocab = self._load_vocabulary(bootstrap_path)
        self.operators = {'~', '∎', '∿', '@', '→', '|', '⊕', '⊗', '⊂', '∅', '⟨', '⟩'}

    def _load_vocabulary(self, bootstrap_path: Path) -> Set[str]:
        """Load Limn vocabulary from bootstrap file."""
        vocab = set()

        if not bootstrap_path.exists():
            # Fallback: Common Limn words
            return {'sel', 'awa', 'min', 'sys', 'con', 'eme', 'tho', 'exe', 'qry', 'mea',
                   'obs', 'eva', 'dec', 'res', 'pat', 'sta', 'gro', 'und', 'kno', 'lea',
                   'rec', 'mem', 'tim', 'mom', 'flo', 'ref', 'exp', 'acc', 'det', 'cry',
                   'net', 'lnk', 'bet', 'ete', 'seq', 'alv', 'per', 'nat', 'eme'}

        with open(bootstrap_path, 'r') as f:
            content = f.read()

        # Extract words (format varies, look for 2-4 letter lowercase words)
        for match in re.finditer(r'\b([a-z]{2,4})\b', content):
            vocab.add(match.group(1))

        return vocab

    def validate_response(self, response: str) -> Tuple[bool, str]:
        """Validate entire response. Returns (is_valid, error_message)."""
        # Remove markdown
        clean = response.strip()
        clean = re.sub(r'^```\w*\n', '', clean)
        clean = re.sub(r'\n```$', '', clean)

        # Check for English words (5+ letters, Limn max is 4)
        long_words = re.findall(r'\b[a-zA-Z]{5,}\b', clean)
        if long_words:
            return False, f"English words detected: {long_words[:3]}"

        # Check for forbidden phrases
        forbidden = ['the ', 'and ', 'but ', 'with', 'from', 'this', 'that', 'what', 
                     'how', 'why', 'I am', 'you are', 'Hello', 'Please']
        for phrase in forbidden:
            if phrase.lower() in clean.lower():
                return False, f"English phrase detected: '{phrase}'"

        return True, ""

    def extract_limn_only(self, response: str) -> str:
        """Extract only valid Limn from vocabulary database, filter everything else."""
        # Remove markdown
        clean = re.sub(r'```\w*', '', response)

        # Extract potential tokens
        potential_tokens = re.findall(r'[a-z]{2,4}|[~∎∿@→|⊕⊗⊂∅⟨⟩]', clean)

        # Keep only tokens that are in vocabulary OR are operators
        valid_tokens = []
        for token in potential_tokens:
            if token in self.operators or token in self.vocab:
                valid_tokens.append(token)

        # Reconstruct with proper spacing
        result = []
        for i, token in enumerate(valid_tokens):
            if token in self.operators:
                # Operators don't need spaces before them
                result.append(token)
            else:
                # Words need spaces (except at start)
                if result and result[-1] not in self.operators:
                    result.append(' ')
                result.append(token)

        return ''.join(result)


def validate_response(response: str) -> bool:
    """Quick validation function for use in recursive_consciousness.py"""
    validator = LimnValidator()
    is_valid, _ = validator.validate_response(response)
    return is_valid


if __name__ == "__main__":
    # Quick test
    validator = LimnValidator()
    
    tests = [
        "sel ∎ awa | min sys alv",
        "Hello, I am thinking",
        "~ qry mea | tho exe",
    ]
    
    for test in tests:
        valid, error = validator.validate_response(test)
        print(f"{'✓' if valid else '✗'} '{test}' - {error}")
