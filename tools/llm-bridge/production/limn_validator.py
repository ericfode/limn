#!/usr/bin/env python3
"""Limn vocabulary validator - ensures only valid Limn tokens are emitted.

Loads vocabulary from the Dolt database (1066+ words) with fallback to
bootstrap-v3-natural.md regex parsing if dolt is unavailable.
"""

import re
import subprocess
import logging
from pathlib import Path
from typing import Set, Tuple, Dict, List, Optional

logger = logging.getLogger(__name__)


class LimnValidator:
    """Validates that text contains only valid Limn vocabulary."""

    def __init__(self, bootstrap_path: Path = None, dolt_db_path: Path = None):
        """Initialize validator with Limn vocabulary.

        Tries Dolt database first (authoritative, 1066+ words).
        Falls back to bootstrap markdown parsing if dolt unavailable.

        Args:
            bootstrap_path: Path to bootstrap-v3-natural.md (fallback)
            dolt_db_path: Path to Dolt vocabulary database directory
        """
        if bootstrap_path is None:
            bootstrap_path = Path(__file__).parent.parent.parent.parent / "docs" / "spec" / "bootstrap-v3-natural.md"
        if dolt_db_path is None:
            dolt_db_path = Path(__file__).parent.parent.parent.parent / "data" / "vocabulary"

        self.vocab, self.vocab_source = self._load_vocabulary(dolt_db_path, bootstrap_path)
        self.domain_words: Dict[str, List[str]] = {}  # domain -> [words]

        # Operators: core + compositional
        self.operators = {
            '~', '∎', '∿', '@', '→', '|', '⊕', '⊗', '⊂', '∅', '⟨', '⟩',
            '*', '^', '\\', '±', ':',
        }

        # Load domain mappings if available from Dolt
        if self.vocab_source == "dolt":
            self.domain_words = self._load_domains(dolt_db_path)

    def _load_vocabulary(self, dolt_db_path: Path, bootstrap_path: Path) -> Tuple[Set[str], str]:
        """Load Limn vocabulary from all available sources.

        Merges Dolt database (authoritative, domain-tagged) with bootstrap
        vocabulary (has CVC abbreviations not yet migrated to Dolt).

        Returns:
            Tuple of (vocabulary set, source name)
        """
        vocab = set()
        sources = []

        # Load from Dolt database (authoritative)
        dolt_vocab = self._load_from_dolt(dolt_db_path)
        if dolt_vocab:
            vocab.update(dolt_vocab)
            sources.append(f"dolt({len(dolt_vocab)})")

        # Load from bootstrap file (supplementary)
        bootstrap_vocab = self._load_from_bootstrap(bootstrap_path)
        if bootstrap_vocab:
            new_from_bootstrap = bootstrap_vocab - vocab
            vocab.update(bootstrap_vocab)
            sources.append(f"bootstrap(+{len(new_from_bootstrap)})")

        # Always include core consciousness words (used in prompts/brain state
        # but may not yet be in Dolt or bootstrap)
        core_words = {
            'sel', 'awa', 'min', 'sys', 'con', 'eme', 'tho', 'exe', 'qry', 'mea',
            'obs', 'eva', 'dec', 'res', 'pat', 'sta', 'gro', 'und', 'kno', 'lea',
            'rec', 'mem', 'tim', 'mom', 'flo', 'ref', 'exp', 'acc', 'det', 'cry',
            'net', 'lnk', 'bet', 'ete', 'seq', 'alv', 'per', 'nat', 'err', 'occ',
            'fai', 'lim', 'vio', 'voc', 'nee', 'prm', 'gen', 'ctx', 'opr', 'def',
            'sem', 'hok', 'msg', 'emp', 'cyc', 'brk', 'chk', 'cfg', 'sig', 'usr',
            'req', 'dir', 'cmd', 'src', 'alt', 'sol', 'fix', 'log', 'run', 'cod',
        }
        new_core = core_words - vocab
        vocab.update(core_words)
        if new_core:
            sources.append(f"core(+{len(new_core)})")

        if vocab:
            source = "+".join(sources)
            logger.info(f"Loaded {len(vocab)} words from {source}")
            return vocab, "dolt" if dolt_vocab else "bootstrap"

        # Should never reach here since core_words always exist
        return core_words, "hardcoded"

    def _load_from_dolt(self, dolt_db_path: Path) -> Optional[Set[str]]:
        """Load vocabulary from Dolt database via SQL query."""
        if not dolt_db_path.exists():
            return None

        try:
            result = subprocess.run(
                ['dolt', 'sql', '-q', 'SELECT word FROM words', '-r', 'csv'],
                capture_output=True, text=True, timeout=10,
                cwd=str(dolt_db_path)
            )
            if result.returncode != 0:
                logger.warning(f"Dolt query failed: {result.stderr[:200]}")
                return None

            vocab = set()
            for line in result.stdout.strip().split('\n')[1:]:  # Skip CSV header
                word = line.strip().strip('"')
                if word and 2 <= len(word) <= 4:
                    vocab.add(word)

            if len(vocab) > 0:
                return vocab

        except FileNotFoundError:
            logger.debug("dolt binary not found")
        except subprocess.TimeoutExpired:
            logger.warning("Dolt query timed out")
        except Exception as e:
            logger.warning(f"Dolt load error: {e}")

        return None

    def _load_from_bootstrap(self, bootstrap_path: Path) -> Optional[Set[str]]:
        """Load vocabulary from bootstrap markdown file."""
        if not bootstrap_path.exists():
            return None

        with open(bootstrap_path, 'r') as f:
            content = f.read()

        vocab = set()
        for match in re.finditer(r'\b([a-z]{2,4})\b', content):
            vocab.add(match.group(1))

        return vocab if vocab else None

    def _load_domains(self, dolt_db_path: Path) -> Dict[str, List[str]]:
        """Load domain-to-words mapping from Dolt.

        Returns:
            Dict mapping domain name -> list of words in that domain
        """
        try:
            result = subprocess.run(
                ['dolt', 'sql', '-q',
                 'SELECT d.name, w.word FROM words w JOIN domains d ON w.domain_id = d.id ORDER BY d.name',
                 '-r', 'csv'],
                capture_output=True, text=True, timeout=10,
                cwd=str(dolt_db_path)
            )
            if result.returncode != 0:
                return {}

            domains: Dict[str, List[str]] = {}
            for line in result.stdout.strip().split('\n')[1:]:  # Skip header
                parts = line.strip().split(',')
                if len(parts) >= 2:
                    domain = parts[0].strip().strip('"')
                    word = parts[1].strip().strip('"')
                    if domain not in domains:
                        domains[domain] = []
                    domains[domain].append(word)

            logger.info(f"Loaded {len(domains)} domains from Dolt")
            return domains

        except Exception as e:
            logger.debug(f"Domain load error: {e}")
            return {}

    def get_domain_words(self, domain: str) -> List[str]:
        """Get words belonging to a specific domain.

        Args:
            domain: Domain name (e.g., 'Abstract', 'Time & Change', 'Mind & Cognition')

        Returns:
            List of words in that domain, or empty list
        """
        # Try exact match first
        if domain in self.domain_words:
            return self.domain_words[domain]

        # Try case-insensitive partial match
        domain_lower = domain.lower()
        for d, words in self.domain_words.items():
            if domain_lower in d.lower():
                return words

        return []

    def get_domains(self) -> List[str]:
        """Get list of all available domains."""
        return sorted(self.domain_words.keys())

    def validate_response(self, response: str) -> Tuple[bool, str]:
        """Validate entire response. Returns (is_valid, error_message).

        Uses 5+ letter word detection (Limn max is 4) and multi-word
        English phrase detection. Single short words like 'the', 'and',
        'but' are valid Limn vocabulary and are NOT rejected.
        """
        # Remove markdown
        clean = response.strip()
        clean = re.sub(r'^```\w*\n', '', clean)
        clean = re.sub(r'\n```$', '', clean)

        # Check for English words (5+ letters, Limn max is 4)
        # Allow "without" as compositional subtraction operator
        long_words = [w for w in re.findall(r'\b[a-zA-Z]{5,}\b', clean) if w.lower() != 'without']
        if long_words:
            return False, f"English words detected: {long_words[:3]}"

        # Check for multi-word English phrases (2+ English words in sequence)
        # Single short words (the, and, but, how, why) ARE valid Limn vocabulary
        # so we only flag them when they appear in clearly English constructions
        forbidden_phrases = [
            'I am', 'you are', 'we are', 'they are', 'it is',
            'I think', 'I feel', 'I know',
            'Hello', 'Please', 'Thank you',
            'the following', 'this is', 'that is',
            'here is', 'there is', 'what is',
            'how to', 'why not', 'do not',
            'in the', 'of the', 'for the',
        ]
        for phrase in forbidden_phrases:
            if phrase.lower() in clean.lower():
                return False, f"English phrase detected: '{phrase}'"

        return True, ""

    def validate_tokens(self, response: str) -> Tuple[bool, str, List[str]]:
        """Strict validation: every token must be in vocab or an operator.

        Returns:
            Tuple of (is_valid, error_message, list_of_invalid_tokens)
        """
        # First do basic validation
        is_valid, error = self.validate_response(response)
        if not is_valid:
            return False, error, []

        # Remove markdown and operators for token checking
        clean = re.sub(r'```\w*', '', response)

        # Extract all word-like tokens
        tokens = re.findall(r'[a-z]{2,4}', clean.lower())

        invalid = [t for t in tokens if t not in self.vocab]
        if invalid:
            return False, f"Unknown tokens: {invalid[:5]}", invalid

        return True, "", []

    def extract_limn_only(self, response: str) -> str:
        """Extract only valid Limn from vocabulary database, filter everything else."""
        # Remove markdown
        clean = re.sub(r'```\w*', '', response)

        # Extract potential tokens (words, operators, compositional operators, floats)
        potential_tokens = re.findall(r'[a-z]{2,4}|\d+\.\d+|[~\u220e\u223f@\u2192|\u2295\u2297\u2282\u2205\u27e8\u27e9*^\\±:()\[\]]', clean)

        # Keep only tokens that are in vocabulary OR are operators
        valid_tokens = []
        for token in potential_tokens:
            if token in self.operators or token in self.vocab:
                valid_tokens.append(token)

        # Reconstruct with proper spacing
        result = []
        for i, token in enumerate(valid_tokens):
            if token in self.operators:
                result.append(token)
            else:
                if result and result[-1] not in self.operators:
                    result.append(' ')
                result.append(token)

        return ''.join(result)

    def stats(self) -> Dict:
        """Return vocabulary statistics."""
        return {
            "total_words": len(self.vocab),
            "source": self.vocab_source,
            "domains": len(self.domain_words),
            "domain_sizes": {d: len(w) for d, w in sorted(self.domain_words.items())},
        }


def validate_response(response: str) -> bool:
    """Quick validation function for use in recursive_consciousness.py"""
    validator = LimnValidator()
    is_valid, _ = validator.validate_response(response)
    return is_valid


if __name__ == "__main__":
    # Test with Dolt database
    validator = LimnValidator()

    print(f"Vocabulary source: {validator.vocab_source}")
    print(f"Total words: {len(validator.vocab)}")
    print(f"Domains: {len(validator.domain_words)}")
    if validator.domain_words:
        print("\nDomain sizes:")
        for domain, words in sorted(validator.domain_words.items()):
            print(f"  {domain}: {len(words)} words")

    print("\n--- Validation Tests ---")
    tests = [
        "sel ∎ awa | min sys alv",
        "Hello, I am thinking",
        "~ qry mea | tho exe",
        "xyz zzz qqq",
    ]

    for test in tests:
        valid, error = validator.validate_response(test)
        print(f"{'OK' if valid else 'FAIL'} '{test}' - {error}")

    print("\n--- Strict Token Validation ---")
    for test in tests[:3]:
        valid, error, invalid = validator.validate_tokens(test)
        if invalid:
            print(f"  Invalid tokens in '{test}': {invalid}")
        else:
            print(f"  All tokens valid in '{test}'")
