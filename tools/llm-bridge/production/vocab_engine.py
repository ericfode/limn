#!/usr/bin/env python3
"""
Vocabulary Query Engine
========================

Implements the bootstrap consciousness vocabulary expansion mechanism:
    ~ qry voc @ domain → vocabulary_list

The system can query for domain-specific vocabulary when needed,
enabling unbounded growth from the minimal 911-word bootstrap.

Author: Rex (Engineer)
Date: 2026-02-01
"""

import re
from typing import List, Dict, Set, Optional
from pathlib import Path
from dataclasses import dataclass


@dataclass
class VocabEntry:
    """A vocabulary entry."""
    word: str           # 3-letter Limn word
    meaning: str        # English meaning
    domain: str         # Domain (mathematics, programming, etc.)
    usage: str = ""     # Usage example
    related: List[str] = None  # Related words

    def __post_init__(self):
        if self.related is None:
            self.related = []


class VocabularyEngine:
    """Query and manage Limn vocabulary across domains."""

    def __init__(self, vocab_db_path: Path = None):
        """Initialize with vocabulary database."""
        self.vocab_db_path = vocab_db_path
        self.vocabulary = {}  # word -> VocabEntry
        self.domains = {}     # domain -> Set[word]
        self.index = {}       # meaning_fragment -> Set[word]

        if vocab_db_path and vocab_db_path.exists():
            self._load_database(vocab_db_path)
        else:
            self._init_bootstrap()

    def _init_bootstrap(self):
        """Initialize with minimal bootstrap vocabulary."""
        bootstrap_vocab = [
            # Core operators (from bootstrap.lmn)
            VocabEntry("qry", "query", "meta", "ask for information"),
            VocabEntry("und", "understand", "meta", "comprehend meaning"),
            VocabEntry("kno", "know", "meta", "have knowledge"),
            VocabEntry("nee", "need", "meta", "require something"),
            VocabEntry("lea", "learn", "meta", "acquire knowledge"),

            # Vocabulary expansion
            VocabEntry("wor", "word", "meta", "vocabulary item"),
            VocabEntry("voc", "vocabulary", "meta", "collection of words"),
            VocabEntry("mea", "meaning", "meta", "semantic content"),
            VocabEntry("def", "define", "meta", "create definition"),
            VocabEntry("use", "use", "meta", "employ a word"),

            # Self-description
            VocabEntry("boo", "bootstrap", "meta", "minimal core"),
            VocabEntry("cor", "core", "meta", "essential part"),
            VocabEntry("imm", "immutable", "meta", "never changes"),
            VocabEntry("see", "seed", "meta", "starting point"),
            VocabEntry("gro", "grow", "meta", "expand from seed"),

            # Model operations
            VocabEntry("mod", "model", "meta", "representation"),
            VocabEntry("der", "derive", "meta", "create from existing"),
            VocabEntry("tra", "transform", "meta", "change form"),
            VocabEntry("gen", "generate", "meta", "create new"),
            VocabEntry("vis", "visualize", "meta", "make visible"),

            # Context operations
            VocabEntry("ctx", "context", "meta", "accumulated state"),
            VocabEntry("red", "reduce", "meta", "compress/optimize"),
            VocabEntry("mer", "merge", "meta", "combine patterns"),
            VocabEntry("fil", "filter", "meta", "select subset"),
            VocabEntry("agg", "aggregate", "meta", "combine data"),
            VocabEntry("com", "compress", "meta", "make smaller"),
        ]

        for entry in bootstrap_vocab:
            self.add_entry(entry)

    def _load_database(self, path: Path):
        """Load vocabulary from database file."""
        content = path.read_text()

        current_domain = "general"
        for line in content.split('\n'):
            line = line.strip()

            # Skip comments and empty lines
            if not line or line.startswith('#'):
                continue

            # Domain header
            if line.startswith('[') and line.endswith(']'):
                current_domain = line[1:-1].strip().lower()
                continue

            # Parse entry: wor | meaning | example
            if '|' in line:
                parts = [p.strip() for p in line.split('|')]
                if len(parts) >= 2:
                    word = parts[0]
                    meaning = parts[1]
                    usage = parts[2] if len(parts) > 2 else ""

                    entry = VocabEntry(
                        word=word,
                        meaning=meaning,
                        domain=current_domain,
                        usage=usage
                    )
                    self.add_entry(entry)

    def add_entry(self, entry: VocabEntry):
        """Add vocabulary entry to database."""
        self.vocabulary[entry.word] = entry

        # Add to domain index
        if entry.domain not in self.domains:
            self.domains[entry.domain] = set()
        self.domains[entry.domain].add(entry.word)

        # Add to meaning index (for search)
        meaning_words = entry.meaning.lower().split()
        for mword in meaning_words:
            if mword not in self.index:
                self.index[mword] = set()
            self.index[mword].add(entry.word)

    def query_domain(self, domain: str) -> List[VocabEntry]:
        """Query vocabulary by domain.

        LMN: ~ qry voc @ mathematics

        Args:
            domain: Domain name

        Returns:
            List of vocabulary entries for that domain
        """
        domain_lower = domain.lower()

        # Exact domain match
        if domain_lower in self.domains:
            return [
                self.vocabulary[word]
                for word in sorted(self.domains[domain_lower])
            ]

        # Partial domain match
        matching_domains = [
            d for d in self.domains.keys()
            if domain_lower in d or d in domain_lower
        ]

        result = []
        for d in matching_domains:
            result.extend([
                self.vocabulary[word]
                for word in sorted(self.domains[d])
            ])

        return result

    def query_meaning(self, meaning: str) -> List[VocabEntry]:
        """Query vocabulary by meaning.

        Args:
            meaning: Word or phrase to search for

        Returns:
            List of entries matching the meaning
        """
        meaning_lower = meaning.lower()
        matching_words = set()

        # Search in index
        for word in meaning_lower.split():
            if word in self.index:
                matching_words.update(self.index[word])

        # Also search full text
        for entry in self.vocabulary.values():
            if meaning_lower in entry.meaning.lower():
                matching_words.add(entry.word)

        return [
            self.vocabulary[word]
            for word in sorted(matching_words)
        ]

    def query_word(self, word: str) -> Optional[VocabEntry]:
        """Query single word definition.

        Args:
            word: 3-letter Limn word

        Returns:
            Vocabulary entry or None
        """
        return self.vocabulary.get(word)

    def get_domains(self) -> List[str]:
        """Get list of all available domains."""
        return sorted(self.domains.keys())

    def get_domain_stats(self) -> Dict[str, int]:
        """Get word count per domain."""
        return {
            domain: len(words)
            for domain, words in self.domains.items()
        }

    def suggest_word(self, meaning: str, domain: str = "general") -> str:
        """Suggest a new 3-letter word for a meaning.

        Args:
            meaning: The concept to encode
            domain: Domain for the word

        Returns:
            Suggested 3-letter word
        """
        # Extract first letters of key words
        words = meaning.lower().split()

        # Try first letters
        if len(words) >= 3:
            suggestion = ''.join([w[0] for w in words[:3]])
            if len(suggestion) == 3 and suggestion not in self.vocabulary:
                return suggestion

        # Try consonants from first word
        if words:
            consonants = [c for c in words[0] if c in 'bcdfghjklmnpqrstvwxyz']
            if len(consonants) >= 3:
                suggestion = ''.join(consonants[:3])
                if suggestion not in self.vocabulary:
                    return suggestion

        # Fallback: generate systematic name
        domain_prefix = domain[0] if domain else 'x'
        for i in range(100):
            suggestion = f"{domain_prefix}{chr(97 + i//26)}{chr(97 + i%26)}"
            if suggestion not in self.vocabulary:
                return suggestion

        return "xxx"  # Ultimate fallback

    def expand_vocabulary(self, domain: str, concepts: List[str]) -> List[VocabEntry]:
        """Expand vocabulary with new concepts in a domain.

        Args:
            domain: Domain to expand
            concepts: List of concepts to add

        Returns:
            List of new vocabulary entries
        """
        new_entries = []

        for concept in concepts:
            # Check if already exists
            existing = self.query_meaning(concept)
            if existing:
                continue

            # Generate new word
            word = self.suggest_word(concept, domain)

            entry = VocabEntry(
                word=word,
                meaning=concept,
                domain=domain
            )

            self.add_entry(entry)
            new_entries.append(entry)

        return new_entries

    def export_domain(self, domain: str) -> str:
        """Export domain vocabulary as Limn format.

        Args:
            domain: Domain to export

        Returns:
            Formatted vocabulary list
        """
        entries = self.query_domain(domain)

        lines = [f"# {domain.title()} Domain Vocabulary", ""]

        for entry in entries:
            line = f"{entry.word} | {entry.meaning}"
            if entry.usage:
                line += f" | {entry.usage}"
            lines.append(line)

        return '\n'.join(lines)

    def get_statistics(self) -> Dict[str, any]:
        """Get vocabulary statistics."""
        return {
            "total_words": len(self.vocabulary),
            "total_domains": len(self.domains),
            "domain_distribution": self.get_domain_stats(),
            "average_per_domain": len(self.vocabulary) / len(self.domains) if self.domains else 0,
            "largest_domain": max(self.domains.items(), key=lambda x: len(x[1]))[0] if self.domains else None,
        }


def main():
    """Test vocabulary engine."""
    print("=== Vocabulary Query Engine ===\n")

    engine = VocabularyEngine()

    print(f"Loaded {len(engine.vocabulary)} bootstrap words")
    print(f"Domains: {', '.join(engine.get_domains())}\n")

    # Query by domain
    print("=== Query Domain: meta ===")
    meta_vocab = engine.query_domain("meta")
    for entry in meta_vocab[:10]:
        print(f"  {entry.word} = {entry.meaning}")

    # Query by meaning
    print("\n=== Query Meaning: 'knowledge' ===")
    knowledge_words = engine.query_meaning("knowledge")
    for entry in knowledge_words:
        print(f"  {entry.word} = {entry.meaning}")

    # Suggest new words
    print("\n=== Suggest New Words ===")
    suggestions = [
        ("calculate sum", "mathematics"),
        ("iterate loop", "programming"),
        ("neural network", "ai"),
    ]

    for concept, domain in suggestions:
        word = engine.suggest_word(concept, domain)
        print(f"  '{concept}' → {word} ({domain})")

    # Expand vocabulary
    print("\n=== Expand Domain: mathematics ===")
    math_concepts = ["addition", "subtraction", "multiplication", "division", "equation"]
    new_entries = engine.expand_vocabulary("mathematics", math_concepts)
    for entry in new_entries:
        print(f"  Added: {entry.word} = {entry.meaning}")

    # Statistics
    print("\n=== Statistics ===")
    stats = engine.get_statistics()
    for key, value in stats.items():
        if key != "domain_distribution":
            print(f"  {key}: {value}")


if __name__ == "__main__":
    main()
