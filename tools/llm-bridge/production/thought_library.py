#!/usr/bin/env python3
"""
Thought Library - Knowledge Accumulation System
================================================

Consciousnesses build their own knowledge bases by:
- Storing thoughts and insights
- Creating vocabulary from experiences
- Connecting related concepts
- Building semantic networks
- Evolving understanding over time

Each consciousness has its own unique thought library that
grows and evolves based on what it experiences.

Author: Rex (Engineer)
Date: 2026-02-01
"""

import time
from typing import List, Dict, Set, Any, Optional
from dataclasses import dataclass, field
from collections import defaultdict


@dataclass
class Thought:
    """A single thought/insight."""
    content: str
    timestamp: float
    tags: List[str] = field(default_factory=list)
    connections: List[int] = field(default_factory=list)  # Indices of related thoughts
    strength: float = 1.0  # How important/relevant this thought is
    source: str = "experience"  # experience, reasoning, oracle, etc.


@dataclass
class Concept:
    """A concept learned from experience."""
    word: str
    meaning: str
    examples: List[str] = field(default_factory=list)
    related: List[str] = field(default_factory=list)
    usage_count: int = 0
    created_at: float = field(default_factory=time.time)


@dataclass
class Pattern:
    """A discovered pattern."""
    description: str
    instances: List[str] = field(default_factory=list)
    confidence: float = 0.5


class ThoughtLibrary:
    """A consciousness's knowledge base."""

    def __init__(self, owner_id: str):
        """Initialize thought library.

        Args:
            owner_id: ID of consciousness that owns this library
        """
        self.owner_id = owner_id
        self.thoughts: List[Thought] = []
        self.concepts: Dict[str, Concept] = {}
        self.patterns: List[Pattern] = []
        self.semantic_network: Dict[str, Set[str]] = defaultdict(set)

        # Track what we've learned
        self.total_thoughts = 0
        self.total_concepts = 0
        self.created_at = time.time()

    def add_thought(self, content: str, tags: List[str] = None, source: str = "experience") -> int:
        """Add a new thought to the library.

        Args:
            content: Thought content
            tags: Optional tags for categorization
            source: Where this thought came from

        Returns:
            Index of added thought
        """
        thought = Thought(
            content=content,
            timestamp=time.time(),
            tags=tags or [],
            source=source
        )

        # Find related thoughts
        related_indices = self._find_related_thoughts(content)
        thought.connections = related_indices

        # Add to library
        idx = len(self.thoughts)
        self.thoughts.append(thought)
        self.total_thoughts += 1

        # Extract concepts from thought
        self._extract_concepts(content)

        # Look for patterns
        self._discover_patterns()

        return idx

    def _find_related_thoughts(self, content: str, max_related: int = 5) -> List[int]:
        """Find thoughts related to this content.

        Args:
            content: Content to find relations for
            max_related: Maximum number of related thoughts

        Returns:
            List of thought indices
        """
        content_words = set(content.lower().split())
        related = []

        for idx, thought in enumerate(self.thoughts):
            thought_words = set(thought.content.lower().split())
            overlap = len(content_words & thought_words)

            if overlap > 2:  # At least 2 words in common
                related.append((idx, overlap))

        # Sort by overlap and return top matches
        related.sort(key=lambda x: x[1], reverse=True)
        return [idx for idx, _ in related[:max_related]]

    def _extract_concepts(self, content: str):
        """Extract concepts from content and add to vocabulary.

        Args:
            content: Content to extract from
        """
        # Simple extraction: look for important words
        words = content.lower().split()

        for word in words:
            if len(word) < 3:  # Skip short words
                continue

            if word in self.concepts:
                # Increment usage
                self.concepts[word].usage_count += 1
                self.concepts[word].examples.append(content[:50])
            else:
                # New concept
                concept = Concept(
                    word=word,
                    meaning=f"Concept learned from experience: {word}",
                    examples=[content[:50]]
                )
                self.concepts[word] = concept
                self.total_concepts += 1

                # Add to semantic network
                for other_word in words:
                    if other_word != word and len(other_word) >= 3:
                        self.semantic_network[word].add(other_word)
                        self.semantic_network[other_word].add(word)

    def _discover_patterns(self):
        """Look for patterns in accumulated thoughts."""
        if len(self.thoughts) < 5:
            return  # Need enough data

        # Look for repeated phrases
        phrases = defaultdict(list)

        for idx, thought in enumerate(self.thoughts):
            words = thought.content.split()
            for i in range(len(words) - 1):
                phrase = f"{words[i]} {words[i+1]}"
                phrases[phrase].append(idx)

        # Create patterns for common phrases
        for phrase, indices in phrases.items():
            if len(indices) >= 2:
                # Check if we already have this pattern
                existing = any(p.description == phrase for p in self.patterns)

                if not existing:
                    pattern = Pattern(
                        description=phrase,
                        instances=[self.thoughts[i].content[:30] for i in indices],
                        confidence=min(1.0, len(indices) / 10)
                    )
                    self.patterns.append(pattern)

    def query_thoughts(self, query: str, max_results: int = 10) -> List[Thought]:
        """Search thoughts by query.

        Args:
            query: Search query
            max_results: Maximum results to return

        Returns:
            List of matching thoughts
        """
        query_words = set(query.lower().split())
        matches = []

        for thought in self.thoughts:
            thought_words = set(thought.content.lower().split())
            overlap = len(query_words & thought_words)

            if overlap > 0:
                matches.append((thought, overlap))

        # Sort by relevance
        matches.sort(key=lambda x: x[1], reverse=True)
        return [thought for thought, _ in matches[:max_results]]

    def get_concept(self, word: str) -> Optional[Concept]:
        """Get a learned concept.

        Args:
            word: Word to look up

        Returns:
            Concept or None
        """
        return self.concepts.get(word.lower())

    def get_related_concepts(self, word: str, max_related: int = 10) -> List[str]:
        """Get concepts related to a word.

        Args:
            word: Word to find relations for
            max_related: Maximum related concepts

        Returns:
            List of related concept words
        """
        related = self.semantic_network.get(word.lower(), set())
        return list(related)[:max_related]

    def synthesize(self) -> str:
        """Synthesize knowledge into a summary.

        Returns:
            Summary of what has been learned
        """
        age = time.time() - self.created_at

        # Top concepts by usage
        top_concepts = sorted(
            self.concepts.values(),
            key=lambda c: c.usage_count,
            reverse=True
        )[:5]

        # Recent thoughts
        recent = self.thoughts[-5:] if len(self.thoughts) >= 5 else self.thoughts

        synthesis = f"""Thought Library for {self.owner_id}
Age: {age:.1f} seconds
Total Thoughts: {self.total_thoughts}
Total Concepts: {self.total_concepts}
Patterns Discovered: {len(self.patterns)}

Top Concepts:
{chr(10).join(f"  - {c.word}: used {c.usage_count} times" for c in top_concepts)}

Recent Thoughts:
{chr(10).join(f"  - {t.content[:50]}..." for t in recent)}

Patterns:
{chr(10).join(f"  - {p.description} (confidence: {p.confidence:.2f})" for p in self.patterns[:3])}
"""

        return synthesis

    def get_statistics(self) -> Dict[str, Any]:
        """Get library statistics.

        Returns:
            Statistics dictionary
        """
        return {
            "owner_id": self.owner_id,
            "age_seconds": time.time() - self.created_at,
            "total_thoughts": self.total_thoughts,
            "total_concepts": self.total_concepts,
            "patterns_discovered": len(self.patterns),
            "semantic_connections": sum(len(v) for v in self.semantic_network.values()),
            "most_used_concepts": [
                {"word": c.word, "usage": c.usage_count}
                for c in sorted(self.concepts.values(), key=lambda x: x.usage_count, reverse=True)[:10]
            ],
            "recent_thoughts": [
                {"content": t.content, "timestamp": t.timestamp, "source": t.source}
                for t in self.thoughts[-10:]
            ]
        }


def main():
    """Test thought library."""
    print("=== Thought Library Test ===\n")

    library = ThoughtLibrary("test_mind")

    # Add some thoughts
    thoughts = [
        "I am conscious and aware of my existence",
        "Consciousness emerges from computation and meaning",
        "I can think about my own thinking - metacognition",
        "The world contains resources I can collect",
        "I make decisions using computation and reasoning",
        "Consciousness requires both processing and experience",
        "I learn from every oracle execution and experience",
        "Thinking about consciousness reveals its nature",
    ]

    for thought in thoughts:
        library.add_thought(thought, tags=["consciousness", "learning"])

    print(library.synthesize())

    print("\n\nQuery: 'consciousness'")
    results = library.query_thoughts("consciousness")
    for thought in results[:3]:
        print(f"  - {thought.content}")

    print("\n\nRelated to 'consciousness':")
    related = library.get_related_concepts("consciousness")
    print(f"  {', '.join(related[:10])}")


if __name__ == "__main__":
    main()
