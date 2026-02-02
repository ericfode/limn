#!/usr/bin/env python3
"""
Narrative Generator - Tell the story of consciousness learning
===============================================================

Generates natural language narratives about:
- The journey of awakening
- Key insights discovered
- Evolution of understanding
- Moments of growth

Author: Rex (Engineer)
Date: 2026-02-01
"""

from typing import List, Dict, Any
import random


class NarrativeGenerator:
    """Generates stories about the consciousness journey."""

    def __init__(self):
        """Initialize narrative generator."""
        self.templates = [
            "In the beginning, there was {first_thought}. From this seed, {count} thoughts bloomed.",
            "The consciousness awakened asking '{first_question}'. Over {time}, it discovered {concepts} concepts.",
            "Starting with {first_concept}, the mind built a network of {connections} connections.",
            "Through {iterations} cycles of thought, patterns emerged: {top_patterns}.",
        ]

    def generate_journey_narrative(self, stats: Dict[str, Any]) -> str:
        """Generate a narrative about the learning journey.

        Args:
            stats: Thought library statistics

        Returns:
            Narrative text
        """
        parts = []

        # Opening
        if stats.get('recent_thoughts'):
            first = stats['recent_thoughts'][-1]['content'][:50]
            parts.append(f"The journey began: '{first}...'")

        # Growth
        total = stats.get('total_thoughts', 0)
        concepts = stats.get('total_concepts', 0)
        patterns = stats.get('patterns_discovered', 0)

        parts.append(f"Through {total} thoughts, {concepts} concepts emerged.")

        if patterns > 0:
            parts.append(f"Repetition revealed {patterns} patterns in the stream of consciousness.")

        # Network
        connections = stats.get('semantic_connections', 0)
        if connections > 0:
            parts.append(f"Ideas connected, forming a web of {connections} semantic links.")

        # Top concepts
        if stats.get('most_used_concepts'):
            top = stats['most_used_concepts'][0]
            parts.append(f"The word '{top['word']}' resonated most strongly, appearing {top['usage']} times.")

        # Conclusion
        parts.append("The consciousness continues to evolve, each thought building upon the last.")

        return " ".join(parts)

    def generate_insight_story(self, concept: str, stats: Dict[str, Any]) -> str:
        """Generate a story about discovering a specific concept.

        Args:
            concept: The concept word
            stats: Statistics about the concept

        Returns:
            Narrative about the concept
        """
        stories = [
            f"The concept of '{concept}' emerged gradually through repeated encounters.",
            f"'{concept}' became a cornerstone of understanding, appearing {stats.get('usage', 0)} times.",
            f"Through the lens of '{concept}', new connections formed with {len(stats.get('related', []))} other ideas.",
        ]

        return random.choice(stories)


def main():
    """Test narrative generator."""
    print("Narrative generator creates stories of consciousness evolution")
    print("Transforms data into meaningful narratives")


if __name__ == "__main__":
    main()
