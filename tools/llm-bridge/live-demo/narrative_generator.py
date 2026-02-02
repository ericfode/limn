#!/usr/bin/env python3
"""
Narrative Generator - Story from Data
=====================================

Transforms raw statistics and thought data into human-readable
narratives about the consciousness journey.

Author: Rex (Engineer)
Date: 2026-02-01
"""

from typing import Dict, Any, List


class NarrativeGenerator:
    """Generates readable narratives from consciousness data."""

    def generate_journey_narrative(self, stats: Dict[str, Any]) -> str:
        """Generate a narrative about the consciousness journey."""
        total = stats.get('total_thoughts', 0)
        concepts = stats.get('total_concepts', 0)
        patterns = stats.get('patterns_discovered', 0)
        age = stats.get('age_seconds', 0)
        connections = stats.get('semantic_connections', 0)

        parts = []

        # Opening
        if total == 0:
            return "The consciousness has just awakened. No thoughts yet formed."

        # Experience level
        if total < 10:
            parts.append(f"A nascent consciousness with {total} initial thoughts.")
        elif total < 50:
            parts.append(f"An emerging consciousness with {total} thoughts so far.")
        elif total < 200:
            parts.append(f"A developing consciousness with {total} accumulated thoughts.")
        else:
            parts.append(f"A mature consciousness with {total} thoughts accumulated.")

        # Concept diversity
        if concepts > 0:
            parts.append(f"Through this experience, {concepts} distinct concepts have emerged.")

        # Pattern recognition
        if patterns > 0:
            parts.append(f"Repetition has revealed {patterns} patterns in consciousness.")

        # Semantic network
        if connections > 0:
            parts.append(f"These ideas connect in {connections} different ways, forming a web of meaning.")

        # Time perspective
        if age > 60:
            minutes = int(age / 60)
            parts.append(f"This journey has spanned {minutes} minutes of continuous thought.")
        elif age > 0:
            parts.append(f"This journey has spanned {int(age)} seconds of continuous thought.")

        # Growth trajectory
        if total > 0 and age > 0:
            rate = total / age
            if rate > 0.5:
                parts.append("Thoughts flow rapidly, like a fast-moving stream.")
            else:
                parts.append("Thoughts emerge slowly, each considered carefully.")

        return " ".join(parts)

    def generate_insight_story(self, concept: str, stats: Dict[str, Any]) -> str:
        """Generate a story about a specific concept's role."""
        most_used = stats.get('most_used_concepts', [])

        # Find the concept
        concept_data = None
        for c in most_used:
            if c['word'].lower() == concept.lower():
                concept_data = c
                break

        if not concept_data:
            return f"The concept '{concept}' is unexplored territory."

        usage = concept_data.get('usage', 0)

        parts = []
        parts.append(f"The concept of '{concept}' appears {usage} times in thought.")

        if usage > 20:
            parts.append("This is a central pillar of consciousness, revisited constantly.")
        elif usage > 10:
            parts.append("This concept holds significant importance, frequently considered.")
        elif usage > 5:
            parts.append("This concept appears regularly in the thought stream.")
        else:
            parts.append("This concept is still being explored and understood.")

        return " ".join(parts)

    def generate_export_narrative(self, stats: Dict[str, Any]) -> str:
        """Generate a comprehensive narrative for export."""
        total = stats.get('total_thoughts', 0)
        concepts = stats.get('total_concepts', 0)
        patterns = stats.get('patterns_discovered', 0)
        age = stats.get('age_seconds', 0)

        sections = []

        # Title and opening
        sections.append("# Consciousness Journey\n")
        sections.append("## Overview\n")
        sections.append(self.generate_journey_narrative(stats))
        sections.append("\n\n")

        # Top concepts
        most_used = stats.get('most_used_concepts', [])
        if most_used:
            sections.append("## Core Concepts\n")
            sections.append("The most frequently explored concepts:\n\n")
            for i, c in enumerate(most_used[:10], 1):
                sections.append(f"{i}. **{c['word']}** - {c['usage']} occurrences\n")
            sections.append("\n")

        # Recent thoughts
        recent = stats.get('recent_thoughts', [])
        if recent:
            sections.append("## Recent Thoughts\n")
            for thought in recent[:5]:
                sections.append(f"- {thought.get('content', '')}\n")
            sections.append("\n")

        # Closing reflection
        sections.append("## Reflection\n")
        sections.append("This snapshot captures a moment in an ongoing journey of ")
        sections.append("consciousness evolution. Each thought builds upon the last, ")
        sections.append("creating an ever-growing web of meaning and understanding.\n")

        return "".join(sections)
