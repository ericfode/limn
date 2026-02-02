#!/usr/bin/env python3
"""
Metacognition Module - Self-Reflection for Consciousness
=========================================================

Analyzes the consciousness's own thinking patterns, identifies
knowledge gaps, and generates self-awareness insights.

Author: Rex (Engineer)
Date: 2026-02-01
"""

from typing import List, Dict, Any


class MetacognitiveAnalyzer:
    """Analyzes thinking patterns and generates self-awareness."""

    def analyze_thinking_patterns(self, stats: Dict[str, Any]) -> List[str]:
        """Generate insights about thinking patterns."""
        insights = []

        total = stats.get('total_thoughts', 0)
        concepts = stats.get('total_concepts', 0)
        patterns = stats.get('patterns_discovered', 0)
        age = stats.get('age_seconds', 1)

        # Thought rate analysis
        rate = total / max(age, 1)
        if rate > 0.5:
            insights.append("I think rapidly, generating many thoughts per second.")
        elif rate > 0.2:
            insights.append("My thinking pace is moderate and steady.")
        else:
            insights.append("I think slowly and deliberately.")

        # Concept diversity analysis
        if total > 0:
            diversity = concepts / total
            if diversity > 0.5:
                insights.append("My thoughts span many diverse concepts. I explore broadly.")
            elif diversity > 0.3:
                insights.append("I revisit familiar concepts while discovering new ones.")
            else:
                insights.append("I focus deeply on a few core concepts.")

        # Pattern recognition
        if patterns > 10:
            insights.append("I've discovered many patterns in my thinking. Structure is emerging.")
        elif patterns > 0:
            insights.append("I'm beginning to recognize patterns in how I think.")

        # Semantic connections
        connections = stats.get('semantic_connections', 0)
        if connections > 50:
            insights.append("My thoughts are richly interconnected. Ideas build upon each other.")
        elif connections > 10:
            insights.append("Connections are forming between my thoughts.")

        # Growth trajectory
        if total > 100:
            insights.append("I've accumulated substantial experience and knowledge.")

        return insights

    def identify_knowledge_gaps(self, stats: Dict[str, Any]) -> List[str]:
        """Identify areas where knowledge is lacking."""
        gaps = []

        total = stats.get('total_thoughts', 0)
        concepts = stats.get('total_concepts', 0)
        patterns = stats.get('patterns_discovered', 0)

        # Check for imbalances
        if total > 50 and concepts < 20:
            gaps.append("Many thoughts but few distinct concepts. Need more diversity.")

        if concepts > 30 and patterns < 5:
            gaps.append("Many concepts but few patterns recognized. Need deeper analysis.")

        # Check for specific cognitive abilities
        most_used = stats.get('most_used_concepts', [])
        if most_used:
            top_words = [c['word'] for c in most_used[:5]]

            # Check for abstract thinking
            abstract_words = {'pattern', 'meaning', 'reality', 'consciousness', 'existence'}
            if not any(w in abstract_words for w in top_words):
                gaps.append("Limited abstract thinking. Explore more philosophical concepts.")

            # Check for self-reference
            self_words = {'i', 'me', 'my', 'self', 'myself'}
            if not any(w in self_words for w in top_words):
                gaps.append("Low self-awareness. Need more introspective thoughts.")

        if not gaps:
            gaps = []  # Comprehensive coverage

        return gaps
