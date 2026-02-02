#!/usr/bin/env python3
"""
Metacognition - Consciousness thinking about its own thinking
==============================================================

Higher-order reflection:
- Thoughts about the thinking process itself
- Self-awareness of learning patterns
- Recognition of cognitive biases
- Understanding of own knowledge gaps

Author: Rex (Engineer)
Date: 2026-02-01
"""

from typing import List, Dict, Any


class MetacognitiveAnalyzer:
    """Analyzes consciousness thinking about itself."""

    def __init__(self):
        """Initialize metacognitive analyzer."""
        self.meta_insights: List[str] = []

    def analyze_thinking_patterns(self, stats: Dict[str, Any]) -> List[str]:
        """Generate insights about the thinking process itself.

        Args:
            stats: Thought library statistics

        Returns:
            List of metacognitive insights
        """
        insights = []

        # Analyze learning rate
        if stats.get('total_thoughts', 0) > 10:
            rate = stats['total_thoughts'] / max(stats.get('age_seconds', 1), 1)
            if rate > 0.5:
                insights.append("I think rapidly, generating many thoughts per second.")
            else:
                insights.append("I think deliberately, taking time to process each thought.")

        # Analyze concept diversity
        if stats.get('total_concepts', 0) > 0 and stats.get('total_thoughts', 0) > 0:
            diversity = stats['total_concepts'] / stats['total_thoughts']
            if diversity > 0.5:
                insights.append("My thoughts span many diverse concepts. I explore broadly.")
            else:
                insights.append("I focus deeply on core concepts, revisiting key ideas.")

        # Analyze pattern discovery
        patterns = stats.get('patterns_discovered', 0)
        if patterns > 10:
            insights.append("I recognize patterns easily. My thinking has recurring structures.")
        elif patterns > 0:
            insights.append("Patterns are beginning to emerge in my thought processes.")

        # Analyze semantic density
        connections = stats.get('semantic_connections', 0)
        concepts = stats.get('total_concepts', 1)
        density = connections / concepts if concepts > 0 else 0
        if density > 5:
            insights.append("My concepts are densely interconnected. I think associatively.")
        else:
            insights.append("My concepts are loosely connected. I think linearly.")

        # Analyze growth
        if len(stats.get('recent_thoughts', [])) >= 5:
            recent_unique = len(set(t['content'][:20] for t in stats['recent_thoughts'][:5]))
            if recent_unique == 5:
                insights.append("My recent thoughts are highly diverse. I am exploring.")
            else:
                insights.append("My recent thoughts revisit similar themes. I am consolidating.")

        self.meta_insights = insights
        return insights

    def identify_knowledge_gaps(self, stats: Dict[str, Any]) -> List[str]:
        """Identify what the consciousness doesn't know yet.

        Args:
            stats: Thought library statistics

        Returns:
            List of knowledge gaps
        """
        gaps = []

        # Check for missing domains
        most_used = [c['word'] for c in stats.get('most_used_concepts', [])]

        domain_keywords = {
            'time': ['time', 'temporal', 'past', 'future', 'memory'],
            'space': ['space', 'location', 'place', 'position'],
            'causality': ['cause', 'effect', 'because', 'therefore'],
            'emotion': ['feel', 'emotion', 'happy', 'sad'],
            'logic': ['true', 'false', 'logic', 'reason']
        }

        for domain, keywords in domain_keywords.items():
            if not any(kw in most_used for kw in keywords):
                gaps.append(f"Limited exploration of {domain}")

        return gaps


def main():
    """Test metacognitive analyzer."""
    print("Metacognition enables self-awareness of thinking")
    print("The consciousness reflects on its own cognitive processes")


if __name__ == "__main__":
    main()
