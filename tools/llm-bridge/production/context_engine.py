#!/usr/bin/env python3
"""
Context Manipulation Engine
============================

Implements LMN context operations:
- red (reduce/compress)
- mer (merge patterns)
- fil (filter)
- agg (aggregate)
- com (compress)

Context is accumulated state that can be transformed.

Author: Rex (Engineer)
Date: 2026-02-01
"""

import re
from typing import List, Dict, Any, Set
from collections import Counter, defaultdict


class ContextEngine:
    """Manages and transforms accumulated context."""

    def __init__(self):
        """Initialize context engine."""
        self.context = []  # List of context items
        self.patterns = defaultdict(int)  # Pattern frequency
        self.vocabulary = set()  # All words seen

    def add(self, item: Dict[str, Any]):
        """Add item to context.

        Args:
            item: Context item (oracle result, state, etc.)
        """
        self.context.append(item)

        # Extract patterns
        if 'limn' in item:
            self._extract_patterns(item['limn'])

    def _extract_patterns(self, limn_text: str):
        """Extract and count patterns from Limn text."""
        # Extract triads (3-word patterns)
        words = re.findall(r'\b\w{3}\b', limn_text)

        for word in words:
            self.vocabulary.add(word)

        # Count triads
        for i in range(len(words) - 2):
            triad = tuple(words[i:i+3])
            self.patterns[triad] += 1

    def reduce(self, threshold: float = 0.5) -> List[Dict]:
        """Reduce context by removing low-frequency patterns.

        LMN: ~ red ctx @ threshold

        Args:
            threshold: Keep items with pattern frequency >= this

        Returns:
            Reduced context
        """
        if not self.context:
            return []

        # Calculate frequency threshold
        max_freq = max(self.patterns.values()) if self.patterns else 1
        min_freq = max_freq * threshold

        # Keep only high-frequency patterns
        important_patterns = {
            pattern for pattern, freq in self.patterns.items()
            if freq >= min_freq
        }

        # Filter context
        reduced = []
        for item in self.context:
            if self._contains_important_pattern(item, important_patterns):
                reduced.append(item)

        return reduced

    def _contains_important_pattern(self, item: Dict, patterns: Set) -> bool:
        """Check if item contains important patterns."""
        if 'limn' not in item:
            return False

        words = re.findall(r'\b\w{3}\b', item['limn'])

        for i in range(len(words) - 2):
            triad = tuple(words[i:i+3])
            if triad in patterns:
                return True

        return False

    def merge(self, similarity_threshold: float = 0.7) -> List[Dict]:
        """Merge similar context items.

        LMN: ~ mer ctx @ threshold

        Args:
            similarity_threshold: Merge items with similarity >= this

        Returns:
            Merged context
        """
        if not self.context:
            return []

        merged = []
        used = set()

        for i, item1 in enumerate(self.context):
            if i in used:
                continue

            # Find similar items
            similar = [item1]
            for j, item2 in enumerate(self.context[i+1:], start=i+1):
                if j not in used:
                    similarity = self._calculate_similarity(item1, item2)
                    if similarity >= similarity_threshold:
                        similar.append(item2)
                        used.add(j)

            # Merge similar items
            merged_item = self._merge_items(similar)
            merged.append(merged_item)
            used.add(i)

        return merged

    def _calculate_similarity(self, item1: Dict, item2: Dict) -> float:
        """Calculate similarity between two items."""
        if 'limn' not in item1 or 'limn' not in item2:
            return 0.0

        words1 = set(re.findall(r'\b\w{3}\b', item1['limn']))
        words2 = set(re.findall(r'\b\w{3}\b', item2['limn']))

        if not words1 or not words2:
            return 0.0

        # Jaccard similarity
        intersection = len(words1 & words2)
        union = len(words1 | words2)

        return intersection / union if union > 0 else 0.0

    def _merge_items(self, items: List[Dict]) -> Dict:
        """Merge multiple items into one."""
        if len(items) == 1:
            return items[0]

        # Combine Limn representations
        limn_parts = [item.get('limn', '') for item in items]
        merged_limn = ' | '.join(limn_parts)

        # Combine metadata
        merged = {
            'limn': merged_limn,
            'count': len(items),
            'type': 'merged',
            'original_types': [item.get('type') for item in items]
        }

        return merged

    def filter(self, predicate: str) -> List[Dict]:
        """Filter context by predicate.

        LMN: ~ fil ctx @ predicate

        Args:
            predicate: Filter criterion (word or pattern)

        Returns:
            Filtered context
        """
        filtered = []

        for item in self.context:
            if 'limn' in item and predicate in item['limn']:
                filtered.append(item)

        return filtered

    def aggregate(self, group_by: str = 'type') -> Dict[str, List[Dict]]:
        """Aggregate context by attribute.

        LMN: ~ agg ctx @ attribute

        Args:
            group_by: Attribute to group by

        Returns:
            Grouped context
        """
        groups = defaultdict(list)

        for item in self.context:
            key = item.get(group_by, 'unknown')
            groups[key].append(item)

        return dict(groups)

    def compress(self, target_size: int = None) -> List[Dict]:
        """Compress context to target size.

        LMN: ~ com ctx @ size

        Uses combination of reduce and merge to reach target size.

        Args:
            target_size: Target number of items (default: len/2)

        Returns:
            Compressed context
        """
        if not self.context:
            return []

        if target_size is None:
            target_size = max(1, len(self.context) // 2)

        # Start with merge
        compressed = self.merge(similarity_threshold=0.6)

        # If still too large, reduce
        while len(compressed) > target_size:
            # Increase threshold progressively
            threshold = 0.5 + (len(compressed) - target_size) / len(compressed) * 0.4
            engine = ContextEngine()
            engine.context = compressed
            engine.patterns = self.patterns
            compressed = engine.reduce(threshold=threshold)

            # Prevent infinite loop
            if len(compressed) == len(engine.context):
                break

        return compressed[:target_size]

    def get_summary(self) -> Dict[str, Any]:
        """Get context summary statistics."""
        return {
            'total_items': len(self.context),
            'unique_patterns': len(self.patterns),
            'vocabulary_size': len(self.vocabulary),
            'most_common_patterns': [
                {
                    'pattern': ' '.join(p),
                    'frequency': f
                }
                for p, f in Counter(self.patterns).most_common(10)
            ],
            'context_size_bytes': sum(
                len(str(item)) for item in self.context
            )
        }

    def transform(self, operation: str, **kwargs) -> List[Dict]:
        """Execute context transformation operation.

        LMN: ~ <operation> ctx @ params

        Args:
            operation: red/mer/fil/agg/com
            **kwargs: Operation-specific parameters

        Returns:
            Transformed context
        """
        operations = {
            'red': self.reduce,
            'mer': self.merge,
            'fil': self.filter,
            'agg': self.aggregate,
            'com': self.compress
        }

        if operation in operations:
            return operations[operation](**kwargs)

        raise ValueError(f"Unknown operation: {operation}")


def main():
    """Test context engine."""
    engine = ContextEngine()

    # Add some test context
    test_items = [
        {'limn': 'sys run | ora exe | tim now', 'type': 'state'},
        {'limn': 'sys run | ora exe | fil rea', 'type': 'state'},
        {'limn': 'tim now | clk get | val ret', 'type': 'oracle'},
        {'limn': 'fil rea | dat lod | con ret', 'type': 'oracle'},
        {'limn': 'sys run | ctx acc | val mat', 'type': 'state'},
    ]

    for item in test_items:
        engine.add(item)

    print("Context Summary:")
    print(engine.get_summary())

    print("\nReduced Context (threshold=0.5):")
    reduced = engine.reduce(threshold=0.5)
    for item in reduced:
        print(f"  {item['limn']}")

    print("\nMerged Context:")
    merged = engine.merge(similarity_threshold=0.3)
    for item in merged:
        print(f"  {item['limn']} [count: {item.get('count', 1)}]")

    print("\nFiltered Context (contains 'ora'):")
    filtered = engine.filter('ora')
    for item in filtered:
        print(f"  {item['limn']}")

    print("\nCompressed Context (target=2):")
    compressed = engine.compress(target_size=2)
    for item in compressed:
        print(f"  {item['limn']}")


if __name__ == "__main__":
    main()
