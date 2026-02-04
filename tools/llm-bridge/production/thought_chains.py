#!/usr/bin/env python3
"""
Thought Chains - Track how thoughts connect and build on each other
====================================================================

Analyzes the flow of thinking:
- Which thoughts led to which insights
- Causal chains of reasoning
- Thought clusters and branches
- Evolution of ideas

Author: Rex (Engineer)
Date: 2026-02-01
"""

from typing import List, Dict, Set, Any, Optional
from dataclasses import dataclass, field
import time


@dataclass
class ThoughtChain:
    """A chain of connected thoughts."""
    root_thought_idx: int
    chain: List[int]  # Indices of thoughts in order
    strength: float = 1.0
    created_at: float = field(default_factory=time.time)


class ThoughtChainAnalyzer:
    """Analyzes how thoughts connect and build on each other."""

    def __init__(self):
        """Initialize chain analyzer."""
        self.chains: List[ThoughtChain] = []

    def build_chains(self, thoughts: List[Any]) -> List[ThoughtChain]:
        """Build chains from thought connections.

        Args:
            thoughts: List of Thought objects with connections

        Returns:
            List of thought chains
        """
        chains = []
        visited = set()

        for idx, thought in enumerate(thoughts):
            if idx in visited:
                continue

            # Start a new chain from this thought
            chain = self._trace_chain(thoughts, idx, visited)
            if len(chain) > 1:  # Only keep chains with 2+ thoughts
                chains.append(ThoughtChain(
                    root_thought_idx=chain[0],
                    chain=chain,
                    strength=len(chain) / len(thoughts)
                ))

        self.chains = chains
        return chains

    def _trace_chain(self, thoughts: List[Any], start_idx: int, visited: Set[int]) -> List[int]:
        """Trace a chain starting from a thought.

        Args:
            thoughts: All thoughts
            start_idx: Starting thought index
            visited: Already visited thought indices

        Returns:
            Chain of thought indices
        """
        chain = [start_idx]
        visited.add(start_idx)

        current = thoughts[start_idx]

        # Follow connections forward
        for conn_idx in current.connections:
            if conn_idx not in visited and conn_idx < len(thoughts):
                sub_chain = self._trace_chain(thoughts, conn_idx, visited)
                chain.extend(sub_chain)

        return chain

    def get_longest_chains(self, n: int = 5) -> List[ThoughtChain]:
        """Get the n longest thought chains.

        Args:
            n: Number of chains to return

        Returns:
            Longest chains
        """
        return sorted(self.chains, key=lambda c: len(c.chain), reverse=True)[:n]

    def get_chain_statistics(self) -> Dict[str, Any]:
        """Get statistics about thought chains.

        Returns:
            Chain statistics
        """
        if not self.chains:
            return {
                "total_chains": 0,
                "avg_chain_length": 0,
                "max_chain_length": 0,
                "branching_factor": 0
            }

        lengths = [len(c.chain) for c in self.chains]

        return {
            "total_chains": len(self.chains),
            "avg_chain_length": sum(lengths) / len(lengths),
            "max_chain_length": max(lengths),
            "branching_factor": len(self.chains) / len(set(c.root_thought_idx for c in self.chains))
        }


def main():
    """Test thought chain analyzer."""
    print("Thought chain analysis would integrate with thought library")
    print("Track: root thoughts → derived insights → evolved understanding")


if __name__ == "__main__":
    main()
