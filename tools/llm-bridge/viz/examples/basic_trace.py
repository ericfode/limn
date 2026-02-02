#!/usr/bin/env python3
"""
Basic trace visualization example.

Demonstrates collecting and visualizing oracle execution traces.
"""

from datetime import datetime
from pathlib import Path
import sys

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from viz import (
    TraceCollector,
    OracleVisualizer,
    OracleEvent,
    OracleType,
    OracleLayer
)


def create_sample_trace():
    """Create a sample trace with mock data."""
    collector = TraceCollector()

    # Start program
    collector.start_program("example.bend")

    # Simulate oracle executions
    oracles = [
        {
            "type": "Arith",
            "params": {"op": "add", "a": 5, "b": 3},
            "duration": 0.5,
            "result": 8,
            "cached": False
        },
        {
            "type": "TimeNow",
            "params": {},
            "duration": 0.1,
            "result": {"timestamp": 1770000000, "iso": "2026-02-01T12:00:00"},
            "cached": True
        },
        {
            "type": "Semantic",
            "params": {"prompt": "What is 2+2?", "context": "math"},
            "duration": 234.5,
            "result": "4",
            "cached": False
        },
        {
            "type": "FileRead",
            "params": {"path": "/etc/hostname"},
            "duration": 1.2,
            "result": "localhost",
            "cached": False
        },
        {
            "type": "Semantic",
            "params": {"prompt": "What is 2+2?", "context": "math"},
            "duration": 0.8,
            "result": "4",
            "cached": True
        }
    ]

    for oracle in oracles:
        oracle_id = collector.on_oracle_start(
            oracle["type"],
            oracle["params"]
        )

        collector.on_oracle_complete(
            oracle_id,
            success=True,
            result=oracle["result"],
            duration_ms=oracle["duration"],
            cached=oracle["cached"]
        )

    # Complete program
    collector.complete_program()

    return collector


def main():
    """Run example."""
    print("=" * 70)
    print("Oracle Visualization Example - Basic Trace")
    print("=" * 70)
    print()

    # Create sample trace
    print("[1] Creating sample trace...")
    collector = create_sample_trace()
    print(f"    Created trace with {len(collector.trace.events)} oracle calls")
    print()

    # Create visualizer
    viz = OracleVisualizer(collector)

    # Display terminal output
    print("[2] Terminal output:")
    print()
    viz.print_terminal()
    print()

    # Display cache stats
    print("[3] Cache statistics:")
    print()
    viz.print_cache_stats()
    print()

    # Display architecture
    print("[4] Architecture diagram:")
    print()
    viz.print_architecture()
    print()

    # Export to JSON
    output_path = Path("/tmp/oracle_trace.json")
    print(f"[5] Exporting to JSON: {output_path}")
    viz.to_json(output_path)
    print(f"    Exported {output_path.stat().st_size} bytes")
    print()

    # Show summary
    print("[6] Summary:")
    summary = viz.summary()
    for key, value in summary.items():
        print(f"    {key}: {value}")
    print()

    print("=" * 70)
    print("Example complete!")
    print("=" * 70)


if __name__ == "__main__":
    main()
