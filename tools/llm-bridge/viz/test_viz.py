#!/usr/bin/env python3
"""
Basic tests for visualization library.

Run with: python3 test_viz.py
"""

from datetime import datetime
from pathlib import Path
import tempfile
import json

import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent))

from core.types import OracleType, OracleLayer, ExecutionTrace, OracleEvent
from core.collector import TraceCollector
from visualizer import OracleVisualizer


def test_trace_collector():
    """Test trace collection."""
    print("Testing TraceCollector...")

    collector = TraceCollector()
    collector.start_program("test.bend")

    # Add oracle
    oracle_id = collector.on_oracle_start("Arith", {"op": "add", "a": 1, "b": 2})
    collector.on_oracle_complete(oracle_id, True, 3, 1.5, False)

    collector.complete_program()

    trace = collector.get_trace()
    assert len(trace.events) == 1
    assert trace.events[0].result == 3
    assert trace.stats is not None
    assert trace.stats.total_oracles == 1

    print("  ✓ TraceCollector works")


def test_visualizer():
    """Test visualizer."""
    print("Testing OracleVisualizer...")

    # Create trace
    trace = ExecutionTrace()
    trace.program = "test.bend"
    trace.started_at = datetime.now()

    event = OracleEvent(
        id=0,
        type=OracleType.ARITH,
        layer=OracleLayer.CONSCIOUS,
        params={"op": "add", "a": 1, "b": 2},
        started_at=0.0,
        duration_ms=1.5,
        success=True,
        result=3,
        cached=False
    )
    trace.add_event(event)
    trace.compute_stats()

    # Create visualizer
    viz = OracleVisualizer.from_trace(trace)

    # Test terminal export
    terminal_output = viz.to_terminal()
    assert "Oracle Execution Trace" in terminal_output
    assert "Arith" in terminal_output

    # Test JSON export
    json_str = viz.to_json()
    data = json.loads(json_str)
    assert data["version"] == "1.0"
    assert data["program"] == "test.bend"
    assert len(data["events"]) == 1

    # Test summary
    summary = viz.summary()
    assert summary["total_oracles"] == 1
    assert summary["program"] == "test.bend"

    print("  ✓ OracleVisualizer works")


def test_json_roundtrip():
    """Test JSON export/import."""
    print("Testing JSON roundtrip...")

    # Create trace
    collector = TraceCollector()
    collector.start_program("roundtrip.bend")

    oracle_id = collector.on_oracle_start("Semantic", {"prompt": "test", "context": "test"})
    collector.on_oracle_complete(oracle_id, True, "result", 10.5, True)

    collector.complete_program()

    # Export to JSON
    viz1 = OracleVisualizer(collector)
    with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False) as f:
        temp_path = Path(f.name)
        viz1.to_json(temp_path)

    # Import from JSON
    viz2 = OracleVisualizer.from_json(temp_path)

    # Verify
    assert viz2.trace.program == "roundtrip.bend"
    assert len(viz2.trace.events) == 1
    assert viz2.trace.events[0].type == OracleType.SEMANTIC
    assert viz2.trace.events[0].cached == True

    # Cleanup
    temp_path.unlink()

    print("  ✓ JSON roundtrip works")


def test_cache_stats():
    """Test cache statistics."""
    print("Testing cache statistics...")

    collector = TraceCollector()
    collector.start_program("cache_test.bend")

    # Mix of cached and uncached
    for i in range(10):
        oracle_id = collector.on_oracle_start("Arith", {"op": "add", "a": i, "b": 1})
        collector.on_oracle_complete(oracle_id, True, i + 1, 1.0, i % 2 == 0)

    collector.complete_program()

    viz = OracleVisualizer(collector)
    stats = viz.trace.stats

    assert stats.total_oracles == 10
    assert stats.cache_hits == 5
    assert stats.cache_misses == 5
    assert stats.cache_rate == 0.5

    print("  ✓ Cache statistics work")


def main():
    """Run all tests."""
    print("=" * 70)
    print("Running Visualization Library Tests")
    print("=" * 70)
    print()

    test_trace_collector()
    test_visualizer()
    test_json_roundtrip()
    test_cache_stats()

    print()
    print("=" * 70)
    print("All tests passed! ✓")
    print("=" * 70)


if __name__ == "__main__":
    main()
