"""
Main visualizer class.

Provides high-level API for generating visualizations.
"""

from pathlib import Path
from typing import Optional, Dict, Any

try:
    from .core import ExecutionTrace, TraceCollector
    from .exporters import JsonExporter, TerminalExporter
except ImportError:
    from core.types import ExecutionTrace
    from core.collector import TraceCollector
    from exporters.json_export import JsonExporter
    from exporters.terminal_export import TerminalExporter


class OracleVisualizer:
    """
    Main interface for oracle visualizations.

    Usage:
        # From collector
        viz = OracleVisualizer(collector)

        # From trace
        viz = OracleVisualizer.from_trace(trace)

        # From JSON
        viz = OracleVisualizer.from_json("trace.json")

        # Generate visualizations
        viz.to_terminal()
        viz.to_json("output.json")
    """

    def __init__(self, source):
        """
        Initialize visualizer.

        Args:
            source: TraceCollector, ExecutionTrace, or dict
        """
        if isinstance(source, TraceCollector):
            self.trace = source.get_trace()
        elif isinstance(source, ExecutionTrace):
            self.trace = source
        elif isinstance(source, dict):
            self.trace = self._trace_from_dict(source)
        else:
            raise ValueError(f"Invalid source type: {type(source)}")

    @classmethod
    def from_trace(cls, trace: ExecutionTrace) -> "OracleVisualizer":
        """Create from execution trace."""
        return cls(trace)

    @classmethod
    def from_json(cls, json_path: Path) -> "OracleVisualizer":
        """Load from JSON file."""
        import json
        with open(json_path) as f:
            data = json.load(f)
        return cls(data)

    def to_terminal(self) -> str:
        """
        Export execution trace to terminal format.

        Returns:
            Formatted terminal output
        """
        return TerminalExporter.execution_trace(self.trace)

    def print_terminal(self):
        """Print execution trace to terminal."""
        print(self.to_terminal())

    def cache_stats_terminal(self) -> str:
        """
        Export cache stats to terminal format.

        Returns:
            Formatted terminal output
        """
        return TerminalExporter.cache_stats(self.trace)

    def print_cache_stats(self):
        """Print cache stats to terminal."""
        print(self.cache_stats_terminal())

    def architecture_diagram_terminal(self) -> str:
        """
        Export architecture diagram to terminal format.

        Returns:
            Formatted terminal output
        """
        return TerminalExporter.architecture_diagram()

    def print_architecture(self):
        """Print architecture diagram to terminal."""
        print(self.architecture_diagram_terminal())

    def to_json(self, output_path: Optional[Path] = None) -> Optional[str]:
        """
        Export trace to JSON.

        Args:
            output_path: If provided, write to file. Otherwise return string.

        Returns:
            JSON string if output_path is None
        """
        data = self.trace.to_dict()

        if output_path:
            JsonExporter.export(data, output_path)
            return None
        else:
            return JsonExporter.export_string(data)

    def summary(self) -> Dict[str, Any]:
        """
        Get summary statistics.

        Returns:
            Dictionary with summary stats
        """
        if not self.trace.stats:
            self.trace.compute_stats()

        return {
            "program": self.trace.program,
            "total_oracles": self.trace.stats.total_oracles,
            "total_duration_ms": self.trace.stats.total_duration_ms,
            "cache_rate": f"{self.trace.stats.cache_rate * 100:.1f}%",
            "avg_oracle_ms": f"{self.trace.stats.avg_oracle_ms:.2f}ms",
            "by_type": self.trace.stats.by_type
        }

    def _trace_from_dict(self, data: dict) -> ExecutionTrace:
        """Reconstruct trace from dictionary."""
        from datetime import datetime
        try:
            from .core import OracleEvent, OracleType, OracleLayer
        except ImportError:
            from core.types import OracleEvent, OracleType, OracleLayer

        trace = ExecutionTrace()
        trace.version = data.get("version", "1.0")
        trace.program = data.get("program")

        if data.get("started_at"):
            trace.started_at = datetime.fromisoformat(data["started_at"])
        if data.get("completed_at"):
            trace.completed_at = datetime.fromisoformat(data["completed_at"])

        # Reconstruct events
        for event_data in data.get("events", []):
            event = OracleEvent(
                id=event_data["id"],
                type=OracleType(event_data["type"]),
                layer=OracleLayer(event_data["layer"]),
                params=event_data["params"],
                started_at=event_data["started_at"],
                duration_ms=event_data["duration_ms"],
                success=event_data["success"],
                result=event_data.get("result"),
                error=event_data.get("error"),
                cached=event_data.get("cached", False),
                parent_id=event_data.get("parent_id"),
                depth=event_data.get("depth", 0)
            )
            trace.add_event(event)

        trace.compute_stats()
        return trace
