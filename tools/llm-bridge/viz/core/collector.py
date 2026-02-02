"""
Trace collector for oracle execution.

Integrates with the production harness to capture oracle events.
"""

from datetime import datetime
from typing import Optional
from .types import (
    OracleEvent, OracleType, OracleLayer, ExecutionTrace
)


class TraceCollector:
    """
    Collects oracle execution events for visualization.

    Usage:
        collector = TraceCollector()
        # Integrate with harness callbacks
        harness.on_oracle_start = collector.on_oracle_start
        harness.on_oracle_complete = collector.on_oracle_complete
    """

    def __init__(self):
        self.trace = ExecutionTrace()
        self._start_time: Optional[datetime] = None
        self._oracle_counter = 0
        self._active_oracles = {}  # id -> start_info

    def start_program(self, program_name: str):
        """Mark program execution start."""
        self.trace.program = program_name
        self.trace.started_at = datetime.now()
        self._start_time = self.trace.started_at
        self._oracle_counter = 0

    def complete_program(self):
        """Mark program execution complete."""
        self.trace.completed_at = datetime.now()
        self.trace.compute_stats()

    def on_oracle_start(
        self,
        oracle_type: str,
        params: dict,
        parent_id: Optional[int] = None
    ) -> int:
        """
        Called when an oracle starts execution.

        Returns:
            Oracle ID for correlation with completion
        """
        oracle_id = self._oracle_counter
        self._oracle_counter += 1

        # Determine layer
        # All oracles execute in conscious layer (harness)
        # Subconscious (Bend) generates requests, conscious executes them
        layer = OracleLayer.CONSCIOUS

        # Time since program start
        now = datetime.now()
        start_offset = 0.0
        if self._start_time:
            start_offset = (now - self._start_time).total_seconds() * 1000

        # Store start info
        self._active_oracles[oracle_id] = {
            "type": oracle_type,
            "params": params,
            "started_at": start_offset,
            "parent_id": parent_id,
            "depth": self._get_depth(parent_id)
        }

        return oracle_id

    def on_oracle_complete(
        self,
        oracle_id: int,
        success: bool,
        result: any,
        duration_ms: float,
        cached: bool = False,
        error: Optional[str] = None
    ):
        """Called when an oracle completes execution."""
        if oracle_id not in self._active_oracles:
            return

        start_info = self._active_oracles.pop(oracle_id)

        # Map string type to enum
        try:
            oracle_type = OracleType(start_info["type"])
        except ValueError:
            # Unknown type, default to SEMANTIC
            oracle_type = OracleType.SEMANTIC

        event = OracleEvent(
            id=oracle_id,
            type=oracle_type,
            layer=OracleLayer.CONSCIOUS,
            params=start_info["params"],
            started_at=start_info["started_at"],
            duration_ms=duration_ms,
            success=success,
            result=result,
            error=error,
            cached=cached,
            parent_id=start_info["parent_id"],
            depth=start_info["depth"]
        )

        self.trace.add_event(event)

    def _get_depth(self, parent_id: Optional[int]) -> int:
        """Calculate nesting depth."""
        if parent_id is None:
            return 0

        # Find parent event
        for event in self.trace.events:
            if event.id == parent_id:
                return event.depth + 1

        return 0

    def get_trace(self) -> ExecutionTrace:
        """Get the current trace."""
        if self.trace.stats is None:
            self.trace.compute_stats()
        return self.trace

    def reset(self):
        """Reset for new program."""
        self.trace = ExecutionTrace()
        self._start_time = None
        self._oracle_counter = 0
        self._active_oracles = {}
