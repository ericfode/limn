"""
Core data types for oracle visualization.

Defines the schema for oracle execution traces and related metadata.
"""

from dataclasses import dataclass, field, asdict
from typing import List, Dict, Any, Optional
from datetime import datetime
from enum import Enum


class OracleLayer(Enum):
    """Which layer of the consciousness architecture."""
    SUBCONSCIOUS = "subconscious"  # Bend/HVM - pure computation
    CONSCIOUS = "conscious"         # Harness - side effects


class OracleType(Enum):
    """All supported oracle types."""
    # Semantic (~ operator)
    SEMANTIC = "Semantic"

    # Arithmetic
    ARITH = "Arith"

    # Filesystem (∎ ground truth)
    FILE_READ = "FileRead"
    FILE_WRITE = "FileWrite"
    FILE_EXISTS = "FileExists"

    # Temporal (∿ time)
    TIME_NOW = "TimeNow"
    TIME_AT = "TimeAt"
    TIME_DELTA = "TimeDelta"

    # Database (∎ persisted state)
    DB_QUERY = "DbQuery"
    DB_WRITE = "DbWrite"

    # Network (∎ external data)
    HTTP_GET = "HttpGet"
    HTTP_POST = "HttpPost"

    # Memory (∿ context)
    MEMORY_STORE = "MemoryStore"
    MEMORY_RETRIEVE = "MemoryRetrieve"


@dataclass
class OracleEvent:
    """A single oracle execution event."""

    # Identity
    id: int
    type: OracleType
    layer: OracleLayer

    # Parameters
    params: Dict[str, Any]

    # Timing
    started_at: float  # Milliseconds from program start
    duration_ms: float

    # Result
    success: bool
    result: Any
    error: Optional[str] = None

    # Performance
    cached: bool = False

    # Context
    parent_id: Optional[int] = None  # For nested oracle calls
    depth: int = 0

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for serialization."""
        return {
            "id": self.id,
            "type": self.type.value,
            "layer": self.layer.value,
            "params": self.params,
            "started_at": self.started_at,
            "duration_ms": self.duration_ms,
            "success": self.success,
            "result": str(self.result) if self.result is not None else None,
            "error": self.error,
            "cached": self.cached,
            "parent_id": self.parent_id,
            "depth": self.depth
        }


@dataclass
class ExecutionStats:
    """Aggregate statistics for an execution."""

    total_oracles: int
    total_duration_ms: float
    cache_hits: int
    cache_misses: int

    # By type
    by_type: Dict[str, int] = field(default_factory=dict)

    # By layer
    subconscious_oracles: int = 0
    conscious_oracles: int = 0

    # Performance
    fastest_oracle_ms: float = float('inf')
    slowest_oracle_ms: float = 0.0
    avg_oracle_ms: float = 0.0

    @property
    def cache_rate(self) -> float:
        """Cache hit rate."""
        total = self.cache_hits + self.cache_misses
        return self.cache_hits / total if total > 0 else 0.0

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary."""
        return {
            "total_oracles": self.total_oracles,
            "total_duration_ms": self.total_duration_ms,
            "cache_hits": self.cache_hits,
            "cache_misses": self.cache_misses,
            "cache_rate": self.cache_rate,
            "by_type": self.by_type,
            "subconscious_oracles": self.subconscious_oracles,
            "conscious_oracles": self.conscious_oracles,
            "fastest_oracle_ms": self.fastest_oracle_ms,
            "slowest_oracle_ms": self.slowest_oracle_ms,
            "avg_oracle_ms": self.avg_oracle_ms
        }


@dataclass
class ExecutionTrace:
    """Complete execution trace."""

    # Metadata
    version: str = "1.0"
    program: Optional[str] = None
    started_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None

    # Events
    events: List[OracleEvent] = field(default_factory=list)

    # Stats
    stats: Optional[ExecutionStats] = None

    def add_event(self, event: OracleEvent):
        """Add an oracle event."""
        self.events.append(event)

    def compute_stats(self) -> ExecutionStats:
        """Compute aggregate statistics."""
        if not self.events:
            return ExecutionStats(
                total_oracles=0,
                total_duration_ms=0.0,
                cache_hits=0,
                cache_misses=0
            )

        by_type: Dict[str, int] = {}
        total_duration = 0.0
        cache_hits = 0
        cache_misses = 0
        conscious_count = 0
        subconscious_count = 0
        fastest = float('inf')
        slowest = 0.0

        for event in self.events:
            # Count by type
            type_name = event.type.value
            by_type[type_name] = by_type.get(type_name, 0) + 1

            # Timing
            total_duration += event.duration_ms
            fastest = min(fastest, event.duration_ms)
            slowest = max(slowest, event.duration_ms)

            # Cache
            if event.cached:
                cache_hits += 1
            else:
                cache_misses += 1

            # Layer
            if event.layer == OracleLayer.CONSCIOUS:
                conscious_count += 1
            else:
                subconscious_count += 1

        stats = ExecutionStats(
            total_oracles=len(self.events),
            total_duration_ms=total_duration,
            cache_hits=cache_hits,
            cache_misses=cache_misses,
            by_type=by_type,
            subconscious_oracles=subconscious_count,
            conscious_oracles=conscious_count,
            fastest_oracle_ms=fastest if fastest != float('inf') else 0.0,
            slowest_oracle_ms=slowest,
            avg_oracle_ms=total_duration / len(self.events)
        )

        self.stats = stats
        return stats

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for serialization."""
        if self.stats is None:
            self.compute_stats()

        return {
            "version": self.version,
            "program": self.program,
            "started_at": self.started_at.isoformat() if self.started_at else None,
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
            "total_duration_ms": (
                (self.completed_at - self.started_at).total_seconds() * 1000
                if self.started_at and self.completed_at
                else None
            ),
            "events": [event.to_dict() for event in self.events],
            "stats": self.stats.to_dict() if self.stats else None
        }
