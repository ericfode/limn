#!/usr/bin/env python3
"""
Metrics & Monitoring Engine
============================

Track oracle performance, usage patterns, and system health.

Metrics collected:
- Oracle execution times (p50, p95, p99)
- Cache hit rates per oracle type
- Error rates and failure modes
- Throughput (oracles/second)
- Resource usage (memory, CPU)
- Composition patterns
- Hot paths and bottlenecks

Author: Rex (Engineer)
Date: 2026-02-01
"""

import time
import statistics
from typing import Dict, List, Any, Optional
from dataclasses import dataclass, field
from collections import defaultdict, deque
from datetime import datetime, timedelta


@dataclass
class OracleMetric:
    """Metrics for a single oracle execution."""
    oracle_type: str
    duration_ms: float
    cached: bool
    success: bool
    error: Optional[str] = None
    timestamp: datetime = field(default_factory=datetime.now)
    params_size: int = 0  # Size of parameters
    result_size: int = 0  # Size of result


@dataclass
class AggregateMetrics:
    """Aggregated metrics for analysis."""
    oracle_type: str
    count: int
    success_rate: float
    cache_hit_rate: float
    avg_duration_ms: float
    p50_duration_ms: float
    p95_duration_ms: float
    p99_duration_ms: float
    total_time_ms: float
    errors: List[str] = field(default_factory=list)


class MetricsEngine:
    """Collect and analyze system metrics."""

    def __init__(self, window_size: int = 1000, retention_hours: int = 24):
        """Initialize metrics engine.

        Args:
            window_size: Number of recent metrics to keep in memory
            retention_hours: Hours to retain detailed metrics
        """
        self.window_size = window_size
        self.retention = timedelta(hours=retention_hours)

        # Recent metrics (rolling window)
        self.recent_metrics = deque(maxlen=window_size)

        # Aggregated counters
        self.counters = defaultdict(int)  # metric_name -> count
        self.timers = defaultdict(list)   # oracle_type -> [durations]

        # Error tracking
        self.errors = defaultdict(list)   # oracle_type -> [errors]

        # Start time for uptime
        self.start_time = datetime.now()

        # Performance tracking
        self.oracle_frequency = defaultdict(int)  # oracle_type -> call count
        self.oracle_last_called = {}               # oracle_type -> last timestamp

    def record(self, metric: OracleMetric):
        """Record a single oracle metric.

        Args:
            metric: Oracle metric to record
        """
        # Add to recent window
        self.recent_metrics.append(metric)

        # Update counters
        self.counters[f"{metric.oracle_type}.total"] += 1
        if metric.success:
            self.counters[f"{metric.oracle_type}.success"] += 1
        else:
            self.counters[f"{metric.oracle_type}.failure"] += 1
            self.errors[metric.oracle_type].append(metric.error or "Unknown error")

        if metric.cached:
            self.counters[f"{metric.oracle_type}.cached"] += 1

        # Update timers
        self.timers[metric.oracle_type].append(metric.duration_ms)

        # Keep timers bounded
        if len(self.timers[metric.oracle_type]) > self.window_size:
            self.timers[metric.oracle_type] = self.timers[metric.oracle_type][-self.window_size:]

        # Track frequency
        self.oracle_frequency[metric.oracle_type] += 1
        self.oracle_last_called[metric.oracle_type] = metric.timestamp

    def get_aggregate(self, oracle_type: str) -> Optional[AggregateMetrics]:
        """Get aggregated metrics for an oracle type.

        Args:
            oracle_type: Type of oracle

        Returns:
            Aggregated metrics or None
        """
        if oracle_type not in self.timers:
            return None

        durations = self.timers[oracle_type]
        if not durations:
            return None

        total = self.counters[f"{oracle_type}.total"]
        successes = self.counters[f"{oracle_type}.success"]
        cached = self.counters[f"{oracle_type}.cached"]

        # Calculate percentiles
        sorted_durations = sorted(durations)
        n = len(sorted_durations)

        p50_idx = int(n * 0.50)
        p95_idx = int(n * 0.95)
        p99_idx = int(n * 0.99)

        return AggregateMetrics(
            oracle_type=oracle_type,
            count=total,
            success_rate=successes / total if total > 0 else 0,
            cache_hit_rate=cached / total if total > 0 else 0,
            avg_duration_ms=statistics.mean(durations),
            p50_duration_ms=sorted_durations[p50_idx] if n > 0 else 0,
            p95_duration_ms=sorted_durations[p95_idx] if n > 0 else 0,
            p99_duration_ms=sorted_durations[p99_idx] if n > 0 else 0,
            total_time_ms=sum(durations),
            errors=self.errors[oracle_type][-10:]  # Last 10 errors
        )

    def get_all_aggregates(self) -> List[AggregateMetrics]:
        """Get aggregated metrics for all oracle types.

        Returns:
            List of aggregated metrics
        """
        return [
            self.get_aggregate(oracle_type)
            for oracle_type in self.timers.keys()
            if self.get_aggregate(oracle_type) is not None
        ]

    def get_throughput(self, window_seconds: int = 60) -> Dict[str, float]:
        """Calculate throughput (oracles/second).

        Args:
            window_seconds: Time window to calculate over

        Returns:
            Throughput metrics
        """
        cutoff = datetime.now() - timedelta(seconds=window_seconds)

        # Count recent metrics
        recent = [m for m in self.recent_metrics if m.timestamp > cutoff]

        if not recent:
            return {
                "total_ops": 0.0,
                "successful_ops": 0.0,
                "failed_ops": 0.0,
                "cached_ops": 0.0
            }

        elapsed = (datetime.now() - recent[0].timestamp).total_seconds()
        if elapsed == 0:
            elapsed = 1

        total = len(recent)
        successful = sum(1 for m in recent if m.success)
        failed = total - successful
        cached = sum(1 for m in recent if m.cached)

        return {
            "total_ops": total / elapsed,
            "successful_ops": successful / elapsed,
            "failed_ops": failed / elapsed,
            "cached_ops": cached / elapsed
        }

    def get_hot_paths(self, top_n: int = 10) -> List[Dict[str, Any]]:
        """Identify hot paths (most frequently called oracles).

        Args:
            top_n: Number of top oracles to return

        Returns:
            List of hot path info
        """
        hot_paths = sorted(
            self.oracle_frequency.items(),
            key=lambda x: x[1],
            reverse=True
        )[:top_n]

        return [
            {
                "oracle_type": oracle_type,
                "call_count": count,
                "last_called": self.oracle_last_called.get(oracle_type),
                "avg_duration_ms": statistics.mean(self.timers[oracle_type]) if self.timers[oracle_type] else 0
            }
            for oracle_type, count in hot_paths
        ]

    def get_bottlenecks(self, top_n: int = 5) -> List[Dict[str, Any]]:
        """Identify bottlenecks (slowest oracles).

        Args:
            top_n: Number of bottlenecks to return

        Returns:
            List of bottleneck info
        """
        # Calculate average duration for each oracle type
        avg_durations = [
            (oracle_type, statistics.mean(durations))
            for oracle_type, durations in self.timers.items()
            if durations
        ]

        # Sort by duration (slowest first)
        bottlenecks = sorted(avg_durations, key=lambda x: x[1], reverse=True)[:top_n]

        return [
            {
                "oracle_type": oracle_type,
                "avg_duration_ms": avg_duration,
                "call_count": self.oracle_frequency[oracle_type],
                "total_time_ms": sum(self.timers[oracle_type]),
                "percentage_of_total": (sum(self.timers[oracle_type]) /
                                       sum(sum(t) for t in self.timers.values()) * 100
                                       if sum(sum(t) for t in self.timers.values()) > 0 else 0)
            }
            for oracle_type, avg_duration in bottlenecks
        ]

    def get_error_report(self) -> Dict[str, Any]:
        """Generate error report.

        Returns:
            Error statistics
        """
        total_errors = sum(self.counters[f"{k}.failure"] for k in self.timers.keys())
        total_calls = sum(self.counters[f"{k}.total"] for k in self.timers.keys())

        error_by_type = {
            oracle_type: {
                "count": self.counters[f"{oracle_type}.failure"],
                "rate": (self.counters[f"{oracle_type}.failure"] /
                        self.counters[f"{oracle_type}.total"] * 100
                        if self.counters[f"{oracle_type}.total"] > 0 else 0),
                "recent_errors": self.errors[oracle_type][-5:]
            }
            for oracle_type in self.timers.keys()
            if self.counters[f"{oracle_type}.failure"] > 0
        }

        return {
            "total_errors": total_errors,
            "total_calls": total_calls,
            "overall_error_rate": (total_errors / total_calls * 100 if total_calls > 0 else 0),
            "errors_by_type": error_by_type
        }

    def get_dashboard(self) -> Dict[str, Any]:
        """Generate comprehensive dashboard data.

        Returns:
            Dashboard metrics
        """
        uptime = (datetime.now() - self.start_time).total_seconds()

        return {
            "uptime_seconds": uptime,
            "total_oracles": sum(self.counters[f"{k}.total"] for k in self.timers.keys()),
            "success_rate": (
                sum(self.counters[f"{k}.success"] for k in self.timers.keys()) /
                sum(self.counters[f"{k}.total"] for k in self.timers.keys()) * 100
                if sum(self.counters[f"{k}.total"] for k in self.timers.keys()) > 0 else 0
            ),
            "cache_hit_rate": (
                sum(self.counters[f"{k}.cached"] for k in self.timers.keys()) /
                sum(self.counters[f"{k}.total"] for k in self.timers.keys()) * 100
                if sum(self.counters[f"{k}.total"] for k in self.timers.keys()) > 0 else 0
            ),
            "throughput": self.get_throughput(),
            "hot_paths": self.get_hot_paths(),
            "bottlenecks": self.get_bottlenecks(),
            "errors": self.get_error_report(),
            "aggregates": [
                {
                    "oracle_type": agg.oracle_type,
                    "count": agg.count,
                    "success_rate": f"{agg.success_rate * 100:.1f}%",
                    "cache_hit_rate": f"{agg.cache_hit_rate * 100:.1f}%",
                    "p50_ms": f"{agg.p50_duration_ms:.2f}",
                    "p95_ms": f"{agg.p95_duration_ms:.2f}",
                    "p99_ms": f"{agg.p99_duration_ms:.2f}"
                }
                for agg in self.get_all_aggregates()
            ]
        }

    def export_prometheus(self) -> str:
        """Export metrics in Prometheus format.

        Returns:
            Prometheus-formatted metrics
        """
        lines = []

        # Total oracle calls
        for oracle_type in self.timers.keys():
            total = self.counters[f"{oracle_type}.total"]
            lines.append(f'oracle_calls_total{{type="{oracle_type}"}} {total}')

        # Success/failure counts
        for oracle_type in self.timers.keys():
            success = self.counters[f"{oracle_type}.success"]
            failure = self.counters[f"{oracle_type}.failure"]
            lines.append(f'oracle_success_total{{type="{oracle_type}"}} {success}')
            lines.append(f'oracle_failure_total{{type="{oracle_type}"}} {failure}')

        # Cache hits
        for oracle_type in self.timers.keys():
            cached = self.counters[f"{oracle_type}.cached"]
            lines.append(f'oracle_cache_hits_total{{type="{oracle_type}"}} {cached}')

        # Duration percentiles
        for oracle_type in self.timers.keys():
            agg = self.get_aggregate(oracle_type)
            if agg:
                lines.append(f'oracle_duration_ms{{type="{oracle_type}",quantile="0.5"}} {agg.p50_duration_ms}')
                lines.append(f'oracle_duration_ms{{type="{oracle_type}",quantile="0.95"}} {agg.p95_duration_ms}')
                lines.append(f'oracle_duration_ms{{type="{oracle_type}",quantile="0.99"}} {agg.p99_duration_ms}')

        return '\n'.join(lines)


def main():
    """Test metrics engine."""
    print("=== Metrics & Monitoring Engine ===\n")

    engine = MetricsEngine()

    # Simulate some oracle executions
    for i in range(100):
        metric = OracleMetric(
            oracle_type="Semantic",
            duration_ms=100 + (i % 50),
            cached=i % 3 == 0,
            success=i % 10 != 0
        )
        engine.record(metric)

    # Get dashboard
    dashboard = engine.get_dashboard()

    print(f"Uptime: {dashboard['uptime_seconds']:.2f}s")
    print(f"Total oracles: {dashboard['total_oracles']}")
    print(f"Success rate: {dashboard['success_rate']:.1f}%")
    print(f"Cache hit rate: {dashboard['cache_hit_rate']:.1f}%")

    print("\nThroughput:")
    for key, value in dashboard['throughput'].items():
        print(f"  {key}: {value:.2f} ops/s")

    print("\nHot paths:")
    for path in dashboard['hot_paths'][:5]:
        print(f"  {path['oracle_type']}: {path['call_count']} calls, {path['avg_duration_ms']:.2f}ms avg")


if __name__ == "__main__":
    main()
