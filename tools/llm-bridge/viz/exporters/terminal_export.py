"""Terminal (ASCII) export for visualizations."""

from typing import List, Dict, Any

try:
    from ..core.types import ExecutionTrace, OracleEvent
except ImportError:
    from core.types import ExecutionTrace, OracleEvent


class TerminalExporter:
    """Export visualizations as terminal-friendly ASCII."""

    # ANSI color codes
    BLUE = '\033[94m'
    GREEN = '\033[92m'
    YELLOW = '\033[93m'
    RED = '\033[91m'
    CYAN = '\033[96m'
    BOLD = '\033[1m'
    END = '\033[0m'

    @classmethod
    def execution_trace(cls, trace: ExecutionTrace) -> str:
        """
        Render execution trace for terminal.

        Args:
            trace: Execution trace

        Returns:
            Formatted ASCII output
        """
        lines = []
        lines.append("=" * 70)
        lines.append(f"{cls.BOLD}Oracle Execution Trace{cls.END}")
        lines.append("=" * 70)
        lines.append("")

        if trace.program:
            lines.append(f"Program: {trace.program}")
        if trace.started_at and trace.completed_at:
            duration = (trace.completed_at - trace.started_at).total_seconds() * 1000
            lines.append(f"Duration: {duration:.2f}ms")
        lines.append("")

        # Events
        for event in trace.events:
            indent = "  " * event.depth
            cache_marker = f"{cls.GREEN}[cached]{cls.END}" if event.cached else ""
            status = f"{cls.GREEN}✓{cls.END}" if event.success else f"{cls.RED}✗{cls.END}"

            lines.append(
                f"{indent}{status} {cls.CYAN}{event.type.value}{cls.END} "
                f"({event.duration_ms:.2f}ms) {cache_marker}"
            )

            # Show params (abbreviated)
            if event.params:
                param_str = cls._format_params(event.params)
                lines.append(f"{indent}   {param_str}")

            # Show result (abbreviated)
            if event.success and event.result is not None:
                result_str = str(event.result)
                if len(result_str) > 60:
                    result_str = result_str[:57] + "..."
                lines.append(f"{indent}   → {result_str}")
            elif event.error:
                lines.append(f"{indent}   {cls.RED}Error: {event.error}{cls.END}")

            lines.append("")

        # Stats
        if trace.stats:
            lines.append("=" * 70)
            lines.append(f"{cls.BOLD}Statistics{cls.END}")
            lines.append("=" * 70)
            lines.append(f"Total oracles: {trace.stats.total_oracles}")
            lines.append(f"Cache hits: {trace.stats.cache_hits}")
            lines.append(f"Cache rate: {trace.stats.cache_rate * 100:.1f}%")
            lines.append(f"Total time: {trace.stats.total_duration_ms:.2f}ms")
            lines.append(f"Avg time: {trace.stats.avg_oracle_ms:.2f}ms")
            lines.append("")
            lines.append("By type:")
            for oracle_type, count in sorted(trace.stats.by_type.items()):
                lines.append(f"  {oracle_type}: {count}")

        return "\n".join(lines)

    @classmethod
    def cache_stats(cls, trace: ExecutionTrace) -> str:
        """
        Render cache statistics for terminal.

        Args:
            trace: Execution trace

        Returns:
            Formatted ASCII output
        """
        if not trace.stats:
            trace.compute_stats()

        lines = []
        lines.append("=" * 70)
        lines.append(f"{cls.BOLD}Cache Statistics{cls.END}")
        lines.append("=" * 70)
        lines.append("")

        stats = trace.stats
        lines.append(f"Cache hits:   {cls.GREEN}{stats.cache_hits}{cls.END}")
        lines.append(f"Cache misses: {cls.RED}{stats.cache_misses}{cls.END}")
        lines.append(f"Hit rate:     {cls.CYAN}{stats.cache_rate * 100:.1f}%{cls.END}")
        lines.append("")

        # Bar chart
        total = stats.cache_hits + stats.cache_misses
        if total > 0:
            hit_width = int((stats.cache_hits / total) * 50)
            miss_width = 50 - hit_width

            lines.append("Distribution:")
            lines.append(
                f"{cls.GREEN}{'█' * hit_width}{cls.END}"
                f"{cls.RED}{'█' * miss_width}{cls.END}"
            )
            lines.append("")

        # By oracle type
        lines.append("By oracle type:")
        cache_by_type = cls._compute_cache_by_type(trace.events)
        for oracle_type, (hits, misses) in sorted(cache_by_type.items()):
            total_type = hits + misses
            rate = (hits / total_type * 100) if total_type > 0 else 0
            lines.append(f"  {oracle_type:15s} {hits:3d}/{total_type:3d} ({rate:.0f}%)")

        return "\n".join(lines)

    @classmethod
    def _format_params(cls, params: Dict[str, Any]) -> str:
        """Format parameters for display."""
        parts = []
        for key, value in params.items():
            value_str = str(value)
            if len(value_str) > 30:
                value_str = value_str[:27] + "..."
            parts.append(f"{key}={value_str}")
        return ", ".join(parts)

    @classmethod
    def _compute_cache_by_type(cls, events: List[OracleEvent]) -> Dict[str, tuple]:
        """Compute cache stats by oracle type."""
        cache_by_type = {}
        for event in events:
            type_name = event.type.value
            if type_name not in cache_by_type:
                cache_by_type[type_name] = (0, 0)

            hits, misses = cache_by_type[type_name]
            if event.cached:
                cache_by_type[type_name] = (hits + 1, misses)
            else:
                cache_by_type[type_name] = (hits, misses + 1)

        return cache_by_type

    @classmethod
    def architecture_diagram(cls) -> str:
        """
        Render consciousness architecture diagram.

        Returns:
            ASCII art diagram
        """
        return f"""
{cls.BOLD}Consciousness Architecture{cls.END}

┌─────────────────────────────────────────────────────────────┐
│  {cls.CYAN}SUBCONSCIOUS{cls.END} (Bend/HVM - Pure Computation)             │
│                                                             │
│  • Zero side effects                                        │
│  • Pure mathematical computation                            │
│  • Generates oracle requests as data structures             │
│                                                             │
└───────────────────────┬─────────────────────────────────────┘
                        │
                   Parse to JSON
                        │
                        ▼
┌─────────────────────────────────────────────────────────────┐
│  {cls.GREEN}CONSCIOUS{cls.END} (Python Harness - Impure Actions)            │
│                                                             │
│  {cls.YELLOW}Executes ALL side effects:{cls.END}                                │
│    ~ Semantic (LLM)        ∎ Files                          │
│    ∿ Time                  ∎ Database                       │
│    ∎ Network               ∿ Memory                         │
│                                                             │
│  {cls.YELLOW}Features:{cls.END}                                               │
│    • Persistent cache      • Error handling                 │
│    • Performance stats     • Resource management            │
│                                                             │
└─────────────────────────────────────────────────────────────┘

{cls.BOLD}Operators:{cls.END}
  ~  Semantic/LLM reasoning
  ∎  Ground truth (files, DB, network)
  ∿  Temporal/stateful (time, memory)
"""
