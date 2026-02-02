#!/usr/bin/env python3
"""
Execution Trace Visualizer for LMN Oracle System

Generates visual traces of oracle execution:
- ASCII tree format
- HTML interactive visualization
- JSON trace export
- Performance flame graphs
"""

import argparse
import json
import sys
import time
from typing import List, Dict, Any, Optional
from dataclasses import dataclass, asdict
from enum import Enum


class OracleType(Enum):
    """Oracle type categories"""
    ARITH = "Arith"
    SEMANTIC = "Semantic"
    FILE = "File"
    TIME = "Time"
    DATABASE = "Database"
    NETWORK = "Network"
    MEMORY = "Memory"


@dataclass
class OracleTrace:
    """Single oracle execution trace"""
    id: int
    oracle_type: str
    params: Dict[str, Any]
    result: Any
    start_time: float
    end_time: float
    parent_id: Optional[int] = None
    children: List['OracleTrace'] = None

    def __post_init__(self):
        if self.children is None:
            self.children = []

    @property
    def duration_ms(self) -> float:
        """Duration in milliseconds"""
        return (self.end_time - self.start_time) * 1000

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary"""
        return {
            "id": self.id,
            "oracle_type": self.oracle_type,
            "params": self.params,
            "result": str(self.result)[:100],  # Truncate long results
            "duration_ms": self.duration_ms,
            "parent_id": self.parent_id,
            "children": [c.to_dict() for c in self.children]
        }


class TraceCollector:
    """Collects oracle execution traces"""

    def __init__(self):
        self.traces: List[OracleTrace] = []
        self.current_id = 0
        self.trace_stack: List[int] = []

    def start_oracle(self, oracle_type: str, params: Dict[str, Any]) -> int:
        """Start tracing an oracle call"""
        trace_id = self.current_id
        self.current_id += 1

        parent_id = self.trace_stack[-1] if self.trace_stack else None

        trace = OracleTrace(
            id=trace_id,
            oracle_type=oracle_type,
            params=params,
            result=None,
            start_time=time.time(),
            end_time=0,
            parent_id=parent_id
        )

        self.traces.append(trace)
        self.trace_stack.append(trace_id)

        return trace_id

    def end_oracle(self, trace_id: int, result: Any) -> None:
        """End tracing an oracle call"""
        trace = self.traces[trace_id]
        trace.end_time = time.time()
        trace.result = result

        if self.trace_stack and self.trace_stack[-1] == trace_id:
            self.trace_stack.pop()

    def get_root_traces(self) -> List[OracleTrace]:
        """Get top-level traces (no parent)"""
        return [t for t in self.traces if t.parent_id is None]

    def build_trace_tree(self) -> None:
        """Build parent-child relationships"""
        trace_map = {t.id: t for t in self.traces}

        for trace in self.traces:
            if trace.parent_id is not None:
                parent = trace_map.get(trace.parent_id)
                if parent:
                    parent.children.append(trace)


class ASCIIVisualizer:
    """Generates ASCII tree visualization"""

    def render(self, traces: List[OracleTrace], indent: int = 0) -> str:
        """Render traces as ASCII tree"""
        lines = []

        for i, trace in enumerate(traces):
            is_last = i == len(traces) - 1
            prefix = "  " * indent

            if indent > 0:
                connector = "└─> " if is_last else "├─> "
                prefix += connector

            # Format oracle call
            params_str = self._format_params(trace.params)
            line = f"{prefix}{trace.oracle_type}{params_str} ({trace.duration_ms:.2f}ms)"
            lines.append(line)

            # Add result
            if trace.result is not None:
                result_str = str(trace.result)
                if len(result_str) > 60:
                    result_str = result_str[:57] + "..."

                result_prefix = "  " * (indent + 1)
                if indent > 0:
                    result_prefix += "    " if is_last else "│   "
                lines.append(f"{result_prefix}└─> {result_str}")

            # Recursively render children
            if trace.children:
                child_indent = indent + 1
                child_lines = self.render(trace.children, child_indent)
                lines.append(child_lines)

        return "\n".join(lines)

    def _format_params(self, params: Dict[str, Any]) -> str:
        """Format parameters for display"""
        if not params:
            return "()"

        items = []
        for key, value in params.items():
            value_str = str(value)
            if len(value_str) > 20:
                value_str = value_str[:17] + "..."
            items.append(f"{key}={value_str}")

        return f"({', '.join(items)})"


class HTMLVisualizer:
    """Generates interactive HTML visualization"""

    def render(self, traces: List[OracleTrace]) -> str:
        """Generate HTML visualization"""
        trace_data = [t.to_dict() for t in traces]

        html = """
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>LMN Oracle Execution Trace</title>
    <style>
        body {
            font-family: 'Courier New', monospace;
            margin: 20px;
            background: #1e1e1e;
            color: #d4d4d4;
        }
        h1 {
            color: #4ec9b0;
        }
        .trace {
            margin: 10px 0;
            padding: 10px;
            background: #252526;
            border-left: 3px solid #4ec9b0;
        }
        .oracle-type {
            color: #4ec9b0;
            font-weight: bold;
        }
        .params {
            color: #ce9178;
        }
        .duration {
            color: #b5cea8;
            float: right;
        }
        .result {
            margin-top: 5px;
            padding: 5px;
            background: #1e1e1e;
            border-left: 2px solid #569cd6;
            color: #9cdcfe;
        }
        .children {
            margin-left: 20px;
            border-left: 2px solid #3e3e42;
            padding-left: 10px;
        }
        .stats {
            margin: 20px 0;
            padding: 15px;
            background: #252526;
            border-left: 3px solid #569cd6;
        }
        .stat-label {
            color: #569cd6;
            font-weight: bold;
        }
    </style>
</head>
<body>
    <h1>🔍 LMN Oracle Execution Trace</h1>

    <div class="stats">
        <div class="stat-label">Statistics</div>
        <div id="stats"></div>
    </div>

    <div id="traces"></div>

    <script>
        const traceData = """ + json.dumps(trace_data, indent=2) + """;

        function renderTrace(trace, container) {
            const div = document.createElement('div');
            div.className = 'trace';

            const header = document.createElement('div');
            header.innerHTML = `
                <span class="oracle-type">${trace.oracle_type}</span>
                <span class="params">${JSON.stringify(trace.params)}</span>
                <span class="duration">${trace.duration_ms.toFixed(2)}ms</span>
            `;
            div.appendChild(header);

            if (trace.result) {
                const result = document.createElement('div');
                result.className = 'result';
                result.textContent = `→ ${trace.result}`;
                div.appendChild(result);
            }

            if (trace.children && trace.children.length > 0) {
                const childrenDiv = document.createElement('div');
                childrenDiv.className = 'children';
                trace.children.forEach(child => renderTrace(child, childrenDiv));
                div.appendChild(childrenDiv);
            }

            container.appendChild(div);
        }

        const tracesContainer = document.getElementById('traces');
        traceData.forEach(trace => renderTrace(trace, tracesContainer));

        // Calculate stats
        function collectStats(traces) {
            let totalCalls = 0;
            let totalTime = 0;
            let byType = {};

            function walk(trace) {
                totalCalls++;
                totalTime += trace.duration_ms;

                if (!byType[trace.oracle_type]) {
                    byType[trace.oracle_type] = { count: 0, time: 0 };
                }
                byType[trace.oracle_type].count++;
                byType[trace.oracle_type].time += trace.duration_ms;

                if (trace.children) {
                    trace.children.forEach(walk);
                }
            }

            traces.forEach(walk);
            return { totalCalls, totalTime, byType };
        }

        const stats = collectStats(traceData);
        const statsDiv = document.getElementById('stats');
        statsDiv.innerHTML = `
            <div>Total oracle calls: ${stats.totalCalls}</div>
            <div>Total execution time: ${stats.totalTime.toFixed(2)}ms</div>
            <div>Average time per oracle: ${(stats.totalTime / stats.totalCalls).toFixed(2)}ms</div>
            <div style="margin-top: 10px;">By type:</div>
            ${Object.entries(stats.byType).map(([type, data]) =>
                `<div style="margin-left: 20px;">
                    ${type}: ${data.count} calls, ${data.time.toFixed(2)}ms total
                </div>`
            ).join('')}
        `;
    </script>
</body>
</html>
        """

        return html


def mock_oracle_execution(collector: TraceCollector) -> None:
    """Execute some mock oracles with tracing"""

    # Arithmetic operations
    trace_id = collector.start_oracle("Arith", {"op": "add", "a": 5, "b": 3})
    time.sleep(0.001)  # Simulate work
    collector.end_oracle(trace_id, 8)

    # Time operation
    trace_id = collector.start_oracle("TimeNow", {})
    time.sleep(0.001)
    collector.end_oracle(trace_id, {"timestamp": 1770000000})

    # File read
    trace_id = collector.start_oracle("FileRead", {"path": "/etc/hostname"})
    time.sleep(0.002)
    collector.end_oracle(trace_id, "myhost\n")

    # Semantic with nested operations
    trace_id = collector.start_oracle("Semantic", {"prompt": "Calculate 10 factorial", "context": "math"})

    # Nested arithmetic (simulating LLM breaking down the problem)
    for i in range(3):
        child_id = collector.start_oracle("Arith", {"op": "mul", "a": i+1, "b": i+2})
        time.sleep(0.0005)
        collector.end_oracle(child_id, (i+1) * (i+2))

    time.sleep(0.01)
    collector.end_oracle(trace_id, "3628800")


def main():
    parser = argparse.ArgumentParser(description="Visualize oracle execution traces")
    parser.add_argument("--format", choices=["ascii", "html", "json"], default="ascii",
                       help="Output format")
    parser.add_argument("--output", help="Output file (default: stdout)")
    parser.add_argument("--mock", action="store_true",
                       help="Generate mock trace data")

    args = parser.parse_args()

    print("\n" + "="*60)
    print("LMN ORACLE EXECUTION TRACE VISUALIZER")
    print("="*60)

    collector = TraceCollector()

    if args.mock:
        print("\nGenerating mock execution trace...")
        mock_oracle_execution(collector)

    # Build trace tree
    collector.build_trace_tree()
    root_traces = collector.get_root_traces()

    # Generate output
    if args.format == "ascii":
        visualizer = ASCIIVisualizer()
        output = visualizer.render(root_traces)
    elif args.format == "html":
        visualizer = HTMLVisualizer()
        output = visualizer.render(root_traces)
    elif args.format == "json":
        output = json.dumps([t.to_dict() for t in root_traces], indent=2)

    # Write output
    if args.output:
        with open(args.output, 'w') as f:
            f.write(output)
        print(f"\n✓ Trace written to: {args.output}")
    else:
        print("\n" + "="*60)
        print("TRACE OUTPUT")
        print("="*60)
        print(output)

    return 0


if __name__ == "__main__":
    sys.exit(main())
