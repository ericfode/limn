# LMN Oracle Visualization Library

**Translate oracle execution state into visual insights**

This library provides comprehensive visualization capabilities for the LMN Oracle system, transforming internal execution state into multiple visual formats for analysis and debugging.

---

## Features

### Visualization Types

1. **Execution Traces** - Timeline view of oracle calls with timing and results
2. **Oracle Flow Graphs** - Dependency graphs showing oracle call relationships
3. **Performance Heatmaps** - Visual representation of execution hotspots
4. **Cache Hit Visualization** - Cache effectiveness metrics and patterns
5. **Consciousness Architecture Diagram** - Subconscious/Conscious boundary visualization
6. **Real-time Computation State** - Live state monitoring and display

### Output Formats

- **JSON** - Structured data for web consumption
- **SVG** - Scalable vector graphics for embedding
- **HTML** - Standalone interactive visualizations
- **Terminal** - ASCII art for CLI display

---

## Quick Start

```python
from viz import OracleVisualizer, TraceCollector

# Collect execution data
collector = TraceCollector()
harness = ProductionHarness(trace_collector=collector)
harness.execute(my_program)

# Generate visualizations
viz = OracleVisualizer(collector.traces)

# Export to different formats
viz.execution_trace().to_json("trace.json")
viz.flow_graph().to_svg("flow.svg")
viz.performance_heatmap().to_html("perf.html")
viz.cache_stats().to_terminal()
```

---

## Architecture

```
viz/
├── README.md              # This file
├── __init__.py            # Main API
├── core/
│   ├── collector.py       # Trace collection
│   ├── types.py           # Data structures
│   └── analyzer.py        # Analysis utilities
├── renderers/
│   ├── trace.py           # Execution trace renderer
│   ├── flow.py            # Flow graph renderer
│   ├── heatmap.py         # Performance heatmap
│   ├── cache.py           # Cache visualization
│   ├── architecture.py    # Consciousness diagram
│   └── realtime.py        # Live state display
├── exporters/
│   ├── json_export.py     # JSON exporter
│   ├── svg_export.py      # SVG exporter
│   ├── html_export.py     # HTML exporter
│   └── terminal_export.py # Terminal exporter
├── examples/
│   ├── basic_trace.py     # Simple trace example
│   ├── flow_analysis.py   # Flow graph example
│   └── live_monitor.py    # Real-time monitoring
└── test_viz.py            # Test suite
```

---

## Integration

### With Production Harness

The visualization library integrates seamlessly with the production oracle harness:

```python
from tools.llm_bridge.production.harness import ProductionHarness
from tools.llm_bridge.viz import TraceCollector, OracleVisualizer

# Inject trace collector
collector = TraceCollector()
harness = ProductionHarness()
harness.set_trace_collector(collector)

# Run your program
harness.execute(my_bend_file)

# Visualize results
viz = OracleVisualizer(collector)
viz.export_all("output/")
```

### Standalone Analysis

Load and visualize pre-recorded traces:

```python
from viz import OracleVisualizer

# Load trace data
viz = OracleVisualizer.from_json("trace.json")

# Generate specific visualizations
viz.flow_graph().to_svg("flow.svg")
viz.performance_heatmap().to_html("perf.html")
```

---

## Visualization Details

### Execution Trace

Shows chronological oracle execution with:
- Oracle type and parameters
- Execution duration
- Cache hit/miss
- Result summary
- Nesting level (for recursive calls)

**Formats:** JSON, HTML timeline, Terminal ASCII

### Flow Graph

Directed graph showing:
- Oracle dependencies
- Data flow between oracles
- Subconscious → Conscious boundary
- Critical path highlighting

**Formats:** SVG, HTML (interactive), Terminal (simple)

### Performance Heatmap

Visual representation of:
- Execution time distribution
- Hotspot identification
- Oracle type performance comparison
- Cache effectiveness zones

**Formats:** SVG, HTML (interactive), JSON

### Cache Visualization

Insights into caching:
- Hit rate per oracle type
- Cache key distribution
- Temporal patterns
- Miss analysis

**Formats:** JSON, HTML charts, Terminal stats

### Consciousness Architecture

Diagram showing:
- Subconscious layer (Bend/HVM - pure)
- Conscious layer (Harness - impure)
- Oracle boundary crossings
- Side effect locations

**Formats:** SVG, HTML, Terminal diagram

### Real-time State

Live monitoring of:
- Active oracles
- Queue depth
- Current cache state
- Performance metrics

**Formats:** Terminal (live refresh), JSON (polling), HTML (websocket)

---

## Data Format

### Trace Schema

```json
{
  "version": "1.0",
  "program": "example.bend",
  "started_at": "2026-02-01T18:50:00Z",
  "completed_at": "2026-02-01T18:50:01Z",
  "total_duration_ms": 1234.56,
  "oracles": [
    {
      "id": 0,
      "type": "Semantic",
      "params": {"prompt": "...", "context": "..."},
      "started_at": 0.0,
      "duration_ms": 523.4,
      "result": "...",
      "cached": false,
      "layer": "conscious"
    }
  ],
  "stats": {
    "total_oracles": 10,
    "cache_hits": 4,
    "cache_rate": 0.4,
    "by_type": {...}
  }
}
```

---

## Examples

See `examples/` directory for:
- Basic trace generation
- Flow graph analysis
- Performance profiling
- Cache optimization
- Real-time monitoring

---

## Design Philosophy

### Pure Visualization

Visualizations are generated from immutable trace data. The library never modifies oracle execution - it only observes and renders.

### Format Independence

All visualizations support multiple output formats through a common renderer interface. Add new formats by implementing the `Exporter` protocol.

### Composability

Individual visualization components can be combined:

```python
viz.combine([
    viz.execution_trace(),
    viz.performance_heatmap(),
    viz.cache_stats()
]).to_html("dashboard.html")
```

### Efficiency

Large traces are handled efficiently:
- Streaming processing for huge traces
- Lazy rendering for on-demand visualization
- Sampling for real-time displays

---

## Future Enhancements

- [ ] Diff visualization (compare two traces)
- [ ] Flame graphs for performance analysis
- [ ] Interactive timeline scrubbing
- [ ] WebSocket streaming for live updates
- [ ] Export to Prometheus/Grafana
- [ ] Trace compression for storage
- [ ] Pattern detection (common oracle sequences)
- [ ] Anomaly highlighting

---

*Illuminate the boundary between thought and action*

**— Built for understanding the consciousness architecture**
