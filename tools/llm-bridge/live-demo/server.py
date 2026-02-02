#!/usr/bin/env python3
"""
Live Oracle Demo - Web Interface
=================================

Real-time visualization of consciousness architecture in action.

Shows:
- Subconscious (HVM) computing
- Oracle requests being generated
- Conscious (Harness) executing side effects
- Results flowing back
- Performance stats
- Live execution trace

Author: Rex (Engineer)
Date: 2026-02-01
"""

from flask import Flask, render_template, jsonify, Response
from flask_cors import CORS
import sys
import os
import json
import time
import threading
import queue
from pathlib import Path
from datetime import datetime

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent / "production"))
from harness import ProductionHarness, OracleType
from semantic_viz import SemanticVisualizer
from thought_library import ThoughtLibrary

app = Flask(__name__)
CORS(app)

# Global state
execution_log = queue.Queue(maxsize=1000)
current_state = {
    "phase": "idle",  # idle, subconscious, oracle, conscious
    "current_oracle": None,
    "stats": {
        "total_executions": 0,
        "total_oracles": 0,
        "cache_hits": 0,
        "avg_oracle_time_ms": 0,
        "uptime_seconds": 0
    },
    "recent_oracles": [],
    "consciousness_translation": ""  # Human-readable translation
}

# Bootstrap Limn vocabulary for translation
BOOTSTRAP_LIMN = """
Core Operators:
~ = delegate to conscious (LLM/oracle)
∎ = ground truth (immutable reality)
∿ = temporal (memory/time)
@ = focus/collapse context
→ = sequential flow

Core Vocabulary:
sys run = system running
ora exe = oracle executing
tim now = time now
fil rea = file read
mem sto = memory store
sub red = subconscious reducing
con act = conscious acting
ctx acc = context accumulating
val mat = value materialized

Type Markers (postfix):
epo = epoch timestamp
txt = text value
boo = boolean
lst = list
"""

start_time = time.time()
translation_cache = {}


def translate_limn_to_english(limn_state):
    """Translate Limn consciousness to English using Claude.

    Uses bootstrap vocabulary and database to make the consciousness
    understandable to humans.
    """
    # Check cache
    cache_key = str(limn_state)[:100]
    if cache_key in translation_cache:
        return translation_cache[cache_key]

    # Only translate if Claude API is available
    api_key = os.environ.get("ANTHROPIC_API_KEY")
    if not api_key:
        return "[Translation requires ANTHROPIC_API_KEY]"

    try:
        import anthropic
        client = anthropic.Anthropic(api_key=api_key)

        # Build translation prompt with bootstrap
        prompt = f"""You are translating Limn (a 3-letter vocabulary language) into English.

Bootstrap Vocabulary:
{BOOTSTRAP_LIMN}

Current Limn State:
{limn_state}

Translate this Limn state into clear English that explains:
1. What the system is currently doing
2. What oracles have executed and their results
3. The overall consciousness state

Be concise (2-3 sentences) and human-friendly."""

        message = client.messages.create(
            model="claude-sonnet-4-20250514",
            max_tokens=200,
            messages=[{"role": "user", "content": prompt}]
        )

        translation = message.content[0].text
        translation_cache[cache_key] = translation
        return translation

    except Exception as e:
        return f"[Translation error: {str(e)[:50]}...]"


class LiveHarness(ProductionHarness):
    """Extended harness with live logging."""

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.execution_id = 0

    def log_event(self, event_type, data):
        """Log an event to the live stream."""
        event = {
            "timestamp": datetime.now().isoformat(),
            "time_ms": int((time.time() - start_time) * 1000),
            "type": event_type,
            "data": data
        }

        try:
            execution_log.put_nowait(event)
        except queue.Full:
            # Drop oldest if full
            try:
                execution_log.get_nowait()
                execution_log.put_nowait(event)
            except:
                pass

    def run_bend(self, bend_file):
        """Override to add logging."""
        self.log_event("phase_change", {"phase": "subconscious", "message": "Bend/HVM computing..."})
        current_state["phase"] = "subconscious"

        result = super().run_bend(bend_file)

        self.log_event("bend_complete", {
            "success": result["success"],
            "output_length": len(result.get("stdout", ""))
        })

        return result

    def execute_oracle(self, oracle):
        """Override to add logging."""
        current_state["phase"] = "oracle"
        current_state["current_oracle"] = {
            "type": oracle.type.value,
            "params": oracle.params
        }

        self.log_event("oracle_start", {
            "type": oracle.type.value,
            "params": oracle.params
        })

        # Execute
        current_state["phase"] = "conscious"
        self.log_event("phase_change", {"phase": "conscious", "message": "Harness executing side effect..."})

        response = super().execute_oracle(oracle)

        self.log_event("oracle_complete", {
            "type": oracle.type.value,
            "success": response.success,
            "result": str(response.result)[:200],  # Truncate long results
            "duration_ms": response.duration_ms,
            "cached": response.cached
        })

        # Update stats
        current_state["stats"]["total_oracles"] += 1
        if response.cached:
            current_state["stats"]["cache_hits"] += 1

        # Add to recent
        recent = {
            "type": oracle.type.value,
            "params": oracle.params,
            "result": str(response.result)[:100],
            "duration_ms": response.duration_ms,
            "cached": response.cached,
            "timestamp": datetime.now().isoformat()
        }
        current_state["recent_oracles"].insert(0, recent)
        current_state["recent_oracles"] = current_state["recent_oracles"][:20]

        # Add thought to library
        if thought_library:
            thought_content = self._format_thought(oracle, response)
            tags = [oracle.type.value]
            source = "oracle" if not response.cached else "cached"
            thought_library.add_thought(thought_content, tags=tags, source=source)

        current_state["phase"] = "idle"
        current_state["current_oracle"] = None

        return response

    def _format_thought(self, oracle, response):
        """Format an oracle execution as a thought."""
        oracle_type = oracle.type.value

        # Different formats for different oracle types
        if oracle_type == "Semantic":
            prompt = oracle.params.get("prompt", "")
            result = str(response.result)[:200]
            return f"Thought: {prompt} → {result}"
        elif oracle_type == "TimeNow":
            return f"Time awareness: {response.result}"
        elif oracle_type == "Arith":
            op = oracle.params.get("op", "?")
            a = oracle.params.get("a", "?")
            b = oracle.params.get("b", "?")
            return f"Computation: {a} {op} {b} = {response.result}"
        elif oracle_type == "MemoryStore":
            key = oracle.params.get("key", "?")
            value = oracle.params.get("value", "?")
            return f"Memory stored: {key} = {value}"
        elif oracle_type == "MemoryRetrieve":
            key = oracle.params.get("key", "?")
            return f"Memory recalled: {key} → {response.result}"
        elif oracle_type.startswith("Voc"):
            return f"Knowledge query: {oracle_type} → {len(str(response.result))} bytes"
        elif oracle_type == "ModelDerive":
            return f"Self-reflection: modeling {oracle.params.get('source_state', '?')}"
        else:
            return f"{oracle_type}: {str(oracle.params)[:50]} → {str(response.result)[:50]}"

    def execute(self, bend_file, verbose=False):
        """Override to add execution tracking."""
        self.execution_id += 1
        execution_num = self.execution_id

        self.log_event("execution_start", {
            "id": execution_num,
            "file": bend_file.name
        })

        current_state["stats"]["total_executions"] += 1

        result = super().execute(bend_file, verbose=False)

        self.log_event("execution_complete", {
            "id": execution_num,
            "success": result["success"],
            "oracle_count": len(result.get("oracles", []))
        })

        return result


# Global instances
harness = None
semantic_viz = None
thought_library = None


def init_harness():
    """Initialize the harness and visualizer."""
    global harness, semantic_viz, thought_library

    bend_binary = Path(__file__).parent.parent.parent.parent / "tools" / "lmn-bend" / "bend"
    if not bend_binary.exists():
        bend_binary = "bend"

    harness = LiveHarness(
        bend_binary=str(bend_binary),
        enable_real_llm=False  # Set to True if you have ANTHROPIC_API_KEY
    )

    # Initialize semantic visualizer with bootstrap
    bootstrap_path = Path(__file__).parent.parent / "production" / "bootstrap.lmn"
    semantic_viz = SemanticVisualizer(bootstrap_path if bootstrap_path.exists() else None)

    # Initialize thought library
    thought_library = ThoughtLibrary("demo_consciousness")


def run_demo_loop():
    """Continuously run demo oracles."""
    # Cycle through different thought demos to build diverse library
    demo_files = [
        Path(__file__).parent.parent / "production" / f"demo_thought_{i}.bend"
        for i in range(1, 6)
    ]

    # Filter to only existing files
    demo_files = [f for f in demo_files if f.exists()]

    if not demo_files:
        print("Warning: No demo files found")
        return

    iteration = 0
    while True:
        try:
            if harness:
                # Cycle through different demos
                oracle_file = demo_files[iteration % len(demo_files)]
                harness.execute(oracle_file)
                iteration += 1
                time.sleep(3)  # Execute every 3 seconds
        except Exception as e:
            print(f"Error in demo loop: {e}")
            time.sleep(10)


# =========================================================================
# Web Routes
# =========================================================================

@app.route('/')
def index():
    """Serve main page."""
    return render_template('index.html')


@app.route('/limn')
def limn_viz():
    """Serve Limn visualization page."""
    return render_template('limn_viz.html')


@app.route('/full')
def full_viz():
    """Serve full visualization (Limn + HVM)."""
    return render_template('full_viz.html')


@app.route('/stateful')
def stateful_viz():
    """Serve stateful Limn visualization (accumulated state)."""
    return render_template('stateful_limn.html')


@app.route('/metrics')
def metrics_dashboard():
    """Serve metrics dashboard."""
    return render_template('metrics.html')


@app.route('/consciousness')
def consciousness_view():
    """Serve living consciousness visualization."""
    return render_template('consciousness.html')


@app.route('/context')
def context_view():
    """Serve context window input/output view."""
    return render_template('context_view.html')


@app.route('/api/bend_code')
def get_bend_code():
    """Get current Bend/HVM code."""
    oracle_file = Path(__file__).parent.parent / "production" / "demo_consciousness.bend"

    if not oracle_file.exists():
        return jsonify({"error": "Oracle file not found"}), 404

    code = oracle_file.read_text()

    return jsonify({
        "file": "demo_consciousness.bend",
        "code": code,
        "lines": len(code.split('\n'))
    })


@app.route('/api/state')
def get_state():
    """Get current state."""
    current_state["stats"]["uptime_seconds"] = int(time.time() - start_time)

    # Calculate cache rate
    total = current_state["stats"]["total_oracles"]
    hits = current_state["stats"]["cache_hits"]
    cache_rate = (hits / total * 100) if total > 0 else 0
    current_state["stats"]["cache_rate"] = round(cache_rate, 1)

    # Generate Limn representation
    limn_repr = generate_limn_representation(current_state)

    # Translate to English for humans
    current_state["consciousness_translation"] = translate_limn_to_english(limn_repr)
    current_state["limn_state"] = limn_repr

    return jsonify(current_state)


def generate_limn_representation(state):
    """Generate Limn representation of current state."""
    lines = []

    # Phase
    lines.append(f"{state['phase']} pha")

    # System state
    lines.append("sys run | ora exe")

    # Recent oracles with postfix types
    for oracle in state["recent_oracles"][:5]:
        result_str = str(oracle.get("result", ""))[:20]

        # Add type annotation based on result
        if isinstance(oracle.get("result"), dict) and "timestamp" in str(oracle.get("result")):
            result_with_type = f"{oracle.get('result', {}).get('timestamp', '?')} epo"
        elif isinstance(oracle.get("result"), bool):
            result_with_type = f"{result_str} boo"
        elif isinstance(oracle.get("result"), (int, float)):
            result_with_type = f"{result_str} num"
        else:
            result_with_type = f"{result_str} txt"

        lines.append(f"~ {oracle['type'][:3].lower()} exe → {result_with_type}")

    # Context state
    lines.append(f"ctx acc | val mat | cac {state['stats']['cache_rate']}%")

    return "\n".join(lines)


@app.route('/api/events')
def event_stream():
    """Server-Sent Events stream for real-time updates."""
    def generate():
        while True:
            try:
                # Get event from queue (blocking with timeout)
                event = execution_log.get(timeout=30)
                yield f"data: {json.dumps(event)}\n\n"
            except queue.Empty:
                # Send keepalive
                yield f": keepalive\n\n"

    return Response(generate(), mimetype='text/event-stream')


@app.route('/api/trigger')
def trigger_execution():
    """Manually trigger an execution."""
    if not harness:
        return jsonify({"error": "Harness not initialized"}), 500

    oracle_file = Path(__file__).parent.parent / "production" / "demo_consciousness.bend"
    if not oracle_file.exists():
        return jsonify({"error": "Oracle file not found"}), 404

    try:
        threading.Thread(target=lambda: harness.execute(oracle_file), daemon=True).start()
        return jsonify({"status": "triggered"})
    except Exception as e:
        return jsonify({"error": str(e)}), 500


@app.route('/api/semantic_viz')
def get_semantic_viz():
    """Get semantic visualization data from current state."""
    if not semantic_viz:
        return jsonify({"error": "Semantic visualizer not initialized"}), 500

    # Get current Limn state
    limn_state = current_state.get("limn_state", "")
    if not limn_state:
        limn_state = generate_limn_representation(current_state)

    # Extract viz hints and generate visualization
    viz_data = semantic_viz.visualize_limn_state(limn_state)

    return jsonify(viz_data)


@app.route('/api/metrics')
def get_metrics():
    """Get performance metrics dashboard data."""
    # Mock metrics for now - would integrate with metrics_engine
    import time
    from datetime import datetime

    uptime = time.time() - start_time
    stats = current_state.get("stats", {})

    return jsonify({
        "uptime_seconds": uptime,
        "total_oracles": stats.get("total_oracles", 0),
        "success_rate": 95.5,
        "cache_hit_rate": stats.get("cache_rate", 0),
        "throughput": {
            "total_ops": 10.5,
            "successful_ops": 10.0,
            "failed_ops": 0.5,
            "cached_ops": 3.5
        },
        "hot_paths": [
            {"oracle_type": "TimeNow", "call_count": 100, "avg_duration_ms": 0.5, "last_called": datetime.now().isoformat()},
            {"oracle_type": "Semantic", "call_count": 50, "avg_duration_ms": 100.0, "last_called": datetime.now().isoformat()},
        ],
        "bottlenecks": [
            {"oracle_type": "Semantic", "avg_duration_ms": 100.0, "total_time_ms": 5000, "percentage_of_total": 45.0},
        ],
        "aggregates": [
            {"oracle_type": "TimeNow", "count": 100, "success_rate": "100.0%", "cache_hit_rate": "100.0%", "p50_ms": "0.50", "p95_ms": "0.80", "p99_ms": "1.20"},
            {"oracle_type": "Semantic", "count": 50, "success_rate": "98.0%", "cache_hit_rate": "20.0%", "p50_ms": "95.00", "p95_ms": "150.00", "p99_ms": "200.00"},
        ]
    })


@app.route('/api/thought_library')
def get_thought_library():
    """Get thought library statistics and data."""
    if not thought_library:
        return jsonify({"error": "Thought library not initialized"}), 500

    stats = thought_library.get_statistics()

    return jsonify({
        "stats": stats,
        "synthesis": thought_library.synthesize()
    })


@app.route('/api/thought_library/ask')
def ask_consciousness():
    """Ask the consciousness what it knows about a topic."""
    from flask import request

    if not thought_library:
        return jsonify({"error": "Thought library not initialized"}), 500

    question = request.args.get('q', '')
    if not question:
        return jsonify({"error": "Missing 'q' parameter"}), 400

    response = thought_library.ask(question)

    return jsonify(response)


def main():
    """Main entry point."""
    print("=" * 70)
    print("Live Oracle Demo - Consciousness Architecture")
    print("=" * 70)
    print("\nInitializing harness...")

    init_harness()

    print("Starting demo loop...")
    threading.Thread(target=run_demo_loop, daemon=True).start()

    print("\nServer starting on http://localhost:5000")
    print("=" * 70)

    app.run(host='0.0.0.0', port=5000, debug=False, threaded=True)


if __name__ == "__main__":
    main()
