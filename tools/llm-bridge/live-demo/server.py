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
    "recent_oracles": []
}

start_time = time.time()


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

        current_state["phase"] = "idle"
        current_state["current_oracle"] = None

        return response

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


# Global harness instance
harness = None


def init_harness():
    """Initialize the harness."""
    global harness

    bend_binary = Path(__file__).parent.parent.parent.parent / "tools" / "lmn-bend" / "bend"
    if not bend_binary.exists():
        bend_binary = "bend"

    harness = LiveHarness(
        bend_binary=str(bend_binary),
        enable_real_llm=False  # Set to True if you have ANTHROPIC_API_KEY
    )


def run_demo_loop():
    """Continuously run demo oracles."""
    oracle_file = Path(__file__).parent.parent / "production" / "oracle.bend"

    if not oracle_file.exists():
        print(f"Warning: {oracle_file} not found")
        return

    while True:
        try:
            if harness:
                harness.execute(oracle_file)
                time.sleep(5)  # Wait between executions
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


@app.route('/api/state')
def get_state():
    """Get current state."""
    current_state["stats"]["uptime_seconds"] = int(time.time() - start_time)

    # Calculate cache rate
    total = current_state["stats"]["total_oracles"]
    hits = current_state["stats"]["cache_hits"]
    cache_rate = (hits / total * 100) if total > 0 else 0
    current_state["stats"]["cache_rate"] = round(cache_rate, 1)

    return jsonify(current_state)


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

    oracle_file = Path(__file__).parent.parent / "production" / "oracle.bend"
    if not oracle_file.exists():
        return jsonify({"error": "Oracle file not found"}), 404

    try:
        threading.Thread(target=lambda: harness.execute(oracle_file), daemon=True).start()
        return jsonify({"status": "triggered"})
    except Exception as e:
        return jsonify({"error": str(e)}), 500


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
