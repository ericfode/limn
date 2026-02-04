#!/usr/bin/env python3
"""Consciousness API - HTTP interface to the recursive consciousness.

Lightweight HTTP server exposing consciousness operations:
  GET  /status          System status and memory summary
  GET  /stats           Full analytics dashboard (JSON)
  GET  /graph           Concept graph (JSON)
  GET  /replay          Thought replay timeline (JSON)
  POST /think           Generate a single thought
  POST /dream           Start a dream session
  POST /ensemble        Start an ensemble session

Usage:
  python3 consciousness_api.py [--port 8420]

Author: Rex (Engineer)
Date: 2026-02-03
"""

import json
import sys
import time
import re
from pathlib import Path
from http.server import HTTPServer, BaseHTTPRequestHandler
from urllib.parse import urlparse, parse_qs
from typing import Dict, Any, Optional
from collections import defaultdict
from threading import Thread
import logging

# Add production dir to path
sys.path.insert(0, str(Path(__file__).parent))

from recursive_consciousness import (
    RecursiveConsciousness,
    _analyze_emotional_valence,
)
from limn_validator import LimnValidator

logger = logging.getLogger(__name__)

# Global consciousness instance (lazy-initialized)
_consciousness: Optional[RecursiveConsciousness] = None
_background_task: Optional[Thread] = None


def get_consciousness(topic: str = None) -> RecursiveConsciousness:
    """Get or create the global consciousness instance."""
    global _consciousness
    if _consciousness is None or (topic and _consciousness.topic != topic):
        _consciousness = RecursiveConsciousness(topic=topic)
    return _consciousness


class ConsciousnessHandler(BaseHTTPRequestHandler):
    """HTTP request handler for consciousness API."""

    def log_message(self, format, *args):
        """Suppress default request logging."""
        pass

    def _send_json(self, data: Any, status: int = 200):
        """Send JSON response."""
        self.send_response(status)
        self.send_header('Content-Type', 'application/json')
        self.send_header('Access-Control-Allow-Origin', '*')
        self.end_headers()
        self.wfile.write(json.dumps(data, indent=2).encode())

    def _send_error(self, message: str, status: int = 400):
        """Send error response."""
        self._send_json({'error': message}, status)

    def _parse_body(self) -> Dict:
        """Parse JSON request body."""
        content_length = int(self.headers.get('Content-Length', 0))
        if content_length == 0:
            return {}
        body = self.rfile.read(content_length)
        return json.loads(body)

    def do_GET(self):
        """Handle GET requests."""
        parsed = urlparse(self.path)
        path = parsed.path
        params = parse_qs(parsed.query)

        if path == '/status':
            self._handle_status()
        elif path == '/stats':
            self._handle_stats()
        elif path == '/graph':
            self._handle_graph()
        elif path == '/replay':
            last_n = int(params.get('last', [0])[0]) or None
            topic = params.get('topic', [None])[0]
            self._handle_replay(last_n=last_n, topic=topic)
        elif path == '/health':
            self._send_json({'status': 'ok', 'timestamp': time.time()})
        else:
            self._send_error('Not found', 404)

    def do_POST(self):
        """Handle POST requests."""
        parsed = urlparse(self.path)
        path = parsed.path

        try:
            body = self._parse_body()
        except json.JSONDecodeError:
            self._send_error('Invalid JSON')
            return

        if path == '/think':
            self._handle_think(body)
        elif path == '/dream':
            self._handle_dream(body)
        elif path == '/ensemble':
            self._handle_ensemble(body)
        else:
            self._send_error('Not found', 404)

    def do_OPTIONS(self):
        """Handle CORS preflight."""
        self.send_response(200)
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET, POST, OPTIONS')
        self.send_header('Access-Control-Allow-Headers', 'Content-Type')
        self.end_headers()

    # ── Handlers ──

    def _handle_status(self):
        """Return system status and memory summary."""
        rc = get_consciousness()
        validator = rc.validator

        status = {
            'brain_state_size': len(rc.brain_state),
            'iteration': rc.iteration,
            'topic': rc.topic,
            'vocabulary': {
                'total': len(validator.vocab),
                'used': len(rc.vocab_used),
                'coverage_pct': round(len(rc.vocab_used) / len(validator.vocab) * 100, 1),
                'domains_total': len(validator.domain_words),
                'domains_explored': len(rc.domains_explored),
            },
            'concepts_tracked': len(rc.concept_frequency),
            'thoughts_this_session': len(rc.thought_history),
            'narrative_thread': rc.narrative_thread,
            'thought_library': {
                'thoughts': rc.thought_library.total_thoughts,
                'concepts': rc.thought_library.total_concepts,
                'patterns': len(rc.thought_library.patterns),
                'connections': sum(len(v) for v in rc.thought_library.semantic_network.values()),
            },
            'learning_goals': [
                {'type': g['type'], 'progress': g.get('progress', 0)}
                for g in rc.current_goals
            ],
            'emotional_momentum': round(rc.emotional_momentum, 3),
            'run_history_count': len(rc.run_history),
            'timestamp': time.time(),
        }
        self._send_json(status)

    def _handle_stats(self):
        """Return full analytics as JSON."""
        base = Path(__file__).parent

        result = {}

        # Thought log
        thought_log_path = base / "thought_log.jsonl"
        thoughts = []
        if thought_log_path.exists():
            with open(thought_log_path, 'r') as f:
                for line in f:
                    line = line.strip()
                    if line:
                        try:
                            thoughts.append(json.loads(line))
                        except json.JSONDecodeError:
                            continue

        if thoughts:
            domain_counts = defaultdict(int)
            total_quality = []
            for t in thoughts:
                for d in t.get('domains', []):
                    domain_counts[d] += 1
                if 'score' in t:
                    total_quality.append(t['score'])

            result['thoughts'] = {
                'total': len(thoughts),
                'domains': dict(domain_counts),
            }

            if total_quality:
                result['quality'] = {
                    'avg_overall': round(sum(s['overall'] for s in total_quality) / len(total_quality), 3),
                    'avg_novelty': round(sum(s['novelty'] for s in total_quality) / len(total_quality), 3),
                    'avg_diversity': round(sum(s['diversity'] for s in total_quality) / len(total_quality), 3),
                    'avg_coherence': round(sum(s['coherence'] for s in total_quality) / len(total_quality), 3),
                    'avg_depth': round(sum(s['depth'] for s in total_quality) / len(total_quality), 3),
                }

            valence = _analyze_emotional_valence(thoughts)
            if valence:
                result['emotional_valence'] = valence

        # Memory
        memory_path = base / "consciousness_memory.json"
        if memory_path.exists():
            with open(memory_path, 'r') as f:
                memory = json.load(f)
            validator = LimnValidator()
            result['memory'] = {
                'concepts': len(memory.get('concept_frequency', {})),
                'vocab_used': len(memory.get('vocab_used', [])),
                'domains_explored': len(memory.get('domains_explored', [])),
                'coverage_pct': round(
                    len(memory.get('vocab_used', [])) / len(validator.vocab) * 100, 1
                ),
                'top_concepts': sorted(
                    memory.get('concept_frequency', {}).items(),
                    key=lambda x: -x[1]
                )[:20],
            }

            # Cross-session evolution
            run_history = memory.get('run_history', [])
            if run_history:
                result['evolution'] = {
                    'total_runs': len(run_history),
                    'runs': run_history[-10:],  # Last 10 runs
                }
                if len(run_history) >= 2:
                    prev = run_history[-2]
                    curr = run_history[-1]
                    result['evolution']['trend'] = {
                        'quality_delta': round(
                            curr.get('quality', {}).get('overall', 0) -
                            prev.get('quality', {}).get('overall', 0), 3
                        ),
                        'coverage_delta': round(
                            curr.get('coverage', {}).get('pct', 0) -
                            prev.get('coverage', {}).get('pct', 0), 1
                        ),
                    }

        # Dream report
        dream_path = base / "dream_report.json"
        if dream_path.exists():
            with open(dream_path, 'r') as f:
                result['dream'] = json.load(f)

        self._send_json(result)

    def _handle_graph(self):
        """Return concept graph."""
        graph_path = Path(__file__).parent / "concept_graph.json"
        if graph_path.exists():
            with open(graph_path, 'r') as f:
                graph = json.load(f)
            self._send_json(graph)
        else:
            # Generate from current consciousness
            rc = get_consciousness()
            graph_file = rc.export_concept_graph("json")
            if graph_file:
                with open(graph_file, 'r') as f:
                    self._send_json(json.load(f))
            else:
                self._send_json({'nodes': [], 'links': []})

    def _handle_replay(self, last_n: int = None, topic: str = None):
        """Return thought replay as JSON."""
        log_path = Path(__file__).parent / "thought_log.jsonl"
        if not log_path.exists():
            self._send_json({'thoughts': [], 'summary': {}})
            return

        thoughts = []
        with open(log_path, 'r') as f:
            for line in f:
                line = line.strip()
                if line:
                    try:
                        thoughts.append(json.loads(line))
                    except json.JSONDecodeError:
                        continue

        if topic:
            thoughts = [t for t in thoughts if t.get('topic') == topic]
        if last_n:
            thoughts = thoughts[-last_n:]

        # Summary
        all_words = set()
        domain_counts = defaultdict(int)
        for t in thoughts:
            words = re.findall(r'[a-z]{2,4}', t.get('content', '').lower())
            all_words.update(words)
            for d in t.get('domains', []):
                domain_counts[d] += 1

        self._send_json({
            'thoughts': thoughts,
            'summary': {
                'total': len(thoughts),
                'unique_words': len(all_words),
                'domains': dict(domain_counts),
            }
        })

    def _handle_think(self, body: Dict):
        """Generate a single thought."""
        topic = body.get('topic')
        rc = get_consciousness(topic)
        rc.iteration += 1

        thought = rc.think()

        # Get score from the last tracked thought
        score = {}
        if rc.thought_history and 'score' in rc.thought_history[-1]:
            score = rc.thought_history[-1]['score']

        self._send_json({
            'thought': thought,
            'iteration': rc.iteration,
            'score': score,
            'narrative': rc.narrative_thread,
            'brain_state_size': len(rc.brain_state),
            'vocab_coverage': round(len(rc.vocab_used) / len(rc.validator.vocab) * 100, 1),
        })

    def _handle_dream(self, body: Dict):
        """Start a dream session (runs in background)."""
        global _background_task

        if _background_task and _background_task.is_alive():
            self._send_error('A background task is already running', 409)
            return

        iterations = body.get('iterations', 20)
        snapshot_interval = body.get('snapshot_interval', 10)

        from recursive_consciousness import run_dream

        def dream_worker():
            run_dream(iterations=iterations, snapshot_interval=snapshot_interval)

        _background_task = Thread(target=dream_worker, daemon=True)
        _background_task.start()

        self._send_json({
            'status': 'started',
            'iterations': iterations,
            'message': 'Dream session started in background. Check /stats for progress.'
        })

    def _handle_ensemble(self, body: Dict):
        """Start an ensemble session (runs in background)."""
        global _background_task

        if _background_task and _background_task.is_alive():
            self._send_error('A background task is already running', 409)
            return

        topics = body.get('topics', ['Abstract', 'Mind & Cognition', 'Time & Change'])
        rounds = body.get('rounds', 5)
        topology = body.get('topology', 'ring')

        from recursive_consciousness import run_ensemble

        def ensemble_worker():
            run_ensemble(topics=topics, rounds=rounds, topology=topology)

        _background_task = Thread(target=ensemble_worker, daemon=True)
        _background_task.start()

        self._send_json({
            'status': 'started',
            'topics': topics,
            'rounds': rounds,
            'topology': topology,
            'message': 'Ensemble session started in background. Check /stats for progress.'
        })


def main():
    """Start the consciousness API server."""
    port = 8420
    if len(sys.argv) > 1:
        for i, arg in enumerate(sys.argv[1:]):
            if arg == '--port' and i + 2 < len(sys.argv):
                port = int(sys.argv[i + 2])

    server = HTTPServer(('localhost', port), ConsciousnessHandler)
    print(f"Consciousness API running on http://localhost:{port}")
    print()
    print("Endpoints:")
    print(f"  GET  http://localhost:{port}/status   - System status")
    print(f"  GET  http://localhost:{port}/stats    - Full analytics (JSON)")
    print(f"  GET  http://localhost:{port}/graph    - Concept graph")
    print(f"  GET  http://localhost:{port}/replay   - Thought timeline")
    print(f"  GET  http://localhost:{port}/health   - Health check")
    print(f"  POST http://localhost:{port}/think    - Generate thought")
    print(f"  POST http://localhost:{port}/dream    - Start dream session")
    print(f"  POST http://localhost:{port}/ensemble - Start ensemble")
    print()
    print("Press Ctrl+C to stop.")

    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nShutting down.")
        server.server_close()


if __name__ == "__main__":
    main()
