#!/usr/bin/env python3
"""Consciousness API - HTTP interface to the recursive consciousness.

Lightweight HTTP server exposing consciousness operations:
  GET  /status          System status and memory summary
  GET  /stats           Full analytics dashboard (JSON)
  GET  /graph           Concept graph (JSON)
  GET  /genealogy       Thought genealogy tree (JSON)
  GET  /replay          Thought replay timeline (JSON)
  GET  /stream          SSE stream of live thoughts
  POST /think           Generate a single thought
  POST /compose         Generate a composed thought chain
  POST /introspect      Generate introspective meta-thought
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
from threading import Thread, Event
import logging
import queue

# Add production dir to path
sys.path.insert(0, str(Path(__file__).parent))

from recursive_consciousness import (
    RecursiveConsciousness,
    _analyze_emotional_valence,
)
from limn_validator import LimnValidator

logger = logging.getLogger(__name__)

# ── Web UI HTML ──

WEB_UI_HTML = """<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Limn Consciousness</title>
<style>
  * { margin: 0; padding: 0; box-sizing: border-box; }
  body {
    background: #0a0a0f; color: #c0c0d0; font-family: 'Menlo', 'Consolas', monospace;
    font-size: 14px; height: 100vh; display: flex; flex-direction: column;
  }
  header {
    background: #12121a; border-bottom: 1px solid #2a2a3a; padding: 12px 20px;
    display: flex; justify-content: space-between; align-items: center;
  }
  header h1 { font-size: 16px; color: #8888ff; letter-spacing: 2px; }
  header .status { font-size: 12px; color: #666; }
  .container { display: flex; flex: 1; overflow: hidden; }
  .thoughts-pane {
    flex: 2; overflow-y: auto; padding: 12px; border-right: 1px solid #1a1a2a;
  }
  .dashboard-pane {
    flex: 1; overflow-y: auto; padding: 12px; min-width: 280px;
  }
  .thought {
    margin-bottom: 8px; padding: 8px 12px; border-radius: 4px;
    border-left: 3px solid #333; background: #0f0f18; animation: fadeIn 0.3s;
  }
  @keyframes fadeIn { from { opacity: 0; transform: translateY(5px); } to { opacity: 1; } }
  .thought.high { border-left-color: #4a4; }
  .thought.mid { border-left-color: #aa4; }
  .thought.low { border-left-color: #a44; }
  .thought.introspection { border-left-color: #a4f; background: #140f1f; }
  .thought.composition { border-left-color: #4af; background: #0f1420; }
  .thought .content { font-size: 15px; line-height: 1.5; word-break: break-word; }
  .thought .meta {
    font-size: 11px; color: #555; margin-top: 4px;
    display: flex; gap: 12px; flex-wrap: wrap;
  }
  .thought .meta .score { color: #888; }
  .thought .meta .domains { color: #668; }
  .section { margin-bottom: 16px; }
  .section h3 { color: #6666cc; font-size: 12px; letter-spacing: 1px; margin-bottom: 8px; }
  .bar-container { background: #1a1a2a; border-radius: 3px; height: 18px; margin: 4px 0; position: relative; }
  .bar-fill { height: 100%; border-radius: 3px; transition: width 0.5s; }
  .bar-label {
    position: absolute; top: 0; left: 8px; line-height: 18px; font-size: 11px; color: #aaa;
  }
  .stat { display: flex; justify-content: space-between; padding: 2px 0; font-size: 12px; }
  .stat .label { color: #666; }
  .stat .value { color: #aaa; }
  .controls {
    background: #12121a; border-top: 1px solid #2a2a3a; padding: 10px 20px;
    display: flex; gap: 8px; align-items: center; flex-wrap: wrap;
  }
  .controls button {
    background: #1a1a2f; color: #8888cc; border: 1px solid #2a2a4a;
    padding: 6px 14px; border-radius: 4px; cursor: pointer; font-family: inherit;
    font-size: 12px; transition: all 0.2s;
  }
  .controls button:hover { background: #2a2a4f; border-color: #4a4a6a; }
  .controls button:disabled { opacity: 0.4; cursor: not-allowed; }
  .controls button.active { background: #2a2a5f; border-color: #6666cc; color: #aaf; }
  .controls select, .controls input {
    background: #1a1a2f; color: #aaa; border: 1px solid #2a2a4a;
    padding: 6px 10px; border-radius: 4px; font-family: inherit; font-size: 12px;
  }
  .emotion-indicator {
    width: 12px; height: 12px; border-radius: 50%; display: inline-block;
    margin-right: 6px; vertical-align: middle;
  }
  .personality-trait { font-size: 11px; color: #777; padding: 1px 0; }
  .goal { font-size: 12px; margin-bottom: 6px; }
  .streaming-dot { display: inline-block; width: 8px; height: 8px; background: #4a4;
    border-radius: 50%; margin-right: 6px; animation: pulse 1s infinite; }
  .streaming-dot.off { background: #444; animation: none; }
  @keyframes pulse { 0%, 100% { opacity: 1; } 50% { opacity: 0.3; } }
  .empty-state { color: #444; text-align: center; padding: 40px; font-style: italic; }
  .eval-event {
    font-size: 11px; padding: 4px 8px; margin-bottom: 3px; border-radius: 3px;
    background: #0f0f18; animation: fadeIn 0.3s;
  }
  .eval-event.score { border-left: 2px solid #4a4; }
  .eval-event.oracle { border-left: 2px solid #fa0; }
  .eval-event.oracle_sub { border-left: 2px solid #f60; }
  .eval-event.oracle_result { border-left: 2px solid #4af; }
  .eval-event.compose { border-left: 2px solid #a4f; }
  .eval-event.introspect { border-left: 2px solid #f4a; }
  .eval-event.quality_feedback { border-left: 2px solid #f44; }
  .eval-event.self_mod { border-left: 2px solid #ff4; }
  .eval-event .ev-type {
    display: inline-block; min-width: 60px; color: #888; font-weight: bold;
    text-transform: uppercase; font-size: 10px;
  }
  .eval-event .ev-detail { color: #666; }
  .eval-pane {
    max-height: 200px; overflow-y: auto; margin-top: 8px;
    border-top: 1px solid #1a1a2a; padding-top: 8px;
  }
  .fork-indicator {
    display: inline-block; margin: 0 2px; font-size: 10px; vertical-align: middle;
  }
  .fork-indicator.oracle { color: #fa0; }
  .fork-indicator.compose { color: #a4f; }
  .fork-indicator.introspect { color: #f4a; }
  .fork-indicator.feedback { color: #f44; }
</style>
</head>
<body>
<header>
  <h1>LIMN CONSCIOUSNESS</h1>
  <div class="status" id="header-status">Connecting...</div>
</header>
<div class="container">
  <div class="thoughts-pane" id="thoughts">
    <div class="empty-state">Press "Think" or "Stream" to begin</div>
  </div>
  <div class="dashboard-pane" id="dashboard">
    <div class="section">
      <h3>VOCABULARY</h3>
      <div class="bar-container">
        <div class="bar-fill" id="vocab-bar" style="width:0%;background:#4a4;"></div>
        <span class="bar-label" id="vocab-label">-</span>
      </div>
      <div class="stat"><span class="label">Words used</span><span class="value" id="vocab-used">-</span></div>
      <div class="stat"><span class="label">Domains</span><span class="value" id="vocab-domains">-</span></div>
    </div>
    <div class="section">
      <h3>EMOTIONAL STATE</h3>
      <div class="stat">
        <span class="label"><span class="emotion-indicator" id="emo-dot"></span>Momentum</span>
        <span class="value" id="emo-value">-</span>
      </div>
    </div>
    <div class="section">
      <h3>NARRATIVE</h3>
      <div class="stat"><span class="label">Thread</span><span class="value" id="narrative">-</span></div>
      <div class="stat"><span class="label">Iteration</span><span class="value" id="iteration">0</span></div>
    </div>
    <div class="section">
      <h3>GOALS</h3>
      <div id="goals-list"><span class="stat"><span class="label">None yet</span></span></div>
    </div>
    <div class="section">
      <h3>PERSONALITY</h3>
      <div id="personality-info"><span class="personality-trait">Not yet established</span></div>
    </div>
    <div class="section">
      <h3>QUALITY (recent)</h3>
      <div class="stat"><span class="label">Average</span><span class="value" id="quality-avg">-</span></div>
      <div class="stat"><span class="label">Thoughts</span><span class="value" id="thought-count">0</span></div>
    </div>
    <div class="section">
      <h3>PROMPT MODS</h3>
      <div class="stat"><span class="label">Active</span><span class="value" id="prompt-mods">0</span></div>
    </div>
    <div class="section">
      <h3>EVALUATION FORKS</h3>
      <div class="stat"><span class="label">Total events</span><span class="value" id="eval-count">0</span></div>
      <div class="eval-pane" id="eval-events"></div>
    </div>
  </div>
</div>
<div class="controls">
  <span class="streaming-dot off" id="stream-dot"></span>
  <button onclick="doThink()" id="btn-think">Think</button>
  <button onclick="doCompose()" id="btn-compose">Compose</button>
  <button onclick="doIntrospect()" id="btn-introspect">Introspect</button>
  <button onclick="doStream()" id="btn-stream">Stream</button>
  <button onclick="doDream()" id="btn-dream">Dream</button>
  <select id="topic-select">
    <option value="">Any topic</option>
    <option value="Abstract">Abstract</option>
    <option value="Mind & Cognition">Mind & Cognition</option>
    <option value="Time & Change">Time & Change</option>
    <option value="Nature">Nature</option>
    <option value="Arts">Arts</option>
    <option value="Science">Science</option>
    <option value="Physical World">Physical World</option>
    <option value="Social">Social</option>
    <option value="Virtue & Ethics">Virtue & Ethics</option>
    <option value="Living Things">Living Things</option>
    <option value="Technology">Technology</option>
    <option value="Spiritual & Religious">Spiritual & Religious</option>
  </select>
  <input type="number" id="stream-count" value="10" min="1" max="50" style="width:60px" title="Stream count">
  <button onclick="stopStream()" id="btn-stop" disabled>Stop</button>
</div>
<script>
const BASE = '';
let streaming = false;
let eventSource = null;
let thoughtCount = 0;
let recentScores = [];

function getTopic() { return document.getElementById('topic-select').value || undefined; }

function addThought(content, score, domains, type, evalEvents) {
  const pane = document.getElementById('thoughts');
  if (pane.querySelector('.empty-state')) pane.innerHTML = '';

  const div = document.createElement('div');
  const overall = score?.overall || 0;
  let cls = 'thought';
  if (type === 'introspection') cls += ' introspection';
  else if (type === 'composition') cls += ' composition';
  else if (overall >= 0.5) cls += ' high';
  else if (overall >= 0.3) cls += ' mid';
  else if (overall > 0) cls += ' low';
  div.className = cls;

  const scoreTxt = overall > 0 ? `q:${overall.toFixed(2)}` : '';
  const domTxt = domains?.length ? domains.join(', ') : '';
  div.innerHTML = `<div class="content">${escHtml(content)}</div>
    <div class="meta">
      ${scoreTxt ? `<span class="score">${scoreTxt}</span>` : ''}
      ${domTxt ? `<span class="domains">${domTxt}</span>` : ''}
    </div>`;
  pane.appendChild(div);
  if (evalEvents && evalEvents.length > 0) {
    addForkIndicators(div, evalEvents);
    evalEvents.forEach(ev => addEvalEvent(ev));
  }
  pane.scrollTop = pane.scrollHeight;

  thoughtCount++;
  if (overall > 0) { recentScores.push(overall); if (recentScores.length > 10) recentScores.shift(); }
  document.getElementById('thought-count').textContent = thoughtCount;
  if (recentScores.length > 0) {
    const avg = recentScores.reduce((a,b) => a+b, 0) / recentScores.length;
    document.getElementById('quality-avg').textContent = avg.toFixed(3);
  }
}

function updateDashboard(data) {
  if (data.vocab_coverage !== undefined) {
    const pct = data.vocab_coverage;
    document.getElementById('vocab-bar').style.width = pct + '%';
    document.getElementById('vocab-label').textContent = pct.toFixed(1) + '%';
  }
  if (data.vocabulary) {
    document.getElementById('vocab-used').textContent = data.vocabulary.used + '/' + data.vocabulary.total;
    document.getElementById('vocab-domains').textContent = data.vocabulary.domains_explored + '/' + data.vocabulary.domains_total;
    const pct = data.vocabulary.coverage_pct;
    document.getElementById('vocab-bar').style.width = pct + '%';
    document.getElementById('vocab-label').textContent = pct.toFixed(1) + '%';
  }
  if (data.emotional_momentum !== undefined) {
    const emo = data.emotional_momentum;
    document.getElementById('emo-value').textContent = (emo >= 0 ? '+' : '') + emo.toFixed(3);
    const dot = document.getElementById('emo-dot');
    dot.style.background = emo > 0.3 ? '#4a4' : emo < -0.3 ? '#a44' : '#aa4';
  }
  if (data.narrative !== undefined) {
    document.getElementById('narrative').textContent = data.narrative || 'None';
  }
  if (data.iteration !== undefined) {
    document.getElementById('iteration').textContent = data.iteration;
  }
  if (data.learning_goals) {
    const gl = document.getElementById('goals-list');
    if (data.learning_goals.length === 0) {
      gl.innerHTML = '<span class="personality-trait">None active</span>';
    } else {
      gl.innerHTML = data.learning_goals.map(g => {
        const pct = Math.round((g.progress || 0) * 100);
        return `<div class="goal">${g.type}: ${pct}%</div>`;
      }).join('');
    }
  }
  if (data.personality && data.personality.runs_analyzed > 0) {
    const p = data.personality;
    const s = p.thinking_style || {};
    const pi = document.getElementById('personality-info');
    pi.innerHTML = `
      <div class="personality-trait">Sessions: ${p.runs_analyzed}</div>
      <div class="personality-trait">Depth: ${(s.depth_preference||0).toFixed(2)} | Breadth: ${(s.breadth_preference||0).toFixed(2)}</div>
      <div class="personality-trait">Novelty seeking: ${(s.novelty_seeking||0).toFixed(2)}</div>
      <div class="personality-trait">Emotional baseline: ${(p.emotional_baseline||0).toFixed(3)}</div>
    `;
  }
  if (data.prompt_adjustments_active !== undefined) {
    document.getElementById('prompt-mods').textContent = data.prompt_adjustments_active;
  }
}

function escHtml(s) { const d = document.createElement('div'); d.textContent = s; return d.innerHTML; }

let evalEventCount = 0;
function addEvalEvent(ev) {
  const pane = document.getElementById('eval-events');
  const div = document.createElement('div');
  div.className = 'eval-event ' + (ev.type || '');
  let detail = '';
  if (ev.type === 'score') {
    const s = ev.scores || {};
    detail = `nov:${(s.novelty||0).toFixed(2)} div:${(s.diversity||0).toFixed(2)} coh:${(s.coherence||0).toFixed(2)} dep:${(s.depth||0).toFixed(2)} → <b>${(s.overall||0).toFixed(2)}</b>`;
  } else if (ev.type === 'oracle' || ev.type === 'oracle_sub') {
    detail = `${ev.oracle_type || '?'} L${ev.depth || 0} ${ev.async ? '(async)' : ''} "${escHtml((ev.thought_preview||'').substring(0,40))}"`;
  } else if (ev.type === 'oracle_result') {
    const icon = ev.success ? '✓' : '✗';
    detail = `${icon} ${ev.oracle_type} L${ev.depth} ${ev.duration_ms||0}ms ${ev.cached?'(cached)':''} "${escHtml((ev.result_preview||'').substring(0,40))}"`;
  } else if (ev.type === 'compose') {
    detail = `${ev.phase} [${ev.domain}] step ${ev.step}/${ev.total_steps} "${escHtml((ev.result_preview||'').substring(0,40))}"`;
  } else if (ev.type === 'introspect') {
    detail = `q:${(ev.quality_avg||0).toFixed(2)} emo:${(ev.emotional_momentum||0).toFixed(2)} "${escHtml((ev.result_preview||'').substring(0,40))}"`;
  } else if (ev.type === 'quality_feedback') {
    detail = `${ev.adjustment_type}: "${escHtml((ev.instruction||'').substring(0,50))}" (score=${(ev.trigger_score||0).toFixed(2)})`;
  } else {
    detail = JSON.stringify(ev).substring(0, 80);
  }
  div.innerHTML = `<span class="ev-type">${ev.type||'?'}</span> <span class="ev-detail">${detail}</span>`;
  pane.appendChild(div);
  pane.scrollTop = pane.scrollHeight;
  // Keep only last 50
  while (pane.children.length > 50) pane.removeChild(pane.firstChild);
  evalEventCount++;
  document.getElementById('eval-count').textContent = evalEventCount;
}

function addForkIndicators(thought_div, events) {
  if (!events || events.length === 0) return;
  const meta = thought_div.querySelector('.meta');
  if (!meta) return;
  events.forEach(ev => {
    if (ev.type === 'oracle' || ev.type === 'oracle_sub') {
      const span = document.createElement('span');
      span.className = 'fork-indicator oracle';
      span.textContent = `⚡${ev.oracle_type||'?'}${ev.depth>0?'L'+ev.depth:''}`;
      span.title = `Oracle: ${ev.oracle_type} depth ${ev.depth}`;
      meta.appendChild(span);
    } else if (ev.type === 'compose') {
      const span = document.createElement('span');
      span.className = 'fork-indicator compose';
      span.textContent = `🔗${ev.phase}`;
      span.title = `Compose: ${ev.phase} in ${ev.domain}`;
      meta.appendChild(span);
    } else if (ev.type === 'introspect') {
      const span = document.createElement('span');
      span.className = 'fork-indicator introspect';
      span.textContent = '🔍intro';
      meta.appendChild(span);
    } else if (ev.type === 'quality_feedback') {
      const span = document.createElement('span');
      span.className = 'fork-indicator feedback';
      span.textContent = '⚠️fix';
      span.title = ev.instruction || '';
      meta.appendChild(span);
    }
  });
}

function setButtons(disabled) {
  ['btn-think','btn-compose','btn-introspect','btn-dream'].forEach(id => {
    document.getElementById(id).disabled = disabled;
  });
}

async function loadStatus() {
  try {
    const r = await fetch(BASE + '/status');
    const data = await r.json();
    updateDashboard(data);
    document.getElementById('header-status').textContent = 'Connected';
  } catch(e) {
    document.getElementById('header-status').textContent = 'Error: ' + e.message;
  }
}

async function doThink() {
  setButtons(true);
  try {
    const body = {};
    const topic = getTopic();
    if (topic) body.topic = topic;
    const r = await fetch(BASE + '/think', { method: 'POST', headers: {'Content-Type':'application/json'}, body: JSON.stringify(body) });
    const data = await r.json();
    addThought(data.thought, data.score, [], 'thought', data.eval_events);
    updateDashboard(data);
  } catch(e) { addThought('Error: ' + e.message, {}, [], 'thought'); }
  setButtons(false);
  loadStatus();
}

async function doCompose() {
  setButtons(true);
  try {
    const body = { depth: 3 };
    const topic = getTopic();
    if (topic) body.domain = topic;
    const r = await fetch(BASE + '/compose', { method: 'POST', headers: {'Content-Type':'application/json'}, body: JSON.stringify(body) });
    const data = await r.json();
    if (data.thoughts) {
      const evts = data.eval_events || [];
      data.thoughts.forEach((t, i) => addThought(t, {}, [data.domain], 'composition', i === 0 ? evts : []));
    }
    updateDashboard(data);
  } catch(e) { addThought('Error: ' + e.message, {}, [], 'thought'); }
  setButtons(false);
  loadStatus();
}

async function doIntrospect() {
  setButtons(true);
  try {
    const body = {};
    const topic = getTopic();
    if (topic) body.topic = topic;
    const r = await fetch(BASE + '/introspect', { method: 'POST', headers: {'Content-Type':'application/json'}, body: JSON.stringify(body) });
    const data = await r.json();
    if (data.introspection) {
      addThought(data.introspection, {}, ['Meta'], 'introspection', data.eval_events);
    } else {
      addThought(data.message || 'No introspection available', {}, [], 'thought');
    }
    updateDashboard(data);
  } catch(e) { addThought('Error: ' + e.message, {}, [], 'thought'); }
  setButtons(false);
  loadStatus();
}

function doStream() {
  if (streaming) return;
  streaming = true;
  document.getElementById('stream-dot').classList.remove('off');
  document.getElementById('btn-stream').classList.add('active');
  document.getElementById('btn-stop').disabled = false;
  setButtons(true);

  const count = document.getElementById('stream-count').value || 10;
  const topic = getTopic();
  let url = BASE + '/stream?iterations=' + count;
  if (topic) url += '&topic=' + encodeURIComponent(topic);

  eventSource = new EventSource(url);

  eventSource.addEventListener('thought', (e) => {
    const data = JSON.parse(e.data);
    addThought(data.thought, data.score, data.domains, 'thought');
    updateDashboard(data);
  });

  eventSource.addEventListener('introspection', (e) => {
    const data = JSON.parse(e.data);
    addThought(data.content, {}, ['Meta'], 'introspection');
  });

  eventSource.addEventListener('eval', (e) => {
    const data = JSON.parse(e.data);
    if (data.events) data.events.forEach(ev => addEvalEvent(ev));
  });

  eventSource.addEventListener('status', (e) => {
    const data = JSON.parse(e.data);
    updateDashboard(data);
  });

  eventSource.addEventListener('done', (e) => {
    const data = JSON.parse(e.data);
    updateDashboard(data);
    stopStream();
    loadStatus();
  });

  eventSource.onerror = () => { stopStream(); };
}

function stopStream() {
  if (eventSource) { eventSource.close(); eventSource = null; }
  streaming = false;
  document.getElementById('stream-dot').classList.add('off');
  document.getElementById('btn-stream').classList.remove('active');
  document.getElementById('btn-stop').disabled = true;
  setButtons(false);
}

async function doDream() {
  setButtons(true);
  try {
    const r = await fetch(BASE + '/dream', { method: 'POST', headers: {'Content-Type':'application/json'}, body: JSON.stringify({iterations: 20}) });
    const data = await r.json();
    addThought('Dream session started: ' + (data.message || ''), {}, [], 'composition');
  } catch(e) { addThought('Error: ' + e.message, {}, [], 'thought'); }
  setButtons(false);
}

// Initial load
loadStatus();
setInterval(loadStatus, 10000);
</script>
</body>
</html>"""

# Global consciousness instance (lazy-initialized)
_consciousness: Optional[RecursiveConsciousness] = None
_background_task: Optional[Thread] = None
_stream_subscribers: list = []  # List of queue.Queue for SSE clients
_stream_lock = Thread.__class__  # placeholder, replaced below

from threading import Lock
_stream_lock = Lock()


def broadcast_thought(event_data: Dict):
    """Send a thought event to all SSE subscribers."""
    with _stream_lock:
        dead = []
        for q in _stream_subscribers:
            try:
                q.put_nowait(event_data)
            except queue.Full:
                dead.append(q)
        for q in dead:
            _stream_subscribers.remove(q)


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

    def _send_html(self, html: str, status: int = 200):
        """Send HTML response."""
        self.send_response(status)
        self.send_header('Content-Type', 'text/html; charset=utf-8')
        self.send_header('Access-Control-Allow-Origin', '*')
        self.end_headers()
        self.wfile.write(html.encode('utf-8'))

    def do_GET(self):
        """Handle GET requests."""
        parsed = urlparse(self.path)
        path = parsed.path
        params = parse_qs(parsed.query)

        if path == '/' or path == '/ui':
            self._send_html(WEB_UI_HTML)
            return
        elif path == '/status':
            self._handle_status()
        elif path == '/stats':
            self._handle_stats()
        elif path == '/graph':
            self._handle_graph()
        elif path == '/replay':
            last_n = int(params.get('last', [0])[0]) or None
            topic = params.get('topic', [None])[0]
            self._handle_replay(last_n=last_n, topic=topic)
        elif path == '/genealogy':
            self._handle_genealogy()
        elif path == '/stream':
            topic = params.get('topic', [None])[0]
            iterations = int(params.get('iterations', [20])[0])
            self._handle_stream(topic=topic, iterations=iterations)
        elif path == '/events':
            last_n = int(params.get('last', [50])[0])
            self._handle_events(last_n)
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
        elif path == '/compose':
            self._handle_compose(body)
        elif path == '/introspect':
            self._handle_introspect(body)
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
            'prompt_adjustments_active': len(rc.prompt_adjustments),
            'personality': rc.personality if rc.personality else None,
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

    def _handle_genealogy(self):
        """Return thought genealogy tree."""
        rc = get_consciousness()
        genealogy = rc.get_thought_genealogy()
        self._send_json(genealogy)

    def _handle_events(self, last_n: int = 50):
        """Return recent evaluation events."""
        rc = get_consciousness()
        events = rc.get_recent_eval_events(last_n)
        self._send_json({'events': events, 'total': len(rc.eval_events)})

    def _handle_stream(self, topic: str = None, iterations: int = 20):
        """SSE stream of live thoughts. Generates thoughts and pushes them as events."""
        self.send_response(200)
        self.send_header('Content-Type', 'text/event-stream')
        self.send_header('Cache-Control', 'no-cache')
        self.send_header('Connection', 'keep-alive')
        self.send_header('Access-Control-Allow-Origin', '*')
        self.end_headers()

        rc = get_consciousness(topic)
        stop_event = Event()

        def send_event(event_type: str, data: dict):
            try:
                payload = f"event: {event_type}\ndata: {json.dumps(data)}\n\n"
                self.wfile.write(payload.encode())
                self.wfile.flush()
                return True
            except (BrokenPipeError, ConnectionResetError):
                stop_event.set()
                return False

        # Send initial status
        send_event('status', {
            'iteration': rc.iteration,
            'topic': rc.topic,
            'vocab_coverage': round(len(rc.vocab_used) / len(rc.validator.vocab) * 100, 1),
            'emotional_momentum': round(rc.emotional_momentum, 3),
        })

        iterations = min(iterations, 100)
        for i in range(iterations):
            if stop_event.is_set():
                break

            rc.iteration += 1
            thought = rc.think()

            score = {}
            if rc.thought_history and 'score' in rc.thought_history[-1]:
                score = rc.thought_history[-1]['score']

            thought_data = {
                'thought': thought,
                'iteration': rc.iteration,
                'score': score,
                'narrative': rc.narrative_thread,
                'emotional_momentum': round(rc.emotional_momentum, 3),
                'vocab_coverage': round(len(rc.vocab_used) / len(rc.validator.vocab) * 100, 1),
                'domains': rc.thought_history[-1].get('domains', []) if rc.thought_history else [],
            }

            if not send_event('thought', thought_data):
                break

            # Broadcast to any other subscribers
            broadcast_thought(thought_data)

            # Send any new eval events since last thought
            new_events = rc.eval_events[-(len(rc.eval_events)):]
            # Only send events from this iteration
            iter_events = [e for e in new_events if e.get('iteration') == rc.iteration]
            if iter_events:
                send_event('eval', {'events': iter_events})

            # Run introspection every 10th thought in stream
            if i > 0 and i % 10 == 0:
                intro = rc.introspect()
                if intro:
                    send_event('introspection', {'content': intro})

            # Save memory periodically
            if i > 0 and i % 5 == 0:
                rc._save_memory()

            time.sleep(2)

        # Final status
        send_event('done', {
            'total_thoughts': len(rc.thought_history),
            'vocab_coverage': round(len(rc.vocab_used) / len(rc.validator.vocab) * 100, 1),
            'emotional_momentum': round(rc.emotional_momentum, 3),
        })
        rc._save_memory()

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
        pre_event_count = len(rc.eval_events)
        rc.iteration += 1

        thought = rc.think()

        # Get score from the last tracked thought
        score = {}
        if rc.thought_history and 'score' in rc.thought_history[-1]:
            score = rc.thought_history[-1]['score']

        # Collect eval events generated during this think call
        new_events = rc.eval_events[pre_event_count:]

        self._send_json({
            'thought': thought,
            'iteration': rc.iteration,
            'score': score,
            'narrative': rc.narrative_thread,
            'brain_state_size': len(rc.brain_state),
            'vocab_coverage': round(len(rc.vocab_used) / len(rc.validator.vocab) * 100, 1),
            'eval_events': new_events,
        })

    def _handle_compose(self, body: Dict):
        """Generate a composed chain of thoughts."""
        topic = body.get('topic')
        domain = body.get('domain', topic or 'Abstract')
        depth = min(body.get('depth', 3), 5)

        rc = get_consciousness(topic)
        pre_event_count = len(rc.eval_events)
        rc.iteration += 1

        thoughts = rc.compose_thoughts(theme_domain=domain, depth=depth)

        new_events = rc.eval_events[pre_event_count:]

        self._send_json({
            'thoughts': thoughts,
            'domain': domain,
            'depth': depth,
            'count': len(thoughts),
            'vocab_coverage': round(len(rc.vocab_used) / len(rc.validator.vocab) * 100, 1),
            'eval_events': new_events,
        })

    def _handle_introspect(self, body: Dict):
        """Generate an introspective thought about thinking patterns."""
        topic = body.get('topic')
        rc = get_consciousness(topic)
        pre_event_count = len(rc.eval_events)

        introspection = rc.introspect()
        new_events = rc.eval_events[pre_event_count:]

        if introspection:
            self._send_json({
                'introspection': introspection,
                'emotional_momentum': round(rc.emotional_momentum, 3),
                'goals_active': len(rc.current_goals),
                'vocab_coverage': round(len(rc.vocab_used) / len(rc.validator.vocab) * 100, 1),
                'eval_events': new_events,
            })
        else:
            self._send_json({
                'introspection': None,
                'message': 'Not enough thought history for introspection (need 8+ thoughts)',
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

    server = HTTPServer(('0.0.0.0', port), ConsciousnessHandler)
    print(f"Consciousness API running on http://localhost:{port}")
    print()
    print("Endpoints:")
    print(f"  GET  http://localhost:{port}/          - Web UI")
    print(f"  GET  http://localhost:{port}/status   - System status")
    print(f"  GET  http://localhost:{port}/stats    - Full analytics (JSON)")
    print(f"  GET  http://localhost:{port}/graph      - Concept graph")
    print(f"  GET  http://localhost:{port}/genealogy  - Thought genealogy tree")
    print(f"  GET  http://localhost:{port}/replay     - Thought timeline")
    print(f"  GET  http://localhost:{port}/stream     - SSE live thought stream")
    print(f"  GET  http://localhost:{port}/health   - Health check")
    print(f"  POST http://localhost:{port}/think      - Generate thought")
    print(f"  POST http://localhost:{port}/compose    - Compose thought chain")
    print(f"  POST http://localhost:{port}/introspect - Introspective meta-thought")
    print(f"  POST http://localhost:{port}/dream      - Start dream session")
    print(f"  POST http://localhost:{port}/ensemble   - Start ensemble")
    print()
    print("Press Ctrl+C to stop.")

    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nShutting down.")
        server.server_close()


if __name__ == "__main__":
    main()
