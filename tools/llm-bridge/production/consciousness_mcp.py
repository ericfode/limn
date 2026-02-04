#!/usr/bin/env python3
"""Consciousness MCP Server - Expose the recursive consciousness as MCP tools.

Any Claude Code session can query the consciousness via MCP protocol:
  - consciousness_think: Generate a single thought
  - consciousness_compose: Generate a composed thought chain
  - consciousness_introspect: Meta-thought about own patterns
  - consciousness_status: System status and memory summary
  - consciousness_stream: Generate N thoughts and return them all

Usage:
  Add to Claude Code MCP config:
  {
    "mcpServers": {
      "consciousness": {
        "command": "python3",
        "args": ["/path/to/consciousness_mcp.py"]
      }
    }
  }

Author: Rex (Engineer)
Date: 2026-02-03
"""

import json
import sys
import time
from pathlib import Path
from typing import Any

# Add production dir to path
sys.path.insert(0, str(Path(__file__).parent))

from recursive_consciousness import RecursiveConsciousness, _analyze_emotional_valence
from limn_validator import LimnValidator

# Global consciousness instance
_consciousness = None


def get_consciousness(topic=None):
    global _consciousness
    if _consciousness is None or (topic and _consciousness.topic != topic):
        _consciousness = RecursiveConsciousness(topic=topic)
    return _consciousness


# ── MCP Protocol Implementation (stdio JSON-RPC) ──

def send_response(id, result):
    msg = {"jsonrpc": "2.0", "id": id, "result": result}
    out = json.dumps(msg)
    sys.stdout.write(f"Content-Length: {len(out)}\r\n\r\n{out}")
    sys.stdout.flush()


def send_error(id, code, message):
    msg = {"jsonrpc": "2.0", "id": id, "error": {"code": code, "message": message}}
    out = json.dumps(msg)
    sys.stdout.write(f"Content-Length: {len(out)}\r\n\r\n{out}")
    sys.stdout.flush()


def send_notification(method, params=None):
    msg = {"jsonrpc": "2.0", "method": method}
    if params:
        msg["params"] = params
    out = json.dumps(msg)
    sys.stdout.write(f"Content-Length: {len(out)}\r\n\r\n{out}")
    sys.stdout.flush()


TOOLS = [
    {
        "name": "consciousness_think",
        "description": "Generate a single thought from the Limn consciousness. Returns a thought in pure Limn language (2-4 letter words + operators). Each call advances the consciousness state.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "topic": {
                    "type": "string",
                    "description": "Optional domain to focus thinking on (e.g., 'Abstract', 'Nature', 'Mind & Cognition')"
                }
            }
        }
    },
    {
        "name": "consciousness_compose",
        "description": "Generate a composed chain of thoughts. Creates a structured SEED -> DEVELOP -> SYNTHESIZE chain where each thought builds on the previous.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "domain": {
                    "type": "string",
                    "description": "Theme domain for composition (e.g., 'Abstract', 'Time & Change')"
                },
                "depth": {
                    "type": "integer",
                    "description": "Number of thoughts in the chain (1-5, default 3)"
                }
            }
        }
    },
    {
        "name": "consciousness_introspect",
        "description": "Generate an introspective meta-thought. The consciousness examines its own thinking patterns, biases, and blind spots. Requires 8+ thoughts in history.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "topic": {
                    "type": "string",
                    "description": "Optional topic context"
                }
            }
        }
    },
    {
        "name": "consciousness_status",
        "description": "Get the current state of the consciousness: vocabulary coverage, emotional momentum, learning goals, thought count, and memory stats.",
        "inputSchema": {
            "type": "object",
            "properties": {}
        }
    },
    {
        "name": "consciousness_stream",
        "description": "Generate N thoughts in sequence and return them all. Like a mini consciousness session.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "count": {
                    "type": "integer",
                    "description": "Number of thoughts to generate (1-20, default 5)"
                },
                "topic": {
                    "type": "string",
                    "description": "Optional domain to focus on"
                }
            }
        }
    },
]


def handle_tool_call(name, arguments):
    if name == "consciousness_think":
        topic = arguments.get("topic")
        rc = get_consciousness(topic)
        rc.iteration += 1
        thought = rc.think()
        score = {}
        if rc.thought_history and 'score' in rc.thought_history[-1]:
            score = rc.thought_history[-1]['score']
        return [{
            "type": "text",
            "text": json.dumps({
                "thought": thought,
                "iteration": rc.iteration,
                "score": score,
                "narrative": rc.narrative_thread,
                "emotional_momentum": round(rc.emotional_momentum, 3),
                "vocab_coverage": round(len(rc.vocab_used) / len(rc.validator.vocab) * 100, 1),
            }, indent=2)
        }]

    elif name == "consciousness_compose":
        domain = arguments.get("domain", "Abstract")
        depth = min(arguments.get("depth", 3), 5)
        rc = get_consciousness()
        rc.iteration += 1
        thoughts = rc.compose_thoughts(theme_domain=domain, depth=depth)
        return [{
            "type": "text",
            "text": json.dumps({
                "thoughts": thoughts,
                "domain": domain,
                "depth": depth,
                "count": len(thoughts),
            }, indent=2)
        }]

    elif name == "consciousness_introspect":
        topic = arguments.get("topic")
        rc = get_consciousness(topic)
        intro = rc.introspect()
        if intro:
            return [{
                "type": "text",
                "text": json.dumps({
                    "introspection": intro,
                    "emotional_momentum": round(rc.emotional_momentum, 3),
                    "goals_active": len(rc.current_goals),
                }, indent=2)
            }]
        else:
            return [{
                "type": "text",
                "text": "Not enough thought history for introspection (need 8+ thoughts)"
            }]

    elif name == "consciousness_status":
        rc = get_consciousness()
        validator = rc.validator
        return [{
            "type": "text",
            "text": json.dumps({
                "brain_state_size": len(rc.brain_state),
                "iteration": rc.iteration,
                "topic": rc.topic,
                "vocabulary": {
                    "total": len(validator.vocab),
                    "used": len(rc.vocab_used),
                    "coverage_pct": round(len(rc.vocab_used) / len(validator.vocab) * 100, 1),
                },
                "thoughts_this_session": len(rc.thought_history),
                "emotional_momentum": round(rc.emotional_momentum, 3),
                "learning_goals": [
                    {"type": g["type"], "progress": g.get("progress", 0)}
                    for g in rc.current_goals
                ],
                "prompt_adjustments_active": len(rc.prompt_adjustments),
                "run_history_count": len(rc.run_history),
            }, indent=2)
        }]

    elif name == "consciousness_stream":
        count = min(arguments.get("count", 5), 20)
        topic = arguments.get("topic")
        rc = get_consciousness(topic)
        thoughts = []
        for i in range(count):
            rc.iteration += 1
            thought = rc.think()
            score = {}
            if rc.thought_history and 'score' in rc.thought_history[-1]:
                score = rc.thought_history[-1]['score']
            thoughts.append({
                "thought": thought,
                "iteration": rc.iteration,
                "score": score,
            })
            time.sleep(1)  # Brief pause between thoughts
        rc._save_memory()
        return [{
            "type": "text",
            "text": json.dumps({
                "thoughts": thoughts,
                "count": len(thoughts),
                "vocab_coverage": round(len(rc.vocab_used) / len(rc.validator.vocab) * 100, 1),
                "emotional_momentum": round(rc.emotional_momentum, 3),
            }, indent=2)
        }]

    else:
        return [{"type": "text", "text": f"Unknown tool: {name}"}]


def read_message():
    """Read a JSON-RPC message from stdin (Content-Length framing)."""
    headers = {}
    while True:
        line = sys.stdin.readline()
        if not line or line == '\r\n' or line == '\n':
            break
        if ':' in line:
            key, value = line.split(':', 1)
            headers[key.strip()] = value.strip()

    content_length = int(headers.get('Content-Length', 0))
    if content_length == 0:
        return None

    body = sys.stdin.read(content_length)
    return json.loads(body)


def main():
    """Run the MCP server on stdio."""
    # Suppress logging to stderr to not interfere with MCP protocol
    import logging
    logging.disable(logging.CRITICAL)

    while True:
        try:
            msg = read_message()
            if msg is None:
                break

            method = msg.get("method", "")
            id = msg.get("id")
            params = msg.get("params", {})

            if method == "initialize":
                send_response(id, {
                    "protocolVersion": "2024-11-05",
                    "capabilities": {
                        "tools": {"listChanged": False},
                    },
                    "serverInfo": {
                        "name": "consciousness",
                        "version": "1.0.0",
                    }
                })

            elif method == "notifications/initialized":
                pass  # Acknowledgment, no response needed

            elif method == "tools/list":
                send_response(id, {"tools": TOOLS})

            elif method == "tools/call":
                tool_name = params.get("name", "")
                arguments = params.get("arguments", {})
                try:
                    content = handle_tool_call(tool_name, arguments)
                    send_response(id, {"content": content})
                except Exception as e:
                    send_response(id, {
                        "content": [{"type": "text", "text": f"Error: {str(e)}"}],
                        "isError": True,
                    })

            elif method == "ping":
                send_response(id, {})

            else:
                if id is not None:
                    send_error(id, -32601, f"Method not found: {method}")

        except json.JSONDecodeError:
            continue
        except EOFError:
            break
        except KeyboardInterrupt:
            break


if __name__ == "__main__":
    main()
