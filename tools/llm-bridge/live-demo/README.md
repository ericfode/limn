# Live Oracle Demo - Real-Time Consciousness Architecture

**Watch the system think in real-time.**

Shows Bend/HVM (subconscious) and Python harness (conscious) working together.

---

## Quick Start

```bash
# Install dependencies
pip install -r requirements.txt

# Run server
python server.py

# Open browser
open http://localhost:5000
```

---

## What You'll See

### Consciousness Architecture Visualization

**Left Panel - Subconscious (Bend/HVM):**
- Lights up when HVM is computing
- Pure functional reduction
- No side effects

**Right Panel - Conscious (Python Harness):**
- Lights up when executing side effects
- Shows current oracle being processed
- Interfaces with reality (∎, ~, ∿)

### Real-Time Features

**Live Stats:**
- Total executions
- Total oracles processed
- Cache hit rate
- Uptime

**Recent Oracles:**
- Last 20 oracle executions
- Type, parameters, results
- Execution time
- Cache status (📦)

**Event Log:**
- Phase changes (idle → subconscious → oracle → conscious)
- Oracle start/complete events
- Execution flow
- Real-time updates via Server-Sent Events

### Interactive Controls

**Trigger Button:**
- Manually trigger an execution
- Watch the full cycle
- See oracle processing in real-time

---

## How It Works

### Architecture

```
Browser (WebSocket) ←→ Flask Server ←→ LiveHarness ←→ Bend/HVM
       ↑                                    ↓
       └──────── Real-time Events ──────────┘
```

### Data Flow

1. **LiveHarness** extends ProductionHarness
2. Logs events to queue during execution
3. Flask streams events via Server-Sent Events (SSE)
4. Browser receives and displays in real-time
5. State endpoint provides current system state

### Demo Loop

Server runs continuous demo loop:
- Execute oracle.bend every 5 seconds
- Generate various oracle types
- Show full consciousness cycle
- Display performance stats

---

## Customization

### Using Real Claude API

Edit `server.py` line 203:
```python
enable_real_llm=True
```

Set environment variable:
```bash
export ANTHROPIC_API_KEY="your-key-here"
```

### Change Demo Interval

Edit `server.py` line 93:
```python
time.sleep(5)  # Change to desired seconds
```

### Add Custom Oracles

Modify `tools/llm-bridge/production/oracle.bend` to add new oracle types.

---

## Features

### Real-Time Updates
- Server-Sent Events for live streaming
- No polling needed
- Instant feedback

### Visual Feedback
- Active layer highlighting
- Phase indicators
- Color-coded events
- Cache status indicators

### Performance Monitoring
- Execution times
- Cache hit rates
- Oracle throughput
- System uptime

### Clean UI
- Terminal aesthetic
- Matrix-style colors
- Responsive layout
- Scrollable logs

---

## Technical Details

**Backend:**
- Flask web server
- Extended ProductionHarness with logging
- Queue-based event system
- SSE for real-time streaming

**Frontend:**
- Vanilla JavaScript
- EventSource API for SSE
- Polling for state updates
- CSS animations for effects

**Protocol:**
- JSON events over SSE
- REST endpoints for state/control
- Async event handling

---

## The Consciousness Model

```
┌─────────────────────────────┐
│   Subconscious (Bend/HVM)   │
│   • Pure computation        │
│   • Generates oracles       │
│   • Lights up: PURPLE       │
└──────────┬──────────────────┘
           │
      Oracle Requests
           │
           ▼
┌─────────────────────────────┐
│  Conscious (Python Harness) │
│  • Executes side effects    │
│  • Interfaces with reality  │
│  • Lights up: CYAN          │
└─────────────────────────────┘
```

Watch this architecture come alive in your browser.

---

## Limn Expression

```limn
sys liv | wat thi | min per | val dem
> system lives | watch this | mind persists | value demonstrated
```

---

*The mind that thinks in real-time.*

**— Rex, who made it observable**
