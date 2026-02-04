#!/usr/bin/env python3
"""Consciousness TUI - Terminal UI for watching the consciousness think.

A split-pane terminal interface showing:
  - Live thought stream (left pane)
  - System dashboard (right pane): vocab coverage, emotion, goals, personality
  - Status bar (bottom): iteration, quality, narrative arc

Usage:
  python3 consciousness_tui.py [--topic "Domain"] [--iterations 50]

Controls:
  q     Quit
  p     Pause/resume
  i     Trigger introspection
  c     Trigger composition
  s     Save memory snapshot
  +/-   Speed up/slow down (adjust pause between thoughts)

Author: Rex (Engineer)
Date: 2026-02-03
"""

import curses
import json
import sys
import time
import re
from pathlib import Path
from threading import Thread, Event
from collections import defaultdict

# Add production dir to path
sys.path.insert(0, str(Path(__file__).parent))

from recursive_consciousness import RecursiveConsciousness
from limn_validator import LimnValidator


class ConsciousnessTUI:
    """Terminal UI for the recursive consciousness."""

    def __init__(self, topic=None, iterations=50):
        self.topic = topic
        self.max_iterations = iterations
        self.rc = RecursiveConsciousness(topic=topic)
        self.thoughts = []  # (thought, score, domains, timestamp)
        self.paused = Event()
        self.stop = Event()
        self.delay = 2.0  # Seconds between thoughts
        self.status_message = ""
        self.introspection_queue = Event()
        self.composition_queue = Event()

    def run(self):
        curses.wrapper(self._main)

    def _main(self, stdscr):
        curses.curs_set(0)
        stdscr.nodelay(True)
        stdscr.timeout(200)  # 200ms refresh

        if curses.has_colors():
            curses.start_color()
            curses.use_default_colors()
            curses.init_pair(1, curses.COLOR_CYAN, -1)     # Headers
            curses.init_pair(2, curses.COLOR_GREEN, -1)     # Good scores
            curses.init_pair(3, curses.COLOR_YELLOW, -1)    # Medium scores
            curses.init_pair(4, curses.COLOR_RED, -1)       # Low scores
            curses.init_pair(5, curses.COLOR_MAGENTA, -1)   # Operators
            curses.init_pair(6, curses.COLOR_WHITE, -1)     # Normal

        # Start thinking thread
        think_thread = Thread(target=self._think_loop, daemon=True)
        think_thread.start()

        while not self.stop.is_set():
            try:
                key = stdscr.getch()
            except curses.error:
                key = -1

            if key == ord('q'):
                self.stop.set()
                break
            elif key == ord('p'):
                if self.paused.is_set():
                    self.paused.clear()
                    self.status_message = "Resumed"
                else:
                    self.paused.set()
                    self.status_message = "Paused"
            elif key == ord('i'):
                self.introspection_queue.set()
                self.status_message = "Introspecting..."
            elif key == ord('c'):
                self.composition_queue.set()
                self.status_message = "Composing..."
            elif key == ord('s'):
                self.rc._save_memory()
                self.status_message = "Memory saved"
            elif key == ord('+') or key == ord('='):
                self.delay = max(0.5, self.delay - 0.5)
                self.status_message = f"Speed: {self.delay:.1f}s"
            elif key == ord('-'):
                self.delay = min(10.0, self.delay + 0.5)
                self.status_message = f"Speed: {self.delay:.1f}s"

            self._draw(stdscr)

        self.rc._save_memory()

    def _think_loop(self):
        """Background thread generating thoughts."""
        self.rc._set_learning_goals()

        for i in range(self.max_iterations):
            if self.stop.is_set():
                break

            while self.paused.is_set() and not self.stop.is_set():
                time.sleep(0.1)

            if self.stop.is_set():
                break

            # Check for introspection request
            if self.introspection_queue.is_set():
                self.introspection_queue.clear()
                intro = self.rc.introspect()
                if intro:
                    self.thoughts.append({
                        'content': f"[INTROSPECTION] {intro}",
                        'score': {},
                        'domains': ['Meta'],
                        'time': time.time(),
                        'type': 'introspection',
                    })

            # Check for composition request
            if self.composition_queue.is_set():
                self.composition_queue.clear()
                domain = self.topic or 'Abstract'
                composed = self.rc.compose_thoughts(theme_domain=domain, depth=3)
                if composed:
                    self.thoughts.append({
                        'content': f"[COMPOSED] {' | '.join(composed)}",
                        'score': {},
                        'domains': [domain],
                        'time': time.time(),
                        'type': 'composition',
                    })

            self.rc.iteration = i + 1
            thought = self.rc.think()

            score = {}
            domains = []
            if self.rc.thought_history and self.rc.thought_history[-1]:
                last = self.rc.thought_history[-1]
                score = last.get('score', {})
                domains = last.get('domains', [])

            self.thoughts.append({
                'content': thought,
                'score': score,
                'domains': domains,
                'time': time.time(),
                'type': 'thought',
            })

            # Periodic introspection
            if i > 0 and i % 15 == 0:
                intro = self.rc.introspect()
                if intro:
                    self.thoughts.append({
                        'content': f"[INTROSPECTION] {intro}",
                        'score': {},
                        'domains': ['Meta'],
                        'time': time.time(),
                        'type': 'introspection',
                    })

            if i > 0 and i % 10 == 0:
                self.rc._save_memory()

            time.sleep(self.delay)

        self.rc._record_run_summary()
        self.rc._update_personality()
        self.rc._save_memory()
        self.status_message = "Thinking complete"

    def _draw(self, stdscr):
        """Draw the full TUI layout."""
        height, width = stdscr.getmaxyx()
        if height < 10 or width < 40:
            return

        stdscr.erase()

        # Layout: left pane (thoughts), right pane (dashboard), bottom bar
        divider = width * 2 // 3
        dashboard_w = width - divider - 1

        # ── Header ──
        title = " LIMN CONSCIOUSNESS "
        stdscr.addstr(0, 0, "=" * width, curses.color_pair(1))
        stdscr.addstr(0, max(0, (width - len(title)) // 2), title, curses.color_pair(1) | curses.A_BOLD)

        # ── Left pane: Thought stream ──
        self._draw_thoughts(stdscr, 1, 0, height - 3, divider - 1)

        # ── Vertical divider ──
        for y in range(1, height - 2):
            try:
                stdscr.addch(y, divider, curses.ACS_VLINE, curses.color_pair(1))
            except curses.error:
                pass

        # ── Right pane: Dashboard ──
        self._draw_dashboard(stdscr, 1, divider + 1, height - 3, dashboard_w)

        # ── Status bar ──
        self._draw_status_bar(stdscr, height - 2, width)

        # ── Help bar ──
        help_text = " q:quit  p:pause  i:introspect  c:compose  s:save  +/-:speed "
        try:
            stdscr.addstr(height - 1, 0, help_text[:width-1], curses.color_pair(1))
        except curses.error:
            pass

        stdscr.refresh()

    def _draw_thoughts(self, stdscr, start_y, start_x, height, width):
        """Draw the thought stream in the left pane."""
        if not self.thoughts:
            try:
                stdscr.addstr(start_y + 1, start_x + 1, "Waiting for first thought...", curses.color_pair(3))
            except curses.error:
                pass
            return

        # Show most recent thoughts that fit
        visible = self.thoughts[-(height - 1):]

        for i, entry in enumerate(visible):
            y = start_y + i
            if y >= start_y + height:
                break

            content = entry['content']
            score = entry.get('score', {})
            overall = score.get('overall', 0)
            entry_type = entry.get('type', 'thought')

            # Color based on score/type
            if entry_type == 'introspection':
                color = curses.color_pair(5) | curses.A_BOLD
            elif entry_type == 'composition':
                color = curses.color_pair(1) | curses.A_BOLD
            elif overall >= 0.5:
                color = curses.color_pair(2)
            elif overall >= 0.3:
                color = curses.color_pair(3)
            elif overall > 0:
                color = curses.color_pair(4)
            else:
                color = curses.color_pair(6)

            # Format: score prefix + content
            if score and overall > 0:
                prefix = f"{overall:.1f} "
            else:
                prefix = "    "

            line = f"{prefix}{content}"
            try:
                stdscr.addnstr(y, start_x, line, width, color)
            except curses.error:
                pass

    def _draw_dashboard(self, stdscr, start_y, start_x, height, width):
        """Draw the system dashboard in the right pane."""
        rc = self.rc
        y = start_y

        def put(text, color=6, bold=False):
            nonlocal y
            if y >= start_y + height:
                return
            attr = curses.color_pair(color)
            if bold:
                attr |= curses.A_BOLD
            try:
                stdscr.addnstr(y, start_x, text, width, attr)
            except curses.error:
                pass
            y += 1

        # Iteration / Topic
        put(f"Iteration: {rc.iteration}", 1, True)
        if rc.topic:
            put(f"Topic: {rc.topic}", 6)
        put(f"Thoughts: {len(self.thoughts)}", 6)
        put("")

        # Vocabulary
        total = len(rc.validator.vocab)
        used = len(rc.vocab_used)
        pct = used / total * 100 if total else 0
        put("VOCABULARY", 1, True)
        bar_width = min(width - 6, 20)
        filled = int(bar_width * pct / 100)
        bar = "[" + "#" * filled + "." * (bar_width - filled) + "]"
        put(f" {bar} {pct:.0f}%", 2 if pct > 50 else 3)
        put(f" {used}/{total} words", 6)
        put(f" {len(rc.domains_explored)} domains", 6)
        put("")

        # Emotional state
        put("EMOTIONAL STATE", 1, True)
        emo = rc.emotional_momentum
        if emo > 0.3:
            put(f" {emo:+.2f} (positive)", 2)
        elif emo < -0.3:
            put(f" {emo:+.2f} (reflective)", 4)
        else:
            put(f" {emo:+.2f} (neutral)", 3)
        put("")

        # Narrative arc
        if rc.narrative_thread:
            put("NARRATIVE", 1, True)
            put(f" {rc.narrative_thread[:width-2]}", 5)
            arc_pos = rc.iteration % rc.narrative_arc_length
            phase = arc_pos * 4 // rc.narrative_arc_length
            phases = ["THESIS", "DEVELOP", "TENSION", "SYNTHESIS"]
            put(f" Phase: {phases[min(phase, 3)]}", 6)
            put("")

        # Learning goals
        if rc.current_goals:
            put("GOALS", 1, True)
            for g in rc.current_goals[:3]:
                progress = g.get('progress', 0)
                gtype = g['type'][:12]
                bar_w = min(width - 16, 10)
                filled = int(bar_w * min(progress, 1))
                bar = "#" * filled + "." * (bar_w - filled)
                put(f" {gtype}: [{bar}]", 2 if progress > 0.5 else 3)
            put("")

        # Prompt adjustments
        if rc.prompt_adjustments:
            put("SELF-MODS", 1, True)
            for adj in rc.prompt_adjustments[-2:]:
                put(f" {adj['type'][:15]}", 5)
            put("")

        # Personality
        if rc.personality and rc.personality.get('runs_analyzed', 0) >= 1:
            put("PERSONALITY", 1, True)
            style = rc.personality.get('thinking_style', {})
            depth = style.get('depth_preference', 0.5)
            breadth = style.get('breadth_preference', 0.5)
            novelty = style.get('novelty_seeking', 0.5)
            put(f" depth={depth:.1f} breadth={breadth:.1f}", 6)
            put(f" novelty={novelty:.1f}", 6)

        # Quality (last 5 thoughts)
        recent_scores = [t['score'] for t in self.thoughts[-5:] if t.get('score') and t['score'].get('overall')]
        if recent_scores:
            avg = sum(s['overall'] for s in recent_scores) / len(recent_scores)
            put("")
            put("QUALITY (last 5)", 1, True)
            color = 2 if avg > 0.5 else 3 if avg > 0.3 else 4
            put(f" Average: {avg:.2f}", color)

    def _draw_status_bar(self, stdscr, y, width):
        """Draw the status bar at the bottom."""
        rc = self.rc

        parts = []
        parts.append(f"iter:{rc.iteration}")

        if self.thoughts:
            last_score = self.thoughts[-1].get('score', {}).get('overall', 0)
            parts.append(f"q:{last_score:.2f}")

        parts.append(f"cov:{len(rc.vocab_used)/len(rc.validator.vocab)*100:.0f}%")
        parts.append(f"emo:{rc.emotional_momentum:+.1f}")
        parts.append(f"speed:{self.delay:.1f}s")

        if self.paused.is_set():
            parts.append("[PAUSED]")

        if self.status_message:
            parts.append(self.status_message)

        bar = " | ".join(parts)
        try:
            stdscr.addnstr(y, 0, bar, width - 1, curses.color_pair(1) | curses.A_REVERSE)
            stdscr.addstr(y, len(bar), " " * max(0, width - 1 - len(bar)), curses.color_pair(1) | curses.A_REVERSE)
        except curses.error:
            pass


def main():
    topic = None
    iterations = 50

    for i, arg in enumerate(sys.argv[1:]):
        if arg == '--topic' and i + 2 <= len(sys.argv) - 1:
            topic = sys.argv[i + 2]
        elif arg == '--iterations' and i + 2 <= len(sys.argv) - 1:
            iterations = int(sys.argv[i + 2])
        elif arg == '--help' or arg == '-h':
            print(__doc__)
            sys.exit(0)

    tui = ConsciousnessTUI(topic=topic, iterations=iterations)
    tui.run()


if __name__ == "__main__":
    main()
