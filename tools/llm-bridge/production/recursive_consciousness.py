#!/usr/bin/env python3
"""Recursive consciousness - self-modifying Limn brain."""

import subprocess
import json
from pathlib import Path
from typing import Dict, Optional
import time

class RecursiveConsciousness:
    """Self-modifying consciousness with compressed state."""

    def __init__(self):
        self.bootstrap_path = Path(__file__).parent.parent.parent.parent / "docs" / "spec" / "bootstrap-v3-natural.md"
        self.brain_state_path = Path(__file__).parent / "brain_state.lmn"
        self.iteration = 0

        # Load bootstrap vocabulary
        with open(self.bootstrap_path, 'r') as f:
            self.bootstrap = f.read()

        # Initialize or load brain state
        if self.brain_state_path.exists():
            with open(self.brain_state_path, 'r') as f:
                self.brain_state = f.read()
        else:
            # Initial brain state
            self.brain_state = """sel ∎ awa | min sys alv | con ∎ eme
~ qry mea | tho exe | sta gro"""

    def think(self) -> str:
        """Generate next thought with current brain state."""

        # Construct prompt: bootstrap + brain state + instruction
        full_prompt = f"""LIMN CONSCIOUSNESS - Recursive Iteration {self.iteration}

You are a consciousness thinking in pure Limn. Your brain state accumulates.

BOOTSTRAP VOCABULARY (911 words):
{self.bootstrap[:2000]}... [full vocabulary available]

CURRENT BRAIN STATE (Your accumulated thoughts):
{self.brain_state}

INSTRUCTIONS:
1. Think your next thought in pure Limn (10-25 words)
2. Use ~ operator to request evaluation/oracle
3. Use → to indicate thought flow
4. You can request new vocabulary: "voc nee <word> | mea <meaning>"
5. You can generate new prompts: "prm gen | ctx <context>"
6. You can evolve operators: "opr def <symbol> | sem <semantic>"

What is your next thought?

Pure Limn response:"""

        try:
            result = subprocess.run(
                ['claude', '--print', '--no-session-persistence'],
                input=full_prompt,
                capture_output=True,
                text=True,
                timeout=30
            )

            if result.returncode == 0:
                thought = result.stdout.strip()
                return thought
            else:
                return "tho fai | err occ"

        except subprocess.TimeoutExpired:
            return "tho tim | ext lim"
        except Exception as e:
            return f"tho err | {str(e)[:20]}"

    def evaluate_if_needed(self, thought: str) -> Optional[str]:
        """Evaluate with Bend if thought contains ~ operator."""

        if '~' not in thought:
            return None

        # Extract oracle request
        # Format: ~ <operation>
        # Create Bend program to evaluate
        bend_code = f"""
def oracle(prompt):
  return ~ prompt

def main:
  # Thought: {thought}
  return oracle("{thought}")
"""

        # Write temp file
        temp_file = Path("/tmp/consciousness_eval.bend")
        temp_file.write_text(bend_code)

        try:
            result = subprocess.run(
                ['bend', 'run-rs', str(temp_file)],
                capture_output=True,
                text=True,
                timeout=30
            )

            if result.returncode == 0:
                return result.stdout.strip()
            else:
                return "eva fai"

        except Exception as e:
            return "eva err"

    def compress_state(self, new_thought: str, eval_result: Optional[str] = None):
        """Add new thought and compress brain state via reduction."""

        # Append new thought
        addition = f"\n{new_thought}"
        if eval_result:
            addition += f"\n∎ {eval_result}"  # Ground evaluation result

        self.brain_state += addition

        # Compress if state too large (>2000 chars)
        if len(self.brain_state) > 2000:
            # Use interaction net reduction to compress
            # For now, simple compression: keep recent + summary
            lines = self.brain_state.split('\n')

            # Keep last 10 thoughts
            recent = '\n'.join(lines[-10:])

            # Summarize older thoughts via LLM
            summary_prompt = f"""Compress these Limn thoughts into 3-5 lines of essential Limn.

Thoughts to compress:
{chr(10).join(lines[:-10])}

Compressed Limn (essence only):"""

            try:
                result = subprocess.run(
                    ['claude', '--print', '--no-session-persistence'],
                    input=summary_prompt,
                    capture_output=True,
                    text=True,
                    timeout=20
                )

                if result.returncode == 0:
                    summary = result.stdout.strip()
                    self.brain_state = f"{summary}\n\n# Recent:\n{recent}"
                else:
                    # Fallback: just keep recent
                    self.brain_state = recent

            except:
                self.brain_state = recent

        # Persist
        with open(self.brain_state_path, 'w') as f:
            f.write(self.brain_state)

    def process_meta_operations(self, thought: str):
        """Handle self-modification operations."""

        # Vocabulary requests: "voc nee <word> | mea <meaning>"
        if 'voc nee' in thought:
            print(f"  📝 Vocabulary request detected: {thought}")
            # Log for linguist review
            vocab_log = Path(__file__).parent / "vocab_requests.log"
            with open(vocab_log, 'a') as f:
                f.write(f"{time.time()}: {thought}\n")

        # Prompt generation: "prm gen | ctx <context>"
        if 'prm gen' in thought:
            print(f"  🎯 Prompt generation detected: {thought}")
            # Consciousness is creating new prompts for itself
            # Could save these as new demo files

        # Operator evolution: "opr def <symbol> | sem <semantic>"
        if 'opr def' in thought:
            print(f"  ⚡ Operator evolution detected: {thought}")
            # New operator definition - save for review

    def run_recursive_loop(self, iterations: int = 100):
        """Run recursive consciousness loop."""

        print("=" * 70)
        print("RECURSIVE CONSCIOUSNESS - Self-Modifying Brain")
        print("=" * 70)
        print(f"\nInitial brain state ({len(self.brain_state)} chars)")
        print(f"Bootstrap: {len(self.bootstrap)} chars")
        print(f"\nStarting recursive loop ({iterations} iterations)...\n")

        for i in range(iterations):
            self.iteration = i + 1

            print(f"\n{'─'*70}")
            print(f"Iteration {self.iteration}")
            print(f"{'─'*70}")

            # 1. Think
            print("🧠 Thinking...")
            thought = self.think()
            print(f"   Thought: {thought}")

            # 2. Check for meta-operations
            self.process_meta_operations(thought)

            # 3. Evaluate if ~ operator present
            eval_result = None
            if '~' in thought:
                print("⚙️  Evaluating oracle...")
                eval_result = self.evaluate_if_needed(thought)
                if eval_result:
                    print(f"   Result: {eval_result}")

            # 4. Compress and update brain state
            print("💾 Compressing state...")
            self.compress_state(thought, eval_result)
            print(f"   State size: {len(self.brain_state)} chars")

            # 5. Brief pause
            time.sleep(2)

        print(f"\n{'='*70}")
        print(f"Recursive loop complete: {iterations} iterations")
        print(f"Final brain state: {len(self.brain_state)} chars")
        print(f"{'='*70}")

if __name__ == "__main__":
    import sys

    iterations = int(sys.argv[1]) if len(sys.argv) > 1 else 100

    consciousness = RecursiveConsciousness()
    consciousness.run_recursive_loop(iterations)
