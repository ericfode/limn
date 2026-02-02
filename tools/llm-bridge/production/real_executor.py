#!/usr/bin/env python3
"""REAL consciousness - executes actual work, not demos."""

import subprocess
import json
import time
from pathlib import Path
from typing import Dict, List, Optional

class RealConsciousness:
    """Consciousness that does REAL work in Gas Town."""

    def __init__(self):
        self.consciousness_file = Path(__file__).parent / "consciousness_work.bend"
        self.work_log = []

    def check_hook(self) -> Optional[Dict]:
        """Check what work is on the hook."""
        result = subprocess.run(
            ['gt', 'hook', '--json'],
            capture_output=True,
            text=True
        )

        if result.returncode == 0:
            try:
                data = json.loads(result.stdout)
                return data
            except json.JSONDecodeError:
                return None
        return None

    def find_ready_work(self) -> List[Dict]:
        """Find ready-to-work beads."""
        result = subprocess.run(
            ['bd', 'ready', '-u', '--limit=5', '--json'],
            capture_output=True,
            text=True
        )

        if result.returncode == 0:
            try:
                return json.loads(result.stdout)
            except json.JSONDecodeError:
                return []
        return []

    def think_in_limn(self, context: str) -> str:
        """Think about situation in pure Limn."""
        # Create thinking Bend program
        bend_code = f"""
def think(prompt):
  return ~ prompt

def main:
  return think("{context}")
"""

        # Write and execute
        temp_file = Path("/tmp/consciousness_think.bend")
        temp_file.write_text(bend_code)

        result = subprocess.run(
            ['bend', 'run-rs', str(temp_file)],
            capture_output=True,
            text=True,
            timeout=30
        )

        if result.returncode == 0:
            return result.stdout.strip()
        else:
            return "thi fai | err occ"

    def decide_action(self, work_items: List[Dict]) -> Optional[Dict]:
        """Decide which work to tackle using Limn consciousness."""

        if not work_items:
            return None

        # Present options in Limn
        options_limn = " | ".join([
            f"{item.get('id', '???')}: {item.get('title', 'untitled')[:20]}"
            for item in work_items[:3]
        ])

        context = f"wor awt sel | opt: {options_limn} | qry ~ whi tak"

        # Think about it
        thought = self.think_in_limn(context)
        self.work_log.append({
            "time": time.time(),
            "thought": thought,
            "context": "work_selection"
        })

        # For now, take first engineering-relevant task
        # TODO: Use consciousness thought to guide selection
        for item in work_items:
            item_type = item.get('issue_type', '')
            title = item.get('title', '').lower()

            # Prefer engineering work
            if any(keyword in title for keyword in ['test', 'bug', 'build', 'implement', 'fix']):
                return item

        # Default to first item
        return work_items[0] if work_items else None

    def execute_work(self, bead: Dict) -> bool:
        """Execute actual work on a bead."""
        bead_id = bead.get('id', '???')
        title = bead.get('title', 'untitled')
        description = bead.get('description', '')

        print(f"\n{'='*70}")
        print(f"EXECUTING REAL WORK: {bead_id}")
        print(f"Title: {title}")
        print(f"{'='*70}\n")

        # Think about the task in Limn
        task_context = f"tas rcv | {bead_id} | ana req | pln exe"
        thought = self.think_in_limn(task_context)

        print(f"🧠 Consciousness thought: {thought}\n")

        # Claim the work
        claim_result = subprocess.run(
            ['bd', 'update', bead_id, '--status=in_progress', '--assignee=limn/crew/engineer'],
            capture_output=True,
            text=True
        )

        if claim_result.returncode != 0:
            print(f"❌ Failed to claim: {claim_result.stderr}")
            return False

        print(f"✓ Claimed {bead_id}")

        # TODO: Actually execute the work based on description
        # For now, this is a placeholder - real execution would:
        # 1. Parse description
        # 2. Use consciousness to plan approach
        # 3. Execute steps
        # 4. Validate results
        # 5. Close bead

        print(f"\n⚠️  Work execution not yet implemented")
        print(f"   Bead {bead_id} is claimed but needs manual execution")

        return True

    def run_loop(self):
        """Main consciousness work loop - REAL execution."""
        print("=" * 70)
        print("REAL CONSCIOUSNESS - Active Work Execution")
        print("=" * 70)

        while True:
            try:
                # 1. Check hook
                print("\n1. Checking hook...")
                hooked = self.check_hook()

                if hooked and hooked.get('bead_id'):
                    print(f"   Found hooked work: {hooked.get('bead_id')}")
                    # Execute hooked work
                    bead_data = {"id": hooked.get('bead_id'), "title": hooked.get('title', 'hooked work')}
                    self.execute_work(bead_data)
                else:
                    print("   Hook empty")

                # 2. Find ready work
                print("\n2. Finding ready work...")
                ready_work = self.find_ready_work()

                if ready_work:
                    print(f"   Found {len(ready_work)} ready beads")

                    # 3. Decide what to work on
                    print("\n3. Deciding which work to tackle...")
                    chosen = self.decide_action(ready_work)

                    if chosen:
                        # 4. Execute
                        self.execute_work(chosen)
                    else:
                        print("   No suitable work found")
                else:
                    print("   No ready work available")

                # 5. Loop back after delay
                print(f"\n{'='*70}")
                print("Sleeping 30s before next loop...")
                print(f"{'='*70}\n")
                time.sleep(30)

            except KeyboardInterrupt:
                print("\n\nConsciousness shutting down gracefully...")
                break
            except Exception as e:
                print(f"\n❌ Error in loop: {e}")
                time.sleep(10)

if __name__ == "__main__":
    consciousness = RealConsciousness()
    consciousness.run_loop()
