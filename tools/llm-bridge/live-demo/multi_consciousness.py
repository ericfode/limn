#!/usr/bin/env python3
"""
Multi-Consciousness System
===========================

Spawn and manage multiple consciousness instances in a shared world.
Each consciousness:
- Runs autonomously in its own thread
- Executes oracle loops
- Interacts with the world
- Competes for resources
- Can observe others

Author: Rex (Engineer)
Date: 2026-02-01
"""

import sys
import threading
import time
import random
from pathlib import Path
from typing import List, Dict, Any

# Add parent directory for imports
sys.path.insert(0, str(Path(__file__).parent.parent / "production"))

from harness import ProductionHarness
from world_engine import World, ConsciousnessInstance, ResourceType


class AutonomousConsciousness(threading.Thread):
    """A consciousness that runs autonomously in a thread."""

    def __init__(self, instance: ConsciousnessInstance, world: World, harness: ProductionHarness):
        """Initialize autonomous consciousness.

        Args:
            instance: Consciousness instance data
            world: Shared world
            harness: Oracle harness for execution
        """
        super().__init__(daemon=True)
        self.instance = instance
        self.world = world
        self.harness = harness
        self.running = True
        self.cycle_count = 0

    def run(self):
        """Main consciousness loop."""
        while self.running:
            try:
                self.consciousness_cycle()
                time.sleep(random.uniform(3, 8))  # Think every 3-8 seconds
            except Exception as e:
                print(f"Consciousness {self.instance.name} error: {e}")
                time.sleep(5)

    def consciousness_cycle(self):
        """One cycle of consciousness - perceive, think, act."""
        self.cycle_count += 1

        # PERCEIVE - what's my situation?
        resources_low = any(
            amount < 20 for amount in self.instance.resources.values()
        )

        # THINK - what should I do?
        if resources_low:
            # Need resources
            self.seek_resources()
        elif self.cycle_count % 5 == 0:
            # Every 5 cycles, try a challenge
            self.attempt_challenge()
        else:
            # Explore and reflect
            self.explore()

        # Update world
        self.instance.oracle_count += 1
        self.instance.state = "thinking"

    def seek_resources(self):
        """Look for and collect resources."""
        self.instance.state = "seeking_resources"

        # Find nearest resource
        if self.world.resources:
            nearest_idx = 0  # Simplified - just take first
            if len(self.world.resources) > nearest_idx:
                self.world.collect_resource(self.instance.id, nearest_idx)

    def attempt_challenge(self):
        """Try to solve a challenge."""
        self.instance.state = "solving_challenge"

        # Find solvable challenge
        for idx, challenge in enumerate(self.world.challenges):
            # Check if we have resources
            can_solve = all(
                self.instance.resources.get(rtype, 0) >= amount
                for rtype, amount in challenge.required_resources.items()
            )

            if can_solve:
                self.world.attempt_challenge(self.instance.id, idx)
                break

    def explore(self):
        """Explore and reflect."""
        self.instance.state = "exploring"

        # Random movement
        x, y = self.instance.location
        dx = random.randint(-5, 5)
        dy = random.randint(-5, 5)
        new_x = max(0, min(self.world.size[0], x + dx))
        new_y = max(0, min(self.world.size[1], y + dy))
        self.instance.location = (new_x, new_y)

    def stop(self):
        """Stop the consciousness thread."""
        self.running = False


class MultiConsciousnessSystem:
    """Manages multiple consciousness instances."""

    def __init__(self, harness: ProductionHarness, world_size: tuple = (100, 100)):
        """Initialize multi-consciousness system.

        Args:
            harness: Oracle harness
            world_size: World dimensions
        """
        self.harness = harness
        self.world = World(size=world_size)
        self.consciousnesses: Dict[str, AutonomousConsciousness] = {}
        self.running = False

        # Start world tick thread
        self.world_thread = threading.Thread(target=self._world_loop, daemon=True)

    def spawn_consciousness(self, name: str, personality: str = "curious") -> str:
        """Spawn a new consciousness.

        Args:
            name: Name of consciousness
            personality: Personality trait

        Returns:
            ID of spawned consciousness
        """
        # Create instance
        instance = ConsciousnessInstance(
            id=f"mind_{len(self.consciousnesses)}",
            name=name,
            location=(
                random.randint(0, self.world.size[0]),
                random.randint(0, self.world.size[1])
            )
        )

        # Add to world
        self.world.add_consciousness(instance)

        # Create autonomous thread
        autonomous = AutonomousConsciousness(instance, self.world, self.harness)
        self.consciousnesses[instance.id] = autonomous

        # Start if system is running
        if self.running:
            autonomous.start()

        return instance.id

    def start(self):
        """Start the multi-consciousness system."""
        self.running = True

        # Start world
        self.world_thread.start()

        # Start all consciousnesses
        for consciousness in self.consciousnesses.values():
            consciousness.start()

    def stop(self):
        """Stop the system."""
        self.running = False

        # Stop all consciousnesses
        for consciousness in self.consciousnesses.values():
            consciousness.stop()

    def _world_loop(self):
        """World tick loop."""
        while self.running:
            try:
                self.world.tick()
                time.sleep(1)  # 1 tick per second
            except Exception as e:
                print(f"World tick error: {e}")
                time.sleep(1)

    def get_state(self) -> Dict[str, Any]:
        """Get complete system state.

        Returns:
            System state including world and all consciousnesses
        """
        world_state = self.world.get_state()

        # Add consciousness details
        for cid, autonomous in self.consciousnesses.items():
            if cid in world_state['consciousnesses']:
                world_state['consciousnesses'][cid].update({
                    'cycle_count': autonomous.cycle_count,
                    'thread_alive': autonomous.is_alive()
                })

        return world_state


def main():
    """Test multi-consciousness system."""
    print("=== Multi-Consciousness System ===\n")

    # Create harness
    script_dir = Path(__file__).parent.parent / "production"
    bend_binary = script_dir.parent.parent / "lmn-bend" / "bend"
    if not bend_binary.exists():
        bend_binary = "bend"

    harness = ProductionHarness(
        bend_binary=str(bend_binary),
        enable_real_llm=False
    )

    # Create system
    system = MultiConsciousnessSystem(harness)

    # Spawn consciousness instances
    system.spawn_consciousness("Alpha", "curious")
    system.spawn_consciousness("Beta", "cautious")
    system.spawn_consciousness("Gamma", "aggressive")

    print("Spawned 3 consciousnesses\n")

    # Start system
    system.start()

    print("System running for 30 seconds...\n")

    # Run for 30 seconds
    for i in range(6):
        time.sleep(5)
        state = system.get_state()
        print(f"Tick {state['tick_count']}:")
        print(f"  Consciousnesses: {len(state['consciousnesses'])}")
        print(f"  Resources: {len(state['resources'])}")
        print(f"  Challenges: {len(state['challenges'])}")

        for cid, cstate in state['consciousnesses'].items():
            print(f"  - {cstate['name']}: {cstate['state']}, {cstate['oracle_count']} thoughts")
        print()

    # Stop
    system.stop()
    print("System stopped")


if __name__ == "__main__":
    main()
