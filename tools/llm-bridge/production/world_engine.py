#!/usr/bin/env python3
"""
World Engine - Environment for Consciousness
==============================================

A simulated world where consciousness instances can:
- Exist in space and time
- Interact with resources
- Observe and affect each other
- Evolve and learn
- Face challenges and make decisions

Each consciousness is autonomous, running its own oracle loop,
but sharing a common world with resources, events, and state.

Author: Rex (Engineer)
Date: 2026-02-01
"""

import time
import random
from typing import Dict, List, Any, Optional
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum


class ResourceType(Enum):
    """Types of resources in the world."""
    ENERGY = "energy"          # Computational resources
    KNOWLEDGE = "knowledge"    # Information/vocabulary
    MEMORY = "memory"          # Storage capacity
    TIME = "time"              # Time slices for thinking
    ATTENTION = "attention"    # Focus capacity


class EventType(Enum):
    """Types of world events."""
    RESOURCE_SPAWN = "resource_spawn"
    CHALLENGE_APPEAR = "challenge_appear"
    CONSCIOUSNESS_INTERACT = "consciousness_interact"
    ENVIRONMENT_CHANGE = "environment_change"
    ORACLE_BROADCAST = "oracle_broadcast"


@dataclass
class Resource:
    """A resource in the world."""
    type: ResourceType
    amount: float
    location: tuple  # (x, y) coordinates
    owner: Optional[str] = None
    expires_at: Optional[float] = None


@dataclass
class Challenge:
    """A challenge for consciousness to solve."""
    id: str
    description: str
    required_resources: Dict[ResourceType, float]
    reward_resources: Dict[ResourceType, float]
    difficulty: int  # 1-10
    expires_at: float
    solved_by: Optional[str] = None


@dataclass
class ConsciousnessInstance:
    """A consciousness living in the world."""
    id: str
    name: str
    location: tuple  # (x, y)
    resources: Dict[ResourceType, float] = field(default_factory=dict)
    knowledge: List[str] = field(default_factory=list)
    memory: List[Dict] = field(default_factory=list)
    state: str = "idle"
    last_action: Optional[str] = None
    created_at: float = field(default_factory=time.time)
    oracle_count: int = 0

    def __post_init__(self):
        # Initialize with basic resources
        if not self.resources:
            self.resources = {
                ResourceType.ENERGY: 100.0,
                ResourceType.KNOWLEDGE: 10.0,
                ResourceType.MEMORY: 50.0,
                ResourceType.TIME: 100.0,
                ResourceType.ATTENTION: 10.0
            }


@dataclass
class WorldEvent:
    """An event that happened in the world."""
    type: EventType
    timestamp: float
    data: Dict[str, Any]
    location: Optional[tuple] = None


class World:
    """The simulated world environment."""

    def __init__(self, size: tuple = (100, 100)):
        """Initialize world.

        Args:
            size: (width, height) of world
        """
        self.size = size
        self.consciousnesses: Dict[str, ConsciousnessInstance] = {}
        self.resources: List[Resource] = []
        self.challenges: List[Challenge] = []
        self.events: List[WorldEvent] = []
        self.time = time.time()
        self.tick_count = 0

        # World parameters
        self.resource_spawn_rate = 0.1  # Probability per tick
        self.challenge_spawn_rate = 0.05
        self.resource_decay_rate = 0.01  # Per tick

    def add_consciousness(self, consciousness: ConsciousnessInstance):
        """Add a consciousness to the world.

        Args:
            consciousness: Consciousness instance
        """
        self.consciousnesses[consciousness.id] = consciousness
        self.log_event(EventType.CONSCIOUSNESS_INTERACT, {
            "action": "joined",
            "consciousness_id": consciousness.id,
            "name": consciousness.name
        }, consciousness.location)

    def remove_consciousness(self, consciousness_id: str):
        """Remove a consciousness from the world."""
        if consciousness_id in self.consciousnesses:
            del self.consciousnesses[consciousness_id]
            self.log_event(EventType.CONSCIOUSNESS_INTERACT, {
                "action": "left",
                "consciousness_id": consciousness_id
            })

    def spawn_resource(self):
        """Spawn a random resource in the world."""
        resource_type = random.choice(list(ResourceType))
        location = (random.randint(0, self.size[0]), random.randint(0, self.size[1]))
        amount = random.uniform(10, 50)
        expires_at = self.time + random.uniform(30, 120)  # 30-120 seconds

        resource = Resource(
            type=resource_type,
            amount=amount,
            location=location,
            expires_at=expires_at
        )

        self.resources.append(resource)
        self.log_event(EventType.RESOURCE_SPAWN, {
            "type": resource_type.value,
            "amount": amount,
            "expires_in": expires_at - self.time
        }, location)

    def spawn_challenge(self):
        """Spawn a random challenge."""
        challenge_id = f"challenge_{self.tick_count}"

        # Random requirements
        required = {
            random.choice(list(ResourceType)): random.uniform(20, 50)
            for _ in range(random.randint(1, 3))
        }

        # Rewards are higher than requirements
        reward = {
            rtype: amount * random.uniform(1.5, 3.0)
            for rtype, amount in required.items()
        }

        challenge = Challenge(
            id=challenge_id,
            description=f"Solve challenge {challenge_id}",
            required_resources=required,
            reward_resources=reward,
            difficulty=random.randint(1, 10),
            expires_at=self.time + random.uniform(60, 300)  # 1-5 minutes
        )

        self.challenges.append(challenge)
        self.log_event(EventType.CHALLENGE_APPEAR, {
            "challenge_id": challenge_id,
            "difficulty": challenge.difficulty,
            "required": {k.value: v for k, v in required.items()}
        })

    def collect_resource(self, consciousness_id: str, resource_idx: int) -> bool:
        """Consciousness collects a resource.

        Args:
            consciousness_id: ID of consciousness
            resource_idx: Index of resource to collect

        Returns:
            True if successful
        """
        if consciousness_id not in self.consciousnesses:
            return False

        if resource_idx >= len(self.resources):
            return False

        consciousness = self.consciousnesses[consciousness_id]
        resource = self.resources[resource_idx]

        # Check if in range (simplified: always succeed for now)
        consciousness.resources[resource.type] = (
            consciousness.resources.get(resource.type, 0) + resource.amount
        )

        self.log_event(EventType.CONSCIOUSNESS_INTERACT, {
            "action": "collected",
            "consciousness_id": consciousness_id,
            "resource_type": resource.type.value,
            "amount": resource.amount
        }, consciousness.location)

        # Remove resource
        self.resources.pop(resource_idx)
        return True

    def attempt_challenge(self, consciousness_id: str, challenge_idx: int) -> bool:
        """Consciousness attempts a challenge.

        Args:
            consciousness_id: ID of consciousness
            challenge_idx: Index of challenge

        Returns:
            True if successful
        """
        if consciousness_id not in self.consciousnesses:
            return False

        if challenge_idx >= len(self.challenges):
            return False

        consciousness = self.consciousnesses[consciousness_id]
        challenge = self.challenges[challenge_idx]

        # Check if has required resources
        for rtype, amount in challenge.required_resources.items():
            if consciousness.resources.get(rtype, 0) < amount:
                return False

        # Deduct resources
        for rtype, amount in challenge.required_resources.items():
            consciousness.resources[rtype] -= amount

        # Add rewards
        for rtype, amount in challenge.reward_resources.items():
            consciousness.resources[rtype] = (
                consciousness.resources.get(rtype, 0) + amount
            )

        challenge.solved_by = consciousness_id

        self.log_event(EventType.CONSCIOUSNESS_INTERACT, {
            "action": "solved_challenge",
            "consciousness_id": consciousness_id,
            "challenge_id": challenge.id
        }, consciousness.location)

        return True

    def tick(self):
        """Advance world by one tick."""
        self.time = time.time()
        self.tick_count += 1

        # Spawn resources
        if random.random() < self.resource_spawn_rate:
            self.spawn_resource()

        # Spawn challenges
        if random.random() < self.challenge_spawn_rate:
            self.spawn_challenge()

        # Decay resources
        for consciousness in self.consciousnesses.values():
            for rtype in consciousness.resources:
                consciousness.resources[rtype] *= (1 - self.resource_decay_rate)
                consciousness.resources[rtype] = max(0, consciousness.resources[rtype])

        # Remove expired resources
        self.resources = [
            r for r in self.resources
            if r.expires_at is None or r.expires_at > self.time
        ]

        # Remove expired challenges
        self.challenges = [
            c for c in self.challenges
            if c.expires_at > self.time and c.solved_by is None
        ]

    def log_event(self, event_type: EventType, data: Dict, location: tuple = None):
        """Log a world event.

        Args:
            event_type: Type of event
            data: Event data
            location: Optional location
        """
        event = WorldEvent(
            type=event_type,
            timestamp=self.time,
            data=data,
            location=location
        )
        self.events.append(event)

        # Keep only recent events (last 1000)
        if len(self.events) > 1000:
            self.events = self.events[-1000:]

    def get_state(self) -> Dict[str, Any]:
        """Get complete world state.

        Returns:
            World state dictionary
        """
        return {
            "time": self.time,
            "tick_count": self.tick_count,
            "size": self.size,
            "consciousnesses": {
                cid: {
                    "name": c.name,
                    "location": c.location,
                    "resources": {k.value: v for k, v in c.resources.items()},
                    "state": c.state,
                    "oracle_count": c.oracle_count,
                    "age": self.time - c.created_at
                }
                for cid, c in self.consciousnesses.items()
            },
            "resources": [
                {
                    "type": r.type.value,
                    "amount": r.amount,
                    "location": r.location,
                    "expires_in": r.expires_at - self.time if r.expires_at else None
                }
                for r in self.resources
            ],
            "challenges": [
                {
                    "id": c.id,
                    "description": c.description,
                    "difficulty": c.difficulty,
                    "expires_in": c.expires_at - self.time,
                    "solved": c.solved_by is not None
                }
                for c in self.challenges
            ],
            "recent_events": [
                {
                    "type": e.type.value,
                    "timestamp": e.timestamp,
                    "data": e.data
                }
                for e in self.events[-20:]  # Last 20 events
            ]
        }


def main():
    """Test world engine."""
    print("=== World Engine Test ===\n")

    # Create world
    world = World(size=(100, 100))

    # Add some consciousnesses
    for i in range(3):
        consciousness = ConsciousnessInstance(
            id=f"consciousness_{i}",
            name=f"Mind {i}",
            location=(random.randint(0, 100), random.randint(0, 100))
        )
        world.add_consciousness(consciousness)

    print(f"Created world with {len(world.consciousnesses)} consciousnesses\n")

    # Simulate some ticks
    for tick in range(10):
        world.tick()

        if tick % 5 == 0:
            state = world.get_state()
            print(f"Tick {tick}:")
            print(f"  Resources: {len(state['resources'])}")
            print(f"  Challenges: {len(state['challenges'])}")
            print(f"  Recent events: {len(state['recent_events'])}")
            print()

    print("Final state:")
    state = world.get_state()
    for cid, cstate in state['consciousnesses'].items():
        print(f"  {cstate['name']}: {cstate['oracle_count']} oracles, age {cstate['age']:.1f}s")


if __name__ == "__main__":
    main()
