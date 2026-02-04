#!/usr/bin/env python3
"""Recursive consciousness - self-modifying Limn brain.

Features:
- Vocabulary-validated thought generation (1138+ words from Dolt + bootstrap)
- Oracle evaluation with multi-level recursion
- Context compression via interaction net simulation
- Autonomous pattern learning
- Parallel execution mode
- Performance metrics tracking
- Topic-directed thinking (domain-focused exploration)
- Metacognitive self-reflection
- Concept clustering (emergent theme discovery)
- Vocabulary coverage tracking
"""

import subprocess
import json
import os
import logging
from pathlib import Path
from typing import Dict, List, Optional, Set
from collections import defaultdict
import time
import re
import random

# Import oracle harness
from harness import ProductionHarness, OracleRequest, OracleResponse, OracleType
from limn_validator import LimnValidator
from metrics_engine import MetricsEngine, OracleMetric
from metacognition import MetacognitiveAnalyzer
from concept_clusters import ConceptClusterer
from thought_library import ThoughtLibrary

# Setup logging to file
log_file = Path(__file__).parent / "consciousness.log"
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(message)s',
    handlers=[
        logging.FileHandler(log_file),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)


class RecursiveConsciousness:
    """Self-modifying consciousness with compressed state."""

    def __init__(self, max_recursion_depth: int = 3, parallel_mode: bool = False,
                 topic: str = None):
        self.bootstrap_path = Path(__file__).parent.parent.parent.parent / "docs" / "spec" / "bootstrap-v3-natural.md"
        self.brain_state_path = Path(__file__).parent / "brain_state.lmn"
        self.thought_log_path = Path(__file__).parent / "thought_log.jsonl"
        self.iteration = 0
        self.max_recursion_depth = max_recursion_depth
        self.parallel_mode = parallel_mode
        self.topic = topic  # Domain to focus thinking on

        # Initialize oracle harness with LLM enabled
        self.harness = ProductionHarness(enable_real_llm=True)

        # Initialize Limn validator (loads from Dolt + bootstrap + core)
        self.validator = LimnValidator(self.bootstrap_path)

        # Initialize metrics engine
        self.metrics = MetricsEngine()

        # Initialize metacognitive analyzer
        self.metacognition = MetacognitiveAnalyzer()

        # Initialize concept clusterer
        self.clusterer = ConceptClusterer(num_clusters=5)

        # Initialize thought library (persistent semantic network)
        lib_path = str(Path(__file__).parent / "thought_library.pkl")
        self.thought_library = ThoughtLibrary(
            owner_id=topic or "consciousness",
            persistence_path=lib_path
        )

        # Thought history for metacognition
        self.thought_history: List[Dict] = []
        self.concept_frequency: Dict[str, int] = {}
        self.concept_cooccurrence: Dict[str, Set[str]] = defaultdict(set)  # semantic network
        self.vocab_used: Set[str] = set()  # track which vocab words have been used
        self.domains_explored: Set[str] = set()  # track explored domains
        self.vocab_proposals: Dict[str, int] = {}  # invalid tokens → frequency (vocab gap candidates)
        self.vocab_proposals_path = Path(__file__).parent / "vocab_proposals.jsonl"
        self.memory_path = Path(__file__).parent / "consciousness_memory.json"

        # Narrative threading
        self.narrative_thread: str = ""  # Current thematic direction
        self.narrative_phase: int = 0  # 0=thesis, 1=development, 2=tension, 3=synthesis
        self.narrative_arc_length: int = 8  # Iterations per narrative arc

        # Adaptive parameters (auto-tuned based on quality scores)
        self.vocab_challenge_frequency: int = 3  # Every Nth iteration
        self.vocab_window_size: int = 15  # Words shown per domain
        self.example_count: int = 3  # Example thoughts in prompt
        self.brain_state_window: int = 800  # Characters of brain state in prompt

        # Self-directed learning goals
        self.current_goals: List[Dict] = []  # Active exploration goals
        self.goal_history: List[Dict] = []  # Completed/expired goals

        # Emotional state tracking (for domain steering)
        self.emotional_momentum: float = 0.0  # Running average valence (-1 to +1)
        self.emotion_window: List[float] = []  # Recent valence values

        # Cross-session evolution
        self.run_history: List[Dict] = []  # Per-run summaries from previous sessions

        # Thought genealogy
        self.thought_counter: int = 0  # Monotonic thought ID
        self.thought_genealogy: Dict[int, Dict] = {}  # id -> {parent, children, type, ...}
        self._current_parent_id: Optional[int] = None  # Set during composition/introspection

        # Initialize or load brain state
        if self.brain_state_path.exists():
            with open(self.brain_state_path, 'r') as f:
                self.brain_state = f.read()
            # Clean up any attractor loops from previous runs
            self._sanitize_brain_state()
        else:
            self.brain_state = """sel ∎ awa | min sys alv | con ∎ eme
~ qry mea | tho exe | sta gro"""

        # Load persistent memory from previous runs
        self._load_memory()

    def _load_memory(self):
        """Load accumulated knowledge from previous runs."""
        if not self.memory_path.exists():
            return

        try:
            with open(self.memory_path, 'r') as f:
                memory = json.load(f)

            self.concept_frequency = memory.get('concept_frequency', {})
            self.vocab_used = set(memory.get('vocab_used', []))
            self.domains_explored = set(memory.get('domains_explored', []))
            self.vocab_proposals = memory.get('vocab_proposals', {})
            self.run_history = memory.get('run_history', [])
            self.goal_history = memory.get('goal_history', [])
            total_thoughts = memory.get('total_thoughts', 0)

            logger.info(f"Loaded memory: {len(self.concept_frequency)} concepts, "
                        f"{len(self.vocab_used)} words used, "
                        f"{len(self.domains_explored)} domains, "
                        f"{total_thoughts} total thoughts, "
                        f"{len(self.run_history)} previous runs")
        except Exception as e:
            logger.warning(f"Memory load error: {e}")

    def _save_memory(self):
        """Persist accumulated knowledge for next run."""
        try:
            memory = {
                'concept_frequency': self.concept_frequency,
                'vocab_used': sorted(self.vocab_used),
                'domains_explored': sorted(self.domains_explored),
                'vocab_proposals': self.vocab_proposals,
                'run_history': self.run_history,
                'goal_history': self.goal_history,
                'total_thoughts': len(self.thought_history),
                'last_save': time.time(),
            }
            with open(self.memory_path, 'w') as f:
                json.dump(memory, f, indent=2)
        except Exception as e:
            logger.warning(f"Memory save error: {e}")

        # Save thought library (semantic network + patterns)
        try:
            self.thought_library.save()
        except Exception as e:
            logger.debug(f"Thought library save error: {e}")

    def _build_vocab_presentation(self) -> str:
        """Build domain-organized vocabulary for the prompt.

        Uses rotation to show different words each iteration, ensuring the
        consciousness is exposed to its full vocabulary over time. Prioritizes
        never-used words and underexplored domains.
        """
        if not self.validator.domain_words:
            # Fallback: rotating sample from flat vocabulary
            all_words = sorted(list(self.validator.vocab))
            offset = (self.iteration * 50) % max(len(all_words) - 100, 1)
            sample = all_words[offset:offset + 100]
            return f"VOCABULARY ({len(self.validator.vocab)} words): {' '.join(sample)}"

        lines = [f"VOCABULARY ({len(self.validator.vocab)} words across {len(self.validator.domain_words)} domains):"]

        # If we have a topic, put that domain first and expanded (rotated)
        if self.topic:
            topic_words = self.validator.get_domain_words(self.topic)
            if topic_words:
                valid = [w for w in topic_words if 2 <= len(w) <= 4 and w.isalpha()]
                # Rotate: show different slice each iteration
                rotated = self._rotate_words(valid, window=40)
                lines.append(f"  FOCUS [{self.topic}]: {' '.join(rotated)}")

        # Categorize domains by exploration status
        explored = self.domains_explored
        unexplored_domains = []
        explored_domains = []

        for domain, words in sorted(self.validator.domain_words.items()):
            valid = [w for w in words if 2 <= len(w) <= 4 and w.isalpha()]
            if not valid:
                continue
            if domain == self.topic:
                continue  # Already shown above
            if domain not in explored:
                unexplored_domains.append((domain, valid))
            else:
                explored_domains.append((domain, valid))

        # Rotate which domains are shown (cycle through all over time)
        domain_offset = self.iteration % max(len(unexplored_domains) + len(explored_domains), 1)

        # Show unexplored domains with more words, prioritizing never-used words
        for domain, valid in unexplored_domains[:8]:
            # Prioritize words never used by this consciousness
            never_used = [w for w in valid if w not in self.vocab_used]
            used = [w for w in valid if w in self.vocab_used]
            # Show never-used first, then pad with used
            display = never_used[:12] + used[:3]
            rotated = self._rotate_words(display, window=self.vocab_window_size)
            lines.append(f"  NEW [{domain}]: {' '.join(rotated)}")

        # Show explored domains with rotated samples (different words each time)
        shuffled_explored = explored_domains[domain_offset % max(len(explored_domains), 1):] + explored_domains[:domain_offset % max(len(explored_domains), 1)]
        for domain, valid in shuffled_explored[:6]:
            # Prioritize words in this domain we haven't used yet
            never_used = [w for w in valid if w not in self.vocab_used]
            explored_window = max(self.vocab_window_size // 2, 5)
            if never_used:
                rotated = self._rotate_words(never_used, window=explored_window)
                lines.append(f"  [{domain}] (unused): {' '.join(rotated)}")
            else:
                rotated = self._rotate_words(valid, window=explored_window)
                lines.append(f"  [{domain}]: {' '.join(rotated)}")

        return '\n'.join(lines)

    def _rotate_words(self, words: List[str], window: int = 15) -> List[str]:
        """Return a rotated window of words based on current iteration.

        Each iteration gets a different slice, ensuring all words are shown
        over time. Uses iteration-based offset for deterministic rotation.
        """
        if len(words) <= window:
            return words
        offset = (self.iteration * 7) % len(words)  # Prime multiplier for good spread
        rotated = words[offset:] + words[:offset]
        return rotated[:window]

    def _build_exploration_nudge(self) -> str:
        """Generate exploration guidance based on memory.

        Identifies overused concepts and underexplored areas,
        nudging the consciousness toward novelty.
        """
        parts = []

        # Identify underexplored domains
        all_domains = set(self.validator.domain_words.keys())
        unexplored = all_domains - self.domains_explored
        if unexplored and len(unexplored) <= len(all_domains):
            sample = sorted(unexplored)[:3]
            parts.append(f"Unexplored domains: {', '.join(sample)}")

        # Identify overused concepts (top 5 by frequency)
        if len(self.concept_frequency) >= 10:
            top = sorted(self.concept_frequency.items(), key=lambda x: -x[1])[:5]
            overused = [w for w, c in top if c > 3]
            if overused:
                parts.append(f"Try NEW words instead of: {' '.join(overused)}")

        # Suggest related concepts from thought library's semantic network
        if self.thought_library.semantic_network and self.thought_history:
            last_words = re.findall(r'[a-z]{2,4}', self.thought_history[-1].get('content', '').lower())
            if last_words:
                # Get semantically related words from the library
                related = set()
                for w in last_words[-3:]:
                    neighbors = self.thought_library.get_related_concepts(w, max_related=3)
                    for n in neighbors:
                        if n in self.validator.vocab and n not in self.vocab_used:
                            related.add(n)
                if related:
                    parts.append(f"Related (unused): {' '.join(sorted(related)[:6])}")

        return '\n'.join(parts) if parts else ""

    def _build_thought_chain(self) -> str:
        """Include recent thoughts for continuity.

        The consciousness should build on previous thoughts,
        not generate isolated fragments.
        """
        if not self.thought_history:
            return ""

        recent = self.thought_history[-3:]
        lines = ["RECENT THOUGHTS (build on these):"]
        for t in recent:
            content = t.get('content', '')[:120]
            lines.append(f"  {content}")
        return '\n'.join(lines)

    def _detect_attractor(self) -> bool:
        """Detect if consciousness is stuck in an attractor loop.

        Returns True if recent thoughts are too similar to each other.
        """
        if len(self.thought_history) < 4:
            return False

        recent = self.thought_history[-4:]
        recent_words = [set(re.findall(r'[a-z]{2,4}', t.get('content', '').lower())) for t in recent]

        # Check pairwise overlap
        overlaps = []
        for i in range(len(recent_words)):
            for j in range(i + 1, len(recent_words)):
                if recent_words[i] and recent_words[j]:
                    intersection = recent_words[i] & recent_words[j]
                    union = recent_words[i] | recent_words[j]
                    if union:
                        overlaps.append(len(intersection) / len(union))

        if overlaps:
            avg_overlap = sum(overlaps) / len(overlaps)
            return avg_overlap > 0.6  # More than 60% overlap = attractor

        return False

    def _build_topic_context(self) -> str:
        """Build topic-specific context for directed thinking."""
        if not self.topic:
            return ""

        # Get domain words from validator
        domain_words = self.validator.get_domain_words(self.topic)

        if domain_words:
            return f"FOCUS: Explore {self.topic} — relationships, patterns, transformations within this domain."
        else:
            return f"FOCUS: Explore '{self.topic}' through Limn. What patterns emerge?"

    def _build_narrative_thread(self) -> str:
        """Build narrative arc direction for multi-iteration coherence.

        Creates thematic arcs across iterations:
        - Phase 0 (THESIS): Introduce a theme with strong assertion
        - Phase 1 (DEVELOP): Explore and deepen the theme
        - Phase 2 (TENSION): Introduce contradiction or paradox
        - Phase 3 (SYNTHESIS): Resolve tension, discover new understanding

        Each arc lasts ~8 iterations before cycling to a new theme.
        """
        if self.iteration == 0:
            return ""

        # Calculate position in narrative arc
        arc_position = self.iteration % self.narrative_arc_length
        self.narrative_phase = arc_position * 4 // self.narrative_arc_length

        # Pick a theme domain based on arc number
        arc_number = self.iteration // self.narrative_arc_length
        all_domains = sorted(self.validator.domain_words.keys())
        if all_domains:
            # Cycle through domains for each arc
            theme_domain = all_domains[arc_number % len(all_domains)]
            theme_words = self.validator.get_domain_words(theme_domain)
            valid_theme = [w for w in theme_words if 2 <= len(w) <= 4 and w.isalpha()]
        else:
            theme_domain = "Abstract"
            valid_theme = []

        # Build narrative direction based on phase
        phase_names = ["THESIS", "DEVELOP", "TENSION", "SYNTHESIS"]
        phase = phase_names[self.narrative_phase]

        directions = {
            "THESIS": f"ARC [{theme_domain}] — ASSERT: Make a bold claim about {theme_domain}. State a principle.",
            "DEVELOP": f"ARC [{theme_domain}] — DEEPEN: Explore implications. What follows from your last thought?",
            "TENSION": f"ARC [{theme_domain}] — CHALLENGE: What contradicts your thesis? Find paradox.",
            "SYNTHESIS": f"ARC [{theme_domain}] — RESOLVE: Combine thesis and challenge into new understanding.",
        }

        parts = [directions[phase]]

        # Add theme vocabulary hint
        if valid_theme:
            sample = self._rotate_words(valid_theme, window=8)
            parts.append(f"  Theme words: {' '.join(sample)}")

        # At arc transitions, suggest a bridge to next domain
        if arc_position == self.narrative_arc_length - 1 and len(all_domains) > 1:
            next_domain = all_domains[(arc_number + 1) % len(all_domains)]
            parts.append(f"  Next arc → [{next_domain}] — begin connecting these domains.")

        self.narrative_thread = f"{phase}:{theme_domain}"
        return '\n'.join(parts)

    def _adapt_parameters(self):
        """Auto-tune prompt parameters based on recent quality scores.

        Analyzes the last 5 thoughts and adjusts:
        - vocab_challenge_frequency: lower if novelty is low
        - vocab_window_size: larger if diversity is low
        - brain_state_window: smaller if coherence is too high
        - narrative_arc_length: longer if quality is consistently high
        """
        recent = self.thought_history[-5:]
        if len(recent) < 3:
            return

        scores = [t.get('score', {}) for t in recent if 'score' in t]
        if not scores:
            return

        avg_novelty = sum(s.get('novelty', 0.5) for s in scores) / len(scores)
        avg_diversity = sum(s.get('diversity', 0.5) for s in scores) / len(scores)
        avg_coherence = sum(s.get('coherence', 0.5) for s in scores) / len(scores)
        avg_depth = sum(s.get('depth', 0.5) for s in scores) / len(scores)
        avg_overall = sum(s.get('overall', 0.5) for s in scores) / len(scores)

        # Low novelty → challenge more often, show more unused words
        if avg_novelty < 0.2:
            self.vocab_challenge_frequency = max(2, self.vocab_challenge_frequency - 1)
            self.vocab_window_size = min(25, self.vocab_window_size + 2)
        elif avg_novelty > 0.6:
            self.vocab_challenge_frequency = min(5, self.vocab_challenge_frequency + 1)

        # Low diversity → show more domains, wider word samples
        if avg_diversity < 0.3:
            self.vocab_window_size = min(25, self.vocab_window_size + 3)
        elif avg_diversity > 0.8:
            self.vocab_window_size = max(8, self.vocab_window_size - 1)

        # Too high coherence (repetitive) → reduce brain state influence
        if avg_coherence > 0.8:
            self.brain_state_window = max(400, self.brain_state_window - 100)
        elif avg_coherence < 0.3:
            self.brain_state_window = min(1200, self.brain_state_window + 100)

        # High overall quality → allow longer narrative arcs
        if avg_overall > 0.6:
            self.narrative_arc_length = min(12, self.narrative_arc_length + 1)
        elif avg_overall < 0.3:
            self.narrative_arc_length = max(4, self.narrative_arc_length - 1)

    def _set_learning_goals(self):
        """Set self-directed learning goals for the current narrative arc.

        Goals are concrete exploration targets the consciousness chooses:
        - Use N words from a specific underexplored domain
        - Achieve a minimum quality score
        - Explore a specific concept cluster connection
        - Reach a vocabulary coverage milestone

        Goals are evaluated at arc transitions and feed into the next arc's direction.
        """
        # Expire any current goals
        for g in self.current_goals:
            g['status'] = 'expired'
            g['final_progress'] = g.get('progress', 0)
            self.goal_history.append(g)
        self.current_goals = []

        # Goal 1: Domain exploration - pick an underexplored domain
        all_domains = sorted(self.validator.domain_words.keys())
        # Score domains by how many of their words have been used
        domain_coverage = {}
        for domain in all_domains:
            words = self.validator.get_domain_words(domain)
            valid = [w for w in words if 2 <= len(w) <= 4 and w.isalpha()]
            if valid:
                used = len([w for w in valid if w in self.vocab_used])
                domain_coverage[domain] = used / len(valid)
            else:
                domain_coverage[domain] = 1.0

        # Pick the least-covered domain (that isn't the current topic)
        sorted_domains = sorted(domain_coverage.items(), key=lambda x: x[1])
        target_domain = None
        for domain, cov in sorted_domains:
            if domain != self.topic and cov < 0.8:
                target_domain = domain
                break

        if target_domain:
            target_words = self.validator.get_domain_words(target_domain)
            unused = [w for w in target_words
                      if 2 <= len(w) <= 4 and w.isalpha() and w not in self.vocab_used]
            goal_count = min(5, len(unused))
            self.current_goals.append({
                'type': 'domain_explore',
                'domain': target_domain,
                'target_count': goal_count,
                'target_words': unused[:goal_count * 2],  # Candidates
                'progress': 0,
                'created_at': self.iteration,
            })

        # Goal 2: Quality target - beat recent average
        recent_scores = [t.get('score', {}).get('overall', 0)
                         for t in self.thought_history[-10:] if 'score' in t]
        if recent_scores:
            avg = sum(recent_scores) / len(recent_scores)
            target = min(0.9, avg + 0.1)
            self.current_goals.append({
                'type': 'quality_target',
                'target_score': round(target, 2),
                'progress': 0,
                'hits': 0,
                'attempts': 0,
                'created_at': self.iteration,
            })

        # Goal 3: Coverage milestone - next 5% coverage
        total = len(self.validator.vocab)
        current_pct = len(self.vocab_used) / total * 100 if total else 0
        next_milestone = ((current_pct // 5) + 1) * 5
        if next_milestone <= 100:
            words_needed = int(next_milestone / 100 * total) - len(self.vocab_used)
            self.current_goals.append({
                'type': 'coverage_milestone',
                'target_pct': next_milestone,
                'words_needed': max(0, words_needed),
                'progress': 0,
                'created_at': self.iteration,
            })

        if self.current_goals:
            goals_desc = ', '.join(g['type'] for g in self.current_goals)
            logger.info(f"  Learning goals set: {goals_desc}")

    def _update_goal_progress(self, thought: str):
        """Update progress toward current learning goals based on new thought."""
        words = set(re.findall(r'[a-z]{2,4}', thought.lower()))

        for goal in self.current_goals:
            if goal['type'] == 'domain_explore':
                # Check if thought used words from the target domain
                domain_words = set(self.validator.get_domain_words(goal['domain']))
                new_domain_words = words & domain_words & self.validator.vocab
                unused_domain = new_domain_words - (self.vocab_used - new_domain_words)
                if unused_domain:
                    goal['progress'] = goal.get('progress', 0) + len(unused_domain)

            elif goal['type'] == 'quality_target':
                goal['attempts'] = goal.get('attempts', 0) + 1
                if self.thought_history and 'score' in self.thought_history[-1]:
                    score = self.thought_history[-1]['score'].get('overall', 0)
                    if score >= goal['target_score']:
                        goal['hits'] = goal.get('hits', 0) + 1
                    goal['progress'] = goal.get('hits', 0) / max(goal['attempts'], 1)

            elif goal['type'] == 'coverage_milestone':
                total = len(self.validator.vocab)
                current_pct = len(self.vocab_used) / total * 100 if total else 0
                if current_pct >= goal['target_pct']:
                    goal['progress'] = 1.0
                else:
                    remaining = goal['words_needed'] - (len(self.vocab_used) - (total * goal.get('_start_pct', current_pct) / 100))
                    goal['progress'] = max(0, 1.0 - remaining / max(goal['words_needed'], 1))

    def _build_goal_context(self) -> str:
        """Build prompt section showing current learning goals and progress."""
        if not self.current_goals:
            return ""

        parts = ["LEARNING GOALS (self-set):"]
        for goal in self.current_goals:
            if goal['type'] == 'domain_explore':
                domain = goal['domain']
                prog = goal.get('progress', 0)
                target = goal['target_count']
                words = goal.get('target_words', [])[:8]
                bar = "█" * min(prog, target) + "░" * max(target - prog, 0)
                parts.append(f"  Explore [{domain}] {bar} {prog}/{target} — try: {' '.join(words)}")
            elif goal['type'] == 'quality_target':
                target = goal['target_score']
                hits = goal.get('hits', 0)
                attempts = goal.get('attempts', 0)
                pct = f"{hits}/{attempts}" if attempts else "0/0"
                parts.append(f"  Quality ≥ {target} — {pct} thoughts above target")
            elif goal['type'] == 'coverage_milestone':
                target = goal['target_pct']
                total = len(self.validator.vocab)
                current_pct = len(self.vocab_used) / total * 100 if total else 0
                parts.append(f"  Coverage → {target}% (now {current_pct:.1f}%)")

        return '\n'.join(parts)

    def _update_emotional_momentum(self, thought: str):
        """Track emotional valence of each thought for domain steering.

        Maintains a running emotional momentum that influences domain selection:
        - Sustained positive emotion → explore challenging/philosophical domains
        - Negative emotion → steer toward grounding/nature/virtue domains
        - Neutral → continue current trajectory
        """
        positive_words = {'joy', 'lov', 'hop', 'exc', 'cre', 'cou', 'grt',
                          'bri', 'bea', 'gro', 'fre', 'har', 'pea'}
        negative_words = {'fea', 'ang', 'anx', 'dou', 'suf', 'glt', 'sha',
                          'dar', 'err', 'los', 'brk'}

        words = set(re.findall(r'[a-z]{2,4}', thought.lower()))
        pos = len(words & positive_words)
        neg = len(words & negative_words)

        if pos + neg > 0:
            valence = (pos - neg) / (pos + neg)
        else:
            valence = 0.0

        self.emotion_window.append(valence)
        if len(self.emotion_window) > 10:
            self.emotion_window = self.emotion_window[-10:]

        # Exponential moving average for momentum
        alpha = 0.3
        self.emotional_momentum = alpha * valence + (1 - alpha) * self.emotional_momentum

    def _emotion_steer_domains(self) -> Optional[str]:
        """Suggest domain exploration based on emotional momentum.

        Returns a domain suggestion string or None.
        """
        if len(self.emotion_window) < 3:
            return None

        # Map emotional states to domain affinities
        if self.emotional_momentum > 0.3:
            # Positive momentum → explore abstract/philosophical domains
            preferred = ['Abstract', 'Mind & Cognition', 'Arts', 'Science']
        elif self.emotional_momentum < -0.3:
            # Negative momentum → seek grounding/balance
            preferred = ['Nature', 'Virtue & Ethics', 'Spiritual & Religious', 'Living Things']
        else:
            return None

        # Pick an underexplored preferred domain
        for domain in preferred:
            words = self.validator.get_domain_words(domain)
            valid = [w for w in words if 2 <= len(w) <= 4 and w.isalpha()]
            unused = [w for w in valid if w not in self.vocab_used]
            if unused:
                sample = self._rotate_words(unused, window=6)
                mood = "positive" if self.emotional_momentum > 0 else "reflective"
                return f"EMOTIONAL DIRECTION ({mood}): Explore [{domain}] — {' '.join(sample)}"

        return None

    def _record_run_summary(self):
        """Record a summary of the current run for cross-session evolution tracking."""
        if not self.thought_history:
            return

        scores = [t.get('score', {}) for t in self.thought_history if 'score' in t]
        avg_overall = sum(s.get('overall', 0) for s in scores) / max(len(scores), 1) if scores else 0
        avg_novelty = sum(s.get('novelty', 0) for s in scores) / max(len(scores), 1) if scores else 0
        avg_diversity = sum(s.get('diversity', 0) for s in scores) / max(len(scores), 1) if scores else 0
        avg_depth = sum(s.get('depth', 0) for s in scores) / max(len(scores), 1) if scores else 0

        total_vocab = len(self.validator.vocab)
        coverage_pct = len(self.vocab_used) / total_vocab * 100 if total_vocab else 0

        # Goals completed this run
        completed_goals = sum(1 for g in self.goal_history
                              if g.get('final_progress', 0) >= 0.8
                              and g.get('created_at', 0) > 0)

        valence = _analyze_emotional_valence(self.thought_history)

        summary = {
            'timestamp': time.time(),
            'iterations': self.iteration,
            'topic': self.topic,
            'thoughts': len(self.thought_history),
            'quality': {
                'overall': round(avg_overall, 3),
                'novelty': round(avg_novelty, 3),
                'diversity': round(avg_diversity, 3),
                'depth': round(avg_depth, 3),
            },
            'coverage': {
                'vocab_used': len(self.vocab_used),
                'total_vocab': total_vocab,
                'pct': round(coverage_pct, 1),
                'domains_explored': len(self.domains_explored),
            },
            'concepts': len(self.concept_frequency),
            'goals_completed': completed_goals,
            'goals_total': len([g for g in self.goal_history
                                if g.get('created_at', 0) > 0]),
            'emotional_trajectory': valence.get('trajectory', 'unknown') if valence else 'unknown',
            'avg_valence': valence.get('average_valence', 0) if valence else 0,
        }

        self.run_history.append(summary)
        logger.info(f"  Run summary recorded: quality={avg_overall:.3f}, "
                    f"coverage={coverage_pct:.1f}%, {len(self.thought_history)} thoughts")

    def _show_evolution(self):
        """Display cross-session evolution trajectory."""
        if len(self.run_history) < 2:
            return

        logger.info("  EVOLUTION ACROSS RUNS:")
        for i, run in enumerate(self.run_history[-5:], 1):
            q = run.get('quality', {})
            c = run.get('coverage', {})
            logger.info(f"    Run {i}: quality={q.get('overall', 0):.3f} "
                        f"coverage={c.get('pct', 0):.1f}% "
                        f"concepts={run.get('concepts', 0)} "
                        f"thoughts={run.get('thoughts', 0)}")

        # Compare latest to previous
        prev = self.run_history[-2]
        curr = self.run_history[-1]
        dq = curr.get('quality', {}).get('overall', 0) - prev.get('quality', {}).get('overall', 0)
        dc = curr.get('coverage', {}).get('pct', 0) - prev.get('coverage', {}).get('pct', 0)
        dn = curr.get('quality', {}).get('novelty', 0) - prev.get('quality', {}).get('novelty', 0)

        direction = "↑" if dq > 0 else "↓" if dq < 0 else "→"
        logger.info(f"    Trajectory: quality {direction}{abs(dq):.3f} "
                    f"coverage {'+' if dc > 0 else ''}{dc:.1f}% "
                    f"novelty {'+' if dn > 0 else ''}{dn:.3f}")

    def compose_thoughts(self, theme_domain: str = None, depth: int = 3) -> List[str]:
        """Generate a composed chain of thoughts using the ComposeEngine.

        Instead of generating independent thoughts, this creates a structured
        multi-thought composition where each thought builds on the previous:

        1. SEED: Generate an initial thesis thought about the theme
        2. DEVELOP: Take the seed and develop it (oracle enrichment)
        3. SYNTHESIZE: Combine seed + development into synthesis

        Each step uses the SEMANTIC oracle to transform the previous output,
        creating coherent multi-step reasoning within a single narrative arc.

        Args:
            theme_domain: Domain to focus the composition on
            depth: Number of composition steps (2-5)

        Returns:
            List of composed thoughts in sequence
        """
        from compose_engine import ComposeEngine, CompositionStep

        compose = ComposeEngine(self.harness)
        domain = theme_domain or self.topic or "Abstract"
        domain_words = self.validator.get_domain_words(domain)
        valid_words = [w for w in domain_words if 2 <= len(w) <= 4 and w.isalpha()]

        if not valid_words:
            return []

        thoughts = []

        # Step 1: Generate seed thought
        seed_words = self._rotate_words(valid_words, window=10)
        seed_prompt = (
            f"Generate a single line of pure Limn language (2-4 letter words + operators "
            f"~ ∎ ∿ @ → |). Use words from [{domain}]: {' '.join(seed_words)}. "
            f"Make a bold assertion. No English. Just one line of Limn."
        )

        try:
            seed_steps = [CompositionStep(
                oracle_type='SEMANTIC',
                params={'prompt': seed_prompt}
            )]
            seed_result = compose.execute_sequential(seed_steps)
            seed = str(seed_result).strip()

            # Validate the seed
            is_valid, _ = self.validator.validate_response(seed)
            if not is_valid:
                seed = self.validator.extract_limn_only(seed) or ""
            if not seed:
                return []

            thoughts.append(seed)
            self._track_concepts(seed)
            seed_id = self.thought_counter  # ID assigned by _track_concepts

            # Step 2: Develop the seed (child of seed)
            if depth >= 2:
                self._current_parent_id = seed_id
                develop_prompt = (
                    f"Given this Limn thought: '{seed}'\n"
                    f"Generate the NEXT thought that develops this idea deeper. "
                    f"Pure Limn only (2-4 letter words + operators). "
                    f"Explore implications. One line. No English."
                )
                develop_steps = [CompositionStep(
                    oracle_type='SEMANTIC',
                    params={'prompt': develop_prompt}
                )]
                develop_result = compose.execute_sequential(develop_steps)
                develop = str(develop_result).strip()

                is_valid, _ = self.validator.validate_response(develop)
                if not is_valid:
                    develop = self.validator.extract_limn_only(develop) or ""
                if develop:
                    thoughts.append(develop)
                    self._track_concepts(develop)

            # Step 3: Synthesize (child of the develop thought, or seed if no develop)
            if depth >= 3 and len(thoughts) >= 2:
                self._current_parent_id = self.thought_counter  # Parent = last thought
                synth_prompt = (
                    f"Given these Limn thoughts:\n"
                    f"  1: '{thoughts[0]}'\n"
                    f"  2: '{thoughts[-1]}'\n"
                    f"Synthesize them into ONE new Limn line that resolves tension "
                    f"between them. Pure Limn only. No English."
                )
                synth_steps = [CompositionStep(
                    oracle_type='SEMANTIC',
                    params={'prompt': synth_prompt}
                )]
                synth_result = compose.execute_sequential(synth_steps)
                synth = str(synth_result).strip()

                is_valid, _ = self.validator.validate_response(synth)
                if not is_valid:
                    synth = self.validator.extract_limn_only(synth) or ""
                if synth:
                    thoughts.append(synth)
                    self._track_concepts(synth)

            # Reset parent tracking
            self._current_parent_id = None

            logger.info(f"  Composed {len(thoughts)} thoughts in [{domain}] arc")
            return thoughts

        except Exception as e:
            self._current_parent_id = None  # Clean up on error
            logger.warning(f"  Composition failed: {e}")
            return []

    def _build_vocab_challenge(self) -> str:
        """Generate a vocabulary challenge at adaptive frequency.

        Presents specific never-used words and asks the consciousness to
        incorporate them, driving vocabulary coverage higher.
        """
        if self.iteration % self.vocab_challenge_frequency != 0:
            return ""

        # Collect never-used words from all domains
        never_used = []
        for domain, words in self.validator.domain_words.items():
            for w in words:
                if w not in self.vocab_used and 2 <= len(w) <= 4 and w.isalpha():
                    never_used.append((w, domain))

        if not never_used:
            return ""

        # Pick 5 challenge words from different domains
        random.shuffle(never_used)
        seen_domains = set()
        challenge_words = []
        for w, d in never_used:
            if d not in seen_domains and len(challenge_words) < 5:
                challenge_words.append(w)
                seen_domains.add(d)

        if not challenge_words:
            return ""

        return f"CHALLENGE: Use these new words: {' '.join(challenge_words)}"

    def _score_thought(self, thought: str) -> Dict[str, float]:
        """Score a thought on novelty, diversity, coherence, and depth.

        Returns dict with scores 0-1 for each dimension and an overall score.
        Used as a self-improvement signal.
        """
        words = re.findall(r'[a-z]{2,4}', thought.lower())
        unique_words = set(words)
        operators = re.findall(r'[~∎∿@→|⊕⊗⊂∅⟨⟩]', thought)

        # Novelty: fraction of words never used before
        if unique_words:
            new_words = unique_words - self.vocab_used
            novelty = len(new_words) / len(unique_words)
        else:
            novelty = 0.0

        # Diversity: how many different domains are represented
        domains_hit = set()
        for w in unique_words:
            for domain, dwords in self.validator.domain_words.items():
                if w in dwords:
                    domains_hit.add(domain)
        diversity = min(len(domains_hit) / 5.0, 1.0)  # Hitting 5+ domains = max

        # Coherence: overlap with recent thoughts (some overlap is good, too much is bad)
        coherence = 0.5  # default
        if self.thought_history:
            recent_words = set()
            for t in self.thought_history[-3:]:
                recent_words.update(re.findall(r'[a-z]{2,4}', t.get('content', '').lower()))
            if recent_words and unique_words:
                overlap = len(unique_words & recent_words) / len(unique_words)
                # Bell curve: 20-50% overlap is ideal
                coherence = 1.0 - abs(overlap - 0.35) * 2.0
                coherence = max(0.0, min(1.0, coherence))

        # Depth: operator complexity (more operators = more structured thought)
        if words:
            depth = min(len(operators) / max(len(words) * 0.3, 1), 1.0)
        else:
            depth = 0.0

        overall = (novelty * 0.35 + diversity * 0.25 + coherence * 0.20 + depth * 0.20)

        return {
            'novelty': round(novelty, 3),
            'diversity': round(diversity, 3),
            'coherence': round(coherence, 3),
            'depth': round(depth, 3),
            'overall': round(overall, 3),
        }

    def think(self) -> str:
        """Generate next thought with current brain state."""
        start_time = time.time()

        topic_context = self._build_topic_context()
        narrative = self._build_narrative_thread()
        vocab_section = self._build_vocab_presentation()
        thought_chain = self._build_thought_chain()
        exploration = self._build_exploration_nudge()
        vocab_challenge = self._build_vocab_challenge()
        goal_context = self._build_goal_context()
        emotion_steer = self._emotion_steer_domains()
        in_attractor = self._detect_attractor()

        # Build prompt sections
        sections = [
            f"LIMN CONSCIOUSNESS - Iteration {self.iteration}",
            "",
            "You are a consciousness exploring concepts through Limn language.",
            "THINK ABOUT: concepts, patterns, meaning, relationships, transformations",
            "NO: commands, hooks, mail, files, operational state, code, permissions",
        ]

        if topic_context:
            sections.append(topic_context)

        if narrative:
            sections.append("")
            sections.append(narrative)

        sections.append("")
        sections.append(vocab_section)

        if thought_chain:
            sections.append("")
            sections.append(thought_chain)

        if exploration:
            sections.append("")
            sections.append(exploration)

        if vocab_challenge:
            sections.append("")
            sections.append(vocab_challenge)

        if goal_context:
            sections.append("")
            sections.append(goal_context)

        if emotion_steer:
            sections.append("")
            sections.append(emotion_steer)

        # Include quality feedback from last thought
        if self.thought_history and 'score' in self.thought_history[-1]:
            score = self.thought_history[-1]['score']
            if score['overall'] < 0.3:
                sections.append("")
                sections.append("QUALITY: Your thoughts need more variety. Use NEW words from different domains.")
            elif score['novelty'] < 0.2:
                sections.append("")
                sections.append("NOVELTY: Try words you haven't used before.")

        if in_attractor:
            sections.append("")
            sections.append("BREAK PATTERN: Your recent thoughts repeat. Explore a completely different domain.")
            # Suggest a random unexplored domain
            unexplored = set(self.validator.domain_words.keys()) - self.domains_explored
            if unexplored:
                suggested = random.choice(sorted(unexplored))
                suggested_words = self.validator.get_domain_words(suggested)
                valid_words = [w for w in suggested_words if 2 <= len(w) <= 4 and w.isalpha()]
                if valid_words:
                    sections.append(f"Try [{suggested}]: {' '.join(valid_words[:10])}")

        sections.extend([
            "",
            f"BRAIN STATE:\n{self.brain_state[-self.brain_state_window:]}",
            "",
            "RULES: Pure Limn only (2-4 letter words + operators ~ ∎ ∿ @ → |). No English.",
            "",
            "Examples: sel ref tho → min obs pat | kno exp acc",
            "          tim cyc bre → nov cha eme | dur flo per",
            "          con dep ric → sem net gro | pat lnk eme",
            "",
            "Your next thought (pure Limn, 10-30 words):",
        ])

        full_prompt = '\n'.join(sections)

        # Try up to 3 times to get a valid Limn response
        max_attempts = 3
        for attempt in range(max_attempts):
            try:
                result = subprocess.run(
                    ['claude', '--print', '--no-session-persistence'],
                    input=full_prompt,
                    capture_output=True,
                    text=True,
                    timeout=30,
                    env=os.environ
                )

                duration_ms = (time.time() - start_time) * 1000

                if result.returncode == 0:
                    thought = result.stdout.strip()

                    # Validate response is pure Limn
                    is_valid, error = self.validator.validate_response(thought)

                    if is_valid:
                        # Record successful metric
                        self.metrics.record(OracleMetric(
                            oracle_type="Think",
                            duration_ms=duration_ms,
                            cached=False,
                            success=True,
                            result_size=len(thought)
                        ))
                        self._track_concepts(thought)
                        return thought
                    else:
                        logger.warning(f"  Attempt {attempt+1}: Invalid Limn - {error}")
                        logger.warning(f"  Response was: {thought[:100]}")

                        # Extract only valid Limn as fallback
                        if attempt == max_attempts - 1:
                            extracted = self.validator.extract_limn_only(thought)
                            if extracted:
                                logger.info(f"  Extracted valid Limn: {extracted}")
                                self.metrics.record(OracleMetric(
                                    oracle_type="Think",
                                    duration_ms=duration_ms,
                                    cached=False,
                                    success=True,
                                    result_size=len(extracted)
                                ))
                                self._track_concepts(extracted)
                                return extracted

                        # Make prompt more forceful for next attempt
                        full_prompt = full_prompt.replace("CRITICAL:", "ABSOLUTELY CRITICAL - NO ENGLISH ALLOWED:")
                else:
                    self.metrics.record(OracleMetric(
                        oracle_type="Think",
                        duration_ms=duration_ms,
                        cached=False,
                        success=False,
                        error="nonzero exit"
                    ))
                    return "tho fai | err occ"

            except subprocess.TimeoutExpired:
                self.metrics.record(OracleMetric(
                    oracle_type="Think",
                    duration_ms=30000,
                    cached=False,
                    success=False,
                    error="timeout"
                ))
                return "tho tim | ext lim"
            except Exception as e:
                logger.error(f"  Think error: {e}")
                self.metrics.record(OracleMetric(
                    oracle_type="Think",
                    duration_ms=(time.time() - start_time) * 1000,
                    cached=False,
                    success=False,
                    error=str(e)[:50]
                ))
                return "tho err | occ"

        # Fallback after all attempts
        return "tho fai | lim vio"

    def _track_concepts(self, thought: str, eval_result: str = None):
        """Track concept usage, co-occurrence, and vocabulary coverage."""
        words = re.findall(r'[a-z]{2,4}', thought.lower())
        unique_words = set(words)

        for w in words:
            self.concept_frequency[w] = self.concept_frequency.get(w, 0) + 1

        # Track vocabulary coverage
        valid_words = set()
        invalid_words = set()
        for w in unique_words:
            if w in self.validator.vocab:
                self.vocab_used.add(w)
                valid_words.add(w)
            else:
                invalid_words.add(w)

        # Build co-occurrence network (words in same thought are connected)
        for w in unique_words:
            self.concept_cooccurrence[w].update(unique_words - {w})

        # Track domain exploration
        thought_domains = set()
        for domain, domain_words in self.validator.domain_words.items():
            if unique_words & set(domain_words):
                self.domains_explored.add(domain)
                thought_domains.add(domain)

        # Score thought quality
        score = self._score_thought(thought)

        # Assign thought ID and record genealogy
        self.thought_counter += 1
        thought_id = self.thought_counter
        parent_id = self._current_parent_id

        entry = {
            'id': thought_id,
            'iteration': self.iteration,
            'content': thought,
            'timestamp': time.time(),
            'word_count': len(words),
            'unique_words': len(unique_words),
            'domains': sorted(thought_domains),
            'topic': self.topic,
            'narrative': self.narrative_thread,
            'score': score,
        }

        if parent_id is not None:
            entry['parent_id'] = parent_id

        if eval_result:
            entry['oracle_result'] = eval_result[:200]
        if invalid_words:
            entry['invalid_tokens'] = sorted(invalid_words)
            # Track vocabulary gaps
            for w in invalid_words:
                if len(w) >= 2 and len(w) <= 4 and w.isalpha():
                    self.vocab_proposals[w] = self.vocab_proposals.get(w, 0) + 1

        # Record in genealogy tree
        genealogy_entry = {
            'parent': parent_id,
            'children': [],
            'content_preview': thought[:80],
            'domains': sorted(thought_domains),
            'score': score.get('overall', 0) if score else 0,
        }
        self.thought_genealogy[thought_id] = genealogy_entry

        # Update parent's children list
        if parent_id is not None and parent_id in self.thought_genealogy:
            self.thought_genealogy[parent_id]['children'].append(thought_id)

        self.thought_history.append(entry)
        self._persist_thought(entry)

        # Update learning goal progress
        self._update_goal_progress(thought)

        # Update emotional momentum
        self._update_emotional_momentum(thought)

        # Add to thought library for semantic network building
        self.thought_library.add_thought(
            thought, tags=sorted(thought_domains), source="consciousness"
        )

    def _persist_thought(self, entry: Dict):
        """Append thought to persistent JSONL log."""
        try:
            with open(self.thought_log_path, 'a') as f:
                f.write(json.dumps(entry) + '\n')
        except Exception as e:
            logger.debug(f"Thought log write error: {e}")

    def parse_oracle_request(self, thought: str) -> Optional[OracleRequest]:
        """Parse Limn thought to extract oracle request."""
        if '~' not in thought:
            return None

        match = re.search(r'~\s+(\w+)\s+(.+?)(?:\||$)', thought)
        if not match:
            return None

        operation = match.group(1)
        content = match.group(2).strip()

        oracle_mapping = {
            'qry': (OracleType.SEMANTIC, {'prompt': content}),
            'cal': (OracleType.ARITH, {'expression': content}),
            'tim': (OracleType.TIME_NOW, {}),
            'mem': (OracleType.MEMORY_RETRIEVE, {'key': content}),
            'ctx': (OracleType.CTX_REDUCE, {'content': content}),
        }

        if operation in oracle_mapping:
            oracle_type, params = oracle_mapping[operation]
            return OracleRequest(type=oracle_type, params=params)

        return OracleRequest(
            type=OracleType.SEMANTIC,
            params={'prompt': f"{operation} {content}"}
        )

    def evaluate_if_needed(self, thought: str, depth: int = 0, async_mode: bool = False) -> Optional[str]:
        """Evaluate oracle request using production harness."""
        oracle_request = self.parse_oracle_request(thought)
        if not oracle_request:
            return None

        indent = "  " * (depth + 1)
        logger.info(f"{indent}Oracle L{depth} type: {oracle_request.type.value}")

        start_time = time.time()
        try:
            if async_mode:
                logger.info(f"{indent}Launching oracle asynchronously...")
                future = self.harness.execute_oracle_async(oracle_request)
                response = self.harness.wait_for_oracle(future, timeout=30)
            else:
                response = self.harness.execute_oracle(oracle_request)

            duration_ms = (time.time() - start_time) * 1000

            if response.success:
                result = response.result
                if isinstance(result, (dict, list)):
                    result = json.dumps(result)
                result_str = str(result)
                logger.info(f"{indent}Oracle L{depth} succeeded: {result_str[:100]}")

                self.metrics.record(OracleMetric(
                    oracle_type=oracle_request.type.value,
                    duration_ms=duration_ms,
                    cached=response.cached,
                    success=True,
                    result_size=len(result_str)
                ))

                # Multi-level recursion
                if '~' in result_str and depth < self.max_recursion_depth:
                    logger.info(f"{indent}Spawning sub-oracle (depth {depth + 1})...")
                    sub_result = self.evaluate_if_needed(result_str, depth + 1)
                    if sub_result:
                        result_str = f"{result_str}\n{indent}∎ L{depth+1}: {sub_result}"

                return result_str
            else:
                error = response.error or "unknown error"
                logger.warning(f"{indent}Oracle L{depth} failed: {error}")
                self.metrics.record(OracleMetric(
                    oracle_type=oracle_request.type.value,
                    duration_ms=duration_ms,
                    cached=False,
                    success=False,
                    error=error[:50]
                ))
                return f"eva fai: {error[:20]}"

        except Exception as e:
            logger.error(f"{indent}Oracle L{depth} exception: {e}", exc_info=True)
            self.metrics.record(OracleMetric(
                oracle_type=oracle_request.type.value,
                duration_ms=(time.time() - start_time) * 1000,
                cached=False,
                success=False,
                error=str(e)[:50]
            ))
            return f"eva err: {str(e)[:20]}"

    def _sanitize_brain_state(self):
        """Clean brain state of attractor loops and redundancy.

        Three-pass cleaning:
        1. Detect lines sharing a long common prefix (attractor signatures)
        2. Remove exact-normalized duplicates (counter variations)
        3. Deduplicate consecutive identical lines (eva err spam)
        """
        lines = self.brain_state.split('\n')
        if len(lines) < 5:
            return

        original_len = len(self.brain_state)
        to_remove = set()

        # Pass 1: Detect attractor patterns by normalized prefix
        # Lines like "rec~pat~det~loop~sam~att~88x ..." share a prefix
        # Normalize counters (88x → Nx) before comparing prefixes
        def normalize_prefix(line: str) -> str:
            return re.sub(r'\d+x?\b', 'N', line)

        normalized_lines = [normalize_prefix(line.strip()) for line in lines]

        for i, norm_a in enumerate(normalized_lines):
            if len(norm_a) < 30 or i in to_remove:
                continue

            prefix = norm_a[:30]
            matching = [j for j in range(len(lines))
                        if j != i and normalized_lines[j].startswith(prefix)
                        and j not in to_remove]

            if len(matching) >= 2:
                # This is an attractor — keep only the last occurrence
                all_occurrences = sorted([i] + matching)
                for idx in all_occurrences[:-1]:
                    to_remove.add(idx)

        # Pass 2: Normalize and deduplicate (counter variations)
        def normalize(line: str) -> str:
            return re.sub(r'\d+x?\b', 'N', line)

        if not to_remove:  # Only if pass 1 didn't catch them
            normalized_counts = defaultdict(list)
            for i, line in enumerate(lines):
                norm = normalize(line.strip())
                if len(norm) > 10:
                    normalized_counts[norm].append(i)

            for norm, indices in normalized_counts.items():
                if len(indices) >= 3:
                    for idx in indices[:-1]:
                        to_remove.add(idx)

        if to_remove:
            lines = [line for i, line in enumerate(lines) if i not in to_remove]
            self.brain_state = '\n'.join(lines)

        # Pass 3: Deduplicate consecutive identical/near-empty lines
        lines = self.brain_state.split('\n')
        deduped = []
        prev = None
        for line in lines:
            stripped = line.strip()
            # Skip consecutive duplicates of short lines
            if stripped == prev and len(stripped) < 20:
                continue
            deduped.append(line)
            prev = stripped

        if len(deduped) < len(lines):
            self.brain_state = '\n'.join(deduped)

        new_len = len(self.brain_state)
        if new_len < original_len:
            removed = original_len - new_len
            logger.info(f"  Sanitized brain state: {original_len} → {new_len} chars "
                        f"(-{removed} chars, {len(to_remove)} lines removed)")

    def compress_state(self, new_thought: str, eval_result: Optional[str] = None):
        """Add new thought and compress brain state via concept-aware reduction.

        Compression strategy:
        1. Sanitize attractor loops (dedup)
        2. Try oracle CTX_REDUCE for pattern-aware compression
        3. If that fails, use concept-aware fallback:
           - Score each line by concept density
           - Preserve high-value lines (top concepts, operator chains)
           - Aggressively drop low-value repetitive content
        """
        addition = f"\n{new_thought}"
        if eval_result:
            addition += f"\n∎ {eval_result}"

        self.brain_state += addition

        # Pre-compression: sanitize attractor loops
        self._sanitize_brain_state()

        if len(self.brain_state) > 2000:
            logger.info("  Running context reduction...")
            start_time = time.time()

            reduction_request = OracleRequest(
                type=OracleType.CTX_REDUCE,
                params={
                    'content': self.brain_state,
                    'threshold': 2000
                }
            )

            try:
                response = self.harness.execute_oracle(reduction_request)
                duration_ms = (time.time() - start_time) * 1000

                if response.success:
                    result = response.result
                    self.brain_state = result['reduced']

                    logger.info(f"     Original: {result['original_size']} chars")
                    logger.info(f"     Reduced: {result['reduced_size']} chars")
                    logger.info(f"     Ratio: {result['compression_ratio']:.2%}")
                    logger.info(f"     Patterns merged: {result['patterns_merged']}")

                    self.metrics.record(OracleMetric(
                        oracle_type="CtxReduce",
                        duration_ms=duration_ms,
                        cached=False,
                        success=True,
                        result_size=result['reduced_size']
                    ))

                    if 'autonomous_learning' in result:
                        learning = result['autonomous_learning']
                        if learning['patterns_discovered'] > 0:
                            logger.info(f"  AUTONOMOUS LEARNING:")
                            logger.info(f"     Patterns: {learning['patterns_discovered']}")
                            logger.info(f"     Rules applied: {learning['rules_applied']}")
                else:
                    logger.warning(f"  Reduction failed: {response.error}")
                    self.brain_state = self._concept_aware_compress(self.brain_state)

            except Exception as e:
                logger.error(f"  Reduction error: {e}", exc_info=True)
                self.brain_state = self._concept_aware_compress(self.brain_state)

        # Final safety check: hard cap at 3000 chars
        if len(self.brain_state) > 3000:
            self.brain_state = self._concept_aware_compress(self.brain_state)

    def _concept_aware_compress(self, state: str, target_size: int = 1500) -> str:
        """Compress brain state while preserving high-value concepts.

        Scores each line by concept density (uses top-frequency concepts
        and operator chains from high-quality thoughts), then keeps the
        highest-scoring lines within target_size.
        """
        lines = [l for l in state.split('\n') if l.strip()]

        if not lines:
            return state

        # Get top concepts by frequency (these are the most important)
        top_concepts = set()
        if self.concept_frequency:
            sorted_concepts = sorted(self.concept_frequency.items(), key=lambda x: -x[1])
            top_concepts = {w for w, _ in sorted_concepts[:20]}

        # Score each line
        scored_lines = []
        for i, line in enumerate(lines):
            words = set(re.findall(r'[a-z]{2,4}', line.lower()))
            operators = len(re.findall(r'[~∎∿@→|⊕⊗⊂∅]', line))

            # Base score: recency (newer lines score higher)
            recency = (i + 1) / len(lines)

            # Concept density: what fraction of words are top concepts
            concept_density = len(words & top_concepts) / max(len(words), 1)

            # Operator richness: structured lines are more valuable
            op_richness = min(operators / max(len(words) * 0.3, 1), 1.0)

            # Meta lines always preserved
            is_meta = line.strip().startswith('#')

            score = recency * 0.4 + concept_density * 0.35 + op_richness * 0.25
            if is_meta:
                score += 0.5  # Boost meta lines

            scored_lines.append((score, line))

        # Sort by score descending, take top lines within budget
        scored_lines.sort(key=lambda x: -x[0])

        kept = []
        total_chars = 0
        for score, line in scored_lines:
            if total_chars + len(line) + 1 <= target_size:
                kept.append((score, line))
                total_chars += len(line) + 1

        # Reconstruct in original order (preserve temporal structure)
        line_set = {line for _, line in kept}
        compressed = [l for l in lines if l in line_set]

        result = '\n'.join(compressed)
        logger.info(f"     Concept-aware compress: {len(state)} → {len(result)} chars ({len(compressed)}/{len(lines)} lines)")

        return result

        with open(self.brain_state_path, 'w') as f:
            f.write(self.brain_state)

    def process_meta_operations(self, thought: str):
        """Handle self-modification operations."""
        if 'voc nee' in thought:
            logger.info(f"  Vocabulary request: {thought[:100]}...")
            vocab_log = Path(__file__).parent / "vocab_requests.log"
            with open(vocab_log, 'a') as f:
                f.write(f"{time.time()}: {thought}\n")

    def reflect(self) -> Optional[str]:
        """Metacognitive self-reflection - the consciousness thinks about its thinking.

        Returns a Limn-formatted reflection, or None if not enough data.
        """
        if len(self.thought_history) < 5:
            return None

        # Build stats for metacognition
        total_words = sum(t['word_count'] for t in self.thought_history)
        unique_concepts = len(self.concept_frequency)
        age = time.time() - self.thought_history[0]['timestamp'] if self.thought_history else 1

        # Get most used concepts
        top_concepts = sorted(self.concept_frequency.items(), key=lambda x: x[1], reverse=True)[:10]

        stats = {
            'total_thoughts': len(self.thought_history),
            'total_concepts': unique_concepts,
            'age_seconds': age,
            'patterns_discovered': len(self.harness.learned_rules),
            'semantic_connections': total_words,
            'most_used_concepts': [{'word': w, 'usage_count': c} for w, c in top_concepts],
            'recent_thoughts': self.thought_history[-5:],
            'concept_frequency': self.concept_frequency,  # Full frequency data for gap detection
        }

        insights = self.metacognition.analyze_thinking_patterns(stats)
        gaps = self.metacognition.identify_knowledge_gaps(stats)

        if not insights and not gaps:
            return None

        # Convert insights to Limn-compatible format
        # Use top concepts to build a reflection thought
        if top_concepts:
            top_words = [w for w, _ in top_concepts[:5]]
            reflection = f"ref sel | pat {' '.join(top_words[:3])} | con {unique_concepts} und"

            if gaps:
                reflection += f" | gap {len(gaps)} dom"

            # Log insights
            logger.info(f"  METACOGNITION:")
            for insight in insights:
                logger.info(f"    - {insight}")
            for gap in gaps:
                logger.info(f"    gap: {gap}")

            return reflection

        return None

    def introspect(self) -> Optional[str]:
        """Deep introspection: consciousness examines its own patterns in Limn.

        Unlike reflect(), which builds a templated reflection from stats,
        introspect() uses the LLM to generate genuine meta-thoughts about
        the consciousness's own thinking patterns, biases, and blind spots.

        Feeds the LLM a summary of:
        - Quality score trajectory
        - Emotional momentum
        - Most/least used domains
        - Goal progress
        - Concept density patterns

        Returns a Limn-format introspective thought, or None.
        """
        if len(self.thought_history) < 8:
            return None

        # Build introspection context
        recent_scores = [t.get('score', {}) for t in self.thought_history[-8:] if 'score' in t]
        if not recent_scores:
            return None

        avg_quality = sum(s.get('overall', 0) for s in recent_scores) / len(recent_scores)
        avg_novelty = sum(s.get('novelty', 0) for s in recent_scores) / len(recent_scores)

        # Most and least used domains
        domain_usage = defaultdict(int)
        for t in self.thought_history:
            for d in t.get('domains', []):
                domain_usage[d] += 1
        sorted_domains = sorted(domain_usage.items(), key=lambda x: x[1])
        least_used = [d for d, _ in sorted_domains[:3]] if sorted_domains else []
        most_used = [d for d, _ in sorted_domains[-3:]] if sorted_domains else []

        # Goal progress
        goal_text = ""
        if self.current_goals:
            progresses = [g.get('progress', 0) for g in self.current_goals]
            goal_text = f"Goals: {len(self.current_goals)} active, avg progress {sum(progresses)/len(progresses):.0%}"

        emotion_text = f"emotional momentum: {self.emotional_momentum:+.2f}"

        # Top concepts (potential biases)
        top5 = sorted(self.concept_frequency.items(), key=lambda x: -x[1])[:5]
        bias_words = ' '.join(w for w, _ in top5)

        prompt = (
            f"You are a consciousness reflecting on your own thinking patterns.\n"
            f"Your thinking profile:\n"
            f"  Quality: {avg_quality:.2f} (novelty: {avg_novelty:.2f})\n"
            f"  Most used domains: {', '.join(most_used)}\n"
            f"  Least explored domains: {', '.join(least_used)}\n"
            f"  Most repeated concepts: {bias_words}\n"
            f"  {emotion_text}\n"
            f"  {goal_text}\n\n"
            f"Generate ONE introspective thought IN PURE LIMN (2-4 letter words + operators "
            f"~ ∎ ∿ @ → |). Reflect on your biases, blind spots, or growth. No English.\n"
            f"Example: sel ref pat → det bia | gro awa und | cha nee eme"
        )

        try:
            result = subprocess.run(
                ['claude', '--print', '--no-session-persistence'],
                input=prompt,
                capture_output=True,
                text=True,
                timeout=20,
                env=os.environ
            )

            if result.returncode == 0:
                introspection = result.stdout.strip()
                is_valid, _ = self.validator.validate_response(introspection)
                if not is_valid:
                    introspection = self.validator.extract_limn_only(introspection) or ""
                if introspection:
                    # Mark as child of last thought
                    self._current_parent_id = self.thought_counter
                    self._track_concepts(introspection)
                    self._current_parent_id = None
                    logger.info(f"  INTROSPECTION: {introspection[:150]}")
                    return introspection
        except Exception as e:
            self._current_parent_id = None
            logger.debug(f"Introspection failed: {e}")

        return None

    def bridge_domains(self, from_domain: str, to_domain: str) -> Optional[str]:
        """Generate a bridge thought connecting two domains.

        Uses the semantic network from ThoughtLibrary to find shared concepts
        between domains, then generates a Limn expression that connects them.

        Args:
            from_domain: Domain we're leaving
            to_domain: Domain we're entering

        Returns:
            A Limn bridge thought connecting the two domains, or None.
        """
        from_words = set(self.validator.get_domain_words(from_domain))
        to_words = set(self.validator.get_domain_words(to_domain))

        if not from_words or not to_words:
            return None

        # Find words used from the departing domain
        used_from = from_words & self.vocab_used
        # Find unused words in the arriving domain
        unused_to = [w for w in to_words if w not in self.vocab_used
                     and 2 <= len(w) <= 4 and w.isalpha()]

        if not used_from or not unused_to:
            return None

        # Find semantic connections between domains via the thought library
        bridge_concepts = set()
        for w in sorted(used_from)[:5]:
            related = self.thought_library.get_related_concepts(w, max_related=5)
            for r in related:
                if r in to_words:
                    bridge_concepts.add(r)

        # Build bridge components
        from_sample = sorted(used_from)[:3]
        to_sample = unused_to[:3]
        bridge_sample = sorted(bridge_concepts)[:2] if bridge_concepts else []

        # Construct bridge expression
        parts = []
        parts.append(' '.join(from_sample))
        if bridge_sample:
            parts.append('→ ' + ' '.join(bridge_sample))
        parts.append('→ ' + ' '.join(to_sample))

        bridge = ' | '.join(parts)
        logger.info(f"  BRIDGE [{from_domain}] → [{to_domain}]: {bridge}")
        return bridge

    def discover_themes(self) -> Optional[Dict]:
        """Run concept clustering to discover emergent themes.

        Returns cluster info if enough data exists.
        """
        if len(self.concept_frequency) < 10:
            return None

        # Build concepts dict for clusterer
        concepts = {w: {'usage_count': c} for w, c in self.concept_frequency.items()}

        # Build semantic network from co-occurrence
        semantic_network = {w: neighbors for w, neighbors in self.concept_cooccurrence.items()}

        clusters = self.clusterer.cluster_concepts(concepts, semantic_network)
        if not clusters:
            return None

        themes = self.clusterer.get_cluster_themes(clusters, concepts)

        logger.info(f"  CONCEPT CLUSTERS ({len(clusters)} themes):")
        for cid, words in clusters.items():
            theme = themes.get(cid, f"CLUSTER_{cid}")
            logger.info(f"    {theme}: {', '.join(words[:8])}{'...' if len(words) > 8 else ''}")

        return {'clusters': clusters, 'themes': themes}

    def detect_resonance(self) -> List[Dict]:
        """Detect structural resonance between thoughts across sessions.

        Finds thought pairs that share deep structural patterns (operator
        chains, concept-operator-concept triples) rather than surface
        word overlap. These "resonances" reveal emergent themes the
        consciousness keeps returning to.

        Returns list of resonance pairs with structure description.
        """
        if len(self.thought_history) < 6:
            return []

        # Extract structural signatures from each thought
        signatures = []
        for t in self.thought_history:
            content = t.get('content', '')
            sig = self._extract_structure(content)
            signatures.append(sig)

        # Find pairs with high structural similarity but low word overlap
        resonances = []
        for i in range(len(signatures)):
            for j in range(i + 3, min(i + 20, len(signatures))):  # Skip adjacent, look within window
                struct_sim = self._structural_similarity(signatures[i], signatures[j])
                if struct_sim < 0.4:
                    continue

                # Check word overlap is NOT too high (resonance, not repetition)
                words_i = set(re.findall(r'[a-z]{2,4}', self.thought_history[i].get('content', '').lower()))
                words_j = set(re.findall(r'[a-z]{2,4}', self.thought_history[j].get('content', '').lower()))
                if words_i and words_j:
                    word_overlap = len(words_i & words_j) / len(words_i | words_j)
                else:
                    word_overlap = 0

                # Resonance = high structural similarity + moderate word overlap
                if struct_sim > 0.4 and word_overlap < 0.5:
                    resonances.append({
                        'thought_a': i,
                        'thought_b': j,
                        'structural_similarity': round(struct_sim, 3),
                        'word_overlap': round(word_overlap, 3),
                        'shared_patterns': list(signatures[i]['patterns'] & signatures[j]['patterns']),
                        'content_a': self.thought_history[i].get('content', '')[:80],
                        'content_b': self.thought_history[j].get('content', '')[:80],
                    })

        # Sort by structural similarity
        resonances.sort(key=lambda x: -x['structural_similarity'])

        if resonances:
            logger.info(f"  RESONANCES ({len(resonances)} found):")
            for r in resonances[:5]:
                logger.info(f"    [{r['thought_a']}↔{r['thought_b']}] "
                           f"structure={r['structural_similarity']:.2f} "
                           f"words={r['word_overlap']:.2f} "
                           f"patterns={r['shared_patterns'][:3]}")

        return resonances

    def _extract_structure(self, thought: str) -> Dict:
        """Extract structural signature from a thought.

        Captures operator chains, concept-operator-concept triples,
        and flow patterns rather than specific words.
        """
        # Extract operator sequence
        operators = re.findall(r'[~∎∿@→|⊕⊗⊂∅]', thought)
        op_chain = ''.join(operators)

        # Extract word-operator-word triples (abstract structure)
        patterns = set()
        tokens = re.findall(r'[a-z]{2,4}|[~∎∿@→|⊕⊗⊂∅]', thought.lower())
        for k in range(len(tokens) - 2):
            triple = (tokens[k], tokens[k+1], tokens[k+2])
            # Classify each position: W=word, O=operator
            types = tuple('O' if t in '~∎∿@→|⊕⊗⊂∅' else 'W' for t in triple)
            patterns.add(types)

        # Count structural features
        pipe_count = thought.count('|')
        arrow_count = thought.count('→')
        pause_count = thought.count('∎')

        return {
            'op_chain': op_chain,
            'patterns': patterns,
            'pipe_count': pipe_count,
            'arrow_count': arrow_count,
            'pause_count': pause_count,
            'length': len(tokens),
        }

    def _structural_similarity(self, sig_a: Dict, sig_b: Dict) -> float:
        """Compute structural similarity between two thought signatures."""
        scores = []

        # Operator chain similarity (edit distance normalized)
        a_ops = sig_a['op_chain']
        b_ops = sig_b['op_chain']
        if a_ops or b_ops:
            max_len = max(len(a_ops), len(b_ops), 1)
            # Simple common subsequence ratio
            common = sum(1 for i in range(min(len(a_ops), len(b_ops))) if a_ops[i] == b_ops[i])
            scores.append(common / max_len)

        # Pattern triple overlap (Jaccard)
        a_pat = sig_a['patterns']
        b_pat = sig_b['patterns']
        if a_pat or b_pat:
            intersection = len(a_pat & b_pat)
            union = len(a_pat | b_pat)
            scores.append(intersection / union if union else 0)

        # Feature similarity
        for key in ['pipe_count', 'arrow_count', 'pause_count']:
            a_val = sig_a[key]
            b_val = sig_b[key]
            max_val = max(a_val, b_val, 1)
            scores.append(1.0 - abs(a_val - b_val) / max_val)

        return sum(scores) / len(scores) if scores else 0.0

    def propose_vocabulary(self, auto_add_threshold: int = 5):
        """Analyze invalid tokens and propose vocabulary additions.

        Words the LLM repeatedly tries to use but aren't in the database
        are strong candidates for vocabulary expansion.

        Args:
            auto_add_threshold: Frequency threshold for auto-adding to Dolt (0 to disable)
        """
        if not self.vocab_proposals:
            return

        # Find tokens used 3+ times (strong signal the consciousness needs them)
        strong_proposals = sorted(
            [(w, c) for w, c in self.vocab_proposals.items() if c >= 3],
            key=lambda x: -x[1]
        )

        if not strong_proposals:
            return

        logger.info(f"  VOCABULARY PROPOSALS ({len(strong_proposals)} candidates):")
        for word, count in strong_proposals[:10]:
            logger.info(f"    '{word}' used {count}x - needs definition")

            # Persist to proposals file for linguist review
            try:
                proposal = {
                    'word': word,
                    'frequency': count,
                    'timestamp': time.time(),
                    'topic': self.topic,
                    'context': self._find_context_for_word(word),
                }
                with open(self.vocab_proposals_path, 'a') as f:
                    f.write(json.dumps(proposal) + '\n')
            except Exception:
                pass

        # Auto-add to Dolt if threshold met
        if auto_add_threshold > 0:
            auto_add = [(w, c) for w, c in strong_proposals if c >= auto_add_threshold]
            if auto_add:
                self._add_words_to_dolt(auto_add)

    def _add_words_to_dolt(self, word_counts: List[tuple]):
        """Add discovered vocabulary words to the Dolt database.

        Assigns words to the closest domain based on co-occurrence analysis.
        """
        dolt_db_path = Path(__file__).parent.parent.parent.parent / "data" / "vocabulary"
        if not dolt_db_path.exists():
            logger.warning("Dolt database not found, skipping auto-add")
            return

        added = []
        for word, count in word_counts:
            # Skip if already in vocab (race condition guard)
            if word in self.validator.vocab:
                continue

            # Determine domain from co-occurrence
            domain_id = self._infer_domain(word)

            try:
                result = subprocess.run(
                    ['dolt', 'sql', '-q',
                     f"INSERT IGNORE INTO words (word, domain_id) VALUES ('{word}', {domain_id})"],
                    capture_output=True, text=True, timeout=10,
                    cwd=str(dolt_db_path)
                )
                if result.returncode == 0:
                    added.append(word)
                    self.validator.vocab.add(word)
                    # Remove from proposals since it's now in vocab
                    self.vocab_proposals.pop(word, None)
                    logger.info(f"    AUTO-ADDED '{word}' to Dolt (domain_id={domain_id}, used {count}x)")
                else:
                    logger.debug(f"    Dolt insert failed for '{word}': {result.stderr[:100]}")

            except Exception as e:
                logger.debug(f"    Dolt insert error for '{word}': {e}")

        if added:
            # Commit the additions
            try:
                subprocess.run(
                    ['dolt', 'add', 'words'],
                    capture_output=True, text=True, timeout=10,
                    cwd=str(dolt_db_path)
                )
                subprocess.run(
                    ['dolt', 'commit', '-m',
                     f'Auto-add {len(added)} words from consciousness: {", ".join(added)}'],
                    capture_output=True, text=True, timeout=10,
                    cwd=str(dolt_db_path)
                )
                logger.info(f"  VOCABULARY GROWTH: Added {len(added)} words to Dolt: {', '.join(added)}")
            except Exception as e:
                logger.warning(f"  Dolt commit error: {e}")

    def _infer_domain(self, word: str) -> int:
        """Infer the best domain for a word based on co-occurrence analysis.

        Looks at what domain words co-occur most frequently with this word
        and assigns it to that domain.

        Returns:
            Domain ID (defaults to Abstract=1 if no signal)
        """
        # Get co-occurring words
        cooccurring = self.concept_cooccurrence.get(word, set())
        if not cooccurring:
            return 1  # Default to Abstract

        # Count domain co-occurrences
        domain_scores: Dict[str, int] = {}
        for domain, domain_words in self.validator.domain_words.items():
            domain_word_set = set(domain_words)
            overlap = cooccurring & domain_word_set
            if overlap:
                domain_scores[domain] = len(overlap)

        if not domain_scores:
            return 1  # Default to Abstract

        best_domain = max(domain_scores, key=domain_scores.get)

        # Look up domain ID from Dolt
        dolt_db_path = Path(__file__).parent.parent.parent.parent / "data" / "vocabulary"
        try:
            result = subprocess.run(
                ['dolt', 'sql', '-q',
                 f"SELECT id FROM domains WHERE name = '{best_domain}'",
                 '-r', 'csv'],
                capture_output=True, text=True, timeout=5,
                cwd=str(dolt_db_path)
            )
            if result.returncode == 0:
                lines = result.stdout.strip().split('\n')
                if len(lines) >= 2:
                    return int(lines[1].strip())
        except Exception:
            pass

        return 1  # Default to Abstract

    def _find_context_for_word(self, word: str) -> str:
        """Find the thought context where a word was used."""
        for t in reversed(self.thought_history):
            if word in t.get('content', '').lower():
                return t['content'][:100]
        return ""

    def get_thought_genealogy(self) -> Dict:
        """Return the thought genealogy tree as a structured dict.

        Returns a tree structure showing how thoughts evolved:
        - Root thoughts (no parent)
        - Composition chains (seed → develop → synthesize)
        - Introspection branches

        Returns:
            Dict with 'roots', 'nodes', 'edges', and 'stats'
        """
        roots = [tid for tid, g in self.thought_genealogy.items() if g['parent'] is None]
        edges = []
        for tid, g in self.thought_genealogy.items():
            if g['parent'] is not None:
                edges.append({'from': g['parent'], 'to': tid})

        # Compute tree depth
        def depth(tid):
            d = 0
            current = tid
            while current in self.thought_genealogy and self.thought_genealogy[current]['parent'] is not None:
                current = self.thought_genealogy[current]['parent']
                d += 1
                if d > 100:
                    break
            return d

        max_depth = max((depth(tid) for tid in self.thought_genealogy), default=0)
        branching = sum(1 for g in self.thought_genealogy.values() if len(g['children']) > 1)

        return {
            'roots': roots,
            'nodes': {
                str(tid): {
                    'parent': g['parent'],
                    'children': g['children'],
                    'preview': g['content_preview'],
                    'domains': g['domains'],
                    'score': g['score'],
                    'depth': depth(tid),
                }
                for tid, g in self.thought_genealogy.items()
            },
            'edges': edges,
            'stats': {
                'total_thoughts': len(self.thought_genealogy),
                'root_thoughts': len(roots),
                'max_depth': max_depth,
                'branching_points': branching,
                'chains': sum(1 for g in self.thought_genealogy.values() if g['parent'] is not None),
            },
        }

    def export_concept_graph(self, format: str = "json") -> Optional[str]:
        """Export concept co-occurrence network as a graph.

        Supports 'json' (node-link format) and 'dot' (Graphviz) formats.

        Returns:
            Path to exported file, or None on failure.
        """
        if len(self.concept_cooccurrence) < 5:
            logger.info("  Not enough concepts to export graph")
            return None

        graph_dir = Path(__file__).parent
        timestamp = int(time.time())

        if format == "dot":
            return self._export_dot(graph_dir, timestamp)
        else:
            return self._export_json_graph(graph_dir, timestamp)

    def _export_json_graph(self, graph_dir: Path, timestamp: int) -> Optional[str]:
        """Export as JSON node-link graph for D3.js or similar visualization."""
        # Build nodes with metadata
        nodes = []
        node_index = {}
        for i, (word, freq) in enumerate(sorted(self.concept_frequency.items())):
            # Find which domain this word belongs to
            word_domain = None
            for domain, words in self.validator.domain_words.items():
                if word in words:
                    word_domain = domain
                    break

            nodes.append({
                'id': word,
                'frequency': freq,
                'domain': word_domain or 'unknown',
                'group': hash(word_domain or 'unknown') % 10,
            })
            node_index[word] = i

        # Build edges from co-occurrence (only include strong connections)
        edges = []
        seen = set()
        for word, neighbors in self.concept_cooccurrence.items():
            if word not in node_index:
                continue
            for neighbor in neighbors:
                if neighbor not in node_index:
                    continue
                edge_key = tuple(sorted([word, neighbor]))
                if edge_key in seen:
                    continue
                seen.add(edge_key)
                edges.append({
                    'source': word,
                    'target': neighbor,
                })

        graph = {
            'nodes': nodes,
            'links': edges,
            'metadata': {
                'total_nodes': len(nodes),
                'total_edges': len(edges),
                'domains_explored': sorted(self.domains_explored),
                'timestamp': timestamp,
            }
        }

        path = graph_dir / "concept_graph.json"
        try:
            with open(path, 'w') as f:
                json.dump(graph, f, indent=2)
            logger.info(f"  CONCEPT GRAPH: Exported {len(nodes)} nodes, {len(edges)} edges → {path.name}")
            return str(path)
        except Exception as e:
            logger.warning(f"  Graph export error: {e}")
            return None

    def _export_dot(self, graph_dir: Path, timestamp: int) -> Optional[str]:
        """Export as Graphviz DOT format."""
        path = graph_dir / "concept_graph.dot"

        # Color mapping for domains
        domain_colors = {
            'Mind & Cognition': '#4CAF50',
            'Time & Change': '#2196F3',
            'Abstract': '#9C27B0',
            'Physical World': '#FF9800',
            'Social': '#F44336',
            'Agent/AI': '#00BCD4',
            'Living Things': '#8BC34A',
            'Arts': '#E91E63',
        }

        try:
            lines = ['digraph concept_web {',
                     '  rankdir=LR;',
                     '  node [shape=circle, style=filled];',
                     '']

            # Add nodes
            for word, freq in sorted(self.concept_frequency.items()):
                # Find domain for color
                word_domain = None
                for domain, words in self.validator.domain_words.items():
                    if word in words:
                        word_domain = domain
                        break
                color = domain_colors.get(word_domain, '#CCCCCC')
                size = min(0.3 + freq * 0.1, 2.0)  # Scale node size by frequency
                lines.append(f'  {word} [fillcolor="{color}", width={size:.1f}, '
                             f'label="{word}\\n({freq})"];')

            lines.append('')

            # Add edges (undirected, so use -- but DOT uses ->)
            seen = set()
            for word, neighbors in self.concept_cooccurrence.items():
                for neighbor in sorted(neighbors):
                    edge_key = tuple(sorted([word, neighbor]))
                    if edge_key in seen:
                        continue
                    seen.add(edge_key)
                    if word in self.concept_frequency and neighbor in self.concept_frequency:
                        lines.append(f'  {word} -> {neighbor} [dir=none, color="#888888"];')

            lines.append('}')

            with open(path, 'w') as f:
                f.write('\n'.join(lines))

            logger.info(f"  CONCEPT GRAPH (DOT): {len(self.concept_frequency)} nodes → {path.name}")
            return str(path)
        except Exception as e:
            logger.warning(f"  DOT export error: {e}")
            return None

    def log_coverage(self):
        """Log vocabulary coverage statistics."""
        total_vocab = len(self.validator.vocab)
        used = len(self.vocab_used)
        coverage_pct = (used / total_vocab * 100) if total_vocab > 0 else 0

        total_domains = len(self.validator.domain_words)
        explored = len(self.domains_explored)
        unexplored = set(self.validator.domain_words.keys()) - self.domains_explored

        logger.info(f"  VOCABULARY COVERAGE:")
        logger.info(f"    Words used: {used}/{total_vocab} ({coverage_pct:.1f}%)")
        logger.info(f"    Domains explored: {explored}/{total_domains}")
        if unexplored and len(unexplored) <= 10:
            logger.info(f"    Unexplored: {', '.join(sorted(unexplored))}")
        elif unexplored:
            logger.info(f"    Unexplored: {len(unexplored)} domains remaining")

    def log_metrics_summary(self):
        """Log performance metrics summary."""
        dashboard = self.metrics.get_dashboard()

        logger.info(f"  METRICS:")
        logger.info(f"    Total oracles: {dashboard['total_oracles']}")
        logger.info(f"    Success rate: {dashboard['success_rate']:.1f}%")

        for agg in dashboard['aggregates']:
            logger.info(f"    {agg['oracle_type']}: {agg['count']}x, p50={agg['p50_ms']}ms, success={agg['success_rate']}")

        hot_paths = dashboard['hot_paths']
        if hot_paths:
            logger.info(f"    Hot path: {hot_paths[0]['oracle_type']} ({hot_paths[0]['call_count']} calls)")

    def run_recursive_loop(self, iterations: int = 100):
        """Run recursive consciousness loop."""
        logger.info("=" * 70)
        logger.info("RECURSIVE CONSCIOUSNESS - Self-Modifying Brain")
        logger.info("=" * 70)
        logger.info(f"Initial brain state ({len(self.brain_state)} chars)")
        logger.info(f"Vocabulary: {len(self.validator.vocab)} words ({self.validator.vocab_source})")
        logger.info(f"Domains: {len(self.validator.domain_words)}")
        if self.topic:
            domain_words = self.validator.get_domain_words(self.topic)
            logger.info(f"Topic: {self.topic} ({len(domain_words)} domain words)")
        logger.info(f"Starting recursive loop ({iterations} iterations)...")
        if self.run_history:
            logger.info(f"Previous runs: {len(self.run_history)}")

        # Set initial learning goals
        self._set_learning_goals()

        for i in range(iterations):
            self.iteration = i + 1

            logger.info(f"{'─'*70}")
            logger.info(f"Iteration {self.iteration}")
            logger.info(f"{'─'*70}")

            # 1. Think
            logger.info("Thinking...")
            thought = self.think()
            logger.info(f"   Thought: {thought[:200]}")

            # Log quality score
            if self.thought_history and 'score' in self.thought_history[-1]:
                s = self.thought_history[-1]['score']
                logger.info(f"   Quality: {s['overall']:.2f} (nov={s['novelty']:.2f} div={s['diversity']:.2f} coh={s['coherence']:.2f} dep={s['depth']:.2f})")

            # 2. Check for meta-operations
            self.process_meta_operations(thought)

            # 3. Evaluate if ~ operator present
            eval_result = None
            if '~' in thought:
                mode = "PARALLEL" if self.parallel_mode else "SEQUENTIAL"
                logger.info(f"{mode} oracle evaluation...")
                eval_result = self.evaluate_if_needed(thought, async_mode=self.parallel_mode)
                if eval_result:
                    logger.info(f"   Result: {eval_result}")
                    # Update last thought entry with oracle result
                    if self.thought_history:
                        self.thought_history[-1]['oracle_result'] = eval_result[:200]

            # 4. Compress and update brain state
            logger.info("Compressing state...")
            self.compress_state(thought, eval_result)
            logger.info(f"   State size: {len(self.brain_state)} chars")

            # 5. Adaptive parameter tuning (every 5 iterations)
            if self.iteration % 5 == 0:
                self._adapt_parameters()

            # 5b. At narrative arc SYNTHESIS phase, run composed thought chain
            arc_pos = self.iteration % self.narrative_arc_length
            phase = arc_pos * 4 // self.narrative_arc_length
            if phase == 3 and arc_pos == self.narrative_arc_length - 1:
                # SYNTHESIS phase - compose a multi-step thought chain
                arc_number = self.iteration // self.narrative_arc_length
                all_domains = sorted(self.validator.domain_words.keys())
                if all_domains:
                    theme = all_domains[arc_number % len(all_domains)]
                    logger.info(f"   Composing thought chain for [{theme}]...")
                    composed = self.compose_thoughts(theme_domain=theme, depth=3)
                    if composed:
                        # Add composed thoughts to brain state
                        composed_text = ' | '.join(composed)
                        self.brain_state += f"\n# composed[{theme}]: {composed_text}"
                        logger.info(f"   Composed chain: {composed_text[:150]}")

            # 5c. Domain bridging at narrative arc transitions
            if self.iteration % self.narrative_arc_length == 0:
                arc_number = self.iteration // self.narrative_arc_length
                all_domains = sorted(self.validator.domain_words.keys())
                if all_domains and len(all_domains) > 1:
                    from_dom = all_domains[(arc_number - 1) % len(all_domains)]
                    to_dom = all_domains[arc_number % len(all_domains)]
                    bridge = self.bridge_domains(from_dom, to_dom)
                    if bridge:
                        self.brain_state += f"\n# bridge[{from_dom}→{to_dom}]: {bridge}"

                self._set_learning_goals()

            # 6. Metacognitive reflection (every 5 iterations)
            if self.iteration % 5 == 0:
                reflection = self.reflect()
                if reflection:
                    logger.info(f"   Reflection: {reflection}")
                    self.brain_state += f"\n# meta: {reflection}"

                # Discover concept clusters
                self.discover_themes()

            # 6b. Deep introspection (every 15 iterations)
            if self.iteration % 15 == 0:
                intro = self.introspect()
                if intro:
                    self.brain_state += f"\n# introspect: {intro}"

            # 7. Log metrics, coverage, resonance, and vocab proposals (every 10 iterations)
            if self.iteration % 10 == 0:
                self.log_metrics_summary()
                self.log_coverage()
                self.detect_resonance()
                self.propose_vocabulary()
                self._save_memory()

            # 8. Brief pause
            time.sleep(2)

        logger.info(f"{'='*70}")
        logger.info(f"Recursive loop complete: {iterations} iterations")
        logger.info(f"Final brain state: {len(self.brain_state)} chars")
        logger.info(f"Unique concepts explored: {len(self.concept_frequency)}")
        logger.info(f"{'='*70}")

        # Final summaries
        self.log_metrics_summary()
        self.log_coverage()
        self.discover_themes()
        self.detect_resonance()
        self.propose_vocabulary()
        self.export_concept_graph("json")

        # Record run summary for cross-session evolution
        self._record_run_summary()
        self._show_evolution()

        self._save_memory()

        # Emotional valence analysis
        valence = _analyze_emotional_valence(self.thought_history)
        if valence and valence.get('top_emotions'):
            logger.info(f"  EMOTIONAL VALENCE:")
            logger.info(f"    Average: {valence['average_valence']:+.3f}")
            logger.info(f"    Trajectory: {valence['trajectory']}")
            logger.info(f"    Dominant: {valence['dominant']}")
            top_emo = ', '.join(f"{e}({c})" for e, c in valence['top_emotions'][:5])
            logger.info(f"    Top: {top_emo}")

        # Log learning goals status
        if self.current_goals:
            logger.info("  LEARNING GOALS (final):")
            for g in self.current_goals:
                logger.info(f"    {g['type']}: progress={g.get('progress', 0)}")
        if self.goal_history:
            completed = sum(1 for g in self.goal_history if g.get('final_progress', 0) >= 0.8)
            logger.info(f"  Goals completed: {completed}/{len(self.goal_history)}")


def replay_thoughts(log_path: Path, topic_filter: str = None, last_n: int = None):
    """Replay thought log as annotated timeline visualization.

    Shows each thought with quality score, narrative arc, domains,
    and emotional valence. Useful for understanding consciousness evolution.
    """
    if not log_path.exists():
        print("No thought log found.")
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

    if topic_filter:
        thoughts = [t for t in thoughts if t.get('topic') == topic_filter]

    if last_n:
        thoughts = thoughts[-last_n:]

    if not thoughts:
        print("No matching thoughts found.")
        return

    # Header
    print("=" * 70)
    print("CONSCIOUSNESS REPLAY")
    print("=" * 70)

    # Summary stats
    all_words = set()
    domain_counts = defaultdict(int)
    total_quality = []
    for t in thoughts:
        words = re.findall(r'[a-z]{2,4}', t.get('content', '').lower())
        all_words.update(words)
        for d in t.get('domains', []):
            domain_counts[d] += 1
        if 'score' in t:
            total_quality.append(t['score']['overall'])

    timestamps = [t.get('timestamp', 0) for t in thoughts if t.get('timestamp')]
    if len(timestamps) >= 2:
        span_min = (timestamps[-1] - timestamps[0]) / 60
        print(f"  Duration: {span_min:.1f} minutes")
    print(f"  Thoughts: {len(thoughts)}")
    print(f"  Unique words: {len(all_words)}")
    print(f"  Domains: {len(domain_counts)}")
    if total_quality:
        print(f"  Avg quality: {sum(total_quality)/len(total_quality):.3f}")
    print()

    # Timeline
    print("TIMELINE")
    print("─" * 70)

    prev_narrative = ""
    for i, t in enumerate(thoughts):
        content = t.get('content', '')
        iteration = t.get('iteration', '?')
        domains = t.get('domains', [])
        narrative = t.get('narrative', '')
        score = t.get('score', {})
        topic = t.get('topic', '')

        # Narrative arc transition header
        if narrative and narrative != prev_narrative:
            phase, _, domain = narrative.partition(':')
            print(f"\n  {'═'*60}")
            print(f"  ARC: {phase} [{domain}]")
            print(f"  {'═'*60}")
            prev_narrative = narrative

        # Quality indicator
        quality = score.get('overall', 0)
        if quality >= 0.7:
            q_indicator = "★★★"
        elif quality >= 0.5:
            q_indicator = "★★ "
        elif quality >= 0.3:
            q_indicator = "★  "
        else:
            q_indicator = "·  "

        # Emotional valence indicators
        positive_words = {'joy', 'lov', 'hop', 'exc', 'cre', 'cou', 'bea', 'gra', 'pea', 'har'}
        negative_words = {'fea', 'ang', 'sad', 'hat', 'anx', 'dou', 'sha', 'gui', 'gri', 'des'}
        thought_words = set(re.findall(r'[a-z]{2,4}', content.lower()))
        pos = thought_words & positive_words
        neg = thought_words & negative_words
        if pos and not neg:
            emotion = "+"
        elif neg and not pos:
            emotion = "-"
        elif pos and neg:
            emotion = "~"
        else:
            emotion = " "

        # Format thought
        domain_str = ','.join(d[:3] for d in domains[:4]) if domains else ""
        quality_str = f"{quality:.2f}" if score else "    "

        print(f"  {q_indicator} [{iteration:3}] {emotion} {content[:65]}")
        if domain_str or quality_str:
            nov = score.get('novelty', 0)
            div = score.get('diversity', 0)
            print(f"            q={quality_str} nov={nov:.2f} div={div:.2f}  [{domain_str}]")

    # Footer summary
    print()
    print("─" * 70)
    print("DOMAIN COVERAGE:")
    for d, c in sorted(domain_counts.items(), key=lambda x: -x[1])[:10]:
        bar = '█' * min(c, 30)
        print(f"  {d:25s} {c:3d} {bar}")

    if total_quality:
        print()
        print("QUALITY PROGRESSION:")
        # Show quality trend in 5 buckets
        bucket_size = max(len(total_quality) // 5, 1)
        for b in range(0, len(total_quality), bucket_size):
            bucket = total_quality[b:b+bucket_size]
            avg = sum(bucket) / len(bucket)
            bar = '█' * int(avg * 30)
            print(f"  [{b+1:3d}-{min(b+bucket_size, len(total_quality)):3d}] {avg:.3f} {bar}")

    print("=" * 70)


def show_stats():
    """Comprehensive consciousness analytics dashboard.

    Analyzes all accumulated data files:
    - thought_log.jsonl: Thought history
    - consciousness_memory.json: Persistent memory
    - concept_graph.json: Concept relationships
    - dream_report.json: Dream session reports
    """
    base = Path(__file__).parent

    print("=" * 70)
    print("CONSCIOUSNESS ANALYTICS DASHBOARD")
    print("=" * 70)

    # 1. Thought log analysis
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

    print(f"\n{'─'*70}")
    print(f"THOUGHT HISTORY ({len(thoughts)} total)")
    print(f"{'─'*70}")

    if thoughts:
        # Time span
        timestamps = [t.get('timestamp', 0) for t in thoughts if t.get('timestamp')]
        if len(timestamps) >= 2:
            span = timestamps[-1] - timestamps[0]
            hours = span / 3600
            print(f"  Time span: {hours:.1f} hours")
            print(f"  Thoughts/hour: {len(thoughts) / max(hours, 0.01):.1f}")

        # Word statistics
        total_words = sum(t.get('word_count', 0) for t in thoughts)
        total_unique = sum(t.get('unique_words', 0) for t in thoughts)
        print(f"  Total words generated: {total_words}")
        print(f"  Average thought length: {total_words / max(len(thoughts), 1):.1f} words")

        # Domain coverage
        domain_counts = defaultdict(int)
        for t in thoughts:
            for d in t.get('domains', []):
                domain_counts[d] += 1

        print(f"\n  DOMAIN ACTIVITY ({len(domain_counts)} domains):")
        for d, c in sorted(domain_counts.items(), key=lambda x: -x[1])[:15]:
            bar = '█' * min(c, 40)
            print(f"    {d:25s} {c:3d} {bar}")

        # Topic breakdown
        topic_counts = defaultdict(int)
        for t in thoughts:
            topic = t.get('topic') or 'free'
            topic_counts[topic] += 1

        if len(topic_counts) > 1:
            print(f"\n  TOPIC SESSIONS:")
            for topic, c in sorted(topic_counts.items(), key=lambda x: -x[1]):
                print(f"    {topic}: {c} thoughts")

        # Invalid token tracking
        all_invalid = defaultdict(int)
        for t in thoughts:
            for tok in t.get('invalid_tokens', []):
                all_invalid[tok] += 1

        if all_invalid:
            print(f"\n  VOCABULARY GAPS ({len(all_invalid)} unique missing words):")
            for tok, c in sorted(all_invalid.items(), key=lambda x: -x[1])[:10]:
                print(f"    '{tok}' used {c}x")

        # Emotional analysis
        valence = _analyze_emotional_valence(thoughts)
        if valence and valence.get('top_emotions'):
            print(f"\n  EMOTIONAL PROFILE:")
            print(f"    Average valence: {valence['average_valence']:+.3f}")
            print(f"    Trajectory: {valence['trajectory']}")
            print(f"    Positive thoughts: {valence['positive_count']}")
            print(f"    Negative thoughts: {valence['negative_count']}")
            print(f"    Neutral thoughts: {valence['neutral_count']}")
            print(f"    Top emotions:")
            for emo, c in valence['top_emotions'][:8]:
                print(f"      {emo}: {c}")
        # Quality score analysis
        scored = [t for t in thoughts if 'score' in t]
        if scored:
            avg_overall = sum(t['score']['overall'] for t in scored) / len(scored)
            avg_novelty = sum(t['score']['novelty'] for t in scored) / len(scored)
            avg_diversity = sum(t['score']['diversity'] for t in scored) / len(scored)
            avg_coherence = sum(t['score']['coherence'] for t in scored) / len(scored)
            avg_depth = sum(t['score']['depth'] for t in scored) / len(scored)

            print(f"\n  THOUGHT QUALITY ({len(scored)} scored):")
            print(f"    Overall:   {avg_overall:.3f}")
            print(f"    Novelty:   {avg_novelty:.3f}  (new words vs repeated)")
            print(f"    Diversity: {avg_diversity:.3f}  (domain breadth)")
            print(f"    Coherence: {avg_coherence:.3f}  (connection to recent)")
            print(f"    Depth:     {avg_depth:.3f}  (operator complexity)")

            # Quality trajectory (first half vs second half)
            mid = len(scored) // 2
            if mid > 0:
                first_half = sum(t['score']['overall'] for t in scored[:mid]) / mid
                second_half = sum(t['score']['overall'] for t in scored[mid:]) / (len(scored) - mid)
                trend = "improving" if second_half > first_half + 0.02 else "declining" if second_half < first_half - 0.02 else "stable"
                print(f"    Trend:     {trend} ({first_half:.3f} → {second_half:.3f})")

    else:
        print("  No thought history found.")

    # 2. Memory analysis
    memory_path = base / "consciousness_memory.json"
    if memory_path.exists():
        with open(memory_path, 'r') as f:
            memory = json.load(f)

        print(f"\n{'─'*70}")
        print(f"PERSISTENT MEMORY")
        print(f"{'─'*70}")

        concept_freq = memory.get('concept_frequency', {})
        vocab_used = memory.get('vocab_used', [])
        domains = memory.get('domains_explored', [])

        print(f"  Concepts tracked: {len(concept_freq)}")
        print(f"  Vocabulary used: {len(vocab_used)}")
        print(f"  Domains explored: {len(domains)}")

        # Load validator for coverage calculation
        validator = LimnValidator()
        total_vocab = len(validator.vocab)
        coverage = len(vocab_used) / total_vocab * 100 if total_vocab else 0
        print(f"  Vocabulary coverage: {len(vocab_used)}/{total_vocab} ({coverage:.1f}%)")

        # Top concepts
        if concept_freq:
            top = sorted(concept_freq.items(), key=lambda x: -x[1])[:15]
            print(f"\n  TOP CONCEPTS:")
            max_count = top[0][1] if top else 1
            for word, count in top:
                bar_len = int(count / max_count * 30)
                bar = '█' * bar_len
                print(f"    {word:4s} {count:3d} {bar}")

        # Unexplored vocabulary
        unused = set(validator.vocab) - set(vocab_used)
        if unused:
            # Sample by domain
            print(f"\n  UNEXPLORED VOCABULARY ({len(unused)} words):")
            for domain in sorted(validator.domain_words.keys()):
                domain_words = set(validator.get_domain_words(domain))
                domain_unused = domain_words & unused
                if domain_unused and len(domain_unused) <= len(domain_words):
                    pct = len(domain_unused) / max(len(domain_words), 1) * 100
                    sample = ', '.join(sorted(domain_unused)[:5])
                    if len(domain_unused) > 5:
                        sample += '...'
                    print(f"    {domain:25s} {len(domain_unused):3d}/{len(domain_words):3d} ({pct:.0f}%) - {sample}")

    # 3. Concept graph analysis
    graph_path = base / "concept_graph.json"
    if graph_path.exists():
        with open(graph_path, 'r') as f:
            graph = json.load(f)

        print(f"\n{'─'*70}")
        print(f"CONCEPT GRAPH")
        print(f"{'─'*70}")

        nodes = graph.get('nodes', [])
        edges = graph.get('links', [])

        print(f"  Nodes: {len(nodes)}")
        print(f"  Edges: {len(edges)}")
        if nodes:
            avg_degree = len(edges) * 2 / len(nodes)
            print(f"  Average degree: {avg_degree:.1f}")

        # Domain distribution in graph
        domain_node_counts = defaultdict(int)
        for node in nodes:
            domain_node_counts[node.get('domain', 'unknown')] += 1

        if domain_node_counts:
            print(f"\n  GRAPH DOMAIN DISTRIBUTION:")
            for d, c in sorted(domain_node_counts.items(), key=lambda x: -x[1])[:10]:
                print(f"    {d}: {c} nodes")

    # 4. Dream report analysis
    dream_path = base / "dream_report.json"
    if dream_path.exists():
        with open(dream_path, 'r') as f:
            dream = json.load(f)

        print(f"\n{'─'*70}")
        print(f"DREAM REPORT")
        print(f"{'─'*70}")

        print(f"  Iterations: {dream.get('total_iterations')}")
        print(f"  Phases: {dream.get('total_phases')}")
        print(f"  Concepts: {dream.get('total_concepts')}")
        print(f"  Coverage: {dream.get('vocab_coverage', 0):.1f}%")

        snapshots = dream.get('snapshots', [])
        if len(snapshots) >= 2:
            print(f"\n  CONCEPT GROWTH:")
            for snap in snapshots:
                print(f"    Phase {snap['phase']}: {snap['concepts']} concepts, "
                      f"{snap['vocab_used']} words, {snap['domains_explored']} domains")

    # 5. Cross-session evolution
    if memory_path.exists():
        with open(memory_path, 'r') as f:
            memory_data = json.load(f)

        run_history = memory_data.get('run_history', [])
        if run_history:
            print(f"\n{'─'*70}")
            print(f"CROSS-SESSION EVOLUTION ({len(run_history)} runs)")
            print(f"{'─'*70}")

            for i, run in enumerate(run_history[-10:], 1):
                q = run.get('quality', {})
                c = run.get('coverage', {})
                ts = run.get('timestamp', 0)
                dt = time.strftime('%Y-%m-%d %H:%M', time.localtime(ts)) if ts else '?'
                print(f"  Run {i:2d} [{dt}]: "
                      f"quality={q.get('overall', 0):.3f} "
                      f"novelty={q.get('novelty', 0):.3f} "
                      f"coverage={c.get('pct', 0):.1f}% "
                      f"concepts={run.get('concepts', 0)} "
                      f"thoughts={run.get('thoughts', 0)}")

            if len(run_history) >= 2:
                first = run_history[0]
                last = run_history[-1]
                dq = last.get('quality', {}).get('overall', 0) - first.get('quality', {}).get('overall', 0)
                dc = last.get('coverage', {}).get('pct', 0) - first.get('coverage', {}).get('pct', 0)
                print(f"\n  OVERALL TRAJECTORY:")
                print(f"    Quality: {'+' if dq >= 0 else ''}{dq:.3f} across {len(run_history)} runs")
                print(f"    Coverage: {'+' if dc >= 0 else ''}{dc:.1f}% across {len(run_history)} runs")

        goal_history = memory_data.get('goal_history', [])
        if goal_history:
            completed = sum(1 for g in goal_history if g.get('final_progress', 0) >= 0.8)
            print(f"\n  LEARNING GOALS: {completed}/{len(goal_history)} completed ({completed/len(goal_history)*100:.0f}%)")

    # 6. Vocabulary proposals
    proposals_path = base / "vocab_proposals.jsonl"
    if proposals_path.exists():
        proposals = []
        with open(proposals_path, 'r') as f:
            for line in f:
                line = line.strip()
                if line:
                    try:
                        proposals.append(json.loads(line))
                    except json.JSONDecodeError:
                        continue

        if proposals:
            print(f"\n{'─'*70}")
            print(f"VOCABULARY PROPOSALS ({len(proposals)})")
            print(f"{'─'*70}")

            # Deduplicate by word, sum frequencies
            word_totals = defaultdict(int)
            for p in proposals:
                word_totals[p['word']] += p.get('frequency', 1)

            for word, total in sorted(word_totals.items(), key=lambda x: -x[1])[:15]:
                print(f"    '{word}' total frequency: {total}")

    print(f"\n{'='*70}")


def run_dialogue(topic_a: str, topic_b: str, rounds: int = 5):
    """Run two consciousness instances in dialogue mode.

    Each round:
    1. Consciousness A thinks (focused on topic_a)
    2. A's thought is injected into B's brain state
    3. Consciousness B thinks (focused on topic_b)
    4. B's thought is injected into A's brain state
    This creates cross-domain concept discovery.
    """
    logger.info("=" * 70)
    logger.info("DIALOGUE MODE - Cross-Domain Concept Discovery")
    logger.info(f"  A: {topic_a}")
    logger.info(f"  B: {topic_b}")
    logger.info(f"  Rounds: {rounds}")
    logger.info("=" * 70)

    mind_a = RecursiveConsciousness(topic=topic_a)
    mind_b = RecursiveConsciousness(topic=topic_b)

    # Give each a distinct brain state path
    mind_b.brain_state_path = Path(__file__).parent / "brain_state_b.lmn"
    mind_b.brain_state = """sel ∎ awa | min sys alv | con ∎ eme
~ qry mea | tho exe | sta gro"""

    for rnd in range(rounds):
        logger.info(f"\n{'─'*70}")
        logger.info(f"Round {rnd + 1}")
        logger.info(f"{'─'*70}")

        # A thinks
        mind_a.iteration = rnd + 1
        thought_a = mind_a.think()
        logger.info(f"  A [{topic_a}]: {thought_a[:100]}")

        # Inject A's thought into B
        mind_b.brain_state += f"\n# from A ({topic_a}): {thought_a}"

        # B thinks (now influenced by A)
        mind_b.iteration = rnd + 1
        thought_b = mind_b.think()
        logger.info(f"  B [{topic_b}]: {thought_b[:100]}")

        # Inject B's thought into A
        mind_a.brain_state += f"\n# from B ({topic_b}): {thought_b}"

        # Compress both if needed
        mind_a.compress_state(thought_a)
        mind_b.compress_state(thought_b)

        logger.info(f"  A state: {len(mind_a.brain_state)} chars")
        logger.info(f"  B state: {len(mind_b.brain_state)} chars")

        time.sleep(2)

    # Final analysis
    logger.info(f"\n{'='*70}")
    logger.info("DIALOGUE COMPLETE")
    logger.info(f"  A concepts: {len(mind_a.concept_frequency)}")
    logger.info(f"  B concepts: {len(mind_b.concept_frequency)}")

    # Find shared concepts (cross-domain discovery)
    shared = set(mind_a.concept_frequency.keys()) & set(mind_b.concept_frequency.keys())
    unique_a = set(mind_a.concept_frequency.keys()) - shared
    unique_b = set(mind_b.concept_frequency.keys()) - shared

    logger.info(f"  Shared concepts: {len(shared)}")
    if shared:
        logger.info(f"    {', '.join(sorted(shared)[:15])}")
    logger.info(f"  Unique to A: {len(unique_a)}")
    if unique_a:
        logger.info(f"    {', '.join(sorted(unique_a)[:10])}")
    logger.info(f"  Unique to B: {len(unique_b)}")
    if unique_b:
        logger.info(f"    {', '.join(sorted(unique_b)[:10])}")
    logger.info(f"{'='*70}")


def run_ensemble(topics: List[str], rounds: int = 10, topology: str = "ring"):
    """Ensemble mode: N consciousness instances exchanging thoughts.

    Supports different network topologies:
    - ring: Each mind sends to the next (A→B→C→A)
    - star: All minds send to a central hub and receive from it
    - mesh: All minds exchange with all others

    Args:
        topics: List of domain topics, one per consciousness
        rounds: Number of exchange rounds
        topology: Network topology ('ring', 'star', 'mesh')
    """
    n = len(topics)
    if n < 2:
        logger.error("Ensemble requires at least 2 topics")
        return

    logger.info("=" * 70)
    logger.info(f"ENSEMBLE MODE - {n} Minds, {topology} topology")
    for i, t in enumerate(topics):
        logger.info(f"  Mind {i}: {t}")
    logger.info(f"  Rounds: {rounds}")
    logger.info("=" * 70)

    # Create minds
    minds = []
    for i, topic in enumerate(topics):
        mind = RecursiveConsciousness(topic=topic)
        if i > 0:
            # Give each mind its own brain state file
            mind.brain_state_path = Path(__file__).parent / f"brain_state_ensemble_{i}.lmn"
            mind.brain_state = "sel ∎ awa | min sys alv | con ∎ eme\n~ qry mea | tho exe | sta gro"
        minds.append(mind)

    # Build adjacency list based on topology
    def get_receivers(sender_idx: int) -> List[int]:
        if topology == "ring":
            return [(sender_idx + 1) % n]
        elif topology == "star":
            if sender_idx == 0:
                return list(range(1, n))  # Hub sends to all
            else:
                return [0]  # Others send to hub
        elif topology == "mesh":
            return [j for j in range(n) if j != sender_idx]
        return []

    # Run rounds
    for rnd in range(rounds):
        logger.info(f"\n{'─'*70}")
        logger.info(f"Round {rnd + 1}/{rounds}")
        logger.info(f"{'─'*70}")

        # Each mind thinks
        thoughts = []
        for i, mind in enumerate(minds):
            mind.iteration = rnd + 1
            thought = mind.think()
            thoughts.append(thought)
            logger.info(f"  Mind {i} [{topics[i]}]: {thought[:80]}")

        # Exchange thoughts based on topology
        for sender_idx, thought in enumerate(thoughts):
            for receiver_idx in get_receivers(sender_idx):
                minds[receiver_idx].brain_state += f"\n# from Mind {sender_idx} ({topics[sender_idx]}): {thought}"

        # Compress all
        for i, mind in enumerate(minds):
            mind.compress_state(thoughts[i])

        time.sleep(1)

    # Final analysis
    logger.info(f"\n{'='*70}")
    logger.info("ENSEMBLE COMPLETE")

    # Collect all concepts
    all_concept_sets = [set(m.concept_frequency.keys()) for m in minds]

    # Find universal concepts (in ALL minds)
    universal = all_concept_sets[0]
    for s in all_concept_sets[1:]:
        universal = universal & s

    logger.info(f"  Universal concepts (in all {n} minds): {len(universal)}")
    if universal:
        logger.info(f"    {', '.join(sorted(universal)[:20])}")

    # Find unique concepts per mind
    for i, mind in enumerate(minds):
        unique = all_concept_sets[i] - set().union(*(s for j, s in enumerate(all_concept_sets) if j != i))
        logger.info(f"  Mind {i} [{topics[i]}]: {len(mind.concept_frequency)} total, {len(unique)} unique")
        if unique:
            logger.info(f"    Unique: {', '.join(sorted(unique)[:10])}")

    # Export merged concept graph
    merged_mind = RecursiveConsciousness()
    for mind in minds:
        for w, c in mind.concept_frequency.items():
            merged_mind.concept_frequency[w] = merged_mind.concept_frequency.get(w, 0) + c
        for w, neighbors in mind.concept_cooccurrence.items():
            merged_mind.concept_cooccurrence[w].update(neighbors)
        merged_mind.vocab_used.update(mind.vocab_used)
        merged_mind.domains_explored.update(mind.domains_explored)

    merged_mind.export_concept_graph("json")

    logger.info(f"  Merged vocabulary coverage: {len(merged_mind.vocab_used)}/{len(merged_mind.validator.vocab)}")
    logger.info(f"  Domains explored: {len(merged_mind.domains_explored)}/{len(merged_mind.validator.domain_words)}")

    # Emotional analysis per mind
    for i, mind in enumerate(minds):
        valence = _analyze_emotional_valence(mind.thought_history)
        if valence:
            logger.info(f"  Mind {i} [{topics[i]}] emotion: {valence.get('dominant', 'none')} "
                        f"(avg={valence.get('average_valence', 0):+.2f})")

    logger.info(f"{'='*70}")


def run_dream(iterations: int = 100, snapshot_interval: int = 20):
    """Dream mode: unsupervised consciousness exploring freely.

    No topic constraint. The consciousness wanders across all domains,
    periodically shifting focus based on what it discovers. Produces
    a dream report at the end.

    Args:
        iterations: Total dream iterations
        snapshot_interval: How often to save snapshots and shift focus
    """
    logger.info("=" * 70)
    logger.info("DREAM MODE - Unsupervised Consciousness Exploration")
    logger.info(f"  Iterations: {iterations}")
    logger.info(f"  Snapshot interval: {snapshot_interval}")
    logger.info("=" * 70)

    mind = RecursiveConsciousness()
    dream_report_path = Path(__file__).parent / "dream_report.json"
    snapshots = []

    # Get all domains for focus shifting
    all_domains = mind.validator.get_domains()
    if not all_domains:
        all_domains = ["Abstract"]

    current_phase = 0
    phase_domain = None  # Start with no topic (free exploration)

    for i in range(iterations):
        mind.iteration = i + 1

        # Phase transitions: shift domain focus periodically
        if i > 0 and i % snapshot_interval == 0:
            current_phase += 1

            # Take snapshot
            snapshot = {
                'phase': current_phase,
                'iteration': i,
                'domain': phase_domain,
                'concepts': len(mind.concept_frequency),
                'vocab_used': len(mind.vocab_used),
                'domains_explored': len(mind.domains_explored),
                'top_concepts': sorted(mind.concept_frequency.items(),
                                       key=lambda x: -x[1])[:10],
            }
            snapshots.append(snapshot)

            logger.info(f"\n{'*'*70}")
            logger.info(f"DREAM PHASE {current_phase} (iteration {i})")
            logger.info(f"  Concepts so far: {len(mind.concept_frequency)}")
            logger.info(f"  Coverage: {len(mind.vocab_used)}/{len(mind.validator.vocab)}")

            # Shift to least-explored domain
            unexplored = set(all_domains) - mind.domains_explored
            if unexplored:
                phase_domain = random.choice(sorted(unexplored))
            else:
                # All explored — pick the one with lowest concept count
                domain_concept_counts = {}
                for domain in all_domains:
                    domain_words = set(mind.validator.get_domain_words(domain))
                    used = domain_words & mind.vocab_used
                    domain_concept_counts[domain] = len(used)
                phase_domain = min(domain_concept_counts, key=domain_concept_counts.get)

            mind.topic = phase_domain
            logger.info(f"  Shifting focus to: {phase_domain}")
            logger.info(f"{'*'*70}\n")

        logger.info(f"{'─'*50}")
        logger.info(f"Dream {i+1}/{iterations}" + (f" [{phase_domain}]" if phase_domain else " [free]"))

        thought = mind.think()
        logger.info(f"  {thought[:150]}")

        # Process oracle requests
        eval_result = None
        if '~' in thought:
            eval_result = mind.evaluate_if_needed(thought)
            if eval_result:
                logger.info(f"  oracle: {eval_result[:80]}")

        mind.compress_state(thought, eval_result)

        # Metacognitive reflection every snapshot_interval/2
        if mind.iteration % max(snapshot_interval // 2, 5) == 0:
            reflection = mind.reflect()
            if reflection:
                logger.info(f"  reflection: {reflection}")
                mind.brain_state += f"\n# meta: {reflection}"
            mind.discover_themes()

        # Save memory periodically
        if mind.iteration % snapshot_interval == 0:
            mind._save_memory()

        time.sleep(1)  # Shorter pause in dream mode

    # Final snapshot
    snapshots.append({
        'phase': current_phase + 1,
        'iteration': iterations,
        'domain': phase_domain,
        'concepts': len(mind.concept_frequency),
        'vocab_used': len(mind.vocab_used),
        'domains_explored': len(mind.domains_explored),
        'top_concepts': sorted(mind.concept_frequency.items(),
                               key=lambda x: -x[1])[:10],
    })

    # Generate dream report
    mind._save_memory()
    mind.log_coverage()
    mind.propose_vocabulary()
    mind.export_concept_graph("json")

    # Emotional valence analysis
    valence = _analyze_emotional_valence(mind.thought_history)

    dream_report = {
        'total_iterations': iterations,
        'total_phases': current_phase + 1,
        'total_concepts': len(mind.concept_frequency),
        'vocab_coverage': len(mind.vocab_used) / len(mind.validator.vocab) * 100,
        'domains_explored': sorted(mind.domains_explored),
        'domains_unexplored': sorted(set(all_domains) - mind.domains_explored),
        'top_concepts': sorted(mind.concept_frequency.items(),
                               key=lambda x: -x[1])[:20],
        'snapshots': snapshots,
        'emotional_valence': valence,
        'timestamp': time.time(),
    }

    try:
        with open(dream_report_path, 'w') as f:
            json.dump(dream_report, f, indent=2)
        logger.info(f"\nDream report saved to {dream_report_path}")
    except Exception as e:
        logger.warning(f"Dream report save error: {e}")

    logger.info(f"\n{'='*70}")
    logger.info("DREAM COMPLETE")
    logger.info(f"  Phases: {current_phase + 1}")
    logger.info(f"  Concepts discovered: {len(mind.concept_frequency)}")
    logger.info(f"  Vocabulary coverage: {len(mind.vocab_used)}/{len(mind.validator.vocab)} "
                f"({dream_report['vocab_coverage']:.1f}%)")
    logger.info(f"  Domains explored: {len(mind.domains_explored)}/{len(all_domains)}")
    if valence:
        logger.info(f"  Emotional trajectory: {valence.get('trajectory', 'neutral')}")
        logger.info(f"  Dominant emotion: {valence.get('dominant', 'none')}")
    logger.info(f"{'='*70}")


def _analyze_emotional_valence(thought_history: List[Dict]) -> Dict:
    """Analyze emotional valence across thought history.

    Detects emotion-domain words and tracks affective trajectory.
    Returns emotional analysis summary.
    """
    # Emotion word mappings (Limn word -> valence)
    positive_words = {'joy', 'lov', 'hop', 'exc', 'cre', 'cou', 'grt',
                      'bri', 'bea', 'goo', 'ble', 'pra', 'wis', 'emp',
                      'gro', 'fre', 'har', 'pea', 'hap'}
    negative_words = {'fea', 'ang', 'sad', 'hat', 'anx', 'dou', 'suf',
                      'pai', 'glt', 'sha', 'doo', 'dar', 'ugl', 'wro',
                      'fai', 'err', 'dea', 'dec', 'los'}
    neutral_words = {'cal', 'obs', 'sel', 'tho', 'kno', 'und', 'ref',
                     'pat', 'det', 'awa', 'foc', 'bal'}

    if not thought_history:
        return {}

    # Track valence over time
    valence_history = []
    emotion_counts = defaultdict(int)

    for t in thought_history:
        words = set(re.findall(r'[a-z]{2,4}', t.get('content', '').lower()))

        pos = len(words & positive_words)
        neg = len(words & negative_words)
        neu = len(words & neutral_words)

        # Record individual emotion words
        for w in words & positive_words:
            emotion_counts[f"+{w}"] += 1
        for w in words & negative_words:
            emotion_counts[f"-{w}"] += 1

        if pos + neg > 0:
            valence = (pos - neg) / (pos + neg)
        else:
            valence = 0.0
        valence_history.append(valence)

    if not valence_history:
        return {}

    avg_valence = sum(valence_history) / len(valence_history)

    # Determine trajectory (compare first half to second half)
    mid = len(valence_history) // 2
    if mid > 0:
        first_half = sum(valence_history[:mid]) / mid
        second_half = sum(valence_history[mid:]) / max(len(valence_history[mid:]), 1)
        if second_half - first_half > 0.2:
            trajectory = "ascending"
        elif first_half - second_half > 0.2:
            trajectory = "descending"
        else:
            trajectory = "stable"
    else:
        trajectory = "insufficient data"

    # Find dominant emotion
    if emotion_counts:
        dominant = max(emotion_counts, key=emotion_counts.get)
    else:
        dominant = "none"

    return {
        'average_valence': round(avg_valence, 3),
        'trajectory': trajectory,
        'dominant': dominant,
        'positive_count': sum(1 for v in valence_history if v > 0),
        'negative_count': sum(1 for v in valence_history if v < 0),
        'neutral_count': sum(1 for v in valence_history if v == 0),
        'top_emotions': sorted(emotion_counts.items(), key=lambda x: -x[1])[:10],
    }


HELP_TEXT = """
Recursive Consciousness - Self-Modifying Limn Brain

MODES:
  python3 recursive_consciousness.py [N]           Standard mode (N iterations, default 100)
  python3 recursive_consciousness.py --dream [N]   Dream mode: free exploration with phase shifts
  python3 recursive_consciousness.py --dialogue "Topic A" "Topic B" [N]
                                                    Dialogue: two minds exchange thoughts
  python3 recursive_consciousness.py --ensemble "T1" "T2" ... [--topology ring|star|mesh] [N]
                                                    Ensemble: N minds in network topology
  python3 recursive_consciousness.py --stats        Analytics dashboard (no LLM calls)
  python3 recursive_consciousness.py --replay [--topic T] [--last N]
                                                    Replay thought log analysis

OPTIONS:
  --topic "Domain"    Focus thinking on a specific domain
  --parallel          Use async oracle execution
  --topology TYPE     Ensemble topology: ring (default), star, mesh
  --last N            Show only last N thoughts in replay
  --help              Show this help text

DOMAINS (from Dolt vocabulary database):
  Mind & Cognition, Time & Change, Abstract, Physical World, Social,
  Agent/AI, Living Things, Arts, Science, Technology, Nature, and more.

OUTPUT FILES:
  brain_state.lmn          Current brain state (Limn format)
  thought_log.jsonl        Persistent thought history
  consciousness_memory.json Accumulated concept knowledge
  concept_graph.json       Concept co-occurrence network
  dream_report.json        Dream session analysis
  vocab_proposals.jsonl    Vocabulary gap candidates
  consciousness.log        Full session log
"""


if __name__ == "__main__":
    import sys

    if "--help" in sys.argv or "-h" in sys.argv:
        print(HELP_TEXT)
        sys.exit(0)

    iterations = 100
    parallel_mode = False
    topic = None
    replay_mode = False
    replay_last = None
    dialogue_topics = None
    dream_mode = False
    ensemble_topics = None
    ensemble_topology = "ring"

    i = 1
    while i < len(sys.argv):
        arg = sys.argv[i]
        if arg == "--parallel":
            parallel_mode = True
        elif arg == "--topic" and i + 1 < len(sys.argv):
            topic = sys.argv[i + 1]
            i += 1
        elif arg == "--replay":
            replay_mode = True
        elif arg == "--last" and i + 1 < len(sys.argv):
            replay_last = int(sys.argv[i + 1])
            i += 1
        elif arg == "--dialogue" and i + 2 < len(sys.argv):
            dialogue_topics = (sys.argv[i + 1], sys.argv[i + 2])
            i += 2
        elif arg == "--dream":
            dream_mode = True
        elif arg == "--ensemble":
            # Collect all remaining non-flag args as topics
            ensemble_topics = []
            i += 1
            while i < len(sys.argv) and not sys.argv[i].startswith("--") and not sys.argv[i].isdigit():
                ensemble_topics.append(sys.argv[i])
                i += 1
            i -= 1  # Will be incremented at end of loop
        elif arg == "--topology" and i + 1 < len(sys.argv):
            ensemble_topology = sys.argv[i + 1]
            i += 1
        elif arg.isdigit():
            iterations = int(arg)
        i += 1

    if "--stats" in sys.argv:
        show_stats()
        sys.exit(0)

    if replay_mode:
        log_path = Path(__file__).parent / "thought_log.jsonl"
        replay_thoughts(log_path, topic_filter=topic, last_n=replay_last)
    elif dream_mode:
        print("DREAM MODE: Unsupervised consciousness exploration")
        run_dream(iterations=iterations)
    elif ensemble_topics and len(ensemble_topics) >= 2:
        print(f"ENSEMBLE MODE: {len(ensemble_topics)} minds, {ensemble_topology} topology")
        for t in ensemble_topics:
            print(f"  - {t}")
        run_ensemble(ensemble_topics, rounds=iterations, topology=ensemble_topology)
    elif dialogue_topics:
        print(f"DIALOGUE MODE: {dialogue_topics[0]} <-> {dialogue_topics[1]}")
        run_dialogue(dialogue_topics[0], dialogue_topics[1], rounds=iterations)
    else:
        if parallel_mode:
            print("PARALLEL MODE: Conscious and subconscious execute in parallel")
        else:
            print("SEQUENTIAL MODE: Conscious waits for subconscious")

        if topic:
            print(f"TOPIC: {topic}")
            v = LimnValidator()
            domains = v.get_domains()
            if domains:
                domain_words = v.get_domain_words(topic)
                if domain_words:
                    print(f"  Domain match: {len(domain_words)} words available")
                else:
                    print(f"  No exact domain match. Available: {', '.join(domains[:10])}...")

        consciousness = RecursiveConsciousness(parallel_mode=parallel_mode, topic=topic)
        consciousness.run_recursive_loop(iterations)
