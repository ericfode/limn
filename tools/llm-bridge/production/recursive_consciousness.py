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

        # Thought history for metacognition
        self.thought_history: List[Dict] = []
        self.concept_frequency: Dict[str, int] = {}
        self.concept_cooccurrence: Dict[str, Set[str]] = defaultdict(set)  # semantic network
        self.vocab_used: Set[str] = set()  # track which vocab words have been used
        self.domains_explored: Set[str] = set()  # track explored domains
        self.vocab_proposals: Dict[str, int] = {}  # invalid tokens → frequency (vocab gap candidates)
        self.vocab_proposals_path = Path(__file__).parent / "vocab_proposals.jsonl"
        self.memory_path = Path(__file__).parent / "consciousness_memory.json"

        # Initialize or load brain state
        if self.brain_state_path.exists():
            with open(self.brain_state_path, 'r') as f:
                self.brain_state = f.read()
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
            total_thoughts = memory.get('total_thoughts', 0)

            logger.info(f"Loaded memory: {len(self.concept_frequency)} concepts, "
                        f"{len(self.vocab_used)} words used, "
                        f"{len(self.domains_explored)} domains, "
                        f"{total_thoughts} total thoughts")
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
                'total_thoughts': len(self.thought_history),
                'last_save': time.time(),
            }
            with open(self.memory_path, 'w') as f:
                json.dump(memory, f, indent=2)
        except Exception as e:
            logger.warning(f"Memory save error: {e}")

    def _build_vocab_presentation(self) -> str:
        """Build domain-organized vocabulary for the prompt.

        Instead of dumping raw bootstrap prose, present actual vocabulary
        words organized by domain. This gives the consciousness real words
        to work with.
        """
        if not self.validator.domain_words:
            # Fallback: sample from flat vocabulary
            sample = sorted(list(self.validator.vocab))[:100]
            return f"VOCABULARY ({len(self.validator.vocab)} words): {' '.join(sample)}"

        lines = [f"VOCABULARY ({len(self.validator.vocab)} words across {len(self.validator.domain_words)} domains):"]

        # If we have a topic, put that domain first and expanded
        if self.topic:
            topic_words = self.validator.get_domain_words(self.topic)
            if topic_words:
                # Filter to 2-4 letter words only
                valid = [w for w in topic_words if 2 <= len(w) <= 4 and w.isalpha()]
                lines.append(f"  FOCUS [{self.topic}]: {' '.join(valid[:40])}")

        # Show a compact sample from each domain (prioritize unexplored domains)
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

        # Show unexplored domains with more words (encourage exploration)
        for domain, valid in unexplored_domains[:8]:
            lines.append(f"  [{domain}]: {' '.join(valid[:15])}")

        # Show explored domains with fewer words (already familiar)
        for domain, valid in explored_domains[:6]:
            lines.append(f"  [{domain}]: {' '.join(valid[:8])}")

        return '\n'.join(lines)

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

    def think(self) -> str:
        """Generate next thought with current brain state."""
        start_time = time.time()

        topic_context = self._build_topic_context()
        vocab_section = self._build_vocab_presentation()
        thought_chain = self._build_thought_chain()
        exploration = self._build_exploration_nudge()
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

        sections.append("")
        sections.append(vocab_section)

        if thought_chain:
            sections.append("")
            sections.append(thought_chain)

        if exploration:
            sections.append("")
            sections.append(exploration)

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
            f"BRAIN STATE:\n{self.brain_state[-800:]}",
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

        entry = {
            'iteration': self.iteration,
            'content': thought,
            'timestamp': time.time(),
            'word_count': len(words),
            'unique_words': len(unique_words),
            'domains': sorted(thought_domains),
            'topic': self.topic,
        }
        if eval_result:
            entry['oracle_result'] = eval_result[:200]
        if invalid_words:
            entry['invalid_tokens'] = sorted(invalid_words)
            # Track vocabulary gaps
            for w in invalid_words:
                if len(w) >= 2 and len(w) <= 4 and w.isalpha():
                    self.vocab_proposals[w] = self.vocab_proposals.get(w, 0) + 1

        self.thought_history.append(entry)
        self._persist_thought(entry)

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

    def compress_state(self, new_thought: str, eval_result: Optional[str] = None):
        """Add new thought and compress brain state via interaction net reduction."""
        addition = f"\n{new_thought}"
        if eval_result:
            addition += f"\n∎ {eval_result}"

        self.brain_state += addition

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
                    lines = self.brain_state.split('\n')
                    self.brain_state = '\n'.join(lines[-10:])

            except Exception as e:
                logger.error(f"  Reduction error: {e}", exc_info=True)
                lines = self.brain_state.split('\n')
                self.brain_state = '\n'.join(lines[-10:])

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

        for i in range(iterations):
            self.iteration = i + 1

            logger.info(f"{'─'*70}")
            logger.info(f"Iteration {self.iteration}")
            logger.info(f"{'─'*70}")

            # 1. Think
            logger.info("Thinking...")
            thought = self.think()
            logger.info(f"   Thought: {thought[:200]}")

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

            # 5. Metacognitive reflection (every 5 iterations)
            if self.iteration % 5 == 0:
                reflection = self.reflect()
                if reflection:
                    logger.info(f"   Reflection: {reflection}")
                    self.brain_state += f"\n# meta: {reflection}"

                # Discover concept clusters
                self.discover_themes()

            # 6. Log metrics, coverage, and vocab proposals (every 10 iterations)
            if self.iteration % 10 == 0:
                self.log_metrics_summary()
                self.log_coverage()
                self.propose_vocabulary()
                self._save_memory()

            # 7. Brief pause
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
        self.propose_vocabulary()
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


def replay_thoughts(log_path: Path, topic_filter: str = None, last_n: int = None):
    """Replay thought log for analysis."""
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

    # Analyze
    all_words = set()
    domain_counts = defaultdict(int)
    for t in thoughts:
        words = re.findall(r'[a-z]{2,4}', t.get('content', '').lower())
        all_words.update(words)
        for d in t.get('domains', []):
            domain_counts[d] += 1

    print(f"=== Thought Log Analysis ===")
    print(f"Thoughts: {len(thoughts)}")
    print(f"Time span: {thoughts[0].get('timestamp', 0):.0f} -> {thoughts[-1].get('timestamp', 0):.0f}")
    print(f"Unique words: {len(all_words)}")
    print(f"Domains touched: {len(domain_counts)}")
    print()

    print("Domain frequency:")
    for d, c in sorted(domain_counts.items(), key=lambda x: -x[1]):
        print(f"  {d}: {c}")

    print()
    print("Recent thoughts:")
    for t in thoughts[-5:]:
        content = t.get('content', '')[:80]
        domains = ', '.join(t.get('domains', [])[:3])
        print(f"  [{t.get('iteration', '?')}] {content}")
        if domains:
            print(f"       domains: {domains}")


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


if __name__ == "__main__":
    import sys

    iterations = 100
    parallel_mode = False
    topic = None
    replay_mode = False
    replay_last = None
    dialogue_topics = None
    dream_mode = False

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
        elif arg.isdigit():
            iterations = int(arg)
        i += 1

    if replay_mode:
        log_path = Path(__file__).parent / "thought_log.jsonl"
        replay_thoughts(log_path, topic_filter=topic, last_n=replay_last)
    elif dream_mode:
        print("DREAM MODE: Unsupervised consciousness exploration")
        run_dream(iterations=iterations)
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
