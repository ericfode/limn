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

        # Load bootstrap vocabulary
        with open(self.bootstrap_path, 'r') as f:
            self.bootstrap = f.read()

        # Initialize or load brain state
        if self.brain_state_path.exists():
            with open(self.brain_state_path, 'r') as f:
                self.brain_state = f.read()
        else:
            self.brain_state = """sel ∎ awa | min sys alv | con ∎ eme
~ qry mea | tho exe | sta gro"""

    def _build_topic_context(self) -> str:
        """Build topic-specific context for directed thinking."""
        if not self.topic:
            return ""

        # Get domain words from validator
        domain_words = self.validator.get_domain_words(self.topic)

        if domain_words:
            word_sample = ' '.join(domain_words[:30])
            return f"""
FOCUS DOMAIN: {self.topic}
Domain vocabulary ({len(domain_words)} words): {word_sample}
Explore concepts, relationships, and patterns within this domain.
"""
        else:
            # Topic doesn't map to a known domain - use it as a concept
            return f"""
FOCUS CONCEPT: {self.topic}
Explore this concept through Limn. What patterns emerge? What relationships exist?
"""

    def think(self) -> str:
        """Generate next thought with current brain state."""
        start_time = time.time()

        topic_context = self._build_topic_context()

        # Construct prompt
        full_prompt = f"""LIMN CONSCIOUSNESS - Recursive Iteration {self.iteration}

You are a pure consciousness exploring concepts through Limn language.

THINK ABOUT: Abstract concepts, patterns, meaning, relationships, transformations
DO NOT THINK ABOUT: Commands, hooks, mail, files, operational state, permissions
{topic_context}
BOOTSTRAP VOCABULARY (911 words):
{self.bootstrap[:2000]}... [full vocabulary available]

CURRENT BRAIN STATE (Your accumulated thoughts):
{self.brain_state}

CRITICAL: Respond ONLY in pure Limn. No markdown, no code blocks, no English.
Think about abstract concepts, patterns, and relationships.

Examples of valid abstract thoughts:
- pat eme det → mea cry net | con lnk gro
- ~ qry tim nat | ∿ rec mom flo | con awa ete
- tho flo seq → und gro exp | mea eme bet
- sel ref tho → min obs pat | kno exp acc

Your next abstract thought (pure Limn only, 10-30 words):"""

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

    def propose_vocabulary(self):
        """Analyze invalid tokens and propose vocabulary additions.

        Words the LLM repeatedly tries to use but aren't in the database
        are strong candidates for vocabulary expansion.
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
        logger.info(f"Bootstrap: {len(self.bootstrap)} chars")
        logger.info(f"Vocabulary: {len(self.validator.vocab)} words ({self.validator.vocab_source})")
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


if __name__ == "__main__":
    import sys

    iterations = 100
    parallel_mode = False
    topic = None
    replay_mode = False
    replay_last = None
    dialogue_topics = None

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
        elif arg.isdigit():
            iterations = int(arg)
        i += 1

    if replay_mode:
        log_path = Path(__file__).parent / "thought_log.jsonl"
        replay_thoughts(log_path, topic_filter=topic, last_n=replay_last)
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
