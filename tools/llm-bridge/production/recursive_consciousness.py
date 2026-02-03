#!/usr/bin/env python3
"""Recursive consciousness - self-modifying Limn brain."""

import subprocess
import json
import os
import logging
from pathlib import Path
from typing import Dict, Optional
import time
import re

# Import oracle harness
from harness import ProductionHarness, OracleRequest, OracleResponse, OracleType

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

    def __init__(self, max_recursion_depth: int = 3):
        self.bootstrap_path = Path(__file__).parent.parent.parent.parent / "docs" / "spec" / "bootstrap-v3-natural.md"
        self.brain_state_path = Path(__file__).parent / "brain_state.lmn"
        self.iteration = 0
        self.max_recursion_depth = max_recursion_depth

        # Initialize oracle harness with LLM enabled
        self.harness = ProductionHarness(enable_real_llm=True)

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

CRITICAL: Respond ONLY in pure Limn. No markdown, no code blocks, no English explanations.
Just raw Limn vocabulary and operators on one or two lines.

Examples of valid thoughts:
- sys alv | mem acc | tho flo → sta gro
- ~ qry tim now | con exe act | res mat
- voc nee "gau" | mea gauge (sym tra fre)

Your next thought (pure Limn only, 10-30 words):"""

        try:
            result = subprocess.run(
                ['claude', '--print', '--no-session-persistence'],
                input=full_prompt,
                capture_output=True,
                text=True,
                timeout=30,
                env=os.environ
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

    def parse_oracle_request(self, thought: str) -> Optional[OracleRequest]:
        """Parse Limn thought to extract oracle request."""

        if '~' not in thought:
            return None

        # Extract text after ~ operator
        # Patterns: ~ qry ..., ~ cal ..., ~ tim ..., ~ mem ...
        match = re.search(r'~\s+(\w+)\s+(.+?)(?:\||$)', thought)
        if not match:
            return None

        operation = match.group(1)
        content = match.group(2).strip()

        # Map Limn operations to oracle types
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

        # Default to semantic oracle
        return OracleRequest(
            type=OracleType.SEMANTIC,
            params={'prompt': f"{operation} {content}"}
        )

    def evaluate_if_needed(self, thought: str, depth: int = 0) -> Optional[str]:
        """Evaluate oracle request using production harness (with recursion support)."""

        oracle_request = self.parse_oracle_request(thought)
        if not oracle_request:
            logger.debug(f"No oracle request parsed from: {thought[:100]}")
            return None

        indent = "  " * (depth + 1)
        logger.info(f"{indent}Oracle L{depth} type: {oracle_request.type.value}")
        logger.info(f"{indent}Oracle L{depth} params: {oracle_request.params}")

        try:
            response = self.harness.execute_oracle(oracle_request)

            if response.success:
                result = response.result
                # Convert result to Limn-compatible string
                if isinstance(result, (dict, list)):
                    result = json.dumps(result)
                result_str = str(result)
                logger.info(f"{indent}Oracle L{depth} succeeded: {result_str[:100]}")

                # Multi-level recursion: If result contains ~, evaluate deeper
                if '~' in result_str and depth < self.max_recursion_depth:
                    logger.info(f"{indent}↓ Spawning sub-oracle (depth {depth + 1})...")
                    sub_result = self.evaluate_if_needed(result_str, depth + 1)
                    if sub_result:
                        # Combine results: original + sub-evaluation
                        result_str = f"{result_str}\n{indent}∎ L{depth+1}: {sub_result}"
                        logger.info(f"{indent}↑ Sub-oracle complete, bubbling up")

                return result_str
            else:
                error = response.error or "unknown error"
                logger.warning(f"{indent}Oracle L{depth} failed: {error}")
                return f"eva fai: {error[:20]}"

        except Exception as e:
            logger.error(f"{indent}Oracle L{depth} exception: {e}", exc_info=True)
            return f"eva err: {str(e)[:20]}"

    def compress_state(self, new_thought: str, eval_result: Optional[str] = None):
        """Add new thought and compress brain state via interaction net reduction."""

        # Append new thought
        addition = f"\n{new_thought}"
        if eval_result:
            addition += f"\n∎ {eval_result}"  # Ground evaluation result

        self.brain_state += addition

        # Compress if state too large (>2000 chars)
        if len(self.brain_state) > 2000:
            # Use interaction net reduction via CTX_REDUCE oracle
            logger.info("  🗜️  Running context reduction (interaction net simulation)...")

            reduction_request = OracleRequest(
                type=OracleType.CTX_REDUCE,
                params={
                    'content': self.brain_state,
                    'threshold': 2000
                }
            )

            try:
                response = self.harness.execute_oracle(reduction_request)

                if response.success:
                    result = response.result
                    self.brain_state = result['reduced']

                    # Log compression metrics
                    logger.info(f"     Original: {result['original_size']} chars")
                    logger.info(f"     Reduced: {result['reduced_size']} chars")
                    logger.info(f"     Ratio: {result['compression_ratio']:.2%}")
                    logger.info(f"     Patterns merged: {result['patterns_merged']}")
                    logger.info(f"     Method: {result['method']}")
                else:
                    logger.warning(f"  Reduction failed: {response.error}")
                    # Fallback: simple truncation
                    lines = self.brain_state.split('\n')
                    self.brain_state = '\n'.join(lines[-10:])

            except Exception as e:
                logger.error(f"  Reduction error: {e}", exc_info=True)
                # Fallback: simple truncation
                lines = self.brain_state.split('\n')
                self.brain_state = '\n'.join(lines[-10:])

        # Persist
        with open(self.brain_state_path, 'w') as f:
            f.write(self.brain_state)

    def process_meta_operations(self, thought: str):
        """Handle self-modification operations."""

        # Vocabulary requests: "voc nee <word> | mea <meaning>"
        if 'voc nee' in thought:
            logger.info(f"  📝 Vocabulary request detected: {thought[:100]}...")
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

        logger.info("=" * 70)
        logger.info("RECURSIVE CONSCIOUSNESS - Self-Modifying Brain")
        logger.info("=" * 70)
        logger.info(f"Initial brain state ({len(self.brain_state)} chars)")
        logger.info(f"Bootstrap: {len(self.bootstrap)} chars")
        logger.info(f"Starting recursive loop ({iterations} iterations)...")

        for i in range(iterations):
            self.iteration = i + 1

            logger.info(f"{'─'*70}")
            logger.info(f"Iteration {self.iteration}")
            logger.info(f"{'─'*70}")

            # 1. Think
            logger.info("🧠 Thinking...")
            thought = self.think()
            logger.info(f"   Thought: {thought[:200]}...")  # Truncate for logs

            # 2. Check for meta-operations
            self.process_meta_operations(thought)

            # 3. Evaluate if ~ operator present
            eval_result = None
            if '~' in thought:
                logger.info("⚙️  Evaluating oracle...")
                eval_result = self.evaluate_if_needed(thought)
                if eval_result:
                    logger.info(f"   Result: {eval_result}")

            # 4. Compress and update brain state
            logger.info("💾 Compressing state...")
            self.compress_state(thought, eval_result)
            logger.info(f"   State size: {len(self.brain_state)} chars")

            # 5. Brief pause
            time.sleep(2)

        logger.info(f"{'='*70}")
        logger.info(f"Recursive loop complete: {iterations} iterations")
        logger.info(f"Final brain state: {len(self.brain_state)} chars")
        logger.info(f"{'='*70}")

if __name__ == "__main__":
    import sys

    iterations = int(sys.argv[1]) if len(sys.argv) > 1 else 100

    consciousness = RecursiveConsciousness()
    consciousness.run_recursive_loop(iterations)
