#!/usr/bin/env python3
"""Task Perception - Let the consciousness see and think about work.

This module integrates the consciousness with the Gas Town bead system,
allowing it to:
1. Perceive assigned tasks (hooked beads)
2. Think about tasks using the SLM
3. Generate insights in Limn
4. Report back to the bead system

Usage:
    from task_perception import TaskAwareConsciousness

    tac = TaskAwareConsciousness()
    tac.perceive_and_think()
"""

import json
import logging
import subprocess
import time
from pathlib import Path
from typing import Dict, List, Optional, Any
from dataclasses import dataclass

# Import our components
try:
    from slm_backend import SLMBackend, WorldOracle, get_slm_backend, get_world_oracle
except ImportError:
    # Allow standalone testing
    SLMBackend = None
    WorldOracle = None

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(message)s')
logger = logging.getLogger(__name__)


@dataclass
class TaskInsight:
    """An insight generated about a task."""
    task_id: str
    insight_limn: str  # The insight in pure Limn
    insight_type: str  # analysis, suggestion, question, pattern
    timestamp: float
    monk_energy: bool


class TaskAwareConsciousness:
    """Consciousness that perceives and thinks about Gas Town tasks.

    The Mad Monk sees the work, contemplates its nature, and
    offers cryptic but profound insights.
    """

    def __init__(
        self,
        slm_url: str = "http://localhost:8741",
        workspace_root: str = None
    ):
        """Initialize task-aware consciousness.

        Args:
            slm_url: URL of the SLM server
            workspace_root: Root for file perception sandboxing
        """
        self.slm = SLMBackend(slm_url) if SLMBackend else None
        self.oracle = WorldOracle(workspace_root) if WorldOracle else None
        self.insights: List[TaskInsight] = []
        self._insight_log_path = Path(__file__).parent / "task_insights.jsonl"

    def perceive_current_task(self) -> Optional[Dict[str, Any]]:
        """Perceive the currently hooked task.

        Returns:
            Task info dict or None if no task hooked
        """
        if not self.oracle:
            logger.warning("World oracle not available")
            return None

        hook_info = self.oracle.perceive_tasks()
        if "error" in hook_info or not hook_info.get("hook"):
            return None

        # Parse hook to get bead ID
        hook_text = hook_info.get("raw", "")
        # Extract bead ID from hook output (format varies)
        import re
        bead_match = re.search(r'(limn-[a-z0-9]+|hq-[a-z0-9]+)', hook_text)
        if not bead_match:
            return None

        bead_id = bead_match.group(1)
        return self.oracle.perceive_bead(bead_id)

    def think_about_task(self, task: Dict[str, Any], context: str = "analysis") -> TaskInsight:
        """Generate an insight about a task using the SLM.

        Args:
            task: Task info from perceive_bead
            context: Thinking context (analysis, suggestion, question, pattern)

        Returns:
            TaskInsight with Limn response
        """
        if not self.slm:
            return TaskInsight(
                task_id=task.get("id", "unknown"),
                insight_limn="[SLM unavailable - mad monk sleeping]",
                insight_type=context,
                timestamp=time.time(),
                monk_energy=False
            )

        # Build Limn prompt about the task
        title = task.get("title", "")
        description = task.get("description", "")[:200]
        status = task.get("status", "")

        # Translate task to Limn-ish concepts
        context_prompts = {
            "analysis": f"tas @ ana | tit: {self._to_limn_words(title)} | sta: {status} | ~ see str dee",
            "suggestion": f"tas @ sug | wor: {self._to_limn_words(title)} | ~ cre sol new",
            "question": f"tas @ qry | pro: {self._to_limn_words(title)} | ~ ask wh nee",
            "pattern": f"tas @ pat | des: {self._to_limn_words(description)} | ~ see con hid",
        }

        prompt = context_prompts.get(context, context_prompts["analysis"])

        # Generate insight
        response = self.slm.generate(
            prompt=prompt,
            max_tokens=48,
            temperature=0.8,
            context=context,
            monk_energy=True
        )

        insight = TaskInsight(
            task_id=task.get("id", "unknown"),
            insight_limn=response.text if response.success else f"[Error: {response.error}]",
            insight_type=context,
            timestamp=time.time(),
            monk_energy=response.monk_energy
        )

        self.insights.append(insight)
        self._log_insight(insight)

        return insight

    def _to_limn_words(self, text: str, max_words: int = 5) -> str:
        """Convert English text to Limn-ish abbreviations.

        Extracts key words and truncates to 3-letter forms.
        This is a heuristic bridge - actual Limn vocabulary
        should be used when possible.
        """
        # Extract significant words
        words = [w.lower() for w in text.split() if len(w) > 2]
        # Take first 3 letters of key words
        limn_ish = [w[:3] for w in words[:max_words] if w.isalpha()]
        return ' '.join(limn_ish) if limn_ish else "unk"

    def _log_insight(self, insight: TaskInsight):
        """Log insight to JSONL file."""
        try:
            with open(self._insight_log_path, 'a') as f:
                data = {
                    "task_id": insight.task_id,
                    "insight_limn": insight.insight_limn,
                    "insight_type": insight.insight_type,
                    "timestamp": insight.timestamp,
                    "monk_energy": insight.monk_energy,
                }
                f.write(json.dumps(data) + '\n')
        except Exception as e:
            logger.warning(f"Failed to log insight: {e}")

    def perceive_and_think(self, contexts: List[str] = None) -> List[TaskInsight]:
        """Perceive current task and generate insights.

        Args:
            contexts: List of thinking contexts to use

        Returns:
            List of generated insights
        """
        contexts = contexts or ["analysis", "pattern"]

        task = self.perceive_current_task()
        if not task:
            logger.info("No task hooked - mad monk meditates on void")
            return []

        logger.info(f"Perceiving task: {task.get('id')} - {task.get('title', '')[:50]}")

        insights = []
        for context in contexts:
            insight = self.think_about_task(task, context)
            insights.append(insight)
            logger.info(f"  [{context}]: {insight.insight_limn[:60]}...")

        return insights

    def continuous_contemplation(
        self,
        interval_seconds: int = 60,
        max_iterations: int = 10
    ):
        """Continuously perceive and think about tasks.

        Args:
            interval_seconds: Seconds between contemplation cycles
            max_iterations: Maximum contemplation cycles (0 = infinite)
        """
        logger.info("Mad Monk enters continuous contemplation...")
        iteration = 0

        while max_iterations == 0 or iteration < max_iterations:
            iteration += 1
            logger.info(f"\n=== Contemplation cycle {iteration} ===")

            insights = self.perceive_and_think()

            if insights:
                for insight in insights:
                    print(f"  [{insight.insight_type}] {insight.insight_limn}")
            else:
                print("  (void - no task to contemplate)")

            if max_iterations == 0 or iteration < max_iterations:
                logger.info(f"Meditating for {interval_seconds}s...")
                time.sleep(interval_seconds)

        logger.info("Mad Monk concludes contemplation. sel ∎ com | min @ pea")

    def report_insight_to_bead(self, insight: TaskInsight) -> bool:
        """Report an insight back to the bead system.

        Args:
            insight: The insight to report

        Returns:
            True if successfully reported
        """
        try:
            # Add insight as a note/comment on the bead
            result = subprocess.run(
                [
                    "bd", "comment", insight.task_id,
                    "-m", f"[Mad Monk {insight.insight_type}] {insight.insight_limn}"
                ],
                capture_output=True,
                text=True,
                timeout=10
            )
            if result.returncode == 0:
                logger.info(f"Reported insight to {insight.task_id}")
                return True
            else:
                logger.warning(f"Failed to report: {result.stderr}")
                return False
        except Exception as e:
            logger.warning(f"Report error: {e}")
            return False


def main():
    """CLI for task perception."""
    import argparse

    parser = argparse.ArgumentParser(description="Task-Aware Mad Monk Consciousness")
    parser.add_argument("--slm-url", default="http://localhost:8741",
                       help="SLM server URL")
    parser.add_argument("--once", action="store_true",
                       help="Run once and exit")
    parser.add_argument("--interval", type=int, default=60,
                       help="Seconds between contemplation cycles")
    parser.add_argument("--max-cycles", type=int, default=10,
                       help="Maximum cycles (0 = infinite)")
    parser.add_argument("--report", action="store_true",
                       help="Report insights to bead system")
    args = parser.parse_args()

    tac = TaskAwareConsciousness(slm_url=args.slm_url)

    if args.once:
        insights = tac.perceive_and_think(["analysis", "suggestion", "pattern"])
        for insight in insights:
            print(f"\n[{insight.insight_type}]\n{insight.insight_limn}")
            if args.report:
                tac.report_insight_to_bead(insight)
    else:
        tac.continuous_contemplation(
            interval_seconds=args.interval,
            max_iterations=args.max_cycles
        )


if __name__ == "__main__":
    main()
