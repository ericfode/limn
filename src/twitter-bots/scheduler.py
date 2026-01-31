"""
Limn Twitter Bot Scheduler

Orchestrates conversation scheduling and automated posting for the Limn
marketing campaign. Handles:
- Independent bot posting schedules
- Cross-bot conversation threads
- @-mention response handling
- Campaign phase management
"""

import json
import random
import logging
import threading
import schedule
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict, List, Optional, Callable, Any
from dataclasses import dataclass, field
from enum import Enum
import time

from personas import get_persona, get_all_personas, PERSONAS, get_relationship
from bot_framework import LimnBot, BotOrchestrator, PostDatabase
from content_generator import ContentGenerator, CONTENT_LIBRARY


logger = logging.getLogger(__name__)


# =============================================================================
# Campaign Phases
# =============================================================================

class CampaignPhase(Enum):
    """Campaign phases with different behaviors."""
    MYSTERY = "mystery"           # Bots post independently, minimal interaction
    INTERACTION = "interaction"   # Bots start noticing each other
    CONVERSATION = "conversation" # Full conversations, outsiders engage
    REVEAL = "reveal"             # Explanatory content, soft launch


@dataclass
class PhaseConfig:
    """Configuration for a campaign phase."""
    name: CampaignPhase
    start_date: datetime
    end_date: datetime
    posts_per_day_per_bot: int
    conversation_probability: float  # 0.0 to 1.0
    reply_probability: float         # Probability of replying to other bots
    human_response_probability: float # Probability of responding to humans
    include_links: bool              # Whether to include links in responses
    description: str


# Default phase configurations
DEFAULT_PHASES = [
    PhaseConfig(
        name=CampaignPhase.MYSTERY,
        start_date=datetime(2024, 1, 1),
        end_date=datetime(2024, 1, 7),
        posts_per_day_per_bot=3,
        conversation_probability=0.1,
        reply_probability=0.1,
        human_response_probability=0.3,
        include_links=False,
        description="Bots post independently, building mystery"
    ),
    PhaseConfig(
        name=CampaignPhase.INTERACTION,
        start_date=datetime(2024, 1, 8),
        end_date=datetime(2024, 1, 14),
        posts_per_day_per_bot=3,
        conversation_probability=0.3,
        reply_probability=0.4,
        human_response_probability=0.5,
        include_links=False,
        description="Bots start noticing each other"
    ),
    PhaseConfig(
        name=CampaignPhase.CONVERSATION,
        start_date=datetime(2024, 1, 15),
        end_date=datetime(2024, 1, 21),
        posts_per_day_per_bot=4,
        conversation_probability=0.6,
        reply_probability=0.7,
        human_response_probability=0.7,
        include_links=False,
        description="Full conversations, outsiders engage"
    ),
    PhaseConfig(
        name=CampaignPhase.REVEAL,
        start_date=datetime(2024, 1, 22),
        end_date=datetime(2024, 1, 31),
        posts_per_day_per_bot=3,
        conversation_probability=0.4,
        reply_probability=0.5,
        human_response_probability=0.9,
        include_links=True,
        description="Explanatory content, links to manifesto"
    ),
]


# =============================================================================
# Conversation Scripts
# =============================================================================

@dataclass
class ConversationScript:
    """A pre-planned conversation between bots."""
    name: str
    description: str
    turns: List[Dict[str, str]]  # Each dict has 'bot', 'limn', 'interpretation'
    delay_hours: List[float]     # Hours between each turn


# Pre-planned conversation scripts
CONVERSATION_SCRIPTS = [
    ConversationScript(
        name="The Question",
        description="Observer asks about above/below, others respond with their keys",
        turns=[
            {
                "bot": "observer",
                "limn": "wh abo | wh bel | wh bet",
                "interpretation": "what's above? what's below? what's between?\nthe telescope sees far.\nbut not everywhere."
            },
            {
                "bot": "gardener",
                "limn": "abo = lux | bel = nox | bet = lif",
                "interpretation": "above = light. below = dark. between = life.\nroots know this.\nleaves confirm it."
            },
            {
                "bot": "merchant",
                "limn": "bet = prf",
                "interpretation": "between = profit.\nyou're both describing arbitrage.\nthe gap is where value lives."
            },
            {
                "bot": "void",
                "limn": "nu abo | nu bel | nu bet | wh?",
                "interpretation": "not-above. not-below. not-between. what?\nyou forgot the option\nof not being anywhere."
            },
        ],
        delay_hours=[0, 5, 4, 5]
    ),
    ConversationScript(
        name="The Disagreement",
        description="Merchant claims growth=good, gardener challenges",
        turns=[
            {
                "bot": "merchant",
                "limn": "gro = goo",
                "interpretation": "growth = good.\nobvious.\nthis is not debatable."
            },
            {
                "bot": "gardener",
                "limn": "gro = gro | nu goo nu bad",
                "interpretation": "growth = growth. not-good, not-bad.\ncancer grows.\ndoes that make it good?"
            },
            {
                "bot": "merchant",
                "limn": "can gro = bad gro | val gro = goo gro",
                "interpretation": "cancer-growth = bad-growth.\nvalue-growth = good-growth.\nkey matters. I stand corrected.\npartially."
            },
            {
                "bot": "observer",
                "limn": "uni gro | ete | end?",
                "interpretation": "universe grows. eternal? or ends?\ngrowth without limit\nis just a different name for explosion."
            },
            {
                "bot": "void",
                "limn": "goo | bad | nu (goo | bad)",
                "interpretation": "good. bad. not-(good or bad).\nthe third option is always there.\nyou just don't want to see it."
            },
        ],
        delay_hours=[0, 2, 3, 4, 4]
    ),
    ConversationScript(
        name="The Shared Sentence",
        description="All bots interpret the same sentence differently",
        turns=[
            {
                "bot": "observer",
                "limn": "gro exp bri far",
                "interpretation": "supernovae. expanding. brightening. infinitely far.\nwe only see them die."
            },
            {
                "bot": "gardener",
                "limn": "gro exp bri far",
                "interpretation": "seedlings reaching for light. growing toward something distant.\nthe sun doesn't know they're there."
            },
            {
                "bot": "merchant",
                "limn": "gro exp bri far",
                "interpretation": "market expansion. bright prospects. far horizons.\nbullish. (but check the fundamentals)"
            },
            {
                "bot": "void",
                "limn": "gro exp bri far",
                "interpretation": "growing into nothing. the brighter the light, the darker its absence."
            },
            {
                "bot": "weaver",
                "limn": "gro exp bri far",
                "interpretation": "four words. infinite meanings.\nI could read this forever\nand never finish."
            },
        ],
        delay_hours=[0, 3, 3, 3, 3]
    ),
]


# =============================================================================
# Scheduler Class
# =============================================================================

class LimnScheduler:
    """
    Manages scheduled posting and conversations for Limn bots.

    Handles:
    - Time-based posting schedules
    - Conversation thread orchestration
    - Campaign phase transitions
    - Mention monitoring and responses
    """

    def __init__(
        self,
        orchestrator: Optional[BotOrchestrator] = None,
        phases: Optional[List[PhaseConfig]] = None,
        timezone: str = "America/New_York",
        dry_run: bool = True
    ):
        """
        Initialize the scheduler.

        Args:
            orchestrator: Bot orchestrator (created if not provided)
            phases: Campaign phase configurations
            timezone: Timezone for scheduling
            dry_run: If True, don't actually post
        """
        self.orchestrator = orchestrator or BotOrchestrator(dry_run=dry_run)
        self.phases = phases or DEFAULT_PHASES
        self.timezone = timezone
        self.dry_run = dry_run

        # Database for persistence
        self.database = PostDatabase()

        # Scheduling state
        self._running = False
        self._scheduler_thread: Optional[threading.Thread] = None

        # Content generator for variety
        self.content_generator = ContentGenerator()

        logger.info("LimnScheduler initialized")

    def get_current_phase(self) -> Optional[PhaseConfig]:
        """Get the current campaign phase based on date."""
        now = datetime.now()
        for phase in self.phases:
            if phase.start_date <= now <= phase.end_date:
                return phase
        return None

    def generate_daily_schedule(
        self,
        date: Optional[datetime] = None,
        phase: Optional[PhaseConfig] = None
    ) -> List[Dict]:
        """
        Generate a complete daily schedule.

        Args:
            date: Date to schedule for (default: today)
            phase: Phase config to use (default: current phase)

        Returns:
            List of scheduled items with times and content
        """
        date = date or datetime.now()
        phase = phase or self.get_current_phase()

        if not phase:
            logger.warning("No active campaign phase, using defaults")
            phase = DEFAULT_PHASES[0]

        schedule_items = []

        # Base posting times (spread throughout the day)
        base_times = ["09:00", "11:00", "13:00", "15:00", "17:00", "19:00", "21:00"]

        # Generate standalone posts
        for bot_name, bot in self.orchestrator.bots.items():
            num_posts = phase.posts_per_day_per_bot

            for i in range(num_posts):
                # Select time with some randomization
                time_idx = (i * len(self.orchestrator.bots) + hash(bot_name)) % len(base_times)
                base_time = base_times[time_idx]

                # Add some randomness (up to 30 minutes)
                hour, minute = map(int, base_time.split(":"))
                minute += random.randint(0, 29)
                if minute >= 60:
                    hour += 1
                    minute -= 60

                post_time = f"{hour:02d}:{minute:02d}"

                # Determine post type
                is_conversation_starter = random.random() < phase.conversation_probability

                # Generate content
                post_data = bot.generate_post()

                schedule_items.append({
                    "type": "conversation_starter" if is_conversation_starter else "standalone",
                    "bot": bot_name,
                    "time": f"{date.strftime('%Y-%m-%d')} {post_time}",
                    "limn": post_data["limn"],
                    "interpretation": post_data["interpretation"],
                    "formatted": post_data["formatted"],
                    "phase": phase.name.value
                })

        # Maybe add a scripted conversation
        if random.random() < phase.conversation_probability:
            script = random.choice(CONVERSATION_SCRIPTS)
            conv_items = self._script_to_schedule(script, date)
            schedule_items.extend(conv_items)

        # Sort by time
        schedule_items.sort(key=lambda x: x["time"])

        return schedule_items

    def _script_to_schedule(
        self,
        script: ConversationScript,
        start_date: datetime
    ) -> List[Dict]:
        """Convert a conversation script to scheduled items."""
        items = []
        current_time = start_date.replace(
            hour=random.randint(9, 12),
            minute=random.randint(0, 59)
        )

        first_tweet_id = None

        for i, (turn, delay) in enumerate(zip(script.turns, script.delay_hours)):
            current_time += timedelta(hours=delay)

            items.append({
                "type": "conversation_turn",
                "bot": turn["bot"],
                "time": current_time.strftime("%Y-%m-%d %H:%M"),
                "limn": turn["limn"],
                "interpretation": turn["interpretation"],
                "formatted": f"{turn['limn']}\n\n[{turn['interpretation']}]",
                "conversation_name": script.name,
                "turn_number": i,
                "is_reply": i > 0,
                "reply_to_bot": script.turns[0]["bot"] if i > 0 else None
            })

        return items

    def execute_schedule_item(self, item: Dict) -> Optional[str]:
        """
        Execute a single scheduled item.

        Args:
            item: Schedule item dictionary

        Returns:
            Tweet ID if posted, None otherwise
        """
        bot_name = item["bot"]
        bot = self.orchestrator.bots.get(bot_name)

        if not bot:
            logger.error(f"Bot not found: {bot_name}")
            return None

        logger.info(f"Executing scheduled item: {item['type']} for @limn_{bot_name}")

        if item.get("is_reply"):
            # This is part of a conversation thread
            # In a real implementation, we'd track the parent tweet ID
            return bot.post(limn_sentence=item["limn"])
        else:
            return bot.post(limn_sentence=item["limn"])

    def start_scheduler(self, check_interval_seconds: int = 60):
        """
        Start the background scheduler.

        Args:
            check_interval_seconds: How often to check for scheduled items
        """
        if self._running:
            logger.warning("Scheduler already running")
            return

        self._running = True

        def scheduler_loop():
            """Main scheduler loop."""
            # Generate initial daily schedule
            daily_schedule = self.generate_daily_schedule()
            schedule_queue = list(daily_schedule)

            while self._running:
                now = datetime.now()
                now_str = now.strftime("%Y-%m-%d %H:%M")

                # Check for items to execute
                to_execute = [
                    item for item in schedule_queue
                    if item["time"] <= now_str
                ]

                for item in to_execute:
                    self.execute_schedule_item(item)
                    schedule_queue.remove(item)

                # Regenerate schedule at midnight
                if now.hour == 0 and now.minute < (check_interval_seconds / 60):
                    daily_schedule = self.generate_daily_schedule()
                    schedule_queue = list(daily_schedule)
                    logger.info(f"Generated new daily schedule with {len(schedule_queue)} items")

                time.sleep(check_interval_seconds)

        self._scheduler_thread = threading.Thread(target=scheduler_loop, daemon=True)
        self._scheduler_thread.start()
        logger.info("Scheduler started")

    def stop_scheduler(self):
        """Stop the background scheduler."""
        self._running = False
        if self._scheduler_thread:
            self._scheduler_thread.join(timeout=5.0)
        logger.info("Scheduler stopped")

    def run_conversation_script(
        self,
        script: ConversationScript,
        immediate: bool = False
    ) -> List[Dict]:
        """
        Run a conversation script.

        Args:
            script: The conversation script to run
            immediate: If True, post all turns immediately

        Returns:
            List of post records
        """
        posts = []
        previous_tweet_id = None

        for i, turn in enumerate(script.turns):
            bot = self.orchestrator.bots.get(turn["bot"])
            if not bot:
                continue

            # Wait between turns (unless immediate)
            if not immediate and i > 0:
                delay_hours = script.delay_hours[i]
                delay_seconds = delay_hours * 3600
                logger.info(f"Waiting {delay_hours} hours until next turn...")
                time.sleep(delay_seconds)

            # Format tweet
            formatted = f"{turn['limn']}\n\n[{turn['interpretation']}]"

            # Add mention if reply
            if i > 0:
                mention = f"@limn_{script.turns[0]['bot']}"
                formatted = f"{mention}\n{formatted}"

            # Post
            if self.dry_run:
                logger.info(f"[DRY RUN] @limn_{turn['bot']}: {turn['limn']}")
                tweet_id = f"dry_run_{i}"
            else:
                tweet_id = bot.twitter.post_tweet(
                    formatted,
                    reply_to_id=previous_tweet_id if i > 0 else None
                ) if bot.twitter else None

            posts.append({
                "bot": turn["bot"],
                "tweet_id": tweet_id,
                "limn": turn["limn"],
                "turn": i
            })

            if i == 0:
                previous_tweet_id = tweet_id

        return posts

    def generate_week_schedule(
        self,
        start_date: Optional[datetime] = None
    ) -> Dict[str, List[Dict]]:
        """
        Generate a full week's schedule.

        Args:
            start_date: Start date for the week

        Returns:
            Dict mapping date strings to daily schedules
        """
        start_date = start_date or datetime.now()
        week_schedule = {}

        for day_offset in range(7):
            date = start_date + timedelta(days=day_offset)
            date_str = date.strftime("%Y-%m-%d")
            week_schedule[date_str] = self.generate_daily_schedule(date)

        return week_schedule

    def export_schedule(
        self,
        schedule: List[Dict],
        output_path: str
    ):
        """Export a schedule to JSON file."""
        Path(output_path).parent.mkdir(parents=True, exist_ok=True)
        with open(output_path, "w") as f:
            json.dump(schedule, f, indent=2, default=str)
        logger.info(f"Exported schedule to {output_path}")

    def import_schedule(self, input_path: str) -> List[Dict]:
        """Import a schedule from JSON file."""
        with open(input_path) as f:
            return json.load(f)


# =============================================================================
# Mention Monitor
# =============================================================================

class MentionMonitor:
    """
    Monitors and responds to @-mentions from humans.

    Handles:
    - Periodic mention checking
    - Response generation based on mention content
    - Rate-limited responding
    """

    def __init__(
        self,
        orchestrator: BotOrchestrator,
        check_interval_seconds: int = 300,  # 5 minutes
        dry_run: bool = True
    ):
        self.orchestrator = orchestrator
        self.check_interval = check_interval_seconds
        self.dry_run = dry_run
        self.database = PostDatabase()

        self._running = False
        self._monitor_thread: Optional[threading.Thread] = None

        # Track last seen mention per bot
        self._last_mention_ids: Dict[str, str] = {}

    def classify_mention(self, text: str) -> str:
        """
        Classify what type of mention this is.

        Returns one of: 'question', 'attempt', 'confusion', 'other'
        """
        text_lower = text.lower()

        # Check for question indicators
        if "?" in text or any(w in text_lower for w in ["what", "why", "how", "who"]):
            if any(w in text_lower for w in ["language", "limn", "mean", "saying"]):
                return "question"

        # Check for Limn-like patterns (2-4 letter lowercase words)
        import re
        limn_pattern = r'\b[a-z]{2,4}\b'
        words = re.findall(limn_pattern, text_lower)
        if len(words) >= 3:
            return "attempt"

        # Check for confusion indicators
        if any(w in text_lower for w in ["confused", "don't understand", "what does"]):
            return "confusion"

        return "other"

    def generate_response(
        self,
        bot: LimnBot,
        mention_text: str,
        mention_type: str,
        phase: Optional[PhaseConfig] = None
    ) -> str:
        """Generate an appropriate response to a mention."""
        template_map = {
            "question": "to_question",
            "attempt": "to_attempt",
            "confusion": "to_confusion",
        }

        template_key = template_map.get(mention_type, "to_question")
        template = bot.persona.response_templates.get(template_key)

        if template:
            response = template
        else:
            # Generate a new response
            limn, interp = bot.content_generator.generate_standalone_post(
                bot.persona, use_ai=bool(bot.anthropic)
            )
            response = f"{limn}\n\n[{interp}]"

        # Maybe add link in reveal phase
        if phase and phase.include_links:
            response += "\n\nlearn more: [link]"

        return response

    def start_monitoring(self):
        """Start the mention monitoring background thread."""
        if self._running:
            logger.warning("Monitor already running")
            return

        self._running = True

        def monitor_loop():
            while self._running:
                for bot_name, bot in self.orchestrator.bots.items():
                    self._check_mentions_for_bot(bot)
                time.sleep(self.check_interval)

        self._monitor_thread = threading.Thread(target=monitor_loop, daemon=True)
        self._monitor_thread.start()
        logger.info("Mention monitor started")

    def stop_monitoring(self):
        """Stop the mention monitoring thread."""
        self._running = False
        if self._monitor_thread:
            self._monitor_thread.join(timeout=5.0)
        logger.info("Mention monitor stopped")

    def _check_mentions_for_bot(self, bot: LimnBot):
        """Check and respond to mentions for a single bot."""
        if not bot.twitter:
            return

        try:
            last_id = self._last_mention_ids.get(bot.persona.name)
            mentions = bot.twitter.get_mentions(since_id=last_id, max_results=20)

            for mention in mentions:
                # Record mention
                self.database.record_mention(
                    tweet_id=mention["id"],
                    author_id=mention["author_id"],
                    author_username=mention.get("author_username", "unknown"),
                    text=mention["text"]
                )

                # Classify and respond
                mention_type = self.classify_mention(mention["text"])
                response = self.generate_response(bot, mention["text"], mention_type)

                if not self.dry_run:
                    reply_id = bot.twitter.post_tweet(response, reply_to_id=mention["id"])
                    if reply_id:
                        self.database.mark_mention_responded(mention["id"], reply_id)

                # Update last seen
                self._last_mention_ids[bot.persona.name] = mention["id"]

        except Exception as e:
            logger.error(f"Error checking mentions for {bot.persona.name}: {e}")


# =============================================================================
# CLI Interface
# =============================================================================

def main():
    """Command-line interface for the scheduler."""
    import argparse

    parser = argparse.ArgumentParser(description="Limn Bot Scheduler")
    subparsers = parser.add_subparsers(dest="command", help="Command to run")

    # Generate schedule command
    gen_parser = subparsers.add_parser("generate", help="Generate a schedule")
    gen_parser.add_argument("--days", type=int, default=1, help="Number of days")
    gen_parser.add_argument("--output", type=str, help="Output file path")

    # Run script command
    script_parser = subparsers.add_parser("script", help="Run a conversation script")
    script_parser.add_argument(
        "--name",
        choices=[s.name for s in CONVERSATION_SCRIPTS],
        required=True,
        help="Script name"
    )
    script_parser.add_argument("--immediate", action="store_true", help="Run immediately")

    # Preview command
    preview_parser = subparsers.add_parser("preview", help="Preview daily schedule")

    # Start daemon command
    daemon_parser = subparsers.add_parser("daemon", help="Start scheduler daemon")

    args = parser.parse_args()

    scheduler = LimnScheduler(dry_run=True)

    if args.command == "generate":
        if args.days == 1:
            schedule = scheduler.generate_daily_schedule()
        else:
            schedule = scheduler.generate_week_schedule()

        if args.output:
            scheduler.export_schedule(schedule, args.output)
        else:
            print(json.dumps(schedule, indent=2, default=str))

    elif args.command == "script":
        script = next(s for s in CONVERSATION_SCRIPTS if s.name == args.name)
        posts = scheduler.run_conversation_script(script, immediate=args.immediate)
        for post in posts:
            print(f"Turn {post['turn']}: @limn_{post['bot']}")
            print(f"  {post['limn']}")
            print()

    elif args.command == "preview":
        schedule = scheduler.generate_daily_schedule()
        print(f"\n{'=' * 60}")
        print(f"DAILY SCHEDULE - {datetime.now().strftime('%Y-%m-%d')}")
        print(f"{'=' * 60}\n")

        for item in schedule:
            print(f"{item['time']} - @limn_{item['bot']} ({item['type']})")
            print(f"  Limn: {item['limn']}")
            print(f"  {item['interpretation'][:80]}...")
            print()

    elif args.command == "daemon":
        print("Starting scheduler daemon...")
        scheduler.start_scheduler()
        try:
            while True:
                time.sleep(60)
        except KeyboardInterrupt:
            scheduler.stop_scheduler()
            print("Daemon stopped")

    else:
        parser.print_help()


if __name__ == "__main__":
    main()
