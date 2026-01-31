"""
Limn Twitter Bot Framework

Core framework for running Limn-speaking Twitter bots using Twitter API v2.
Handles authentication, posting, reply handling, rate limiting, and logging.

Usage:
    from bot_framework import LimnBot, BotOrchestrator

    # Single bot operation
    bot = LimnBot("observer")
    bot.post()

    # Multi-bot orchestration
    orchestrator = BotOrchestrator()
    orchestrator.run_conversation("gro exp bri far")
"""

import os
import time
import json
import sqlite3
import logging
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict, List, Optional, Tuple, Any
from dataclasses import dataclass, field
from contextlib import contextmanager
from abc import ABC, abstractmethod

# Third-party imports (lazy loaded for flexibility)
try:
    import tweepy
    TWEEPY_AVAILABLE = True
except ImportError:
    TWEEPY_AVAILABLE = False

try:
    from anthropic import Anthropic
    ANTHROPIC_AVAILABLE = True
except ImportError:
    ANTHROPIC_AVAILABLE = False

from personas import Persona, get_persona, get_all_personas, PERSONAS
from content_generator import ContentGenerator


# =============================================================================
# Logging Configuration
# =============================================================================

def setup_logging(log_level: str = "INFO", log_dir: str = "./logs") -> logging.Logger:
    """Configure logging for the bot framework."""
    Path(log_dir).mkdir(parents=True, exist_ok=True)

    log_file = Path(log_dir) / f"limn_bots_{datetime.now().strftime('%Y%m%d')}.log"

    logging.basicConfig(
        level=getattr(logging, log_level.upper()),
        format="%(asctime)s [%(levelname)s] %(name)s: %(message)s",
        handlers=[
            logging.FileHandler(log_file),
            logging.StreamHandler()
        ]
    )

    return logging.getLogger("limn_bots")


logger = setup_logging()


# =============================================================================
# Rate Limiter
# =============================================================================

@dataclass
class RateLimitState:
    """Tracks rate limit state for API calls."""
    requests_made: int = 0
    window_start: datetime = field(default_factory=datetime.now)
    window_duration: timedelta = field(default_factory=lambda: timedelta(minutes=15))
    max_requests: int = 50

    def can_make_request(self) -> bool:
        """Check if a request can be made within rate limits."""
        self._maybe_reset_window()
        return self.requests_made < self.max_requests

    def record_request(self):
        """Record that a request was made."""
        self._maybe_reset_window()
        self.requests_made += 1

    def wait_time(self) -> float:
        """Get seconds to wait before next request is allowed."""
        if self.can_make_request():
            return 0.0
        window_end = self.window_start + self.window_duration
        return max(0.0, (window_end - datetime.now()).total_seconds())

    def _maybe_reset_window(self):
        """Reset the window if it has expired."""
        if datetime.now() >= self.window_start + self.window_duration:
            self.requests_made = 0
            self.window_start = datetime.now()


class RateLimiter:
    """
    Manages rate limiting across multiple endpoints.

    Twitter API v2 rate limits:
    - Tweet creation: 200 per 15 min (per user)
    - Mentions timeline: 450 per 15 min
    - User lookup: 900 per 15 min
    """

    def __init__(self, limits: Optional[Dict[str, int]] = None):
        self.limits = limits or {
            "tweet": 50,  # Conservative
            "mentions": 450,
            "user_lookup": 900,
        }
        self.states: Dict[str, RateLimitState] = {}

    def get_state(self, endpoint: str) -> RateLimitState:
        """Get or create rate limit state for an endpoint."""
        if endpoint not in self.states:
            max_requests = self.limits.get(endpoint, 100)
            self.states[endpoint] = RateLimitState(max_requests=max_requests)
        return self.states[endpoint]

    def can_request(self, endpoint: str) -> bool:
        """Check if a request to endpoint is allowed."""
        return self.get_state(endpoint).can_make_request()

    def record(self, endpoint: str):
        """Record a request to an endpoint."""
        self.get_state(endpoint).record_request()

    def wait_if_needed(self, endpoint: str):
        """Wait if rate limited, then record the request."""
        state = self.get_state(endpoint)
        wait_time = state.wait_time()
        if wait_time > 0:
            logger.info(f"Rate limited on {endpoint}, waiting {wait_time:.1f}s")
            time.sleep(wait_time)
        state.record_request()


# =============================================================================
# Database for Tracking Posts
# =============================================================================

class PostDatabase:
    """SQLite database for tracking posted content and conversations."""

    def __init__(self, db_path: str = "./data/limn_bots.db"):
        self.db_path = Path(db_path)
        self.db_path.parent.mkdir(parents=True, exist_ok=True)
        self._init_db()

    def _init_db(self):
        """Initialize database schema."""
        with self._connection() as conn:
            conn.executescript("""
                CREATE TABLE IF NOT EXISTS posts (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    tweet_id TEXT UNIQUE,
                    bot_name TEXT NOT NULL,
                    limn_sentence TEXT NOT NULL,
                    interpretation TEXT,
                    full_text TEXT NOT NULL,
                    posted_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    reply_to_id TEXT,
                    reply_to_bot TEXT,
                    conversation_id TEXT,
                    engagement_likes INTEGER DEFAULT 0,
                    engagement_retweets INTEGER DEFAULT 0,
                    engagement_replies INTEGER DEFAULT 0
                );

                CREATE TABLE IF NOT EXISTS conversations (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    conversation_id TEXT UNIQUE,
                    starter_sentence TEXT NOT NULL,
                    started_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    status TEXT DEFAULT 'active',
                    participant_bots TEXT
                );

                CREATE TABLE IF NOT EXISTS mentions (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    tweet_id TEXT UNIQUE,
                    author_id TEXT,
                    author_username TEXT,
                    text TEXT,
                    received_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    responded BOOLEAN DEFAULT FALSE,
                    response_tweet_id TEXT
                );

                CREATE INDEX IF NOT EXISTS idx_posts_bot ON posts(bot_name);
                CREATE INDEX IF NOT EXISTS idx_posts_conversation ON posts(conversation_id);
                CREATE INDEX IF NOT EXISTS idx_mentions_responded ON mentions(responded);
            """)

    @contextmanager
    def _connection(self):
        """Context manager for database connections."""
        conn = sqlite3.connect(self.db_path)
        conn.row_factory = sqlite3.Row
        try:
            yield conn
            conn.commit()
        finally:
            conn.close()

    def record_post(
        self,
        bot_name: str,
        limn_sentence: str,
        interpretation: str,
        full_text: str,
        tweet_id: Optional[str] = None,
        reply_to_id: Optional[str] = None,
        reply_to_bot: Optional[str] = None,
        conversation_id: Optional[str] = None
    ) -> int:
        """Record a posted tweet."""
        with self._connection() as conn:
            cursor = conn.execute("""
                INSERT INTO posts (
                    tweet_id, bot_name, limn_sentence, interpretation,
                    full_text, reply_to_id, reply_to_bot, conversation_id
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
            """, (tweet_id, bot_name, limn_sentence, interpretation,
                  full_text, reply_to_id, reply_to_bot, conversation_id))
            return cursor.lastrowid

    def get_recent_posts(
        self,
        bot_name: Optional[str] = None,
        hours: int = 24,
        limit: int = 100
    ) -> List[Dict]:
        """Get recent posts, optionally filtered by bot."""
        with self._connection() as conn:
            if bot_name:
                rows = conn.execute("""
                    SELECT * FROM posts
                    WHERE bot_name = ? AND posted_at > datetime('now', '-' || ? || ' hours')
                    ORDER BY posted_at DESC LIMIT ?
                """, (bot_name, hours, limit)).fetchall()
            else:
                rows = conn.execute("""
                    SELECT * FROM posts
                    WHERE posted_at > datetime('now', '-' || ? || ' hours')
                    ORDER BY posted_at DESC LIMIT ?
                """, (hours, limit)).fetchall()
            return [dict(row) for row in rows]

    def record_mention(
        self,
        tweet_id: str,
        author_id: str,
        author_username: str,
        text: str
    ) -> int:
        """Record an incoming mention."""
        with self._connection() as conn:
            cursor = conn.execute("""
                INSERT OR IGNORE INTO mentions (tweet_id, author_id, author_username, text)
                VALUES (?, ?, ?, ?)
            """, (tweet_id, author_id, author_username, text))
            return cursor.lastrowid

    def get_unresponded_mentions(self, limit: int = 10) -> List[Dict]:
        """Get mentions that haven't been responded to."""
        with self._connection() as conn:
            rows = conn.execute("""
                SELECT * FROM mentions
                WHERE responded = FALSE
                ORDER BY received_at ASC LIMIT ?
            """, (limit,)).fetchall()
            return [dict(row) for row in rows]

    def mark_mention_responded(self, mention_id: int, response_tweet_id: str):
        """Mark a mention as responded to."""
        with self._connection() as conn:
            conn.execute("""
                UPDATE mentions
                SET responded = TRUE, response_tweet_id = ?
                WHERE id = ?
            """, (response_tweet_id, mention_id))


# =============================================================================
# Twitter Client Wrapper
# =============================================================================

class TwitterClient:
    """
    Wrapper for Twitter API v2 operations.

    Handles authentication and provides a clean interface for bot operations.
    """

    def __init__(
        self,
        bearer_token: str,
        api_key: str,
        api_secret: str,
        access_token: str,
        access_token_secret: str,
        dry_run: bool = True
    ):
        """
        Initialize Twitter client.

        Args:
            bearer_token: Twitter API bearer token
            api_key: Twitter API key
            api_secret: Twitter API secret
            access_token: User access token
            access_token_secret: User access token secret
            dry_run: If True, don't actually post to Twitter
        """
        self.dry_run = dry_run

        if not TWEEPY_AVAILABLE:
            logger.warning("tweepy not installed, running in simulation mode")
            self.client = None
            return

        if dry_run:
            logger.info("Running in dry-run mode, tweets will not be posted")
            self.client = None
            return

        self.client = tweepy.Client(
            bearer_token=bearer_token,
            consumer_key=api_key,
            consumer_secret=api_secret,
            access_token=access_token,
            access_token_secret=access_token_secret
        )

    def post_tweet(self, text: str, reply_to_id: Optional[str] = None) -> Optional[str]:
        """
        Post a tweet.

        Args:
            text: Tweet text
            reply_to_id: Optional tweet ID to reply to

        Returns:
            Tweet ID if posted, None if dry run or error
        """
        if self.dry_run or not self.client:
            logger.info(f"[DRY RUN] Would post: {text[:100]}...")
            return f"dry_run_{int(time.time())}"

        try:
            response = self.client.create_tweet(
                text=text,
                in_reply_to_tweet_id=reply_to_id
            )
            tweet_id = response.data["id"]
            logger.info(f"Posted tweet {tweet_id}")
            return tweet_id
        except Exception as e:
            logger.error(f"Failed to post tweet: {e}")
            return None

    def get_mentions(
        self,
        since_id: Optional[str] = None,
        max_results: int = 100
    ) -> List[Dict]:
        """
        Get recent mentions of the authenticated user.

        Args:
            since_id: Only get mentions after this tweet ID
            max_results: Maximum number of mentions to retrieve

        Returns:
            List of mention dictionaries
        """
        if self.dry_run or not self.client:
            logger.info("[DRY RUN] Would fetch mentions")
            return []

        try:
            response = self.client.get_users_mentions(
                id=self.client.get_me().data.id,
                since_id=since_id,
                max_results=max_results,
                expansions=["author_id"],
                tweet_fields=["created_at", "text", "author_id"]
            )

            if not response.data:
                return []

            mentions = []
            for tweet in response.data:
                mentions.append({
                    "id": tweet.id,
                    "text": tweet.text,
                    "author_id": tweet.author_id,
                    "created_at": tweet.created_at,
                })

            return mentions

        except Exception as e:
            logger.error(f"Failed to fetch mentions: {e}")
            return []

    def get_tweet(self, tweet_id: str) -> Optional[Dict]:
        """Get a single tweet by ID."""
        if self.dry_run or not self.client:
            return None

        try:
            response = self.client.get_tweet(
                tweet_id,
                expansions=["author_id"],
                tweet_fields=["created_at", "text"]
            )
            if response.data:
                return {
                    "id": response.data.id,
                    "text": response.data.text,
                    "author_id": response.data.author_id,
                }
            return None
        except Exception as e:
            logger.error(f"Failed to get tweet {tweet_id}: {e}")
            return None


# =============================================================================
# Main Bot Class
# =============================================================================

class LimnBot:
    """
    A Twitter bot that speaks Limn with a specific persona/key.

    The bot can:
    - Generate and post original Limn content
    - Reply to other bots with different interpretations
    - Respond to human mentions
    - Track its posting history
    """

    def __init__(
        self,
        persona_name: str,
        twitter_client: Optional[TwitterClient] = None,
        anthropic_client: Optional[Any] = None,
        database: Optional[PostDatabase] = None,
        rate_limiter: Optional[RateLimiter] = None,
        dry_run: bool = True
    ):
        """
        Initialize a Limn bot.

        Args:
            persona_name: Name of the persona to use
            twitter_client: Optional pre-configured Twitter client
            anthropic_client: Optional pre-configured Anthropic client
            database: Optional pre-configured database
            rate_limiter: Optional pre-configured rate limiter
            dry_run: If True, don't actually post to Twitter
        """
        self.persona = get_persona(persona_name)
        self.dry_run = dry_run

        # Initialize components
        self.twitter = twitter_client
        self.anthropic = anthropic_client
        self.database = database or PostDatabase()
        self.rate_limiter = rate_limiter or RateLimiter()

        # Content generator
        self.content_generator = ContentGenerator(self.anthropic)

        logger.info(f"Initialized LimnBot: {self.persona.handle}")

    @classmethod
    def from_config(cls, persona_name: str, config_module) -> "LimnBot":
        """
        Create a bot from a configuration module.

        Args:
            persona_name: Name of the persona
            config_module: Module containing configuration (e.g., imported config.py)

        Returns:
            Configured LimnBot instance
        """
        dry_run = not getattr(config_module, "ENABLE_LIVE_POSTING", False)

        # Initialize Twitter client if credentials available
        twitter_client = None
        if TWEEPY_AVAILABLE and hasattr(config_module, "BOT_CREDENTIALS"):
            creds = config_module.BOT_CREDENTIALS.get(persona_name, {})
            if creds:
                twitter_client = TwitterClient(
                    bearer_token=config_module.TWITTER_BEARER_TOKEN,
                    api_key=config_module.TWITTER_API_KEY,
                    api_secret=config_module.TWITTER_API_SECRET,
                    access_token=creds["access_token"],
                    access_token_secret=creds["access_token_secret"],
                    dry_run=dry_run
                )

        # Initialize Anthropic client if available
        anthropic_client = None
        if ANTHROPIC_AVAILABLE and hasattr(config_module, "ANTHROPIC_API_KEY"):
            anthropic_client = Anthropic(api_key=config_module.ANTHROPIC_API_KEY)

        # Initialize database
        db_path = getattr(config_module, "DATABASE_PATH", "./data/limn_bots.db")
        database = PostDatabase(db_path)

        return cls(
            persona_name=persona_name,
            twitter_client=twitter_client,
            anthropic_client=anthropic_client,
            database=database,
            dry_run=dry_run
        )

    def generate_post(
        self,
        theme: Optional[str] = None,
        limn_sentence: Optional[str] = None
    ) -> Dict[str, str]:
        """
        Generate a post with Limn sentence and interpretation.

        Args:
            theme: Optional theme to focus on
            limn_sentence: Optional specific sentence to interpret

        Returns:
            Dict with 'limn', 'interpretation', and 'formatted' keys
        """
        if limn_sentence:
            interpretation = self.content_generator.interpret_sentence(
                self.persona, limn_sentence
            )
        else:
            limn_sentence, interpretation = self.content_generator.generate_standalone_post(
                self.persona, theme, use_ai=bool(self.anthropic)
            )

        formatted = self.content_generator.format_tweet(limn_sentence, interpretation)

        return {
            "limn": limn_sentence,
            "interpretation": interpretation,
            "formatted": formatted
        }

    def post(
        self,
        theme: Optional[str] = None,
        limn_sentence: Optional[str] = None
    ) -> Optional[str]:
        """
        Generate and post a tweet.

        Args:
            theme: Optional theme to focus on
            limn_sentence: Optional specific sentence to use

        Returns:
            Tweet ID if posted, None otherwise
        """
        # Check rate limit
        self.rate_limiter.wait_if_needed("tweet")

        # Generate content
        post_data = self.generate_post(theme, limn_sentence)

        logger.info(f"[{self.persona.handle}] Posting: {post_data['limn']}")

        # Post to Twitter
        tweet_id = None
        if self.twitter:
            tweet_id = self.twitter.post_tweet(post_data["formatted"])

        # Record in database
        self.database.record_post(
            bot_name=self.persona.name,
            limn_sentence=post_data["limn"],
            interpretation=post_data["interpretation"],
            full_text=post_data["formatted"],
            tweet_id=tweet_id
        )

        return tweet_id

    def reply_to(
        self,
        tweet_id: str,
        original_limn: str,
        original_persona: Optional[str] = None
    ) -> Optional[str]:
        """
        Reply to another tweet with this persona's interpretation.

        Args:
            tweet_id: ID of tweet to reply to
            original_limn: The Limn sentence to reinterpret
            original_persona: Name of the original poster's persona (if bot)

        Returns:
            Reply tweet ID if posted, None otherwise
        """
        # Check rate limit
        self.rate_limiter.wait_if_needed("tweet")

        # Get original persona if specified
        orig_persona = get_persona(original_persona) if original_persona else None

        # Generate reply
        reply_limn, interpretation = self.content_generator.generate_reply(
            self.persona, original_limn, orig_persona
        )

        # Format with mention
        mention = f"@limn_{original_persona}" if original_persona else ""
        formatted = self.content_generator.format_tweet(
            reply_limn, interpretation, mention
        )

        logger.info(f"[{self.persona.handle}] Replying to {tweet_id}: {reply_limn}")

        # Post reply
        reply_id = None
        if self.twitter:
            reply_id = self.twitter.post_tweet(formatted, reply_to_id=tweet_id)

        # Record in database
        self.database.record_post(
            bot_name=self.persona.name,
            limn_sentence=reply_limn,
            interpretation=interpretation,
            full_text=formatted,
            tweet_id=reply_id,
            reply_to_id=tweet_id,
            reply_to_bot=original_persona
        )

        return reply_id

    def process_mentions(self, limit: int = 10) -> int:
        """
        Process and respond to unhandled mentions.

        Args:
            limit: Maximum number of mentions to process

        Returns:
            Number of mentions processed
        """
        if not self.twitter:
            logger.info("No Twitter client, skipping mention processing")
            return 0

        # Get new mentions
        self.rate_limiter.wait_if_needed("mentions")
        mentions = self.twitter.get_mentions(max_results=limit)

        processed = 0
        for mention in mentions:
            # Record mention
            self.database.record_mention(
                tweet_id=mention["id"],
                author_id=mention["author_id"],
                author_username=mention.get("author_username", "unknown"),
                text=mention["text"]
            )

            # Generate and post response
            # TODO: Parse Limn from mention text or use template response
            self.rate_limiter.wait_if_needed("tweet")

            response = self.persona.response_templates.get(
                "to_question",
                "amb = des | cle = key + amb\n[ambiguity = design.\nclarity = key + ambiguity.]"
            )

            reply_id = self.twitter.post_tweet(response, reply_to_id=mention["id"])

            if reply_id:
                self.database.mark_mention_responded(mention["id"], reply_id)
                processed += 1

        return processed

    def get_recent_posts(self, hours: int = 24) -> List[Dict]:
        """Get this bot's recent posts."""
        return self.database.get_recent_posts(self.persona.name, hours)


# =============================================================================
# Bot Orchestrator
# =============================================================================

class BotOrchestrator:
    """
    Orchestrates multi-bot conversations and coordinated posting.

    Manages multiple LimnBot instances and coordinates their interactions
    to create organic-feeling conversations.
    """

    def __init__(
        self,
        bot_names: Optional[List[str]] = None,
        dry_run: bool = True,
        config_module=None
    ):
        """
        Initialize the orchestrator.

        Args:
            bot_names: List of persona names to include
            dry_run: If True, don't actually post
            config_module: Optional configuration module
        """
        self.dry_run = dry_run
        self.config = config_module

        # Initialize bots
        bot_names = bot_names or list(PERSONAS.keys())
        self.bots: Dict[str, LimnBot] = {}

        for name in bot_names:
            if config_module:
                self.bots[name] = LimnBot.from_config(name, config_module)
            else:
                self.bots[name] = LimnBot(name, dry_run=dry_run)

        # Shared database
        self.database = PostDatabase()

        logger.info(f"Orchestrator initialized with bots: {list(self.bots.keys())}")

    def run_conversation(
        self,
        starter_sentence: str,
        participants: Optional[List[str]] = None,
        num_turns: int = 4,
        delay_seconds: float = 0
    ) -> List[Dict]:
        """
        Run a scripted conversation between bots.

        Each bot interprets the same sentence through their key,
        creating divergent meanings from identical text.

        Args:
            starter_sentence: The Limn sentence to discuss
            participants: List of bot names to include (default: all)
            num_turns: Number of turns in the conversation
            delay_seconds: Seconds to wait between posts (0 for immediate)

        Returns:
            List of post records
        """
        participants = participants or list(self.bots.keys())
        posts = []

        logger.info(f"Starting conversation about: {starter_sentence}")

        # First bot posts the original
        first_bot = self.bots[participants[0]]
        tweet_id = first_bot.post(limn_sentence=starter_sentence)

        posts.append({
            "bot": participants[0],
            "tweet_id": tweet_id,
            "limn": starter_sentence,
            "turn": 0
        })

        # Other bots reply
        for i, bot_name in enumerate(participants[1:num_turns], 1):
            if delay_seconds > 0:
                time.sleep(delay_seconds)

            bot = self.bots[bot_name]

            # Reply to the first post
            reply_id = bot.reply_to(
                tweet_id=tweet_id or "simulated",
                original_limn=starter_sentence,
                original_persona=participants[0]
            )

            posts.append({
                "bot": bot_name,
                "tweet_id": reply_id,
                "limn": starter_sentence,
                "turn": i
            })

        return posts

    def schedule_day(
        self,
        date: Optional[datetime] = None,
        posts_per_bot: int = 2
    ) -> List[Dict]:
        """
        Generate a day's worth of scheduled posts.

        Args:
            date: Date to schedule for (default: today)
            posts_per_bot: Number of posts per bot

        Returns:
            List of scheduled post dictionaries
        """
        date = date or datetime.now()
        schedule = []

        # Base times for posts
        base_times = ["09:00", "12:00", "15:00", "18:00", "21:00"]

        # Distribute posts throughout the day
        all_posts = []
        for bot_name, bot in self.bots.items():
            for i in range(posts_per_bot):
                time_idx = (hash(bot_name) + i) % len(base_times)
                post_time = base_times[time_idx]

                post_data = bot.generate_post()
                all_posts.append({
                    "time": f"{date.strftime('%Y-%m-%d')} {post_time}",
                    "bot": bot_name,
                    "limn": post_data["limn"],
                    "interpretation": post_data["interpretation"],
                    "formatted": post_data["formatted"]
                })

        # Sort by time
        all_posts.sort(key=lambda x: x["time"])

        return all_posts

    def print_schedule(self, schedule: List[Dict]):
        """Print a formatted schedule."""
        print("\n" + "=" * 60)
        print("SCHEDULED POSTS")
        print("=" * 60)

        for post in schedule:
            print(f"\n{post['time']} - @limn_{post['bot']}:")
            print(post["formatted"])
            print("-" * 40)


# =============================================================================
# CLI Interface
# =============================================================================

def main():
    """Command-line interface for the bot framework."""
    import argparse

    parser = argparse.ArgumentParser(
        description="Limn Twitter Bot Framework",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python bot_framework.py --persona observer --action post
  python bot_framework.py --persona gardener --action respond --sentence "gro exp bri far"
  python bot_framework.py --action converse --sentence "lif dea tra cyc"
  python bot_framework.py --action schedule
        """
    )

    parser.add_argument(
        "--persona",
        choices=list(PERSONAS.keys()),
        default="observer",
        help="Bot persona to use (default: observer)"
    )
    parser.add_argument(
        "--action",
        choices=["post", "respond", "converse", "schedule", "interpret"],
        default="post",
        help="Action to perform (default: post)"
    )
    parser.add_argument(
        "--sentence",
        type=str,
        help="Limn sentence to use"
    )
    parser.add_argument(
        "--theme",
        type=str,
        help="Theme for content generation"
    )
    parser.add_argument(
        "--tweet-id",
        type=str,
        help="Tweet ID to respond to"
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        default=True,
        help="Don't actually post to Twitter (default: True)"
    )
    parser.add_argument(
        "--live",
        action="store_true",
        help="Actually post to Twitter (requires config.py)"
    )

    args = parser.parse_args()

    dry_run = not args.live

    if args.action == "post":
        bot = LimnBot(args.persona, dry_run=dry_run)
        post_data = bot.generate_post(args.theme, args.sentence)
        print(f"\n[@limn_{args.persona}] Generated post:")
        print("-" * 40)
        print(post_data["formatted"])
        print("-" * 40)

        if not dry_run:
            tweet_id = bot.post(args.theme, args.sentence)
            print(f"Posted as tweet {tweet_id}")

    elif args.action == "interpret":
        if not args.sentence:
            print("Error: --sentence required for interpret action")
            return

        bot = LimnBot(args.persona, dry_run=dry_run)
        post_data = bot.generate_post(limn_sentence=args.sentence)
        print(f"\n[@limn_{args.persona}] Interpretation:")
        print("-" * 40)
        print(post_data["formatted"])
        print("-" * 40)

    elif args.action == "respond":
        if not args.sentence:
            print("Error: --sentence required for respond action")
            return

        bot = LimnBot(args.persona, dry_run=dry_run)
        tweet_id = args.tweet_id or "simulated_tweet"
        reply_id = bot.reply_to(tweet_id, args.sentence)
        print(f"Reply {'would be ' if dry_run else ''}posted")

    elif args.action == "converse":
        orchestrator = BotOrchestrator(dry_run=dry_run)
        sentence = args.sentence or "gro exp bri far"

        print(f"\n=== Conversation about: {sentence} ===\n")

        # Just print the conversation without actually posting
        for name, bot in orchestrator.bots.items():
            post_data = bot.generate_post(limn_sentence=sentence)
            print(f"@limn_{name}:")
            print(post_data["formatted"])
            print()

    elif args.action == "schedule":
        orchestrator = BotOrchestrator(dry_run=dry_run)
        schedule = orchestrator.schedule_day()
        orchestrator.print_schedule(schedule)


if __name__ == "__main__":
    main()
