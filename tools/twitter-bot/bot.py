#!/usr/bin/env python3
"""Limn Twitter Bot - Posts Limn expressions with interpretations.

Generates and posts diverse Limn content: daily words, compositions,
poetry fragments, operator examples, and interpretive threads.

Usage:
    python bot.py post           # Post one tweet (random type)
    python bot.py post --type=daily_word
    python bot.py post --dry-run # Generate without posting
    python bot.py schedule       # Run on schedule (every 6 hours)
    python bot.py thread         # Post a multi-tweet thread

Environment variables:
    TWITTER_API_KEY        - Twitter API key
    TWITTER_API_SECRET     - Twitter API secret
    TWITTER_ACCESS_TOKEN   - Access token
    TWITTER_ACCESS_SECRET  - Access token secret
    TWITTER_BEARER_TOKEN   - Bearer token (for v2 API)
    LIMN_BOT_DRY_RUN      - Set to "1" for dry-run mode
"""

import argparse
import json
import os
import random
import subprocess
import sys
import time
from pathlib import Path

# Content generators
from generators import (
    generate_daily_word,
    generate_composition,
    generate_operator_example,
    generate_poetry_fragment,
    generate_interpretation,
    generate_thread,
)

BOT_DIR = Path(__file__).parent
STATE_FILE = BOT_DIR / "state.json"


def load_state() -> dict:
    """Load bot state (posted history, schedule info)."""
    if STATE_FILE.exists():
        with open(STATE_FILE) as f:
            return json.load(f)
    return {"posted": [], "last_post": None, "post_count": 0}


def save_state(state: dict):
    """Save bot state."""
    with open(STATE_FILE, "w") as f:
        json.dump(state, f, indent=2)


def get_twitter_client():
    """Create authenticated Twitter client using tweepy."""
    try:
        import tweepy
    except ImportError:
        print("Error: tweepy not installed. Run: pip install tweepy", file=sys.stderr)
        sys.exit(1)

    api_key = os.environ.get("TWITTER_API_KEY")
    api_secret = os.environ.get("TWITTER_API_SECRET")
    access_token = os.environ.get("TWITTER_ACCESS_TOKEN")
    access_secret = os.environ.get("TWITTER_ACCESS_SECRET")
    bearer_token = os.environ.get("TWITTER_BEARER_TOKEN")

    if not all([api_key, api_secret, access_token, access_secret]):
        print(
            "Error: Missing Twitter credentials. Set environment variables:\n"
            "  TWITTER_API_KEY, TWITTER_API_SECRET,\n"
            "  TWITTER_ACCESS_TOKEN, TWITTER_ACCESS_SECRET",
            file=sys.stderr,
        )
        sys.exit(1)

    # v2 client for posting
    client = tweepy.Client(
        bearer_token=bearer_token,
        consumer_key=api_key,
        consumer_secret=api_secret,
        access_token=access_token,
        access_token_secret=access_secret,
    )
    return client


def post_tweet(text: str, dry_run: bool = False, reply_to: str = None) -> str | None:
    """Post a tweet. Returns tweet ID or None."""
    if len(text) > 280:
        print(f"Warning: Tweet is {len(text)} chars (max 280)", file=sys.stderr)
        text = text[:277] + "..."

    if dry_run:
        print(f"[DRY RUN] Would post ({len(text)} chars):")
        print(text)
        print("---")
        return "dry-run-id"

    client = get_twitter_client()
    kwargs = {}
    if reply_to:
        kwargs["in_reply_to_tweet_id"] = reply_to

    response = client.create_tweet(text=text, **kwargs)
    tweet_id = response.data["id"]
    print(f"Posted tweet: {tweet_id}")
    return tweet_id


def cmd_post(args):
    """Post a single tweet."""
    dry_run = args.dry_run or os.environ.get("LIMN_BOT_DRY_RUN") == "1"
    state = load_state()

    # Pick content type
    generators = {
        "daily_word": generate_daily_word,
        "composition": generate_composition,
        "operator": generate_operator_example,
        "poetry": generate_poetry_fragment,
        "interpretation": generate_interpretation,
    }

    if args.type and args.type in generators:
        gen = generators[args.type]
    else:
        gen = random.choice(list(generators.values()))

    # Generate content
    content = gen(state)
    tweet_text = content["text"]

    # Post
    tweet_id = post_tweet(tweet_text, dry_run=dry_run)

    if tweet_id and not dry_run:
        state["posted"].append(
            {
                "id": tweet_id,
                "type": content["type"],
                "text": tweet_text,
                "timestamp": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
            }
        )
        # Keep last 100 posts
        state["posted"] = state["posted"][-100:]
        state["last_post"] = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())
        state["post_count"] = state.get("post_count", 0) + 1
        save_state(state)


def cmd_thread(args):
    """Post a multi-tweet thread."""
    dry_run = args.dry_run or os.environ.get("LIMN_BOT_DRY_RUN") == "1"
    state = load_state()

    thread = generate_thread(state)
    prev_id = None

    for i, tweet_text in enumerate(thread["tweets"]):
        tweet_id = post_tweet(tweet_text, dry_run=dry_run, reply_to=prev_id)
        prev_id = tweet_id
        if not dry_run and i < len(thread["tweets"]) - 1:
            time.sleep(2)  # Rate limit buffer

    if not dry_run:
        state["post_count"] = state.get("post_count", 0) + len(thread["tweets"])
        state["last_post"] = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())
        save_state(state)


def cmd_schedule(args):
    """Run bot on a schedule (every N hours)."""
    interval = args.interval * 3600  # Convert hours to seconds
    print(f"Starting scheduled posting every {args.interval} hours")

    while True:
        try:
            cmd_post(args)
        except Exception as e:
            print(f"Error posting: {e}", file=sys.stderr)
        time.sleep(interval)


def main():
    parser = argparse.ArgumentParser(description="Limn Twitter Bot")
    subparsers = parser.add_subparsers(dest="command", required=True)

    # post command
    post_parser = subparsers.add_parser("post", help="Post a single tweet")
    post_parser.add_argument(
        "--type",
        choices=["daily_word", "composition", "operator", "poetry", "interpretation"],
        help="Content type (default: random)",
    )
    post_parser.add_argument(
        "--dry-run", action="store_true", help="Print without posting"
    )
    post_parser.set_defaults(func=cmd_post)

    # thread command
    thread_parser = subparsers.add_parser("thread", help="Post a multi-tweet thread")
    thread_parser.add_argument(
        "--dry-run", action="store_true", help="Print without posting"
    )
    thread_parser.set_defaults(func=cmd_thread)

    # schedule command
    sched_parser = subparsers.add_parser("schedule", help="Run on schedule")
    sched_parser.add_argument(
        "--interval", type=float, default=6, help="Hours between posts (default: 6)"
    )
    sched_parser.add_argument(
        "--dry-run", action="store_true", help="Print without posting"
    )
    sched_parser.set_defaults(func=cmd_schedule)

    args = parser.parse_args()
    args.func(args)


if __name__ == "__main__":
    main()
