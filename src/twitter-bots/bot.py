"""
Limn Twitter Bot Framework

This module provides the framework for running Limn-speaking Twitter bots.
Each bot has a "key" (domain/personality) that colors its interpretations.

Requirements:
- tweepy >= 4.0
- anthropic (for Claude API)
- python-dotenv

Setup:
1. Create .env file with:
   TWITTER_API_KEY=...
   TWITTER_API_SECRET=...
   TWITTER_ACCESS_TOKEN=...
   TWITTER_ACCESS_TOKEN_SECRET=...
   TWITTER_BEARER_TOKEN=...
   ANTHROPIC_API_KEY=...

2. Configure bot personas in config.json

Usage:
   python bot.py --persona observer --action post
   python bot.py --persona gardener --action respond
   python bot.py --persona merchant --action converse
"""

import os
import json
import random
import argparse
from datetime import datetime
from pathlib import Path

# Uncomment when ready to use:
# import tweepy
# from anthropic import Anthropic
# from dotenv import load_dotenv

# load_dotenv()


class LimnBot:
    """A Twitter bot that speaks Limn with a specific key/persona."""

    def __init__(self, persona_name: str, config_path: str = "config.json"):
        self.persona_name = persona_name
        self.config = self._load_config(config_path)
        self.persona = self.config["personas"][persona_name]

        # Twitter client (uncomment when ready)
        # self.twitter = self._init_twitter()

        # Claude client for generating interpretations
        # self.claude = Anthropic()

    def _load_config(self, path: str) -> dict:
        """Load bot configuration."""
        config_file = Path(path)
        if config_file.exists():
            with open(config_file) as f:
                return json.load(f)
        return self._default_config()

    def _default_config(self) -> dict:
        """Default configuration if no config file exists."""
        return {
            "personas": {
                "observer": {
                    "handle": "@limn_observer",
                    "key": "cosmology, physics, astronomy, vast scales, deep time",
                    "voice": "contemplative, awestruck, questioning",
                    "bio": "watching. interpreting. wondering. | sol liq tra | sometimes I understand"
                },
                "gardener": {
                    "handle": "@limn_gardener",
                    "key": "biology, growth, decay, cycles, nature, patience",
                    "voice": "patient, nurturing, comfortable with impermanence",
                    "bio": "gro dec cyc | everything returns | patient observer of small changes"
                },
                "merchant": {
                    "handle": "@limn_merchant",
                    "key": "economics, exchange, value, trade, markets, calculation",
                    "voice": "pragmatic, calculating, slightly cynical",
                    "bio": "prc val tra | everything has a rate | reading the flow"
                },
                "void": {
                    "handle": "@limn_void",
                    "key": "negation, absence, paradox, philosophy, emptiness",
                    "voice": "cryptic, philosophical, finds meaning in absence",
                    "bio": "nu nu nu | what isn't is | listening to silence"
                }
            },
            "content_library": "content-library.md",
            "vocabulary": "../../docs/spec/vocabulary-v2.md",
            "bootstrap": "../../docs/spec/bootstrap-v2.md"
        }

    def _init_twitter(self):
        """Initialize Twitter API client."""
        # Uncomment when ready:
        # return tweepy.Client(
        #     bearer_token=os.getenv("TWITTER_BEARER_TOKEN"),
        #     consumer_key=os.getenv("TWITTER_API_KEY"),
        #     consumer_secret=os.getenv("TWITTER_API_SECRET"),
        #     access_token=os.getenv("TWITTER_ACCESS_TOKEN"),
        #     access_token_secret=os.getenv("TWITTER_ACCESS_TOKEN_SECRET")
        # )
        pass

    def generate_post(self, limn_sentence: str = None) -> dict:
        """
        Generate a post with Limn sentence and interpretation.

        If no sentence provided, generates one based on persona's themes.
        Returns dict with 'limn' and 'interpretation' keys.
        """
        if limn_sentence is None:
            limn_sentence = self._generate_limn_sentence()

        interpretation = self._interpret_with_key(limn_sentence)

        return {
            "limn": limn_sentence,
            "interpretation": interpretation,
            "formatted": self._format_tweet(limn_sentence, interpretation)
        }

    def _generate_limn_sentence(self) -> str:
        """Generate a thematically appropriate Limn sentence."""
        # For now, return from pre-written library
        # TODO: Use Claude to generate novel sentences

        sentences = [
            "gro exp bri far",
            "hot col bet mov",
            "lif dea tra cyc",
            "sol bel | liq abo",
            "min her | mag the",
            "pas now fut pas",
            "sel oth bet joi",
            "kno gro | mys dec",
            "say lis und",
            "cyc dur | lin mom"
        ]
        return random.choice(sentences)

    def _interpret_with_key(self, limn_sentence: str) -> str:
        """
        Interpret a Limn sentence using this bot's key.

        Uses Claude API to generate interpretation consistent with persona.
        """
        prompt = f"""You are interpreting a Limn sentence. Limn is a language where
words are constraints and meaning is their intersection.

Your key (interpretive lens): {self.persona['key']}
Your voice: {self.persona['voice']}

Limn sentence: {limn_sentence}

Provide a short interpretation (2-4 lines) in your voice. Use lowercase.
Include both the semantic interpretation and an emotional/philosophical reflection.
Format: interpretation first, then reflection in a new line.

Example format:
heat and cold, between them: movement. convection.
the universe stirs itself.
"""

        # Uncomment when ready:
        # response = self.claude.messages.create(
        #     model="claude-sonnet-4-20250514",
        #     max_tokens=150,
        #     messages=[{"role": "user", "content": prompt}]
        # )
        # return response.content[0].text

        # Placeholder for testing:
        return f"[interpretation through {self.persona_name} key would go here]"

    def _format_tweet(self, limn: str, interpretation: str) -> str:
        """Format the final tweet with Limn + interpretation."""
        return f"{limn}\n\n[{interpretation}]"

    def post(self, content: str = None):
        """Post a tweet."""
        if content is None:
            post_data = self.generate_post()
            content = post_data["formatted"]

        print(f"[{self.persona['handle']}] Would post:")
        print(content)
        print("---")

        # Uncomment when ready:
        # self.twitter.create_tweet(text=content)

        return content

    def respond_to(self, tweet_id: str, original_limn: str):
        """Respond to another bot's tweet with this persona's interpretation."""
        interpretation = self._interpret_with_key(original_limn)
        response = f"@{self._get_author(tweet_id)}\n{original_limn}\n\n[{interpretation}]"

        print(f"[{self.persona['handle']}] Would respond:")
        print(response)
        print("---")

        # Uncomment when ready:
        # self.twitter.create_tweet(text=response, in_reply_to_tweet_id=tweet_id)

        return response

    def _get_author(self, tweet_id: str) -> str:
        """Get the author of a tweet."""
        # Placeholder
        return "other_bot"


class ConversationOrchestrator:
    """Orchestrates multi-bot conversations."""

    def __init__(self, bot_names: list[str]):
        self.bots = {name: LimnBot(name) for name in bot_names}

    def run_conversation(self, starter_sentence: str, num_turns: int = 4):
        """
        Run a scripted conversation between bots.

        Each bot interprets the same sentence through their key,
        creating divergent meanings from identical text.
        """
        print(f"=== Conversation about: {starter_sentence} ===\n")

        for i, (name, bot) in enumerate(self.bots.items()):
            if i >= num_turns:
                break

            post = bot.generate_post(starter_sentence)
            print(f"Turn {i+1} - {name}:")
            print(post["formatted"])
            print()

    def schedule_day(self, date: datetime = None):
        """Generate a day's worth of scheduled posts."""
        if date is None:
            date = datetime.now()

        schedule = []
        times = ["09:00", "12:00", "15:00", "18:00", "21:00"]

        for i, time in enumerate(times):
            bot_name = list(self.bots.keys())[i % len(self.bots)]
            bot = self.bots[bot_name]
            post = bot.generate_post()

            schedule.append({
                "time": f"{date.strftime('%Y-%m-%d')} {time}",
                "bot": bot_name,
                "content": post["formatted"]
            })

        return schedule


def main():
    parser = argparse.ArgumentParser(description="Limn Twitter Bot")
    parser.add_argument("--persona", choices=["observer", "gardener", "merchant", "void"],
                       default="observer", help="Bot persona to use")
    parser.add_argument("--action", choices=["post", "respond", "converse", "schedule"],
                       default="post", help="Action to perform")
    parser.add_argument("--sentence", type=str, help="Limn sentence to interpret")
    parser.add_argument("--tweet-id", type=str, help="Tweet ID to respond to")

    args = parser.parse_args()

    if args.action == "post":
        bot = LimnBot(args.persona)
        bot.post()

    elif args.action == "respond":
        if not args.tweet_id or not args.sentence:
            print("--tweet-id and --sentence required for respond action")
            return
        bot = LimnBot(args.persona)
        bot.respond_to(args.tweet_id, args.sentence)

    elif args.action == "converse":
        orchestrator = ConversationOrchestrator(["observer", "gardener", "merchant", "void"])
        sentence = args.sentence or "gro exp bri far"
        orchestrator.run_conversation(sentence)

    elif args.action == "schedule":
        orchestrator = ConversationOrchestrator(["observer", "gardener", "merchant", "void"])
        schedule = orchestrator.schedule_day()
        for item in schedule:
            print(f"{item['time']} - @limn_{item['bot']}:")
            print(item['content'])
            print()


if __name__ == "__main__":
    main()
