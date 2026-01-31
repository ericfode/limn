#!/usr/bin/env python3
"""
LimnBot - Discord bot for Limn interpretation and composition

Features:
- /limn interpret <sentence> [key] - Interpret a Limn sentence
- /limn compose "<english>" - Create Limn from English
- /limn validate <sentence> - Check if valid Limn
- /limn vocab <word|domain> - Look up vocabulary
- /limn random - Generate random sentence
- /limn poetry <theme> - Generate Limn poetry
- /limn help - Show help

Requires:
- discord.py >= 2.0
- anthropic (for Claude API)
- python-dotenv

Environment Variables:
- DISCORD_TOKEN: Bot token from Discord Developer Portal
- ANTHROPIC_API_KEY: Claude API key
"""

import os
import re
import json
import random
import logging
from pathlib import Path
from typing import Optional, List, Dict, Tuple

import discord
from discord import app_commands
from discord.ext import commands

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger('limnbot')

# Try to import Anthropic
try:
    from anthropic import Anthropic
    ANTHROPIC_AVAILABLE = True
except ImportError:
    ANTHROPIC_AVAILABLE = False
    logger.warning("anthropic not installed - AI features disabled")


# =============================================================================
# Vocabulary Database
# =============================================================================

VOCABULARY = {
    "physical": {
        "sol": "solid, rigid, fixed, hard",
        "liq": "liquid, flowing, fluid, adaptable",
        "gas": "gaseous, diffuse, ethereal, spread",
        "hot": "hot, thermal, passionate, intense",
        "col": "cold, frozen, numb, distant",
        "bri": "bright, luminous, clear, obvious",
        "dim": "dim, faint, unclear, subtle",
        "mag": "large, vast, significant, major",
        "min": "small, tiny, minimal, minor",
    },
    "spatial": {
        "abo": "above, over, superior, heaven",
        "bel": "below, under, inferior, ground",
        "bet": "between, among, middle, mediating",
        "nea": "near, close, proximal, soon",
        "far": "far, distant, remote, later",
        "ins": "inside, interior, internal, included",
        "out": "outside, exterior, external, excluded",
        "cen": "center, core, focus, essence",
        "per": "periphery, edge, margin, fringe",
    },
    "temporal": {
        "now": "present, current, immediate, actual",
        "pas": "past, history, memory, before",
        "fut": "future, potential, hope, after",
        "beg": "beginning, start, birth, genesis",
        "end": "ending, finish, death, terminus",
        "dur": "during, while, throughout, continuous",
        "cyc": "cycle, rhythm, repeat, pattern",
        "mom": "moment, instant, flash, point",
        "ete": "eternal, timeless, infinite, always",
    },
    "change": {
        "gro": "growth, increase, develop, expand",
        "dec": "decay, decrease, deteriorate, shrink",
        "tra": "transformation, change, metamorphosis",
        "mov": "movement, motion, flow, shift",
        "sta": "stability, stillness, unchanged, balanced",
        "flo": "flowing, streaming, continuous, fluid",
        "ris": "rising, ascending, improving, hope",
        "fal": "falling, descending, declining, failing",
        "exp": "expansion, spreading, growing outward",
        "con": "contraction, shrinking, collapsing",
    },
    "life": {
        "lif": "life, alive, living, animate",
        "dea": "death, dying, end, cessation",
        "bir": "birth, creation, emergence, origin",
        "see": "seed, potential, beginning, small-start",
        "roo": "root, foundation, origin, hidden-source",
        "lea": "leaf, surface, visible, temporary",
        "tre": "tree, growth, structure, life-form",
    },
    "mind": {
        "thi": "thinking, cognition, reasoning, mind",
        "fee": "feeling, emotion, sensation, intuition",
        "kno": "knowing, knowledge, certainty, understanding",
        "bel": "believing, faith, assumption, trust",
        "dou": "doubting, uncertainty, questioning",
        "rem": "remembering, memory, recall, past-thought",
        "ima": "imagining, fantasy, creativity, possibility",
        "und": "understanding, comprehension, grasping",
        "cur": "curiosity, questioning, seeking, exploring",
    },
    "communication": {
        "say": "saying, speaking, expressing, stating",
        "lis": "listening, receiving, attending, hearing",
        "wor": "word, symbol, unit, name",
        "mea": "meaning, content, sense, significance",
        "amb": "ambiguous, unclear, multiple, uncertain",
        "cle": "clear, obvious, single, certain",
        "que": "question, inquiry, seeking, asking",
        "ans": "answer, response, solution, resolution",
    },
    "social": {
        "sel": "self, I, me, identity",
        "oth": "other, they, different, external",
        "fri": "friend, ally, close, trusted",
        "joi": "joining, connecting, uniting, together",
        "sep": "separating, dividing, apart, alone",
        "giv": "giving, providing, sharing, offering",
        "tak": "taking, receiving, acquiring, getting",
        "sha": "sharing, mutual, common, together",
    },
    "values": {
        "goo": "good, positive, right, beneficial",
        "bad": "bad, negative, wrong, harmful",
        "tru": "true, real, actual, honest",
        "fal": "false, fake, pretend, deceptive",
        "val": "value, worth, importance, significance",
    },
    "quantity": {
        "zer": "zero, none, nothing, empty",
        "one": "one, single, unique, individual",
        "few": "few, some, several, limited",
        "man": "many, multiple, numerous, several",
        "all": "all, every, total, complete",
        "mor": "more, greater, additional",
        "les": "less, fewer, reduced",
    },
    "operators": {
        "nu": "not, negation, opposite, absence",
        "ve": "very, intensely, extremely, truly",
        "so": "somewhat, slightly, partially",
        "yo": "this, here, proximal, present",
        "an": "that, there, distal, distant",
        "wh": "what, which, who, query",
    },
}

# Flatten vocabulary
ALL_WORDS = {}
for domain, words in VOCABULARY.items():
    for word, meaning in words.items():
        ALL_WORDS[word] = {"domain": domain, "meaning": meaning}


# =============================================================================
# Helper Functions
# =============================================================================

def validate_limn(sentence: str) -> Tuple[bool, List[str], List[str]]:
    """Validate a Limn sentence. Returns (is_valid, valid_words, unknown_words)."""
    words = re.findall(r'\b[a-z]+\b', sentence.lower())
    valid = []
    unknown = []

    for word in words:
        if word in ALL_WORDS or word == "|" or len(word) == 1:
            valid.append(word)
        else:
            unknown.append(word)

    return len(unknown) == 0, valid, unknown


def get_word_info(word: str) -> Optional[Dict]:
    """Get information about a Limn word."""
    return ALL_WORDS.get(word.lower())


def get_domain_words(domain: str) -> Optional[Dict[str, str]]:
    """Get all words in a domain."""
    return VOCABULARY.get(domain.lower())


def generate_random_sentence(length: int = 4) -> str:
    """Generate a random valid Limn sentence."""
    words = list(ALL_WORDS.keys())
    selected = random.sample(words, min(length, len(words)))
    if length > 2 and random.random() > 0.5:
        # Add a scope operator
        insert_pos = random.randint(1, len(selected) - 1)
        selected.insert(insert_pos, "|")
    return " ".join(selected)


# =============================================================================
# Discord Bot
# =============================================================================

class LimnBot(commands.Bot):
    """Discord bot for Limn language interpretation."""

    def __init__(self):
        intents = discord.Intents.default()
        intents.message_content = True

        super().__init__(
            command_prefix="!",
            intents=intents,
            description="Limn Language Interpreter"
        )

        # Claude client for AI features
        self.claude = None
        if ANTHROPIC_AVAILABLE:
            api_key = os.getenv("ANTHROPIC_API_KEY")
            if api_key:
                self.claude = Anthropic(api_key=api_key)
                logger.info("Claude API initialized")

    async def setup_hook(self):
        """Set up slash commands."""
        await self.tree.sync()
        logger.info("Slash commands synced")

    async def on_ready(self):
        """Called when bot is ready."""
        logger.info(f"Logged in as {self.user} (ID: {self.user.id})")
        logger.info(f"Connected to {len(self.guilds)} guilds")

        # Set status
        await self.change_presence(
            activity=discord.Activity(
                type=discord.ActivityType.listening,
                name="amb | key | cle"
            )
        )


# Create bot instance
bot = LimnBot()


# =============================================================================
# Slash Commands
# =============================================================================

@bot.tree.command(name="limn", description="Limn language interpreter")
@app_commands.describe(
    action="What to do (interpret, compose, validate, vocab, random, poetry, help)",
    input="The Limn sentence, English concept, word, or theme",
    key="Optional: Context key for interpretation"
)
@app_commands.choices(action=[
    app_commands.Choice(name="interpret", value="interpret"),
    app_commands.Choice(name="compose", value="compose"),
    app_commands.Choice(name="validate", value="validate"),
    app_commands.Choice(name="vocab", value="vocab"),
    app_commands.Choice(name="random", value="random"),
    app_commands.Choice(name="poetry", value="poetry"),
    app_commands.Choice(name="help", value="help"),
])
async def limn_command(
    interaction: discord.Interaction,
    action: str,
    input: str = "",
    key: str = ""
):
    """Main Limn command handler."""

    if action == "help":
        await send_help(interaction)

    elif action == "interpret":
        if not input:
            await interaction.response.send_message(
                "Please provide a Limn sentence to interpret.\n"
                "Example: `/limn interpret sol liq tra`",
                ephemeral=True
            )
            return
        await interpret_sentence(interaction, input, key)

    elif action == "compose":
        if not input:
            await interaction.response.send_message(
                "Please provide an English concept to compose.\n"
                'Example: `/limn compose "ice melting in spring"`',
                ephemeral=True
            )
            return
        await compose_sentence(interaction, input)

    elif action == "validate":
        if not input:
            await interaction.response.send_message(
                "Please provide a Limn sentence to validate.\n"
                "Example: `/limn validate sol liq tra`",
                ephemeral=True
            )
            return
        await validate_sentence(interaction, input)

    elif action == "vocab":
        await lookup_vocab(interaction, input)

    elif action == "random":
        await generate_random(interaction)

    elif action == "poetry":
        if not input:
            await interaction.response.send_message(
                "Please provide a theme for the poetry.\n"
                "Example: `/limn poetry hope`",
                ephemeral=True
            )
            return
        await generate_poetry(interaction, input)


async def send_help(interaction: discord.Interaction):
    """Send help message."""
    embed = discord.Embed(
        title="Limn Language Interpreter",
        description="Limn is a constructed language where words define regions of meaning and sentences are their intersection.",
        color=0x5c7cfa
    )

    embed.add_field(
        name="/limn interpret <sentence> [key]",
        value="Interpret a Limn sentence. Add a key for contextual interpretation.",
        inline=False
    )
    embed.add_field(
        name='/limn compose "<english>"',
        value="Create a Limn sentence from an English concept.",
        inline=False
    )
    embed.add_field(
        name="/limn validate <sentence>",
        value="Check if a sentence uses valid Limn vocabulary.",
        inline=False
    )
    embed.add_field(
        name="/limn vocab <word|domain>",
        value="Look up a word or browse a vocabulary domain.",
        inline=False
    )
    embed.add_field(
        name="/limn random",
        value="Generate a random Limn sentence.",
        inline=False
    )
    embed.add_field(
        name="/limn poetry <theme>",
        value="Generate Limn poetry on a theme.",
        inline=False
    )

    embed.set_footer(text="amb | key | cle — ambiguity, key, clarity")

    await interaction.response.send_message(embed=embed)


async def interpret_sentence(interaction: discord.Interaction, sentence: str, key: str = ""):
    """Interpret a Limn sentence."""
    await interaction.response.defer()

    # Validate first
    is_valid, valid_words, unknown = validate_limn(sentence)

    if not is_valid:
        await interaction.followup.send(
            f"**Unknown words:** {', '.join(unknown)}\n"
            f"Check `/limn vocab` for valid vocabulary."
        )
        return

    # Get word meanings
    word_info = []
    for word in valid_words:
        if word in ALL_WORDS:
            info = ALL_WORDS[word]
            word_info.append(f"**{word}**: {info['meaning']}")

    # Build response
    embed = discord.Embed(
        title=f"Interpreting: `{sentence}`",
        color=0x5c7cfa
    )

    embed.add_field(
        name="Word Analysis",
        value="\n".join(word_info) if word_info else "No content words",
        inline=False
    )

    # Generate interpretations using Claude if available
    if bot.claude:
        try:
            interpretations = await generate_interpretations(sentence, key)
            embed.add_field(
                name=f"Interpretations{' (Key: ' + key + ')' if key else ''}",
                value=interpretations[:1024],  # Discord limit
                inline=False
            )
        except Exception as e:
            logger.error(f"Claude API error: {e}")
            embed.add_field(
                name="Interpretations",
                value="*AI interpretation unavailable. See word analysis above.*",
                inline=False
            )
    else:
        embed.add_field(
            name="Interpretations",
            value="*The intersection of these meanings. Context (key) collapses to specific reading.*",
            inline=False
        )

    embed.set_footer(text="Meaning = intersection of all word regions")

    await interaction.followup.send(embed=embed)


async def generate_interpretations(sentence: str, key: str = "") -> str:
    """Generate interpretations using Claude."""
    if key:
        prompt = f"""Interpret this Limn sentence with the given key. Limn words define regions; sentences are intersections.

Sentence: {sentence}
Key: {key}

Give 2-3 specific interpretations under this key. Be concise, poetic. Use lowercase. 2-3 lines each."""
    else:
        prompt = f"""Interpret this Limn sentence across different domains. Limn words define regions; sentences are intersections.

Sentence: {sentence}

Give 5 different interpretations from different domains (physics, biology, relationships, philosophy, etc.). Be concise, poetic. Use lowercase. 1-2 lines each. Number them."""

    response = bot.claude.messages.create(
        model="claude-sonnet-4-20250514",
        max_tokens=500,
        messages=[{"role": "user", "content": prompt}]
    )

    return response.content[0].text


async def compose_sentence(interaction: discord.Interaction, concept: str):
    """Compose Limn from English."""
    await interaction.response.defer()

    if not bot.claude:
        await interaction.followup.send(
            "Composition requires AI. Currently unavailable."
        )
        return

    prompt = f"""Create a Limn sentence expressing: "{concept}"

Use ONLY these vocabulary categories:
- Physical: sol, liq, gas, hot, col, bri, dim, mag, min
- Spatial: abo, bel, bet, nea, far, ins, out, cen, per
- Temporal: now, pas, fut, beg, end, dur, cyc, mom, ete
- Change: gro, dec, tra, mov, sta, flo, ris, fal, exp, con
- Life: lif, dea, bir, see, roo, lea, tre
- Mind: thi, fee, kno, bel, dou, rem, ima, und, cur
- Communication: say, lis, wor, mea, amb, cle, que, ans
- Social: sel, oth, fri, joi, sep, giv, tak, sha
- Values: goo, bad, tru, fal, val
- Operators: nu (not), | (scope boundary)

Provide 2-3 options with explanations. Format:
**Option 1:** `limn sentence`
Explanation...

**Option 2:** `limn sentence`
Explanation..."""

    try:
        response = bot.claude.messages.create(
            model="claude-sonnet-4-20250514",
            max_tokens=600,
            messages=[{"role": "user", "content": prompt}]
        )

        embed = discord.Embed(
            title=f'Composing: "{concept}"',
            description=response.content[0].text[:2000],
            color=0x5c7cfa
        )

        await interaction.followup.send(embed=embed)

    except Exception as e:
        logger.error(f"Compose error: {e}")
        await interaction.followup.send("Composition failed. Please try again.")


async def validate_sentence(interaction: discord.Interaction, sentence: str):
    """Validate a Limn sentence."""
    is_valid, valid_words, unknown = validate_limn(sentence)

    if is_valid:
        embed = discord.Embed(
            title="Validation: Valid",
            description=f"`{sentence}`\n\nAll words are valid Limn vocabulary.",
            color=0x40c057
        )
        embed.add_field(
            name="Words Found",
            value=", ".join(valid_words) if valid_words else "None",
            inline=False
        )
    else:
        embed = discord.Embed(
            title="Validation: Invalid",
            description=f"`{sentence}`\n\nSome words are not in the vocabulary.",
            color=0xfa5252
        )
        embed.add_field(
            name="Valid Words",
            value=", ".join(valid_words) if valid_words else "None",
            inline=True
        )
        embed.add_field(
            name="Unknown Words",
            value=", ".join(unknown),
            inline=True
        )

    await interaction.response.send_message(embed=embed)


async def lookup_vocab(interaction: discord.Interaction, query: str):
    """Look up vocabulary."""
    query = query.lower().strip()

    # Check if it's a word
    if query in ALL_WORDS:
        info = ALL_WORDS[query]
        embed = discord.Embed(
            title=f"Vocabulary: `{query}`",
            color=0x5c7cfa
        )
        embed.add_field(name="Domain", value=info["domain"], inline=True)
        embed.add_field(name="Meaning Region", value=info["meaning"], inline=False)

        await interaction.response.send_message(embed=embed)
        return

    # Check if it's a domain
    if query in VOCABULARY:
        words = VOCABULARY[query]
        word_list = "\n".join([f"`{w}`: {m}" for w, m in words.items()])

        embed = discord.Embed(
            title=f"Domain: {query}",
            description=word_list[:2000],
            color=0x5c7cfa
        )

        await interaction.response.send_message(embed=embed)
        return

    # List all domains
    if not query:
        domains = "\n".join([f"**{d}**: {len(w)} words" for d, w in VOCABULARY.items()])

        embed = discord.Embed(
            title="Vocabulary Domains",
            description=domains,
            color=0x5c7cfa
        )
        embed.set_footer(text="Use /limn vocab <domain> to browse")

        await interaction.response.send_message(embed=embed)
        return

    # Not found
    await interaction.response.send_message(
        f"Word or domain `{query}` not found.\n"
        f"Use `/limn vocab` to see all domains.",
        ephemeral=True
    )


async def generate_random(interaction: discord.Interaction):
    """Generate a random Limn sentence."""
    sentence = generate_random_sentence(random.randint(3, 5))

    embed = discord.Embed(
        title="Random Limn Sentence",
        description=f"```{sentence}```",
        color=0x5c7cfa
    )
    embed.set_footer(text="Use /limn interpret to see meanings")

    await interaction.response.send_message(embed=embed)


async def generate_poetry(interaction: discord.Interaction, theme: str):
    """Generate Limn poetry."""
    await interaction.response.defer()

    if not bot.claude:
        await interaction.followup.send(
            "Poetry generation requires AI. Currently unavailable."
        )
        return

    prompt = f"""Generate a short Limn poem on the theme: "{theme}"

Create 4-6 lines of valid Limn with English annotations.

Use ONLY valid Limn vocabulary. Create evocative, meaningful combinations.

Format:
```
line 1
line 2
line 3
line 4
```

Then provide line-by-line interpretation."""

    try:
        response = bot.claude.messages.create(
            model="claude-sonnet-4-20250514",
            max_tokens=800,
            messages=[{"role": "user", "content": prompt}]
        )

        embed = discord.Embed(
            title=f"Limn Poetry: {theme}",
            description=response.content[0].text[:2000],
            color=0x5c7cfa
        )

        await interaction.followup.send(embed=embed)

    except Exception as e:
        logger.error(f"Poetry error: {e}")
        await interaction.followup.send("Poetry generation failed. Please try again.")


# =============================================================================
# Message Handlers
# =============================================================================

@bot.event
async def on_message(message: discord.Message):
    """Handle messages - respond to Limn in certain channels."""
    if message.author.bot:
        return

    # Process commands first
    await bot.process_commands(message)

    # Check if in Limn-practice channel
    if "limn" in message.channel.name.lower() and "practice" in message.channel.name.lower():
        # Check if message looks like Limn (short lowercase words)
        words = message.content.lower().split()
        if all(len(w) <= 4 or w == "|" for w in words) and len(words) >= 2:
            # React to valid Limn
            is_valid, _, _ = validate_limn(message.content)
            if is_valid:
                await message.add_reaction("")


# =============================================================================
# Main
# =============================================================================

def main():
    """Run the bot."""
    token = os.getenv("DISCORD_TOKEN")

    if not token:
        logger.error("DISCORD_TOKEN not set")
        return

    bot.run(token)


if __name__ == "__main__":
    main()
