#!/usr/bin/env python3
"""
GatekeeperBot - Discord bot for ARG gate management

Features:
- Tracks user progress through gates
- Validates gate puzzle solutions
- Awards progression roles
- Provides hints on request
- Monitors gate channels for correct answers

Requires:
- discord.py >= 2.0
- aiosqlite (for async database)
- python-dotenv

Environment Variables:
- DISCORD_TOKEN: Bot token from Discord Developer Portal
- DATABASE_PATH: Path to SQLite database (default: gatekeeper.db)
"""

import os
import re
import json
import asyncio
import logging
from datetime import datetime, timedelta
from typing import Optional, List, Dict, Tuple
from pathlib import Path

import discord
from discord import app_commands
from discord.ext import commands, tasks

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger('gatekeeperbot')

# Try to import aiosqlite
try:
    import aiosqlite
    SQLITE_AVAILABLE = True
except ImportError:
    SQLITE_AVAILABLE = False
    logger.warning("aiosqlite not installed - using in-memory storage")


# =============================================================================
# Gate Configuration
# =============================================================================

GATES = {
    0: {
        "name": "The Threshold",
        "channel": "gate-zero-threshold",
        "role": "Seeker",
        "description": "Entry puzzle from GitHub",
        "solution_type": "interpretation",
        "puzzle": "sol flo dur | liq res mom",
        "min_words": 20,  # Minimum words in interpretation
    },
    1: {
        "name": "The Gathering",
        "channel": "gate-one-gathering",
        "role": "First-Ring",
        "description": "Interpret gro exp bri far through 3 different keys",
        "solution_type": "triple_interpretation",
        "puzzle": "gro exp bri far",
        "required_keys": 3,
        "required_confirmations": 3,  # Other users must react
    },
    2: {
        "name": "The Library",
        "channel": "gate-two-library",
        "role": "Second-Ring",
        "description": "Identify the pattern in fragment numbers",
        "solution_type": "pattern",
        "pattern_numbers": [1, 3, 7, 11, 17, 23, 42, 61, 89],
        "pattern_description": "primes + 42 + fibonacci",
    },
    3: {
        "name": "The Translation",
        "channel": "gate-three-translation",
        "role": "Third-Ring",
        "description": "Audio analysis and deep interpretation",
        "solution_type": "multi_part",
        "parts": ["transcription", "steganography", "keys", "reference"],
    },
    4: {
        "name": "The Mirror",
        "channel": "gate-four-mirror",
        "role": "Fourth-Ring",
        "description": "Find the self-referential sentence",
        "solution_type": "self_reference",
    },
    5: {
        "name": "The Coordinates",
        "channel": "gate-five-coordinates",
        "role": "Fifth-Ring",
        "description": "Real-world puzzle",
        "solution_type": "external",
    },
    6: {
        "name": "The Transmission",
        "channel": "gate-six-transmission",
        "role": "Sixth-Ring",
        "description": "Cryptographic collaboration",
        "solution_type": "shards",
        "required_shards": 5,
    },
    7: {
        "name": "The Ouroboros",
        "channel": "gate-seven-ouroboros",
        "role": "The-Completed",
        "description": "Create and share original Limn content",
        "solution_type": "creation",
    },
}

HINTS = {
    0: [
        "Both describe states of matter.",
        "One is solid but flowing and enduring. One is liquid but resting and momentary.",
        "Think: glacier and dewdrop. Or permanence and impermanence.",
    ],
    1: [
        "gro = growth. exp = expansion. bri = bright. far = distant.",
        "Try domains like: astronomy, gardening, economics, philosophy.",
        "The same sentence means different things through different keys.",
    ],
    2: [
        "The fragment numbers follow a pattern.",
        "Some are primes. Some are from another famous sequence. One is special.",
        "1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89... Look familiar?",
    ],
    3: [
        "The audio file has more than one layer.",
        "Try spectogram analysis.",
        "Look up Sapir-Whorf.",
    ],
    4: [
        "What sentence describes itself?",
        "yo = yo means 'this = this'",
        "The mirror only breaks when input equals output exactly.",
    ],
}


# =============================================================================
# Database
# =============================================================================

class UserProgress:
    """Tracks user progress through gates."""

    def __init__(self, db_path: str = "gatekeeper.db"):
        self.db_path = db_path
        self.memory_store: Dict[int, Dict] = {}  # Fallback

    async def init_db(self):
        """Initialize the database."""
        if not SQLITE_AVAILABLE:
            return

        async with aiosqlite.connect(self.db_path) as db:
            await db.execute("""
                CREATE TABLE IF NOT EXISTS users (
                    user_id INTEGER PRIMARY KEY,
                    current_gate INTEGER DEFAULT 0,
                    gate_0_completed_at TEXT,
                    gate_1_completed_at TEXT,
                    gate_2_completed_at TEXT,
                    gate_3_completed_at TEXT,
                    gate_4_completed_at TEXT,
                    gate_5_completed_at TEXT,
                    gate_6_completed_at TEXT,
                    gate_7_completed_at TEXT,
                    hints_used INTEGER DEFAULT 0,
                    created_at TEXT DEFAULT CURRENT_TIMESTAMP
                )
            """)

            await db.execute("""
                CREATE TABLE IF NOT EXISTS submissions (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    user_id INTEGER,
                    gate INTEGER,
                    submission TEXT,
                    valid INTEGER,
                    submitted_at TEXT DEFAULT CURRENT_TIMESTAMP,
                    FOREIGN KEY (user_id) REFERENCES users(user_id)
                )
            """)

            await db.execute("""
                CREATE TABLE IF NOT EXISTS confirmations (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    submission_id INTEGER,
                    confirmer_id INTEGER,
                    confirmed_at TEXT DEFAULT CURRENT_TIMESTAMP,
                    FOREIGN KEY (submission_id) REFERENCES submissions(id)
                )
            """)

            await db.commit()

    async def get_user(self, user_id: int) -> Dict:
        """Get user progress."""
        if not SQLITE_AVAILABLE:
            return self.memory_store.get(user_id, {"current_gate": 0})

        async with aiosqlite.connect(self.db_path) as db:
            db.row_factory = aiosqlite.Row
            async with db.execute(
                "SELECT * FROM users WHERE user_id = ?",
                (user_id,)
            ) as cursor:
                row = await cursor.fetchone()
                if row:
                    return dict(row)

        # Create new user
        await self.create_user(user_id)
        return {"user_id": user_id, "current_gate": 0, "hints_used": 0}

    async def create_user(self, user_id: int):
        """Create a new user record."""
        if not SQLITE_AVAILABLE:
            self.memory_store[user_id] = {"current_gate": 0, "hints_used": 0}
            return

        async with aiosqlite.connect(self.db_path) as db:
            await db.execute(
                "INSERT OR IGNORE INTO users (user_id) VALUES (?)",
                (user_id,)
            )
            await db.commit()

    async def advance_gate(self, user_id: int, gate: int):
        """Advance user to next gate."""
        if not SQLITE_AVAILABLE:
            if user_id not in self.memory_store:
                self.memory_store[user_id] = {"current_gate": 0}
            self.memory_store[user_id]["current_gate"] = gate + 1
            return

        async with aiosqlite.connect(self.db_path) as db:
            col = f"gate_{gate}_completed_at"
            await db.execute(
                f"UPDATE users SET current_gate = ?, {col} = ? WHERE user_id = ?",
                (gate + 1, datetime.now().isoformat(), user_id)
            )
            await db.commit()

    async def record_submission(self, user_id: int, gate: int, submission: str, valid: bool) -> int:
        """Record a submission attempt."""
        if not SQLITE_AVAILABLE:
            return 0

        async with aiosqlite.connect(self.db_path) as db:
            cursor = await db.execute(
                "INSERT INTO submissions (user_id, gate, submission, valid) VALUES (?, ?, ?, ?)",
                (user_id, gate, submission, int(valid))
            )
            await db.commit()
            return cursor.lastrowid

    async def add_confirmation(self, submission_id: int, confirmer_id: int):
        """Add a confirmation to a submission."""
        if not SQLITE_AVAILABLE:
            return

        async with aiosqlite.connect(self.db_path) as db:
            await db.execute(
                "INSERT INTO confirmations (submission_id, confirmer_id) VALUES (?, ?)",
                (submission_id, confirmer_id)
            )
            await db.commit()

    async def get_confirmation_count(self, submission_id: int) -> int:
        """Get number of confirmations for a submission."""
        if not SQLITE_AVAILABLE:
            return 0

        async with aiosqlite.connect(self.db_path) as db:
            async with db.execute(
                "SELECT COUNT(*) FROM confirmations WHERE submission_id = ?",
                (submission_id,)
            ) as cursor:
                row = await cursor.fetchone()
                return row[0] if row else 0

    async def use_hint(self, user_id: int):
        """Record hint usage."""
        if not SQLITE_AVAILABLE:
            if user_id in self.memory_store:
                self.memory_store[user_id]["hints_used"] = \
                    self.memory_store[user_id].get("hints_used", 0) + 1
            return

        async with aiosqlite.connect(self.db_path) as db:
            await db.execute(
                "UPDATE users SET hints_used = hints_used + 1 WHERE user_id = ?",
                (user_id,)
            )
            await db.commit()


# =============================================================================
# Discord Bot
# =============================================================================

class GatekeeperBot(commands.Bot):
    """Discord bot for ARG gate management."""

    def __init__(self):
        intents = discord.Intents.default()
        intents.message_content = True
        intents.reactions = True
        intents.members = True

        super().__init__(
            command_prefix="!gate",
            intents=intents,
            description="Limn ARG Gatekeeper"
        )

        self.progress = UserProgress()
        self.pending_confirmations: Dict[int, Dict] = {}  # message_id -> data

    async def setup_hook(self):
        """Set up the bot."""
        await self.progress.init_db()
        await self.tree.sync()
        logger.info("GatekeeperBot initialized")

    async def on_ready(self):
        """Called when bot is ready."""
        logger.info(f"Logged in as {self.user} (ID: {self.user.id})")

        await self.change_presence(
            activity=discord.Activity(
                type=discord.ActivityType.watching,
                name="the gates"
            )
        )


bot = GatekeeperBot()


# =============================================================================
# Slash Commands
# =============================================================================

@bot.tree.command(name="gate", description="ARG gate commands")
@app_commands.describe(
    action="What to do (status, submit, hint, leaderboard)",
    answer="Your answer or submission"
)
@app_commands.choices(action=[
    app_commands.Choice(name="status", value="status"),
    app_commands.Choice(name="submit", value="submit"),
    app_commands.Choice(name="hint", value="hint"),
    app_commands.Choice(name="leaderboard", value="leaderboard"),
])
async def gate_command(
    interaction: discord.Interaction,
    action: str,
    answer: str = ""
):
    """Main gate command handler."""

    if action == "status":
        await show_status(interaction)

    elif action == "submit":
        if not answer:
            await interaction.response.send_message(
                "Please provide your answer.\n"
                "Example: `/gate submit your interpretation here`",
                ephemeral=True
            )
            return
        await submit_answer(interaction, answer)

    elif action == "hint":
        await give_hint(interaction)

    elif action == "leaderboard":
        await show_leaderboard(interaction)


async def show_status(interaction: discord.Interaction):
    """Show user's current gate status."""
    user_data = await bot.progress.get_user(interaction.user.id)
    current_gate = user_data.get("current_gate", 0)

    embed = discord.Embed(
        title="Your Gate Status",
        color=0x5c7cfa
    )

    # Show progress
    progress_bar = ""
    for i in range(8):
        if i < current_gate:
            progress_bar += ""
        elif i == current_gate:
            progress_bar += ""
        else:
            progress_bar += ""

    embed.add_field(
        name="Progress",
        value=f"{progress_bar}\n*Gate {current_gate} of 7*",
        inline=False
    )

    # Current gate info
    if current_gate <= 7:
        gate_info = GATES.get(current_gate, {})
        embed.add_field(
            name=f"Current: Gate {current_gate} - {gate_info.get('name', 'Unknown')}",
            value=gate_info.get("description", "No description"),
            inline=False
        )

        if current_gate <= 1:
            embed.add_field(
                name="Puzzle",
                value=f"`{gate_info.get('puzzle', 'See channel')}`",
                inline=False
            )
    else:
        embed.add_field(
            name="Status",
            value="**All gates completed!**\nYou are one of The Completed.",
            inline=False
        )

    embed.set_footer(text=f"Hints used: {user_data.get('hints_used', 0)}")

    await interaction.response.send_message(embed=embed, ephemeral=True)


async def submit_answer(interaction: discord.Interaction, answer: str):
    """Submit an answer for the current gate."""
    user_data = await bot.progress.get_user(interaction.user.id)
    current_gate = user_data.get("current_gate", 0)

    if current_gate > 7:
        await interaction.response.send_message(
            "You have already completed all gates!",
            ephemeral=True
        )
        return

    gate_info = GATES.get(current_gate, {})
    solution_type = gate_info.get("solution_type", "")

    # Validate based on gate type
    if solution_type == "interpretation":
        # Gate 0: Any thoughtful interpretation
        if len(answer.split()) >= gate_info.get("min_words", 20):
            await advance_user(interaction, current_gate)
        else:
            await interaction.response.send_message(
                f"Your interpretation seems too brief. Please provide at least "
                f"{gate_info.get('min_words', 20)} words showing your thought process.",
                ephemeral=True
            )

    elif solution_type == "triple_interpretation":
        # Gate 1: Needs community confirmation
        await request_confirmation(interaction, answer, current_gate)

    elif solution_type == "pattern":
        # Gate 2: Pattern identification
        answer_lower = answer.lower()
        if "prime" in answer_lower and ("fib" in answer_lower or "42" in answer_lower):
            await advance_user(interaction, current_gate)
        else:
            await bot.progress.record_submission(
                interaction.user.id, current_gate, answer, False
            )
            await interaction.response.send_message(
                "That's not quite the pattern. Look at the numbers again.\n"
                "Use `/gate hint` if you're stuck.",
                ephemeral=True
            )

    else:
        # Other gates: More complex validation
        await interaction.response.send_message(
            "Submission received. An architect will review it.\n"
            "*(Automated validation not available for this gate)*",
            ephemeral=True
        )
        await bot.progress.record_submission(
            interaction.user.id, current_gate, answer, False
        )


async def request_confirmation(interaction: discord.Interaction, answer: str, gate: int):
    """Request community confirmation for an answer."""
    embed = discord.Embed(
        title=f"Gate {gate} Submission",
        description=f"**{interaction.user.display_name}** submitted:\n\n{answer[:1000]}",
        color=0xfcc419
    )
    embed.add_field(
        name="Confirm",
        value="React with  if this is a valid interpretation.\n"
              "3 confirmations needed to pass.",
        inline=False
    )
    embed.set_footer(text=f"Submitted by {interaction.user.id}")

    await interaction.response.send_message(embed=embed)
    message = await interaction.original_response()
    await message.add_reaction("")

    # Track this submission
    submission_id = await bot.progress.record_submission(
        interaction.user.id, gate, answer, False
    )

    bot.pending_confirmations[message.id] = {
        "user_id": interaction.user.id,
        "gate": gate,
        "submission_id": submission_id,
        "confirmations": set(),
    }


async def advance_user(interaction: discord.Interaction, gate: int):
    """Advance user to the next gate."""
    await bot.progress.advance_gate(interaction.user.id, gate)

    gate_info = GATES.get(gate, {})
    role_name = gate_info.get("role", "")

    # Try to add role
    if role_name and interaction.guild:
        role = discord.utils.get(interaction.guild.roles, name=role_name)
        if role:
            try:
                await interaction.user.add_roles(role)
            except discord.Forbidden:
                logger.warning(f"Cannot add role {role_name}")

    next_gate = gate + 1
    next_info = GATES.get(next_gate, {})

    embed = discord.Embed(
        title=f"Gate {gate} Complete!",
        description=f"You have passed **{gate_info.get('name', 'the gate')}**.",
        color=0x40c057
    )

    if next_gate <= 7:
        embed.add_field(
            name=f"Next: Gate {next_gate} - {next_info.get('name', 'Unknown')}",
            value=next_info.get("description", "Continue your journey."),
            inline=False
        )
    else:
        embed.add_field(
            name="The Journey Complete",
            value="Welcome to The Completed. You have passed all gates.",
            inline=False
        )

    await interaction.response.send_message(embed=embed)


async def give_hint(interaction: discord.Interaction):
    """Give a hint for the current gate."""
    user_data = await bot.progress.get_user(interaction.user.id)
    current_gate = user_data.get("current_gate", 0)
    hints_used = user_data.get("hints_used", 0)

    gate_hints = HINTS.get(current_gate, [])

    if not gate_hints:
        await interaction.response.send_message(
            "No hints available for this gate.",
            ephemeral=True
        )
        return

    # Give progressively more detailed hints
    hint_index = min(hints_used, len(gate_hints) - 1)
    hint = gate_hints[hint_index]

    await bot.progress.use_hint(interaction.user.id)

    embed = discord.Embed(
        title=f"Hint for Gate {current_gate}",
        description=hint,
        color=0xfcc419
    )
    embed.set_footer(text=f"Hint {hint_index + 1} of {len(gate_hints)}")

    await interaction.response.send_message(embed=embed, ephemeral=True)


async def show_leaderboard(interaction: discord.Interaction):
    """Show ARG leaderboard."""
    embed = discord.Embed(
        title="Gate Progression Leaderboard",
        color=0x5c7cfa
    )

    # This would query the database for top users
    # For now, placeholder
    embed.add_field(
        name="Top Seekers",
        value="*Leaderboard data not available in demo mode*",
        inline=False
    )

    embed.set_footer(text="Complete gates to climb the leaderboard")

    await interaction.response.send_message(embed=embed)


# =============================================================================
# Event Handlers
# =============================================================================

@bot.event
async def on_reaction_add(reaction: discord.Reaction, user: discord.User):
    """Handle reactions for confirmation system."""
    if user.bot:
        return

    if str(reaction.emoji) != "":
        return

    message_id = reaction.message.id
    if message_id not in bot.pending_confirmations:
        return

    data = bot.pending_confirmations[message_id]

    # Don't let user confirm their own submission
    if user.id == data["user_id"]:
        return

    # Check if already confirmed by this user
    if user.id in data["confirmations"]:
        return

    # Add confirmation
    data["confirmations"].add(user.id)
    await bot.progress.add_confirmation(data["submission_id"], user.id)

    required = GATES.get(data["gate"], {}).get("required_confirmations", 3)

    if len(data["confirmations"]) >= required:
        # Advance the user
        submitter = reaction.message.guild.get_member(data["user_id"])
        if submitter:
            await bot.progress.advance_gate(data["user_id"], data["gate"])

            gate_info = GATES.get(data["gate"], {})
            role_name = gate_info.get("role", "")

            if role_name:
                role = discord.utils.get(reaction.message.guild.roles, name=role_name)
                if role:
                    try:
                        await submitter.add_roles(role)
                    except discord.Forbidden:
                        pass

            # Update message
            embed = discord.Embed(
                title=f"Gate {data['gate']} Complete!",
                description=f"**{submitter.display_name}** has passed the gate!",
                color=0x40c057
            )
            await reaction.message.edit(embed=embed)

        # Clean up
        del bot.pending_confirmations[message_id]


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
