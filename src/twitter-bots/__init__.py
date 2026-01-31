"""
Limn Twitter Bot Framework

A complete framework for running Limn-speaking Twitter bots as part of
the Limn marketing campaign. The bots create an "overheard conversation"
effect where people discover cryptic exchanges in an unfamiliar language.

Main Components:
- bot_framework.py: Core bot classes and Twitter API integration
- personas.py: Bot personality definitions and interpretive keys
- content_generator.py: Limn sentence generation and interpretation
- scheduler.py: Automated posting and conversation orchestration

Quick Start:
    from twitter_bots import LimnBot, BotOrchestrator

    # Create a single bot
    bot = LimnBot("observer")
    post = bot.generate_post()
    print(post["formatted"])

    # Orchestrate multiple bots
    orchestrator = BotOrchestrator()
    orchestrator.run_conversation("gro exp bri far")

For full documentation, see the README.md in this directory.
"""

__version__ = "1.0.0"
__author__ = "Limn Project"

from .personas import (
    Persona,
    PersonaType,
    get_persona,
    get_all_personas,
    PERSONAS,
    OBSERVER,
    GARDENER,
    MERCHANT,
    VOID,
    WEAVER,
)

from .content_generator import (
    ContentGenerator,
    LIMN_VOCABULARY,
    CONTENT_LIBRARY,
    generate_random_sentence,
    get_themed_words,
)

from .bot_framework import (
    LimnBot,
    BotOrchestrator,
    TwitterClient,
    RateLimiter,
    PostDatabase,
    setup_logging,
)

from .scheduler import (
    LimnScheduler,
    MentionMonitor,
    CampaignPhase,
    PhaseConfig,
    ConversationScript,
    CONVERSATION_SCRIPTS,
)

__all__ = [
    # Version info
    "__version__",
    "__author__",

    # Personas
    "Persona",
    "PersonaType",
    "get_persona",
    "get_all_personas",
    "PERSONAS",
    "OBSERVER",
    "GARDENER",
    "MERCHANT",
    "VOID",
    "WEAVER",

    # Content generation
    "ContentGenerator",
    "LIMN_VOCABULARY",
    "CONTENT_LIBRARY",
    "generate_random_sentence",
    "get_themed_words",

    # Bot framework
    "LimnBot",
    "BotOrchestrator",
    "TwitterClient",
    "RateLimiter",
    "PostDatabase",
    "setup_logging",

    # Scheduler
    "LimnScheduler",
    "MentionMonitor",
    "CampaignPhase",
    "PhaseConfig",
    "ConversationScript",
    "CONVERSATION_SCRIPTS",
]
