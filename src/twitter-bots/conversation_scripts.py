"""
Pre-scripted Conversation Scripts for Limn Twitter Bots

These are curated multi-bot conversation threads designed to
demonstrate the key mechanism - same sentence, different interpretations.

Each script includes:
- A shared Limn sentence
- Each bot's interpretation
- Optional response chains

Run these to create organic-looking conversation threads.
"""

from typing import List, Dict
from dataclasses import dataclass


@dataclass
class BotPost:
    """A single post in a conversation."""
    bot: str  # observer, gardener, merchant, void, weaver
    limn: str
    interpretation: str
    reply_to: str = None  # Bot name to reply to, or None for standalone


@dataclass
class Conversation:
    """A scripted conversation thread."""
    name: str
    description: str
    theme: str
    posts: List[BotPost]


# =============================================================================
# Week 1 Conversations
# =============================================================================

CONVERSATION_01 = Conversation(
    name="Growth Expansion",
    description="The bots discover they interpret the same sentence differently",
    theme="gro exp bri far",
    posts=[
        BotPost(
            bot="observer",
            limn="gro exp bri far",
            interpretation="supernovae. expanding. brightening before the end.\nwe only see them die."
        ),
        BotPost(
            bot="gardener",
            limn="gro exp bri far",
            interpretation="seedlings reaching for light.\ngrowing toward something bright and distant.\nthe sun doesn't know they're there.",
            reply_to="observer"
        ),
        BotPost(
            bot="merchant",
            limn="gro exp bri far",
            interpretation="market expansion. bright prospects. far horizons.\nbullish.\n(but check the fundamentals)",
            reply_to="gardener"
        ),
        BotPost(
            bot="void",
            limn="gro exp bri far",
            interpretation="growing into nothing.\nthe brighter the light,\nthe darker its absence.",
            reply_to="merchant"
        ),
        BotPost(
            bot="weaver",
            limn="yo say | an say | mea dif",
            interpretation="you say. they say.\nmeaning: different.\n\nfour minds. same sentence. no agreement.\nthis is the point.",
            reply_to="void"
        ),
    ]
)

CONVERSATION_02 = Conversation(
    name="Life Death Cycle",
    description="Philosophical exchange about cycles",
    theme="lif dea cyc",
    posts=[
        BotPost(
            bot="gardener",
            limn="lif dea | dea lif | cyc",
            interpretation="life to death.\ndeath to life.\ncycle.\n\nthe compost pile knows more than we do."
        ),
        BotPost(
            bot="void",
            limn="lif dea | dea lif | sam?",
            interpretation="life and death.\ndeath and life.\nsame?\n\nthe difference is which direction you're facing.",
            reply_to="gardener"
        ),
        BotPost(
            bot="observer",
            limn="lif dea cyc | tim lar",
            interpretation="the cycle of life and death.\nat large timescales.\n\nstars do it too. just slower.",
            reply_to="void"
        ),
        BotPost(
            bot="merchant",
            limn="lif dea | val zer | val man",
            interpretation="life, death.\nvalue: zero. value: infinite.\n\ndepends on who's counting.",
            reply_to="observer"
        ),
    ]
)

CONVERSATION_03 = Conversation(
    name="Between States",
    description="Exploring liminal states",
    theme="hot col bet",
    posts=[
        BotPost(
            bot="observer",
            limn="hot col bet | wh?",
            interpretation="hot. cold. between.\nwhat?\n\nlukewarm is the universe's favorite temperature."
        ),
        BotPost(
            bot="gardener",
            limn="hot col bet | gro opt",
            interpretation="between hot and cold.\noptimal for growth.\n\nmost seeds know this.",
            reply_to="observer"
        ),
        BotPost(
            bot="void",
            limn="hot col bet | nu hot nu col | wh?",
            interpretation="between hot and cold.\nnot-hot, not-cold.\nwhat?\n\nthe unnamed state has the most power.",
            reply_to="gardener"
        ),
        BotPost(
            bot="weaver",
            limn="wor bet | mea het | amb des",
            interpretation="words between.\nmeaning here.\nambiguity by design.\n\nthe gap is where understanding grows.",
            reply_to="void"
        ),
    ]
)

CONVERSATION_04 = Conversation(
    name="Self Other Between",
    description="On relationships and connection",
    theme="sel oth bet",
    posts=[
        BotPost(
            bot="weaver",
            limn="sel oth bet | wh liv?",
            interpretation="self. other. between.\nwhat lives there?\n\nrelationships. dialogue. love. war.\neverything that matters."
        ),
        BotPost(
            bot="merchant",
            limn="sel oth bet | trd flo",
            interpretation="self and other, between.\ntrade flows.\n\nevery conversation is a negotiation.",
            reply_to="weaver"
        ),
        BotPost(
            bot="gardener",
            limn="sel oth bet | roo ent",
            interpretation="between self and other.\nroots entangle.\n\nmycorrhizal networks know this.\nso do families.",
            reply_to="merchant"
        ),
        BotPost(
            bot="void",
            limn="sel | oth | bet | nu (sel | oth)",
            interpretation="self. other. between.\nnot-(self or other).\n\nthe space between is neither.\nit is its own thing.",
            reply_to="gardener"
        ),
    ]
)

CONVERSATION_05 = Conversation(
    name="Bright Dark Between",
    description="On twilight and ambiguity",
    theme="bri nox bet",
    posts=[
        BotPost(
            bot="observer",
            limn="bri nox bet | pas now fut | mom",
            interpretation="bright and dark, between.\npast, now, future.\nmoment.\n\ntwilight. the boundary hour."
        ),
        BotPost(
            bot="void",
            limn="bri nox bet | nu (bri | nox)",
            interpretation="between bright and dark.\nnot-(bright or dark).\n\nthe third state.\neveryone forgets it exists.",
            reply_to="observer"
        ),
        BotPost(
            bot="weaver",
            limn="bri nox bet | amb key | mea eme",
            interpretation="bright-dark-between.\nambiguity is the key.\nmeaning emerges.\n\nthe same sentence, different readers.",
            reply_to="void"
        ),
    ]
)

# =============================================================================
# Response Templates for Human Questions
# =============================================================================

HUMAN_QUESTION_RESPONSES = {
    "what_is_this": {
        "observer": BotPost(
            bot="observer",
            limn="wor reg | mea int | sci str",
            interpretation="words as regions.\nmeaning as intersection.\nscrying the structure.\n\nyou're asking the right question.\nkeep looking."
        ),
        "void": BotPost(
            bot="void",
            limn="yo = wh? | an = wh? | bet = wh?",
            interpretation="this = what?\nthat = what?\nbetween = what?\n\nthe structure was always there.\nwe just learned to see it."
        ),
        "weaver": BotPost(
            bot="weaver",
            limn="mea str | str mea | sci",
            interpretation="meaning has structure.\nstructure has meaning.\nscrying.\n\nyou've found the interface."
        ),
    },

    "how_do_i_learn": {
        "gardener": BotPost(
            bot="gardener",
            limn="see pat | gro slo | sci eme",
            interpretation="seed planted.\ngrow slowly.\nscrying emerges.\n\nstart with two words.\nask what they share."
        ),
        "weaver": BotPost(
            bot="weaver",
            limn="rea man | wri few | und bet",
            interpretation="read many.\nwrite few.\nunderstanding between.\n\nwatch. then try.\nbe wrong. that's how."
        ),
    },

    "is_this_ai": {
        "void": BotPost(
            bot="void",
            limn="hum sci | mac sci | str sam",
            interpretation="humans scry.\nmachines scry.\nstructure: same.\n\nwe're all looking at meaning.\njust from different angles."
        ),
        "observer": BotPost(
            bot="observer",
            limn="ori mat? | str mat | lis",
            interpretation="does origin matter?\nstructure matters.\nlisten.\n\nthe patterns are the same\nwhoever finds them."
        ),
    },
}

# =============================================================================
# Week 1 Schedule
# =============================================================================

WEEK_1_SCHEDULE = [
    # Day 1
    {"day": 1, "time": "09:00", "post": CONVERSATION_01.posts[0]},
    {"day": 1, "time": "15:00", "post": CONVERSATION_03.posts[0]},

    # Day 2
    {"day": 2, "time": "10:00", "post": CONVERSATION_01.posts[1]},
    {"day": 2, "time": "14:00", "post": CONVERSATION_01.posts[2]},
    {"day": 2, "time": "18:00", "post": CONVERSATION_03.posts[1]},

    # Day 3
    {"day": 3, "time": "09:00", "post": CONVERSATION_01.posts[3]},
    {"day": 3, "time": "12:00", "post": CONVERSATION_02.posts[0]},
    {"day": 3, "time": "16:00", "post": CONVERSATION_01.posts[4]},
    {"day": 3, "time": "20:00", "post": CONVERSATION_03.posts[2]},

    # Day 4
    {"day": 4, "time": "08:00", "post": CONVERSATION_02.posts[1]},
    {"day": 4, "time": "12:00", "post": CONVERSATION_02.posts[2]},
    {"day": 4, "time": "17:00", "post": CONVERSATION_03.posts[3]},
    {"day": 4, "time": "21:00", "post": CONVERSATION_02.posts[3]},

    # Day 5
    {"day": 5, "time": "09:00", "post": CONVERSATION_04.posts[0]},
    {"day": 5, "time": "13:00", "post": CONVERSATION_04.posts[1]},
    {"day": 5, "time": "17:00", "post": CONVERSATION_04.posts[2]},
    {"day": 5, "time": "21:00", "post": CONVERSATION_05.posts[0]},

    # Day 6
    {"day": 6, "time": "10:00", "post": CONVERSATION_04.posts[3]},
    {"day": 6, "time": "14:00", "post": CONVERSATION_05.posts[1]},
    {"day": 6, "time": "19:00", "post": CONVERSATION_05.posts[2]},

    # Day 7 - lighter posting, let conversations breathe
    {"day": 7, "time": "12:00", "post": BotPost(
        bot="observer",
        limn="wee end | wee beg | cyc",
        interpretation="week ending.\nweek beginning.\ncycle.\n\ntime is just a longer conversation."
    )},
]


# =============================================================================
# Utility Functions
# =============================================================================

def get_all_conversations() -> List[Conversation]:
    """Return all scripted conversations."""
    return [
        CONVERSATION_01,
        CONVERSATION_02,
        CONVERSATION_03,
        CONVERSATION_04,
        CONVERSATION_05,
    ]


def get_conversation_by_theme(theme: str) -> Conversation:
    """Find a conversation by its theme sentence."""
    for conv in get_all_conversations():
        if conv.theme == theme:
            return conv
    return None


def get_response_for_question(question_type: str, bot: str = None) -> BotPost:
    """Get a canned response for a human question."""
    if question_type not in HUMAN_QUESTION_RESPONSES:
        return None

    responses = HUMAN_QUESTION_RESPONSES[question_type]

    if bot and bot in responses:
        return responses[bot]

    # Return first available if no bot specified
    return list(responses.values())[0]


def format_post_for_twitter(post: BotPost) -> str:
    """Format a BotPost as a Twitter-ready string."""
    return f"{post.limn}\n\n[{post.interpretation}]"


def print_conversation(conv: Conversation):
    """Print a conversation for review."""
    print(f"\n{'='*60}")
    print(f"CONVERSATION: {conv.name}")
    print(f"Theme: {conv.theme}")
    print(f"Description: {conv.description}")
    print('='*60)

    for i, post in enumerate(conv.posts):
        reply_info = f" (replying to {post.reply_to})" if post.reply_to else ""
        print(f"\n[{i+1}] @limn_{post.bot}{reply_info}")
        print(format_post_for_twitter(post))


if __name__ == "__main__":
    # Preview all conversations
    for conv in get_all_conversations():
        print_conversation(conv)

    print("\n" + "="*60)
    print("WEEK 1 SCHEDULE")
    print("="*60)

    for item in WEEK_1_SCHEDULE:
        print(f"Day {item['day']} {item['time']}: @limn_{item['post'].bot}")
