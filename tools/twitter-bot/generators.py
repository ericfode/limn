"""Content generators for Limn Twitter Bot.

Each generator returns a dict with:
    type: str       - content category
    text: str       - tweet text (max 280 chars)
    tweets: list    - (thread only) list of tweet texts
"""

import random
import subprocess
from pathlib import Path

# Sample vocabulary organized by semantic affinity
WORD_GROUPS = {
    "emotion": [
        ("joy", "joy, happiness"),
        ("sad", "sadness, sorrow"),
        ("lov", "love"),
        ("fea", "fear"),
        ("hop", "hope"),
        ("ang", "anger"),
        ("cal", "calm"),
        ("exc", "excitement"),
        ("cur", "curiosity"),
        ("sha", "shame"),
    ],
    "physical": [
        ("sol", "solid, rigid"),
        ("liq", "liquid, fluid"),
        ("gas", "gas, vapor"),
        ("hot", "hot, heat"),
        ("col", "cold"),
        ("bri", "bright"),
        ("dim", "dim, dark"),
        ("har", "hard"),
        ("sof", "soft"),
        ("dry", "dry"),
    ],
    "temporal": [
        ("now", "now, present"),
        ("pas", "past"),
        ("fut", "future"),
        ("beg", "beginning"),
        ("end", "ending"),
        ("dur", "duration"),
        ("cha", "change"),
        ("tra", "transformation"),
        ("gro", "growth"),
        ("dec", "decay"),
    ],
    "cognitive": [
        ("thi", "thinking"),
        ("kno", "knowing"),
        ("dre", "dreaming"),
        ("rem", "remembering"),
        ("ima", "imagining"),
        ("cre", "creating"),
        ("see", "seeing"),
        ("und", "understanding"),
        ("dou", "doubt"),
        ("bel", "belief"),
    ],
    "social": [
        ("sel", "self"),
        ("oth", "other"),
        ("fri", "friend"),
        ("ene", "enemy"),
        ("fam", "family"),
        ("lov", "love"),
        ("los", "loss"),
        ("giv", "giving"),
        ("tak", "taking"),
        ("joi", "joining"),
    ],
    "nature": [
        ("sun", "sun"),
        ("rai", "rain"),
        ("oce", "ocean"),
        ("tre", "tree"),
        ("win", "wind"),
        ("sno", "snow"),
        ("riv", "river"),
        ("sky", "sky"),
        ("fog", "fog"),
        ("tid", "tide"),
    ],
}

OPERATORS = [
    ("@", "projection", "extract component", "lov@fea = the fear within love"),
    ("*", "interference", "emergent meaning", "sol*liq = gel (neither/both)"),
    ("^", "gradient", "intensity scaling", "joy^0.3 = mild joy (contentment)"),
    ("\\", "subtraction", "remove component", "fea\\dan = fear without danger (anxiety)"),
    (":", "conditional", "given context", "gro:tim = growth given time"),
    ("\u00b1", "superposition", "uncollapsed both", "hop\u00b1fea = hope-and-fear"),
]

# Pre-composed expressions with interpretations
COMPOSITIONS = [
    {
        "limn": "sol*liq",
        "short": "gel",
        "long": "Neither solid nor liquid. The interference creates emergence.",
    },
    {
        "limn": "(joy@sad)^0.7:tim",
        "short": "strong nostalgia",
        "long": "The sadness-component of joy, at high intensity, given time as context.",
    },
    {
        "limn": "fea\\dan",
        "short": "anxiety",
        "long": "Fear with the danger removed. What remains is pure dread without cause.",
    },
    {
        "limn": "kno\\cer",
        "short": "doubt",
        "long": "Knowledge without certainty. You know, but you can't be sure.",
    },
    {
        "limn": "lov@sel",
        "short": "self-love",
        "long": "The self-component of love. Not narcissism - the foundation.",
    },
    {
        "limn": "hop*fea",
        "short": "anticipation",
        "long": "Hope and fear interfere. What emerges is the weight of the future.",
    },
    {
        "limn": "rem@joy",
        "short": "fond memory",
        "long": "The joy-component of remembering. Not all memories - just the warm ones.",
    },
    {
        "limn": "thi*fee",
        "short": "intuition",
        "long": "Thinking and feeling interfere. The pattern that emerges before reasoning.",
    },
    {
        "limn": "beg*end",
        "short": "threshold",
        "long": "Beginning and ending simultaneously. The doorway. The pivot point.",
    },
    {
        "limn": "sel\\oth",
        "short": "solitude",
        "long": "Self with other removed. Not loneliness - just the pure experience of being alone.",
    },
    {
        "limn": "now^0.1",
        "short": "barely present",
        "long": "The faintest grip on the present moment. Almost gone.",
    },
    {
        "limn": "oce*sky",
        "short": "horizon",
        "long": "Where ocean and sky interfere. The line that doesn't exist.",
    },
    {
        "limn": "dre:fut",
        "short": "vision",
        "long": "Dreaming given the future as context. Not fantasy - foresight.",
    },
    {
        "limn": "(lov*los):tim",
        "short": "grief aging into grace",
        "long": "Love and loss interfering, through the lens of time.",
    },
    {
        "limn": "cur\\fea",
        "short": "wonder",
        "long": "Curiosity with fear removed. Pure exploration.",
    },
]

POETRY_LINES = [
    ("bri beg lif | dim end lif", "dawn of consciousness | twilight of consciousness"),
    ("now^0.5 | pas^0.5", "half-present | half-past"),
    ("sel | oth | bet: mea", "self | other | between them: meaning"),
    ("lov tra | tra lov", "love transforms | transformation loves"),
    (
        "nu cle | nu hid | amb dur",
        "not clear | not hidden | ambiguity endures",
    ),
    ("gro slo | bri dee", "growth slow | brightness deep"),
    (
        "rem | nu rem | bet: tru",
        "remembering | not-remembering | between: truth",
    ),
    ("fea pas | hop fut | now: cha", "fear of past | hope for future | now: change"),
]

INTERPRETATIONS = [
    {
        "sentence": "sol aqu dur",
        "words": "solid + water + duration",
        "readings": [
            "ice persisting through winter",
            "a glacier's slow march",
            "permafrost, ancient and enduring",
            "frozen tears, held indefinitely",
        ],
    },
    {
        "sentence": "lif gro you",
        "words": "alive + growth + young",
        "readings": [
            "a seedling pushing through soil",
            "a child learning to walk",
            "a startup finding its market",
            "the first spark of an idea",
        ],
    },
    {
        "sentence": "hot ris flo",
        "words": "hot + rising + flowing",
        "readings": [
            "magma ascending through rock",
            "bread dough rising in an oven",
            "anger building in a conversation",
            "passion spreading between people",
        ],
    },
    {
        "sentence": "nox dee sel",
        "words": "dark + deep + self",
        "readings": [
            "the unconscious mind",
            "3am introspection",
            "shadow work in therapy",
            "the parts of yourself you hide",
        ],
    },
]


def _get_vocab_word():
    """Get a random word from the vocabulary database."""
    try:
        result = subprocess.run(
            [
                "dolt",
                "sql",
                "-q",
                "SELECT word, meaning FROM words ORDER BY RAND() LIMIT 1",
                "-r",
                "csv",
            ],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent.parent / "data" / "vocabulary",
            timeout=5,
        )
        if result.returncode == 0:
            lines = result.stdout.strip().split("\n")
            if len(lines) > 1:
                word, meaning = lines[1].split(",", 1)
                return word.strip(), meaning.strip().strip('"')
    except (subprocess.TimeoutExpired, FileNotFoundError):
        pass

    # Fallback: pick from embedded lists
    group = random.choice(list(WORD_GROUPS.values()))
    return random.choice(group)


def _recently_posted(state: dict, content_type: str, text: str) -> bool:
    """Check if similar content was recently posted."""
    recent = state.get("posted", [])[-20:]
    for post in recent:
        if post.get("type") == content_type and post.get("text") == text:
            return True
    return False


def generate_daily_word(state: dict) -> dict:
    """Generate a 'Word of the Day' tweet."""
    word, meaning = _get_vocab_word()

    # Build a simple composition with the word
    group = random.choice(list(WORD_GROUPS.values()))
    partner_word, partner_meaning = random.choice(group)
    while partner_word == word:
        partner_word, partner_meaning = random.choice(group)

    text = (
        f"Limn word: {word}\n"
        f'"{meaning}"\n\n'
        f"Try: {word} {partner_word}\n"
        f"({meaning} + {partner_meaning})\n\n"
        f"What meanings emerge in the intersection?\n\n"
        f"#Limn #conlang"
    )

    return {"type": "daily_word", "text": text}


def generate_composition(state: dict) -> dict:
    """Generate a compositional expression tweet."""
    comp = random.choice(COMPOSITIONS)
    while _recently_posted(state, "composition", comp["limn"]):
        comp = random.choice(COMPOSITIONS)

    text = (
        f"{comp['limn']}\n\n"
        f"> {comp['long']}\n\n"
        f"In Limn, {len(comp['limn'])} characters.\n"
        f'In English, "{comp["short"]}" barely covers it.\n\n'
        f"#Limn #conlang"
    )

    if len(text) > 280:
        text = f"{comp['limn']}\n\n> {comp['long']}\n\n#Limn #conlang"

    return {"type": "composition", "text": text}


def generate_operator_example(state: dict) -> dict:
    """Generate an operator explanation tweet."""
    op_char, op_name, op_desc, op_example = random.choice(OPERATORS)

    text = (
        f"Limn operator: {op_char} ({op_name})\n\n"
        f"{op_desc}\n\n"
        f"Example: {op_example}\n\n"
        f"Six operators turn 1,040 words into infinite expressions.\n\n"
        f"#Limn #conlang"
    )

    return {"type": "operator", "text": text}


def generate_poetry_fragment(state: dict) -> dict:
    """Generate a Limn poetry fragment tweet."""
    line_limn, line_english = random.choice(POETRY_LINES)

    text = (
        f"{line_limn}\n\n"
        f"> {line_english}\n\n"
        f"In Limn, words are constraints. Poetry is their intersection.\n\n"
        f"#Limn #conlang #poetry"
    )

    return {"type": "poetry", "text": text}


def generate_interpretation(state: dict) -> dict:
    """Generate a 'what does this mean?' tweet."""
    interp = random.choice(INTERPRETATIONS)

    readings_text = "\n".join(f"  - {r}" for r in interp["readings"][:3])

    text = (
        f"What does this Limn sentence mean?\n\n"
        f"{interp['sentence']}\n"
        f"({interp['words']})\n\n"
        f"Some readings:\n{readings_text}\n\n"
        f"What key would YOU use?\n\n"
        f"#Limn #conlang"
    )

    if len(text) > 280:
        text = (
            f"{interp['sentence']}\n"
            f"({interp['words']})\n\n"
            f"Readings:\n{readings_text}\n\n"
            f"Your key?\n\n"
            f"#Limn"
        )

    return {"type": "interpretation", "text": text}


def generate_thread(state: dict) -> dict:
    """Generate a multi-tweet thread explaining a concept."""
    threads = [
        {
            "title": "constraint_intersection",
            "tweets": [
                (
                    "In Limn, words aren't labels. They're regions.\n\n"
                    "`sol` isn't just 'solid'. It's the entire territory: "
                    "rock, bone, ice, commitment, stubbornness.\n\n"
                    "A sentence? The intersection of regions.\n\n"
                    "1/4 #Limn"
                ),
                (
                    "`sol liq`\n\n"
                    "Where does solid AND liquid overlap?\n\n"
                    "Ice melting. Water freezing. Gel. Slush.\n"
                    "The boundary state.\n\n"
                    "Not 'solid then liquid'. Both, simultaneously.\n\n"
                    "2/4"
                ),
                (
                    "Now add a key.\n\n"
                    '`sol liq` with key "kitchen" = ice cubes\n'
                    '`sol liq` with key "geology" = permafrost\n'
                    '`sol liq` with key "emotion" = mixed feelings\n\n'
                    "Same words. Different meaning. All valid.\n\n"
                    "3/4"
                ),
                (
                    "This is Limn.\n\n"
                    "1,040 words. Six compositional operators. "
                    "Order doesn't matter. Meaning is intersection.\n\n"
                    "A language for minds that think in parallel.\n\n"
                    "github.com/ericfode/limn\n\n"
                    "4/4"
                ),
            ],
        },
        {
            "title": "operators",
            "tweets": [
                (
                    "Limn has 6 operators that turn 1,040 words "
                    "into infinite expressions.\n\n"
                    "Each maps to something LLMs already do "
                    "in embedding space.\n\n"
                    "1/4 #Limn"
                ),
                (
                    "@ (projection): extract a component\n"
                    "  lov@fea = the fear within love\n\n"
                    "* (interference): emergent meaning\n"
                    "  sol*liq = gel\n\n"
                    "^ (gradient): intensity\n"
                    "  joy^0.3 = contentment\n\n"
                    "2/4"
                ),
                (
                    "\\ (subtraction): remove component\n"
                    "  fea\\dan = anxiety (fear - danger)\n\n"
                    ": (conditional): given context\n"
                    "  gro:tim = growth over time\n\n"
                    "\u00b1 (superposition): uncollapsed\n"
                    "  hop\u00b1fea = anticipation\n\n"
                    "3/4"
                ),
                (
                    "Now compose:\n\n"
                    "(lov@fea)*(hop@dou)^0.7\n\n"
                    "The fear-component of love interfering "
                    "with doubtful hope, at high intensity.\n\n"
                    "19 characters. Try that in English.\n\n"
                    "github.com/ericfode/limn\n\n"
                    "4/4"
                ),
            ],
        },
    ]

    thread = random.choice(threads)
    return {"type": "thread", "tweets": thread["tweets"]}
