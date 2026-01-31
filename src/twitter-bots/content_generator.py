"""
Limn Content Generator

Generates valid Limn sentences and interpretations based on bot personas.
Uses Claude API for intelligent content generation while maintaining
consistency with Limn's constraint-based semantics.
"""

import random
import re
import logging
from dataclasses import dataclass
from typing import List, Dict, Optional, Tuple
from enum import Enum

from personas import Persona, get_persona, PERSONAS

logger = logging.getLogger(__name__)


# =============================================================================
# Limn Vocabulary Reference
# =============================================================================

LIMN_VOCABULARY = {
    # Physical World
    "matter": ["sol", "liq", "gas", "gel", "pow", "cry", "org", "met"],
    "elements": ["aqu", "pyr", "ter", "aer", "lux", "nox", "fer", "aur", "lig", "pet"],
    "properties": ["mag", "min", "lon", "bre", "wid", "nar", "hev", "lit", "den", "spa",
                   "smo", "rou", "har", "sof", "wet", "dry", "hot", "col", "bri", "dim"],
    "actions": ["mov", "res", "ris", "fal", "rot", "osc", "exp", "con", "flo", "blo",
                "pul", "pus", "cut", "joi", "brk", "bui", "bur", "fre", "mel", "mix"],
    "structures": ["hol", "wal", "pat", "nod", "edg", "cor", "sur", "vol", "lay", "net",
                   "bra", "loo", "spi", "fra", "fil"],

    # Space & Position
    "directions": ["nor", "sou", "eas", "wes", "ver", "hor", "for", "bac", "lef", "rig"],
    "positions": ["abo", "bel", "ins", "out", "fro", "beh", "bes", "bet", "amo", "aro",
                  "thr", "alo", "acr", "nea", "far", "her", "the", "whe"],
    "regions": ["poi", "lin", "are", "spa", "loc", "glo", "mic", "mac", "cos", "inn",
                "cen", "per", "zon", "bou", "ori", "des", "hom", "awa", "ret"],

    # Time & Change
    "time_points": ["now", "pas", "fut", "beg", "end", "mid", "daw", "dus", "noo",
                    "mom", "era", "eve", "whe", "ete"],
    "durations": ["ins", "bri", "lon", "per", "tem", "int", "cnt", "fre", "rar", "onc",
                  "alw", "nev"],
    "change": ["gro", "dec", "bir", "dea", "tra", "prs", "evo", "rev", "cyc", "pro",
               "reg", "sta", "cha", "ord", "eme", "van", "flu"],
    "temporal": ["bef", "aft", "dur", "unt", "sin", "whi", "seq", "sim", "cau", "eff",
                 "tri", "del", "acc", "ret", "rhy"],

    # Living Things
    "life_properties": ["lif", "dea", "hea", "sic", "you", "old", "str", "wea", "fer",
                        "ste", "wil", "dom", "awa", "sle", "hun"],
    "body": ["hed", "fac", "eye", "ear", "mou", "han", "foo", "hrt", "bra", "lun",
             "sto", "blo", "bon", "ski", "mus", "ner", "cel", "gen", "see", "roo"],
    "organisms": ["hum", "ani", "pla", "fun", "bac", "vir", "bir", "fis", "ins", "rep",
                  "mam", "prd", "pry", "par", "sym", "tre", "gra", "flo", "fru", "lea",
                  "wor", "spi", "bee", "wol", "sna"],

    # Mind & Cognition
    "mental_states": ["thi", "fee", "wan", "fea", "kno", "bel", "dou", "rem", "obl",
                      "ima", "dre", "att", "ign", "und", "cof", "cle", "cur", "bor",
                      "sur", "exp"],
    "emotions": ["joy", "sad", "ang", "lov", "hat", "hop", "des", "pri", "sha", "gui",
                 "gra", "env", "jea", "tru", "sus", "cal", "anx", "exc", "apa", "emp",
                 "pit", "awe", "dis", "ctm"],
    "cognitive": ["per", "jud", "rea", "int", "lea", "tea", "que", "ans", "sol", "cre",
                  "cop", "mod", "cho", "pla", "act", "ref", "abs", "ana", "syn", "com",
                  "cat", "pdc", "val"],

    # Communication
    "comm_acts": ["say", "ask", "tel", "lis", "rea", "wri", "sho", "hid", "pro", "war",
                  "pra", "cri", "agr", "dis", "per"],
    "language": ["wor", "sen", "mea", "frm", "nam", "def", "tru", "fal", "amb", "lit",
                 "met", "iro", "que", "ans"],
    "social_comm": ["gre", "far", "tha", "apo", "req", "off", "ref", "acc", "inv", "exc"],

    # Social
    "relationships": ["sel", "oth", "fri", "ene", "str", "fam", "par", "chi", "sib",
                      "lea", "fol", "tea", "stu", "mas", "ser", "gue", "hos", "nei", "riv"],
    "social_actions": ["hel", "har", "sha", "tak", "giv", "coo", "com", "bet", "fgv",
                       "rev", "pro", "att", "joi", "inc", "rul", "obe", "reb"],
    "social_structures": ["gro", "ind", "com", "soc", "org", "net", "hie", "equ", "pow",
                          "aut", "fre", "cns", "rig", "dut", "law", "nor", "val", "cul",
                          "tra", "inn"],

    # Abstract
    "quantities": ["zer", "one", "few", "man", "all", "mor", "les", "equ", "dif", "max",
                   "min", "ave", "ext", "par", "who"],
    "logic": ["and", "or", "not", "if", "bec", "thu", "nec", "pos", "imp", "cer", "pro",
              "exi", "abs", "sam", "sim", "opp", "ctr"],
    "values": ["goo", "bad", "bea", "ugl", "rig", "wro", "use", "nus", "eas", "har",
               "rea", "fak", "nat", "art", "com", "rar", "uni"],

    # Economics
    "economic": ["buy", "sel", "trd", "mon", "prc", "own", "dbt", "prf", "los", "wlf",
                 "pov", "sav", "spn", "inv", "rsk", "pay", "ern", "bor", "len", "hir",
                 "fir", "tax", "sub", "auc", "bar"],

    # Operators
    "operators": ["nu", "ve", "so", "al", "ex", "on", "ma", "mi", "eq", "di"],
    "reference": ["yo", "an", "sa", "wh", "ev", "th", "a"],
    "scope": ["|", "+", "-", "*", "/", ">", "<", "="],
}

# Flatten for quick lookup
ALL_LIMN_WORDS = set()
for category_words in LIMN_VOCABULARY.values():
    ALL_LIMN_WORDS.update(category_words)


# =============================================================================
# Content Templates
# =============================================================================

@dataclass
class ContentTemplate:
    """A template for generating Limn content."""
    pattern: str  # e.g., "{property} {change} | {property} {change}"
    theme: str
    example: str


# Templates for different content types
SENTENCE_TEMPLATES = [
    # Contrasts
    ContentTemplate("{positions} | {positions}", "contrast", "abo | bel"),
    ContentTemplate("{properties} | {properties}", "duality", "hot | col"),
    ContentTemplate("{change} | {change} | {change}", "sequence", "gro | dec | gro"),

    # Questions
    ContentTemplate("{positions} | {reference}?", "questioning", "bet | wh?"),
    ContentTemplate("{change} | {values}? | {operators} {values}?", "evaluation", "gro | goo? | nu goo?"),

    # Observations
    ContentTemplate("{properties} {properties} {properties}", "description", "bri far dur"),
    ContentTemplate("{matter} {positions} {temporal}", "scene", "sol bel dur"),

    # Cycles
    ContentTemplate("{time_points} | {time_points} | {time_points} | {time_points}", "cycle", "pas | now | fut | pas"),
    ContentTemplate("{change} {change} | {change}", "transformation", "lif dea | tra"),

    # Meta/Language
    ContentTemplate("{language} | {language} | {language}", "meta", "wor | mea | wor"),
    ContentTemplate("{comm_acts} {operators} {comm_acts}", "communication", "say nu say"),

    # Negation patterns
    ContentTemplate("{operators} {any} | {any}", "negation", "nu sol | liq"),
    ContentTemplate("{operators} {operators} {operators}", "paradox", "nu nu nu"),
]


# =============================================================================
# Pre-written Content Library
# =============================================================================

CONTENT_LIBRARY = {
    "standalone": {
        "cosmic": [
            ("min her | mag the | bet = yo", "small here. vast there. between = me.\nscale is personal."),
            ("far lux dur | nea nox bri", "far light endures. near darkness brightens.\ndistance and time play tricks."),
            ("cos exp | loc con | sim?", "cosmos expands. local contracts.\nsimultaneous?\ndepends where you're standing."),
            ("poi | lin | are | vol | ???", "point. line. area. volume. ???\nwhat comes after space?\nI keep asking."),
            ("mag min | min mag | sam", "big contains small. small contains big. same.\nfractals don't care about your sense of scale."),
        ],
        "cycles": [
            ("pas | now | fut | pas", "past. now. future. past.\ntime is a circle\npretending to be a line."),
            ("beg end | end beg | nu dif", "beginning ends. ending begins. not-different.\nwe just insist on calling them separate things."),
            ("cyc dur | lin mom | wh tru?", "cycle endures. line is momentary. which is true?\nprobably both.\nprobably neither."),
            ("now now now | pas?", "now. now. now. past?\nby the time you read this,\neverything has changed."),
            ("ete | nu ete | bet", "eternal. not-eternal. between.\nthat's where we live.\nuncomfortable but true."),
        ],
        "growth": [
            ("gro | dec | gro | dec | gro", "growth. decay. growth. decay. growth.\nthe last word is arbitrary.\nask again in a million years."),
            ("see roo | tre lea | sam lif", "seed and root. tree and leaf. same life.\nwhat looks like transformation\nis just patience."),
            ("dec nu sad | tra", "decay not-sad. transformation.\nthe mushroom doesn't mourn.\nit celebrates."),
            ("fer ste | ste fer | cyc", "fertile becomes sterile. sterile becomes fertile.\nthe desert was ocean.\nthe ocean was desert."),
            ("gro lim? | nu lim gro | end", "growth limited? unlimited growth = end.\ncancer is just ambition\nwithout boundaries."),
        ],
        "paradox": [
            ("tru | fal | tru fal | ???", "true. false. true and false. ???\nthe fourth option isn't in the vocabulary.\ndraw your own conclusions."),
            ("nu nu nu | yes?", "not-not-not. yes?\ntriple negation.\nI lost track."),
            ("say nu say | mea", "saying not-saying. meaning.\nsilence is a sentence\nwithout words."),
            ("yo nu yo | an nu an | wh?", "this not-this. that not-that. what?\nidentity is\na leaky boat."),
            ("sam | dif | sam dif | sam", "same. different. same and different. same.\neverything is like this\nif you look long enough."),
        ],
        "meta": [
            ("wor | wor wor | sen | mea", "word. words. sentence. meaning.\nfour things.\nthree transformations.\nmystery intact."),
            ("say key | lis key | sam? dif?", "speaker's key. listener's key. same? different?\ncommunication is\noptimistic guessing."),
            ("amb des | cle tra", "ambiguity by design. clarity through transformation.\nthe fog is not an error.\nit's the feature."),
            ("yo mea | an mea | bet = ???", "my meaning. your meaning. between = ???\nthat gap is\neither tragedy or comedy.\nkey dependent."),
            ("wor def wor | cyc | loo", "words define words. cycle. loop.\ndictionaries are\ncircular arguments\nwe agreed to accept."),
        ],
        "connection": [
            ("sel | oth | bet joi?", "self. other. between: joining?\nthe question is always\nwhether the line connects or divides."),
            ("nea nea | far far | sam dis", "near to near. far to far. same distance.\nproximity is\nagreement about measurement."),
            ("ins | out | wh bou?", "inside. outside. where's the boundary?\nI keep looking.\nit keeps moving."),
            ("joi cut | cut joi | ord?", "joining cuts. cutting joins. order?\nevery connection\nis also a division."),
            ("sel = oth | oth = sel | or nu?", "self = other? other = self? or not?\nthe answer changes\ndepending on how lonely I feel."),
        ],
    },
    "conversation_starters": [
        ("hot col bet | wh yo key?", "hot-cold-between. what's your key?\nI see thermal dynamics.\nwhat do you see?"),
        ("lif gro dea | goo? bad? nu?", "life grows, dies. good? bad? neither?\ngenuinely asking.\nnot rhetorical."),
        ("wor man | mea one | pos?", "many words. one meaning. possible?\nor does every reader\nread a different book?"),
        ("yo kno | an kno | sam kno?", "you know. they know. same knowing?\nhow would we check?"),
        ("say | lis | und? | nu und?", "saying. listening. understanding? not-understanding?\nwhich is happening right now?\nI can't tell from here."),
    ],
    "shared_sentences": [
        # Sentences designed for multiple interpretations
        "gro exp bri far",
        "hot col bet mov",
        "lif dea tra cyc",
        "sol bel | liq abo",
        "wh abo | wh bel | wh bet",
    ],
}


# =============================================================================
# Content Generator Class
# =============================================================================

class ContentGenerator:
    """
    Generates Limn content for Twitter bots.

    Can use either pre-written content from the library or
    generate new content using Claude API.
    """

    def __init__(self, anthropic_client=None):
        """
        Initialize the content generator.

        Args:
            anthropic_client: Optional Anthropic client for AI generation.
                            If None, only library content is available.
        """
        self.client = anthropic_client
        self.model = "claude-sonnet-4-20250514"

    def set_model(self, model: str):
        """Set the Claude model to use for generation."""
        self.model = model

    def generate_standalone_post(
        self,
        persona: Persona,
        theme: Optional[str] = None,
        use_ai: bool = True
    ) -> Tuple[str, str]:
        """
        Generate a standalone post for a persona.

        Args:
            persona: The bot persona generating the post
            theme: Optional theme to focus on
            use_ai: Whether to use AI generation (if available)

        Returns:
            Tuple of (limn_sentence, interpretation)
        """
        # Try AI generation first if available and requested
        if use_ai and self.client:
            try:
                return self._generate_with_ai(persona, theme)
            except Exception as e:
                logger.warning(f"AI generation failed, falling back to library: {e}")

        # Fall back to library content
        return self._get_library_content(persona, theme)

    def _generate_with_ai(
        self,
        persona: Persona,
        theme: Optional[str] = None
    ) -> Tuple[str, str]:
        """Generate content using Claude API."""
        prompt = persona.get_generation_prompt(theme)

        response = self.client.messages.create(
            model=self.model,
            max_tokens=300,
            messages=[{"role": "user", "content": prompt}]
        )

        text = response.content[0].text

        # Parse the response
        limn_match = re.search(r"LIMN:\s*(.+?)(?:\n|INTERPRETATION)", text, re.DOTALL)
        interp_match = re.search(r"INTERPRETATION:\s*(.+)", text, re.DOTALL)

        if limn_match and interp_match:
            limn = limn_match.group(1).strip()
            interpretation = interp_match.group(1).strip()
            return limn, interpretation
        else:
            # If parsing fails, try to extract sensibly
            lines = text.strip().split("\n")
            limn = lines[0] if lines else "gro exp bri far"
            interpretation = "\n".join(lines[1:]) if len(lines) > 1 else "[interpretation pending]"
            return limn, interpretation

    def _get_library_content(
        self,
        persona: Persona,
        theme: Optional[str] = None
    ) -> Tuple[str, str]:
        """Get content from the pre-written library."""
        # Map persona themes to library categories
        theme_mapping = {
            "observer": ["cosmic", "cycles"],
            "gardener": ["growth", "cycles"],
            "merchant": ["connection"],  # Uses connection metaphors for exchange
            "void": ["paradox"],
            "weaver": ["meta"],
        }

        # Select appropriate category
        if theme and theme in CONTENT_LIBRARY["standalone"]:
            category = theme
        else:
            categories = theme_mapping.get(persona.name, list(CONTENT_LIBRARY["standalone"].keys()))
            category = random.choice(categories)

        # Get content from category
        options = CONTENT_LIBRARY["standalone"].get(category, [])
        if not options:
            options = CONTENT_LIBRARY["standalone"]["cosmic"]

        limn, base_interpretation = random.choice(options)

        # Re-interpret through persona's key if needed
        if self.client and persona.name not in ["observer"]:  # Observer's interpretations are baseline
            try:
                interpretation = self.interpret_sentence(persona, limn)
                return limn, interpretation
            except Exception:
                pass

        return limn, base_interpretation

    def interpret_sentence(
        self,
        persona: Persona,
        limn_sentence: str
    ) -> str:
        """
        Interpret a Limn sentence through a persona's key.

        Args:
            persona: The persona providing interpretation
            limn_sentence: The Limn sentence to interpret

        Returns:
            The interpretation string
        """
        if not self.client:
            # Return a template interpretation
            return f"[{persona.name} interpretation through {persona.key_domain} key]"

        prompt = persona.get_interpretation_prompt(limn_sentence)

        response = self.client.messages.create(
            model=self.model,
            max_tokens=200,
            messages=[{"role": "user", "content": prompt}]
        )

        return response.content[0].text.strip()

    def generate_reply(
        self,
        replying_persona: Persona,
        original_limn: str,
        original_persona: Optional[Persona] = None,
        is_human: bool = False
    ) -> Tuple[str, str]:
        """
        Generate a reply to another post.

        Args:
            replying_persona: The persona making the reply
            original_limn: The Limn sentence being replied to
            original_persona: The persona who made the original post (if bot)
            is_human: Whether the original was from a human

        Returns:
            Tuple of (limn_sentence, interpretation)
        """
        if is_human:
            # Use response templates for humans
            template_key = "to_question"  # Default
            template = replying_persona.response_templates.get(template_key, "")
            if template:
                # Parse template into limn and interpretation
                parts = template.split("\n[")
                limn = parts[0].strip()
                interpretation = parts[1].rstrip("]") if len(parts) > 1 else ""
                return limn, interpretation

        # For bot-to-bot replies, reinterpret the original sentence
        interpretation = self.interpret_sentence(replying_persona, original_limn)

        # Optionally generate a response sentence
        if self.client and random.random() > 0.5:
            # Generate a new sentence that responds to the theme
            try:
                new_limn, new_interp = self._generate_response_sentence(
                    replying_persona, original_limn, original_persona
                )
                return new_limn, new_interp
            except Exception:
                pass

        return original_limn, interpretation

    def _generate_response_sentence(
        self,
        persona: Persona,
        original_limn: str,
        original_persona: Optional[Persona]
    ) -> Tuple[str, str]:
        """Generate a new sentence that responds to another."""
        prompt = f"""You are {persona.full_name}, responding to another Limn speaker.

Your key: {persona.key_domain}
Your voice: {persona.voice}

Original Limn sentence: {original_limn}
{"Original speaker's key: " + original_persona.key_domain if original_persona else ""}

Create a short Limn response (2-4 words) that:
1. References or builds on the original
2. Shifts the interpretation through your key
3. Adds a new dimension to the conversation

Format:
LIMN: [your response sentence]
INTERPRETATION: [your interpretation, lowercase, 2-3 lines]"""

        response = self.client.messages.create(
            model=self.model,
            max_tokens=200,
            messages=[{"role": "user", "content": prompt}]
        )

        text = response.content[0].text
        limn_match = re.search(r"LIMN:\s*(.+?)(?:\n|INTERPRETATION)", text, re.DOTALL)
        interp_match = re.search(r"INTERPRETATION:\s*(.+)", text, re.DOTALL)

        if limn_match and interp_match:
            return limn_match.group(1).strip(), interp_match.group(1).strip()
        return original_limn, "[response interpretation]"

    def get_conversation_starter(self) -> Tuple[str, str]:
        """Get a sentence designed to start conversations."""
        return random.choice(CONTENT_LIBRARY["conversation_starters"])

    def get_shared_sentence(self) -> str:
        """Get a sentence designed for multiple bot interpretations."""
        return random.choice(CONTENT_LIBRARY["shared_sentences"])

    def format_tweet(
        self,
        limn: str,
        interpretation: str,
        mention: Optional[str] = None,
        include_hashtag: bool = False
    ) -> str:
        """
        Format a complete tweet with Limn and interpretation.

        Args:
            limn: The Limn sentence
            interpretation: The interpretation text
            mention: Optional @mention to include at start
            include_hashtag: Whether to include a hashtag

        Returns:
            Formatted tweet string
        """
        parts = []

        if mention:
            parts.append(mention)

        parts.append(limn)
        parts.append("")  # Blank line
        parts.append(f"[{interpretation}]")

        if include_hashtag:
            parts.append("")
            parts.append(random.choice(["#limn", "#conlang"]))

        tweet = "\n".join(parts)

        # Ensure within Twitter limits
        if len(tweet) > 280:
            # Truncate interpretation if needed
            max_interp = 280 - len(limn) - 10  # Account for formatting
            interpretation = interpretation[:max_interp] + "..."
            tweet = self.format_tweet(limn, interpretation, mention, False)

        return tweet

    def validate_limn(self, sentence: str) -> Tuple[bool, List[str]]:
        """
        Validate that a string contains valid Limn words.

        Args:
            sentence: The sentence to validate

        Returns:
            Tuple of (is_valid, list_of_unknown_words)
        """
        # Extract words (ignore operators)
        words = re.findall(r'\b[a-z]+\b', sentence.lower())
        unknown = [w for w in words if w not in ALL_LIMN_WORDS and len(w) > 1]

        return len(unknown) == 0, unknown


# =============================================================================
# Utility Functions
# =============================================================================

def generate_random_sentence(
    word_count: int = 4,
    categories: Optional[List[str]] = None
) -> str:
    """
    Generate a random Limn sentence from vocabulary.

    Args:
        word_count: Number of words to include
        categories: Optional list of vocabulary categories to draw from

    Returns:
        A random Limn sentence
    """
    if categories is None:
        categories = list(LIMN_VOCABULARY.keys())

    # Exclude operators and reference from random generation
    categories = [c for c in categories if c not in ["operators", "reference", "scope"]]

    words = []
    for _ in range(word_count):
        category = random.choice(categories)
        word = random.choice(LIMN_VOCABULARY[category])
        words.append(word)

    # Maybe add a scope operator
    if random.random() > 0.7 and word_count > 2:
        insert_pos = random.randint(1, word_count - 1)
        words.insert(insert_pos, "|")

    return " ".join(words)


def get_themed_words(theme: str) -> List[str]:
    """Get vocabulary words related to a theme."""
    theme_map = {
        "cosmic": ["cosmic", "properties", "regions"],
        "growth": ["change", "life_properties", "body"],
        "exchange": ["economic", "quantities", "social_actions"],
        "paradox": ["operators", "logic", "mental_states"],
        "meta": ["language", "comm_acts", "cognitive"],
    }

    categories = theme_map.get(theme, list(LIMN_VOCABULARY.keys()))
    words = []
    for cat in categories:
        if cat in LIMN_VOCABULARY:
            words.extend(LIMN_VOCABULARY[cat])

    return words
