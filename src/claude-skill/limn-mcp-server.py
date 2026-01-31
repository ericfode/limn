#!/usr/bin/env python3
"""
Limn MCP Server - Model Context Protocol server for Limn language interpretation

This server provides tools for interpreting, composing, and learning Limn,
a constructed language where words are constraints and meaning is their intersection.

Features:
- interpret: Interpret Limn sentences with or without keys
- compose: Create Limn from English concepts
- validate: Check if Limn sentences are valid
- teach: Interactive teaching mode
- poetry: Generate Limn poetry
- converse: Conversational Limn mode

Usage:
    python limn-mcp-server.py

Configuration:
    Add to Claude Code's MCP settings:
    {
      "mcpServers": {
        "limn": {
          "command": "python",
          "args": ["path/to/limn-mcp-server.py"]
        }
      }
    }
"""

import json
import sys
import asyncio
from typing import Optional, List, Dict, Any
from dataclasses import dataclass
from enum import Enum

# MCP Protocol implementation
# This is a simplified MCP server - in production, use the official MCP SDK

VOCABULARY = {
    # Core domains with semantic regions
    "physical": {
        "sol": "solid, rigid, fixed, hard",
        "liq": "liquid, flowing, fluid, adaptable",
        "gas": "gaseous, diffuse, ethereal, spread",
        "hot": "hot, thermal, passionate, intense",
        "col": "cold, frozen, numb, distant",
        "bri": "bright, luminous, clear, obvious",
        "dim": "dim, faint, unclear, subtle",
        "mag": "large, vast, significant, major",
        "min": "small, tiny, minimal, minor"
    },
    "spatial": {
        "abo": "above, over, superior, heaven",
        "bel": "below, under, inferior, ground",
        "ins": "inside, interior, internal, included",
        "out": "outside, exterior, external, excluded",
        "nea": "near, close, proximal, soon",
        "far": "far, distant, remote, later",
        "bet": "between, among, middle, mediating",
        "cen": "center, core, focus, essence",
        "per": "periphery, edge, margin, fringe"
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
        "ete": "eternal, timeless, infinite, always"
    },
    "change": {
        "gro": "growth, increase, develop, expand",
        "dec": "decay, decrease, deteriorate, shrink",
        "tra": "transformation, change, metamorphosis, shift",
        "mov": "movement, motion, flow, shift",
        "sta": "stability, stillness, unchanged, balanced",
        "flo": "flowing, streaming, continuous, fluid",
        "ris": "rising, ascending, improving, hope",
        "fal": "falling, descending, declining, failing"
    },
    "life": {
        "lif": "life, alive, living, animate",
        "dea": "death, dying, end, cessation",
        "hea": "health, wellness, thriving, strong",
        "sic": "sickness, illness, weak, failing",
        "you": "young, new, fresh, immature",
        "old": "old, aged, ancient, mature",
        "bir": "birth, creation, emergence, origin",
        "see": "seed, potential, beginning, small-start",
        "roo": "root, foundation, origin, hidden-source"
    },
    "mind": {
        "thi": "thinking, cognition, reasoning, mind",
        "fee": "feeling, emotion, sensation, intuition",
        "kno": "knowing, knowledge, certainty, understanding",
        "bel": "believing, faith, assumption, trust",
        "dou": "doubting, uncertainty, questioning, skepticism",
        "rem": "remembering, memory, recall, past-thought",
        "ima": "imagining, fantasy, creativity, possibility",
        "dre": "dreaming, vision, hope, sleep-thought",
        "cur": "curiosity, questioning, seeking, exploring"
    },
    "communication": {
        "say": "saying, speaking, expressing, stating",
        "lis": "listening, receiving, attending, hearing",
        "wor": "word, symbol, unit, name",
        "mea": "meaning, content, sense, significance",
        "amb": "ambiguous, unclear, multiple, uncertain",
        "cle": "clear, obvious, single, certain",
        "que": "question, inquiry, seeking, asking",
        "ans": "answer, response, solution, resolution"
    },
    "social": {
        "sel": "self, I, me, identity",
        "oth": "other, they, different, external",
        "fri": "friend, ally, close, trusted",
        "ene": "enemy, opponent, hostile, threat",
        "joi": "joining, connecting, uniting, together",
        "sep": "separating, dividing, apart, alone",
        "giv": "giving, providing, sharing, offering",
        "tak": "taking, receiving, acquiring, getting"
    },
    "values": {
        "goo": "good, positive, right, beneficial",
        "bad": "bad, negative, wrong, harmful",
        "tru": "true, real, actual, honest",
        "fal": "false, fake, pretend, deceptive",
        "val": "value, worth, importance, significance",
        "zer": "zero, none, nothing, empty",
        "one": "one, single, unique, individual",
        "man": "many, multiple, numerous, several",
        "all": "all, every, total, complete"
    },
    "operators": {
        "nu": "not, negation, opposite, absence",
        "ve": "very, intensely, extremely, truly",
        "so": "somewhat, slightly, partially, a-little",
        "yo": "this, here, proximal, present",
        "an": "that, there, distal, distant",
        "wh": "what, which, who, query",
        "|": "scope boundary, beside, separate, parallel"
    }
}

# Flatten vocabulary for quick lookup
ALL_WORDS = {}
for domain, words in VOCABULARY.items():
    for word, meaning in words.items():
        ALL_WORDS[word] = {"domain": domain, "meaning": meaning}


def validate_limn(sentence: str) -> tuple[bool, list[str], list[str]]:
    """Validate a Limn sentence and return (is_valid, valid_words, unknown_words)."""
    import re
    # Extract words, ignoring operators and punctuation
    words = re.findall(r'\b[a-z]+\b', sentence.lower())

    valid = []
    unknown = []

    for word in words:
        if word in ALL_WORDS or len(word) == 1:
            valid.append(word)
        else:
            unknown.append(word)

    return len(unknown) == 0, valid, unknown


def interpret_sentence(sentence: str, key: Optional[str] = None, num_interpretations: int = 10) -> dict:
    """Interpret a Limn sentence, optionally with a key for disambiguation."""

    is_valid, valid_words, unknown = validate_limn(sentence)

    # Get meanings for each word
    word_meanings = []
    for word in valid_words:
        if word in ALL_WORDS:
            word_meanings.append({
                "word": word,
                "domain": ALL_WORDS[word]["domain"],
                "meaning": ALL_WORDS[word]["meaning"]
            })

    result = {
        "sentence": sentence,
        "valid": is_valid,
        "unknown_words": unknown,
        "word_analysis": word_meanings,
        "interpretation_prompt": _build_interpretation_prompt(sentence, word_meanings, key)
    }

    return result


def _build_interpretation_prompt(sentence: str, word_meanings: list, key: Optional[str] = None) -> str:
    """Build a prompt for Claude to interpret the sentence."""

    word_breakdown = "\n".join([
        f"  - {wm['word']}: {wm['meaning']}"
        for wm in word_meanings
    ])

    base_prompt = f"""Interpret this Limn sentence: "{sentence}"

Limn is a constructed language where:
- Words define REGIONS of meaning, not single referents
- Sentences combine words by INTERSECTION (where all meanings overlap)
- Word ORDER does not matter (commutative)
- Ambiguity is intentional until collapsed by a KEY

Word analysis:
{word_breakdown}

The sentence's meaning is the INTERSECTION of all these constraint regions.
"""

    if key:
        prompt = base_prompt + f"""
KEY PROVIDED: "{key}"

With this key, collapse the ambiguity. Provide 2-3 specific interpretations
that fit within this contextual domain. Be precise but poetic.

Format:
## Key Collapse: {sentence}
Key: "{key}"

### Interpretations
1. [First interpretation]
2. [Second interpretation]
3. [Third interpretation - if applicable]

### Core Meaning
[The essential meaning under this key]
"""
    else:
        prompt = base_prompt + f"""
NO KEY PROVIDED - Generate {10} diverse interpretations across different domains.

For each interpretation:
- Name the implicit key/domain
- Give the specific meaning
- Be creative but valid

Format:
## Interpretations of: {sentence}
({' + '.join([wm['word'] for wm in word_meanings])})

### Without Key (Superposition)
1. **Domain 1:** [interpretation]
2. **Domain 2:** [interpretation]
... (continue for 10 interpretations)

### Core Semantic Region
[What all interpretations share - the geometric intersection]
"""

    return prompt


def compose_sentence(english_concept: str) -> dict:
    """Create a Limn sentence from an English concept."""

    prompt = f"""Compose a Limn sentence expressing: "{english_concept}"

You must use ONLY valid Limn vocabulary words:

PHYSICAL: sol (solid), liq (liquid), gas, hot, col (cold), bri (bright), dim, mag (large), min (small)
SPATIAL: abo (above), bel (below), ins (inside), out (outside), nea (near), far, bet (between), cen (center)
TEMPORAL: now, pas (past), fut (future), beg (begin), end, dur (during), cyc (cycle), mom (moment), ete (eternal)
CHANGE: gro (growth), dec (decay), tra (transformation), mov (movement), sta (stable), flo (flow), ris (rise), fal (fall)
LIFE: lif (life), dea (death), hea (health), sic (sick), you (young), old, bir (birth), see (seed), roo (root)
MIND: thi (think), fee (feel), kno (know), bel (believe), dou (doubt), rem (remember), ima (imagine), dre (dream), cur (curious)
COMMUNICATION: say, lis (listen), wor (word), mea (meaning), amb (ambiguous), cle (clear), que (question), ans (answer)
SOCIAL: sel (self), oth (other), fri (friend), ene (enemy), joi (join), sep (separate), giv (give), tak (take)
VALUES: goo (good), bad, tru (true), fal (false), val (value), zer (zero), one, man (many), all
OPERATORS: nu (not), ve (very), so (somewhat), yo (this), an (that), wh (what/which), | (scope boundary)

Rules:
- Use 3-6 words typically
- Use | to separate contrasting concepts or create scope
- Use lowercase for all Limn
- Be evocative, not explicit
- Multiple valid translations exist - provide 2-3 options

Format:
## Composing: "{english_concept}"

### Proposed Limn Sentences

**Option 1:** `[limn sentence]`
- Word breakdown: [word] + [word] + ...
- Why it works: [explanation]

**Option 2:** `[limn sentence]`
- Word breakdown: [word] + [word] + ...
- Why it works: [explanation]

**Option 3:** `[limn sentence]` (if applicable)
- Word breakdown: [word] + [word] + ...
- Why it works: [explanation]

**Recommendation:** [which option and why]
"""

    return {
        "concept": english_concept,
        "composition_prompt": prompt
    }


def teach_lesson(level: int = 1) -> dict:
    """Generate an interactive teaching lesson."""

    lessons = {
        1: {
            "title": "Basic Intersection",
            "content": """## Limn Lesson 1: Basic Intersection

In Limn, words don't have single meanings - they define REGIONS.
When you combine words, their meanings INTERSECT.

**Exercise 1:** What could `hot col` mean?

This seems contradictory - hot AND cold. But think about what exists in that intersection...

Possible answers:
- Lukewarm (the temperature region between hot and cold)
- Thermal contrast (a situation where both exist)
- Temperature shock (rapid transition between states)
- Ambivalence (metaphorically, mixed feelings)

**Your turn:** What could `bri nox` (bright + dark/night) mean?

Think about it, then try to come up with 3 interpretations.
""",
            "exercise": "bri nox",
            "hints": ["Think about things that are bright in darkness", "Consider metaphorical meanings", "What exists at the boundary?"]
        },
        2: {
            "title": "Order Independence",
            "content": """## Limn Lesson 2: Order Independence

In Limn, word order doesn't matter. This is because we're describing
a REGION in meaning-space, and intersection is commutative.

`sol aqu` = `aqu sol` (solid + water = water + solid)

Both mean: ice, glacier, frozen water

**Exercise:** Are these the same?
- `lif gro you`
- `you gro lif`
- `gro lif you`

Yes! All three describe the same intersection:
alive + growing + young = seedling, child, new venture

**Your turn:** Create 3 different orderings of: `hot mov ris`
What do they all mean?
""",
            "exercise": "hot mov ris (and permutations)",
            "hints": ["All orderings are equivalent", "Think: hot + movement + rising", "Physical and metaphorical"]
        },
        3: {
            "title": "Operators and Negation",
            "content": """## Limn Lesson 3: Operators

Operators modify the word that follows them.

`nu` = not/negation
`nu sol` = not-solid = liquid, gas, flexible, yielding

**Important:** `nu` only applies to the NEXT word.

`nu sol liq` = (not-solid) AND liquid = emphasized liquidity
`sol nu liq` = solid AND (not-liquid) = dry solid, powder, gas

**Exercise:** What's the difference between:
- `nu lif gro`
- `lif nu gro`

First: (not-alive) AND growth = machine process, crystal formation, market expansion
Second: alive AND (not-growth) = mature organism, stable population, stasis

**Your turn:** Interpret `nu hot col` vs `hot nu col`
""",
            "exercise": "nu hot col vs hot nu col",
            "hints": ["nu binds to the next word only", "Think about what changes", "Both have cold, but one negates hot"]
        }
    }

    lesson = lessons.get(level, lessons[1])
    return {
        "level": level,
        "title": lesson["title"],
        "content": lesson["content"],
        "exercise": lesson["exercise"],
        "hints": lesson["hints"]
    }


def generate_poetry(theme: str) -> dict:
    """Generate Limn poetry on a theme."""

    prompt = f"""Generate Limn poetry on the theme: "{theme}"

Create a short poem (4-6 lines of Limn) with English annotations.

Use ONLY valid Limn vocabulary. Create evocative, meaningful combinations.

Format:
## Limn Poetry: {theme}

**Title:** [limn title]

```
[line 1 in limn]
[line 2 in limn]
[line 3 in limn]
[line 4 in limn]
(optional lines 5-6)
```

---

**Annotations:**

**Line 1:** `[limn]`
([word] + [word] + ...)
[Interpretation and reflection]

**Line 2:** `[limn]`
([word] + [word] + ...)
[Interpretation and reflection]

(continue for all lines)

---

**The Poem's Journey:**
[Brief essay on what the poem expresses, how lines connect]
"""

    return {
        "theme": theme,
        "poetry_prompt": prompt
    }


# MCP Protocol handlers
class MCPServer:
    """Simple MCP server implementation."""

    def __init__(self):
        self.tools = {
            "limn_interpret": {
                "name": "limn_interpret",
                "description": "Interpret a Limn sentence. Limn is a constructed language where words are constraints and meaning is their intersection. Returns interpretations across domains, or collapsed interpretations if a key is provided.",
                "inputSchema": {
                    "type": "object",
                    "properties": {
                        "sentence": {
                            "type": "string",
                            "description": "The Limn sentence to interpret (e.g., 'sol liq tra')"
                        },
                        "key": {
                            "type": "string",
                            "description": "Optional context key to collapse ambiguity (e.g., 'chemistry', 'relationships')"
                        }
                    },
                    "required": ["sentence"]
                }
            },
            "limn_compose": {
                "name": "limn_compose",
                "description": "Create Limn sentences from an English concept. Returns multiple valid Limn translations with explanations.",
                "inputSchema": {
                    "type": "object",
                    "properties": {
                        "concept": {
                            "type": "string",
                            "description": "The English concept to express in Limn (e.g., 'a hidden truth slowly becoming known')"
                        }
                    },
                    "required": ["concept"]
                }
            },
            "limn_validate": {
                "name": "limn_validate",
                "description": "Check if a Limn sentence contains valid vocabulary words.",
                "inputSchema": {
                    "type": "object",
                    "properties": {
                        "sentence": {
                            "type": "string",
                            "description": "The Limn sentence to validate"
                        }
                    },
                    "required": ["sentence"]
                }
            },
            "limn_teach": {
                "name": "limn_teach",
                "description": "Get an interactive Limn teaching lesson.",
                "inputSchema": {
                    "type": "object",
                    "properties": {
                        "level": {
                            "type": "integer",
                            "description": "Lesson level (1-3)",
                            "default": 1
                        }
                    }
                }
            },
            "limn_poetry": {
                "name": "limn_poetry",
                "description": "Generate Limn poetry on a given theme.",
                "inputSchema": {
                    "type": "object",
                    "properties": {
                        "theme": {
                            "type": "string",
                            "description": "Theme for the poetry (e.g., 'grief', 'hope', 'transformation')"
                        }
                    },
                    "required": ["theme"]
                }
            },
            "limn_vocabulary": {
                "name": "limn_vocabulary",
                "description": "Look up a Limn word or browse vocabulary by domain.",
                "inputSchema": {
                    "type": "object",
                    "properties": {
                        "word": {
                            "type": "string",
                            "description": "Specific word to look up"
                        },
                        "domain": {
                            "type": "string",
                            "description": "Domain to browse (physical, spatial, temporal, change, life, mind, communication, social, values, operators)"
                        }
                    }
                }
            }
        }

    def handle_tool_call(self, tool_name: str, arguments: dict) -> dict:
        """Handle a tool call and return the result."""

        if tool_name == "limn_interpret":
            result = interpret_sentence(
                arguments["sentence"],
                arguments.get("key")
            )
            return {"content": [{"type": "text", "text": result["interpretation_prompt"]}]}

        elif tool_name == "limn_compose":
            result = compose_sentence(arguments["concept"])
            return {"content": [{"type": "text", "text": result["composition_prompt"]}]}

        elif tool_name == "limn_validate":
            is_valid, valid_words, unknown = validate_limn(arguments["sentence"])
            response = f"""## Validation: {arguments["sentence"]}

**Valid:** {"Yes" if is_valid else "No"}

**Recognized words:** {', '.join(valid_words) if valid_words else "None"}

**Unknown words:** {', '.join(unknown) if unknown else "None"}
"""
            if unknown:
                response += f"""
**Suggestions:** The following words are not in the Limn vocabulary: {', '.join(unknown)}

Check the vocabulary or use similar concepts:
"""
                for word in unknown:
                    response += f"- `{word}`: Try looking for a similar concept in the vocabulary\n"

            return {"content": [{"type": "text", "text": response}]}

        elif tool_name == "limn_teach":
            result = teach_lesson(arguments.get("level", 1))
            return {"content": [{"type": "text", "text": result["content"]}]}

        elif tool_name == "limn_poetry":
            result = generate_poetry(arguments["theme"])
            return {"content": [{"type": "text", "text": result["poetry_prompt"]}]}

        elif tool_name == "limn_vocabulary":
            word = arguments.get("word")
            domain = arguments.get("domain")

            if word:
                if word in ALL_WORDS:
                    info = ALL_WORDS[word]
                    response = f"""## Vocabulary: `{word}`

**Domain:** {info["domain"]}
**Meaning region:** {info["meaning"]}

**Usage examples:**
- `{word} gro` = {info["meaning"].split(',')[0]} + growth
- `nu {word}` = not-{info["meaning"].split(',')[0]}
"""
                else:
                    response = f"Word `{word}` not found in Limn vocabulary."
            elif domain:
                if domain in VOCABULARY:
                    response = f"## Vocabulary Domain: {domain}\n\n"
                    for w, m in VOCABULARY[domain].items():
                        response += f"- `{w}`: {m}\n"
                else:
                    response = f"Domain `{domain}` not found. Available: {', '.join(VOCABULARY.keys())}"
            else:
                response = "## Limn Vocabulary Domains\n\n"
                for d in VOCABULARY.keys():
                    response += f"- **{d}**: {len(VOCABULARY[d])} words\n"
                response += "\nUse `domain` parameter to browse a specific domain."

            return {"content": [{"type": "text", "text": response}]}

        return {"error": f"Unknown tool: {tool_name}"}

    def get_tools(self) -> list:
        """Return list of available tools."""
        return list(self.tools.values())


def main():
    """Main entry point for MCP server."""
    server = MCPServer()

    # Simple stdio-based MCP protocol
    for line in sys.stdin:
        try:
            request = json.loads(line)

            if request.get("method") == "tools/list":
                response = {
                    "jsonrpc": "2.0",
                    "id": request.get("id"),
                    "result": {"tools": server.get_tools()}
                }

            elif request.get("method") == "tools/call":
                params = request.get("params", {})
                tool_name = params.get("name")
                arguments = params.get("arguments", {})

                result = server.handle_tool_call(tool_name, arguments)
                response = {
                    "jsonrpc": "2.0",
                    "id": request.get("id"),
                    "result": result
                }

            else:
                response = {
                    "jsonrpc": "2.0",
                    "id": request.get("id"),
                    "error": {"code": -32601, "message": "Method not found"}
                }

            print(json.dumps(response), flush=True)

        except Exception as e:
            error_response = {
                "jsonrpc": "2.0",
                "id": None,
                "error": {"code": -32603, "message": str(e)}
            }
            print(json.dumps(error_response), flush=True)


if __name__ == "__main__":
    main()
