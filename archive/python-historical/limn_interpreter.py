#!/usr/bin/env python3
"""
Limn Language Interpreter

A Claude Code skill for interpreting, composing, and teaching Limn -
a constraint-based constructed language where words are regions and
meaning is their intersection.

Usage:
    /limn interpret <sentence> [--key="context"]
    /limn compose "<english description>"
    /limn teach [--level=N]
    /limn validate <sentence>
    /limn poetry theme: <theme>
    /limn converse --key="<context>"
"""

import json
import re
import sys
from pathlib import Path
from dataclasses import dataclass
from typing import Optional, List, Dict, Any, Tuple
from enum import Enum


class Command(Enum):
    INTERPRET = "interpret"
    COMPOSE = "compose"
    TEACH = "teach"
    VALIDATE = "validate"
    POETRY = "poetry"
    CONVERSE = "converse"
    HELP = "help"


@dataclass
class LimnWord:
    """Represents a single Limn word with its constraint region."""
    token: str
    region: str
    examples: List[str]
    domain: str
    category: str

    def __str__(self) -> str:
        return f"{self.token} ({self.region})"


@dataclass
class LimnSentence:
    """Represents a parsed Limn sentence with scopes and operators."""
    raw: str
    tokens: List[str]
    scopes: List[List[str]]
    operators: Dict[int, str]  # position -> operator applied
    words: List[LimnWord]
    validation_errors: List[str]

    @property
    def is_valid(self) -> bool:
        return len(self.validation_errors) == 0

    def constraint_description(self) -> str:
        """Return human-readable constraint expansion."""
        parts = []
        for scope in self.scopes:
            scope_parts = []
            for token in scope:
                word = self._find_word(token)
                if word:
                    scope_parts.append(word.region)
                elif token in OPERATORS:
                    scope_parts.append(OPERATORS[token])
                else:
                    scope_parts.append(f"[{token}]")
            parts.append(" + ".join(scope_parts))
        return " | ".join(parts) if len(parts) > 1 else parts[0] if parts else ""

    def _find_word(self, token: str) -> Optional[LimnWord]:
        for word in self.words:
            if word.token == token:
                return word
        return None


# Operator definitions
OPERATORS = {
    "nu": "NOT",
    "ve": "VERY",
    "so": "SOMEWHAT",
    "ma": "MAX",
    "mi": "MIN",
    "yo": "THIS/HERE",
    "an": "THAT/THERE",
    "sa": "SAME",
    "wh": "WHAT/WHICH",
    "al": "ALL",
    "ex": "SOME",
    "on": "ONE",
    "eq": "EQUAL",
    "di": "DIFFERENT",
    "|": "BESIDE/SCOPE",
    "(": "GROUP-START",
    ")": "GROUP-END",
    ">": "TOWARD",
    "<": "FROM",
    "=": "EQUALS",
    "+": "AND",
    "-": "EXCEPT",
}


class VocabularyLoader:
    """Loads and provides access to the Limn vocabulary."""

    def __init__(self, vocab_path: Optional[Path] = None):
        self.vocab_path = vocab_path or Path(__file__).parent / "vocabulary.json"
        self.vocabulary: Dict[str, LimnWord] = {}
        self.domains: Dict[str, Any] = {}
        self._load()

    def _load(self):
        """Load vocabulary from JSON file."""
        try:
            with open(self.vocab_path, 'r', encoding='utf-8') as f:
                data = json.load(f)

            self.domains = data.get("domains", {})

            # Flatten vocabulary into lookup table
            for domain_key, domain_data in self.domains.items():
                domain_name = domain_data.get("name", domain_key)
                categories = domain_data.get("categories", {})

                for category_key, words in categories.items():
                    for token, word_data in words.items():
                        self.vocabulary[token] = LimnWord(
                            token=token,
                            region=word_data.get("region", ""),
                            examples=word_data.get("examples", []),
                            domain=domain_name,
                            category=category_key
                        )
        except FileNotFoundError:
            print(f"Warning: Vocabulary file not found at {self.vocab_path}", file=sys.stderr)
        except json.JSONDecodeError as e:
            print(f"Warning: Invalid JSON in vocabulary file: {e}", file=sys.stderr)

    def lookup(self, token: str) -> Optional[LimnWord]:
        """Look up a word in the vocabulary."""
        return self.vocabulary.get(token.lower())

    def is_operator(self, token: str) -> bool:
        """Check if token is an operator."""
        return token.lower() in OPERATORS

    def find_similar(self, token: str, limit: int = 5) -> List[str]:
        """Find similar words to an unknown token."""
        token = token.lower()
        similar = []

        # Check prefix matches
        for word in self.vocabulary:
            if word.startswith(token[:2]) or token.startswith(word[:2]):
                similar.append(word)

        return similar[:limit]

    def get_domain_words(self, domain: str) -> List[LimnWord]:
        """Get all words in a domain."""
        return [w for w in self.vocabulary.values()
                if w.domain.lower() == domain.lower()]


class LimnParser:
    """Parses Limn sentences into structured representations."""

    def __init__(self, vocabulary: VocabularyLoader):
        self.vocabulary = vocabulary

    def parse(self, sentence: str) -> LimnSentence:
        """Parse a Limn sentence into structured form."""
        raw = sentence.strip().lower()

        # Tokenize
        tokens = self._tokenize(raw)

        # Split into scopes
        scopes = self._split_scopes(tokens)

        # Identify operators and their bindings
        operators = self._identify_operators(tokens)

        # Look up words
        words = []
        validation_errors = []

        for token in tokens:
            if token in ['|', '(', ')', '>', '<', '=', '+', '-']:
                continue

            word = self.vocabulary.lookup(token)
            if word:
                words.append(word)
            elif self.vocabulary.is_operator(token):
                continue  # Operators are valid
            else:
                validation_errors.append(f"Unknown word: '{token}'")
                similar = self.vocabulary.find_similar(token)
                if similar:
                    validation_errors[-1] += f" (did you mean: {', '.join(similar)}?)"

        return LimnSentence(
            raw=raw,
            tokens=tokens,
            scopes=scopes,
            operators=operators,
            words=words,
            validation_errors=validation_errors
        )

    def _tokenize(self, text: str) -> List[str]:
        """Tokenize a Limn sentence."""
        # Handle scope operators specially
        text = text.replace('|', ' | ')
        text = text.replace('(', ' ( ')
        text = text.replace(')', ' ) ')
        text = text.replace('>', ' > ')
        text = text.replace('<', ' < ')

        # Split on whitespace
        tokens = text.split()

        # Clean tokens
        tokens = [t.strip() for t in tokens if t.strip()]

        return tokens

    def _split_scopes(self, tokens: List[str]) -> List[List[str]]:
        """Split tokens into scopes based on | delimiter."""
        scopes = []
        current_scope = []

        for token in tokens:
            if token == '|':
                if current_scope:
                    scopes.append(current_scope)
                    current_scope = []
            else:
                current_scope.append(token)

        if current_scope:
            scopes.append(current_scope)

        return scopes

    def _identify_operators(self, tokens: List[str]) -> Dict[int, str]:
        """Identify operators and their positions."""
        operators = {}
        for i, token in enumerate(tokens):
            if self.vocabulary.is_operator(token):
                operators[i] = token
        return operators


class LimnInterpreter:
    """Main interpreter for Limn commands."""

    def __init__(self):
        self.vocabulary = VocabularyLoader()
        self.parser = LimnParser(self.vocabulary)

    def execute(self, args: List[str]) -> str:
        """Execute a Limn command."""
        if not args:
            return self.help()

        command = args[0].lower()
        rest = args[1:]

        # Handle command dispatch
        if command == "interpret":
            return self.interpret(rest)
        elif command == "compose":
            return self.compose(rest)
        elif command == "teach":
            return self.teach(rest)
        elif command == "validate":
            return self.validate(rest)
        elif command == "poetry":
            return self.poetry(rest)
        elif command == "converse":
            return self.converse(rest)
        elif command == "help":
            return self.help()
        else:
            # Try to detect if it's a Limn sentence or English
            full_input = " ".join([command] + rest)
            if self._looks_like_limn(full_input):
                return self.interpret([full_input])
            else:
                return self.compose([full_input])

    def _looks_like_limn(self, text: str) -> bool:
        """Heuristic to detect if input looks like Limn."""
        words = text.lower().split()
        limn_words = sum(1 for w in words if self.vocabulary.lookup(w) or w in OPERATORS)
        return limn_words >= len(words) * 0.5

    def _extract_key(self, args: List[str]) -> Tuple[List[str], Optional[str]]:
        """Extract --key="value" from arguments."""
        key = None
        filtered_args = []

        for arg in args:
            if arg.startswith('--key='):
                key = arg[6:].strip('"\'')
            elif arg.startswith('--key'):
                continue  # Skip bare --key
            else:
                filtered_args.append(arg)

        return filtered_args, key

    def interpret(self, args: List[str]) -> str:
        """Interpret a Limn sentence."""
        args, key = self._extract_key(args)
        sentence = " ".join(args)

        if not sentence:
            return "Error: No sentence provided. Usage: /limn interpret <sentence>"

        parsed = self.parser.parse(sentence)

        output = []
        output.append(f"## Interpretations of: {parsed.raw}")
        output.append(f"({parsed.constraint_description()})")
        output.append("")

        if not parsed.is_valid:
            output.append("### Validation Warnings")
            for error in parsed.validation_errors:
                output.append(f"- {error}")
            output.append("")

        if key:
            output.append(f'### Key Collapse: "{key}"')
            output.append("")
            output.append(self._generate_key_collapse(parsed, key))
        else:
            output.append("### Without Key (Superposition)")
            output.append("")
            output.append(self._generate_interpretations(parsed))
            output.append("")
            output.append("### Core Semantic Region")
            output.append(self._generate_core_region(parsed))

        return "\n".join(output)

    def _generate_interpretations(self, parsed: LimnSentence) -> str:
        """Generate diverse interpretations for a sentence."""
        # This would ideally use the vocabulary and constraint logic
        # For now, we provide a template that Claude will fill in
        domains = ["Physics", "Chemistry", "Biology", "Psychology",
                   "Relationships", "Business", "Art", "Philosophy",
                   "Nature", "Technology"]

        lines = []
        for i, domain in enumerate(domains[:10], 1):
            lines.append(f"{i}. **{domain}:** [interpretation based on constraint intersection]")

        return "\n".join(lines)

    def _generate_key_collapse(self, parsed: LimnSentence, key: str) -> str:
        """Generate collapsed interpretation with key."""
        return f"""### Collapsed Interpretation

Applying key "{key}" to the constraint space of `{parsed.raw}`:

[Primary collapsed reading based on key context]

**Possible readings under this key:**
- [Reading 1]
- [Reading 2]
- [Reading 3]

**What the key excludes:**
[Interpretations ruled out by this context]"""

    def _generate_core_region(self, parsed: LimnSentence) -> str:
        """Generate the core semantic region description."""
        return f"The intersection of all readings: *[the fundamental meaning where all constraint regions overlap]*"

    def compose(self, args: List[str]) -> str:
        """Compose a Limn sentence from English."""
        description = " ".join(args).strip('"\'')

        if not description:
            return "Error: No description provided. Usage: /limn compose \"<english description>\""

        output = []
        output.append(f'## Composing: "{description}"')
        output.append("")
        output.append("### Proposed Limn Sentences")
        output.append("")
        output.append("**Option 1:** `[limn sentence]`")
        output.append("- [constraint breakdown]")
        output.append("- [explanation of why this captures the concept]")
        output.append("")
        output.append("**Option 2:** `[alternative limn sentence]`")
        output.append("- [constraint breakdown]")
        output.append("- [different emphasis or structure]")
        output.append("")
        output.append("**Option 3:** `[third option with scope operators]`")
        output.append("- [using | to separate distinct elements]")
        output.append("")
        output.append("**Recommendation:** [which option best captures the concept and why]")

        return "\n".join(output)

    def teach(self, args: List[str]) -> str:
        """Enter teaching mode."""
        level = 1
        for arg in args:
            if arg.startswith('--level='):
                try:
                    level = int(arg[8:])
                except ValueError:
                    pass

        lessons = {
            1: self._lesson_basic_intersection(),
            2: self._lesson_three_word(),
            3: self._lesson_operators(),
            4: self._lesson_scope(),
            5: self._lesson_complex()
        }

        return lessons.get(level, lessons[1])

    def _lesson_basic_intersection(self) -> str:
        return """## Limn Lesson 1: Basic Intersection

Let's start with two-word combinations.

In Limn, each word defines a *region* of meaning. When you combine words, you find where those regions *intersect*.

**Exercise 1:** What could `hot col` mean?

This seems contradictory - hot AND cold. But think about what exists in that intersection...

*Hint: Consider temperature ranges, sensations, or metaphorical readings.*

---

When you're ready, share your thoughts and I'll provide feedback.

**Vocabulary for this lesson:**
- `hot` = hot, thermal (fire, sun, fever, passion)
- `col` = cold, frozen (ice, winter, death, numb)
- `sol` = solid, rigid (rock, bone, ice, metal)
- `liq` = liquid, flowing (water, blood, oil)
- `bri` = bright, luminous (sun, star, idea, joy)
- `dim` = dim, faint (twilight, memory, whisper)"""

    def _lesson_three_word(self) -> str:
        return """## Limn Lesson 2: Three-Word Combinations

Now let's add a third constraint to narrow the meaning further.

**Exercise:** Interpret `sol liq tra`

Breaking it down:
- `sol` = solid
- `liq` = liquid
- `tra` = transformation

What exists at the intersection of solid + liquid + transformation?

*Hint: Think about phase changes, transitions, boundary states.*

---

Try to generate at least 5 different readings across different domains (physics, emotion, society, etc.)."""

    def _lesson_operators(self) -> str:
        return """## Limn Lesson 3: Operators

Operators modify words. The most important is `nu` (not/negation).

**Key Rule:** Operators bind to the *next word only*.

```
nu sol liq = (NOT solid) AND liquid = pure liquid, flowing freely
sol nu liq = solid AND (NOT liquid) = dry solid, no moisture
```

**Exercise:** What's the difference between:
1. `nu lif gro`
2. `lif nu gro`

---

Other operators:
- `ve` = very (intensifier)
- `so` = somewhat (weakener)
- `yo` = this/here (proximal)
- `an` = that/there (distal)"""

    def _lesson_scope(self) -> str:
        return """## Limn Lesson 4: Scope Boundaries

Use `|` to separate entities or create distinct constraint groups.

Without `|`:
```
sol abo liq bel = solid + above + liquid + below
                = one entity that is somehow all these things (nearly impossible)
```

With `|`:
```
sol abo | liq bel = (solid + above) BESIDE (liquid + below)
                  = cliff over river, ice over water, rock above pool
```

**Exercise:** Create a sentence meaning "bright hope here, dim fear there"

*Hint: Use `yo` (here) and `an` (there) with the scope operator.*

---

Build your sentence and explain its structure."""

    def _lesson_complex(self) -> str:
        return """## Limn Lesson 5: Complex Compositions

Now let's compose full Limn expressions for complex concepts.

**Challenge:** Express "a hidden truth slowly becoming known"

Consider:
- What constraints capture "hidden truth"?
- What captures "slowly"?
- What captures "becoming known"?
- Should you use scope boundaries?

**Vocabulary hints:**
- `hid` = hiding, concealed
- `tru` = true, truth
- `kno` = knowing, knowledge
- `slo` = [construct with operators: `mi acc` = minimal acceleration = slow]
- `eme` = emergence, appearing
- `cle` = clear, obvious

---

Create 2-3 different compositions and explain your choices."""

    def validate(self, args: List[str]) -> str:
        """Validate a Limn sentence."""
        sentence = " ".join(args)

        if not sentence:
            return "Error: No sentence provided. Usage: /limn validate <sentence>"

        parsed = self.parser.parse(sentence)

        output = []
        output.append(f"## Validation: {parsed.raw}")
        output.append("")

        if parsed.is_valid:
            output.append("**Status:** Valid Limn sentence")
            output.append("")
            output.append(f"**Constraint expansion:** {parsed.constraint_description()}")
            output.append("")
            output.append("**Word breakdown:**")
            for word in parsed.words:
                output.append(f"- `{word.token}` = {word.region} ({word.domain})")
        else:
            output.append("**Status:** Invalid - corrections needed")
            output.append("")
            output.append("**Issues found:**")
            for error in parsed.validation_errors:
                output.append(f"- {error}")
            output.append("")
            output.append("**Suggestions:**")
            output.append("[Corrected sentence with explanations]")

        return "\n".join(output)

    def poetry(self, args: List[str]) -> str:
        """Generate Limn poetry."""
        # Extract theme
        theme_text = " ".join(args)
        theme = theme_text.replace("theme:", "").strip()

        if not theme:
            return "Error: No theme provided. Usage: /limn poetry theme: <theme>"

        return f"""## Limn Poetry: {theme.title()}

**Title:** [evocative limn phrase]

[line 1: limn sentence]
[line 2: limn sentence]
[line 3: limn sentence]
[line 4: limn sentence with ???]

---

**Annotations:**

**Line 1:** `[limn]`
([constraint breakdown])
[What this line evokes, its multiple readings]

**Line 2:** `[limn]`
([constraint breakdown])
[Meaning and connection to theme]

**Line 3:** `[limn]`
([constraint breakdown])
[The turn or deepening]

**Line 4:** `[limn]`
([constraint breakdown])
[The question left open, the silence that speaks]

---

*The poem sits with {theme}, not solving it but inhabiting it.*"""

    def converse(self, args: List[str]) -> str:
        """Enter conversation mode."""
        args, key = self._extract_key(args)

        if not key:
            return """Error: Conversation mode requires a key.
Usage: /limn converse --key="<your interpretive lens>"

Example: /limn converse --key="late night thoughts about meaning"

The key establishes the shared context for our Limn dialogue."""

        return f"""## Limn Conversation Mode

**Key:** "{key}"

---

I am now in conversation mode. I will respond in Limn with annotations.

Your key - "{key}" - establishes our shared context. My Limn responses will be interpreted through this lens.

*Speak to me in English. I'll respond in Limn, then translate.*

---

[waiting for your message]

gre | cur lis | yo an bet mea eme

[greeting. curious listening. between us, meaning emerges.]"""

    def help(self) -> str:
        """Show help information."""
        return """## Limn Language Interpreter

**Usage:** `/limn <command> [arguments]`

### Commands

**interpret** - Interpret a Limn sentence
```
/limn interpret sol liq tra
/limn interpret gro exp bri --key="astronomy"
```

**compose** - Create Limn from English
```
/limn compose "a hidden truth slowly becoming known"
```

**teach** - Interactive lessons
```
/limn teach
/limn teach --level=3
```

**validate** - Check sentence validity
```
/limn validate sol liq hot
```

**poetry** - Generate Limn poetry
```
/limn poetry theme: grief
```

**converse** - Limn conversation mode
```
/limn converse --key="late night thoughts"
```

### Quick Start

Just type a Limn sentence:
```
/limn sol liq tra
```

Or describe a concept in English:
```
/limn "water becoming ice"
```

### Core Principles

1. Words are regions, not points
2. Sentences are intersections
3. Order is irrelevant
4. Keys collapse ambiguity
5. Operators bind locally

### Essential Operators

- `nu` = not (negation)
- `ve` = very (intensifier)
- `so` = somewhat (weakener)
- `yo` = this/here
- `an` = that/there
- `|` = scope boundary"""


def main():
    """Main entry point for CLI usage."""
    interpreter = LimnInterpreter()

    if len(sys.argv) < 2:
        print(interpreter.help())
        return

    args = sys.argv[1:]
    result = interpreter.execute(args)
    print(result)


if __name__ == "__main__":
    main()
