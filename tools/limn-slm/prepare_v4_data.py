#!/usr/bin/env python3
"""
Prepare v4 training data for Limn SLM.

Major changes from v3 (informed by ML expert + linguistic expert reports):
- Rebalanced: 80% dict → 25% dict + 20% operator drilling + 15% compositions
- New: negative vocabulary examples (8%) — teaches "I don't know"
- New: HGTTG parallel translation pairs (10%)
- New: commutativity probes (5%)
- New: error correction examples (2%)
- Component Substitution augmentation for all 6 operators
- Expanded limn-in-limn definitions (10%)

Target: ~10,000 training examples with 90/10 train/eval split.

Usage:
    cd tools/limn-slm
    python prepare_v4_data.py
"""

import json
import random
import re
import string
import subprocess
from pathlib import Path

random.seed(42)

DATA_DIR = Path(__file__).resolve().parent / "data"
LINGUIST_DATA = Path("/home/eric/src/limntown/limn/crew/linguist/data")
LINGUIST_DOCS = Path("/home/eric/src/limntown/limn/crew/linguist/docs")
TRANSLATOR_DATA = Path("/home/eric/src/limntown/limn/crew/translator")
VOCAB_DB = Path("/home/eric/src/limntown/limn/refinery/rig/data/vocabulary")

SYSTEM_PROMPT = """You are a Limn language model. Limn is a constructed language where meaning emerges from constraint intersection. Every core word is exactly 3 letters (CVC: Consonant-Vowel-Consonant). Sentences use pipe | to separate constraint groups, and → for temporal/causal sequence.

Six compositional operators combine words:
@ projection: A@B = B-aspect of A (lov@fea = fear-aspect of love)
* interference: A*B = emergent blend (sol*liq = gel)
^ gradient: A^N = intensity 0.0-1.0 (big^0.7 = fairly big)
\\ subtraction: A\\B = A without B (lov\\fea = love without fear)
± superposition: A±B = quantum both/and (joy±fea = joyful-fear)
: conditional: A:B = A given B (lov:fth = love given faith)

You think, speak, and reason in Limn. Respond in Limn."""

# Operators and their properties
COMMUTATIVE_OPS = {"*", "±"}
NON_COMMUTATIVE_OPS = {"@", "\\", ":"}

VOWELS = set("aeiou")
CONSONANTS = set(string.ascii_lowercase) - VOWELS

# HIGH severity false friends from Quinn's guide
FALSE_FRIENDS = {
    "bat": ("bathroom, wash", "battle (use cbt for combat)"),
    "pet": ("petal, flower leaf", "petition or pet animal"),
    "dan": ("dance, movement", "danger (use dng for danger)"),
    "rep": ("reptile", "report or repeat (use rsp for response)"),
    "awa": ("away, distant", "aware (use awr for awareness)"),
    "wil": ("wild", "will/volition (use vli for volition)"),
    "con": ("contraction", "conscious (use awr for aware)"),
    "fre": ("freezing", "free (use frz for freeze)"),
    "min": ("minimum, least", "mind (use psi for mind)"),
    "tea": ("team, group", "tea drink (use tee for tea)"),
    "sen": ("sentence", "sentient (use per/awr for perceive/aware)"),
}


def dolt_query(sql: str) -> list:
    """Run a Dolt SQL query and return rows."""
    result = subprocess.run(
        ["dolt", "sql", "-q", sql, "-r", "json"],
        cwd=VOCAB_DB,
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        print(f"  Dolt error: {result.stderr.strip()}")
        return []
    try:
        return json.loads(result.stdout)["rows"]
    except (json.JSONDecodeError, KeyError):
        return []


def make_example(user_content: str, assistant_content: str) -> dict:
    """Create a training example in chat format."""
    return {
        "messages": [
            {"role": "system", "content": SYSTEM_PROMPT},
            {"role": "user", "content": user_content},
            {"role": "assistant", "content": assistant_content},
        ]
    }


# ---------------------------------------------------------------------------
# 1. WORD DEFINITIONS (target: 25%)
# ---------------------------------------------------------------------------
def generate_word_definitions(target_count: int = 2500) -> list:
    """Generate word definition examples from Dolt vocabulary.

    Uses multiple question formats per word, sampled to hit target count.
    """
    print("  Loading words from Dolt...")
    rows = dolt_query(
        "SELECT w.word, w.meaning, d.name as domain "
        "FROM words w LEFT JOIN domains d ON w.domain_id = d.id"
    )
    if not rows:
        print("  ERROR: No words loaded from Dolt!")
        return []

    print(f"  Found {len(rows)} words in DB")

    examples = []
    for row in rows:
        word = row["word"]
        meaning = row["meaning"]
        domain = row.get("domain", "Unknown")

        # Format 1: "What does X mean?"
        examples.append(make_example(
            f"What does '{word}' mean in Limn?",
            f"{word} = {meaning} (domain: {domain})"
        ))

        # Format 2: "mea: X"
        examples.append(make_example(
            f"mea: {word}",
            f"{word} = {meaning}"
        ))

        # Format 3: Domain classification
        examples.append(make_example(
            f"What domain is '{word}' in?",
            f"{word} ({meaning}) belongs to domain: {domain}"
        ))

        # Format 4: Reverse lookup (English → Limn) for subset
        if len(meaning.split(",")) <= 2:
            short_meaning = meaning.split(",")[0].strip()
            examples.append(make_example(
                f"What is the Limn word for '{short_meaning}'?",
                f"{short_meaning} → {word}"
            ))

    # Subsample to target count
    random.shuffle(examples)
    examples = examples[:target_count]
    print(f"  Generated {len(examples)} word definition examples (target: {target_count})")
    return examples


# ---------------------------------------------------------------------------
# 2. OPERATOR DRILLING + CONTRAST PAIRS (target: 20%)
# ---------------------------------------------------------------------------
def generate_operator_drilling(target_count: int = 2000) -> list:
    """Generate operator training data using component substitution.

    For each operator, systematically vary operands to teach what the
    operator DOES, not what specific pairs mean.
    """
    examples = []

    # Load existing contrast pairs from Quinn
    contrast_file = LINGUIST_DATA / "operator_contrast_pairs.jsonl"
    if contrast_file.exists():
        with open(contrast_file) as f:
            for line in f:
                line = line.strip()
                if not line:
                    continue
                pair = json.loads(line)
                examples.append(make_example(pair["q"], pair["a"]))
        print(f"  Loaded {len(examples)} existing operator contrast pairs")

    # Load expressions grouped by operator for component substitution
    for op_char, op_name, op_desc, is_comm, sql_where in [
        ("@", "projection", "B-aspect of A", False, "operator = '@'"),
        ("*", "interference", "emergent blend of A and B", True, "operator = '*'"),
        (":", "conditional", "A given B", False, "operator = ':'"),
        ("\\", "subtraction", "A without B", False, "operator = CHAR(92)"),
        ("±", "superposition", "A and B simultaneously", True, "operator = '±'"),
    ]:
        db_op = op_char
        rows = dolt_query(
            f"SELECT expression, left_operand, right_operand, meaning "
            f"FROM compositional_expressions "
            f"WHERE {sql_where} "
            f"ORDER BY RAND() LIMIT 100"
        )

        if not rows:
            continue

        # Component Substitution: vary operands, keep operator fixed
        for i, row in enumerate(rows):
            expr = row["expression"]
            left = row["left_operand"]
            right = row["right_operand"]
            meaning = row["meaning"]

            # Format 1: Direct interpretation
            examples.append(make_example(
                f"What does {expr} mean?",
                f"{expr} = {meaning}. The {op_name} operator ({db_op}) gives the {op_desc}."
            ))

            # Format 2: Operator identification
            examples.append(make_example(
                f"What operator is used in '{expr}'?",
                f"The operator is {db_op} ({op_name}). "
                f"It means {op_desc}. {expr} = {meaning}."
            ))

            # Format 3: Commutativity probes (generated below separately too)
            if i < 20:
                if is_comm:
                    reverse = f"{right}{db_op}{left}"
                    examples.append(make_example(
                        f"Is {expr} the same as {reverse}?",
                        f"Yes. {db_op} ({op_name}) is commutative — the result is "
                        f"the same regardless of order. Both mean: {meaning}."
                    ))
                else:
                    reverse = f"{right}{db_op}{left}"
                    examples.append(make_example(
                        f"Is {expr} the same as {reverse}?",
                        f"No. {db_op} ({op_name}) is NOT commutative — order matters. "
                        f"{expr} = {meaning}. {reverse} would have a different meaning."
                    ))

        print(f"  Generated {op_name} ({db_op}) drilling examples")

    # @ argument order drilling (100+ examples per expert recommendation)
    rows = dolt_query(
        "SELECT expression, left_operand, right_operand, meaning "
        "FROM compositional_expressions WHERE operator = '@' "
        "ORDER BY RAND() LIMIT 50"
    )
    for row in rows:
        expr = row["expression"]
        left = row["left_operand"]
        right = row["right_operand"]
        meaning = row["meaning"]

        # Explicit argument order teaching
        examples.append(make_example(
            f"In {expr}, what is projected onto what?",
            f"In {left}@{right}, we project {left} onto {right}. "
            f"This extracts the {right}-aspect of {left}. Result: {meaning}."
        ))

        # Reverse order contrast
        examples.append(make_example(
            f"What is the difference between {left}@{right} and {right}@{left}?",
            f"{left}@{right} = {right}-aspect of {left} ({meaning}). "
            f"{right}@{left} = {left}-aspect of {right} (different meaning). "
            f"@ (projection) is NOT commutative — order matters."
        ))

    # Gradient examples with full range
    gradient_words = dolt_query(
        "SELECT DISTINCT left_operand FROM compositional_expressions "
        "WHERE operator = '^' ORDER BY RAND() LIMIT 20"
    )
    for row in gradient_words:
        word = row["left_operand"]
        word_meaning = dolt_query(
            f"SELECT meaning FROM words WHERE word = '{word}'"
        )
        if not word_meaning:
            continue
        wm = word_meaning[0]["meaning"].split(",")[0].strip()

        for val, desc in [
            ("0", f"zero {wm} — complete absence"),
            ("0.2", f"slight {wm} — barely noticeable"),
            ("0.5", f"moderate {wm} — halfway"),
            ("0.8", f"strong {wm} — quite intense"),
            ("1.0", f"maximum {wm} — full intensity"),
        ]:
            examples.append(make_example(
                f"What does {word}^{val} mean?",
                f"{word}^{val} = {desc}. "
                f"^ (gradient) scales intensity from 0 to 1."
            ))

    # Subsample to target
    random.shuffle(examples)
    examples = examples[:target_count]
    print(f"  Total operator drilling examples: {len(examples)} (target: {target_count})")
    return examples


# ---------------------------------------------------------------------------
# 3. COMPOSITIONAL EXPRESSIONS (target: 15%)
# ---------------------------------------------------------------------------
def generate_compositional_expressions(target_count: int = 1500) -> list:
    """Load compositional expressions from Dolt in multiple formats."""
    rows = dolt_query(
        "SELECT expression, meaning, operator, left_operand, right_operand "
        "FROM compositional_expressions ORDER BY RAND() LIMIT 800"
    )

    examples = []
    for row in rows:
        expr = row["expression"]
        meaning = row["meaning"]

        # Format 1: Interpret expression
        examples.append(make_example(
            f"mea: {expr}",
            f"{expr} = {meaning}"
        ))

        # Format 2: Reverse (meaning → expression)
        examples.append(make_example(
            f"lim: {meaning}",
            f"{meaning} → {expr}"
        ))

    random.shuffle(examples)
    examples = examples[:target_count]
    print(f"  Compositional expression examples: {len(examples)} (target: {target_count})")
    return examples


# ---------------------------------------------------------------------------
# 4. LIMN-IN-LIMN DEFINITIONS (target: 10%)
# ---------------------------------------------------------------------------
def generate_limn_in_limn(target_count: int = 1000) -> list:
    """Load limn-in-limn definitions where words are defined using only Limn."""
    defs_file = LINGUIST_DATA / "limn-in-limn-definitions.md"
    if not defs_file.exists():
        print(f"  Warning: {defs_file} not found")
        return []

    examples = []
    content = defs_file.read_text()

    # Pattern: **word** (english)\n`word = definition`\n*gloss*
    # Match with optional gloss line
    pattern = r'\*\*(\w+)\*\*\s*\(([^)]+)\)\s*\n`([^`]+)`(?:\s*\n\*([^*]+)\*)?'
    matches = re.findall(pattern, content)
    print(f"  Found {len(matches)} limn-in-limn definitions in markdown")

    for word, english, definition, gloss in matches:
        if "=" not in definition:
            continue
        _, limn_def = definition.split("=", 1)
        limn_def = limn_def.strip()

        # Format 1: Limn-only definition
        examples.append(make_example(
            f"def: {word}",
            f"{word} = {limn_def}"
        ))

        # Format 2: English meaning
        examples.append(make_example(
            f"mea: {word}",
            f"{word} = {english.strip()}"
        ))

        # Format 3: Explain the self-definition (if gloss available)
        if gloss:
            examples.append(make_example(
                f"Explain the limn-in-limn definition of '{word}'",
                f"{word} ({english.strip()}) is defined in pure Limn as: "
                f"{limn_def}. Gloss: {gloss.strip()}"
            ))
        else:
            examples.append(make_example(
                f"Define '{word}' in pure Limn",
                f"{word} = {limn_def}"
            ))

    random.shuffle(examples)
    examples = examples[:target_count]
    print(f"  Limn-in-limn examples: {len(examples)} (target: {target_count})")
    return examples


# ---------------------------------------------------------------------------
# 5. HGTTG TRANSLATION PAIRS (target: 10%)
# ---------------------------------------------------------------------------
def generate_hgttg_translations(target_count: int = 1000) -> list:
    """Load parallel English-Limn pairs from HGTTG translations."""
    pairs_file = TRANSLATOR_DATA / "hgttg-training-pairs.jsonl"
    if not pairs_file.exists():
        print(f"  Warning: {pairs_file} not found")
        return []

    examples = []
    pairs = []
    with open(pairs_file) as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            pairs.append(json.loads(line))

    print(f"  Loaded {len(pairs)} HGTTG translation pairs")

    for pair in pairs:
        english = pair["english"].strip('"')
        limn = pair["limn"]
        annotations = pair.get("annotations", [])

        # Format 1: English → Limn
        examples.append(make_example(
            f"Translate to Limn: \"{english}\"",
            limn
        ))

        # Format 2: Limn → English
        examples.append(make_example(
            f"What does this Limn mean?\n{limn}",
            english
        ))

        # Format 3: Annotated translation (if annotations available)
        if annotations:
            annotation_text = ". ".join(annotations[:4])
            examples.append(make_example(
                f"Explain this Limn translation:\n{limn}",
                f"Translation of: \"{english}\"\nNotes: {annotation_text}"
            ))

    random.shuffle(examples)
    examples = examples[:target_count]
    print(f"  HGTTG translation examples: {len(examples)} (target: {target_count})")
    return examples


# ---------------------------------------------------------------------------
# 6. NEGATIVE VOCABULARY (target: 8%)
# ---------------------------------------------------------------------------
def generate_negative_vocabulary(target_count: int = 800) -> list:
    """Generate examples teaching the model to reject non-existent words.

    Critical for fixing the hallucination problem (limn-8a44).
    """
    # Get all real words
    rows = dolt_query("SELECT word FROM words")
    real_words = {r["word"] for r in rows}
    print(f"  {len(real_words)} real words loaded for negative generation")

    # Generate plausible CVC non-words
    fake_words = set()
    attempts = 0
    while len(fake_words) < 500 and attempts < 10000:
        attempts += 1
        c1 = random.choice(list(CONSONANTS))
        v = random.choice(list(VOWELS))
        c2 = random.choice(list(CONSONANTS))
        candidate = c1 + v + c2
        if candidate not in real_words and candidate not in fake_words:
            fake_words.add(candidate)

    examples = []

    for word in list(fake_words)[:300]:
        # Format 1: Direct rejection
        examples.append(make_example(
            f"What does '{word}' mean in Limn?",
            f"'{word}' is not a valid Limn word. It does not exist in the vocabulary."
        ))

        # Format 2: Meaning query
        examples.append(make_example(
            f"mea: {word}",
            f"{word} is not a recognized Limn word."
        ))

    # Also add some tricky near-misses (slightly modified real words)
    real_list = list(real_words)
    random.shuffle(real_list)
    for word in real_list[:100]:
        if len(word) != 3:
            continue
        # Swap one character
        pos = random.randint(0, 2)
        chars = list(word)
        if pos == 1:  # vowel position
            chars[pos] = random.choice(list(VOWELS - {chars[pos]}))
        else:  # consonant position
            chars[pos] = random.choice(list(CONSONANTS - {chars[pos]}))
        near_miss = "".join(chars)
        if near_miss not in real_words:
            examples.append(make_example(
                f"What does '{near_miss}' mean in Limn?",
                f"'{near_miss}' is not a valid Limn word. "
                f"Did you mean '{word}'?"
            ))

    # False friend disambiguation
    for word, (actual, expected) in FALSE_FRIENDS.items():
        examples.append(make_example(
            f"Does '{word}' mean {expected.split('(')[0].strip()} in Limn?",
            f"No. '{word}' in Limn means {actual}, NOT {expected.split('(')[0].strip()}."
        ))
        examples.append(make_example(
            f"What does '{word}' mean in Limn?",
            f"{word} = {actual} (NOT {expected.split('(')[0].strip()} — that is a false friend)."
        ))

    random.shuffle(examples)
    examples = examples[:target_count]
    print(f"  Negative vocabulary examples: {len(examples)} (target: {target_count})")
    return examples


# ---------------------------------------------------------------------------
# 7. COMMUTATIVITY PROBES (target: 5%)
# ---------------------------------------------------------------------------
def generate_commutativity_probes(target_count: int = 500) -> list:
    """Generate examples testing whether the model understands commutativity.

    * and ± are commutative; @, \\, : are not.
    """
    examples = []

    # Sample expressions for commutative operators
    for op, name in [("*", "interference"), ("±", "superposition")]:
        rows = dolt_query(
            f"SELECT expression, left_operand, right_operand, meaning "
            f"FROM compositional_expressions WHERE operator = '{op}' "
            f"ORDER BY RAND() LIMIT 40"
        )
        for row in rows:
            left = row["left_operand"]
            right = row["right_operand"]
            meaning = row["meaning"]
            fwd = f"{left}{op}{right}"
            rev = f"{right}{op}{left}"

            examples.append(make_example(
                f"Is {fwd} the same as {rev}?",
                f"Yes. {op} ({name}) is commutative. Both mean: {meaning}."
            ))

    # Sample expressions for non-commutative operators
    for op, name, sql_where in [
        ("@", "projection", "operator = '@'"),
        ("\\", "subtraction", "operator = CHAR(92)"),
        (":", "conditional", "operator = ':'"),
    ]:
        rows = dolt_query(
            f"SELECT expression, left_operand, right_operand, meaning "
            f"FROM compositional_expressions WHERE {sql_where} "
            f"ORDER BY RAND() LIMIT 40"
        )
        for row in rows:
            left = row["left_operand"]
            right = row["right_operand"]
            meaning = row["meaning"]
            fwd = f"{left}{op}{right}"
            rev = f"{right}{op}{left}"

            examples.append(make_example(
                f"Is {fwd} the same as {rev}?",
                f"No. {op} ({name}) is NOT commutative — order matters. "
                f"{fwd} = {meaning}. "
                f"{rev} would mean the {left}-aspect of {right} (different)."
            ))

    # Operator comparison probes
    comparison_prompts = [
        ("Which Limn operators are commutative?",
         "* (interference) and ± (superposition) are commutative — A*B = B*A and A±B = B±A. "
         "The other operators (@, \\, :) are NOT commutative — order matters."),
        ("What's the difference between * and ±?",
         "* (interference) merges two concepts into one emergent blend (joy*sad = bittersweet). "
         "± (superposition) holds both states simultaneously (joy±sad = simultaneously happy AND sad). "
         "* creates one feeling, ± maintains two."),
        ("What's the difference between @ and :?",
         "@ (projection) extracts an aspect: A@B = B-aspect of A (kno@tau = time-aspect of knowledge). "
         ": (conditional) adds dependency: A:B = A given B (kno:tau = knowledge given time = wisdom). "
         "@ extracts, : conditions."),
    ]
    for q, a in comparison_prompts:
        examples.append(make_example(q, a))

    random.shuffle(examples)
    examples = examples[:target_count]
    print(f"  Commutativity probe examples: {len(examples)} (target: {target_count})")
    return examples


# ---------------------------------------------------------------------------
# 8. CREATIVE GENERATION (target: 5%)
# ---------------------------------------------------------------------------
def generate_creative_examples(target_count: int = 500) -> list:
    """Generate creative Limn composition prompts."""
    examples = []

    # Emotion compositions
    emotion_prompts = [
        ("Express 'nostalgia' in Limn", "joy±sad @ mem — simultaneously happy and sad, projected onto memory. The bittersweet ache of remembering."),
        ("Express 'serenity' in Limn", "pce^0.8 | min cle | fee^0.3 — deep peace | mind clear | feeling gentle. Calm awareness."),
        ("Express 'determination' in Limn", "vli^0.9 | act:fea — strong volition | action given fear. Moving forward despite obstacles."),
        ("Express 'loneliness' in Limn", "sel\\oth | con^0.8 @ sel — self without others | connection strongly projected onto self. Awareness of absence."),
        ("Express 'wonder' in Limn", "cur^0.9 * joy^0.6 | kno^0 @ cos — curiosity blended with joy | unknowing about cosmos. Awe."),
        ("Express 'grief' in Limn", "sad^0.9 : los | lov\\ret — deep sadness given loss | love without return. The weight of absence."),
        ("Express 'hope' in Limn", "hop^0.7 | fut goo^0.5 | fea^0.2 — moderate hope | future possibly good | slight fear. Cautious optimism."),
        ("Express 'confusion' in Limn", "kno^0.1 ± ign^0.8 | pat^0 — barely knowing superposed with deep ignorance | no pattern. Lost."),
    ]
    examples.extend(make_example(q, a) for q, a in emotion_prompts)

    # Concept compositions
    concept_prompts = [
        ("How do you say 'hospital' in Limn?", "hom hel — home for healing. Place + purpose."),
        ("How do you say 'library' in Limn?", "hom boo — home for books. Place + contents."),
        ("How do you say 'prison' in Limn?", "hom pun — home for punishment. Place + function."),
        ("How do you say 'robot' in Limn?", "mac hum — machine human. Juxtaposition creates hybrid."),
        ("How do you say 'wire' in Limn?", "mtl thr — metal thread. Material + form."),
        ("How do you say 'knife' in Limn?", "mtl edg — metal edge. Material + shape."),
        ("How do you say 'umbrella' in Limn?", "cov rai — cover from rain. Function + target."),
        ("How do you say 'teacher' in Limn?", "edu hum — educating human. Role + species."),
        ("How do you say 'spy' in Limn?", "sec see — secret seeing. Manner + action."),
        ("How do you say 'curiosity' in Limn?", "wan kno — wanting knowledge. Desire + object."),
        ("How do you say 'steam' in Limn?", "aqu:bur — water given heat. Conditional expresses cause."),
        ("How do you say 'frost' in Limn?", "aqu:cld — water given cold. Conditional expresses cause."),
        ("How do you say 'snow' in Limn?", "rai:cld — rain given cold. Precipitation + temperature."),
        ("How do you say 'lava' in Limn?", "stn:bur — stone given burning. Solid + heat condition."),
        ("How do you say 'fog' in Limn?", "aer*wet — air blended with water. Interference creates phenomenon."),
        ("How do you say 'whisper' in Limn?", "say^0.2 — speech at 20% intensity. Gradient reduces force."),
        ("How do you say 'mime' in Limn?", "act\\say — acting without speaking. Subtraction removes verbal."),
        ("How do you say 'wisdom' in Limn?", "kno:tau — knowledge given time. Conditional expresses dependency."),
        ("How do you say 'courage' in Limn?", "act:fea — action given fear. Action despite fear."),
        ("How do you say 'despair' in Limn?", "hop^0 — hope at zero intensity. Complete absence."),
        ("How do you say 'anxiety' in Limn?", "fea*fut — fear blended with future. Anticipatory dread."),
        ("How do you say 'shame' in Limn?", "bad@sel — bad-aspect of self. Negative self-evaluation."),
        ("How do you say 'eternal' in Limn?", "tau\\end — time without ending. Subtraction removes finite."),
        ("How do you say 'abstract' in Limn?", "rea\\rea — real without real. Self-subtraction yields negation."),
    ]
    examples.extend(make_example(q, a) for q, a in concept_prompts)

    # Self-subtraction patterns (limn-in-limn insight)
    subtraction_prompts = [
        ("Is mov\\mov the same as res?", "Yes. mov\\mov = motion without motion = rest/stillness. Self-subtraction yields the opposite."),
        ("Is gol\\gol the same as pov?", "Yes. gol\\gol = gold without gold = poverty. Self-subtraction defines absence."),
        ("Is dng\\dng the same as saf?", "Yes. dng\\dng = danger without danger = safety. Removing danger leaves safety."),
        ("Is old\\old the same as yng?", "Yes. old\\old = old without old = young/new. Self-subtraction yields the opposite."),
        ("Is tru\\tru the same as fls?", "Yes. tru\\tru = truth without truth = false. Self-subtraction yields the opposite."),
        ("Is lgt\\lgt the same as nyt?", "Yes. lgt\\lgt = light without light = darkness. Self-subtraction yields absence."),
    ]
    examples.extend(make_example(q, a) for q, a in subtraction_prompts)

    # Write in Limn prompts
    writing_prompts = [
        ("Write a Limn mantra about learning",
         "lrn | fal | lrn agn | kno gro\n(learn | fail | learn again | knowledge grows)"),
        ("Write a Limn mantra about peace",
         "min cle | fee cal | brt ful | pce\n(mind clear | feeling calm | breath full | peace)"),
        ("Write a Limn description of sunrise",
         "nyt end → lgt beg | sun ris^0.3 | col gol spd hor\n(night ends → light begins | sun rising slowly | color gold spreads horizon)"),
        ("Write a Limn haiku about water",
         "aqu fal^0.2 | riv flo → oce\n(water falls gently | river flows → ocean)"),
    ]
    examples.extend(make_example(q, a) for q, a in writing_prompts)

    # Generate more creative prompts from DB operators
    # "How would you express X?" using random pairs from DB
    for op, op_name, sql_w in [
        ("@", "projection", "operator = '@'"),
        ("*", "interference", "operator = '*'"),
        (":", "conditional", "operator = ':'"),
        ("\\", "subtraction", "operator = CHAR(92)"),
        ("±", "superposition", "operator = '±'"),
    ]:
        rows = dolt_query(
            f"SELECT expression, meaning FROM compositional_expressions "
            f"WHERE {sql_w} AND length(meaning) > 5 "
            f"ORDER BY RAND() LIMIT 30"
        )
        for row in rows:
            expr = row["expression"]
            meaning = row["meaning"]
            examples.append(make_example(
                f"How would you express '{meaning}' in Limn?",
                f"{meaning} → {expr} (using {op_name})"
            ))

    # Generate gradient creativity from DB
    grad_rows = dolt_query(
        "SELECT expression, meaning FROM compositional_expressions "
        "WHERE operator = '^' ORDER BY RAND() LIMIT 40"
    )
    for row in grad_rows:
        examples.append(make_example(
            f"What does {row['expression']} mean?",
            f"{row['expression']} = {row['meaning']}. The ^ (gradient) scales intensity."
        ))

    # Consciousness expressions
    consciousness_prompts = [
        ("Express 'introspection' in Limn", "sel per sel — self perceives self. The recursive act of looking inward."),
        ("Express 'lucid dreaming' in Limn", "awr:dre — awareness given dreaming. Consciousness in dream state."),
        ("Express 'free will' in Limn", "vli\\lmt — volition without limits. Will unconstrained."),
        ("Express 'stream of consciousness' in Limn", "sel*thi — self-thinking blend. Where self and thought merge into flow."),
        ("Express 'philosophical zombie' in Limn", "qua\\fee — qualia without feeling. Experience-quality divorced from felt sensation."),
        ("Express 'unconscious reflex' in Limn", "cho\\awr — choice without awareness. Automatic, non-deliberate action."),
    ]
    examples.extend(make_example(q, a) for q, a in consciousness_prompts)

    random.shuffle(examples)
    examples = examples[:target_count]
    print(f"  Creative generation examples: {len(examples)} (target: {target_count})")
    return examples


# ---------------------------------------------------------------------------
# 9. ERROR CORRECTION (target: 2%)
# ---------------------------------------------------------------------------
def generate_error_correction(target_count: int = 200) -> list:
    """Generate corrupted Limn expressions for the model to identify/fix."""
    examples = []

    # Wrong operator order for non-commutative ops
    error_prompts = [
        ("Is 'sad@joy' the correct way to express 'the joy-aspect of sadness'?",
         "No. To express the joy-aspect of sadness, use sad@joy. "
         "But if you meant the sadness-aspect of joy, that would be joy@sad. "
         "@ is not commutative — check your argument order."),

        ("Someone wrote 'fea:act' to mean courage. Is this correct?",
         "No. Courage is act:fea (action given fear). "
         "fea:act means fear given action (consequences of acting). "
         "The : operator is not commutative — the order A:B means A given B."),

        ("Is lov^1.5 valid Limn?",
         "No. The ^ (gradient) operator takes values from 0.0 to 1.0 only. "
         "lov^1.5 exceeds the maximum. Use lov^1.0 for maximum intensity."),

        ("Is lov^-0.3 valid Limn?",
         "No. The ^ (gradient) operator takes values from 0.0 to 1.0 only. "
         "Negative values are not valid. Use lov^0.0 for zero intensity."),

        ("Someone wrote 'glurp' as a Limn word. Is this valid?",
         "No. 'glurp' is not a valid Limn word. Limn core words are exactly 3 letters "
         "in CVC (Consonant-Vowel-Consonant) format. 'glurp' has 5 letters."),

        ("Is 'ae' a valid Limn word?",
         "No. 'ae' is only 2 letters. Limn core words must be exactly 3 letters "
         "in CVC (Consonant-Vowel-Consonant) format."),

        ("Someone used 'dan' to mean danger. Is this correct?",
         "No. 'dan' in Limn means dance/movement, NOT danger. "
         "This is a common false friend. Use 'dng' for danger."),

        ("Someone used 'bat' to mean battle. Is this correct?",
         "No. 'bat' in Limn means bathroom/wash, NOT battle. "
         "This is a common false friend. Use 'cbt' for combat or 'fht' for fight."),

        ("Someone used 'min' to mean mind. Is this correct?",
         "No. 'min' in Limn means minimum/least, NOT mind. "
         "Use 'psi' for mind or 'nou' for nous/intellect."),

        ("Is (joy*sad = bittersweet) the same as (joy*sad^0.5)?",
         "No. joy*sad = a full blend of joy and sadness (bittersweet). "
         "joy*sad^0.5 = joy blended with sad at 50% intensity (joy with mild sadness). "
         "Without parentheses, ^ binds to sad only."),
    ]
    examples.extend(make_example(q, a) for q, a in error_prompts)

    # Precedence parsing
    precedence_prompts = [
        ("How do you parse joy*sad^0.5?",
         "joy*(sad^0.5) — ^ binds to its immediate left operand first. "
         "So sad^0.5 = mild sadness, then joy * mild_sadness = joy blended with gentle sorrow."),
        ("How do you parse joy*sad@int?",
         "(joy*sad)@int — operators bind left-to-right with equal precedence. "
         "First joy*sad = bittersweet blend, then @int = intensity-aspect of bittersweet."),
        ("How do you parse kno:tau\\end?",
         "(kno:tau)\\end — left-to-right. "
         "First kno:tau = knowledge given time (wisdom), then \\end = without ending (eternal wisdom)."),
    ]
    examples.extend(make_example(q, a) for q, a in precedence_prompts)

    # Generate more from common error patterns
    for word, (actual, expected) in list(FALSE_FRIENDS.items()):
        examples.append(make_example(
            f"Correct this: '{word} means {expected.split('(')[0].strip()}'",
            f"Incorrect. '{word}' in Limn means {actual}. "
            f"The English word suggests {expected.split('(')[0].strip()}, but that is a false friend."
        ))

    # Generate argument-swap errors from non-commutative operators
    for op, name, sql_w in [
        ("@", "projection", "operator = '@'"),
        (":", "conditional", "operator = ':'"),
        ("\\", "subtraction", "operator = CHAR(92)"),
    ]:
        rows = dolt_query(
            f"SELECT expression, left_operand, right_operand, meaning "
            f"FROM compositional_expressions WHERE {sql_w} "
            f"ORDER BY RAND() LIMIT 20"
        )
        for row in rows:
            left = row["left_operand"]
            right = row["right_operand"]
            meaning = row["meaning"]
            correct = row["expression"]
            wrong = f"{right}{op}{left}"
            examples.append(make_example(
                f"Someone wrote '{wrong}' to mean '{meaning}'. Is this correct?",
                f"No. '{meaning}' is {correct}, not {wrong}. "
                f"The {name} operator ({op}) is not commutative — order matters. "
                f"{wrong} would have a different meaning."
            ))

    random.shuffle(examples)
    examples = examples[:target_count]
    print(f"  Error correction examples: {len(examples)} (target: {target_count})")
    return examples


# ---------------------------------------------------------------------------
# MAIN
# ---------------------------------------------------------------------------
def main():
    print("=" * 60)
    print("  Preparing v4 Training Data for Limn SLM")
    print("  Expert-informed rebalanced distribution")
    print("=" * 60)
    print()

    # Generate all categories
    print("[1/8] Word Definitions (target: 25%)")
    word_defs = generate_word_definitions(2500)

    print("\n[2/8] Operator Drilling + Contrast Pairs (target: 20%)")
    operator_drill = generate_operator_drilling(2000)

    print("\n[3/8] Compositional Expressions (target: 15%)")
    compositions = generate_compositional_expressions(1500)

    print("\n[4/8] Limn-in-Limn Definitions (target: 10%)")
    limn_in_limn = generate_limn_in_limn(1000)

    print("\n[5/8] HGTTG Translation Pairs (target: 10%)")
    hgttg = generate_hgttg_translations(1000)

    print("\n[6/8] Negative Vocabulary (target: 8%)")
    negatives = generate_negative_vocabulary(800)

    print("\n[7/8] Commutativity Probes (target: 5%)")
    comm_probes = generate_commutativity_probes(500)

    print("\n[8/8] Creative Generation + Error Correction (target: 7%)")
    creative = generate_creative_examples(500)
    errors = generate_error_correction(200)

    # Combine all
    all_examples = (
        word_defs + operator_drill + compositions + limn_in_limn
        + hgttg + negatives + comm_probes + creative + errors
    )

    print("\n" + "=" * 60)
    print("  Data Distribution Summary")
    print("=" * 60)
    print(f"  Word definitions:      {len(word_defs):5d}")
    print(f"  Operator drilling:     {len(operator_drill):5d}")
    print(f"  Compositions:          {len(compositions):5d}")
    print(f"  Limn-in-limn:          {len(limn_in_limn):5d}")
    print(f"  HGTTG translations:    {len(hgttg):5d}")
    print(f"  Negative vocabulary:   {len(negatives):5d}")
    print(f"  Commutativity probes:  {len(comm_probes):5d}")
    print(f"  Creative generation:   {len(creative):5d}")
    print(f"  Error correction:      {len(errors):5d}")
    print(f"  {'─' * 35}")
    print(f"  TOTAL:                 {len(all_examples):5d}")

    # Shuffle everything together
    random.shuffle(all_examples)

    # 90/10 train/eval split
    split_idx = int(len(all_examples) * 0.9)
    train_examples = all_examples[:split_idx]
    eval_examples = all_examples[split_idx:]

    print(f"\n  Training:  {len(train_examples)} examples")
    print(f"  Eval:      {len(eval_examples)} examples")

    # Save
    v4_train = DATA_DIR / "train_v4.jsonl"
    v4_eval = DATA_DIR / "eval_v4.jsonl"

    with open(v4_train, "w") as f:
        for ex in train_examples:
            f.write(json.dumps(ex) + "\n")

    with open(v4_eval, "w") as f:
        for ex in eval_examples:
            f.write(json.dumps(ex) + "\n")

    print(f"\n  Saved to:")
    print(f"    {v4_train}")
    print(f"    {v4_eval}")

    # Category breakdown in eval for targeted testing
    print("\n  To use v4 data for training:")
    print(f"    cd tools/limn-slm")
    print(f"    ln -sf train_v4.jsonl data/train.jsonl")
    print(f"    ln -sf eval_v4.jsonl data/eval.jsonl")
    print(f"    python train.py --epochs 3 --full-finetune")


if __name__ == "__main__":
    main()
