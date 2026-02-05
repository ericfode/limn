#!/usr/bin/env python3
"""
Prepare Limn language training data for fine-tuning.

Extracts training examples from:
1. Vocabulary database (dolt)
2. Bootstrap spec (grammar + operator rules)
3. .limn literary works (parallel Limn/English)
4. Compositional operator examples
"""

import csv
import json
import os
import re
import subprocess
import random
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent.parent  # crew/engineer/
VOCAB_DIR = REPO_ROOT / "data" / "vocabulary"
SPEC_DIR = REPO_ROOT / "docs" / "spec"
OUTPUT_DIR = Path(__file__).resolve().parent / "data"
OUTPUT_DIR.mkdir(exist_ok=True)

SYSTEM_PROMPT = """You are a Limn language model. Limn is a constructed language where meaning emerges from constraint intersection. Every core word is exactly 3 letters (CVC: Consonant-Vowel-Consonant). Sentences use pipe | to separate constraint groups, and → for temporal/causal sequence.

Six compositional operators combine words:
@ projection: A@B = B-aspect of A (lov@fea = fear-aspect of love)
* interference: A*B = emergent blend (sol*liq = gel)
^ gradient: A^N = intensity 0.0-1.0 (big^0.7 = fairly big)
\\ subtraction: A\\B = A without B (lov\\fea = love without fear)
± superposition: A±B = quantum both/and (joy±fea = joyful-fear)
: conditional: A:B = A given B (lov:fth = love given faith)

You think, speak, and reason in Limn. Respond in Limn."""

DOMAINS = {}

def load_vocabulary():
    """Load vocabulary from dolt database."""
    words = []
    result = subprocess.run(
        ["dolt", "sql", "-q",
         "SELECT w.word, w.meaning, w.source, d.name as domain, w.examples "
         "FROM words w LEFT JOIN domains d ON w.domain_id = d.id ORDER BY w.word",
         "-r", "csv"],
        capture_output=True, text=True, cwd=str(VOCAB_DIR)
    )
    reader = csv.DictReader(result.stdout.strip().split("\n"))
    for row in reader:
        words.append(row)
    return words


def load_operators():
    """Load operators from dolt database."""
    ops = []
    result = subprocess.run(
        ["dolt", "sql", "-q",
         "SELECT word, op_type, precedence, description FROM operators ORDER BY word",
         "-r", "csv"],
        capture_output=True, text=True, cwd=str(VOCAB_DIR)
    )
    reader = csv.DictReader(result.stdout.strip().split("\n"))
    for row in reader:
        ops.append(row)
    return ops


def load_domains():
    """Load domain list."""
    result = subprocess.run(
        ["dolt", "sql", "-q",
         "SELECT id, name FROM domains ORDER BY id",
         "-r", "csv"],
        capture_output=True, text=True, cwd=str(VOCAB_DIR)
    )
    reader = csv.DictReader(result.stdout.strip().split("\n"))
    domains = {}
    for row in reader:
        domains[row["id"]] = row["name"]
    return domains


def parse_limn_file(filepath):
    """Extract parallel Limn/English pairs from .limn files.

    Handles multiple formats:
    1. ```limn blocks followed by > gloss lines
    2. ```limn blocks followed by **was:**/**now:** temporal readings
    3. Inline limn lines followed by > gloss lines
    4. Lines with (english gloss in parens)
    """
    pairs = []
    try:
        text = filepath.read_text(encoding="utf-8", errors="replace")
    except Exception:
        return pairs

    lines = text.split("\n")
    i = 0
    while i < len(lines):
        line = lines[i].strip()

        # Pattern 1: ```limn code block
        if line == "```limn" or line == "```limn\r":
            limn_lines = []
            j = i + 1
            while j < len(lines):
                bl = lines[j].strip()
                if bl.startswith("```"):
                    j += 1
                    break
                if bl:
                    limn_lines.append(bl)
                j += 1

            if not limn_lines:
                i = j
                continue

            # Now look for glosses after the closing ```
            # Skip blank lines
            while j < len(lines) and not lines[j].strip():
                j += 1

            gloss_lines = []

            # Check for > gloss lines
            while j < len(lines):
                gl = lines[j].strip()
                if gl.startswith("> "):
                    gloss = gl[2:].strip().lstrip("*").rstrip("*")
                    if gloss:
                        gloss_lines.append(gloss)
                    j += 1
                elif gl.startswith(">") and len(gl) > 1:
                    gloss = gl[1:].strip().lstrip("*").rstrip("*")
                    if gloss:
                        gloss_lines.append(gloss)
                    j += 1
                else:
                    break

            # If no > lines, check for **was:** or **now:** temporal readings
            if not gloss_lines:
                while j < len(lines):
                    tl = lines[j].strip()
                    if tl.startswith("**was:**") or tl.startswith("**now:**") or tl.startswith("**wil:**"):
                        # Extract the english reading
                        reading = tl.split(":**", 1)[1].strip() if ":**" in tl else ""
                        if reading:
                            gloss_lines.append(reading)
                        j += 1
                        # Check for > elaboration after temporal reading
                        while j < len(lines):
                            el = lines[j].strip()
                            if el.startswith("> "):
                                j += 1
                            elif not el:
                                j += 1
                                break
                            else:
                                break
                    elif not tl:
                        j += 1
                    elif tl.startswith("---") or tl.startswith("#") or tl.startswith("```"):
                        break
                    else:
                        j += 1

            if limn_lines and gloss_lines:
                limn_text = "\n".join(limn_lines)
                # Take the first gloss as the primary translation
                english_text = "\n".join(gloss_lines[:len(limn_lines)] if len(gloss_lines) >= len(limn_lines) else gloss_lines)
                pairs.append({"limn": limn_text, "english": english_text})
            elif limn_lines:
                # Even without gloss, save as raw limn for language modeling
                pairs.append({"limn": "\n".join(limn_lines), "english": ""})

            i = j
            continue

        # Pattern 2: Inline limn with > gloss (no code block)
        if line and not line.startswith("#") and not line.startswith(">") and not line.startswith("```") and not line.startswith("*") and not line.startswith("-"):
            has_limn_words = bool(re.search(r'\b[a-z]{3}\b.*\|.*\b[a-z]{3}\b', line))
            has_operators = bool(re.search(r'[a-z]{3}[@*^\\±:][a-z]{3}', line))

            if has_limn_words or has_operators:
                limn_lines = [line]
                j = i + 1
                while j < len(lines):
                    next_line = lines[j].strip()
                    if next_line.startswith(">"):
                        break
                    if not next_line:
                        j += 1
                        continue
                    if next_line.startswith("#") or next_line.startswith("```") or next_line.startswith("*"):
                        break
                    has_lw = bool(re.search(r'\b[a-z]{3}\b', next_line))
                    if has_lw and not next_line.startswith(">"):
                        limn_lines.append(next_line)
                        j += 1
                    else:
                        break

                gloss_lines = []
                while j < len(lines):
                    next_line = lines[j].strip()
                    if next_line.startswith("> ") or (next_line.startswith(">") and len(next_line) > 1):
                        gloss = next_line.lstrip(">").strip().lstrip("*").rstrip("*")
                        if gloss:
                            gloss_lines.append(gloss)
                        j += 1
                    else:
                        break

                if gloss_lines:
                    limn_text = "\n".join(limn_lines)
                    english_text = "\n".join(gloss_lines)
                    pairs.append({"limn": limn_text, "english": english_text})
                    i = j
                    continue

        # Pattern 3: Lines with (english) parenthetical glosses
        if line and not line.startswith("#"):
            match = re.match(r'^([a-z]{3}(?:\s+[a-z]{3})*(?:\s*\|\s*[a-z]{3}(?:\s+[a-z]{3})*)*)\s*$', line)
            if not match:
                # Check for pattern: limn | limn > english | english
                gt_match = re.match(r'^(.+?)\s*>\s*(.+)$', line)
                if gt_match:
                    limn_part = gt_match.group(1).strip()
                    eng_part = gt_match.group(2).strip()
                    has_lw = bool(re.search(r'\b[a-z]{3}\b.*\|', limn_part))
                    if has_lw:
                        pairs.append({"limn": limn_part, "english": eng_part})

        i += 1

    return pairs


def find_limn_files():
    """Find all .limn files and markdown files with Limn examples."""
    limn_files = []
    search_dirs = [
        REPO_ROOT.parent / "author" / "works",
        REPO_ROOT / "lib",
        REPO_ROOT.parent.parent / "refinery" / "rig" / "lib",
        REPO_ROOT.parent.parent / "mayor" / "rig" / "examples",
        REPO_ROOT.parent / "student" / "experiments",
    ]
    for d in search_dirs:
        if d.exists():
            for f in d.rglob("*.limn"):
                limn_files.append(f)
            # Also grab markdown files that contain ```limn blocks
            for f in d.rglob("*.md"):
                try:
                    content = f.read_text(encoding="utf-8", errors="replace")
                    if "```limn" in content:
                        limn_files.append(f)
                except Exception:
                    pass

    # Add the bootstrap spec itself — rich with examples
    spec = SPEC_DIR / "bootstrap-v4-compositional.md"
    if spec.exists():
        limn_files.append(spec)
    spec3 = SPEC_DIR / "bootstrap-v3-natural.md"
    if spec3.exists():
        limn_files.append(spec3)

    # Add CLAUDE.md files with limn mantras
    for crew_dir in (REPO_ROOT.parent).iterdir():
        if crew_dir.is_dir():
            claude_md = crew_dir / "CLAUDE.md"
            if claude_md.exists():
                try:
                    content = claude_md.read_text(encoding="utf-8", errors="replace")
                    if "```limn" in content:
                        limn_files.append(claude_md)
                except Exception:
                    pass

    return limn_files


def generate_vocab_examples(words):
    """Generate training examples from vocabulary."""
    examples = []

    for w in words:
        word = w["word"]
        meaning = w["meaning"]
        domain = w.get("domain", "")
        source = w.get("source", "")
        exs = w.get("examples", "")

        # Word → meaning
        examples.append({
            "messages": [
                {"role": "system", "content": SYSTEM_PROMPT},
                {"role": "user", "content": f"What does '{word}' mean in Limn?"},
                {"role": "assistant", "content": f"{word} = {meaning}" + (f" (domain: {domain})" if domain else "")}
            ]
        })

        # Meaning → word
        primary_meaning = meaning.split(",")[0].strip()
        examples.append({
            "messages": [
                {"role": "system", "content": SYSTEM_PROMPT},
                {"role": "user", "content": f"What is the Limn word for '{primary_meaning}'?"},
                {"role": "assistant", "content": f"{primary_meaning} → {word}"}
            ]
        })

        # Domain query
        if domain:
            examples.append({
                "messages": [
                    {"role": "system", "content": SYSTEM_PROMPT},
                    {"role": "user", "content": f"What domain is '{word}' in?"},
                    {"role": "assistant", "content": f"{word} ({meaning}) belongs to domain: {domain}"}
                ]
            })

    return examples


def generate_operator_examples():
    """Generate compositional operator training examples."""
    # These come from the bootstrap spec
    operator_data = [
        # Projection @
        ("lov@fer", "fear-component of love", "anxiety of loss", "@", "projection"),
        ("joy@sad", "sadness within joy", "bittersweet", "@", "projection"),
        ("cur@fer", "courage's relationship to fear", "facing fear", "@", "projection"),
        ("ang@fer", "fear beneath anger", "defensive reaction", "@", "projection"),
        ("hop@dbt", "doubt-component of hope", "uncertainty in hoping", "@", "projection"),
        ("trs@los", "loss-component of trust", "vulnerability of trusting", "@", "projection"),
        # Interference *
        ("sol*liq", "emergent blend of solid and liquid", "gel/glass", "*", "interference"),
        ("joy*sad", "both joy and sadness interfering", "bittersweet", "*", "interference"),
        ("lov*fer", "love and fear together", "passion/obsession", "*", "interference"),
        ("hot*col", "temperature interference", "lukewarm/thermal shock", "*", "interference"),
        ("new*old", "liminal between new and old", "transitional", "*", "interference"),
        ("win*los", "winning and losing interfering", "pyrrhic victory", "*", "interference"),
        # Gradient ^
        ("big^0.1", "barely big", "tiny", "^", "gradient"),
        ("big^0.5", "moderately big", "medium", "^", "gradient"),
        ("big^0.7", "considerably big", "large", "^", "gradient"),
        ("big^1.0", "maximally big", "huge", "^", "gradient"),
        ("lov^0.3", "slight love", "affection", "^", "gradient"),
        ("lov^0.9", "intense love", "devotion", "^", "gradient"),
        ("hot^0.2", "slightly hot", "warm", "^", "gradient"),
        ("hot^0.8", "very hot", "scalding", "^", "gradient"),
        ("sad^0.4", "moderately sad", "melancholy", "^", "gradient"),
        ("tru^0.9", "very high trust", "nearly absolute trust", "^", "gradient"),
        # Subtraction \
        ("lov\\fer", "love without fear component", "secure love", "\\", "subtraction"),
        ("joy\\sad", "joy without any sadness", "pure joy", "\\", "subtraction"),
        ("dar\\lig", "darkness without light", "void", "\\", "subtraction"),
        # Superposition ±
        ("yes±no", "both yes and no simultaneously", "undecided/ambivalent", "±", "superposition"),
        ("tru±fal", "both true and false", "paradox", "±", "superposition"),
        ("lov±hat", "love and hate coexisting", "ambivalence", "±", "superposition"),
        ("kno±unk", "knowing and not-knowing", "paradox", "±", "superposition"),
        ("pre±abs", "both present and absent", "liminal", "±", "superposition"),
        # Conditional :
        ("lov:trs", "love given trust exists", "trusting love", ":", "conditional"),
        ("fer:dan", "fear when danger is present", "appropriate fear", ":", "conditional"),
        ("joy:tim", "joy experienced over time", "nostalgia", ":", "conditional"),
        ("kno:exp", "knowledge through experience", "wisdom", ":", "conditional"),
        ("hop:los", "hope in context of loss", "resilient hope", ":", "conditional"),
        ("cal:sto", "calm given storm", "equanimity", ":", "conditional"),
    ]

    examples = []
    for expr, meaning, gloss, op, op_name in operator_data:
        # Expression → meaning
        examples.append({
            "messages": [
                {"role": "system", "content": SYSTEM_PROMPT},
                {"role": "user", "content": f"What does '{expr}' mean?"},
                {"role": "assistant", "content": f"{expr} = {meaning} ({gloss}). Operator: {op} ({op_name})"}
            ]
        })

        # Compose request
        if op == "@":
            parts = expr.split("@")
            examples.append({
                "messages": [
                    {"role": "system", "content": SYSTEM_PROMPT},
                    {"role": "user", "content": f"Extract the {parts[1]}-aspect of {parts[0]}"},
                    {"role": "assistant", "content": f"{expr} = {meaning}"}
                ]
            })
        elif op == "*":
            parts = expr.split("*")
            examples.append({
                "messages": [
                    {"role": "system", "content": SYSTEM_PROMPT},
                    {"role": "user", "content": f"What emerges from blending {parts[0]} and {parts[1]}?"},
                    {"role": "assistant", "content": f"{expr} = {meaning} ({gloss})"}
                ]
            })

    # Operator explanation examples
    for op, name, desc, ex in [
        ("@", "projection", "Extract B-component of A. Non-commutative: A@B ≠ B@A", "lov@fer = fear in love"),
        ("*", "interference", "Emergent meaning from A and B. Commutative: A*B = B*A", "sol*liq = gel"),
        ("^", "gradient", "Intensity from 0.0 to 1.0. Continuous scale.", "big^0.7 = fairly big"),
        ("\\", "subtraction", "A with B-component removed. Reveals essence.", "lov\\fer = love without fear"),
        ("±", "superposition", "Quantum both/and. Both states unresolved.", "yes±no = undecided"),
        (":", "conditional", "A in context of B. Contextualizes meaning.", "lov:trs = love given trust"),
    ]:
        examples.append({
            "messages": [
                {"role": "system", "content": SYSTEM_PROMPT},
                {"role": "user", "content": f"Explain the {op} operator in Limn."},
                {"role": "assistant", "content": f"{op} = {name}: {desc}\nExample: {ex}"}
            ]
        })

    return examples


def generate_translation_examples(pairs):
    """Generate translation training examples from parallel Limn/English pairs."""
    examples = []
    for pair in pairs:
        limn = pair["limn"].strip()
        english = pair["english"].strip()

        if not limn or not english:
            continue
        if len(limn) < 5 or len(english) < 5:
            continue

        # Limn → English
        examples.append({
            "messages": [
                {"role": "system", "content": SYSTEM_PROMPT},
                {"role": "user", "content": f"Translate to English:\n{limn}"},
                {"role": "assistant", "content": english}
            ]
        })

        # English → Limn
        examples.append({
            "messages": [
                {"role": "system", "content": SYSTEM_PROMPT},
                {"role": "user", "content": f"Express in Limn:\n{english}"},
                {"role": "assistant", "content": limn}
            ]
        })

    return examples


def generate_grammar_examples():
    """Generate grammar and structure teaching examples."""
    examples = []

    grammar_qa = [
        ("What is the word format in Limn?",
         "Core Limn words use CVC (Consonant-Vowel-Consonant) format — exactly 3 letters. Examples: lov (love), fer (fear), sol (solid), liq (liquid). The core vocabulary has 1,076 CVC words."),
        ("How does word order work in Limn?",
         "Limn is order-free within constraint groups. lov fer = fer lov. Position carries no semantic weight. Use | to separate constraint groups and → for temporal/causal sequence."),
        ("What does | mean in Limn?",
         "The pipe | separates constraint groups. Each group is a set of words that constrain meaning together. Example: nox dee | sta bri = night deep | stars bright — two separate but related observations."),
        ("What does → mean in Limn?",
         "The arrow → marks temporal or causal sequence. Unlike |, it implies ordering. Example: I sta → cha scr → dor clo = I stand → chair scrapes → door closes."),
        ("How do I negate in Limn?",
         "Use the operator nu (negation). nu lov = not-love. nu can prefix any word or phrase."),
        ("What is the operator precedence?",
         "Precedence from highest to lowest: ^ (gradient) > @ (projection) > * (interference) > \\ (subtraction) > : (conditional) > ± (superposition)."),
        ("How does Limn achieve expressiveness with only 1,076 words?",
         "Through compositional operators. 6 operators (@, *, ^, \\, ±, :) enable 30,000+ expressions from the core vocabulary — a 27.9x expressiveness multiplier. Composition handles concepts beyond the core 1,000."),
        ("What makes Limn different from English abbreviations?",
         "Limn words are NOT English abbreviations. They are semantic atoms from Latin, Greek, and phonaesthetic roots. 'lis' means listen (not list), 'des' means desire (not describe), 'res' means rest (not result). Always validate with vocab.sh."),
    ]

    for q, a in grammar_qa:
        examples.append({
            "messages": [
                {"role": "system", "content": SYSTEM_PROMPT},
                {"role": "user", "content": q},
                {"role": "assistant", "content": a}
            ]
        })

    return examples


def generate_raw_limn_examples(pairs):
    """Generate raw Limn continuation examples for language modeling."""
    examples = []

    # Group pairs into passages
    for i in range(0, len(pairs) - 1, 2):
        p1 = pairs[i]
        if i + 1 < len(pairs):
            p2 = pairs[i + 1]
            prompt_limn = p1["limn"]
            continuation = p2["limn"]
            examples.append({
                "messages": [
                    {"role": "system", "content": SYSTEM_PROMPT},
                    {"role": "user", "content": f"Continue this Limn passage:\n{prompt_limn}"},
                    {"role": "assistant", "content": continuation}
                ]
            })

    return examples


def generate_validation_examples(words):
    """Generate word validation examples."""
    examples = []
    word_set = {w["word"] for w in words}

    # Valid words
    for w in random.sample(words, min(100, len(words))):
        examples.append({
            "messages": [
                {"role": "system", "content": SYSTEM_PROMPT},
                {"role": "user", "content": f"Is '{w['word']}' a valid Limn word?"},
                {"role": "assistant", "content": f"Yes. {w['word']} = {w['meaning']}"}
            ]
        })

    # Invalid words (false friends from gotchas)
    false_friends = [
        ("lis", "listen", "list", "Use 'lis' for listen. For list/enumerate, try 'enu' or 'cat'."),
        ("des", "desire", "describe", "Use 'des' for desire. For describe, try 'dep' (depict)."),
        ("imp", "implode", "improve", "Use 'imp' for implode. For improve, try 'enh' (enhance)."),
        ("rea", "real", "read", "Use 'rea' for real. For read, try 'red'."),
        ("res", "rest", "result", "Use 'res' for rest. For result, try 'ans' or 'out'."),
        ("whe", "wheel", "where", "Use 'whe' for wheel. For where, try 'loc' (location)."),
        ("lea", "leader", "learn", "Use 'lea' for leader. For learn, try 'gro' (grow)."),
        ("lan", "land/arrive", "language", "Use 'lan' for land. For language, try 'lin' (linguistic)."),
        ("dan", "dance", "danger", "Use 'dan' for dance. For danger, try 'ris' (risk) or 'thr' (threat)."),
        ("cer", "ceremony", "certainty", "Use 'cer' for ceremony. For certainty, try 'sur' (sure)."),
    ]

    for word, actual, assumed, explanation in false_friends:
        examples.append({
            "messages": [
                {"role": "system", "content": SYSTEM_PROMPT},
                {"role": "user", "content": f"Does '{word}' mean '{assumed}' in Limn?"},
                {"role": "assistant", "content": f"No! '{word}' means '{actual}', not '{assumed}'. {explanation} This is a common false friend."}
            ]
        })

    # Random invalid 3-letter combos
    import string
    for _ in range(50):
        fake = ""
        while len(fake) != 3 or fake in word_set:
            fake = random.choice(string.ascii_lowercase) + random.choice("aeiou") + random.choice(string.ascii_lowercase)
        if fake not in word_set:
            examples.append({
                "messages": [
                    {"role": "system", "content": SYSTEM_PROMPT},
                    {"role": "user", "content": f"Is '{fake}' a valid Limn word?"},
                    {"role": "assistant", "content": f"No. '{fake}' is not in the Limn vocabulary."}
                ]
            })

    return examples


def main():
    random.seed(42)

    print("Loading vocabulary...")
    words = load_vocabulary()
    print(f"  {len(words)} words loaded")

    print("Loading operators...")
    operators = load_operators()
    print(f"  {len(operators)} operators loaded")

    print("Finding .limn files...")
    limn_files = find_limn_files()
    print(f"  {len(limn_files)} files found")

    print("Parsing parallel pairs...")
    all_pairs = []
    for f in limn_files:
        pairs = parse_limn_file(f)
        all_pairs.extend(pairs)
        if pairs:
            print(f"    {f.name}: {len(pairs)} pairs")
    print(f"  {len(all_pairs)} total pairs")

    print("\nGenerating training examples...")

    all_examples = []

    vocab_ex = generate_vocab_examples(words)
    print(f"  Vocabulary: {len(vocab_ex)} examples")
    all_examples.extend(vocab_ex)

    op_ex = generate_operator_examples()
    print(f"  Operators: {len(op_ex)} examples")
    all_examples.extend(op_ex)

    trans_ex = generate_translation_examples(all_pairs)
    print(f"  Translations: {len(trans_ex)} examples")
    all_examples.extend(trans_ex)

    grammar_ex = generate_grammar_examples()
    print(f"  Grammar: {len(grammar_ex)} examples")
    all_examples.extend(grammar_ex)

    raw_ex = generate_raw_limn_examples(all_pairs)
    print(f"  Continuations: {len(raw_ex)} examples")
    all_examples.extend(raw_ex)

    valid_ex = generate_validation_examples(words)
    print(f"  Validation: {len(valid_ex)} examples")
    all_examples.extend(valid_ex)

    # Shuffle
    random.shuffle(all_examples)

    # Split 90/10 train/eval
    split_idx = int(len(all_examples) * 0.9)
    train_data = all_examples[:split_idx]
    eval_data = all_examples[split_idx:]

    # Save
    train_path = OUTPUT_DIR / "train.jsonl"
    eval_path = OUTPUT_DIR / "eval.jsonl"

    with open(train_path, "w") as f:
        for ex in train_data:
            f.write(json.dumps(ex) + "\n")

    with open(eval_path, "w") as f:
        for ex in eval_data:
            f.write(json.dumps(ex) + "\n")

    print(f"\n=== Dataset Statistics ===")
    print(f"Total examples: {len(all_examples)}")
    print(f"Train: {len(train_data)} ({train_path})")
    print(f"Eval: {len(eval_data)} ({eval_path})")

    # Also save the bootstrap spec as raw text for potential continued pretraining
    spec_path = SPEC_DIR / "bootstrap-v4-compositional.md"
    if spec_path.exists():
        import shutil
        shutil.copy(spec_path, OUTPUT_DIR / "bootstrap-v4.md")
        print(f"Bootstrap spec copied to {OUTPUT_DIR / 'bootstrap-v4.md'}")


if __name__ == "__main__":
    main()
