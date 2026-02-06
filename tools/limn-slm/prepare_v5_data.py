#!/usr/bin/env python3
"""
Prepare v5 training data for Limn SLM.

Fixes critical issues from v4:
- DECONTAMINATION: No input overlap between train/val/test
- STRATIFIED SPLITS: 80/10/10 balanced by category
- WORD PARTITIONING: 20% of vocabulary held out for Tier 3 generalization eval
- OPERATOR BALANCE: Oversampling underrepresented operators (especially ±)
- AUDIT LOG: Full provenance tracking for every example

Usage:
    cd tools/limn-slm
    python prepare_v5_data.py [--seed 42] [--held-out-frac 0.2]
"""

import argparse
import hashlib
import json
import random
import re
import string
import subprocess
import sys
from collections import Counter, defaultdict
from pathlib import Path

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------
SCRIPT_DIR = Path(__file__).resolve().parent
DATA_DIR = SCRIPT_DIR / "data" / "v5"
FINETUNING_DIR = SCRIPT_DIR.parent.parent / "finetuning"
ENGINEER_V4_DIR = Path("/home/eric/src/limntown/limn/crew/engineer/tools/limn-slm/data")
LINGUIST_DATA = Path("/home/eric/src/limntown/limn/crew/linguist/data")
TRANSLATOR_DATA = Path("/home/eric/src/limntown/limn/crew/translator")
VOCAB_DB = Path("/home/eric/src/limntown/limn/refinery/rig/data/vocabulary")

SYSTEM_PROMPT = (
    "You are a Limn language model. Limn is a constructed language where meaning "
    "emerges from constraint intersection. Every core word is exactly 3 letters "
    "(CVC: Consonant-Vowel-Consonant). Sentences use pipe | to separate constraint "
    "groups, and → for temporal/causal sequence.\n\n"
    "Six compositional operators combine words:\n"
    "@ projection: A@B = B-aspect of A (lov@fea = fear-aspect of love)\n"
    "* interference: A*B = emergent blend (sol*liq = gel)\n"
    "^ gradient: A^N = intensity 0.0-1.0 (big^0.7 = fairly big)\n"
    "\\ subtraction: A\\B = A without B (lov\\fea = love without fear)\n"
    "± superposition: A±B = quantum both/and (joy±fea = joyful-fear)\n"
    ": conditional: A:B = A given B (lov:fth = love given faith)\n\n"
    "You think, speak, and reason in Limn. Respond in Limn."
)

OPERATORS = {"@", "*", "^", "\\", "±", ":", "+-"}
COMMUTATIVE_OPS = {"*", "±", "+-"}
NON_COMMUTATIVE_OPS = {"@", "\\", ":"}
OP_NAMES = {
    "@": "projection",
    "*": "interference",
    "^": "gradient",
    "\\": "subtraction",
    "±": "superposition",
    "+-": "superposition",
    ":": "conditional",
}

# CVC pattern for extracting Limn words
LIMN_WORD_RE = re.compile(r'\b([bcdfghjklmnpqrstvwxyz][aeiou][bcdfghjklmnpqrstvwxyz])\b', re.I)
OP_RE = re.compile(r'[@*^\\±:\+\-]{1,2}')

FALSE_FRIENDS = {
    "bat": ("bathroom, wash", "battle"),
    "pet": ("petal, flower leaf", "pet animal"),
    "dan": ("dance, movement", "danger"),
    "rep": ("reptile", "report"),
    "awa": ("away, distant", "aware"),
    "wil": ("wild", "will/volition"),
    "con": ("contraction", "conscious"),
    "fre": ("freezing", "free"),
    "min": ("minimum, least", "mind"),
    "tea": ("team, group", "tea drink"),
    "sen": ("sentence", "sentient"),
}


# ---------------------------------------------------------------------------
# Dolt helpers
# ---------------------------------------------------------------------------
def dolt_query(sql: str) -> list:
    """Run a Dolt SQL query and return rows."""
    result = subprocess.run(
        ["dolt", "sql", "-q", sql, "-r", "json"],
        cwd=VOCAB_DB,
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        print(f"  Dolt error: {result.stderr.strip()}", file=sys.stderr)
        return []
    try:
        return json.loads(result.stdout)["rows"]
    except (json.JSONDecodeError, KeyError):
        return []


def load_vocabulary() -> dict:
    """Load full vocabulary from Dolt. Returns {word: {meaning, domain}}."""
    rows = dolt_query(
        "SELECT w.word, w.meaning, COALESCE(d.name, 'Unknown') as domain "
        "FROM words w LEFT JOIN domains d ON w.domain_id = d.id"
    )
    vocab = {}
    for row in rows:
        vocab[row["word"]] = {"meaning": row["meaning"], "domain": row["domain"]}
    return vocab


def load_expressions() -> list:
    """Load compositional expressions from Dolt."""
    rows = dolt_query(
        "SELECT expression, operator, left_operand, right_operand, meaning "
        "FROM compositional_expressions"
    )
    return rows


# ---------------------------------------------------------------------------
# Feature extraction
# ---------------------------------------------------------------------------
def extract_limn_words(text: str, vocab: dict) -> set:
    """Extract valid Limn words from text."""
    candidates = set(LIMN_WORD_RE.findall(text.lower()))
    return candidates & set(vocab.keys())


def extract_operators(text: str) -> set:
    """Extract compositional operators from text."""
    found = set()
    if "±" in text or "+-" in text:
        found.add("±")
    if "@" in text:
        found.add("@")
    # Only count * when it looks like an operator (word*word), not multiplication
    if re.search(r'[a-z]\*[a-z]', text):
        found.add("*")
    if "^" in text:
        found.add("^")
    if "\\" in text:
        found.add("\\")
    # Colon as operator: word:word pattern, not general punctuation
    if re.search(r'[a-z]:[a-z]', text):
        found.add(":")
    return found


def input_fingerprint(text: str) -> str:
    """Stable fingerprint for deduplication."""
    normalized = re.sub(r'\s+', ' ', text.strip().lower())
    return hashlib.md5(normalized.encode()).hexdigest()


# ---------------------------------------------------------------------------
# Unified example format
# ---------------------------------------------------------------------------
def make_example(user_content: str, assistant_content: str,
                 category: str, source: str, vocab: dict) -> dict:
    """Create a v5 training example with full metadata."""
    words_used = extract_limn_words(
        user_content + " " + assistant_content, vocab
    )
    ops_used = extract_operators(user_content + " " + assistant_content)

    return {
        "messages": [
            {"role": "system", "content": SYSTEM_PROMPT},
            {"role": "user", "content": user_content},
            {"role": "assistant", "content": assistant_content},
        ],
        "v5_meta": {
            "category": category,
            "source": source,
            "words": sorted(words_used),
            "operators": sorted(ops_used),
            "fingerprint": input_fingerprint(user_content),
        }
    }


# ---------------------------------------------------------------------------
# Data loaders
# ---------------------------------------------------------------------------
def load_v4_data(vocab: dict) -> list:
    """Load v4 chat-format data from engineer's workspace."""
    examples = []
    for fname in ["train_v4.jsonl", "eval_v4.jsonl"]:
        fpath = ENGINEER_V4_DIR / fname
        if not fpath.exists():
            print(f"  WARN: {fpath} not found, skipping")
            continue
        with open(fpath) as f:
            for line in f:
                d = json.loads(line)
                msgs = d["messages"]
                user_msg = next(
                    (m["content"] for m in msgs if m["role"] == "user"), ""
                )
                asst_msg = next(
                    (m["content"] for m in msgs if m["role"] == "assistant"), ""
                )
                category = classify_v4_example(user_msg, asst_msg)
                words = extract_limn_words(user_msg + " " + asst_msg, vocab)
                ops = extract_operators(user_msg + " " + asst_msg)
                examples.append({
                    "messages": msgs,
                    "v5_meta": {
                        "category": category,
                        "source": f"v4/{fname}",
                        "words": sorted(words),
                        "operators": sorted(ops),
                        "fingerprint": input_fingerprint(user_msg),
                    }
                })
    print(f"  Loaded {len(examples)} v4 examples")
    return examples


def classify_v4_example(user_msg: str, asst_msg: str) -> str:
    """Heuristic classification of v4 examples into categories."""
    ul = user_msg.lower()
    al = asst_msg.lower()

    if "is not a recognized limn word" in al:
        return "negative_vocab"
    if "false friend" in al or "false friend" in ul:
        return "error_correction"
    if "correct" in ul and ("no." in al[:4].lower() or "yes." in al[:5].lower()):
        return "error_correction"
    if "commutative" in al:
        return "commutativity_probe"
    if "domain" in ul and "domain:" in al:
        return "word_definition"
    if ul.startswith("mea:") and len(ul) < 20:
        return "word_definition"
    if "what does" in ul and "mean" in ul:
        if any(op in ul for op in ["@", "*", "^", "\\", "±", ":"]):
            return "operator_drilling"
        return "word_definition"
    if "operator" in ul or "projection" in al or "interference" in al:
        return "operator_drilling"
    if "translate" in ul or "english" in ul.lower() or "→" in al:
        return "translation"
    if "limn word for" in ul:
        return "word_definition"
    if "gradient" in al and "^" in (ul + al):
        return "operator_drilling"
    if "express" in ul or "haiku" in ul or "mantra" in ul:
        return "creative"
    if "parse" in ul or "precedence" in ul:
        return "error_correction"
    if any(op in ul for op in ["@", "*", "±", "+-"]):
        return "composition"
    return "other"


def load_finetuning_data(vocab: dict) -> list:
    """Load Mei's finetuning data (input/output format) and normalize to chat format."""
    examples = []
    for fname in ["limn_finetune_dataset.jsonl", "limn_synthetic_dataset.jsonl"]:
        fpath = FINETUNING_DIR / fname
        if not fpath.exists():
            print(f"  WARN: {fpath} not found, skipping")
            continue
        with open(fpath) as f:
            for line in f:
                d = json.loads(line)
                inp = d.get("input", "")
                out = d.get("output", "")
                meta_type = d.get("metadata", {}).get("type", "unknown")

                # Normalize to chat format
                category = f"finetuning_{meta_type}"
                words = extract_limn_words(inp + " " + out, vocab)
                ops = extract_operators(inp + " " + out)

                examples.append({
                    "messages": [
                        {"role": "system", "content": SYSTEM_PROMPT},
                        {"role": "user", "content": inp},
                        {"role": "assistant", "content": out},
                    ],
                    "v5_meta": {
                        "category": category,
                        "source": f"finetuning/{fname}",
                        "words": sorted(words),
                        "operators": sorted(ops),
                        "fingerprint": input_fingerprint(inp),
                    }
                })
    print(f"  Loaded {len(examples)} finetuning examples")
    return examples


# ---------------------------------------------------------------------------
# v5 data generation (new examples for operator balance)
# ---------------------------------------------------------------------------
def generate_superposition_examples(vocab: dict, expressions: list,
                                    train_words: set, count: int = 300) -> list:
    """Generate ± examples to fix critical underrepresentation."""
    examples = []
    word_list = sorted(train_words)

    # Existing ± expressions from DB
    sp_exprs = [e for e in expressions if e["operator"] in ("±", "+-")]
    for expr in sp_exprs:
        left = expr["left_operand"]
        right = expr["right_operand"]
        meaning = expr["meaning"]
        examples.append(make_example(
            f"What does {left}±{right} mean?",
            f"{left}±{right} = {meaning}. The ± (superposition) operator means "
            f"both {left} and {right} simultaneously — a quantum both/and state.",
            "operator_drilling", "v5_generated/superposition", vocab
        ))
        # Commutativity
        examples.append(make_example(
            f"Is {left}±{right} the same as {right}±{left}?",
            f"Yes. ± (superposition) is commutative — order doesn't matter. "
            f"Both mean: {meaning}.",
            "commutativity_probe", "v5_generated/superposition", vocab
        ))

    # Generate novel ± combinations from vocabulary
    domains = defaultdict(list)
    for w in word_list:
        if w in vocab:
            domains[vocab[w]["domain"]].append(w)

    # Cross-domain superpositions (most interesting)
    domain_list = [d for d in sorted(domains.keys()) if len(domains[d]) >= 3]
    for i in range(min(count, len(domain_list) * 10)):
        d1, d2 = random.sample(domain_list, 2)
        w1 = random.choice(domains[d1])
        w2 = random.choice(domains[d2])
        m1 = vocab[w1]["meaning"].split(",")[0].strip()
        m2 = vocab[w2]["meaning"].split(",")[0].strip()

        examples.append(make_example(
            f"What does {w1}±{w2} mean?",
            f"{w1}±{w2} = {m1} and {m2} simultaneously. "
            f"The ± operator holds both states in superposition — "
            f"not a blend (that's *), but genuine co-existence.",
            "operator_drilling", "v5_generated/superposition", vocab
        ))

        # Contrast with *
        examples.append(make_example(
            f"What's the difference between {w1}±{w2} and {w1}*{w2}?",
            f"{w1}±{w2} = both {m1} and {m2} existing simultaneously (superposition). "
            f"{w1}*{w2} = an emergent blend of {m1} and {m2} (interference). "
            f"± preserves both; * creates something new from both.",
            "operator_drilling", "v5_generated/superposition", vocab
        ))

    random.shuffle(examples)
    return examples[:count]


def generate_operator_balanced_examples(vocab: dict, expressions: list,
                                        train_words: set,
                                        target_per_op: int = 200) -> list:
    """Generate balanced examples for each operator."""
    examples = []
    word_list = sorted(train_words)

    op_specs = [
        ("@", "projection", "B-aspect of A", False),
        ("*", "interference", "emergent blend of A and B", True),
        ("^", "gradient", "intensity scaling", False),
        ("\\", "subtraction", "A without B", False),
        ("±", "superposition", "A and B simultaneously", True),
        (":", "conditional", "A given B", False),
    ]

    for op_char, op_name, op_desc, is_comm in op_specs:
        op_examples = []

        # From DB
        db_exprs = [e for e in expressions if e["operator"] == op_char]
        for expr in db_exprs:
            left = expr["left_operand"]
            right = expr["right_operand"]
            meaning = expr["meaning"]

            op_examples.append(make_example(
                f"mea: {expr['expression']}",
                f"{expr['expression']} = {meaning}",
                "composition", f"v5_generated/{op_name}", vocab
            ))

        # Generate novel combinations
        for _ in range(target_per_op - len(op_examples)):
            w1, w2 = random.sample(word_list, 2)
            if w1 not in vocab or w2 not in vocab:
                continue
            m1 = vocab[w1]["meaning"].split(",")[0].strip()
            m2 = vocab[w2]["meaning"].split(",")[0].strip()

            if op_char == "^":
                val = random.choice(["0", "0.2", "0.5", "0.8", "1.0"])
                expr_str = f"{w1}^{val}"
                meaning = f"{m1} at intensity {val}"
            else:
                expr_str = f"{w1}{op_char}{w2}"
                meaning = f"{op_desc.replace('A', m1).replace('B', m2)}"

            op_examples.append(make_example(
                f"What does {expr_str} mean?",
                f"{expr_str} = {meaning}. "
                f"The {op_name} operator ({op_char}) gives the {op_desc}.",
                "operator_drilling", f"v5_generated/{op_name}", vocab
            ))

        random.shuffle(op_examples)
        examples.extend(op_examples[:target_per_op])
        print(f"    {op_char} ({op_name}): {min(len(op_examples), target_per_op)} examples")

    return examples


def generate_tier2_eval(vocab: dict, expressions: list,
                        train_words: set, train_combos: set,
                        count: int = 200) -> list:
    """Generate Tier 2 eval: seen operators, UNSEEN word combinations.

    Words are individually in training data, but never combined this way.
    """
    examples = []
    word_list = sorted(train_words)
    attempts = 0

    while len(examples) < count and attempts < count * 20:
        attempts += 1
        op = random.choice(["@", "*", "\\", "±", ":"])
        w1, w2 = random.sample(word_list, 2)
        combo_key = f"{w1}{op}{w2}"

        if combo_key in train_combos:
            continue
        if w1 not in vocab or w2 not in vocab:
            continue

        m1 = vocab[w1]["meaning"].split(",")[0].strip()
        m2 = vocab[w2]["meaning"].split(",")[0].strip()
        op_name = OP_NAMES.get(op, op)

        examples.append(make_example(
            f"What does {combo_key} mean?",
            f"{combo_key} — applying {op_name} ({op}): "
            f"{m1} {op} {m2}.",
            "tier2_eval", "v5_generated/tier2", vocab
        ))
        train_combos.add(combo_key)  # Don't reuse in eval either

    return examples


def generate_tier3_eval(vocab: dict, held_out_words: set,
                        count: int = 100) -> list:
    """Generate Tier 3 eval: seen operators, UNSEEN words.

    Words never appear in ANY training example.
    """
    examples = []
    word_list = sorted(held_out_words)
    if len(word_list) < 2:
        print("  WARN: Not enough held-out words for Tier 3 eval")
        return []

    for _ in range(count):
        op = random.choice(["@", "*", "\\", "±", ":"])
        w1, w2 = random.sample(word_list, 2)

        if w1 not in vocab or w2 not in vocab:
            continue

        m1 = vocab[w1]["meaning"].split(",")[0].strip()
        m2 = vocab[w2]["meaning"].split(",")[0].strip()
        op_name = OP_NAMES.get(op, op)

        examples.append(make_example(
            f"What does {w1}{op}{w2} mean?",
            f"{w1}{op}{w2} — applying {op_name} ({op}): "
            f"{m1} {op} {m2}.",
            "tier3_eval", "v5_generated/tier3", vocab
        ))

    return examples


# ---------------------------------------------------------------------------
# Deduplication and splitting
# ---------------------------------------------------------------------------
def deduplicate(examples: list) -> list:
    """Remove duplicates by input fingerprint. Keep first occurrence."""
    seen = set()
    unique = []
    dupes = 0
    for ex in examples:
        fp = ex["v5_meta"]["fingerprint"]
        if fp in seen:
            dupes += 1
            continue
        seen.add(fp)
        unique.append(ex)
    print(f"  Deduplication: {len(examples)} → {len(unique)} ({dupes} duplicates removed)")
    return unique


def stratified_split(examples: list, train_frac: float = 0.8,
                     val_frac: float = 0.1, seed: int = 42) -> tuple:
    """Split examples into train/val/test with category stratification.

    Returns (train, val, test) lists.
    """
    rng = random.Random(seed)

    # Group by category
    by_category = defaultdict(list)
    for ex in examples:
        cat = ex["v5_meta"]["category"]
        by_category[cat].append(ex)

    train, val, test = [], [], []

    for cat, cat_examples in sorted(by_category.items()):
        rng.shuffle(cat_examples)
        n = len(cat_examples)
        n_train = max(1, int(n * train_frac))
        n_val = max(1, int(n * val_frac))

        train.extend(cat_examples[:n_train])
        val.extend(cat_examples[n_train:n_train + n_val])
        test.extend(cat_examples[n_train + n_val:])

    rng.shuffle(train)
    rng.shuffle(val)
    rng.shuffle(test)

    return train, val, test


def verify_no_leakage(train: list, val: list, test: list) -> dict:
    """Verify zero input overlap between splits."""
    train_fps = {ex["v5_meta"]["fingerprint"] for ex in train}
    val_fps = {ex["v5_meta"]["fingerprint"] for ex in val}
    test_fps = {ex["v5_meta"]["fingerprint"] for ex in test}

    tv = len(train_fps & val_fps)
    tt = len(train_fps & test_fps)
    vt = len(val_fps & test_fps)

    return {
        "train_val_overlap": tv,
        "train_test_overlap": tt,
        "val_test_overlap": vt,
        "clean": tv == 0 and tt == 0 and vt == 0,
    }


# ---------------------------------------------------------------------------
# Audit report
# ---------------------------------------------------------------------------
def generate_audit(train: list, val: list, test: list,
                   tier2: list, tier3: list,
                   vocab: dict, held_out_words: set) -> dict:
    """Generate comprehensive audit report."""
    def split_stats(examples, name):
        cats = Counter(ex["v5_meta"]["category"] for ex in examples)
        sources = Counter(ex["v5_meta"]["source"].split("/")[0] for ex in examples)
        all_ops = Counter()
        all_words = set()
        for ex in examples:
            for op in ex["v5_meta"]["operators"]:
                all_ops[op] += 1
            all_words.update(ex["v5_meta"]["words"])
        return {
            "name": name,
            "count": len(examples),
            "categories": dict(cats.most_common()),
            "sources": dict(sources.most_common()),
            "operators": dict(all_ops.most_common()),
            "unique_words": len(all_words),
        }

    return {
        "splits": {
            "train": split_stats(train, "train"),
            "val": split_stats(val, "val"),
            "test": split_stats(test, "test"),
            "tier2_eval": split_stats(tier2, "tier2_eval"),
            "tier3_eval": split_stats(tier3, "tier3_eval"),
        },
        "vocabulary": {
            "total_words": len(vocab),
            "held_out_words": len(held_out_words),
            "held_out_fraction": len(held_out_words) / max(1, len(vocab)),
        },
        "leakage_check": verify_no_leakage(train, val, test),
    }


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
def main():
    parser = argparse.ArgumentParser(description="Prepare v5 Limn training data")
    parser.add_argument("--seed", type=int, default=42, help="Random seed")
    parser.add_argument("--held-out-frac", type=float, default=0.2,
                        help="Fraction of vocabulary to hold out for Tier 3")
    parser.add_argument("--target-per-op", type=int, default=200,
                        help="Target examples per operator for balance")
    parser.add_argument("--tier2-count", type=int, default=200,
                        help="Number of Tier 2 eval examples")
    parser.add_argument("--tier3-count", type=int, default=100,
                        help="Number of Tier 3 eval examples")
    args = parser.parse_args()

    random.seed(args.seed)

    print("=" * 60)
    print("  Preparing v5 Training Data for Limn SLM")
    print("  Decontaminated • Stratified • Operator-balanced")
    print("=" * 60)
    print(f"  Seed: {args.seed}")
    print(f"  Held-out vocab fraction: {args.held_out_frac}")
    print()

    # ------------------------------------------------------------------
    # Step 1: Load vocabulary and partition words
    # ------------------------------------------------------------------
    print("[1/7] Loading vocabulary from Dolt...")
    vocab = load_vocabulary()
    print(f"  {len(vocab)} words loaded")

    expressions = load_expressions()
    print(f"  {len(expressions)} compositional expressions loaded")

    # Partition vocabulary: 80% train, 20% held-out
    all_words = sorted(vocab.keys())
    rng = random.Random(args.seed)
    rng.shuffle(all_words)
    split_idx = int(len(all_words) * (1 - args.held_out_frac))
    train_words = set(all_words[:split_idx])
    held_out_words = set(all_words[split_idx:])
    print(f"  Train words: {len(train_words)}, Held-out words: {len(held_out_words)}")

    # ------------------------------------------------------------------
    # Step 2: Load all existing data
    # ------------------------------------------------------------------
    print("\n[2/7] Loading existing data sources...")
    all_examples = []

    print("  Loading v4 data...")
    all_examples.extend(load_v4_data(vocab))

    print("  Loading finetuning data...")
    all_examples.extend(load_finetuning_data(vocab))
    print(f"  Total raw examples: {len(all_examples)}")

    # ------------------------------------------------------------------
    # Step 3: Deduplicate
    # ------------------------------------------------------------------
    print("\n[3/7] Deduplicating by input...")
    all_examples = deduplicate(all_examples)

    # ------------------------------------------------------------------
    # Step 4: Filter — separate examples by word partition
    # ------------------------------------------------------------------
    print("\n[4/7] Filtering by word partition...")
    trainable = []  # All words in this example are train words
    mixed = []      # Some train, some held-out words
    held_out_only = []  # All words are held-out

    for ex in all_examples:
        words = set(ex["v5_meta"]["words"])
        if not words:
            # No recognized Limn words — still trainable
            trainable.append(ex)
        elif words <= train_words:
            trainable.append(ex)
        elif words <= held_out_words:
            held_out_only.append(ex)
        else:
            mixed.append(ex)

    print(f"  Trainable (train words only): {len(trainable)}")
    print(f"  Mixed (some held-out words): {len(mixed)} — EXCLUDED from training")
    print(f"  Held-out only: {len(held_out_only)} — reserved for Tier 3")

    # ------------------------------------------------------------------
    # Step 5: Generate balanced operator examples
    # ------------------------------------------------------------------
    print("\n[5/7] Generating operator-balanced examples...")

    print("  Superposition (±) oversampling:")
    sp_examples = generate_superposition_examples(
        vocab, expressions, train_words, count=300
    )
    print(f"    Generated {len(sp_examples)} ± examples")

    print("  Balanced operator drilling:")
    balanced_ops = generate_operator_balanced_examples(
        vocab, expressions, train_words, target_per_op=args.target_per_op
    )
    print(f"    Generated {len(balanced_ops)} balanced operator examples")

    # Add generated examples to trainable pool
    trainable.extend(sp_examples)
    trainable.extend(balanced_ops)

    # Re-deduplicate after generation
    print("\n  Re-deduplicating after generation...")
    trainable = deduplicate(trainable)

    # ------------------------------------------------------------------
    # Step 6: Stratified split
    # ------------------------------------------------------------------
    print("\n[6/7] Stratified 80/10/10 split...")
    train, val, test = stratified_split(trainable, seed=args.seed)

    # Build set of all word combos in training for Tier 2 exclusion
    train_combos = set()
    for ex in train:
        words = ex["v5_meta"]["words"]
        ops = ex["v5_meta"]["operators"]
        for op in ops:
            for i, w1 in enumerate(words):
                for w2 in words[i+1:]:
                    train_combos.add(f"{w1}{op}{w2}")
                    train_combos.add(f"{w2}{op}{w1}")

    # Generate eval tiers
    print("  Generating Tier 2 eval (unseen combinations)...")
    tier2 = generate_tier2_eval(
        vocab, expressions, train_words, train_combos, count=args.tier2_count
    )
    print(f"    {len(tier2)} Tier 2 examples")

    print("  Generating Tier 3 eval (unseen words)...")
    tier3 = generate_tier3_eval(
        vocab, held_out_words, count=args.tier3_count
    )
    print(f"    {len(tier3)} Tier 3 examples")

    # Verify no leakage
    leakage = verify_no_leakage(train, val, test)
    print(f"\n  Leakage check: {'CLEAN ✓' if leakage['clean'] else 'CONTAMINATED ✗'}")
    if not leakage["clean"]:
        print(f"    train↔val: {leakage['train_val_overlap']}")
        print(f"    train↔test: {leakage['train_test_overlap']}")
        print(f"    val↔test: {leakage['val_test_overlap']}")

    # ------------------------------------------------------------------
    # Step 7: Save
    # ------------------------------------------------------------------
    print("\n[7/7] Saving...")
    DATA_DIR.mkdir(parents=True, exist_ok=True)

    splits = {
        "train.jsonl": train,
        "val.jsonl": val,
        "test.jsonl": test,
        "tier2_eval.jsonl": tier2,
        "tier3_eval.jsonl": tier3,
    }

    for fname, data in splits.items():
        fpath = DATA_DIR / fname
        with open(fpath, "w") as f:
            for ex in data:
                f.write(json.dumps(ex) + "\n")
        print(f"  {fname}: {len(data)} examples")

    # Save held-out word list for reference
    with open(DATA_DIR / "held_out_words.json", "w") as f:
        json.dump(sorted(held_out_words), f, indent=2)

    # Audit report
    audit = generate_audit(train, val, test, tier2, tier3, vocab, held_out_words)
    with open(DATA_DIR / "audit.json", "w") as f:
        json.dump(audit, f, indent=2)
    print(f"\n  Audit report: {DATA_DIR / 'audit.json'}")

    # ------------------------------------------------------------------
    # Summary
    # ------------------------------------------------------------------
    print("\n" + "=" * 60)
    print("  v5 Data Summary")
    print("=" * 60)
    print(f"  Train:      {len(train):6d} examples")
    print(f"  Val:        {len(val):6d} examples")
    print(f"  Test:       {len(test):6d} examples (HELD OUT — do not use during dev)")
    print(f"  Tier 2:     {len(tier2):6d} eval (unseen word combos)")
    print(f"  Tier 3:     {len(tier3):6d} eval (unseen words)")
    print(f"  {'─' * 45}")
    print(f"  Total:      {sum(len(s) for s in splits.values()):6d}")
    print()

    # Category breakdown for training
    train_cats = Counter(ex["v5_meta"]["category"] for ex in train)
    print("  Training category distribution:")
    for cat, count in train_cats.most_common():
        pct = count / len(train) * 100
        print(f"    {cat:30s} {count:5d} ({pct:5.1f}%)")

    # Operator distribution
    train_ops = Counter()
    for ex in train:
        for op in ex["v5_meta"]["operators"]:
            train_ops[op] += 1
    print("\n  Training operator distribution:")
    for op, count in train_ops.most_common():
        name = OP_NAMES.get(op, op)
        print(f"    {op} ({name:14s}): {count:5d}")

    print(f"\n  Output: {DATA_DIR}")
    print()


if __name__ == "__main__":
    main()
