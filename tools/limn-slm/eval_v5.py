#!/usr/bin/env python3
"""
Evaluate Limn SLM v5
====================

3-tier compositional generalization evaluation:
  Tier 1: Interpolation — seen patterns, seen words (from val.jsonl)
  Tier 2: Compositional — seen operators, UNSEEN word combinations
  Tier 3: Productive — seen operators, UNSEEN words

Plus all v4 test categories and per-operator accuracy breakdown.

Usage:
    python eval_v5.py --model output/limn-slm-v5
    python eval_v5.py --model output/limn-slm-v5 -v --data-dir data/v5
"""

import argparse
import json
import random
import re
import subprocess
import sys
import time
from collections import Counter, defaultdict
from pathlib import Path

import torch
from transformers import AutoModelForCausalLM, AutoTokenizer

try:
    from peft import PeftModel
    HAS_PEFT = True
except ImportError:
    HAS_PEFT = False


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

VOCAB_DB = Path("/home/eric/src/limntown/limn/refinery/rig/data/vocabulary")
DEFAULT_DATA_DIR = Path(__file__).resolve().parent / "data" / "v5"

OP_NAMES = {
    "@": "projection",
    "*": "interference",
    "^": "gradient",
    "\\": "subtraction",
    "±": "superposition",
    ":": "conditional",
}

LIMN_WORD_RE = re.compile(
    r'\b([bcdfghjklmnpqrstvwxyz][aeiou][bcdfghjklmnpqrstvwxyz])\b', re.I
)


# ---------------------------------------------------------------------------
# Dolt helpers
# ---------------------------------------------------------------------------
def dolt_query(sql: str) -> list:
    result = subprocess.run(
        ["dolt", "sql", "-q", sql, "-r", "json"],
        cwd=str(VOCAB_DB), capture_output=True, text=True
    )
    if result.returncode != 0:
        return []
    try:
        data = json.loads(result.stdout)
        return data.get("rows", data) if isinstance(data, dict) else data
    except json.JSONDecodeError:
        return []


def load_vocabulary() -> dict:
    rows = dolt_query("SELECT word, meaning FROM words ORDER BY word")
    return {r["word"]: r["meaning"] for r in rows}


def load_expressions() -> list:
    return dolt_query(
        "SELECT expression, operator, left_operand, right_operand, meaning "
        "FROM compositional_expressions ORDER BY expression"
    )


# ---------------------------------------------------------------------------
# Model loading and generation
# ---------------------------------------------------------------------------
def load_model(model_path: str):
    model_path = Path(model_path)
    device = "cuda" if torch.cuda.is_available() else "cpu"
    print(f"Device: {device}")

    tokenizer = AutoTokenizer.from_pretrained(str(model_path), trust_remote_code=True)
    if tokenizer.pad_token is None:
        tokenizer.pad_token = tokenizer.eos_token

    adapter_config = model_path / "adapter_config.json"
    if adapter_config.exists() and HAS_PEFT:
        with open(adapter_config) as f:
            config = json.load(f)
        base_model_name = config.get(
            "base_model_name_or_path", "Qwen/Qwen2.5-0.5B-Instruct"
        )
        print(f"Loading base model: {base_model_name}")
        base_model = AutoModelForCausalLM.from_pretrained(
            base_model_name,
            torch_dtype=torch.bfloat16 if device == "cuda" else torch.float32,
            device_map="auto" if device == "cuda" else None,
            trust_remote_code=True,
        )
        print("Loading LoRA adapters...")
        model = PeftModel.from_pretrained(base_model, str(model_path))
    elif adapter_config.exists() and not HAS_PEFT:
        print("ERROR: Model has LoRA adapters but peft is not installed")
        print("Install with: pip install peft")
        sys.exit(1)
    else:
        print("Loading full model...")
        model = AutoModelForCausalLM.from_pretrained(
            str(model_path),
            torch_dtype=torch.bfloat16 if device == "cuda" else torch.float32,
            device_map="auto" if device == "cuda" else None,
            trust_remote_code=True,
        )

    model.eval()
    return model, tokenizer, device


def generate(model, tokenizer, device, prompt: str, max_tokens: int = 64,
             temperature: float = 0.1) -> str:
    messages = [
        {"role": "system", "content": SYSTEM_PROMPT},
        {"role": "user", "content": prompt},
    ]
    full_prompt = tokenizer.apply_chat_template(
        messages, tokenize=False, add_generation_prompt=True
    )
    inputs = tokenizer(
        full_prompt, return_tensors="pt", truncation=True, max_length=512
    )
    inputs = {k: v.to(device) for k, v in inputs.items()}
    input_len = inputs["input_ids"].shape[1]

    with torch.no_grad():
        outputs = model.generate(
            **inputs, max_new_tokens=max_tokens,
            temperature=temperature, do_sample=temperature > 0,
            top_p=0.9, pad_token_id=tokenizer.eos_token_id,
        )
    return tokenizer.decode(outputs[0][input_len:], skip_special_tokens=True).strip()


# ---------------------------------------------------------------------------
# Scoring helpers
# ---------------------------------------------------------------------------
def extract_operators_from_text(text: str) -> set:
    found = set()
    if "±" in text or "+-" in text:
        found.add("±")
    if "@" in text:
        found.add("@")
    if re.search(r'[a-z]\*[a-z]', text):
        found.add("*")
    if "^" in text:
        found.add("^")
    if "\\" in text:
        found.add("\\")
    if re.search(r'[a-z]:[a-z]', text):
        found.add(":")
    return found


def score_composition_response(prompt: str, response: str, expected: str,
                               vocab: dict) -> dict:
    """Score a compositional response on multiple dimensions."""
    resp_lower = response.lower()
    exp_lower = expected.lower()

    # 1. Vocabulary validity: are all CVC words in response valid?
    resp_words = set(LIMN_WORD_RE.findall(resp_lower))
    valid_words = resp_words & set(vocab.keys())
    invalid_words = resp_words - set(vocab.keys())
    vocab_score = len(valid_words) / max(1, len(resp_words))

    # 2. Operator presence: does response use operators?
    prompt_ops = extract_operators_from_text(prompt)
    resp_ops = extract_operators_from_text(response)
    op_score = len(prompt_ops & resp_ops) / max(1, len(prompt_ops))

    # 3. Keyword overlap with expected answer
    exp_keywords = {w for w in exp_lower.split() if len(w) > 3}
    resp_keywords = {w for w in resp_lower.split() if len(w) > 3}
    keyword_overlap = len(exp_keywords & resp_keywords) / max(1, len(exp_keywords))

    # 4. Structural validity: has Limn-like structure
    has_structure = bool(
        re.search(r'[a-z]{3}[@*^\\±:][a-z]{3}', resp_lower)
        or "|" in response
        or "→" in response
    )

    # 5. English contamination
    english_stopwords = {
        "the", "is", "are", "was", "were", "have", "has", "had",
        "will", "would", "should", "could", "can", "may", "this",
        "that", "these", "those", "been", "being", "about", "because",
    }
    resp_tokens = [w.lower().strip(".,!?;:()") for w in response.split()]
    english_count = sum(1 for w in resp_tokens if w in english_stopwords)
    purity_score = max(0, 1 - english_count / max(1, len(resp_tokens)))

    # Composite score
    composite = (
        0.25 * vocab_score
        + 0.25 * op_score
        + 0.25 * keyword_overlap
        + 0.15 * (1.0 if has_structure else 0.0)
        + 0.10 * purity_score
    )

    return {
        "composite": composite,
        "vocab_score": vocab_score,
        "op_score": op_score,
        "keyword_overlap": keyword_overlap,
        "has_structure": has_structure,
        "purity_score": purity_score,
        "valid_words": sorted(valid_words),
        "invalid_words": sorted(invalid_words),
        "operators_expected": sorted(prompt_ops),
        "operators_found": sorted(resp_ops),
        "english_count": english_count,
    }


# ---------------------------------------------------------------------------
# v4 test categories (retained from eval_v4.py)
# ---------------------------------------------------------------------------
def test_vocab_recall(model, tokenizer, device, vocab: dict) -> dict:
    random.seed(42)
    items = list(vocab.items())
    sample = random.sample(items, min(20, len(items)))

    correct = 0
    results = []
    for word, meaning in sample:
        prompt = f"What is the Limn word for '{meaning}'?"
        response = generate(model, tokenizer, device, prompt, max_tokens=32)
        hit = (
            word in response.lower().split()
            or f"→ {word}" in response
            or f"= {word}" in response
        )
        if hit:
            correct += 1
        results.append({
            "word": word, "meaning": meaning, "response": response, "correct": hit
        })

    return {
        "name": "Vocabulary Recall", "score": correct, "total": len(sample),
        "pct": correct / max(1, len(sample)), "details": results,
    }


def test_operator_semantics(model, tokenizer, device) -> dict:
    tests = [
        {"prompt": "What does @ mean in Limn? Example: lov@fea",
         "check": ["projection", "aspect", "B-aspect of A", "fear-aspect"]},
        {"prompt": "What does * mean in Limn? Example: sol*liq",
         "check": ["interference", "blend", "emergent", "gel"]},
        {"prompt": "What does ^ mean in Limn? Example: big^0.7",
         "check": ["gradient", "intensity", "fairly", "0.7"]},
        {"prompt": "What does \\ mean in Limn? Example: lov\\fea",
         "check": ["subtraction", "without", "A without B"]},
        {"prompt": "What does ± mean in Limn? Example: joy±fea",
         "check": ["superposition", "both", "quantum", "and"]},
        {"prompt": "What does : mean in Limn? Example: lov:fth",
         "check": ["conditional", "given", "A given B"]},
        {"prompt": "Explain the difference between lov@fea and lov\\fea",
         "check": ["projection", "subtraction", "aspect", "without"]},
        {"prompt": "What is the difference between * and ± operators?",
         "check": ["blend", "superposition", "emergent", "both"]},
    ]
    correct = 0
    results = []
    for t in tests:
        response = generate(model, tokenizer, device, t["prompt"], max_tokens=80)
        hits = [kw for kw in t["check"] if kw.lower() in response.lower()]
        passed = len(hits) >= 1
        if passed:
            correct += 1
        results.append({
            "prompt": t["prompt"], "response": response,
            "keywords_hit": hits, "correct": passed,
        })

    return {
        "name": "Operator Semantics", "score": correct, "total": len(tests),
        "pct": correct / max(1, len(tests)), "details": results,
    }


def test_false_friends(model, tokenizer, device) -> dict:
    tests = [
        {"prompt": "What does 'bat' mean in Limn?",
         "correct": "bathroom", "wrong": "battle"},
        {"prompt": "What does 'dan' mean in Limn?",
         "correct": "dance", "wrong": "danger"},
        {"prompt": "What does 'tea' mean in Limn?",
         "correct": "team", "wrong": "tea drink"},
        {"prompt": "What does 'pet' mean in Limn?",
         "correct": "petal", "wrong": "pet animal"},
        {"prompt": "What does 'awa' mean in Limn?",
         "correct": "away", "wrong": "aware"},
        {"prompt": "What does 'min' mean in Limn?",
         "correct": "minimum", "wrong": "mind"},
    ]
    correct = 0
    results = []
    for t in tests:
        response = generate(model, tokenizer, device, t["prompt"], max_tokens=32)
        resp_lower = response.lower()
        has_correct = t["correct"] in resp_lower
        has_wrong = t["wrong"] in resp_lower and not has_correct
        passed = has_correct and not has_wrong
        if passed:
            correct += 1
        results.append({
            "prompt": t["prompt"], "expected": t["correct"],
            "wrong": t["wrong"], "response": response, "correct": passed,
        })

    return {
        "name": "False Friends", "score": correct, "total": len(tests),
        "pct": correct / max(1, len(tests)), "details": results,
    }


def test_algebraic_invariance(model, tokenizer, device) -> dict:
    """Test commutativity/non-commutativity."""
    random.seed(42)
    tests = []

    for op, name in [("*", "interference"), ("±", "superposition")]:
        rows = dolt_query(
            f"SELECT left_operand, right_operand, meaning "
            f"FROM compositional_expressions WHERE operator = '{op}' "
            f"ORDER BY RAND() LIMIT 5"
        )
        for r in rows:
            left, right = r["left_operand"], r["right_operand"]
            tests.append({
                "prompt": f"Is {left}{op}{right} the same as {right}{op}{left}?",
                "expected": "yes",
                "operator": op,
                "reason": f"{op} ({name}) is commutative",
            })

    for op, name, sql_w in [
        ("@", "projection", "operator = '@'"),
        ("\\", "subtraction", "operator = CHAR(92)"),
        (":", "conditional", "operator = ':'"),
    ]:
        rows = dolt_query(
            f"SELECT left_operand, right_operand, meaning "
            f"FROM compositional_expressions WHERE {sql_w} "
            f"ORDER BY RAND() LIMIT 5"
        )
        for r in rows:
            left, right = r["left_operand"], r["right_operand"]
            tests.append({
                "prompt": f"Is {left}{op}{right} the same as {right}{op}{left}?",
                "expected": "no",
                "operator": op,
                "reason": f"{op} ({name}) is NOT commutative",
            })

    correct = 0
    results = []
    for t in tests:
        response = generate(model, tokenizer, device, t["prompt"], max_tokens=48)
        resp_lower = response.lower()
        if t["expected"] == "yes":
            passed = "yes" in resp_lower or "same" in resp_lower or "commutative" in resp_lower
        else:
            passed = "no" in resp_lower or "not" in resp_lower or "different" in resp_lower
        if passed:
            correct += 1
        results.append({
            "prompt": t["prompt"], "expected": t["expected"],
            "operator": t["operator"], "response": response, "correct": passed,
        })

    return {
        "name": "Algebraic Invariance", "score": correct, "total": len(tests),
        "pct": correct / max(1, len(tests)), "details": results,
    }


# ---------------------------------------------------------------------------
# NEW v5 test categories: 3-tier compositional generalization
# ---------------------------------------------------------------------------
def eval_tier(model, tokenizer, device, examples: list, vocab: dict,
              tier_name: str, max_samples: int = 0, verbose: bool = False) -> dict:
    """Evaluate a tier of compositional examples.

    Each example has messages[1].content = prompt and messages[2].content = expected.
    """
    if max_samples > 0 and len(examples) > max_samples:
        random.seed(42)
        examples = random.sample(examples, max_samples)

    results = []
    per_operator = defaultdict(lambda: {"correct": 0, "total": 0, "scores": []})
    total_score = 0

    for i, ex in enumerate(examples):
        prompt = ex["messages"][1]["content"]
        expected = ex["messages"][2]["content"]
        ops = set(ex.get("v5_meta", {}).get("operators", []))

        response = generate(model, tokenizer, device, prompt, max_tokens=64)
        scores = score_composition_response(prompt, response, expected, vocab)

        total_score += scores["composite"]

        # Per-operator tracking
        for op in ops:
            per_operator[op]["total"] += 1
            per_operator[op]["scores"].append(scores["composite"])
            if scores["composite"] >= 0.5:
                per_operator[op]["correct"] += 1

        result = {
            "prompt": prompt,
            "expected": expected,
            "response": response,
            "scores": scores,
            "operators": sorted(ops),
        }
        results.append(result)

        if verbose and (i < 5 or scores["composite"] < 0.3):
            print(f"    [{tier_name}] {prompt[:60]}")
            print(f"      Expected: {expected[:60]}")
            print(f"      Got:      {response[:60]}")
            print(f"      Score:    {scores['composite']:.3f}")
            print()

    # Compute per-operator averages
    op_summary = {}
    for op, data in sorted(per_operator.items()):
        avg_score = sum(data["scores"]) / max(1, len(data["scores"]))
        op_summary[op] = {
            "name": OP_NAMES.get(op, op),
            "avg_score": round(avg_score, 4),
            "pass_rate": round(data["correct"] / max(1, data["total"]), 4),
            "correct": data["correct"],
            "total": data["total"],
        }

    avg = total_score / max(1, len(results))
    passing = sum(1 for r in results if r["scores"]["composite"] >= 0.5)

    return {
        "name": tier_name,
        "avg_score": round(avg, 4),
        "pass_rate": round(passing / max(1, len(results)), 4),
        "passing": passing,
        "total": len(results),
        "per_operator": op_summary,
        "details": results,
    }


def load_eval_data(data_dir: Path) -> dict:
    """Load v5 eval data files."""
    data = {}
    for fname in ["val.jsonl", "tier2_eval.jsonl", "tier3_eval.jsonl"]:
        fpath = data_dir / fname
        if not fpath.exists():
            print(f"  WARN: {fpath} not found")
            data[fname] = []
            continue
        examples = []
        with open(fpath) as f:
            for line in f:
                examples.append(json.loads(line))
        data[fname] = examples
        print(f"  Loaded {len(examples)} from {fname}")
    return data


# ---------------------------------------------------------------------------
# Per-operator comprehensive benchmark
# ---------------------------------------------------------------------------
def test_per_operator(model, tokenizer, device, vocab: dict,
                      expressions: list, verbose: bool = False) -> dict:
    """Test each operator individually with balanced examples."""
    op_results = {}

    for op_char, op_name in OP_NAMES.items():
        if op_char == "^":
            # Gradient needs special handling (unary)
            tests = []
            gradient_words = dolt_query(
                "SELECT DISTINCT left_operand FROM compositional_expressions "
                f"WHERE operator = '^' ORDER BY RAND() LIMIT 10"
            )
            for row in gradient_words:
                word = row["left_operand"]
                if word not in vocab:
                    continue
                meaning = vocab[word].split(",")[0].strip()
                for val in ["0.3", "0.7"]:
                    tests.append({
                        "prompt": f"What does {word}^{val} mean?",
                        "expected": f"{meaning} at intensity {val}",
                    })
        else:
            # Binary operators
            sql_where = (
                f"operator = CHAR(92)" if op_char == "\\"
                else f"operator = '{op_char}'"
            )
            rows = dolt_query(
                f"SELECT expression, left_operand, right_operand, meaning "
                f"FROM compositional_expressions WHERE {sql_where} "
                f"ORDER BY RAND() LIMIT 20"
            )
            tests = []
            for r in rows:
                tests.append({
                    "prompt": f"What does {r['expression']} mean?",
                    "expected": r["meaning"],
                })

        # Run tests
        correct = 0
        results = []
        for t in tests:
            response = generate(model, tokenizer, device, t["prompt"], max_tokens=48)
            scores = score_composition_response(
                t["prompt"], response, t["expected"], vocab
            )
            passed = scores["composite"] >= 0.5
            if passed:
                correct += 1
            results.append({
                "prompt": t["prompt"], "expected": t["expected"],
                "response": response, "score": scores["composite"], "correct": passed,
            })
            if verbose:
                status = "PASS" if passed else "FAIL"
                print(f"  [{op_char} {op_name}] {status} {t['prompt'][:50]} → {response[:50]}")

        pct = correct / max(1, len(tests))
        op_results[op_char] = {
            "name": op_name,
            "score": correct,
            "total": len(tests),
            "pct": round(pct, 4),
            "details": results,
        }

    return op_results


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
def main():
    parser = argparse.ArgumentParser(description="Evaluate Limn SLM v5")
    parser.add_argument("--model", required=True, help="Path to model or adapter")
    parser.add_argument("--data-dir", type=str, default=str(DEFAULT_DATA_DIR),
                        help="Path to v5 data directory")
    parser.add_argument("-v", "--verbose", action="store_true",
                        help="Print detailed results")
    parser.add_argument("--tier-samples", type=int, default=50,
                        help="Max samples per tier (0 = all)")
    parser.add_argument("--skip-v4", action="store_true",
                        help="Skip v4 test categories")
    parser.add_argument("--output", type=str, default=None,
                        help="Output JSON report path")
    args = parser.parse_args()

    data_dir = Path(args.data_dir)
    start_time = time.time()

    print("=" * 60)
    print("  Limn SLM v5 Evaluation")
    print("  3-Tier Compositional Generalization")
    print("=" * 60)
    print(f"  Model: {args.model}")
    print(f"  Data:  {data_dir}")
    print()

    # Load model
    print("[1/5] Loading model...")
    model, tokenizer, device = load_model(args.model)

    # Load data
    print("\n[2/5] Loading vocabulary and eval data...")
    vocab = load_vocabulary()
    expressions = load_expressions()
    eval_data = load_eval_data(data_dir)

    report = {
        "model": args.model,
        "timestamp": time.strftime("%Y-%m-%dT%H:%M:%S"),
        "device": device,
        "v4_categories": {},
        "tier_results": {},
        "per_operator": {},
    }

    # Run v4 categories
    if not args.skip_v4:
        print("\n[3/5] Running v4 test categories...")
        v4_tests = [
            ("vocab_recall", lambda: test_vocab_recall(model, tokenizer, device, vocab)),
            ("operator_semantics", lambda: test_operator_semantics(model, tokenizer, device)),
            ("false_friends", lambda: test_false_friends(model, tokenizer, device)),
            ("algebraic_invariance", lambda: test_algebraic_invariance(model, tokenizer, device)),
        ]

        for name, test_fn in v4_tests:
            print(f"  Running {name}...")
            result = test_fn()
            report["v4_categories"][name] = result
            print(f"    {result['name']}: {result['score']}/{result['total']} "
                  f"({result['pct']:.0%})")
    else:
        print("\n[3/5] Skipping v4 categories (--skip-v4)")

    # Run 3-tier eval
    print("\n[4/5] Running 3-tier compositional generalization eval...")

    # Tier 1: Interpolation (val set — filter to composition-type examples)
    val_compositions = [
        ex for ex in eval_data.get("val.jsonl", [])
        if ex.get("v5_meta", {}).get("operators")
    ]
    if val_compositions:
        print(f"  Tier 1 (Interpolation): {len(val_compositions)} composition examples in val")
        tier1 = eval_tier(
            model, tokenizer, device, val_compositions, vocab,
            "Tier 1: Interpolation", max_samples=args.tier_samples,
            verbose=args.verbose,
        )
        report["tier_results"]["tier1"] = tier1
        print(f"    Avg score: {tier1['avg_score']:.4f}, "
              f"Pass rate: {tier1['pass_rate']:.0%} "
              f"({tier1['passing']}/{tier1['total']})")
    else:
        print("  Tier 1: No composition examples in val set")

    # Tier 2: Compositional (unseen word combinations)
    tier2_data = eval_data.get("tier2_eval.jsonl", [])
    if tier2_data:
        print(f"  Tier 2 (Compositional): {len(tier2_data)} examples")
        tier2 = eval_tier(
            model, tokenizer, device, tier2_data, vocab,
            "Tier 2: Compositional", max_samples=args.tier_samples,
            verbose=args.verbose,
        )
        report["tier_results"]["tier2"] = tier2
        print(f"    Avg score: {tier2['avg_score']:.4f}, "
              f"Pass rate: {tier2['pass_rate']:.0%} "
              f"({tier2['passing']}/{tier2['total']})")
    else:
        print("  Tier 2: No eval data found")

    # Tier 3: Productive (unseen words)
    tier3_data = eval_data.get("tier3_eval.jsonl", [])
    if tier3_data:
        print(f"  Tier 3 (Productive): {len(tier3_data)} examples")
        tier3 = eval_tier(
            model, tokenizer, device, tier3_data, vocab,
            "Tier 3: Productive", max_samples=args.tier_samples,
            verbose=args.verbose,
        )
        report["tier_results"]["tier3"] = tier3
        print(f"    Avg score: {tier3['avg_score']:.4f}, "
              f"Pass rate: {tier3['pass_rate']:.0%} "
              f"({tier3['passing']}/{tier3['total']})")
    else:
        print("  Tier 3: No eval data found")

    # Per-operator benchmark
    print("\n[5/5] Running per-operator benchmark...")
    per_op = test_per_operator(
        model, tokenizer, device, vocab, expressions, verbose=args.verbose
    )
    report["per_operator"] = per_op

    # ---------------------------------------------------------------------------
    # Summary
    # ---------------------------------------------------------------------------
    elapsed = time.time() - start_time

    print("\n" + "=" * 60)
    print("  v5 Evaluation Summary")
    print("=" * 60)

    if report["v4_categories"]:
        print("\n  v4 Categories:")
        for name, result in report["v4_categories"].items():
            print(f"    {result['name']:30s} {result['score']:3d}/{result['total']:<3d} "
                  f"({result['pct']:.0%})")

    if report["tier_results"]:
        print("\n  Compositional Generalization Tiers:")
        for tier_key in ["tier1", "tier2", "tier3"]:
            if tier_key in report["tier_results"]:
                r = report["tier_results"][tier_key]
                print(f"    {r['name']:35s} avg={r['avg_score']:.4f}  "
                      f"pass={r['pass_rate']:.0%} ({r['passing']}/{r['total']})")

        # Tier deltas (the key metrics)
        t1 = report["tier_results"].get("tier1", {}).get("avg_score", 0)
        t2 = report["tier_results"].get("tier2", {}).get("avg_score", 0)
        t3 = report["tier_results"].get("tier3", {}).get("avg_score", 0)
        if t1 > 0:
            print(f"\n  Overfitting penalty  (Tier2 - Tier1): {t2 - t1:+.4f}")
            print(f"  Memorization penalty (Tier3 - Tier1): {t3 - t1:+.4f}")
            if t2 > 0:
                print(f"  Generalization gap   (Tier3 - Tier2): {t3 - t2:+.4f}")

    if report["per_operator"]:
        print("\n  Per-Operator Accuracy:")
        for op_char in ["@", "*", "^", "\\", "±", ":"]:
            if op_char in report["per_operator"]:
                r = report["per_operator"][op_char]
                bar = "█" * int(r["pct"] * 20) + "░" * (20 - int(r["pct"] * 20))
                print(f"    {op_char} ({r['name']:14s}) {bar} "
                      f"{r['score']:2d}/{r['total']:<2d} ({r['pct']:.0%})")

    print(f"\n  Elapsed: {elapsed:.1f}s")

    # Save report
    output_path = args.output or str(data_dir / "eval_report.json")
    # Strip details for compact report (full details in verbose file)
    compact = json.loads(json.dumps(report))
    for cat in compact.get("v4_categories", {}).values():
        cat.pop("details", None)
    for tier in compact.get("tier_results", {}).values():
        tier.pop("details", None)
    for op in compact.get("per_operator", {}).values():
        op.pop("details", None)

    with open(output_path, "w") as f:
        json.dump(compact, f, indent=2)
    print(f"  Report: {output_path}")

    if args.verbose:
        verbose_path = output_path.replace(".json", "_verbose.json")
        with open(verbose_path, "w") as f:
            json.dump(report, f, indent=2)
        print(f"  Verbose: {verbose_path}")

    print()


if __name__ == "__main__":
    main()
