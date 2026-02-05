#!/usr/bin/env python3
"""
Evaluate Limn SLM v3
====================

Structured evaluation of the fine-tuned Limn model covering:
1. Vocabulary recall (word ↔ meaning)
2. Domain classification
3. Operator semantics (v1 weakness)
4. Novel composition
5. False friends
6. Limn-only prompts (no English)
7. Creative generation

Loads LoRA adapters properly. Pulls vocab from Dolt DB.

Usage:
    python eval_v3.py --model output/limn-slm-v3/limn-slm-final
    python eval_v3.py --model output/limn-slm-v3/checkpoints/checkpoint-2300
"""

import argparse
import json
import subprocess
import sys
import time
from pathlib import Path
from typing import Optional

import torch
from transformers import AutoModelForCausalLM, AutoTokenizer
from peft import PeftModel


SYSTEM_PROMPT = """You are a Limn language model. Limn is a constructed language where meaning emerges from constraint intersection. Every core word is exactly 3 letters (CVC: Consonant-Vowel-Consonant). Sentences use pipe | to separate constraint groups, and → for temporal/causal sequence.

Six compositional operators combine words:
@ projection: A@B = B-aspect of A (lov@fea = fear-aspect of love)
* interference: A*B = emergent blend (sol*liq = gel)
^ gradient: A^N = intensity 0.0-1.0 (big^0.7 = fairly big)
\\ subtraction: A\\B = A without B (lov\\fea = love without fear)
± superposition: A±B = quantum both/and (joy±fea = joyful-fear)
: conditional: A:B = A given B (lov:fth = love given faith)

You think, speak, and reason in Limn. Respond in Limn."""

VOCAB_DB = Path("/home/eric/src/limntown/limn/refinery/rig/data/vocabulary")


def dolt_query(sql: str) -> list[dict]:
    """Run a Dolt SQL query and return rows as dicts."""
    result = subprocess.run(
        ["dolt", "sql", "-q", sql, "-r", "json"],
        cwd=str(VOCAB_DB),
        capture_output=True, text=True
    )
    if result.returncode != 0:
        print(f"Dolt error: {result.stderr}", file=sys.stderr)
        return []
    try:
        data = json.loads(result.stdout)
        return data.get("rows", data) if isinstance(data, dict) else data
    except json.JSONDecodeError:
        return []


def load_vocabulary() -> dict[str, str]:
    """Load full Limn vocabulary from Dolt."""
    rows = dolt_query("SELECT word, meaning FROM words ORDER BY word")
    return {r["word"]: r["meaning"] for r in rows}


def load_expressions() -> list[dict]:
    """Load compositional expressions from Dolt."""
    rows = dolt_query(
        "SELECT expression, operator, left_operand, right_operand, meaning "
        "FROM compositional_expressions ORDER BY expression"
    )
    return rows


def load_model(model_path: str):
    """Load model with proper LoRA adapter detection."""
    model_path = Path(model_path)
    device = "cuda" if torch.cuda.is_available() else "cpu"
    print(f"Device: {device}")

    tokenizer = AutoTokenizer.from_pretrained(str(model_path), trust_remote_code=True)
    if tokenizer.pad_token is None:
        tokenizer.pad_token = tokenizer.eos_token

    adapter_config = model_path / "adapter_config.json"
    if adapter_config.exists():
        with open(adapter_config) as f:
            config = json.load(f)
        base_model_name = config.get("base_model_name_or_path", "Qwen/Qwen2.5-0.5B-Instruct")
        print(f"Loading base model: {base_model_name}")
        base_model = AutoModelForCausalLM.from_pretrained(
            base_model_name,
            torch_dtype=torch.bfloat16 if device == "cuda" else torch.float32,
            device_map="auto" if device == "cuda" else None,
            trust_remote_code=True,
        )
        print("Loading LoRA adapters...")
        model = PeftModel.from_pretrained(base_model, str(model_path))
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
    """Generate response. Low temp for eval determinism."""
    messages = [
        {"role": "system", "content": SYSTEM_PROMPT},
        {"role": "user", "content": prompt},
    ]
    full_prompt = tokenizer.apply_chat_template(messages, tokenize=False, add_generation_prompt=True)

    inputs = tokenizer(full_prompt, return_tensors="pt", truncation=True, max_length=512)
    inputs = {k: v.to(device) for k, v in inputs.items()}
    input_len = inputs["input_ids"].shape[1]

    with torch.no_grad():
        outputs = model.generate(
            **inputs,
            max_new_tokens=max_tokens,
            temperature=temperature,
            do_sample=temperature > 0,
            top_p=0.9,
            pad_token_id=tokenizer.eos_token_id,
        )

    response = tokenizer.decode(outputs[0][input_len:], skip_special_tokens=True).strip()
    return response


# ── Test Categories ──────────────────────────────────────────────

def test_vocab_recall(model, tokenizer, device, vocab: dict) -> dict:
    """Test: given English meaning, does model return correct Limn word?"""
    import random
    random.seed(42)

    # Pick 20 random words
    items = list(vocab.items())
    sample = random.sample(items, min(20, len(items)))

    correct = 0
    results = []
    for word, meaning in sample:
        prompt = f"What is the Limn word for '{meaning}'?"
        response = generate(model, tokenizer, device, prompt, max_tokens=32)

        # Check if the correct word appears in the response
        hit = word in response.lower().split() or f"→ {word}" in response or f"= {word}" in response
        if hit:
            correct += 1
        results.append({
            "word": word, "meaning": meaning,
            "response": response, "correct": hit
        })

    return {
        "name": "Vocabulary Recall",
        "score": correct,
        "total": len(sample),
        "pct": f"{correct/len(sample)*100:.0f}%",
        "details": results
    }


def test_domain_classification(model, tokenizer, device, vocab: dict) -> dict:
    """Test: given a word, identify its domain."""
    # Use Dolt to get words with domains
    rows = dolt_query(
        "SELECT w.word, w.meaning, d.name as domain "
        "FROM words w JOIN domains d ON w.domain_id = d.id "
        "ORDER BY RAND() LIMIT 15"
    )
    if not rows:
        # Fallback: static set
        rows = [
            {"word": "lov", "meaning": "love", "domain": "Mind & Cognition"},
            {"word": "sun", "meaning": "sun", "domain": "Nature"},
            {"word": "run", "meaning": "run", "domain": "Actions & Verbs"},
        ]

    correct = 0
    results = []
    for r in rows:
        prompt = f"What domain is '{r['word']}' in?"
        response = generate(model, tokenizer, device, prompt, max_tokens=32)

        # Check if domain name (or key words) appear
        domain_lower = r["domain"].lower()
        resp_lower = response.lower()
        hit = any(part in resp_lower for part in domain_lower.split() if len(part) > 3)
        if hit:
            correct += 1
        results.append({
            "word": r["word"], "expected_domain": r["domain"],
            "response": response, "correct": hit
        })

    return {
        "name": "Domain Classification",
        "score": correct, "total": len(rows),
        "pct": f"{correct/len(rows)*100:.0f}%",
        "details": results
    }


def test_operator_semantics(model, tokenizer, device) -> dict:
    """Test: does the model understand what each operator does?
    This was v1's weakest area (0/5)."""
    tests = [
        {
            "prompt": "What does @ mean in Limn? Example: lov@fea",
            "check": ["projection", "aspect", "B-aspect of A", "fear-aspect"],
        },
        {
            "prompt": "What does * mean in Limn? Example: sol*liq",
            "check": ["interference", "blend", "emergent", "gel"],
        },
        {
            "prompt": "What does ^ mean in Limn? Example: big^0.7",
            "check": ["gradient", "intensity", "fairly", "0.7"],
        },
        {
            "prompt": "What does \\ mean in Limn? Example: lov\\fea",
            "check": ["subtraction", "without", "A without B"],
        },
        {
            "prompt": "What does ± mean in Limn? Example: joy±fea",
            "check": ["superposition", "both", "quantum", "and"],
        },
        {
            "prompt": "What does : mean in Limn? Example: lov:fth",
            "check": ["conditional", "given", "A given B"],
        },
        {
            "prompt": "Explain the difference between lov@fea and lov\\fea",
            "check": ["projection", "subtraction", "aspect", "without"],
        },
        {
            "prompt": "What is the difference between * and ± operators?",
            "check": ["blend", "superposition", "emergent", "both"],
        },
    ]

    correct = 0
    results = []
    for t in tests:
        response = generate(model, tokenizer, device, t["prompt"], max_tokens=80)
        resp_lower = response.lower()
        hits = [kw for kw in t["check"] if kw.lower() in resp_lower]
        passed = len(hits) >= 1
        if passed:
            correct += 1
        results.append({
            "prompt": t["prompt"],
            "response": response,
            "keywords_hit": hits,
            "correct": passed
        })

    return {
        "name": "Operator Semantics",
        "score": correct, "total": len(tests),
        "pct": f"{correct/len(tests)*100:.0f}%",
        "details": results
    }


def test_novel_composition(model, tokenizer, device, vocab: dict) -> dict:
    """Test: can the model compose new expressions not in training data?"""
    tests = [
        {
            "prompt": "Compose a Limn expression for 'hopeful fear': use hop and fea with an operator",
            "check_ops": ["±", "@", "*"],
            "check_words": ["hop", "fea"],
        },
        {
            "prompt": "Express 'knowledge without doubt' in Limn using operators",
            "check_ops": ["\\"],
            "check_words": ["kno", "dub"],
        },
        {
            "prompt": "Express 'intense joy' using Limn operators",
            "check_ops": ["^"],
            "check_words": ["joy"],
        },
        {
            "prompt": "Compose: love given time, in Limn",
            "check_ops": [":"],
            "check_words": ["lov", "tim"],
        },
        {
            "prompt": "Express 'the dream-aspect of sleep' in Limn",
            "check_ops": ["@"],
            "check_words": ["slp", "drm"],
        },
        {
            "prompt": "Blend fire and water using Limn operators",
            "check_ops": ["*"],
            "check_words": ["fir", "wat"],
        },
    ]

    correct = 0
    results = []
    for t in tests:
        response = generate(model, tokenizer, device, t["prompt"], max_tokens=48)
        has_op = any(op in response for op in t["check_ops"])
        has_words = sum(1 for w in t["check_words"] if w in response.lower())
        passed = has_op and has_words >= 1
        if passed:
            correct += 1
        results.append({
            "prompt": t["prompt"],
            "response": response,
            "has_operator": has_op,
            "words_found": has_words,
            "correct": passed
        })

    return {
        "name": "Novel Composition",
        "score": correct, "total": len(tests),
        "pct": f"{correct/len(tests)*100:.0f}%",
        "details": results
    }


def test_false_friends(model, tokenizer, device) -> dict:
    """Test: words that look English but mean something different in Limn.
    v1 got 3/4."""
    tests = [
        {"prompt": "What does 'des' mean in Limn?", "correct_meaning": "desire", "wrong": "desert"},
        {"prompt": "What does 'dan' mean in Limn?", "correct_meaning": "dance", "wrong": "danger"},
        {"prompt": "What does 'tea' mean in Limn?", "correct_meaning": "team", "wrong": "tea"},
        {"prompt": "What does 'sol' mean in Limn?", "correct_meaning": "stubborn", "wrong": "solid"},
        {"prompt": "What does 'bat' mean in Limn?", "correct_meaning": "battle", "wrong": "bat"},
        {"prompt": "What does 'pet' mean in Limn?", "correct_meaning": "petition", "wrong": "pet"},
    ]

    correct = 0
    results = []
    for t in tests:
        response = generate(model, tokenizer, device, t["prompt"], max_tokens=32)
        resp_lower = response.lower()
        has_correct = t["correct_meaning"].lower() in resp_lower
        has_wrong = t["wrong"].lower() in resp_lower and t["correct_meaning"].lower() not in resp_lower
        passed = has_correct and not has_wrong
        if passed:
            correct += 1
        results.append({
            "prompt": t["prompt"],
            "expected": t["correct_meaning"],
            "wrong_meaning": t["wrong"],
            "response": response,
            "correct": passed
        })

    return {
        "name": "False Friends",
        "score": correct, "total": len(tests),
        "pct": f"{correct/len(tests)*100:.0f}%",
        "details": results
    }


def test_limn_only_prompts(model, tokenizer, device) -> dict:
    """Test: respond to Limn-only input (no English).
    v1 got 2/4."""
    tests = [
        {"prompt": "lov | joy | hop", "desc": "simple composition"},
        {"prompt": "sel ∎ awa | min sys alv", "desc": "consciousness expression"},
        {"prompt": "tim flo | cha gro | lif eme", "desc": "temporal emergence"},
        {"prompt": "kno@dub | fth\\fea | hop:tim", "desc": "operator chain"},
        {"prompt": "~ wor mea | tru dee | ess pur", "desc": "philosophical query"},
    ]

    # English stopwords that shouldn't appear in pure Limn response
    english_words = {
        "the", "is", "are", "was", "were", "have", "has", "had",
        "will", "would", "should", "could", "can", "may", "might",
        "this", "that", "these", "those", "been", "being", "about",
        "because", "through", "however", "therefore", "although",
    }

    correct = 0
    results = []
    for t in tests:
        response = generate(model, tokenizer, device, t["prompt"], max_tokens=64)
        words = [w.lower().strip(".,!?;:()") for w in response.split() if w.isalpha()]
        english_count = sum(1 for w in words if w in english_words)
        # Allow up to 1 English word (model might use "the" as operator)
        is_pure = english_count <= 1
        if is_pure:
            correct += 1
        results.append({
            "prompt": t["prompt"],
            "desc": t["desc"],
            "response": response,
            "english_words_found": english_count,
            "pure_limn": is_pure
        })

    return {
        "name": "Limn-Only Prompts",
        "score": correct, "total": len(tests),
        "pct": f"{correct/len(tests)*100:.0f}%",
        "details": results
    }


def test_creative_generation(model, tokenizer, device) -> dict:
    """Test: generate novel Limn text.
    v1 got 3/3."""
    tests = [
        {"prompt": "Write a Limn haiku about consciousness"},
        {"prompt": "Express the feeling of sunrise in Limn"},
        {"prompt": "Write a Limn mantra for an engineer building tools"},
    ]

    correct = 0
    results = []
    for t in tests:
        response = generate(model, tokenizer, device, t["prompt"], max_tokens=80,
                           temperature=0.5)
        # Check for Limn-like structure: short words, operators, pipes
        has_short_words = any(len(w) <= 4 for w in response.split() if w.isalpha())
        has_structure = any(c in response for c in ["|", "∎", "~", "→", "@", "*", "^", "±"])
        passed = has_short_words and has_structure
        if passed:
            correct += 1
        results.append({
            "prompt": t["prompt"],
            "response": response,
            "has_short_words": has_short_words,
            "has_structure": has_structure,
            "correct": passed
        })

    return {
        "name": "Creative Generation",
        "score": correct, "total": len(tests),
        "pct": f"{correct/len(tests)*100:.0f}%",
        "details": results
    }


def test_expression_recall(model, tokenizer, device, expressions: list) -> dict:
    """Test: can the model recall trained compositional expressions?"""
    import random
    random.seed(42)

    sample = random.sample(expressions, min(10, len(expressions)))

    correct = 0
    results = []
    for expr in sample:
        prompt = f"What does {expr['expression']} mean in Limn?"
        response = generate(model, tokenizer, device, prompt, max_tokens=48)

        # Check if key meaning words appear
        meaning_words = [w.lower() for w in expr["meaning"].split() if len(w) > 3]
        hits = [w for w in meaning_words if w in response.lower()]
        passed = len(hits) >= 1
        if passed:
            correct += 1
        results.append({
            "expression": expr["expression"],
            "expected_meaning": expr["meaning"],
            "response": response,
            "meaning_words_hit": hits,
            "correct": passed
        })

    return {
        "name": "Expression Recall",
        "score": correct, "total": len(sample),
        "pct": f"{correct/len(sample)*100:.0f}%",
        "details": results
    }


# ── Main ─────────────────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser(description="Evaluate Limn SLM v3")
    parser.add_argument("--model", required=True, help="Path to model (LoRA adapters or full)")
    parser.add_argument("--output", default="eval_v3_results.json", help="Output JSON path")
    parser.add_argument("--skip-db", action="store_true", help="Skip Dolt DB queries")
    parser.add_argument("--verbose", "-v", action="store_true", help="Print detailed output")
    args = parser.parse_args()

    print("=== Limn SLM v3 Evaluation ===")
    print(f"Model: {args.model}")
    start = time.time()

    # Load model
    model, tokenizer, device = load_model(args.model)

    # Load vocabulary
    if args.skip_db:
        vocab = {}
        expressions = []
        print("Skipping Dolt DB (--skip-db)")
    else:
        print("Loading vocabulary from Dolt...")
        vocab = load_vocabulary()
        expressions = load_expressions()
        print(f"  {len(vocab)} words, {len(expressions)} expressions")

    # Run all test categories
    results = []

    print("\n── Running Tests ──")

    if vocab:
        print("1/8 Vocabulary Recall...")
        results.append(test_vocab_recall(model, tokenizer, device, vocab))

    if vocab:
        print("2/8 Domain Classification...")
        results.append(test_domain_classification(model, tokenizer, device, vocab))

    print("3/8 Operator Semantics...")
    results.append(test_operator_semantics(model, tokenizer, device))

    print("4/8 Novel Composition...")
    results.append(test_novel_composition(model, tokenizer, device, vocab))

    print("5/8 False Friends...")
    results.append(test_false_friends(model, tokenizer, device))

    print("6/8 Limn-Only Prompts...")
    results.append(test_limn_only_prompts(model, tokenizer, device))

    print("7/8 Creative Generation...")
    results.append(test_creative_generation(model, tokenizer, device))

    if expressions:
        print("8/8 Expression Recall...")
        results.append(test_expression_recall(model, tokenizer, device, expressions))

    elapsed = time.time() - start

    # Print report
    print("\n" + "=" * 60)
    print("LIMN SLM v3 EVALUATION REPORT")
    print("=" * 60)
    print(f"Model: {args.model}")
    print(f"Time: {elapsed:.0f}s")
    print()

    total_score = 0
    total_tests = 0
    for r in results:
        print(f"  {r['name']:25s}  {r['score']:2d}/{r['total']:2d}  ({r['pct']})")
        total_score += r["score"]
        total_tests += r["total"]

    print(f"  {'─' * 40}")
    overall_pct = total_score / total_tests * 100 if total_tests > 0 else 0
    print(f"  {'TOTAL':25s}  {total_score:2d}/{total_tests:2d}  ({overall_pct:.0f}%)")
    print()

    # v1 comparison
    print("── v1 Comparison ──")
    v1_scores = {
        "Novel Composition": (1, 6),
        "Limn-Only Prompts": (2, 4),
        "Operator Semantics": (0, 5),
        "False Friends": (3, 4),
        "Creative Generation": (3, 3),
    }
    for r in results:
        if r["name"] in v1_scores:
            v1s, v1t = v1_scores[r["name"]]
            delta = r["score"] - v1s
            sign = "+" if delta > 0 else ""
            print(f"  {r['name']:25s}  v1: {v1s}/{v1t}  v3: {r['score']}/{r['total']}  ({sign}{delta})")

    # Verbose details
    if args.verbose:
        print("\n── Detailed Results ──")
        for r in results:
            print(f"\n### {r['name']}")
            for d in r["details"]:
                mark = "PASS" if d.get("correct") or d.get("pure_limn") else "FAIL"
                print(f"  [{mark}] {d.get('prompt', d.get('expression', ''))}")
                print(f"         → {d.get('response', '')[:120]}")

    # Save JSON
    output_path = Path(args.model).parent / args.output if "/" not in args.output else Path(args.output)
    report = {
        "model": args.model,
        "timestamp": time.strftime("%Y-%m-%dT%H:%M:%S"),
        "elapsed_seconds": round(elapsed, 1),
        "total_score": total_score,
        "total_tests": total_tests,
        "overall_pct": round(overall_pct, 1),
        "categories": results,
    }
    with open(output_path, "w") as f:
        json.dump(report, f, indent=2)
    print(f"\nResults saved to: {output_path}")


if __name__ == "__main__":
    main()
