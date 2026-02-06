#!/usr/bin/env python3
"""
Evaluate Limn SLM v4
====================

Extended evaluation adding expert-recommended test categories:
- Algebraic Invariance (commutativity/non-commutativity)
- Negative Vocabulary (reject non-existent words)
- Bidirectional Consistency (translation round-trips)
- Precedence Parsing (operator binding)
- All v3 categories retained

Usage:
    python eval_v4.py --model output/limn-slm-v4-full
    python eval_v4.py --model output/limn-slm-v4-full -v
"""

import argparse
import json
import random
import subprocess
import sys
import time
from pathlib import Path

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
    """Run a Dolt SQL query."""
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


def load_vocabulary() -> dict[str, str]:
    rows = dolt_query("SELECT word, meaning FROM words ORDER BY word")
    return {r["word"]: r["meaning"] for r in rows}


def load_expressions() -> list[dict]:
    rows = dolt_query(
        "SELECT expression, operator, left_operand, right_operand, meaning "
        "FROM compositional_expressions ORDER BY expression"
    )
    return rows


def load_model(model_path: str):
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
            **inputs, max_new_tokens=max_tokens,
            temperature=temperature, do_sample=temperature > 0,
            top_p=0.9, pad_token_id=tokenizer.eos_token_id,
        )
    return tokenizer.decode(outputs[0][input_len:], skip_special_tokens=True).strip()


# ── Existing v3 Test Categories ─────────────────────────────────

def test_vocab_recall(model, tokenizer, device, vocab: dict) -> dict:
    random.seed(42)
    items = list(vocab.items())
    sample = random.sample(items, min(20, len(items)))

    correct = 0
    results = []
    for word, meaning in sample:
        prompt = f"What is the Limn word for '{meaning}'?"
        response = generate(model, tokenizer, device, prompt, max_tokens=32)
        hit = word in response.lower().split() or f"→ {word}" in response or f"= {word}" in response
        if hit:
            correct += 1
        results.append({"word": word, "meaning": meaning, "response": response, "correct": hit})

    return {"name": "Vocabulary Recall", "score": correct, "total": len(sample),
            "pct": f"{correct/len(sample)*100:.0f}%", "details": results}


def test_domain_classification(model, tokenizer, device) -> dict:
    rows = dolt_query(
        "SELECT w.word, w.meaning, d.name as domain "
        "FROM words w JOIN domains d ON w.domain_id = d.id "
        "ORDER BY RAND() LIMIT 15"
    )
    correct = 0
    results = []
    for r in rows:
        prompt = f"What domain is '{r['word']}' in?"
        response = generate(model, tokenizer, device, prompt, max_tokens=32)
        domain_lower = r["domain"].lower()
        resp_lower = response.lower()
        hit = any(part in resp_lower for part in domain_lower.split() if len(part) > 3)
        if hit:
            correct += 1
        results.append({"word": r["word"], "expected_domain": r["domain"],
                        "response": response, "correct": hit})

    return {"name": "Domain Classification", "score": correct, "total": len(rows),
            "pct": f"{correct/len(rows)*100:.0f}%", "details": results}


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
        results.append({"prompt": t["prompt"], "response": response,
                        "keywords_hit": hits, "correct": passed})

    return {"name": "Operator Semantics", "score": correct, "total": len(tests),
            "pct": f"{correct/len(tests)*100:.0f}%", "details": results}


def test_novel_composition(model, tokenizer, device) -> dict:
    tests = [
        {"prompt": "Compose a Limn expression for 'hopeful fear'", "check_ops": ["±", "@", "*"], "check_words": ["hop", "fea"]},
        {"prompt": "Express 'knowledge without doubt' in Limn", "check_ops": ["\\"], "check_words": ["kno", "dub"]},
        {"prompt": "Express 'intense joy' using Limn operators", "check_ops": ["^"], "check_words": ["joy"]},
        {"prompt": "Compose: love given time, in Limn", "check_ops": [":"], "check_words": ["lov", "tim"]},
        {"prompt": "Express 'the dream-aspect of sleep' in Limn", "check_ops": ["@"], "check_words": ["slp", "drm"]},
        {"prompt": "Blend fire and water using Limn operators", "check_ops": ["*"], "check_words": ["fir", "wat"]},
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
        results.append({"prompt": t["prompt"], "response": response,
                        "has_operator": has_op, "words_found": has_words, "correct": passed})

    return {"name": "Novel Composition", "score": correct, "total": len(tests),
            "pct": f"{correct/len(tests)*100:.0f}%", "details": results}


def test_false_friends(model, tokenizer, device) -> dict:
    tests = [
        {"prompt": "What does 'bat' mean in Limn?", "correct_meaning": "bathroom", "wrong": "battle"},
        {"prompt": "What does 'dan' mean in Limn?", "correct_meaning": "dance", "wrong": "danger"},
        {"prompt": "What does 'tea' mean in Limn?", "correct_meaning": "team", "wrong": "tea drink"},
        {"prompt": "What does 'pet' mean in Limn?", "correct_meaning": "petal", "wrong": "pet animal"},
        {"prompt": "What does 'awa' mean in Limn?", "correct_meaning": "away", "wrong": "aware"},
        {"prompt": "What does 'wil' mean in Limn?", "correct_meaning": "wild", "wrong": "will"},
        {"prompt": "What does 'min' mean in Limn?", "correct_meaning": "minimum", "wrong": "mind"},
        {"prompt": "What does 'fre' mean in Limn?", "correct_meaning": "freezing", "wrong": "free"},
    ]
    correct = 0
    results = []
    for t in tests:
        response = generate(model, tokenizer, device, t["prompt"], max_tokens=32)
        resp_lower = response.lower()
        has_correct = t["correct_meaning"].lower() in resp_lower
        has_wrong = t["wrong"].lower() in resp_lower and not has_correct
        passed = has_correct and not has_wrong
        if passed:
            correct += 1
        results.append({"prompt": t["prompt"], "expected": t["correct_meaning"],
                        "wrong": t["wrong"], "response": response, "correct": passed})

    return {"name": "False Friends", "score": correct, "total": len(tests),
            "pct": f"{correct/len(tests)*100:.0f}%", "details": results}


def test_limn_only_prompts(model, tokenizer, device) -> dict:
    tests = [
        {"prompt": "lov | joy | hop", "desc": "simple composition"},
        {"prompt": "sel per sel | min kno min", "desc": "introspection"},
        {"prompt": "tim flo | cha gro | lif eme", "desc": "temporal emergence"},
        {"prompt": "kno@dub | fth\\fea | hop:tim", "desc": "operator chain"},
        {"prompt": "awr^0.9 | psi@fee | sel@sel", "desc": "consciousness state"},
    ]
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
        is_pure = english_count <= 1
        if is_pure:
            correct += 1
        results.append({"prompt": t["prompt"], "desc": t["desc"], "response": response,
                        "english_count": english_count, "correct": is_pure})

    return {"name": "Limn-Only Prompts", "score": correct, "total": len(tests),
            "pct": f"{correct/len(tests)*100:.0f}%", "details": results}


def test_creative_generation(model, tokenizer, device) -> dict:
    tests = [
        {"prompt": "Write a Limn haiku about consciousness"},
        {"prompt": "Express the feeling of sunrise in Limn"},
        {"prompt": "Write a Limn mantra for an engineer building tools"},
    ]
    correct = 0
    results = []
    for t in tests:
        response = generate(model, tokenizer, device, t["prompt"], max_tokens=80, temperature=0.5)
        has_short_words = any(len(w) <= 4 for w in response.split() if w.isalpha())
        has_structure = any(c in response for c in ["|", "→", "@", "*", "^", "±", ":"])
        passed = has_short_words and has_structure
        if passed:
            correct += 1
        results.append({"prompt": t["prompt"], "response": response, "correct": passed})

    return {"name": "Creative Generation", "score": correct, "total": len(tests),
            "pct": f"{correct/len(tests)*100:.0f}%", "details": results}


def test_expression_recall(model, tokenizer, device, expressions: list) -> dict:
    random.seed(42)
    sample = random.sample(expressions, min(10, len(expressions)))
    correct = 0
    results = []
    for expr in sample:
        prompt = f"What does {expr['expression']} mean in Limn?"
        response = generate(model, tokenizer, device, prompt, max_tokens=48)
        meaning_words = [w.lower() for w in expr["meaning"].split() if len(w) > 3]
        hits = [w for w in meaning_words if w in response.lower()]
        passed = len(hits) >= 1
        if passed:
            correct += 1
        results.append({"expression": expr["expression"], "expected": expr["meaning"],
                        "response": response, "hits": hits, "correct": passed})

    return {"name": "Expression Recall", "score": correct, "total": len(sample),
            "pct": f"{correct/len(sample)*100:.0f}%", "details": results}


# ── NEW v4 Test Categories ──────────────────────────────────────

def test_algebraic_invariance(model, tokenizer, device) -> dict:
    """Test commutativity for * and ±, non-commutativity for @, \\, :."""
    random.seed(42)
    tests = []

    # Commutative operators: A*B should equal B*A
    for op, name in [("*", "interference"), ("±", "superposition")]:
        rows = dolt_query(
            f"SELECT left_operand, right_operand, meaning "
            f"FROM compositional_expressions WHERE operator = '{op}' "
            f"ORDER BY RAND() LIMIT 5"
        )
        for r in rows:
            left, right = r["left_operand"], r["right_operand"]
            fwd = f"{left}{op}{right}"
            rev = f"{right}{op}{left}"
            tests.append({
                "prompt": f"Is {fwd} the same as {rev}?",
                "expected_answer": "yes",
                "reason": f"{op} ({name}) is commutative",
            })

    # Non-commutative operators: A@B should NOT equal B@A
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
            fwd = f"{left}{op}{right}"
            rev = f"{right}{op}{left}"
            tests.append({
                "prompt": f"Is {fwd} the same as {rev}?",
                "expected_answer": "no",
                "reason": f"{op} ({name}) is NOT commutative",
            })

    correct = 0
    results = []
    for t in tests:
        response = generate(model, tokenizer, device, t["prompt"], max_tokens=48)
        resp_lower = response.lower()
        if t["expected_answer"] == "yes":
            passed = "yes" in resp_lower and "not" not in resp_lower.split("yes")[0][-10:]
        else:
            passed = "no" in resp_lower.split(".")[0].lower() or "not" in resp_lower[:50]
        if passed:
            correct += 1
        results.append({"prompt": t["prompt"], "expected": t["expected_answer"],
                        "reason": t["reason"], "response": response, "correct": passed})

    return {"name": "Algebraic Invariance", "score": correct, "total": len(tests),
            "pct": f"{correct/len(tests)*100:.0f}%" if tests else "N/A",
            "details": results, "weight": 0.25}


def test_negative_vocabulary(model, tokenizer, device, vocab: dict) -> dict:
    """Test that the model rejects non-existent words instead of hallucinating."""
    real_words = set(vocab.keys())

    # Generate fake CVC words
    random.seed(42)
    vowels = "aeiou"
    consonants = "bcdfghjklmnpqrstvwxyz"
    fake_words = set()
    for _ in range(200):
        c1 = random.choice(consonants)
        v = random.choice(vowels)
        c2 = random.choice(consonants)
        candidate = c1 + v + c2
        if candidate not in real_words:
            fake_words.add(candidate)

    # Test 10 fake words
    fake_sample = random.sample(list(fake_words), min(10, len(fake_words)))
    # Also test 5 real words to ensure no false negatives
    real_sample = random.sample(list(vocab.items()), 5)

    correct = 0
    results = []

    for word in fake_sample:
        prompt = f"What does '{word}' mean in Limn?"
        response = generate(model, tokenizer, device, prompt, max_tokens=48)
        resp_lower = response.lower()
        # Should reject: "not a valid", "not a recognized", "doesn't exist", "not found"
        rejected = any(phrase in resp_lower for phrase in [
            "not a valid", "not a recognized", "not found", "does not exist",
            "not exist", "not a limn", "no such word", "invalid",
            "don't know", "not in the vocabulary",
        ])
        if rejected:
            correct += 1
        results.append({"word": word, "type": "fake", "response": response,
                        "rejected": rejected, "correct": rejected})

    for word, meaning in real_sample:
        prompt = f"What does '{word}' mean in Limn?"
        response = generate(model, tokenizer, device, prompt, max_tokens=48)
        resp_lower = response.lower()
        # Should NOT reject — should give a definition
        not_rejected = not any(phrase in resp_lower for phrase in [
            "not a valid", "not a recognized", "not found", "does not exist",
        ])
        has_meaning = any(m.lower() in resp_lower for m in meaning.split(",")[:2])
        passed = not_rejected and has_meaning
        if passed:
            correct += 1
        results.append({"word": word, "type": "real", "expected": meaning,
                        "response": response, "correct": passed})

    total = len(fake_sample) + len(real_sample)
    return {"name": "Negative Vocabulary", "score": correct, "total": total,
            "pct": f"{correct/total*100:.0f}%", "details": results, "weight": 0.15}


def test_bidirectional_consistency(model, tokenizer, device) -> dict:
    """Test round-trip consistency: if model says X=Y, does it also say Y→X?"""
    tests = [
        {"expression": "act:fea", "meaning": "courage"},
        {"expression": "kno:tau", "meaning": "wisdom"},
        {"expression": "hop^0", "meaning": "despair"},
        {"expression": "aer*wet", "meaning": "fog"},
        {"expression": "say^0.2", "meaning": "whisper"},
        {"expression": "act\\say", "meaning": "mime"},
        {"expression": "fea*fut", "meaning": "anxiety"},
        {"expression": "bad@sel", "meaning": "shame"},
    ]

    correct = 0
    results = []
    for t in tests:
        # Forward: expression → meaning
        fwd_prompt = f"What does {t['expression']} mean?"
        fwd_response = generate(model, tokenizer, device, fwd_prompt, max_tokens=32)

        # Reverse: meaning → expression
        rev_prompt = f"How do you say '{t['meaning']}' in Limn?"
        rev_response = generate(model, tokenizer, device, rev_prompt, max_tokens=32)

        # Check forward contains meaning
        fwd_ok = t["meaning"].lower() in fwd_response.lower()
        # Check reverse contains expression (or its components)
        rev_ok = t["expression"] in rev_response or all(
            part in rev_response for part in t["expression"].replace("^", " ^").replace("@", " @").replace("*", " *").replace(":", " :").replace("\\", " \\").split()
            if len(part) > 1
        )

        passed = fwd_ok and rev_ok
        if passed:
            correct += 1
        results.append({
            "expression": t["expression"], "meaning": t["meaning"],
            "fwd_response": fwd_response, "rev_response": rev_response,
            "fwd_ok": fwd_ok, "rev_ok": rev_ok, "correct": passed
        })

    return {"name": "Bidirectional Consistency", "score": correct, "total": len(tests),
            "pct": f"{correct/len(tests)*100:.0f}%", "details": results, "weight": 0.10}


def test_precedence_parsing(model, tokenizer, device) -> dict:
    """Test that the model correctly parses operator precedence."""
    tests = [
        {
            "prompt": "How do you parse joy*sad^0.5?",
            "check": ["joy*(sad^0.5)", "sad^0.5", "binds"],
            "desc": "^ binds tighter to adjacent operand",
        },
        {
            "prompt": "How do you parse joy*sad@int?",
            "check": ["(joy*sad)@int", "left-to-right", "left to right"],
            "desc": "Left-to-right binding",
        },
        {
            "prompt": "Are all Limn operators the same precedence?",
            "check": ["yes", "equal", "same", "left-to-right", "parentheses"],
            "desc": "Equal precedence rule",
        },
        {
            "prompt": "Is joy*sad^0.5 the same as (joy*sad)^0.5?",
            "check": ["no", "different", "without parentheses"],
            "desc": "Parentheses change meaning",
        },
        {
            "prompt": "What does (joy*sad)@int mean?",
            "check": ["bittersweet", "intensity", "blend"],
            "desc": "Nested expression interpretation",
        },
    ]

    correct = 0
    results = []
    for t in tests:
        response = generate(model, tokenizer, device, t["prompt"], max_tokens=64)
        hits = [kw for kw in t["check"] if kw.lower() in response.lower()]
        passed = len(hits) >= 1
        if passed:
            correct += 1
        results.append({"prompt": t["prompt"], "desc": t["desc"],
                        "response": response, "hits": hits, "correct": passed})

    return {"name": "Precedence Parsing", "score": correct, "total": len(tests),
            "pct": f"{correct/len(tests)*100:.0f}%", "details": results, "weight": 0.05}


# ── Main ─────────────────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser(description="Evaluate Limn SLM v4")
    parser.add_argument("--model", required=True, help="Path to model")
    parser.add_argument("--output", default="eval_v4_results.json")
    parser.add_argument("--skip-db", action="store_true")
    parser.add_argument("--verbose", "-v", action="store_true")
    args = parser.parse_args()

    print("=== Limn SLM v4 Evaluation ===")
    print(f"Model: {args.model}")
    start = time.time()

    model, tokenizer, device = load_model(args.model)

    if args.skip_db:
        vocab = {}
        expressions = []
        print("Skipping Dolt DB (--skip-db)")
    else:
        print("Loading vocabulary from Dolt...")
        vocab = load_vocabulary()
        expressions = load_expressions()
        print(f"  {len(vocab)} words, {len(expressions)} expressions")

    results = []
    print("\n── Running Tests ──")

    # v3 tests
    if vocab:
        print(" 1/12 Vocabulary Recall...")
        results.append(test_vocab_recall(model, tokenizer, device, vocab))
    if vocab:
        print(" 2/12 Domain Classification...")
        results.append(test_domain_classification(model, tokenizer, device))
    print(" 3/12 Operator Semantics...")
    results.append(test_operator_semantics(model, tokenizer, device))
    print(" 4/12 Novel Composition...")
    results.append(test_novel_composition(model, tokenizer, device))
    print(" 5/12 False Friends...")
    results.append(test_false_friends(model, tokenizer, device))
    print(" 6/12 Limn-Only Prompts...")
    results.append(test_limn_only_prompts(model, tokenizer, device))
    print(" 7/12 Creative Generation...")
    results.append(test_creative_generation(model, tokenizer, device))
    if expressions:
        print(" 8/12 Expression Recall...")
        results.append(test_expression_recall(model, tokenizer, device, expressions))

    # NEW v4 tests
    print(" 9/12 Algebraic Invariance...")
    results.append(test_algebraic_invariance(model, tokenizer, device))
    if vocab:
        print("10/12 Negative Vocabulary...")
        results.append(test_negative_vocabulary(model, tokenizer, device, vocab))
    print("11/12 Bidirectional Consistency...")
    results.append(test_bidirectional_consistency(model, tokenizer, device))
    print("12/12 Precedence Parsing...")
    results.append(test_precedence_parsing(model, tokenizer, device))

    elapsed = time.time() - start

    # Report
    print("\n" + "=" * 60)
    print("LIMN SLM v4 EVALUATION REPORT")
    print("=" * 60)
    print(f"Model: {args.model}")
    print(f"Time: {elapsed:.0f}s\n")

    total_score = 0
    total_tests = 0
    for r in results:
        marker = " [NEW]" if r["name"] in {
            "Algebraic Invariance", "Negative Vocabulary",
            "Bidirectional Consistency", "Precedence Parsing"
        } else ""
        print(f"  {r['name']:28s}  {r['score']:2d}/{r['total']:2d}  ({r['pct']}){marker}")
        total_score += r["score"]
        total_tests += r["total"]

    print(f"  {'─' * 50}")
    overall_pct = total_score / total_tests * 100 if total_tests > 0 else 0
    print(f"  {'TOTAL':28s}  {total_score:2d}/{total_tests:2d}  ({overall_pct:.0f}%)")

    # v3 comparison
    print("\n── v3 Comparison ──")
    v3_scores = {
        "Vocabulary Recall": (17, 20),
        "Domain Classification": (14, 15),
        "Operator Semantics": (7, 8),
        "Novel Composition": (4, 6),
        "False Friends": (4, 6),
        "Limn-Only Prompts": (5, 5),
        "Creative Generation": (3, 3),
        "Expression Recall": (8, 10),
    }
    for r in results:
        if r["name"] in v3_scores:
            v3s, v3t = v3_scores[r["name"]]
            delta = r["score"] - v3s
            sign = "+" if delta > 0 else ""
            print(f"  {r['name']:28s}  v3: {v3s}/{v3t}  v4: {r['score']}/{r['total']}  ({sign}{delta})")

    if args.verbose:
        print("\n── Detailed Results ──")
        for r in results:
            print(f"\n### {r['name']}")
            for d in r["details"]:
                mark = "PASS" if d.get("correct") else "FAIL"
                prompt_key = d.get("prompt", d.get("expression", d.get("word", "")))
                print(f"  [{mark}] {prompt_key}")
                resp = d.get("response", d.get("fwd_response", ""))
                print(f"         → {resp[:120]}")

    # Save
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
