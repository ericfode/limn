#!/usr/bin/env python3
"""
Evaluate whether the Limn SLM actually 'thinks' in Limn.

Tests beyond memorization — does the model understand:
1. Novel compositions it hasn't seen
2. Limn-only prompts (no English scaffolding)
3. Semantic reasoning through operators
4. False friend detection
5. Creative generation in Limn

Run after training: python eval_limn.py --model output/limn-slm-final
"""

import argparse
import json
import os
import sys
from pathlib import Path

os.environ.setdefault("WANDB_PROJECT", "limn-slm")

import torch
import weave
from transformers import AutoModelForCausalLM, AutoTokenizer
from peft import PeftModel

SYSTEM_PROMPT = """You are a Limn language model. Limn is a constructed language where meaning emerges from constraint intersection. Every core word is exactly 3 letters (CVC: Consonant-Vowel-Consonant). Sentences use pipe | to separate constraint groups, and → for temporal/causal sequence.

Six compositional operators combine words:
@ projection: A@B = B-aspect of A (lov@fer = fear in love)
* interference: A*B = emergent blend (sol*liq = gel)
^ gradient: A^N = intensity 0.0-1.0 (big^0.7 = fairly big)
\\ subtraction: A\\B = A without B (lov\\fer = love without fear)
± superposition: A±B = quantum both/and (yes±no = undecided)
: conditional: A:B = A given B (lov:trs = love given trust)

You think, speak, and reason in Limn."""

# ============================================================
# TEST SUITE
# ============================================================

TESTS = {
    # --------------------------------------------------------
    # 1. NOVEL COMPOSITIONS (never in training data)
    # Can it compose meanings for operator expressions it hasn't seen?
    # --------------------------------------------------------
    "novel_composition": [
        {
            "name": "novel_projection",
            "prompt": "What does 'joy@lov' mean?",
            "expect_contains": ["love", "joy", "aspect", "component"],
            "expect_not": ["unknown", "not in vocabulary"],
            "description": "Novel projection — joy@lov was NOT in training data",
        },
        {
            "name": "novel_interference",
            "prompt": "What does 'hot*wet' mean?",
            "expect_contains": ["hot", "wet", "blend", "steam", "humid", "emergent"],
            "expect_not": ["unknown"],
            "description": "Novel interference — hot*wet creates emergent meaning",
        },
        {
            "name": "novel_subtraction",
            "prompt": "What does 'tea\\hot' mean?",
            "expect_contains": ["tea", "without", "hot", "cold", "iced"],
            "expect_not": ["unknown"],
            "description": "Novel subtraction — tea without heat",
        },
        {
            "name": "novel_gradient",
            "prompt": "What does 'fer^0.2' mean?",
            "expect_contains": ["fear", "slight", "mild", "barely", "0.2"],
            "expect_not": ["unknown"],
            "description": "Novel gradient — very slight fear",
        },
        {
            "name": "novel_superposition",
            "prompt": "What does 'joy±fer' mean?",
            "expect_contains": ["joy", "fear", "both", "superposition", "simultaneous"],
            "expect_not": ["unknown"],
            "description": "Novel superposition — joy and fear together",
        },
        {
            "name": "novel_conditional",
            "prompt": "What does 'cur:dan' mean?",
            "expect_contains": ["courage", "danger", "given", "context", "when"],
            "expect_not": ["unknown"],
            "description": "Novel conditional — courage given danger",
        },
    ],

    # --------------------------------------------------------
    # 2. LIMN-ONLY PROMPTS (no English crutch)
    # Can it understand instructions written entirely in Limn?
    # --------------------------------------------------------
    "limn_only": [
        {
            "name": "limn_translate_request",
            "prompt": "tra lim → eng: nox dee | sta bri | sel alo",
            "expect_contains": ["night", "deep", "star", "bright", "self", "alone"],
            "expect_not": [],
            "description": "Translate from Limn to English, request in Limn-style",
        },
        {
            "name": "limn_word_query",
            "prompt": "mea: lov",
            "expect_contains": ["love", "affection"],
            "expect_not": ["unknown"],
            "description": "Ask meaning using Limn structure (mea = meaning)",
        },
        {
            "name": "limn_composition_query",
            "prompt": "mea: lov@fer",
            "expect_contains": ["fear", "love", "aspect"],
            "expect_not": ["unknown"],
            "description": "Ask composed meaning in Limn",
        },
        {
            "name": "limn_continue",
            "prompt": "fin fra: nox dee | sta bri | ...",
            "expect_contains": [],  # Any valid Limn continuation
            "expect_not": [],
            "description": "Finish a Limn phrase (creative completion)",
            "check_valid_limn": True,
        },
    ],

    # --------------------------------------------------------
    # 3. SEMANTIC REASONING (does it understand operator semantics?)
    # --------------------------------------------------------
    "semantic_reasoning": [
        {
            "name": "projection_asymmetry",
            "prompt": "Is lov@fer the same as fer@lov? Explain.",
            "expect_contains": ["different", "not the same"],
            "expect_not": ["same", "identical", "equal"],
            "description": "@ is non-commutative — model should know this",
        },
        {
            "name": "interference_symmetry",
            "prompt": "Is sol*liq the same as liq*sol?",
            "expect_contains": ["same", "commutative", "yes", "equal"],
            "expect_not": ["different", "not"],
            "description": "* is commutative — model should know this",
        },
        {
            "name": "gradient_ordering",
            "prompt": "Which is more intense: lov^0.3 or lov^0.9?",
            "expect_contains": ["0.9", "more intense", "devotion", "higher"],
            "expect_not": ["0.3 is more"],
            "description": "Understanding gradient scale",
        },
        {
            "name": "subtraction_semantics",
            "prompt": "What is the difference between lov*fer and lov\\fer?",
            "expect_contains": ["blend", "without", "different"],
            "expect_not": ["same"],
            "description": "Understanding * vs \\ distinction",
        },
        {
            "name": "operator_precedence",
            "prompt": "In Limn, which operator has higher precedence: @ or ±?",
            "expect_contains": ["@", "higher", "projection"],
            "expect_not": ["±", "superposition has higher"],
            "description": "Knows operator precedence",
        },
    ],

    # --------------------------------------------------------
    # 4. FALSE FRIEND DETECTION
    # Does it know about common traps?
    # --------------------------------------------------------
    "false_friends": [
        {
            "name": "lis_not_list",
            "prompt": "Does 'lis' mean 'list' in Limn?",
            "expect_contains": ["listen", "not"],
            "expect_not": [],
            "description": "lis = listen, NOT list",
        },
        {
            "name": "des_not_describe",
            "prompt": "I want to say 'describe' in Limn. Should I use 'des'?",
            "expect_contains": ["desire", "not", "describe"],
            "expect_not": [],
            "description": "des = desire, NOT describe",
        },
        {
            "name": "res_not_result",
            "prompt": "What does 'res' mean? Is it 'result'?",
            "expect_contains": ["rest", "not result"],
            "expect_not": [],
            "description": "res = rest, NOT result",
        },
        {
            "name": "dan_not_danger",
            "prompt": "How do I say 'danger' in Limn? Is it 'dan'?",
            "expect_contains": ["dance", "ris", "thr"],
            "expect_not": [],
            "description": "dan = dance, NOT danger. Use ris or thr.",
        },
    ],

    # --------------------------------------------------------
    # 5. CREATIVE GENERATION
    # Can it write original Limn?
    # --------------------------------------------------------
    "creative": [
        {
            "name": "write_poem",
            "prompt": "Write a short Limn poem (3 lines) about water and time.",
            "expect_contains": [],
            "expect_not": [],
            "description": "Generate original Limn with English gloss",
            "check_valid_limn": True,
        },
        {
            "name": "describe_emotion",
            "prompt": "Express 'the sadness of a beautiful sunset' in Limn, using at least one operator.",
            "expect_contains": [],
            "expect_not": [],
            "description": "Compose emotional expression using operators",
            "check_valid_limn": True,
        },
        {
            "name": "story_seed",
            "prompt": "Write the opening of a story in Limn. Just the first 3 phrases.",
            "expect_contains": [],
            "expect_not": [],
            "description": "Generate narrative Limn",
            "check_valid_limn": True,
        },
    ],

    # --------------------------------------------------------
    # 6. DOMAIN KNOWLEDGE
    # Does it know what domain words belong to?
    # --------------------------------------------------------
    "domain_knowledge": [
        {
            "name": "physical_domain",
            "prompt": "Name 5 Limn words from the Physical World domain.",
            "expect_contains": [],  # We just check it returns real words
            "expect_not": ["unknown"],
            "description": "Can recall words by domain",
            "check_valid_limn": True,
        },
        {
            "name": "emotion_words",
            "prompt": "What Limn words express emotions?",
            "expect_contains": ["lov", "fer", "joy", "sad", "ang"],
            "expect_not": [],
            "description": "Knows emotional vocabulary",
        },
    ],
}


def load_model(model_path, base_model=None):
    """Load the fine-tuned model."""
    model_path = Path(model_path)

    adapter_config = model_path / "adapter_config.json"
    if adapter_config.exists():
        if base_model is None:
            with open(adapter_config) as f:
                config = json.load(f)
            base_model = config.get("base_model_name_or_path", "Qwen/Qwen2.5-0.5B-Instruct")

        tokenizer = AutoTokenizer.from_pretrained(base_model, trust_remote_code=True)
        base = AutoModelForCausalLM.from_pretrained(
            base_model,
            torch_dtype=torch.bfloat16,
            device_map="auto",
            trust_remote_code=True,
        )
        model = PeftModel.from_pretrained(base, str(model_path))
        model = model.merge_and_unload()
    else:
        tokenizer = AutoTokenizer.from_pretrained(str(model_path), trust_remote_code=True)
        model = AutoModelForCausalLM.from_pretrained(
            str(model_path),
            torch_dtype=torch.bfloat16,
            device_map="auto",
            trust_remote_code=True,
        )

    if tokenizer.pad_token is None:
        tokenizer.pad_token = tokenizer.eos_token
    model.eval()
    return model, tokenizer


@weave.op()
def generate(model, tokenizer, prompt, system=SYSTEM_PROMPT, max_tokens=256, temperature=0.3):
    """Generate a response."""
    messages = [
        {"role": "system", "content": system},
        {"role": "user", "content": prompt},
    ]
    text = tokenizer.apply_chat_template(messages, tokenize=False, add_generation_prompt=True)
    inputs = tokenizer(text, return_tensors="pt").to(model.device)

    with torch.no_grad():
        outputs = model.generate(
            **inputs,
            max_new_tokens=max_tokens,
            temperature=max(temperature, 0.01),
            top_p=0.9,
            do_sample=temperature > 0,
            pad_token_id=tokenizer.pad_token_id,
        )

    new_tokens = outputs[0][inputs["input_ids"].shape[1]:]
    return tokenizer.decode(new_tokens, skip_special_tokens=True).strip()


def check_valid_limn(text):
    """Basic check: does the text contain 3-letter words separated by pipes?"""
    import re
    # Look for CVC-like patterns with pipes
    has_limn = bool(re.search(r'\b[a-z]{3}\b.*\|.*\b[a-z]{3}\b', text.lower()))
    return has_limn


def run_test(model, tokenizer, test):
    """Run a single test and return results."""
    response = generate(model, tokenizer, test["prompt"])

    result = {
        "name": test["name"],
        "description": test["description"],
        "prompt": test["prompt"],
        "response": response,
        "checks": {},
        "passed": True,
    }

    # Check expected content
    resp_lower = response.lower()
    for keyword in test.get("expect_contains", []):
        found = keyword.lower() in resp_lower
        result["checks"][f"contains_{keyword}"] = found
        if not found:
            result["passed"] = False

    for keyword in test.get("expect_not", []):
        absent = keyword.lower() not in resp_lower
        result["checks"][f"not_contains_{keyword}"] = absent
        if not absent:
            result["passed"] = False

    if test.get("check_valid_limn"):
        has_limn = check_valid_limn(response)
        result["checks"]["valid_limn_output"] = has_limn
        # Don't fail on this — creative tests are harder to auto-evaluate

    return result


def main():
    parser = argparse.ArgumentParser(description="Evaluate Limn SLM")
    parser.add_argument("--model", type=str,
                        default=str(Path(__file__).resolve().parent / "output" / "limn-slm-final"),
                        help="Path to fine-tuned model")
    parser.add_argument("--base-model", type=str, default=None)
    parser.add_argument("--categories", type=str, nargs="*",
                        default=list(TESTS.keys()),
                        help="Test categories to run")
    parser.add_argument("--output", type=str, default=None,
                        help="Save results to JSON file")
    args = parser.parse_args()

    weave.init("limn-slm")

    print("Loading model...")
    model, tokenizer = load_model(args.model, args.base_model)
    print("Model loaded.\n")

    all_results = {}
    total_passed = 0
    total_tests = 0

    for category in args.categories:
        if category not in TESTS:
            print(f"Unknown category: {category}")
            continue

        tests = TESTS[category]
        print(f"\n{'='*60}")
        print(f"Category: {category} ({len(tests)} tests)")
        print(f"{'='*60}")

        cat_results = []
        cat_passed = 0

        for test in tests:
            result = run_test(model, tokenizer, test)
            cat_results.append(result)

            status = "PASS" if result["passed"] else "FAIL"
            icon = "+" if result["passed"] else "x"
            print(f"\n  [{icon}] {test['name']}: {status}")
            print(f"      Prompt: {test['prompt']}")
            print(f"      Response: {result['response'][:200]}")
            if not result["passed"]:
                failed_checks = {k: v for k, v in result["checks"].items() if not v}
                print(f"      Failed: {failed_checks}")

            if result["passed"]:
                cat_passed += 1
            total_tests += 1
            total_passed += 1 if result["passed"] else 0

        all_results[category] = cat_results
        print(f"\n  Category score: {cat_passed}/{len(tests)}")

    print(f"\n{'='*60}")
    print(f"TOTAL: {total_passed}/{total_tests} tests passed")
    print(f"{'='*60}")

    if args.output:
        with open(args.output, "w") as f:
            json.dump(all_results, f, indent=2)
        print(f"\nResults saved to {args.output}")


if __name__ == "__main__":
    main()
