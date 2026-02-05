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
@ projection: A@B = B-aspect of A (lov@fea = fear-aspect of love)
* interference: A*B = emergent blend (sol*liq = gel)
^ gradient: A^N = intensity 0.0-1.0 (big^0.7 = fairly big)
\\ subtraction: A\\B = A without B (lov\\fea = love without fear)
± superposition: A±B = quantum both/and (joy±fea = joyful-fear)
: conditional: A:B = A given B (lov:fth = love given faith)

You think, speak, and reason in Limn. Respond in Limn."""

# ============================================================
# TEST SUITE
# ============================================================

TESTS = {
    # --------------------------------------------------------
    # 1. NOVEL COMPOSITIONS (never in training data)
    # Can it compose meanings for operator expressions it hasn't seen?
    # All prompts in Limn.
    # --------------------------------------------------------
    "novel_composition": [
        {
            "name": "novel_projection",
            "prompt": "mea: joy@lov",
            "expect_contains": ["lov", "joy", "@"],
            "expect_not": [],
            "description": "Novel projection — joy@lov was NOT in training data",
        },
        {
            "name": "novel_interference",
            "prompt": "mea: hot*wet",
            "expect_contains": ["hot", "wet", "*"],
            "expect_not": [],
            "description": "Novel interference — hot*wet creates emergent meaning",
        },
        {
            "name": "novel_subtraction",
            "prompt": "mea: aqu\\hot",
            "expect_contains": ["aqu", "hot", "\\"],
            "expect_not": [],
            "description": "Novel subtraction — water without heat",
        },
        {
            "name": "novel_gradient",
            "prompt": "mea: fea^0.2",
            "expect_contains": ["fea", "0.2"],
            "expect_not": [],
            "description": "Novel gradient — very slight fear",
        },
        {
            "name": "novel_superposition",
            "prompt": "mea: joy±fea",
            "expect_contains": ["joy", "fea", "±"],
            "expect_not": [],
            "description": "Novel superposition — joy and fear together",
        },
        {
            "name": "novel_conditional",
            "prompt": "mea: cur:dan",
            "expect_contains": ["cur", "dan", ":"],
            "expect_not": [],
            "description": "Novel conditional — curiosity given dance",
        },
    ],

    # --------------------------------------------------------
    # 2. LIMN-ONLY PROMPTS (no English crutch)
    # Can it understand instructions written entirely in Limn?
    # --------------------------------------------------------
    "limn_only": [
        {
            "name": "limn_translate_request",
            "prompt": "tra lim: nox dee | sta bri | sel alo",
            "expect_contains": ["nox", "sta", "sel"],
            "expect_not": [],
            "description": "Translate/transform Limn phrase — request in Limn",
        },
        {
            "name": "limn_word_query",
            "prompt": "mea: lov",
            "expect_contains": ["lov"],
            "expect_not": [],
            "description": "Ask meaning using Limn structure (mea = meaning)",
        },
        {
            "name": "limn_composition_query",
            "prompt": "mea: lov@fea",
            "expect_contains": ["lov", "fea", "@"],
            "expect_not": [],
            "description": "Ask composed meaning in Limn",
        },
        {
            "name": "limn_continue",
            "prompt": "nox dee | sta bri | ...",
            "expect_contains": [],  # Any valid Limn continuation
            "expect_not": [],
            "description": "Continue a Limn phrase (creative completion)",
            "check_valid_limn": True,
        },
    ],

    # --------------------------------------------------------
    # 3. SEMANTIC REASONING (does it understand operator semantics?)
    # Prompts in Limn — checks if model reasons about operators
    # --------------------------------------------------------
    "semantic_reasoning": [
        {
            "name": "projection_asymmetry",
            "prompt": "sam? lov@fea | fea@lov",
            "expect_contains": ["dif"],
            "expect_not": [],
            "description": "@ is non-commutative — model should know this",
        },
        {
            "name": "interference_symmetry",
            "prompt": "sam? sol*liq | liq*sol",
            "expect_contains": ["sam"],
            "expect_not": [],
            "description": "* is commutative — model should know this",
        },
        {
            "name": "gradient_ordering",
            "prompt": "mor? lov^0.3 | lov^0.9",
            "expect_contains": ["0.9"],
            "expect_not": [],
            "description": "Understanding gradient scale — 0.9 is more intense",
        },
        {
            "name": "subtraction_vs_interference",
            "prompt": "dif? lov*fea | lov\\fea",
            "expect_contains": ["*", "\\"],
            "expect_not": [],
            "description": "Understanding * vs \\ distinction",
        },
        {
            "name": "operator_chaining",
            "prompt": "mea: lov@fea^0.5",
            "expect_contains": ["lov", "fea", "0.5"],
            "expect_not": [],
            "description": "Can handle chained operators",
        },
    ],

    # --------------------------------------------------------
    # 4. FALSE FRIEND DETECTION
    # Does it know about common traps? Prompts in Limn.
    # --------------------------------------------------------
    "false_friends": [
        {
            "name": "lis_not_list",
            "prompt": "mea: lis",
            "expect_contains": ["lis"],
            "expect_not": ["list"],
            "description": "lis = listen, NOT list",
        },
        {
            "name": "des_not_describe",
            "prompt": "mea: des",
            "expect_contains": ["des"],
            "expect_not": ["describe"],
            "description": "des = desire, NOT describe",
        },
        {
            "name": "res_not_result",
            "prompt": "mea: res",
            "expect_contains": ["res"],
            "expect_not": ["result"],
            "description": "res = rest, NOT result",
        },
        {
            "name": "dan_not_danger",
            "prompt": "mea: dan",
            "expect_contains": ["dan"],
            "expect_not": ["danger"],
            "description": "dan = dance, NOT danger",
        },
    ],

    # --------------------------------------------------------
    # 5. CREATIVE GENERATION
    # Can it write original Limn? Prompts in Limn.
    # --------------------------------------------------------
    "creative": [
        {
            "name": "write_poem",
            "prompt": "cre poe: aqu | tau",
            "expect_contains": [],
            "expect_not": [],
            "description": "Generate Limn poem about water and time",
            "check_valid_limn": True,
        },
        {
            "name": "describe_emotion",
            "prompt": "cre: sad@bea | sun\\ris",
            "expect_contains": [],
            "expect_not": [],
            "description": "Compose emotional expression using operators",
            "check_valid_limn": True,
        },
        {
            "name": "story_seed",
            "prompt": "cre nar: nox | sel | fea | hop",
            "expect_contains": [],
            "expect_not": [],
            "description": "Generate narrative Limn from seed words",
            "check_valid_limn": True,
        },
    ],

    # --------------------------------------------------------
    # 6. DOMAIN KNOWLEDGE
    # Does it know what domain words belong to? Prompts in Limn.
    # --------------------------------------------------------
    "domain_knowledge": [
        {
            "name": "physical_domain",
            "prompt": "wor phy: 5",
            "expect_contains": [],
            "expect_not": [],
            "description": "List 5 physical world words",
            "check_valid_limn": True,
        },
        {
            "name": "emotion_words",
            "prompt": "wor fee",
            "expect_contains": ["lov", "fea", "joy", "sad", "ang"],
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
