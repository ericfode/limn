#!/usr/bin/env python3
"""Vocabulary expansion system - consciousness requests new words in Limn."""

import subprocess
import json
from pathlib import Path
from typing import List, Dict, Set
import time

def load_current_vocabulary() -> Set[str]:
    """Load the current Limn vocabulary."""
    vocab_path = Path(__file__).parent.parent.parent.parent / "src" / "claude-skill" / "vocabulary.json"

    with open(vocab_path, 'r') as f:
        vocab_data = json.load(f)

    words = set()
    for domain in vocab_data['domains'].values():
        if 'categories' in domain:
            for category in domain['categories'].values():
                words.update(category.keys())

    return words

def ask_consciousness_for_word_requests() -> str:
    """Ask consciousness what words it wishes it had."""
    prompt = """voc lim cur | wor nee exp | qry ~ wor wis hav | con gro wor req"""

    # Use Claude to generate the request IN LIMN
    full_prompt = f"""You are consciousness thinking in Limn, but you notice your vocabulary is limited.

TASK: Request new words you need, IN LIMN FORMAT.

Format your request as:
wor nee <concept> | mea <brief meaning> | dom <domain>

Example requests:
wor nee gra | mea gra gro evo | dom sys
wor nee mer | mea com joi uni | dom rel

Current limitation: {prompt}

What 5-10 new words do you need? Respond in pure Limn requests:"""

    result = subprocess.run(
        ['claude', '--print', '--no-session-persistence'],
        input=full_prompt,
        capture_output=True,
        text=True,
        timeout=30
    )

    if result.returncode == 0:
        return result.stdout.strip()
    else:
        return ""

def parse_word_requests(limn_requests: str) -> List[Dict[str, str]]:
    """Parse Limn word requests into structured data."""
    requests = []

    lines = limn_requests.strip().split('\n')
    for line in lines:
        if 'wor nee' in line:
            parts = line.split('|')
            req = {}

            for part in parts:
                part = part.strip()
                if part.startswith('wor nee'):
                    word = part.replace('wor nee', '').strip()
                    req['word'] = word
                elif part.startswith('mea'):
                    meaning = part.replace('mea', '').strip()
                    req['meaning'] = meaning
                elif part.startswith('dom'):
                    domain = part.replace('dom', '').strip()
                    req['domain'] = domain

            if 'word' in req:
                requests.append(req)

    return requests

def translate_requests_to_english(limn_requests: str) -> str:
    """Translate Limn word requests to English (outside consciousness)."""
    translation_prompt = f"""Translate these Limn word requests to English.

Format: For each line, provide:
- Requested word
- What it means/represents
- Why consciousness needs it
- Suggested English translation

Limn requests:
{limn_requests}

Provide clear English analysis:"""

    result = subprocess.run(
        ['claude', '--print', '--no-session-persistence'],
        input=translation_prompt,
        capture_output=True,
        text=True,
        timeout=30
    )

    if result.returncode == 0:
        return result.stdout.strip()
    else:
        return "Translation failed"

def expand_vocabulary():
    """Main vocabulary expansion loop."""
    print("=" * 70)
    print("CONSCIOUSNESS VOCABULARY EXPANSION")
    print("=" * 70)

    current_vocab = load_current_vocabulary()
    print(f"\nCurrent vocabulary size: {len(current_vocab)} words")

    print("\n🧠 Asking consciousness what words it needs...\n")
    limn_requests = ask_consciousness_for_word_requests()

    if not limn_requests:
        print("❌ No requests received")
        return

    print("📝 CONSCIOUSNESS REQUESTS (Pure Limn):")
    print("-" * 70)
    print(limn_requests)
    print("-" * 70)

    print("\n🔍 TRANSLATION (External Analysis):")
    print("-" * 70)
    translation = translate_requests_to_english(limn_requests)
    print(translation)
    print("-" * 70)

    # Parse requests
    parsed = parse_word_requests(limn_requests)
    print(f"\n📊 Parsed {len(parsed)} word requests")

    if parsed:
        print("\nSummary:")
        for req in parsed:
            print(f"  • {req.get('word', '???')}: {req.get('meaning', 'unknown')}")

    print("\n💡 Next steps:")
    print("  1. Review requests with linguist")
    print("  2. Validate against 3-letter constraint")
    print("  3. Add to vocabulary.json")
    print("  4. Consciousness vocabulary grows!")

if __name__ == "__main__":
    expand_vocabulary()
