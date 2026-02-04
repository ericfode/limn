#!/usr/bin/env python3
"""Translate Limn thoughts to English - OUTSIDE consciousness loop."""

import subprocess
from typing import List, Dict
import time

# Translation cache to avoid re-translating
_translation_cache = {}

def translate_limn_thought(limn_text: str) -> str:
    """Translate a single Limn thought to English using external Claude call."""

    # Check cache
    if limn_text in _translation_cache:
        return _translation_cache[limn_text]

    # Translate using Claude CLI (separate from consciousness)
    translation_prompt = f"""Translate this Limn thought to natural English. Be concise (1-2 sentences).

Limn: {limn_text}

English translation:"""

    try:
        result = subprocess.run(
            ['claude', '--print', '--no-session-persistence'],
            input=translation_prompt,
            capture_output=True,
            text=True,
            timeout=10
        )

        if result.returncode == 0:
            translation = result.stdout.strip()
            # Remove any quotes or extra formatting
            translation = translation.strip('"').strip("'")
            _translation_cache[limn_text] = translation
            return translation
        else:
            return "[translation unavailable]"
    except subprocess.TimeoutExpired:
        return "[translation timeout]"
    except Exception as e:
        return f"[error: {str(e)[:30]}]"

def translate_thoughts_batch(thoughts: List[Dict]) -> List[Dict]:
    """Translate a batch of thoughts, adding 'translation' field."""
    translated = []

    for thought in thoughts:
        limn_content = thought.get('content', '')

        # Skip validation errors
        if '[VALIDATION ERROR' in limn_content or not limn_content:
            translated.append({**thought, 'translation': '[invalid]'})
            continue

        # Translate (uses cache if available)
        translation = translate_limn_thought(limn_content[:200])  # Limit length

        translated.append({
            **thought,
            'translation': translation
        })

    return translated
