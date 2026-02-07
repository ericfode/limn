#!/usr/bin/env python3
"""
Noise Injection Experiment: H16 — Density as Fragility

Tests whether Limn's 53% density advantage is actually fragility under noise.

Shannon's source-channel separation theorem tells us:
- Dense codes are efficient for noiseless channels
- Redundancy is error correction for noisy channels
- English has ~50% redundancy (Shannon 1948)
- Limn has less redundancy (higher density → less slack)

Experimental design:
1. Take 329 HGttG parallel pairs (Limn + English)
2. For each corruption rate (1%, 5%, 10%, 20%):
   - Corrupt characters uniformly at random (substitution)
   - Measure: word survival rate (corrupted word == original word)
   - Measure: word recoverability (nearest valid word == original word)
   - Measure: collision rate (corrupted word is a DIFFERENT valid word)
   - Measure: meaning preservation (Levenshtein similarity of full text)
3. Compare Limn vs English degradation curves

Key prediction (H16): Limn degrades FASTER than English because:
- 3-letter words: single corruption affects 33% of the word
- Dense namespace: 2005/17576 = 11.4% saturation → corrupted words often
  accidentally become other valid words (confusables, not just broken words)
- No contextual redundancy: Limn lacks function words, articles, determiners

— Lex
"""

import json
import math
import random
import statistics
import string
import subprocess
from pathlib import Path

PAIRS_FILE = Path("/home/eric/src/limntown/limn/crew/translator/hgttg-training-pairs.jsonl")
OUTPUT_DIR = Path(__file__).resolve().parent
SEED = 42
CORRUPTION_RATES = [0.01, 0.05, 0.10, 0.20]
N_TRIALS = 10  # Average over multiple corruption runs


def load_pairs() -> list:
    pairs = []
    with open(PAIRS_FILE) as f:
        for line in f:
            d = json.loads(line)
            pairs.append({
                "id": d["id"],
                "english": d["english"].strip('"'),
                "limn": d["limn"],
            })
    return pairs


def load_limn_vocab() -> set:
    """Load Limn vocabulary from the database via vocab.sh."""
    result = subprocess.run(
        ["./scripts/vocab.sh", "sql", "SELECT word FROM words"],
        capture_output=True, text=True,
        cwd="/home/eric/src/limntown2/limn/crew/researcher"
    )
    words = set()
    for line in result.stdout.splitlines():
        line = line.strip().strip("|").strip()
        if line and line.isalpha() and 2 <= len(line) <= 4:
            words.add(line.lower())
    return words


def load_english_vocab(pairs: list = None) -> set:
    """Load English vocabulary. Uses system dict if available, else builds
    from corpus + comprehensive common word list.

    The key requirement: we need enough words to measure collision rates
    accurately. A corrupted word might become any other valid English word,
    so the vocab should include all common English words, not just those
    in the corpus.
    """
    words = set()

    # Try system dictionary first
    dict_path = Path("/usr/share/dict/words")
    if dict_path.exists():
        with open(dict_path) as f:
            for line in f:
                w = line.strip().lower()
                if w.isalpha() and len(w) >= 2:
                    words.add(w)

    # If no system dict, use comprehensive common word list
    # This is the 3000 most common English words (covers 95%+ of typical text)
    if len(words) < 1000:
        # Build from corpus
        if pairs:
            for p in pairs:
                for token in p["english"].split():
                    w = token.strip('.,!?;:\'"()-…""').lower()
                    if w.isalpha() and len(w) >= 2:
                        words.add(w)

        # Add the 1000 most common English words to ensure adequate coverage
        common_words = {
            "the", "be", "to", "of", "and", "a", "in", "that", "have", "i",
            "it", "for", "not", "on", "with", "he", "as", "you", "do", "at",
            "this", "but", "his", "by", "from", "they", "we", "say", "her",
            "she", "or", "an", "will", "my", "one", "all", "would", "there",
            "their", "what", "so", "up", "out", "if", "about", "who", "get",
            "which", "go", "me", "when", "make", "can", "like", "time", "no",
            "just", "him", "know", "take", "people", "into", "year", "your",
            "good", "some", "could", "them", "see", "other", "than", "then",
            "now", "look", "only", "come", "its", "over", "think", "also",
            "back", "after", "use", "two", "how", "our", "work", "first",
            "well", "way", "even", "new", "want", "because", "any", "these",
            "give", "day", "most", "us", "is", "are", "was", "were", "been",
            "being", "has", "had", "did", "does", "doing", "made", "got",
            "went", "going", "came", "coming", "took", "taken", "said",
            "let", "much", "more", "too", "very", "still", "own", "may",
            "might", "should", "must", "need", "never", "each", "every",
            "few", "many", "while", "where", "why", "how", "same", "here",
            "such", "through", "off", "down", "between", "long", "great",
            "little", "old", "big", "man", "small", "large", "next", "right",
            "left", "high", "last", "far", "end", "hand", "part", "place",
            "case", "point", "thing", "world", "life", "home", "head",
            "water", "room", "house", "side", "night", "light", "story",
            "fact", "month", "lot", "number", "name", "school", "body",
            "word", "family", "book", "girl", "boy", "city", "eye", "door",
            "yes", "again", "already", "always", "another", "before",
            "both", "during", "enough", "nothing", "something", "without",
            "found", "keep", "put", "run", "seem", "set", "show", "start",
            "turn", "try", "kind", "young", "away", "under", "around",
            "really", "however", "thought", "sure", "whole", "together",
            "possible", "rather", "almost", "often", "usually", "perhaps",
            "above", "along", "across", "behind", "against", "upon",
            "form", "line", "face", "power", "money", "change", "state",
            "less", "best", "better", "hard", "early", "ever", "idea",
            "answer", "close", "open", "watch", "begin", "began", "begun",
            "half", "mind", "feel", "felt", "sense", "second", "child",
            "children", "stood", "short", "land", "move", "asked", "lost",
            "told", "love", "full", "piece", "walk", "clear", "fall",
            "true", "dead", "white", "black", "able", "toward", "least",
            "matter", "heard", "reason", "stood", "later", "quite",
            "death", "heart", "read", "once", "held", "until", "known",
            "looked", "seemed", "turned", "several", "real", "car", "sat",
            "table", "four", "five", "six", "seven", "eight", "nine", "ten",
            "sun", "arm", "star", "deep", "dark", "sea", "air", "fire",
            "earth", "red", "blue", "green", "gold", "bad", "happy", "sad",
            "cold", "hot", "war", "peace", "god", "king", "queen", "sir",
            "strange", "simple", "single", "either", "often", "certain",
            "human", "perhaps", "among", "thus", "itself", "herself",
            "himself", "myself", "yourself", "itself", "ourselves",
            "whether", "whose", "became", "brought", "became", "gone",
            "given", "sort", "voice", "road", "stop", "field", "figure",
            "hit", "miss", "act", "age", "cut", "bit", "sat", "lay",
            "yet", "nor", "per", "via", "add", "aid", "aim", "ask",
            "ate", "bag", "ban", "bar", "bat", "bay", "bed", "bet",
            "bid", "bin", "bow", "box", "bug", "bus", "buy", "cab",
            "cap", "cat", "cop", "cow", "cry", "cup", "dam", "dig",
            "dog", "dot", "dry", "due", "ear", "eat", "egg", "era",
            "fan", "fat", "fee", "fit", "fix", "fly", "fog", "fun",
            "fur", "gap", "gas", "got", "gun", "gut", "guy", "hat",
            "hen", "hip", "hog", "ice", "ill", "ink", "inn", "ion",
            "jam", "jar", "jaw", "jet", "job", "joy", "jug", "key",
            "kid", "kit", "lab", "lap", "law", "leg", "lid", "lie",
            "lip", "log", "low", "mad", "map", "mat", "mix", "mob",
            "mud", "mug", "net", "new", "nod", "not", "now", "nut",
            "oak", "odd", "oil", "ore", "our", "out", "owe", "own",
            "pad", "pan", "pat", "pay", "pen", "pet", "pie", "pig",
            "pin", "pit", "pop", "pot", "pub", "raw", "ray", "red",
            "rib", "rid", "rig", "rim", "rip", "rob", "rod", "rot",
            "row", "rug", "rum", "sad", "sew", "shy", "sin", "sip",
            "ski", "sky", "sly", "sob", "son", "spy", "sum", "tab",
            "tag", "tan", "tap", "tar", "tax", "tea", "ten", "tie",
            "tin", "tip", "toe", "ton", "top", "tow", "toy", "tub",
            "van", "vet", "vow", "war", "wax", "web", "wet", "wig",
            "win", "wit", "woe", "wok", "won", "zoo",
        }
        words |= common_words

    return words


def corrupt_char(c: str, rng: random.Random) -> str:
    """Replace a character with a random different lowercase letter."""
    if not c.isalpha():
        return c  # Don't corrupt non-alpha characters
    pool = string.ascii_lowercase
    new = rng.choice(pool)
    while new == c.lower():
        new = rng.choice(pool)
    return new if c.islower() else new.upper()


def corrupt_text(text: str, rate: float, rng: random.Random) -> str:
    """Corrupt text at a given character-level rate (substitution only on alpha chars)."""
    chars = list(text)
    alpha_indices = [i for i, c in enumerate(chars) if c.isalpha()]
    n_corrupt = max(1, int(len(alpha_indices) * rate))
    targets = rng.sample(alpha_indices, min(n_corrupt, len(alpha_indices)))
    for i in targets:
        chars[i] = corrupt_char(chars[i], rng)
    return "".join(chars)


def extract_words(text: str, is_limn: bool) -> list:
    """Extract content words from text."""
    if is_limn:
        # Limn: split on spaces, pipes, colons, newlines; strip operators
        import re
        # Remove operators and modifiers: @, *, ^, ±, +-, ^0.x, etc.
        tokens = re.split(r'[\s|:\n]+', text)
        words = []
        for t in tokens:
            # Strip operators and numeric modifiers
            clean = re.sub(r'[@*^±].*', '', t)
            clean = re.sub(r'[^a-zA-Z\-]', '', clean)
            # Handle negation prefix
            if clean.startswith('nu-'):
                words.append('nu')
                clean = clean[3:]
            parts = clean.split('-')
            for p in parts:
                p = p.strip().lower()
                if p and p.isalpha() and len(p) >= 2:
                    words.append(p)
        return words
    else:
        # English: split on whitespace, strip punctuation
        tokens = text.split()
        words = []
        for t in tokens:
            clean = t.strip('.,!?;:\'"()-…""')
            if clean and clean.isalpha() and len(clean) >= 2:
                words.append(clean.lower())
        return words


def edit_distance(a: str, b: str) -> int:
    """Levenshtein distance."""
    if len(a) < len(b):
        return edit_distance(b, a)
    if len(b) == 0:
        return len(a)
    prev = list(range(len(b) + 1))
    for i in range(len(a)):
        curr = [i + 1]
        for j in range(len(b)):
            cost = 0 if a[i] == b[j] else 1
            curr.append(min(curr[j] + 1, prev[j + 1] + 1, prev[j] + cost))
        prev = curr
    return prev[len(b)]


def nearest_word(corrupted: str, vocab: set) -> tuple:
    """Find nearest word in vocabulary by edit distance.
    Returns (word, distance). If exact match, distance=0."""
    if corrupted in vocab:
        return corrupted, 0
    best_word = None
    best_dist = float('inf')
    for w in vocab:
        # Only check words of similar length for efficiency
        if abs(len(w) - len(corrupted)) > 2:
            continue
        d = edit_distance(corrupted, w)
        if d < best_dist:
            best_dist = d
            best_word = w
            if d == 1:
                break  # Can't do better for a corrupted word
    return best_word, best_dist


def analyze_word_level(original_words: list, corrupted_text: str,
                       is_limn: bool, vocab: set) -> dict:
    """Analyze word-level corruption effects."""
    corrupted_words = extract_words(corrupted_text, is_limn)

    # Pad/truncate to match (corruption might split/merge tokens)
    n = min(len(original_words), len(corrupted_words))
    if n == 0:
        return {"survived": 0, "recovered": 0, "collided": 0,
                "broken": 0, "total": max(1, len(original_words))}

    survived = 0  # Corrupted word == original word
    recovered = 0  # Nearest valid word == original word
    collided = 0  # Corrupted word is a DIFFERENT valid word (dangerous!)
    broken = 0    # Corrupted word is not valid, nearest ≠ original

    for i in range(n):
        orig = original_words[i].lower()
        corr = corrupted_words[i].lower()

        if corr == orig:
            survived += 1
        elif corr in vocab:
            # Corrupted INTO a different valid word — semantic collision
            collided += 1
        else:
            nearest, dist = nearest_word(corr, vocab)
            if nearest == orig:
                recovered += 1
            else:
                broken += 1

    total = max(1, len(original_words))
    return {
        "survived": survived,
        "recovered": recovered,
        "collided": collided,
        "broken": broken,
        "total": total,
    }


def text_similarity(original: str, corrupted: str) -> float:
    """Character-level Levenshtein similarity (1 - normalized_distance)."""
    d = edit_distance(original.lower(), corrupted.lower())
    return 1.0 - d / max(len(original), len(corrupted), 1)


def run_experiment():
    print("Noise Injection Experiment — H16: Density as Fragility")
    print("=" * 70)

    pairs = load_pairs()
    print(f"  Loaded {len(pairs)} parallel pairs")

    limn_vocab = load_limn_vocab()
    print(f"  Limn vocabulary: {len(limn_vocab)} words")

    english_vocab = load_english_vocab(pairs)
    print(f"  English vocabulary: {len(english_vocab)} words")

    # Pre-extract original words
    for p in pairs:
        p["limn_words"] = extract_words(p["limn"], is_limn=True)
        p["english_words"] = extract_words(p["english"], is_limn=False)

    # Limn word stats
    limn_word_lengths = [len(w) for p in pairs for w in p["limn_words"]]
    en_word_lengths = [len(w) for p in pairs for w in p["english_words"]]
    print(f"  Mean Limn word length: {statistics.mean(limn_word_lengths):.1f} chars")
    print(f"  Mean English word length: {statistics.mean(en_word_lengths):.1f} chars")

    # Namespace density analysis
    # For Limn: 3-letter words in 26^3 = 17,576 possible slots
    n_limn_3char = sum(1 for w in limn_vocab if len(w) == 3)
    limn_density = n_limn_3char / (26 ** 3)
    print(f"  Limn 3-char namespace density: {n_limn_3char}/{26**3} = {limn_density:.3%}")

    # For English: average word length ~5, in 26^5 = ~12M possible slots
    en_3to7 = sum(1 for w in english_vocab if 3 <= len(w) <= 7)
    # Approximate: count words per length bucket
    en_by_len = {}
    for w in english_vocab:
        l = len(w)
        en_by_len[l] = en_by_len.get(l, 0) + 1
    print(f"  English words by common lengths: " +
          ", ".join(f"{l}:{c}" for l, c in sorted(en_by_len.items()) if 2 <= l <= 8))

    results = {}
    rng = random.Random(SEED)

    for rate in CORRUPTION_RATES:
        print(f"\n{'=' * 70}")
        print(f"  Corruption Rate: {rate:.0%}")
        print(f"{'=' * 70}")

        limn_metrics = {
            "survival_rates": [],
            "recovery_rates": [],
            "collision_rates": [],
            "broken_rates": [],
            "text_similarities": [],
        }
        en_metrics = {
            "survival_rates": [],
            "recovery_rates": [],
            "collision_rates": [],
            "broken_rates": [],
            "text_similarities": [],
        }

        for trial in range(N_TRIALS):
            trial_rng = random.Random(rng.randint(0, 2**32))

            for p in pairs:
                # Corrupt Limn
                corrupted_limn = corrupt_text(p["limn"], rate, trial_rng)
                lm_analysis = analyze_word_level(
                    p["limn_words"], corrupted_limn, True, limn_vocab)
                lm_total = lm_analysis["total"]
                limn_metrics["survival_rates"].append(lm_analysis["survived"] / lm_total)
                limn_metrics["recovery_rates"].append(lm_analysis["recovered"] / lm_total)
                limn_metrics["collision_rates"].append(lm_analysis["collided"] / lm_total)
                limn_metrics["broken_rates"].append(lm_analysis["broken"] / lm_total)
                limn_metrics["text_similarities"].append(
                    text_similarity(p["limn"], corrupted_limn))

                # Corrupt English
                corrupted_en = corrupt_text(p["english"], rate, trial_rng)
                en_analysis = analyze_word_level(
                    p["english_words"], corrupted_en, False, english_vocab)
                en_total = en_analysis["total"]
                en_metrics["survival_rates"].append(en_analysis["survived"] / en_total)
                en_metrics["recovery_rates"].append(en_analysis["recovered"] / en_total)
                en_metrics["collision_rates"].append(en_analysis["collided"] / en_total)
                en_metrics["broken_rates"].append(en_analysis["broken"] / en_total)
                en_metrics["text_similarities"].append(
                    text_similarity(p["english"], corrupted_en))

        # Compute means and report
        def mean(lst):
            return statistics.mean(lst) if lst else 0

        lm_surv = mean(limn_metrics["survival_rates"])
        en_surv = mean(en_metrics["survival_rates"])
        lm_recv = mean(limn_metrics["recovery_rates"])
        en_recv = mean(en_metrics["recovery_rates"])
        lm_coll = mean(limn_metrics["collision_rates"])
        en_coll = mean(en_metrics["collision_rates"])
        lm_brok = mean(limn_metrics["broken_rates"])
        en_brok = mean(en_metrics["broken_rates"])
        lm_sim = mean(limn_metrics["text_similarities"])
        en_sim = mean(en_metrics["text_similarities"])

        print(f"\n  {'Metric':<35s} {'English':>10s} {'Limn':>10s} {'Delta':>10s}")
        print(f"  {'─' * 67}")
        print(f"  {'Word survival rate':<35s} {en_surv:>10.3f} {lm_surv:>10.3f} {lm_surv - en_surv:>+10.3f}")
        print(f"  {'Word recovery rate':<35s} {en_recv:>10.3f} {lm_recv:>10.3f} {lm_recv - en_recv:>+10.3f}")
        print(f"  {'Collision rate (silent error!)':<35s} {en_coll:>10.3f} {lm_coll:>10.3f} {lm_coll - en_coll:>+10.3f}")
        print(f"  {'Broken word rate':<35s} {en_brok:>10.3f} {lm_brok:>10.3f} {lm_brok - en_brok:>+10.3f}")
        print(f"  {'Text char similarity':<35s} {en_sim:>10.3f} {lm_sim:>10.3f} {lm_sim - en_sim:>+10.3f}")

        # Effective meaning preservation = survived + recovered (not collided)
        lm_preserved = lm_surv + lm_recv
        en_preserved = en_surv + en_recv
        print(f"\n  {'Meaning preserved (surv+recv)':<35s} {en_preserved:>10.3f} {lm_preserved:>10.3f} {lm_preserved - en_preserved:>+10.3f}")
        print(f"  {'Silent corruption (collision)':<35s} {en_coll:>10.3f} {lm_coll:>10.3f} {lm_coll - en_coll:>+10.3f}")

        results[f"rate_{rate}"] = {
            "rate": rate,
            "limn": {
                "word_survival": lm_surv,
                "word_recovery": lm_recv,
                "collision_rate": lm_coll,
                "broken_rate": lm_brok,
                "text_similarity": lm_sim,
                "meaning_preserved": lm_preserved,
            },
            "english": {
                "word_survival": en_surv,
                "word_recovery": en_recv,
                "collision_rate": en_coll,
                "broken_rate": en_brok,
                "text_similarity": en_sim,
                "meaning_preserved": en_preserved,
            },
            "delta": {
                "survival": lm_surv - en_surv,
                "recovery": lm_recv - en_recv,
                "collision": lm_coll - en_coll,
                "meaning_preserved": lm_preserved - en_preserved,
            },
        }

    # =========================================================================
    # Summary
    # =========================================================================
    print(f"\n{'=' * 70}")
    print(f"  SUMMARY — Degradation Curves")
    print(f"{'=' * 70}")
    print(f"\n  {'Rate':>6s} | {'English':>12s} {'Limn':>12s} {'Delta':>10s} | {'En Coll':>10s} {'Lm Coll':>10s}")
    print(f"  {'':>6s} | {'preserved':>12s} {'preserved':>12s} {'':>10s} | {'rate':>10s} {'rate':>10s}")
    print(f"  {'─' * 70}")

    for rate in CORRUPTION_RATES:
        r = results[f"rate_{rate}"]
        en = r["english"]
        lm = r["limn"]
        d = r["delta"]
        print(f"  {rate:>5.0%}  | {en['meaning_preserved']:>12.3f} {lm['meaning_preserved']:>12.3f} {d['meaning_preserved']:>+10.3f} | {en['collision_rate']:>10.4f} {lm['collision_rate']:>10.4f}")

    # Determine if H16 is supported
    rates_limn_worse = sum(
        1 for rate in CORRUPTION_RATES
        if results[f"rate_{rate}"]["delta"]["meaning_preserved"] < -0.01
    )
    rates_collision_worse = sum(
        1 for rate in CORRUPTION_RATES
        if results[f"rate_{rate}"]["delta"]["collision"] > 0.001
    )

    print(f"\n  Limn preserves LESS meaning at {rates_limn_worse}/{len(CORRUPTION_RATES)} corruption rates")
    print(f"  Limn has HIGHER collision rate at {rates_collision_worse}/{len(CORRUPTION_RATES)} corruption rates")

    if rates_limn_worse >= 3:
        print(f"\n  H16 SUPPORTED: Limn's density is fragility under noise.")
        print(f"  The 53% encoding advantage comes at the cost of error resilience.")
        print(f"  Limn needs an explicit error-correction layer for noisy channels.")
        h16_result = "SUPPORTED"
    elif rates_limn_worse >= 2:
        print(f"\n  H16 PARTIALLY SUPPORTED: Limn degrades faster at most corruption rates.")
        h16_result = "PARTIALLY SUPPORTED"
    else:
        print(f"\n  H16 CHALLENGED: Limn does NOT degrade faster than English under noise.")
        print(f"  The density advantage does not correspond to fragility.")
        h16_result = "CHALLENGED"

    if rates_collision_worse >= 2:
        print(f"\n  CRITICAL: Limn's dense namespace causes SILENT corruption.")
        print(f"  Corrupted words become other valid words — the receiver gets")
        print(f"  wrong meaning, not no meaning. This is worse than just losing data.")

    # Save results
    output = {
        "experiment": "noise_injection_h16",
        "pairs_count": len(pairs),
        "n_trials": N_TRIALS,
        "limn_vocab_size": len(limn_vocab),
        "english_vocab_size": len(english_vocab),
        "corruption_rates": CORRUPTION_RATES,
        "results": results,
        "h16_verdict": h16_result,
    }

    output_file = OUTPUT_DIR / "noise_injection_results.json"
    with open(output_file, "w") as f:
        json.dump(output, f, indent=2)
    print(f"\n  Results saved to: {output_file}")


if __name__ == "__main__":
    run_experiment()
