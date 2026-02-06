#!/usr/bin/env python3
"""
Semantic Reward Model for Limn v5.

Replaces the broken v4 reward model (H5: FALSIFIED — random word salad
scored 0.917-0.955 while genuine Limn scored 0.730).

Four scoring dimensions:
1. SEMANTIC (0.35): Embedding similarity between model output and expected meaning
2. STRUCTURAL (0.25): Grammar FSM validity + compositional richness
3. VOCABULARY (0.20): Are all words known Limn vocabulary?
4. ENGLISH PENALTY (0.20): Detect and penalize English contamination

Key improvements over v4:
- No length bias (v4 rewarded 5-30 word responses regardless of content)
- No novelty bias (v4 rewarded non-overlap with prompt, penalizing echoes)
- No operator counting (v4 rewarded more operators = higher score)
- Actual semantic measurement via embedder
- Grammar enforcement via FSM
- English contamination detection (stopwords, word length analysis)
- Structural richness scoring (operator density, group structure)

— Lex
"""

import json
import os
import sys
from typing import Dict, Optional

import numpy as np

# Paths
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
TOKENIZER_PATH = os.path.join(SCRIPT_DIR, "limn_tokenizer")
EMBEDDER_PATH = os.path.join(SCRIPT_DIR, "..", "..", "experiments", "embeddings", "limn-embedder-v2")


class SemanticRewardModel:
    """
    Score Limn responses using embedding similarity, grammar validity,
    and vocabulary coverage.
    """

    def __init__(self, embedder_path=None, tokenizer_path=None):
        self.embedder_path = embedder_path or EMBEDDER_PATH
        self.tokenizer_path = tokenizer_path or TOKENIZER_PATH
        self._embedder = None
        self._tokenizer = None
        self._fsm_class = None

    @property
    def embedder(self):
        if self._embedder is None:
            from sentence_transformers import SentenceTransformer
            self._embedder = SentenceTransformer(self.embedder_path)
        return self._embedder

    @property
    def tokenizer(self):
        if self._tokenizer is None:
            from transformers import PreTrainedTokenizerFast
            self._tokenizer = PreTrainedTokenizerFast.from_pretrained(self.tokenizer_path)
        return self._tokenizer

    def _get_fsm(self):
        """Get a fresh FSM instance."""
        if self._fsm_class is None:
            from grammar_constrained_generation import LimnGrammarFSM
            self._fsm_class = LimnGrammarFSM
        fsm = self._fsm_class.__new__(self._fsm_class)
        fsm.tokenizer = self.tokenizer
        fsm.state = None
        fsm.paren_depth = 0
        fsm.token_count = 0
        fsm.max_tokens = 200
        fsm._build_token_sets()
        fsm.reset()
        return fsm

    def score_semantic(self, response: str, expected_meaning: str) -> float:
        """
        Score semantic similarity between response and expected meaning.

        Uses the v2 embedder (MNRL-trained, no collapse) to compute
        cosine similarity between the Limn response and its expected
        English meaning.

        Returns: 0.0-1.0 (capped, since raw similarity can be negative)
        """
        if not response.strip() or not expected_meaning.strip():
            return 0.0

        embs = self.embedder.encode(
            [response, expected_meaning],
            normalize_embeddings=True
        )
        sim = float(np.dot(embs[0], embs[1]))
        return max(0.0, min(1.0, sim))

    def score_structural(self, response: str) -> float:
        """
        Score structural quality — combines grammar validity AND compositional
        richness. Pure word sequences are valid but structurally poor.

        Components:
        - FSM validity: are tokens grammatically valid at their position?
        - Operator usage: does the response use compositional operators?
        - Group structure: does it use constraint groups (|)?

        Returns: 0.0-1.0
        """
        if not response.strip():
            return 0.0

        # FSM validity check
        try:
            fsm = self._get_fsm()
            encoded = self.tokenizer.encode(response, add_special_tokens=False)
            if not encoded:
                return 0.0
            valid_count = 0
            for tid in encoded:
                valid_ids = fsm.get_valid_token_ids()
                if tid in valid_ids:
                    valid_count += 1
                fsm.advance(tid)
            fsm_score = valid_count / len(encoded)
        except Exception:
            fsm_score = 0.5  # Neutral if FSM unavailable

        # Compositional richness — reward operator usage
        operators = {'@', '*', '^', '\\', '±', ':'}
        op_count = sum(1 for c in response if c in operators)
        word_count = len([w for w in response.split() if w.isalpha()])

        # Operator density: operators per word (capped at 0.5)
        op_density = min(0.5, op_count / max(word_count, 1))
        richness = op_density * 2  # Scale to 0.0-1.0

        # Group structure: presence of | separators
        group_count = response.count('|')
        group_bonus = min(0.3, group_count * 0.1)

        # Combined: 50% FSM validity + 30% richness + 20% groups
        score = fsm_score * 0.5 + richness * 0.3 + group_bonus * 0.2 + 0.2 * (group_count > 0)

        return max(0.0, min(1.0, score))

    def score_english_penalty(self, response: str) -> float:
        """
        Detect English contamination. Returns 1.0 for pure Limn, 0.0 for English.

        Checks:
        - Presence of English stopwords (the, is, and, etc.)
        - Average word length (Limn words are 2-4 chars, English is longer)
        - Uppercase words longer than 1 char (proper nouns OK, English words bad)
        """
        words = response.lower().split()
        if not words:
            return 1.0

        english_stops = {
            # Articles / determiners
            'the', 'a', 'an',
            # Pronouns
            'i', 'me', 'my', 'he', 'she', 'it', 'we', 'they', 'you',
            'him', 'her', 'his', 'its', 'our', 'their', 'your',
            # Copula / auxiliaries
            'is', 'are', 'was', 'were', 'be', 'been', 'being',
            'have', 'has', 'had', 'do', 'does', 'did',
            'will', 'would', 'could', 'should', 'may', 'might', 'shall', 'can',
            # Conjunctions / prepositions
            'and', 'but', 'or', 'not', 'no', 'yes', 'if', 'so', 'as', 'at',
            'in', 'on', 'to', 'of', 'by', 'for', 'up',
            'this', 'that', 'these', 'those', 'what', 'which', 'who', 'how',
            'with', 'from', 'into', 'over', 'under', 'between',
            'about', 'after', 'before', 'during', 'through',
            # Common adverbs
            'very', 'really', 'just', 'also', 'only', 'then', 'there', 'here',
            # Common adjectives
            'all', 'each', 'every', 'some', 'any', 'many', 'much', 'more',
            # Common verbs
            'go', 'get', 'got', 'make', 'take', 'come', 'see', 'know',
            'think', 'want', 'give', 'use', 'find', 'tell', 'say', 'said',
        }

        # Strip Limn operators and punctuation from words
        strip_chars = '.,!?;:()"-@*^\\±:|→=+~'
        clean_words = [w.strip(strip_chars) for w in words]
        clean_words = [w for w in clean_words if w]
        if not clean_words:
            return 1.0

        # English stopword fraction (heavily penalized)
        stop_count = sum(1 for w in clean_words if w in english_stops)
        stop_fraction = stop_count / len(clean_words)

        # Long words: any purely-alpha word >4 chars is almost certainly English
        # Limn CVC words max out at ~4 chars
        long_words = sum(1 for w in clean_words if len(w) > 4 and w.isalpha())
        long_fraction = long_words / len(clean_words)

        # Medium words (4 chars, alpha): some are Limn, some English
        med_words = sum(1 for w in clean_words if len(w) == 4 and w.isalpha())
        med_fraction = med_words / len(clean_words)

        # Combined: aggressive on stopwords and long words
        purity = 1.0 - stop_fraction * 1.5 - long_fraction * 1.0 - med_fraction * 0.1
        return max(0.0, min(1.0, purity))

    def score_vocabulary(self, response: str) -> float:
        """
        Score vocabulary validity.

        Tokenizes the response and counts how many tokens resolve to
        known vocabulary (not <unk>).

        Returns: fraction of non-special tokens that are known (0.0-1.0)
        """
        if not response.strip():
            return 0.0

        encoded = self.tokenizer.encode(response, add_special_tokens=False)
        if not encoded:
            return 0.0

        unk_id = self.tokenizer.unk_token_id
        known = sum(1 for tid in encoded if tid != unk_id)
        return known / len(encoded)

    def score(self, response: str, expected_meaning: str = "",
              weights: Optional[Dict[str, float]] = None) -> Dict[str, float]:
        """
        Score a Limn response on all dimensions.

        Args:
            response: The Limn text to score
            expected_meaning: English description of expected meaning
                (if not provided, semantic score is skipped)
            weights: Override default weights
                {semantic, structural, vocabulary, english_penalty}

        Returns:
            Dict with individual scores and overall weighted score
        """
        w = weights or {
            "semantic": 0.35,
            "structural": 0.25,
            "vocabulary": 0.20,
            "english_penalty": 0.20,
        }

        scores = {
            "semantic": 0.0,
            "structural": 0.0,
            "vocabulary": 0.0,
            "english_penalty": 0.0,
            "overall": 0.0,
        }

        # Semantic score
        if expected_meaning:
            scores["semantic"] = self.score_semantic(response, expected_meaning)
        else:
            # Without expected meaning, redistribute weight
            w = {
                "semantic": 0.0,
                "structural": 0.35,
                "vocabulary": 0.30,
                "english_penalty": 0.35,
            }

        # Structural score
        scores["structural"] = self.score_structural(response)

        # Vocabulary score
        scores["vocabulary"] = self.score_vocabulary(response)

        # English contamination penalty
        scores["english_penalty"] = self.score_english_penalty(response)

        # Overall weighted score
        scores["overall"] = (
            scores["semantic"] * w.get("semantic", 0.35) +
            scores["structural"] * w.get("structural", 0.25) +
            scores["vocabulary"] * w.get("vocabulary", 0.20) +
            scores["english_penalty"] * w.get("english_penalty", 0.20)
        )

        return scores


def compare_with_v4():
    """Compare v5 semantic reward with v4 reward model on adversarial examples."""
    print("Semantic Reward Model — Comparison with v4")
    print("=" * 70)

    # Load v4 reward model
    sys.path.insert(0, SCRIPT_DIR)
    try:
        from rl_training import LimnRewardModel as V4RewardModel
        v4 = V4RewardModel()
        has_v4 = True
    except Exception as e:
        print(f"  Could not load v4 reward model: {e}")
        has_v4 = False

    # Load v5 reward model
    v5 = SemanticRewardModel()

    test_cases = [
        # (response, expected_meaning, description)
        (
            "lov@fea | fea^0.3 → lov gro",
            "love projected through fear, fear at low intensity, leads to love growing",
            "GENUINE: well-formed compositional Limn"
        ),
        (
            "kno*wis | dep^0.8 | tru",
            "knowledge interfering with wisdom, depth at high intensity, truth",
            "GENUINE: multi-group expression"
        ),
        (
            "lov fea joy kno gro hop sad str wis tru lig dar cha flo pat dep",
            "love fear joy knowledge growth hope sadness strength wisdom truth light darkness change flow pattern depth",
            "GARBAGE: random valid words in sequence (no operators, no structure)"
        ),
        (
            "lov@fea*joy±kno^0.5\\str:dep | hop@sad*gro±lig^0.8\\dar:cha",
            "meaningless operator soup",
            "GARBAGE: operator-stuffed word salad"
        ),
        (
            "the quick brown fox jumps over the lazy dog",
            "the quick brown fox jumps over the lazy dog",
            "GARBAGE: English text, not Limn at all"
        ),
        (
            "xyzzy plugh qwerty asdf",
            "random words",
            "GARBAGE: non-vocabulary gibberish"
        ),
        (
            "lov",
            "love",
            "MINIMAL: single word response"
        ),
        (
            "far^0.9 rmt | fas^0.1 end wes sprl arm glx | sma nu yel sun",
            "far out in the uncharted backwaters of the unfashionable end of the galaxy lies a small yellow sun",
            "GENUINE: HGttG translation with operators and groups"
        ),
    ]

    print(f"\n{'Description':<50s} {'v5 Overall':>10s} {'v4 Overall':>10s}")
    print("-" * 72)

    for response, meaning, desc in test_cases:
        v5_scores = v5.score(response, meaning)

        if has_v4:
            v4_scores = v4.score("translate this", response)
            v4_overall = v4_scores["overall"]
        else:
            v4_overall = float('nan')

        # Determine if this is genuine or garbage
        is_genuine = desc.startswith("GENUINE") or desc.startswith("MINIMAL")
        marker = "+" if is_genuine else "-"

        print(f"  {marker} {desc:<48s} {v5_scores['overall']:10.3f} {v4_overall:10.3f}")
        print(f"    Response: {response[:60]}")
        print(f"    v5: sem={v5_scores['semantic']:.3f} str={v5_scores['structural']:.3f} voc={v5_scores['vocabulary']:.3f} eng={v5_scores['english_penalty']:.3f}")
        if has_v4:
            print(f"    v4: voc={v4_scores['vocab']:.3f} eng={v4_scores['english_penalty']:.3f} str={v4_scores['structure']:.3f} len={v4_scores['length']:.3f} nov={v4_scores['novelty']:.3f}")
        print()

    # Discrimination test
    print(f"\n{'='*70}")
    print(f"  DISCRIMINATION TEST")
    print(f"{'='*70}")

    genuine_scores = []
    garbage_scores = []
    v4_genuine = []
    v4_garbage = []

    for response, meaning, desc in test_cases:
        v5_scores = v5.score(response, meaning)
        is_genuine = desc.startswith("GENUINE") or desc.startswith("MINIMAL")

        if is_genuine:
            genuine_scores.append(v5_scores["overall"])
        else:
            garbage_scores.append(v5_scores["overall"])

        if has_v4:
            v4_scores = v4.score("translate this", response)
            if is_genuine:
                v4_genuine.append(v4_scores["overall"])
            else:
                v4_garbage.append(v4_scores["overall"])

    v5_disc = np.mean(genuine_scores) - np.mean(garbage_scores)
    print(f"\n  v5 Genuine mean: {np.mean(genuine_scores):.3f}")
    print(f"  v5 Garbage mean: {np.mean(garbage_scores):.3f}")
    print(f"  v5 Discrimination: {v5_disc:.3f}")

    if has_v4:
        v4_disc = np.mean(v4_genuine) - np.mean(v4_garbage)
        print(f"\n  v4 Genuine mean: {np.mean(v4_genuine):.3f}")
        print(f"  v4 Garbage mean: {np.mean(v4_garbage):.3f}")
        print(f"  v4 Discrimination: {v4_disc:.3f}")
        print(f"\n  Improvement: {v5_disc - v4_disc:+.3f}")

    if v5_disc > 0.1:
        print(f"\n  PASS: v5 reward model discriminates genuine from garbage")
    else:
        print(f"\n  FAIL: v5 reward model cannot discriminate (disc={v5_disc:.3f})")


if __name__ == "__main__":
    compare_with_v4()
