#!/usr/bin/env python3
"""
Grammar-Constrained Generation for Limn (v5 Phase 2.2).

Implements a finite state machine (FSM) that tracks valid next tokens
at each generation step, based on Limn's formal grammar. This eliminates
invalid outputs that free-form generation produces (H4).

The FSM encodes these rules:
1. A phrase is a sequence of constraint groups separated by |
2. Each group is a sequence of tokens (words, expressions)
3. Expressions use operators: A@B, A*B, A^0.7, A\\B, A±B, A:B
4. Gradient (^) must be followed by a float value
5. Binary operators must be followed by a word
6. Parentheses must be balanced

Usage:
    from grammar_constrained_generation import LimnGrammarFSM

    fsm = LimnGrammarFSM(tokenizer)
    valid_ids = fsm.get_valid_token_ids()  # Initial valid tokens
    fsm.advance(token_id)                   # After generating a token
    valid_ids = fsm.get_valid_token_ids()  # Next valid tokens

For logit masking during generation:
    logits = model(input_ids)
    mask = fsm.get_logit_mask(vocab_size)   # Boolean mask
    logits[~mask] = float('-inf')           # Mask invalid tokens

— Lex
"""

import json
import os
import sys
from enum import Enum, auto
from typing import Set, List, Optional

import torch
import numpy as np

# Token categories
OPERATORS = {"@", "*", "^", "\\", "±", ":"}
BINARY_OPERATORS = {"@", "*", "\\", "±", ":"}  # Require left and right operands
UNARY_POSTFIX = {"^"}  # Requires float after
STRUCTURAL = {"|", "→", "="}
PARENS = {"(", ")"}


class State(Enum):
    """FSM states for Limn grammar."""
    START = auto()           # Beginning of generation
    AFTER_WORD = auto()      # Just produced a word
    AFTER_BINARY_OP = auto() # Just produced @, *, \\, ±, or :
    AFTER_CARET = auto()     # Just produced ^, need float
    AFTER_FLOAT = auto()     # Just produced a gradient float
    AFTER_PIPE = auto()      # Just produced |, start new group
    AFTER_ARROW = auto()     # Just produced →
    AFTER_LPAREN = auto()    # Just produced (
    AFTER_RPAREN = auto()    # Just produced )
    DONE = auto()            # Generated EOS


class LimnGrammarFSM:
    """
    Finite State Machine for grammar-constrained Limn generation.

    Tracks the current state and provides valid next token IDs
    at each generation step.
    """

    def __init__(self, tokenizer):
        self.tokenizer = tokenizer
        self.state = State.START
        self.paren_depth = 0
        self.token_count = 0
        self.max_tokens = 50  # Safety limit

        # Precompute token ID sets for each category
        self._build_token_sets()

    def _build_token_sets(self):
        """Precompute token ID sets for fast lookup."""
        vocab = self.tokenizer.get_vocab()

        self.word_ids = set()
        self.binary_op_ids = set()
        self.caret_id = None
        self.pipe_id = None
        self.arrow_id = None
        self.float_ids = set()
        self.lparen_id = None
        self.rparen_id = None
        self.eos_id = self.tokenizer.eos_token_id
        self.bos_id = self.tokenizer.bos_token_id
        self.pad_id = self.tokenizer.pad_token_id
        self.unk_id = self.tokenizer.unk_token_id
        self.uppercase_ids = set()
        self.digit_ids = set()

        for token, tid in vocab.items():
            # Skip special tokens
            if token in ("<pad>", "<unk>", "<bos>", "<eos>", "<sep>", "<space>"):
                continue

            # Operators
            if token in BINARY_OPERATORS:
                self.binary_op_ids.add(tid)
            elif token == "^":
                self.caret_id = tid
            elif token == "|":
                self.pipe_id = tid
            elif token == "→":
                self.arrow_id = tid
            elif token == "(":
                self.lparen_id = tid
            elif token == ")":
                self.rparen_id = tid

            # Float values (gradient intensities)
            elif self._is_float_token(token):
                self.float_ids.add(tid)

            # Single digits
            elif len(token) == 1 and token.isdigit():
                self.digit_ids.add(tid)

            # Uppercase letters (proper nouns)
            elif len(token) == 1 and token.isupper():
                self.uppercase_ids.add(tid)

            # Everything else is a vocabulary word
            elif token.isalpha() and token.islower():
                self.word_ids.add(tid)
            elif token in ("+", "-", "!", ".", "=", '"', "\n"):
                # Structural tokens treated as words for flexibility
                pass
            else:
                # Remaining tokens (numbers, mixed) — check if word-like
                if all(c.isalpha() and c.islower() for c in token) and len(token) >= 2:
                    self.word_ids.add(tid)

        # Add uppercase as word-like (for proper nouns)
        self.word_like_ids = self.word_ids | self.uppercase_ids | {self.unk_id}

        print(f"  FSM token sets:")
        print(f"    Words: {len(self.word_ids)}")
        print(f"    Binary operators: {len(self.binary_op_ids)}")
        print(f"    Caret (^): {self.caret_id}")
        print(f"    Pipe (|): {self.pipe_id}")
        print(f"    Arrow (→): {self.arrow_id}")
        print(f"    Float values: {len(self.float_ids)}")
        print(f"    Uppercase: {len(self.uppercase_ids)}")

    def _is_float_token(self, token):
        """Check if token is a valid gradient float value."""
        try:
            v = float(token)
            return 0.0 <= v <= 1.0
        except ValueError:
            return False

    def reset(self):
        """Reset FSM to initial state."""
        self.state = State.START
        self.paren_depth = 0
        self.token_count = 0

    def get_valid_token_ids(self) -> Set[int]:
        """Return the set of valid next token IDs given current state."""
        if self.state == State.DONE:
            return {self.pad_id}

        if self.token_count >= self.max_tokens:
            # Force end
            valid = {self.eos_id}
            if self.paren_depth > 0 and self.rparen_id:
                valid.add(self.rparen_id)
            return valid

        valid = set()

        if self.state == State.START:
            # Can start with a word, (, or uppercase (proper noun)
            valid = self.word_like_ids.copy()
            if self.lparen_id:
                valid.add(self.lparen_id)

        elif self.state == State.AFTER_WORD:
            # After a word: can have operator, ^, another word (space-separated),
            # |, →, ), or EOS
            valid = self.word_like_ids.copy()  # Next word in sequence
            valid |= self.binary_op_ids       # Binary operator
            if self.caret_id:
                valid.add(self.caret_id)       # Gradient
            if self.pipe_id:
                valid.add(self.pipe_id)        # Constraint group separator
            if self.arrow_id:
                valid.add(self.arrow_id)       # Arrow
            valid.add(self.eos_id)             # End of sequence
            if self.paren_depth > 0 and self.rparen_id:
                valid.add(self.rparen_id)      # Close paren

        elif self.state == State.AFTER_BINARY_OP:
            # After @, *, \, ±, :: must have a word or (
            valid = self.word_like_ids.copy()
            if self.lparen_id:
                valid.add(self.lparen_id)

        elif self.state == State.AFTER_CARET:
            # After ^: must have a float value or digit
            valid = self.float_ids.copy()
            valid |= self.digit_ids

        elif self.state == State.AFTER_FLOAT:
            # After gradient value: can have operator, another word,
            # |, →, ), or EOS (same as AFTER_WORD)
            valid = self.word_like_ids.copy()
            valid |= self.binary_op_ids
            if self.caret_id:
                valid.add(self.caret_id)
            if self.pipe_id:
                valid.add(self.pipe_id)
            if self.arrow_id:
                valid.add(self.arrow_id)
            valid.add(self.eos_id)
            if self.paren_depth > 0 and self.rparen_id:
                valid.add(self.rparen_id)

        elif self.state == State.AFTER_PIPE:
            # After |: start new constraint group with word or (
            valid = self.word_like_ids.copy()
            if self.lparen_id:
                valid.add(self.lparen_id)

        elif self.state == State.AFTER_ARROW:
            # After →: start result with word or (
            valid = self.word_like_ids.copy()
            if self.lparen_id:
                valid.add(self.lparen_id)

        elif self.state == State.AFTER_LPAREN:
            # After (: start sub-expression with word or (
            valid = self.word_like_ids.copy()
            if self.lparen_id:
                valid.add(self.lparen_id)

        elif self.state == State.AFTER_RPAREN:
            # After ): can have operator, ^, |, →, ), or EOS
            valid = self.binary_op_ids.copy()
            if self.caret_id:
                valid.add(self.caret_id)
            if self.pipe_id:
                valid.add(self.pipe_id)
            if self.arrow_id:
                valid.add(self.arrow_id)
            valid.add(self.eos_id)
            if self.paren_depth > 0 and self.rparen_id:
                valid.add(self.rparen_id)

        return valid

    def advance(self, token_id: int):
        """Advance FSM state after generating a token."""
        self.token_count += 1

        if token_id == self.eos_id:
            self.state = State.DONE
        elif token_id == self.caret_id:
            self.state = State.AFTER_CARET
        elif token_id in self.binary_op_ids:
            self.state = State.AFTER_BINARY_OP
        elif token_id == self.pipe_id:
            self.state = State.AFTER_PIPE
        elif token_id == self.arrow_id:
            self.state = State.AFTER_ARROW
        elif token_id == self.lparen_id:
            self.paren_depth += 1
            self.state = State.AFTER_LPAREN
        elif token_id == self.rparen_id:
            self.paren_depth = max(0, self.paren_depth - 1)
            self.state = State.AFTER_RPAREN
        elif token_id in self.float_ids or token_id in self.digit_ids:
            self.state = State.AFTER_FLOAT
        elif token_id in self.word_like_ids:
            self.state = State.AFTER_WORD
        else:
            # Unknown token — treat as word
            self.state = State.AFTER_WORD

    def get_logit_mask(self, vocab_size: int) -> torch.Tensor:
        """Get a boolean mask for valid tokens (True = valid)."""
        mask = torch.zeros(vocab_size, dtype=torch.bool)
        valid_ids = self.get_valid_token_ids()
        for tid in valid_ids:
            if 0 <= tid < vocab_size:
                mask[tid] = True
        return mask

    def apply_mask(self, logits: torch.Tensor) -> torch.Tensor:
        """Apply grammar mask to logits (set invalid tokens to -inf)."""
        mask = self.get_logit_mask(logits.shape[-1])
        logits[~mask] = float('-inf')
        return logits


def demo():
    """Demonstrate the grammar FSM."""
    from transformers import PreTrainedTokenizerFast

    tokenizer_path = os.path.join(
        os.path.dirname(os.path.abspath(__file__)),
        "limn_tokenizer"
    )
    tokenizer = PreTrainedTokenizerFast.from_pretrained(tokenizer_path)

    print("Grammar-Constrained Generation Demo")
    print("=" * 60)

    fsm = LimnGrammarFSM(tokenizer)

    # Simulate generating "lov@fea | kno^0.7"
    test_tokens = ["lov", "@", "fea", "|", "kno", "^", "0.7"]
    test_ids = [tokenizer.convert_tokens_to_ids(t) for t in test_tokens]

    print(f"\nSimulating generation of: {' '.join(test_tokens)}")
    print(f"{'Step':<6s} {'Token':<10s} {'State':<20s} {'Valid next (sample)':<40s}")
    print("-" * 76)

    fsm.reset()
    for i, (tok, tid) in enumerate(zip(test_tokens, test_ids)):
        valid = fsm.get_valid_token_ids()
        is_valid = tid in valid
        status = "OK" if is_valid else "BLOCKED"

        # Sample some valid next tokens for display
        valid_sample = []
        for vid in list(valid)[:8]:
            vt = tokenizer.convert_ids_to_tokens(vid)
            valid_sample.append(vt)

        fsm.advance(tid)

        print(f"{i:<6d} {tok:<10s} {fsm.state.name:<20s} {', '.join(valid_sample[:5]):<40s} [{status}]")

    # Final state — show valid next tokens
    valid = fsm.get_valid_token_ids()
    valid_sample = [tokenizer.convert_ids_to_tokens(vid) for vid in list(valid)[:8]]
    print(f"{'—':<6s} {'(next)':<10s} {'':<20s} {', '.join(valid_sample[:5]):<40s}")

    # Test invalid sequences
    print(f"\n{'='*60}")
    print(f"  INVALID SEQUENCE TESTS")
    print(f"{'='*60}")

    invalid_tests = [
        (["@", "lov"], "operator before word"),
        (["lov", "^", "lov"], "word after caret (should be float)"),
        (["lov", "@", "@"], "double operator"),
        (["lov", "0.7"], "float without caret"),
    ]

    for tokens, desc in invalid_tests:
        fsm.reset()
        blocked_at = None
        for i, tok in enumerate(tokens):
            tid = tokenizer.convert_tokens_to_ids(tok)
            valid = fsm.get_valid_token_ids()
            if tid not in valid:
                blocked_at = i
                break
            fsm.advance(tid)

        if blocked_at is not None:
            print(f"  BLOCKED: {' '.join(tokens)} — '{tokens[blocked_at]}' blocked at step {blocked_at} ({desc})")
        else:
            print(f"  ALLOWED: {' '.join(tokens)} ({desc})")

    # Test valid sequences
    print(f"\n  VALID SEQUENCE TESTS")
    valid_tests = [
        ["lov", "@", "fea"],
        ["joy", "*", "sad", "|", "hop"],
        ["kno", "^", "0.7"],
        ["lov", "±", "fea"],
        ["str", "\\", "fea", ":", "hop"],
        ["far", "^", "0.9", "rmt"],
    ]

    for tokens in valid_tests:
        fsm.reset()
        blocked_at = None
        for i, tok in enumerate(tokens):
            tid = tokenizer.convert_tokens_to_ids(tok)
            valid = fsm.get_valid_token_ids()
            if tid not in valid:
                blocked_at = i
                break
            fsm.advance(tid)

        if blocked_at is not None:
            print(f"  BLOCKED: {' '.join(tokens)} — '{tokens[blocked_at]}' blocked at step {blocked_at}")
        else:
            print(f"  OK:      {' '.join(tokens)}")

    # Logit mask demo
    print(f"\n{'='*60}")
    print(f"  LOGIT MASK DEMO")
    print(f"{'='*60}")

    fsm.reset()
    mask = fsm.get_logit_mask(tokenizer.vocab_size)
    print(f"\n  At START state:")
    print(f"    Vocab size: {tokenizer.vocab_size}")
    print(f"    Valid tokens: {mask.sum().item()}")
    print(f"    Blocked tokens: {(~mask).sum().item()}")
    print(f"    Valid %: {mask.sum().item() / tokenizer.vocab_size:.1%}")

    fsm.advance(tokenizer.convert_tokens_to_ids("lov"))
    mask = fsm.get_logit_mask(tokenizer.vocab_size)
    print(f"\n  After 'lov' (AFTER_WORD state):")
    print(f"    Valid tokens: {mask.sum().item()}")
    print(f"    Valid %: {mask.sum().item() / tokenizer.vocab_size:.1%}")

    fsm.advance(tokenizer.convert_tokens_to_ids("@"))
    mask = fsm.get_logit_mask(tokenizer.vocab_size)
    print(f"\n  After 'lov @' (AFTER_BINARY_OP state):")
    print(f"    Valid tokens: {mask.sum().item()}")
    print(f"    Valid %: {mask.sum().item() / tokenizer.vocab_size:.1%}")

    fsm.advance(tokenizer.convert_tokens_to_ids("fea"))
    fsm.advance(tokenizer.convert_tokens_to_ids("^"))
    mask = fsm.get_logit_mask(tokenizer.vocab_size)
    print(f"\n  After 'lov @ fea ^' (AFTER_CARET state):")
    print(f"    Valid tokens: {mask.sum().item()}")
    print(f"    Valid %: {mask.sum().item() / tokenizer.vocab_size:.1%}")


if __name__ == "__main__":
    demo()
