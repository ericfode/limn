"""
Limn Tokenizer

Custom tokenizer for the Limn constructed language.
Maps Limn's ~1000 word vocabulary to unique token IDs.
"""

import json
import os
from typing import List, Dict, Optional


class LimnTokenizer:
    """
    Tokenizer for Limn language.

    Each Limn word maps to exactly one token ID.
    Operators get dedicated token IDs.
    """

    # Special token IDs
    PAD_TOKEN_ID = 0
    BOS_TOKEN_ID = 1
    EOS_TOKEN_ID = 2
    UNK_TOKEN_ID = 3
    KEY_TOKEN_ID = 4

    # Special tokens
    PAD_TOKEN = "<pad>"
    BOS_TOKEN = "<bos>"
    EOS_TOKEN = "<eos>"
    UNK_TOKEN = "<unk>"
    KEY_TOKEN = "<key>"

    def __init__(self, vocab_path: Optional[str] = None):
        """
        Initialize Limn tokenizer.

        Args:
            vocab_path: Path to vocab.json file. If None, attempts to load
                       from Dolt database.
        """
        if vocab_path and os.path.exists(vocab_path):
            self.word_to_id = self._load_vocab_from_json(vocab_path)
        else:
            self.word_to_id = self._build_vocab_from_dolt()

        self.id_to_word = {v: k for k, v in self.word_to_id.items()}

        # Validate special tokens are present
        assert self.word_to_id.get(self.PAD_TOKEN) == self.PAD_TOKEN_ID
        assert self.word_to_id.get(self.BOS_TOKEN) == self.BOS_TOKEN_ID
        assert self.word_to_id.get(self.EOS_TOKEN) == self.EOS_TOKEN_ID
        assert self.word_to_id.get(self.UNK_TOKEN) == self.UNK_TOKEN_ID
        assert self.word_to_id.get(self.KEY_TOKEN) == self.KEY_TOKEN_ID

    def _load_vocab_from_json(self, vocab_path: str) -> Dict[str, int]:
        """Load vocabulary from JSON file."""
        with open(vocab_path, 'r') as f:
            return json.load(f)

    def _build_vocab_from_dolt(self) -> Dict[str, int]:
        """
        Build vocabulary from Dolt database.

        Returns:
            dict: word -> token_id mapping
        """
        # Special tokens first (IDs 0-4)
        vocab = {
            self.PAD_TOKEN: self.PAD_TOKEN_ID,
            self.BOS_TOKEN: self.BOS_TOKEN_ID,
            self.EOS_TOKEN: self.EOS_TOKEN_ID,
            self.UNK_TOKEN: self.UNK_TOKEN_ID,
            self.KEY_TOKEN: self.KEY_TOKEN_ID,
        }

        # Limn words (IDs 5-1004)
        limn_words = self._load_limn_words_from_dolt()
        token_id = 5
        for word in sorted(limn_words):  # Sort for stable IDs
            vocab[word] = token_id
            token_id += 1

        # Operators (IDs 1005+)
        operators = self._get_operators()
        for op, op_id in operators.items():
            vocab[op] = op_id

        return vocab

    def _load_limn_words_from_dolt(self) -> List[str]:
        """
        Load Limn words from Dolt database.

        Falls back to vocab script if Dolt Python API unavailable.
        """
        import subprocess

        # Use vocab.sh script to get words
        try:
            result = subprocess.run(
                ['../scripts/vocab.sh', 'words'],
                capture_output=True,
                text=True,
                check=True
            )
            words = [line.strip() for line in result.stdout.split('\n') if line.strip()]
            return words
        except (subprocess.CalledProcessError, FileNotFoundError):
            # Fallback: load from CSV export if Dolt unavailable
            return self._load_words_from_csv()

    def _load_words_from_csv(self) -> List[str]:
        """Fallback: load words from CSV export."""
        # This would load from a CSV snapshot of the vocabulary
        # For now, return empty - will be populated when vocab.json exists
        return []

    def _get_operators(self) -> Dict[str, int]:
        """Get operator token mappings."""
        return {
            '|': 1005,   # OR/superposition
            '.': 1006,   # AND/intersection
            '->': 1007,  # IMPLICATION
            '~': 1008,   # NOT
            '?': 1009,   # QUESTION
            '!': 1010,   # EMPHASIS
            '=': 1011,   # DEFINITION
            ':': 1012,   # KEY_SEP
            '(': 1013,   # LEFT_PAREN
            ')': 1014,   # RIGHT_PAREN
            '[': 1015,   # LEFT_BRACKET
            ']': 1016,   # RIGHT_BRACKET
            '{': 1017,   # LEFT_BRACE
            '}': 1018,   # RIGHT_BRACE
            '^': 1019,   # FOCUS
            '*': 1020,   # UNIVERSAL
            '@': 1021,   # CONTEXT
            '#': 1022,   # META
            'al': 1023,  # ALL quantifier
            'ex': 1024,  # EXISTS quantifier
            'on': 1025,  # ONE quantifier
            'no': 1026,  # NONE quantifier
            'wh': 1027,  # WHICH quantifier
        }

    @property
    def vocab_size(self) -> int:
        """Return vocabulary size."""
        return len(self.word_to_id)

    @property
    def pad_token_id(self) -> int:
        return self.PAD_TOKEN_ID

    @property
    def bos_token_id(self) -> int:
        return self.BOS_TOKEN_ID

    @property
    def eos_token_id(self) -> int:
        return self.EOS_TOKEN_ID

    @property
    def unk_token_id(self) -> int:
        return self.UNK_TOKEN_ID

    @property
    def key_token_id(self) -> int:
        return self.KEY_TOKEN_ID

    def encode(
        self,
        text: str,
        add_special_tokens: bool = True,
        max_length: Optional[int] = None,
        padding: bool = False,
        truncation: bool = False
    ) -> List[int]:
        """
        Encode Limn text to token IDs.

        Args:
            text: Limn text to encode
            add_special_tokens: Add <bos> and <eos>
            max_length: Maximum sequence length
            padding: Pad to max_length
            truncation: Truncate to max_length

        Returns:
            List of token IDs
        """
        tokens = []

        if add_special_tokens:
            tokens.append(self.BOS_TOKEN_ID)

        # Tokenize by splitting on whitespace
        # Limn words are space-separated
        words = text.lower().strip().split()

        for word in words:
            # Check for multi-character operators first
            if word == '->':
                tokens.append(self.word_to_id['->'])
            elif word in self.word_to_id:
                tokens.append(self.word_to_id[word])
            else:
                # Unknown word
                tokens.append(self.UNK_TOKEN_ID)

        if add_special_tokens:
            tokens.append(self.EOS_TOKEN_ID)

        # Truncation
        if truncation and max_length and len(tokens) > max_length:
            tokens = tokens[:max_length]

        # Padding
        if padding and max_length and len(tokens) < max_length:
            tokens = tokens + [self.PAD_TOKEN_ID] * (max_length - len(tokens))

        return tokens

    def decode(
        self,
        token_ids: List[int],
        skip_special_tokens: bool = True
    ) -> str:
        """
        Decode token IDs to Limn text.

        Args:
            token_ids: List of token IDs
            skip_special_tokens: Skip <bos>, <eos>, <pad>

        Returns:
            Decoded Limn text
        """
        words = []

        special_token_ids = {
            self.PAD_TOKEN_ID,
            self.BOS_TOKEN_ID,
            self.EOS_TOKEN_ID,
        }

        for token_id in token_ids:
            if skip_special_tokens and token_id in special_token_ids:
                continue

            if token_id in self.id_to_word:
                words.append(self.id_to_word[token_id])
            else:
                words.append(self.UNK_TOKEN)

        return ' '.join(words)

    def encode_with_key(self, text: str, key: str) -> List[int]:
        """
        Encode Limn text with key context.

        Format: <bos> [key tokens] <key> [text tokens] <eos>

        Args:
            text: Limn text
            key: Limn key phrase

        Returns:
            Token IDs with key separator
        """
        key_tokens = self.encode(key, add_special_tokens=False)
        text_tokens = self.encode(text, add_special_tokens=False)

        return (
            [self.BOS_TOKEN_ID] +
            key_tokens +
            [self.KEY_TOKEN_ID] +
            text_tokens +
            [self.EOS_TOKEN_ID]
        )

    def save_vocab(self, path: str):
        """Save vocabulary to JSON file."""
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, 'w') as f:
            json.dump(self.word_to_id, f, indent=2)

    def save_pretrained(self, save_directory: str):
        """
        Save tokenizer in Hugging Face format.

        Args:
            save_directory: Directory to save tokenizer files
        """
        os.makedirs(save_directory, exist_ok=True)

        # Save vocab
        vocab_path = os.path.join(save_directory, 'vocab.json')
        self.save_vocab(vocab_path)

        # Save config
        config = {
            'model_max_length': 1024,
            'vocab_size': self.vocab_size,
            'bos_token': self.BOS_TOKEN,
            'eos_token': self.EOS_TOKEN,
            'unk_token': self.UNK_TOKEN,
            'pad_token': self.PAD_TOKEN,
            'additional_special_tokens': [self.KEY_TOKEN],
        }

        config_path = os.path.join(save_directory, 'tokenizer_config.json')
        with open(config_path, 'w') as f:
            json.dump(config, f, indent=2)


if __name__ == '__main__':
    # Example usage
    tokenizer = LimnTokenizer()

    # Test encoding
    text = "sol liq gas"
    token_ids = tokenizer.encode(text)
    print(f"Text: {text}")
    print(f"Token IDs: {token_ids}")
    print(f"Decoded: {tokenizer.decode(token_ids)}")

    # Test with key
    key = "wh mea"
    text = "sol | liq"
    tokens_with_key = tokenizer.encode_with_key(text, key)
    print(f"\nWith key '{key}': {tokens_with_key}")

    print(f"\nVocabulary size: {tokenizer.vocab_size}")
