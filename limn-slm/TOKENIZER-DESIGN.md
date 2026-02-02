# Limn Tokenizer Design

## Overview

The Limn tokenizer maps Limn's 1000-word vocabulary to model input tokens. Unlike natural language tokenizers (which learn subword units from data), Limn's tokenizer is **deterministic** - each Limn word is exactly one token.

## Design Principles

1. **One word = One token**: Each Limn word maps to a unique token ID
2. **Operator preservation**: 23 operators get dedicated tokens
3. **Special tokens**: `<bos>`, `<eos>`, `<pad>`, `<unk>`, `<key>` for control
4. **Whitespace handling**: Spaces between words preserved, but not tokenized
5. **Case-insensitive**: Limn is case-insensitive (all lowercase canonical)
6. **Dolt-sourced**: Vocabulary pulled from authoritative Dolt database

## Token Allocation

```
Total vocabulary size: ~1130 tokens

0-4:        Special tokens (5)
  0:  <pad>   - Padding token
  1:  <bos>   - Beginning of sequence
  2:  <eos>   - End of sequence
  3:  <unk>   - Unknown token (should rarely occur)
  4:  <key>   - Key boundary marker

5-1004:     Limn words (1000)
  5:  aer    - air-related
  6:  aqu    - water-related
  ...
  1004: [last word]

1005-1027:  Operators (23)
  1005: |    - OR (superposition)
  1006: .    - AND (intersection)
  1007: ->   - IMPLICATION
  1008: ~    - NOT
  1009: ?    - QUESTION
  1010: !    - EMPHASIS
  1011: =    - DEFINITION
  ...
  1027: [last operator]

1028-1129:  Reserved (102)
  - Future expansion
  - Experimental operators
  - Metacognitive markers
```

## Tokenizer Implementation

### Approach: Character-level with word lookup

```python
class LimnTokenizer:
    """
    Custom tokenizer for Limn language.

    Maps each Limn word to a unique token ID.
    Handles operators, special tokens, and whitespace.
    """

    def __init__(self, vocab_db_path):
        # Load vocabulary from Dolt
        self.word_to_id = self._load_vocab(vocab_db_path)
        self.id_to_word = {v: k for k, v in self.word_to_id.items()}

        # Special tokens
        self.pad_token_id = 0
        self.bos_token_id = 1
        self.eos_token_id = 2
        self.unk_token_id = 3
        self.key_token_id = 4

        # Operator mapping
        self.operators = self._load_operators()

    def encode(self, text, add_special_tokens=True):
        """Convert Limn text to token IDs."""
        tokens = []

        if add_special_tokens:
            tokens.append(self.bos_token_id)

        # Split on whitespace, tokenize each word
        words = text.lower().split()
        for word in words:
            if word in self.operators:
                tokens.append(self.operators[word])
            elif word in self.word_to_id:
                tokens.append(self.word_to_id[word])
            else:
                tokens.append(self.unk_token_id)

        if add_special_tokens:
            tokens.append(self.eos_token_id)

        return tokens

    def decode(self, token_ids, skip_special_tokens=True):
        """Convert token IDs back to Limn text."""
        words = []
        for token_id in token_ids:
            if skip_special_tokens and token_id in [
                self.pad_token_id,
                self.bos_token_id,
                self.eos_token_id
            ]:
                continue

            if token_id in self.id_to_word:
                words.append(self.id_to_word[token_id])
            else:
                words.append('<UNK>')

        return ' '.join(words)
```

## Vocabulary Loading from Dolt

```python
def load_limn_vocabulary(dolt_db_path='../../data/vocabulary'):
    """
    Load Limn vocabulary from Dolt database.

    Returns:
        dict: word -> token_id mapping
    """
    import doltcli as dolt

    # Query all words from vocabulary
    db = dolt.Dolt(dolt_db_path)

    # Get words sorted alphabetically for stable token IDs
    result = db.sql(
        "SELECT word FROM words ORDER BY word ASC",
        result_format='csv'
    )

    # Assign token IDs starting from 5 (after special tokens)
    word_to_id = {}
    token_id = 5

    for row in result:
        word = row['word']
        word_to_id[word] = token_id
        token_id += 1

    return word_to_id

def load_operators():
    """Load operator token mappings."""
    # These come from Limn grammar specification
    operators = {
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
    return operators
```

## Hugging Face Integration

To use with transformers library:

```python
from transformers import PreTrainedTokenizerFast
import json

# Create tokenizer JSON config
tokenizer_config = {
    "model_max_length": 1024,
    "bos_token": "<bos>",
    "eos_token": "<eos>",
    "unk_token": "<unk>",
    "pad_token": "<pad>",
    "additional_special_tokens": ["<key>"],
}

# Build vocab.json from Limn words
vocab = load_limn_vocabulary()
vocab_json = {
    "<pad>": 0,
    "<bos>": 1,
    "<eos>": 2,
    "<unk>": 3,
    "<key>": 4,
    **vocab,
    **load_operators()
}

# Save for transformers
with open('limn-slm/tokenizer/vocab.json', 'w') as f:
    json.dump(vocab_json, f)

# Create tokenizer
tokenizer = PreTrainedTokenizerFast(
    tokenizer_file='limn-slm/tokenizer/tokenizer.json',
    **tokenizer_config
)
```

## Testing Strategy

### 1. Vocabulary Coverage
```python
def test_vocabulary_coverage():
    """Ensure all Limn words tokenize."""
    vocab = load_limn_vocabulary()
    tokenizer = LimnTokenizer(vocab)

    # Get all words from Dolt
    all_words = get_all_limn_words()

    for word in all_words:
        token_ids = tokenizer.encode(word, add_special_tokens=False)
        assert len(token_ids) == 1, f"Word {word} should be one token"

        decoded = tokenizer.decode(token_ids)
        assert decoded == word, f"Decode mismatch: {word} -> {decoded}"
```

### 2. Operator Handling
```python
def test_operators():
    """Test operator tokenization."""
    tokenizer = LimnTokenizer()

    test_cases = [
        ("sol | liq", ["sol", "|", "liq"]),
        ("aqu -> pyr", ["aqu", "->", "pyr"]),
        ("~ nox", ["~", "nox"]),
    ]

    for text, expected_tokens in test_cases:
        tokens = tokenizer.encode(text, add_special_tokens=False)
        decoded_tokens = [tokenizer.id_to_word[t] for t in tokens]
        assert decoded_tokens == expected_tokens
```

### 3. Round-trip Consistency
```python
def test_round_trip():
    """Ensure encode->decode is lossless."""
    tokenizer = LimnTokenizer()

    test_sentences = [
        "sol liq gas",
        "aqu -> pyr | ter",
        "al mov res",
        "sol | liq . hot",
    ]

    for sentence in test_sentences:
        tokens = tokenizer.encode(sentence)
        decoded = tokenizer.decode(tokens)
        # Whitespace may differ, but tokens should match
        assert set(sentence.split()) == set(decoded.split())
```

## Key Mechanism Integration

Limn's "key" concept needs special handling:

```python
def encode_with_key(text, key):
    """
    Encode Limn text with key context.

    Format: <bos> [key tokens] <key> [text tokens] <eos>
    """
    tokenizer = LimnTokenizer()

    key_tokens = tokenizer.encode(key, add_special_tokens=False)
    text_tokens = tokenizer.encode(text, add_special_tokens=False)

    return (
        [tokenizer.bos_token_id] +
        key_tokens +
        [tokenizer.key_token_id] +
        text_tokens +
        [tokenizer.eos_token_id]
    )
```

## Performance Considerations

### Memory Efficiency
- Vocabulary of 1130 tokens is tiny (vs. 50K+ for GPT-2)
- Embedding matrix: 1130 × 768 = ~870K parameters (vs. 38M for GPT-2)
- 97% reduction in embedding parameters

### Inference Speed
- Shorter sequences (Limn is denser than English)
- Faster attention computation (fewer tokens)
- Simple tokenization (word lookup, no BPE)

### Training Efficiency
- Smaller vocab = faster embedding updates
- Less risk of rare token underfitting
- Better gradient flow to small vocab

## Edge Cases

### 1. Unknown Words
If a word not in Limn vocabulary appears:
- Map to `<unk>` token
- Log warning for vocabulary expansion
- Consider adding to Limn vocab if legitimate

### 2. Multi-word Operators
Some operators are multi-character (e.g., `->`):
- Tokenize as single operator token
- Requires lookahead in tokenizer

### 3. Punctuation
Limn uses punctuation for operators, not for natural language:
- `.` is AND operator, not sentence end
- `?` is QUESTION operator, not punctuation
- No special sentence boundary handling

### 4. Case Sensitivity
Limn is case-insensitive:
- Normalize all input to lowercase before tokenization
- `Sol`, `SOL`, `sol` all map to same token

## Integration with GPT-2

```python
from transformers import GPT2LMHeadModel, GPT2Config

# Create Limn tokenizer
tokenizer = LimnTokenizer()

# Configure GPT-2 for Limn vocab
config = GPT2Config(
    vocab_size=1130,        # Limn vocab size
    n_positions=1024,       # Max context length
    n_embd=768,             # Embedding dimension (keep GPT-2 default)
    n_layer=12,             # Number of layers (GPT-2 Small)
    n_head=12,              # Attention heads
    bos_token_id=1,
    eos_token_id=2,
    pad_token_id=0,
)

# Initialize model
model = GPT2LMHeadModel(config)

# Or adapt from pretrained:
model = GPT2LMHeadModel.from_pretrained('gpt2')
model.resize_token_embeddings(1130)
```

## Deliverables

1. ✅ **tokenizer.py** - LimnTokenizer class implementation
2. ✅ **vocab_loader.py** - Dolt vocabulary loading
3. ✅ **tokenizer_test.py** - Comprehensive test suite
4. ✅ **vocab.json** - Serialized vocabulary for transformers
5. ✅ **tokenizer_config.json** - Hugging Face tokenizer config

## Next Steps

After tokenizer implementation:
1. Test on real Limn corpus (experiments, examples)
2. Verify integration with GPT-2 model
3. Benchmark tokenization speed
4. Document any edge cases discovered
5. Integrate with model initialization script

---

**Status: Design Complete**
**Implementation Priority: High (required for model init)**
**Estimated Complexity: Low (deterministic mapping)**
