# Limn-SLM: Small Language Model for Limn

A custom small language model optimized for the Limn constructed language.

## Quick Start

```bash
# Install dependencies
pip install -r requirements.txt

# Test tokenizer
python test_tokenizer.py

# Initialize model
python init_model.py --output-dir ./limn-gpt2-small

# (Future) Train model
# python train.py --corpus ../data/limn-corpus.txt
```

## Overview

Limn-SLM is a GPT-2 Small-based language model customized for Limn's unique characteristics:

- **Tiny vocabulary**: ~1000 words + 23 operators vs. 50K+ for standard LLMs
- **Constraint-based semantics**: Words define hyperplanes in meaning-space
- **Key-collapsible ambiguity**: Phrases are ambiguous until a "key" collapses meaning
- **High information density**: Fewer tokens needed than natural language

## Architecture

### Model Selection: GPT-2 Small (124M parameters)

Selected over larger alternatives (TinyLlama, Phi-2, RWKV) because:

1. **Right-sized for vocabulary**: 124M params for ~1100 tokens = ~110K params/token
2. **Proven customization**: Extensive community knowledge for token vocab reduction
3. **Training efficiency**: Fast convergence on small datasets
4. **Deployment simplicity**: Runs on consumer hardware, well-supported
5. **De-risked**: Battle-tested architecture with known failure modes

See [MODEL-SELECTION.md](MODEL-SELECTION.md) for detailed analysis.

### Model Configuration

```
Vocabulary size:     ~1130 tokens (vs. 50257 for GPT-2)
Context length:      512 tokens (expandable to 1024)
Embedding dim:       768
Transformer layers:  12
Attention heads:     12
Total parameters:    ~45M (vs. 124M for standard GPT-2)
```

Parameter reduction comes from smaller embedding matrix:
- Standard GPT-2: 50257 × 768 = 38.6M embedding params
- Limn-GPT2: 1130 × 768 = 0.87M embedding params
- **97% reduction in embedding parameters**

## Tokenizer

### Design: Deterministic Word Mapping

Unlike BPE/WordPiece tokenizers that learn subword units, Limn tokenizer is **deterministic**:

- Each Limn word → Exactly one token ID
- Operators → Dedicated token IDs
- No subword splitting (Limn words are atomic)

### Token Allocation

```
0-4:        Special tokens (<pad>, <bos>, <eos>, <unk>, <key>)
5-1004:     Limn words (1000 words from Dolt vocabulary)
1005-1027:  Operators (23 operators: |, ., ->, ~, ?, !, etc.)
1028-1129:  Reserved (future expansion)
```

### Tokenizer Features

- **Vocabulary loading**: From Dolt database (source of truth)
- **Operator handling**: Multi-character operators (e.g., `->`)
- **Key mechanism**: Special `<key>` token for key-text separation
- **Case-insensitive**: Normalizes to lowercase
- **Hugging Face compatible**: Saves in transformers format

See [TOKENIZER-DESIGN.md](TOKENIZER-DESIGN.md) for complete specification.

## Files

```
limn-slm/
├── README.md                   # This file
├── MODEL-SELECTION.md          # Model selection analysis
├── TOKENIZER-DESIGN.md         # Tokenizer specification
├── requirements.txt            # Python dependencies
├── tokenizer.py                # Limn tokenizer implementation
├── init_model.py              # Model initialization script
├── test_tokenizer.py          # Tokenizer test suite
└── train.py                   # (Future) Training pipeline
```

## Usage

### 1. Initialize Model

```bash
# Random initialization (recommended for Limn-specific training)
python init_model.py --output-dir ./limn-gpt2-small

# Or adapt from pretrained GPT-2
python init_model.py --from-pretrained --output-dir ./limn-gpt2-pretrained
```

This creates:
- `limn-gpt2-small/` - Model weights and config
- `limn-gpt2-small/tokenizer/` - Tokenizer vocab and config

### 2. Test Tokenizer

```bash
python test_tokenizer.py
```

Validates:
- ✓ Special token IDs
- ✓ Encoding/decoding round-trip
- ✓ Operator handling
- ✓ Key mechanism
- ✓ Unknown word handling

### 3. Use Model (After Initialization)

```python
from transformers import GPT2LMHeadModel
from tokenizer import LimnTokenizer

# Load model and tokenizer
model = GPT2LMHeadModel.from_pretrained('./limn-gpt2-small')
tokenizer = LimnTokenizer('./limn-gpt2-small/tokenizer/vocab.json')

# Encode Limn text
text = "sol liq gas"
input_ids = tokenizer.encode(text)

# Generate (note: model is untrained, output will be random)
output = model.generate(
    input_ids,
    max_length=20,
    num_return_sequences=1
)

# Decode
generated_text = tokenizer.decode(output[0])
print(generated_text)
```

### 4. Training (Future)

Training pipeline not yet implemented. When ready:

```bash
# Collect Limn corpus
# - experiments/*.md (example sentences)
# - data/translations/* (Limn translations)
# - docs/spec/*.md (Limn grammar examples)

# Train model
python train.py \
  --model-dir ./limn-gpt2-small \
  --corpus ../data/limn-corpus.txt \
  --epochs 10 \
  --batch-size 8 \
  --learning-rate 5e-5
```

## Performance Expectations

### Inference Speed

Target performance on consumer hardware:

| Hardware | Tokens/sec | Use Case |
|----------|-----------|----------|
| CPU (Intel i7) | 20-50 | Development, testing |
| GPU (RTX 3080) | 200-500 | Training, production |
| Mobile (iPhone 14) | 5-15 | On-device inference |

Limn sequences are typically shorter than English, so effective throughput is higher.

### Memory Requirements

| Phase | CPU RAM | GPU VRAM |
|-------|---------|----------|
| Inference (FP32) | 500 MB | 500 MB |
| Inference (INT8) | 200 MB | 200 MB |
| Training | 2 GB | 4 GB |

## Evaluation Metrics

### 1. Baseline Perplexity

Measure model uncertainty on Limn corpus:
```
perplexity = exp(mean(cross_entropy_loss))
```

Target: < 50 (lower is better)

### 2. Generation Quality

Manual evaluation:
- Grammatical correctness (Limn grammar rules)
- Semantic coherence (constraint consistency)
- Operator usage (proper syntax)

### 3. Constraint Learning

Test model's ability to learn Limn's constraint semantics:
- Superposition: `sol | liq` = "solid OR liquid"
- Intersection: `sol . col` = "solid AND cold"
- Negation: `~ hot` = "NOT hot"

### 4. Key Mechanism

Evaluate key-based disambiguation:
```
Text: "sol | liq"
Key: "wh = wat" (which = water)
Expected: Model biases toward "liq" given water context
```

## Next Steps

- [x] Model selection analysis
- [x] Tokenizer design and implementation
- [x] Model initialization script
- [x] Tokenizer test suite
- [ ] Vocabulary export from Dolt to JSON
- [ ] Baseline model initialization test
- [ ] Training data collection pipeline
- [ ] Training script implementation
- [ ] Evaluation metrics implementation
- [ ] Baseline perplexity measurement
- [ ] Generation quality assessment
- [ ] Key mechanism evaluation

## Development

### Adding to Limn Vocabulary

When new words are added to Limn vocabulary:

1. Update Dolt database (source of truth)
2. Re-export vocabulary: `./scripts/vocab.sh export`
3. Rebuild tokenizer: `python init_model.py --rebuild-tokenizer`
4. Re-train model on updated corpus

### Debugging

```python
# Inspect tokenizer
tokenizer = LimnTokenizer()
print(f"Vocab size: {tokenizer.vocab_size}")
print(f"Sample tokens: {list(tokenizer.word_to_id.items())[:10]}")

# Test specific words
word = "sol"
token_id = tokenizer.word_to_id.get(word, tokenizer.unk_token_id)
print(f"'{word}' -> token {token_id}")

# Test round-trip
text = "sol liq gas"
tokens = tokenizer.encode(text, add_special_tokens=False)
decoded = tokenizer.decode(tokens)
print(f"Original: {text}")
print(f"Tokens: {tokens}")
print(f"Decoded: {decoded}")
```

## References

- [Limn Language Specification](../docs/spec/bootstrap-v3-natural.md)
- [Limn Vocabulary](../docs/spec/vocabulary-v3-natural.md)
- [GPT-2 Paper](https://d4mucfpksywv.cloudfront.net/better-language-models/language_models_are_unsupervised_multitask_learners.pdf)
- [Hugging Face Transformers](https://huggingface.co/docs/transformers/)

## License

Same as parent Limn project (TBD).

---

**Status**: Architecture complete, implementation ready for testing and training.

**Deliverables**:
- ✅ Model selection report with benchmarks
- ✅ Configured model architecture
- ✅ Custom Limn tokenizer
- ✅ Initialization script

**Success Criteria** (to be validated):
- ⏳ Model loads and runs inference
- ⏳ Tokenizer handles all Limn vocabulary
- ⏳ Baseline perplexity established
