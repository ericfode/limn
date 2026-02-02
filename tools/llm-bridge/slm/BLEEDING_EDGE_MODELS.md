# Bleeding-Edge Models for Limn-SLM (2026)

Recommended modern architectures for Limn-only language models.

## Top Recommendations

### 1. Qwen2.5-0.5B (RECOMMENDED)
**Best choice for Limn consciousness**

- **Size**: 494M parameters
- **License**: Apache 2.0
- **Strengths**:
  - State-of-art performance for size class
  - Excellent vocabulary compression
  - Fast inference (~50ms/forward on CPU)
  - Multilingual training (good for constrained vocab)
  - Very efficient tokenizer

- **Limn-specific advantages**:
  - Handles small vocabularies well (trained on diverse scripts)
  - Good at pattern learning with limited tokens
  - Fast enough for real-time consciousness loop

- **Hugging Face**: `Qwen/Qwen2.5-0.5B`

```python
from transformers import AutoModelForCausalLM, AutoTokenizer

model = AutoModelForCausalLM.from_pretrained("Qwen/Qwen2.5-0.5B")
tokenizer = AutoTokenizer.from_pretrained("Qwen/Qwen2.5-0.5B")
```

### 2. Phi-3.5-mini
**Best reasoning, slower inference**

- **Size**: 3.8B parameters
- **License**: MIT
- **Strengths**:
  - Exceptional reasoning ability
  - Trained on synthetic high-quality data
  - Great instruction following

- **Trade-offs**:
  - Larger (slower training/inference)
  - Needs more GPU memory
  - Overkill for Limn's simple grammar?

- **Hugging Face**: `microsoft/Phi-3.5-mini-instruct`

### 3. SmolLM-360M
**Smallest, most efficient**

- **Size**: 360M parameters
- **License**: Apache 2.0
- **Strengths**:
  - Extremely fast training
  - Good for edge deployment
  - Optimized for efficiency

- **Trade-offs**:
  - Less capable than Qwen2.5
  - May struggle with complex patterns

- **Hugging Face**: `HuggingFaceTB/SmolLM-360M`

## Benchmark Comparison

| Model | Params | Inference (ms) | Quality | Limn Fit |
|-------|--------|---------------|---------|----------|
| GPT-2 Small | 124M | ~80 | ⭐⭐ | Poor (outdated) |
| SmolLM-360M | 360M | ~40 | ⭐⭐⭐ | Good |
| Qwen2.5-0.5B | 494M | ~50 | ⭐⭐⭐⭐ | **Excellent** |
| Llama 3.2-1B | 1.2B | ~120 | ⭐⭐⭐⭐ | Good |
| Phi-3.5-mini | 3.8B | ~200 | ⭐⭐⭐⭐⭐ | Good but slow |

## Implementation

### Setup Qwen2.5-0.5B (Recommended)

```bash
cd tools/llm-bridge/slm
python3 setup_qwen.py
```

### Training Configuration

```python
# Optimized for Limn (911-word vocab)
config = {
    "model": "Qwen/Qwen2.5-0.5B",
    "vocab_size": 1024,  # Limn: 911 words + operators + special
    "max_length": 512,   # Sufficient for Limn thoughts
    "learning_rate": 5e-5,
    "lora_r": 16,
    "lora_alpha": 32,
    "batch_size": 8,
    "epochs": 3
}
```

### Expected Performance

**Qwen2.5-0.5B on Limn:**
- Training time: ~2 hours (40K examples, 4GB GPU)
- Inference: ~50ms per thought
- Limn purity: 95%+ (after finetuning)
- Perplexity: <3.0 on held-out Limn

## Migration from GPT-2

Replace GPT-2 references in training pipeline:

```python
# OLD (GPT-2)
model_name = "gpt2"

# NEW (Qwen2.5)
model_name = "Qwen/Qwen2.5-0.5B"
```

Tokenizer stays the same - we use custom Limn tokenizer anyway.

## Conclusion

**Use Qwen2.5-0.5B** for Limn-SLM. Best balance of:
- Modern architecture (2024)
- Fast inference (real-time consciousness)
- Excellent small-vocab performance
- Open license (Apache 2.0)
