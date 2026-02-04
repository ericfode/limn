# Limn-SLM Model Selection Report

## Executive Summary

**Recommended Model: GPT-2 Small (124M parameters)**

For Limn's unique constraints (1000-word vocabulary, 512-1024 token context), GPT-2 Small offers the optimal balance of trainability, inference speed, and well-understood architecture. The small vocabulary size makes larger models unnecessary, while GPT-2's proven tokenizer customization path and extensive community knowledge reduce implementation risk.

## Requirements Analysis

### Limn Vocabulary Constraints
- **Vocabulary size**: 1000 words + 23 operators + special tokens ≈ 1100 tokens
- **Context length**: 512-1024 tokens (sufficient for Limn expressions)
- **Use case**: Consciousness stream generation, constraint-based reasoning
- **Training data**: Limited Limn corpus (experiments, examples, translations)

### Evaluation Criteria
1. **Training efficiency**: Memory, time, convergence on small datasets
2. **Inference speed**: Thoughts/second for consciousness streaming
3. **Quality**: Perplexity and coherence on Limn generation
4. **Ease of deployment**: Model size, dependencies, compatibility
5. **Customization**: Tokenizer modification, architecture flexibility

## Candidate Analysis

### 1. TinyLlama (1.1B parameters)

**Pros:**
- Good performance for size
- Apache 2.0 license
- Recent architecture (Llama-based)
- Active community

**Cons:**
- **Overkill for 1000-word vocab**: 1.1B params for ~1100 tokens is massive waste
- Higher memory requirements (4-8GB for inference)
- Slower training on small datasets
- More prone to overfitting with limited Limn corpus
- Tokenizer designed for 32K vocab, not 1K

**Verdict**: Too large. The parameter count assumes you need to learn complex natural language semantics. Limn's constraint-based design doesn't require this capacity.

### 2. GPT-2 Small (124M parameters)

**Pros:**
- **Right-sized for task**: 124M params can easily model 1K vocab semantics
- **Well-understood**: Extensive documentation, tutorials, community knowledge
- **Proven finetuning**: Countless successful custom tokenizer examples
- **Fast inference**: ~50-100 tokens/sec on CPU, faster on GPU
- **Low memory**: 500MB model, runs on modest hardware
- **Easy deployment**: Hugging Face transformers, ONNX, quantization ready
- **Battle-tested**: Used for domain-specific languages before

**Cons:**
- Older architecture (2019) vs. newer models
- Not as "state-of-art" as Phi-2

**Verdict**: **RECOMMENDED**. Sweet spot of capability, trainability, and operational simplicity. The "boring" choice that will work.

### 3. Phi-2 (2.7B parameters)

**Pros:**
- Excellent reasoning for size
- State-of-art small model performance
- Microsoft backing

**Cons:**
- **Too large for vocab**: 2.7B params is excessive for 1K tokens
- Higher training cost (memory, time)
- More complex architecture (mixture-of-experts approach)
- Less community knowledge than GPT-2
- Overkill for constraint-based language

**Verdict**: Impressive model, wrong fit. You don't need "excellent reasoning" if your language semantics are deterministic constraints. The extra capacity buys you nothing.

### 4. RWKV (100M-1.5B parameters)

**Pros:**
- RNN architecture: O(1) inference (constant memory)
- Fast inference, especially for long sequences
- Interesting for streaming consciousness
- Lower memory during generation

**Cons:**
- **Experimental architecture**: Less proven for small-vocab languages
- Smaller community, fewer examples
- Tokenizer customization less documented
- Training dynamics less understood
- Higher implementation risk

**Verdict**: Intriguing for "consciousness streaming" use case, but higher risk. Consider for Phase 2 after validating GPT-2 baseline.

## Decision Matrix

| Model | Params | Training | Inference | Quality | Deployment | Risk |
|-------|--------|----------|-----------|---------|------------|------|
| TinyLlama | 1.1B | ⚠️ Slow | ⚠️ Moderate | ✓ Good | ✓ Easy | Low |
| GPT-2 Small | 124M | ✓✓ Fast | ✓✓ Fast | ✓ Good | ✓✓ Easy | **Lowest** |
| Phi-2 | 2.7B | ✗ Slow | ✗ Slow | ✓✓ Excellent | ⚠️ Moderate | Low |
| RWKV | 100M-1.5B | ⚠️ Unknown | ✓✓ Fast | ⚠️ Unknown | ⚠️ Harder | **Highest** |

## Recommendation: GPT-2 Small

### Rationale

**1. Right-sized for vocabulary:**
- Limn has ~1100 tokens (1000 words + 23 operators + specials)
- GPT-2 Small has 124M parameters
- This gives ~110K params per token - plenty for semantic modeling
- Larger models waste capacity (and money/time/energy)

**2. Proven customization path:**
```python
# Custom Limn tokenizer - well-documented pattern
from transformers import GPT2TokenizerFast, GPT2LMHeadModel

# Build vocab from Dolt database
limn_vocab = load_limn_vocabulary()  # 1000 words + operators
tokenizer = GPT2TokenizerFast(vocab=limn_vocab, ...)

# Initialize model with small vocab
model = GPT2LMHeadModel.from_pretrained('gpt2')
model.resize_token_embeddings(len(tokenizer))  # Shrink to ~1100
```

**3. Training efficiency:**
- Can finetune on single GPU (even consumer-grade)
- Fast convergence on small datasets
- Less prone to overfitting than larger models
- Well-understood hyperparameters

**4. Inference speed:**
- Target: 50-100 Limn tokens/second (more than sufficient for consciousness streaming)
- Achievable on CPU, faster on modest GPU
- Model fits entirely in L3 cache after quantization

**5. Deployment simplicity:**
- Hugging Face transformers (industry standard)
- ONNX export for production
- Quantization to INT8 (200MB model)
- No exotic dependencies

**6. De-risked path:**
- Thousands of successful GPT-2 customizations exist
- Extensive debugging resources
- Easy to swap to larger model later if needed
- Can always experiment with RWKV/Phi-2 after baseline

### The "Worse is Better" Argument

GPT-2 (2019) is ancient in ML terms. Phi-2 and TinyLlama are objectively better at natural language tasks. But:

- **Limn is not natural language** - it's a 1000-word constraint system
- **Complexity is risk** - newer architectures have unknown failure modes
- **Community matters** - when you're stuck at 2am, GPT-2 has answers

Choose the boring technology. Get it working. Optimize later if needed.

## Next Steps

1. **Custom Tokenizer** (See `TOKENIZER-DESIGN.md`)
   - Load Limn vocabulary from Dolt
   - Create BPE tokenizer with Limn atoms as base tokens
   - Add special tokens: `<bos>`, `<eos>`, `<pad>`, `<unk>`, `<key>`

2. **Model Configuration**
   - Start with GPT-2 Small architecture
   - Reduce vocab size from 50257 to ~1100
   - Adjust embedding dimensions if needed
   - Configure for 512 context length (can expand to 1024)

3. **Initialization Script** (See `init_model.py`)
   - Load pretrained GPT-2 embeddings
   - Resize token embeddings to Limn vocab
   - Reinitialize position embeddings if context < 1024
   - Save custom configuration

4. **Baseline Evaluation**
   - Perplexity on Limn corpus
   - Generation quality (coherence, grammar)
   - Inference speed benchmarks
   - Memory profiling

5. **Training Pipeline** (Future)
   - Collect/generate Limn training data
   - Finetune on Limn corpus
   - Evaluate improvements
   - Document training dynamics

## Risk Mitigation

**If GPT-2 Small proves insufficient:**

1. **First, check tokenizer** - Most "model" problems are tokenizer problems
2. **Then, check training data** - Garbage in, garbage out
3. **Then, try GPT-2 Medium** (355M params) - Same architecture, more capacity
4. **Last resort: Switch architecture** - Phi-2 or custom transformer

But start simple. GPT-2 Small will likely suffice.

## Conclusion

For Limn's unique constraints, GPT-2 Small is the optimal starting point:
- Right-sized for 1K vocabulary
- Proven customization path
- Fast training and inference
- Low operational complexity
- De-risked implementation

We can always scale up later. But we probably won't need to.

---

**Selected Model: GPT-2 Small (124M parameters)**
**Implementation Priority: Custom tokenizer → Model initialization → Baseline eval**
**Risk Level: Low**
**Time to First Result: ~1-2 days**
