"""
Limn-SLM Model Initialization

Initialize GPT-2 Small with custom Limn tokenizer and vocabulary.
"""

import os
import json
import argparse
from typing import Optional

try:
    from transformers import GPT2Config, GPT2LMHeadModel
    TRANSFORMERS_AVAILABLE = True
except ImportError:
    TRANSFORMERS_AVAILABLE = False
    print("Warning: transformers library not available")

from tokenizer import LimnTokenizer


def init_limn_model(
    output_dir: str = './limn-gpt2-small',
    from_pretrained: bool = False,
    model_name: str = 'gpt2',
    n_positions: int = 512,
    n_embd: int = 768,
    n_layer: int = 12,
    n_head: int = 12,
) -> Optional[object]:
    """
    Initialize GPT-2 model for Limn.

    Args:
        output_dir: Directory to save initialized model
        from_pretrained: Whether to start from pretrained GPT-2
        model_name: Pretrained model name (if from_pretrained=True)
        n_positions: Max context length
        n_embd: Embedding dimension
        n_layer: Number of transformer layers
        n_head: Number of attention heads

    Returns:
        Initialized model (if transformers available)
    """
    if not TRANSFORMERS_AVAILABLE:
        print("Error: transformers library required")
        print("Install with: pip install transformers torch")
        return None

    # Create tokenizer
    print("Creating Limn tokenizer...")
    tokenizer = LimnTokenizer()
    vocab_size = tokenizer.vocab_size

    print(f"Limn vocabulary size: {vocab_size}")

    # Save tokenizer
    tokenizer_dir = os.path.join(output_dir, 'tokenizer')
    print(f"Saving tokenizer to {tokenizer_dir}...")
    tokenizer.save_pretrained(tokenizer_dir)

    # Create model config
    print(f"\nCreating GPT-2 config for Limn...")
    config = GPT2Config(
        vocab_size=vocab_size,
        n_positions=n_positions,
        n_embd=n_embd,
        n_layer=n_layer,
        n_head=n_head,
        bos_token_id=tokenizer.bos_token_id,
        eos_token_id=tokenizer.eos_token_id,
        pad_token_id=tokenizer.pad_token_id,
    )

    print("Model config:")
    print(f"  Vocab size: {config.vocab_size}")
    print(f"  Context length: {config.n_positions}")
    print(f"  Embedding dim: {config.n_embd}")
    print(f"  Layers: {config.n_layer}")
    print(f"  Attention heads: {config.n_head}")

    # Calculate model size
    params_per_layer = (
        # Self-attention
        4 * n_embd * n_embd +  # Q, K, V, O projections
        # MLP
        4 * n_embd * n_embd +  # up and down projections (GPT-2 uses 4x)
        # Layer norms
        2 * n_embd * 2  # gamma, beta for 2 layer norms
    )

    total_params = (
        # Embeddings
        vocab_size * n_embd +  # Token embeddings
        n_positions * n_embd +  # Position embeddings
        # Transformer layers
        n_layer * params_per_layer +
        # Final layer norm
        n_embd * 2
    )

    print(f"\nEstimated parameters: {total_params:,} (~{total_params/1e6:.1f}M)")

    # Initialize model
    if from_pretrained:
        print(f"\nLoading pretrained {model_name} and adapting...")
        model = GPT2LMHeadModel.from_pretrained(model_name)

        # Resize embeddings to Limn vocab
        print(f"Resizing token embeddings from {model.config.vocab_size} to {vocab_size}...")
        model.resize_token_embeddings(vocab_size)

        # Update config
        model.config.bos_token_id = tokenizer.bos_token_id
        model.config.eos_token_id = tokenizer.eos_token_id
        model.config.pad_token_id = tokenizer.pad_token_id

        print("Warning: Embeddings for tokens beyond Limn vocab have been truncated")
        print("Consider random initialization instead of pretrained for Limn-specific training")

    else:
        print(f"\nInitializing fresh model from scratch...")
        model = GPT2LMHeadModel(config)

    # Save model
    print(f"\nSaving model to {output_dir}...")
    model.save_pretrained(output_dir)

    # Save config separately for reference
    config_path = os.path.join(output_dir, 'config.json')
    with open(config_path, 'w') as f:
        json.dump(config.to_dict(), f, indent=2)

    # Create README
    readme_path = os.path.join(output_dir, 'README.md')
    with open(readme_path, 'w') as f:
        f.write(f"""# Limn-GPT2-Small

GPT-2 Small model initialized for Limn constructed language.

## Configuration

- **Vocabulary size**: {vocab_size} (Limn words + operators + special tokens)
- **Context length**: {n_positions} tokens
- **Embedding dimension**: {n_embd}
- **Layers**: {n_layer}
- **Attention heads**: {n_head}
- **Total parameters**: ~{total_params/1e6:.1f}M

## Model Architecture

Based on GPT-2 Small, adapted for Limn's minimal vocabulary:

- Token embeddings: {vocab_size} × {n_embd} = {vocab_size * n_embd:,} params
- Position embeddings: {n_positions} × {n_embd} = {n_positions * n_embd:,} params
- Transformer layers: {n_layer} layers
- Total: ~{total_params/1e6:.1f}M parameters

Compare to standard GPT-2 (124M params): Limn model is ~{100 - (total_params/124e6)*100:.1f}% smaller due to tiny vocabulary.

## Usage

```python
from transformers import GPT2LMHeadModel
from tokenizer import LimnTokenizer

# Load model
model = GPT2LMHeadModel.from_pretrained('./{output_dir.split('/')[-1]}')

# Load tokenizer
tokenizer = LimnTokenizer('./{output_dir.split('/')[-1]}/tokenizer/vocab.json')

# Generate
text = "sol liq"
input_ids = tokenizer.encode(text)
output = model.generate(input_ids, max_length=50)
generated = tokenizer.decode(output[0])
print(generated)
```

## Training

This model is randomly initialized (or adapted from pretrained GPT-2).
To train on Limn corpus:

1. Collect Limn training data (experiments, examples, translations)
2. Tokenize with LimnTokenizer
3. Finetune with transformers Trainer
4. Evaluate perplexity and generation quality

See `train.py` for training script.

## Inference Speed

Expected performance on consumer hardware:
- CPU (Intel i7): ~20-50 tokens/sec
- GPU (RTX 3080): ~200-500 tokens/sec

Limn sequences are typically shorter than English due to higher information density.

## Next Steps

1. ✅ Model initialized
2. ⏳ Collect Limn training corpus
3. ⏳ Implement training pipeline
4. ⏳ Baseline perplexity evaluation
5. ⏳ Generation quality assessment

---

Created by Limn-SLM initialization script
Model: GPT-2 Small adapted for Limn
""")

    print(f"\n✅ Model initialization complete!")
    print(f"\nModel saved to: {output_dir}")
    print(f"Tokenizer saved to: {tokenizer_dir}")
    print(f"\nNext steps:")
    print(f"  1. Test model loading: python test_model.py")
    print(f"  2. Collect Limn training data")
    print(f"  3. Train on Limn corpus: python train.py")

    return model


def main():
    parser = argparse.ArgumentParser(description='Initialize Limn-GPT2 model')
    parser.add_argument('--output-dir', type=str, default='./limn-gpt2-small',
                       help='Output directory for model')
    parser.add_argument('--from-pretrained', action='store_true',
                       help='Start from pretrained GPT-2 (default: random init)')
    parser.add_argument('--model-name', type=str, default='gpt2',
                       help='Pretrained model name (if --from-pretrained)')
    parser.add_argument('--context-length', type=int, default=512,
                       help='Max context length (default: 512)')
    parser.add_argument('--embedding-dim', type=int, default=768,
                       help='Embedding dimension (default: 768)')
    parser.add_argument('--layers', type=int, default=12,
                       help='Number of layers (default: 12)')
    parser.add_argument('--heads', type=int, default=12,
                       help='Number of attention heads (default: 12)')

    args = parser.parse_args()

    init_limn_model(
        output_dir=args.output_dir,
        from_pretrained=args.from_pretrained,
        model_name=args.model_name,
        n_positions=args.context_length,
        n_embd=args.embedding_dim,
        n_layer=args.layers,
        n_head=args.heads,
    )


if __name__ == '__main__':
    main()
