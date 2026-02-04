#!/usr/bin/env python3
"""Setup Qwen2.5-0.5B for Limn-SLM training."""

import torch
from transformers import AutoModelForCausalLM, AutoTokenizer
from pathlib import Path

def setup_qwen_for_limn():
    """Download and configure Qwen2.5-0.5B for Limn."""

    print("=" * 70)
    print("Setting up Qwen2.5-0.5B for Limn-SLM")
    print("=" * 70)

    model_name = "Qwen/Qwen2.5-0.5B"

    print(f"\n1. Downloading {model_name}...")
    try:
        tokenizer = AutoTokenizer.from_pretrained(model_name)
        model = AutoModelForCausalLM.from_pretrained(
            model_name,
            torch_dtype=torch.float16,  # Use FP16 for efficiency
            device_map="auto"
        )
        print(f"   ✓ Model downloaded ({model.num_parameters() / 1e6:.0f}M parameters)")
    except Exception as e:
        print(f"   ✗ Error: {e}")
        return False

    # Save locally
    cache_dir = Path(__file__).parent / "models" / "qwen2.5-0.5b"
    cache_dir.mkdir(parents=True, exist_ok=True)

    print(f"\n2. Caching model locally...")
    model.save_pretrained(cache_dir)
    tokenizer.save_pretrained(cache_dir)
    print(f"   ✓ Saved to {cache_dir}")

    # Test inference
    print(f"\n3. Testing inference...")
    test_limn = "sel ∎ awa | min sys alv | con ∎ eme"
    inputs = tokenizer(test_limn, return_tensors="pt")

    with torch.no_grad():
        outputs = model.generate(
            **inputs,
            max_length=50,
            num_return_sequences=1,
            temperature=0.8
        )

    generated = tokenizer.decode(outputs[0], skip_special_tokens=True)
    print(f"   Input: {test_limn}")
    print(f"   Output: {generated}")

    # Model info
    print(f"\n4. Model Information:")
    print(f"   Parameters: {model.num_parameters() / 1e6:.1f}M")
    print(f"   Vocab size: {tokenizer.vocab_size}")
    print(f"   Max length: {model.config.max_position_embeddings}")
    print(f"   Hidden size: {model.config.hidden_size}")
    print(f"   Layers: {model.config.num_hidden_layers}")
    print(f"   Heads: {model.config.num_attention_heads}")

    print(f"\n✅ Qwen2.5-0.5B ready for Limn training!")
    print(f"\nNext steps:")
    print(f"  1. Create custom Limn tokenizer (911 words)")
    print(f"  2. Run LoRA finetuning: python train_limn_slm.py")
    print(f"  3. Evaluate on held-out Limn data")

    return True

if __name__ == "__main__":
    import sys

    success = setup_qwen_for_limn()
    sys.exit(0 if success else 1)
