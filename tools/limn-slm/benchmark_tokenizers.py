#!/usr/bin/env python3
"""
Benchmark Limn tokenizer vs BPE on v5 training data.

Compares:
  - Sequence lengths (tokens per example)
  - Vocabulary utilization
  - Information density (content tokens vs overhead)
  - Estimated training throughput impact

Usage:
    python benchmark_tokenizers.py [--data data/v5/train.jsonl]
"""

import argparse
import json
import sys
from collections import Counter
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
DEFAULT_DATA = SCRIPT_DIR / "data" / "v5" / "train.jsonl"
DEFAULT_TOKENIZER = SCRIPT_DIR / "limn_tokenizer"


def load_examples(path, limit=None):
    """Load JSONL examples."""
    examples = []
    with open(path) as f:
        for i, line in enumerate(f):
            if limit and i >= limit:
                break
            examples.append(json.loads(line))
    return examples


def benchmark_bpe(examples, model_name="Qwen/Qwen2.5-0.5B-Instruct"):
    """Benchmark BPE tokenizer on examples."""
    from transformers import AutoTokenizer

    tokenizer = AutoTokenizer.from_pretrained(model_name, trust_remote_code=True)

    lengths = []
    unk_count = 0
    token_freq = Counter()

    for ex in examples:
        text = tokenizer.apply_chat_template(
            ex["messages"], tokenize=False, add_generation_prompt=False
        )
        ids = tokenizer.encode(text)
        lengths.append(len(ids))
        tokens = tokenizer.convert_ids_to_tokens(ids)
        token_freq.update(tokens)
        if tokenizer.unk_token_id is not None:
            unk_count += sum(1 for t in ids if t == tokenizer.unk_token_id)

    return {
        "name": f"BPE ({model_name.split('/')[-1]})",
        "vocab_size": tokenizer.vocab_size,
        "lengths": lengths,
        "total_tokens": sum(lengths),
        "avg_len": sum(lengths) / len(lengths),
        "max_len": max(lengths),
        "min_len": min(lengths),
        "median_len": sorted(lengths)[len(lengths) // 2],
        "unique_tokens_used": len(token_freq),
        "unk_tokens": unk_count,
        "vocab_utilization": len(token_freq) / tokenizer.vocab_size * 100,
    }


def benchmark_limn(examples, tokenizer_path):
    """Benchmark Limn tokenizer on examples."""
    from transformers import PreTrainedTokenizerFast

    tokenizer = PreTrainedTokenizerFast.from_pretrained(str(tokenizer_path))

    # Add chat tokens
    chat_tokens = ["<|user|>", "<|assistant|>"]
    existing = set(tokenizer.get_vocab().keys())
    new_tokens = [t for t in chat_tokens if t not in existing]
    if new_tokens:
        tokenizer.add_special_tokens({"additional_special_tokens": new_tokens})

    from retokenize_v5 import LIMN_CHAT_TEMPLATE_WITH_GEN
    tokenizer.chat_template = LIMN_CHAT_TEMPLATE_WITH_GEN

    lengths = []
    unk_count = 0
    token_freq = Counter()

    unk_id = tokenizer.unk_token_id

    for ex in examples:
        # Strip system messages (English, not tokenizable)
        messages = [m for m in ex["messages"] if m["role"] != "system"]
        text = tokenizer.apply_chat_template(
            messages, tokenize=False, add_generation_prompt=False
        )
        ids = tokenizer.encode(text)
        lengths.append(len(ids))
        tokens = tokenizer.convert_ids_to_tokens(ids)
        token_freq.update(tokens)
        if unk_id is not None:
            unk_count += sum(1 for t in ids if t == unk_id)

    return {
        "name": f"Limn ({tokenizer.vocab_size} tokens)",
        "vocab_size": tokenizer.vocab_size,
        "lengths": lengths,
        "total_tokens": sum(lengths),
        "avg_len": sum(lengths) / len(lengths),
        "max_len": max(lengths),
        "min_len": min(lengths),
        "median_len": sorted(lengths)[len(lengths) // 2],
        "unique_tokens_used": len(token_freq),
        "unk_tokens": unk_count,
        "vocab_utilization": len(token_freq) / tokenizer.vocab_size * 100,
    }


def print_comparison(bpe, limn):
    """Print side-by-side comparison."""
    print(f"\n{'='*70}")
    print(f"  TOKENIZER BENCHMARK: BPE vs Limn")
    print(f"{'='*70}")

    rows = [
        ("Tokenizer", bpe["name"], limn["name"]),
        ("Vocab size", f"{bpe['vocab_size']:,}", f"{limn['vocab_size']:,}"),
        ("", "", ""),
        ("--- Sequence Lengths ---", "", ""),
        ("Avg tokens/example", f"{bpe['avg_len']:.1f}", f"{limn['avg_len']:.1f}"),
        ("Median tokens/example", f"{bpe['median_len']}", f"{limn['median_len']}"),
        ("Max tokens/example", f"{bpe['max_len']}", f"{limn['max_len']}"),
        ("Min tokens/example", f"{bpe['min_len']}", f"{limn['min_len']}"),
        ("Total tokens", f"{bpe['total_tokens']:,}", f"{limn['total_tokens']:,}"),
        ("", "", ""),
        ("--- Coverage ---", "", ""),
        ("UNK tokens", f"{bpe['unk_tokens']:,}", f"{limn['unk_tokens']:,}"),
        ("Unique tokens used", f"{bpe['unique_tokens_used']:,}", f"{limn['unique_tokens_used']:,}"),
        ("Vocab utilization", f"{bpe['vocab_utilization']:.1f}%", f"{limn['vocab_utilization']:.1f}%"),
    ]

    for label, bpe_val, limn_val in rows:
        if label.startswith("---"):
            print(f"\n  {label}")
        elif not label:
            continue
        else:
            print(f"  {label:<25s}  {bpe_val:>20s}  {limn_val:>20s}")

    # Derived metrics
    compression = bpe["total_tokens"] / limn["total_tokens"] if limn["total_tokens"] > 0 else 0
    density_advantage = (1 - limn["avg_len"] / bpe["avg_len"]) * 100

    print(f"\n  --- Derived Metrics ---")
    print(f"  {'Compression ratio':<25s}  {compression:.2f}x (BPE tokens / Limn tokens)")
    print(f"  {'Density advantage':<25s}  {density_advantage:.1f}% fewer tokens per example")
    print(f"  {'Vocab reduction':<25s}  {(1 - limn['vocab_size'] / bpe['vocab_size']) * 100:.1f}%")

    # Training impact estimate
    # Fewer tokens = faster attention (quadratic) + more examples per batch
    attn_speedup = (bpe["avg_len"] / limn["avg_len"]) ** 2 if limn["avg_len"] > 0 else 0
    print(f"\n  --- Estimated Training Impact ---")
    print(f"  {'Attention speedup':<25s}  {attn_speedup:.1f}x (quadratic in seq length)")
    print(f"  {'Embedding params':<25s}  {limn['vocab_size'] * 896:,} vs {bpe['vocab_size'] * 896:,}")
    print(f"  {'Embedding reduction':<25s}  {(1 - limn['vocab_size'] / bpe['vocab_size']) * 100:.1f}%")

    print(f"\n{'='*70}")


def main():
    parser = argparse.ArgumentParser(description="Benchmark Limn vs BPE tokenizers")
    parser.add_argument("--data", type=str, default=str(DEFAULT_DATA),
                        help="Training data path")
    parser.add_argument("--tokenizer", type=str, default=str(DEFAULT_TOKENIZER),
                        help="Limn tokenizer path")
    parser.add_argument("--model", type=str, default="Qwen/Qwen2.5-0.5B-Instruct",
                        help="BPE model for comparison")
    parser.add_argument("--limit", type=int, default=None,
                        help="Limit number of examples (for speed)")
    args = parser.parse_args()

    print("Loading data...")
    examples = load_examples(args.data, limit=args.limit)
    print(f"  {len(examples)} examples loaded")

    print("\nBenchmarking BPE tokenizer...")
    bpe_stats = benchmark_bpe(examples, args.model)

    print("Benchmarking Limn tokenizer...")
    limn_stats = benchmark_limn(examples, args.tokenizer)

    print_comparison(bpe_stats, limn_stats)

    # Save results
    output_path = SCRIPT_DIR / "output" / "tokenizer_benchmark.json"
    output_path.parent.mkdir(exist_ok=True)
    results = {
        "bpe": {k: v for k, v in bpe_stats.items() if k != "lengths"},
        "limn": {k: v for k, v in limn_stats.items() if k != "lengths"},
        "compression_ratio": bpe_stats["total_tokens"] / limn_stats["total_tokens"],
        "examples": len(examples),
    }
    with open(output_path, "w") as f:
        json.dump(results, f, indent=2)
    print(f"\n  Results saved to: {output_path}")


if __name__ == "__main__":
    main()
