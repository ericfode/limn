#!/usr/bin/env python3
"""
Retokenize v5 datasets for Limn-native training.

Validates that the Limn tokenizer can handle all training data,
reports coverage statistics, and writes retokenized outputs.

The key transformation: strip English system prompts (constant across
all examples, redundant for a Limn-native model) and format with
minimal chat role markers.

Usage:
    python retokenize_v5.py [--tokenizer limn_tokenizer/] [--data-dir data/v5/]
    python retokenize_v5.py --filter-unk   # Drop examples with UNK tokens
"""

import argparse
import json
import os
import sys
from collections import Counter
from pathlib import Path

from transformers import PreTrainedTokenizerFast


SCRIPT_DIR = Path(__file__).resolve().parent
DEFAULT_TOKENIZER = SCRIPT_DIR / "limn_tokenizer"
DEFAULT_DATA_DIR = SCRIPT_DIR / "data" / "v5"
OUTPUT_DIR = SCRIPT_DIR / "data" / "v5_limn"

# Chat template for Limn-native tokenizer (Jinja2)
# Skips system messages (English, can't be tokenized), uses role markers
LIMN_CHAT_TEMPLATE = (
    "{% for message in messages %}"
    "{% if message['role'] == 'user' %}"
    "<|user|> {{ message['content'] }} "
    "{% elif message['role'] == 'assistant' %}"
    "<|assistant|> {{ message['content'] }}"
    "{% endif %}"
    "{% endfor %}"
)

LIMN_CHAT_TEMPLATE_WITH_GEN = (
    "{% for message in messages %}"
    "{% if message['role'] == 'user' %}"
    "<|user|> {{ message['content'] }} "
    "{% elif message['role'] == 'assistant' %}"
    "<|assistant|> {{ message['content'] }}"
    "{% endif %}"
    "{% endfor %}"
    "{% if add_generation_prompt %}<|assistant|> {% endif %}"
)


def load_limn_tokenizer(tokenizer_path):
    """Load and configure the Limn tokenizer with chat role tokens."""
    tokenizer = PreTrainedTokenizerFast.from_pretrained(str(tokenizer_path))

    # Add chat role tokens if not already present
    chat_tokens = ["<|user|>", "<|assistant|>"]
    existing = set(tokenizer.get_vocab().keys())
    new_tokens = [t for t in chat_tokens if t not in existing]

    if new_tokens:
        tokenizer.add_special_tokens({"additional_special_tokens": new_tokens})
        print(f"  Added chat tokens: {new_tokens}")

    # Set chat template
    tokenizer.chat_template = LIMN_CHAT_TEMPLATE_WITH_GEN

    return tokenizer


# Limn structural symbols that should be in vocab
LIMN_SYMBOLS = {"∎", "~", "∿", "—", "#"}


def is_likely_limn_token(word):
    """Check if a word is likely a Limn vocabulary word (not English)."""
    # 3-letter lowercase CVC words are canonical Limn
    if len(word) == 3 and word.isalpha() and word.islower():
        return True
    # 2-letter Limn function words (quantifiers, particles)
    if len(word) == 2 and word.isalpha() and word.islower():
        return True
    # Known Limn symbols
    if word in LIMN_SYMBOLS:
        return True
    # Single lowercase letters used as Limn variables
    if len(word) == 1 and word.islower():
        return True
    # 4-letter Limn words (some exist, like "sprl", "pani")
    if len(word) == 4 and word.isalpha() and word.islower():
        return True
    return False


def extend_vocab_from_data(tokenizer_path, data_dir, min_occurrences=5):
    """Scan training data and rebuild tokenizer with missing Limn tokens.

    Must rebuild the base WordLevel model to avoid added_tokens interference.
    Returns (new_tokenizer, list of tokens added).
    """
    from tokenizers import Tokenizer, Regex, pre_tokenizers, models
    from tokenizers.processors import TemplateProcessing

    # Load existing tokenizer.json to get base vocab
    tok_json_path = Path(tokenizer_path) / "tokenizer.json"
    with open(tok_json_path) as f:
        tok_data = json.load(f)

    base_vocab = dict(tok_data["model"]["vocab"])
    next_id = max(base_vocab.values()) + 1

    # Build a temporary tokenizer for UNK detection
    tmp_tok = PreTrainedTokenizerFast.from_pretrained(str(tokenizer_path))
    unk_id = tmp_tok.unk_token_id

    # Scan training data for missing tokens
    missing = Counter()
    train_path = data_dir / "train.jsonl"
    if not train_path.exists():
        return tmp_tok, []

    with open(train_path) as f:
        for line in f:
            ex = json.loads(line)
            for msg in ex["messages"]:
                if msg["role"] in ("user", "assistant"):
                    for word in msg["content"].split():
                        clean = word.strip(".,?!;()[]{}\"'")
                        ids = tmp_tok.encode(clean, add_special_tokens=False)
                        if any(t == unk_id for t in ids):
                            if is_likely_limn_token(clean):
                                missing[clean] += 1

    # Add missing tokens to base vocab
    new_tokens = [w for w, c in missing.most_common() if c >= min_occurrences]
    for tok in new_tokens:
        if tok not in base_vocab:
            base_vocab[tok] = next_id
            next_id += 1

    # Also add chat role tokens
    for tok in ["<|user|>", "<|assistant|>"]:
        if tok not in base_vocab:
            base_vocab[tok] = next_id
            next_id += 1

    # Rebuild tokenizer with extended vocab
    tokenizer = Tokenizer(models.WordLevel(vocab=base_vocab, unk_token="<unk>"))

    # Same pre-tokenizer as original
    tokenizer.pre_tokenizer = pre_tokenizers.Sequence([
        pre_tokenizers.WhitespaceSplit(),
        pre_tokenizers.Split(
            Regex(r'[@*\^\\±:|→=!\+\-\n"]'),
            behavior="isolated",
        ),
    ])

    # Same post-processor
    tokenizer.post_processor = TemplateProcessing(
        single="<bos> $A <eos>",
        pair="<bos> $A <sep> $B:1 <eos>",
        special_tokens=[
            ("<bos>", base_vocab["<bos>"]),
            ("<eos>", base_vocab["<eos>"]),
            ("<sep>", base_vocab["<sep>"]),
        ],
    )

    # Wrap as HuggingFace tokenizer
    hf_tokenizer = PreTrainedTokenizerFast(
        tokenizer_object=tokenizer,
        unk_token="<unk>",
        pad_token="<pad>",
        bos_token="<bos>",
        eos_token="<eos>",
        sep_token="<sep>",
        additional_special_tokens=["<|user|>", "<|assistant|>"],
    )

    # Set chat template
    hf_tokenizer.chat_template = LIMN_CHAT_TEMPLATE_WITH_GEN

    return hf_tokenizer, new_tokens


def format_for_limn(example, tokenizer):
    """Format a training example for Limn-native tokenization.

    Strips the English system prompt and formats with chat role markers.
    Returns the formatted text string.
    """
    # Filter out system messages (English, can't tokenize)
    messages = [m for m in example["messages"] if m["role"] != "system"]
    return tokenizer.apply_chat_template(
        messages, tokenize=False, add_generation_prompt=False
    )


def analyze_coverage(tokenizer, examples, split_name):
    """Analyze tokenizer coverage on a dataset split."""
    stats = {
        "total_examples": len(examples),
        "total_tokens": 0,
        "unk_tokens": 0,
        "unk_examples": 0,
        "seq_lengths": [],
        "unk_words": Counter(),
    }

    unk_id = tokenizer.unk_token_id

    for ex in examples:
        text = format_for_limn(ex, tokenizer)
        token_ids = tokenizer.encode(text)
        tokens = tokenizer.convert_ids_to_tokens(token_ids)

        stats["total_tokens"] += len(token_ids)
        stats["seq_lengths"].append(len(token_ids))

        unk_count = sum(1 for t in token_ids if t == unk_id)
        if unk_count > 0:
            stats["unk_tokens"] += unk_count
            stats["unk_examples"] += 1
            # Find UNK words by re-tokenizing without special tokens
            words = text.split()
            for w in words:
                w_ids = tokenizer.encode(w, add_special_tokens=False)
                if any(t == unk_id for t in w_ids):
                    stats["unk_words"][w] += 1

    # Compute derived stats
    lengths = stats["seq_lengths"]
    stats["avg_seq_len"] = sum(lengths) / len(lengths) if lengths else 0
    stats["max_seq_len"] = max(lengths) if lengths else 0
    stats["min_seq_len"] = min(lengths) if lengths else 0
    stats["median_seq_len"] = sorted(lengths)[len(lengths) // 2] if lengths else 0
    stats["coverage_pct"] = (
        100.0 * (stats["total_tokens"] - stats["unk_tokens"]) / stats["total_tokens"]
        if stats["total_tokens"] > 0 else 0
    )

    return stats


def print_stats(stats, split_name):
    """Print coverage statistics for a split."""
    print(f"\n  [{split_name}]")
    print(f"    Examples:     {stats['total_examples']}")
    print(f"    Total tokens: {stats['total_tokens']}")
    print(f"    Coverage:     {stats['coverage_pct']:.2f}%")
    print(f"    UNK tokens:   {stats['unk_tokens']}")
    print(f"    UNK examples: {stats['unk_examples']}/{stats['total_examples']}")
    print(f"    Seq length:   avg={stats['avg_seq_len']:.1f}, "
          f"med={stats['median_seq_len']}, "
          f"max={stats['max_seq_len']}, "
          f"min={stats['min_seq_len']}")
    if stats["unk_words"]:
        top_unk = stats["unk_words"].most_common(20)
        print(f"    Top UNK words: {top_unk}")


def has_unk_tokens(tokenizer, example):
    """Check if an example produces UNK tokens when tokenized."""
    text = format_for_limn(example, tokenizer)
    token_ids = tokenizer.encode(text)
    return any(t == tokenizer.unk_token_id for t in token_ids)


def retokenize_split(tokenizer, examples, output_path, filter_unk=False):
    """Retokenize a dataset split and write formatted output.

    If filter_unk=True, drops examples that produce UNK tokens.
    This filters out English-containing examples (meta-questions like
    "What does X mean?") that can't be tokenized by the Limn tokenizer.
    """
    output_path.parent.mkdir(parents=True, exist_ok=True)

    written = 0
    dropped = 0
    with open(output_path, "w") as f:
        for ex in examples:
            if filter_unk and has_unk_tokens(tokenizer, ex):
                dropped += 1
                continue
            text = format_for_limn(ex, tokenizer)
            out = {"text": text}
            # Preserve metadata if present
            if "v5_meta" in ex:
                out["v5_meta"] = ex["v5_meta"]
            f.write(json.dumps(out) + "\n")
            written += 1

    if dropped > 0:
        print(f"    Filtered: {dropped} examples with UNK tokens dropped")

    return written


def compare_with_bpe(limn_tokenizer, examples, bpe_model="Qwen/Qwen2.5-0.5B-Instruct"):
    """Compare sequence lengths between Limn and BPE tokenizers."""
    try:
        from transformers import AutoTokenizer
        bpe_tokenizer = AutoTokenizer.from_pretrained(bpe_model, trust_remote_code=True)
    except Exception as e:
        print(f"\n  Skipping BPE comparison (can't load {bpe_model}): {e}")
        return None

    limn_lengths = []
    bpe_lengths = []

    for ex in examples[:1000]:  # Sample for speed
        # Limn: just user/assistant content
        limn_text = format_for_limn(ex, limn_tokenizer)
        limn_ids = limn_tokenizer.encode(limn_text)
        limn_lengths.append(len(limn_ids))

        # BPE: full message with system prompt
        bpe_text = bpe_tokenizer.apply_chat_template(
            ex["messages"], tokenize=False, add_generation_prompt=False
        )
        bpe_ids = bpe_tokenizer.encode(bpe_text)
        bpe_lengths.append(len(bpe_ids))

    return {
        "limn_avg": sum(limn_lengths) / len(limn_lengths),
        "bpe_avg": sum(bpe_lengths) / len(bpe_lengths),
        "limn_total": sum(limn_lengths),
        "bpe_total": sum(bpe_lengths),
        "compression_ratio": sum(bpe_lengths) / sum(limn_lengths) if sum(limn_lengths) > 0 else 0,
        "samples": len(limn_lengths),
    }


def main():
    parser = argparse.ArgumentParser(description="Retokenize v5 datasets for Limn tokenizer")
    parser.add_argument("--tokenizer", type=str, default=str(DEFAULT_TOKENIZER),
                        help="Path to Limn tokenizer directory")
    parser.add_argument("--data-dir", type=str, default=str(DEFAULT_DATA_DIR),
                        help="Path to v5 data directory")
    parser.add_argument("--output-dir", type=str, default=str(OUTPUT_DIR),
                        help="Output directory for retokenized data")
    parser.add_argument("--compare-bpe", action="store_true",
                        help="Compare with BPE tokenizer (requires model download)")
    parser.add_argument("--extend-vocab", action="store_true",
                        help="Add missing Limn words from training data to tokenizer")
    parser.add_argument("--filter-unk", action="store_true",
                        help="Drop examples with UNK tokens (English content)")
    parser.add_argument("--dry-run", action="store_true",
                        help="Only analyze coverage, don't write output")
    args = parser.parse_args()

    data_dir = Path(args.data_dir)
    output_dir = Path(args.output_dir)

    print("=" * 60)
    print("  Limn v5 Retokenization")
    print("=" * 60)

    # Load tokenizer
    print(f"\nLoading Limn tokenizer from {args.tokenizer}...")
    tokenizer = load_limn_tokenizer(args.tokenizer)
    print(f"  Vocab size: {tokenizer.vocab_size}")

    # Extend vocab with missing Limn words from training data
    if args.extend_vocab:
        print(f"\nExtending vocabulary from training data...")
        tokenizer, added = extend_vocab_from_data(args.tokenizer, data_dir)
        if added:
            print(f"  Added {len(added)} missing Limn tokens to base vocab")
            print(f"  New vocab size: {len(tokenizer)}")
            print(f"  Sample additions: {added[:20]}")
            # Save extended tokenizer
            ext_dir = Path(args.tokenizer).parent / (Path(args.tokenizer).name + "_extended")
            tokenizer.save_pretrained(str(ext_dir))
            print(f"  Extended tokenizer saved to: {ext_dir}")
        else:
            print(f"  No missing tokens found")

    # Process each split
    splits = {
        "train": "train.jsonl",
        "val": "val.jsonl",
        "test": "test.jsonl",
        "tier2_eval": "tier2_eval.jsonl",
        "tier3_eval": "tier3_eval.jsonl",
    }

    all_stats = {}
    for split_name, filename in splits.items():
        path = data_dir / filename
        if not path.exists():
            print(f"\n  [{split_name}] SKIPPED (file not found: {path})")
            continue

        # Load data
        with open(path) as f:
            examples = [json.loads(line) for line in f]

        # Analyze coverage
        stats = analyze_coverage(tokenizer, examples, split_name)
        all_stats[split_name] = stats
        print_stats(stats, split_name)

        # Write retokenized output
        if not args.dry_run:
            out_path = output_dir / filename
            n = retokenize_split(tokenizer, examples, out_path,
                                 filter_unk=args.filter_unk)
            print(f"    Written: {out_path} ({n} examples)")

    # BPE comparison
    if args.compare_bpe:
        print(f"\n{'='*60}")
        print("  BPE vs Limn Tokenizer Comparison")
        print(f"{'='*60}")

        train_path = data_dir / "train.jsonl"
        if train_path.exists():
            with open(train_path) as f:
                examples = [json.loads(line) for line in f]
            comparison = compare_with_bpe(tokenizer, examples)
            if comparison:
                print(f"\n  Sample size:        {comparison['samples']}")
                print(f"  BPE avg seq len:    {comparison['bpe_avg']:.1f}")
                print(f"  Limn avg seq len:   {comparison['limn_avg']:.1f}")
                print(f"  Compression ratio:  {comparison['compression_ratio']:.2f}x")
                print(f"  (BPE tokens / Limn tokens)")

    # Summary
    print(f"\n{'='*60}")
    print("  Summary")
    print(f"{'='*60}")

    total_unk = sum(s["unk_tokens"] for s in all_stats.values())
    total_tok = sum(s["total_tokens"] for s in all_stats.values())
    overall_coverage = 100.0 * (total_tok - total_unk) / total_tok if total_tok > 0 else 0

    print(f"\n  Overall coverage: {overall_coverage:.2f}%")
    print(f"  Total tokens:     {total_tok}")
    print(f"  Total UNK:        {total_unk}")

    if total_unk > 0:
        print("\n  WARNING: Non-zero UNK count. Some training examples contain")
        print("  words not in the Limn vocabulary. Review UNK words above.")
        # Aggregate UNK words across splits
        all_unk = Counter()
        for s in all_stats.values():
            all_unk.update(s["unk_words"])
        if all_unk:
            print(f"\n  All UNK words (top 30): {all_unk.most_common(30)}")

    if not args.dry_run:
        # Save stats
        stats_path = output_dir / "retokenization_stats.json"
        output_dir.mkdir(parents=True, exist_ok=True)
        with open(stats_path, "w") as f:
            # Convert Counters to dicts for JSON
            serializable = {}
            for k, v in all_stats.items():
                sv = dict(v)
                sv["unk_words"] = dict(sv["unk_words"])
                del sv["seq_lengths"]  # Too large for JSON
                serializable[k] = sv
            json.dump(serializable, f, indent=2)
        print(f"\n  Stats saved to: {stats_path}")

    print("\n  Done.")


if __name__ == "__main__":
    main()
