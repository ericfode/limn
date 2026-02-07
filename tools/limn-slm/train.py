#!/usr/bin/env python3
"""
Fine-tune a small language model on Limn.

Uses Qwen2.5-0.5B-Instruct as base with QLoRA fine-tuning.
RTX 4090 (24GB) can handle this easily.

Supports two tokenizer modes:
  1. BPE (default): Uses base model's tokenizer (Qwen's 151K vocab)
  2. Limn-native: Uses custom 2189-token Limn tokenizer (--tokenizer flag)
     Addresses H3: BPE fragments Limn words → 1 word = 1 token

Usage:
    # BPE baseline
    python train.py [--model MODEL] [--epochs N]

    # Limn tokenizer (run retokenize_v5.py first)
    python train.py --tokenizer limn_tokenizer/ --full-finetune
"""

import argparse
import json
import os
from pathlib import Path

import torch

# Wandb setup - can be disabled with --no-wandb or WANDB_MODE=disabled
_wandb_enabled = os.environ.get("WANDB_MODE", "").lower() not in ("disabled", "offline", "dryrun")
from datasets import Dataset
from transformers import (
    AutoModelForCausalLM,
    AutoTokenizer,
    BitsAndBytesConfig,
    EarlyStoppingCallback,
    PreTrainedTokenizerFast,
    TrainingArguments,
)
from peft import LoraConfig, get_peft_model, prepare_model_for_kbit_training
from trl import SFTTrainer, SFTConfig

DATA_DIR = Path(__file__).resolve().parent / "data"
OUTPUT_DIR = Path(__file__).resolve().parent / "output"

# Chat template for Limn-native tokenizer
LIMN_CHAT_TEMPLATE = (
    "{% for message in messages %}"
    "{% if message['role'] == 'user' %}"
    "<|user|> {{ message['content'] }} "
    "{% elif message['role'] == 'assistant' %}"
    "<|assistant|> {{ message['content'] }}"
    "{% endif %}"
    "{% endfor %}"
    "{% if add_generation_prompt %}<|assistant|> {% endif %}"
)


def load_jsonl(path):
    """Load JSONL dataset."""
    examples = []
    with open(path) as f:
        for line in f:
            examples.append(json.loads(line))
    return examples


def format_chat(example, tokenizer):
    """Format example as chat template string."""
    return tokenizer.apply_chat_template(
        example["messages"],
        tokenize=False,
        add_generation_prompt=False,
    )


def format_limn_chat(example, tokenizer):
    """Format example for Limn-native tokenizer.

    Strips English system prompts (constant, redundant for Limn-native model).
    """
    messages = [m for m in example["messages"] if m["role"] != "system"]
    return tokenizer.apply_chat_template(
        messages, tokenize=False, add_generation_prompt=False
    )


def load_limn_tokenizer(tokenizer_path):
    """Load Limn tokenizer and configure for training.

    Use limn_tokenizer_extended/ (from retokenize_v5.py --extend-vocab)
    for best coverage. The extended tokenizer has chat role tokens and
    missing vocab words already in the base WordLevel model.
    """
    tokenizer = PreTrainedTokenizerFast.from_pretrained(tokenizer_path)

    # Set chat template (may already be set in saved config)
    if tokenizer.chat_template is None:
        tokenizer.chat_template = LIMN_CHAT_TEMPLATE

    # Ensure pad token
    if tokenizer.pad_token is None:
        tokenizer.pad_token = tokenizer.eos_token
    tokenizer.padding_side = "right"

    return tokenizer


def main():
    parser = argparse.ArgumentParser(description="Fine-tune LLM on Limn")
    parser.add_argument("--model", default="Qwen/Qwen2.5-0.5B-Instruct",
                        help="Base model (default: Qwen2.5-0.5B-Instruct)")
    parser.add_argument("--tokenizer", type=str, default=None,
                        help="Path to Limn tokenizer (enables Limn-native mode)")
    parser.add_argument("--epochs", type=int, default=3,
                        help="Training epochs (default: 3)")
    parser.add_argument("--batch-size", type=int, default=4,
                        help="Per-device batch size (default: 4)")
    parser.add_argument("--lr", type=float, default=2e-4,
                        help="Learning rate (default: 2e-4)")
    parser.add_argument("--lora-r", type=int, default=64,
                        help="LoRA rank (default: 64)")
    parser.add_argument("--lora-alpha", type=int, default=128,
                        help="LoRA alpha (default: 128)")
    parser.add_argument("--max-seq-len", type=int, default=1024,
                        help="Max sequence length (default: 1024)")
    parser.add_argument("--gradient-accumulation", type=int, default=4,
                        help="Gradient accumulation steps (default: 4)")
    parser.add_argument("--full-finetune", action="store_true",
                        help="Full fine-tune instead of LoRA (uses more VRAM)")
    parser.add_argument("--resume", type=str, default=None,
                        help="Resume from checkpoint directory")
    parser.add_argument("--no-wandb", action="store_true",
                        help="Disable wandb logging")
    parser.add_argument("--train-data", type=str, default=None,
                        help="Custom training data path")
    parser.add_argument("--eval-data", type=str, default=None,
                        help="Custom eval data path")
    parser.add_argument("--output-dir", type=str, default=None,
                        help="Custom output directory")
    parser.add_argument("--early-stopping", type=int, default=3,
                        help="Early stopping patience (0=disabled, default: 3)")
    args = parser.parse_args()

    # Handle wandb
    global _wandb_enabled
    if args.no_wandb:
        _wandb_enabled = False
        os.environ["WANDB_MODE"] = "disabled"

    OUTPUT_DIR.mkdir(exist_ok=True)

    use_limn_tokenizer = args.tokenizer is not None

    print(f"=== Limn SLM Training ===")
    print(f"Base model: {args.model}")
    print(f"Tokenizer:  {'Limn-native (' + args.tokenizer + ')' if use_limn_tokenizer else 'BPE (base model)'}")
    print(f"Device: {torch.cuda.get_device_name(0) if torch.cuda.is_available() else 'CPU'}")
    if torch.cuda.is_available():
        print(f"VRAM: {torch.cuda.get_device_properties(0).total_memory / 1e9:.1f}GB")

    # Load tokenizer
    print("\nLoading tokenizer...")
    if use_limn_tokenizer:
        tokenizer = load_limn_tokenizer(args.tokenizer)
        print(f"  Limn tokenizer: {tokenizer.vocab_size} tokens")
        if not args.full_finetune:
            print("  NOTE: Limn tokenizer reinitializes embeddings.")
            print("        --full-finetune recommended for best results.")
    else:
        tokenizer = AutoTokenizer.from_pretrained(args.model, trust_remote_code=True)
        if tokenizer.pad_token is None:
            tokenizer.pad_token = tokenizer.eos_token
        tokenizer.padding_side = "right"

    # Load model
    print("Loading model...")
    if args.full_finetune:
        model = AutoModelForCausalLM.from_pretrained(
            args.model,
            torch_dtype=torch.bfloat16,
            device_map="auto",
            trust_remote_code=True,
        )
    else:
        # QLoRA: 4-bit quantized base + LoRA adapters
        bnb_config = BitsAndBytesConfig(
            load_in_4bit=True,
            bnb_4bit_quant_type="nf4",
            bnb_4bit_compute_dtype=torch.bfloat16,
            bnb_4bit_use_double_quant=True,
        )
        model = AutoModelForCausalLM.from_pretrained(
            args.model,
            quantization_config=bnb_config,
            device_map="auto",
            trust_remote_code=True,
        )
        model = prepare_model_for_kbit_training(model)

    # Resize embeddings for Limn tokenizer
    if use_limn_tokenizer:
        old_vocab = model.config.vocab_size
        new_vocab = len(tokenizer)
        print(f"  Resizing embeddings: {old_vocab} → {new_vocab}")
        model.resize_token_embeddings(new_vocab)

        # Reinitialize embeddings — old token IDs don't map to Limn vocab
        # Transformer backbone retains pretrained knowledge (attention/FFN)
        embed = model.get_input_embeddings()
        embed.weight.data.normal_(mean=0.0, std=0.02)
        lm_head = model.get_output_embeddings()
        if lm_head is not None:
            lm_head.weight.data.normal_(mean=0.0, std=0.02)
        print(f"  Embeddings reinitialized (std=0.02)")

    # Apply LoRA if not full fine-tune
    if not args.full_finetune:
        lora_config = LoraConfig(
            r=args.lora_r,
            lora_alpha=args.lora_alpha,
            lora_dropout=0.05,
            bias="none",
            task_type="CAUSAL_LM",
            target_modules=["q_proj", "k_proj", "v_proj", "o_proj",
                            "gate_proj", "up_proj", "down_proj"],
        )
        model = get_peft_model(model, lora_config)
        model.print_trainable_parameters()

    # Load data
    print("\nLoading training data...")
    if args.train_data:
        train_path = Path(args.train_data)
    elif use_limn_tokenizer:
        # Prefer retokenized data if available
        limn_data = DATA_DIR / "v5_limn" / "train.jsonl"
        train_path = limn_data if limn_data.exists() else DATA_DIR / "v5" / "train.jsonl"
    else:
        train_path = DATA_DIR / "train.jsonl"

    if args.eval_data:
        eval_path = Path(args.eval_data)
    elif use_limn_tokenizer:
        limn_eval = DATA_DIR / "v5_limn" / "val.jsonl"
        eval_path = limn_eval if limn_eval.exists() else DATA_DIR / "v5" / "val.jsonl"
    else:
        eval_path = DATA_DIR / "eval.jsonl"

    train_examples = load_jsonl(train_path)
    eval_examples = load_jsonl(eval_path)
    print(f"  Train: {len(train_examples)} examples from {train_path}")
    print(f"  Eval:  {len(eval_examples)} examples from {eval_path}")

    # Format as text
    # Retokenized data has "text" field; original data has "messages" field
    def format_example(ex):
        if "text" in ex:
            return ex["text"]
        elif use_limn_tokenizer:
            return format_limn_chat(ex, tokenizer)
        else:
            return format_chat(ex, tokenizer)

    train_texts = [format_example(ex) for ex in train_examples]
    eval_texts = [format_example(ex) for ex in eval_examples]

    train_dataset = Dataset.from_dict({"text": train_texts})
    eval_dataset = Dataset.from_dict({"text": eval_texts})

    # Custom output dir
    out_dir = Path(args.output_dir) if args.output_dir else OUTPUT_DIR

    # Adjust LR
    lr = args.lr
    if args.full_finetune and args.lr == 2e-4:
        lr = 5e-5  # More conservative for full fine-tune
        print(f"  Auto-adjusted LR to {lr} for full fine-tune")
    if use_limn_tokenizer and not args.full_finetune and args.lr == 2e-4:
        lr = 1e-3  # Higher LR for LoRA with fresh embeddings
        print(f"  Auto-adjusted LR to {lr} for LoRA + fresh embeddings")

    # Determine run version
    if use_limn_tokenizer:
        version_tag = "v5-limn"
        run_name = "limn-slm-v5-limntok"
    else:
        version_tag = "v5-bpe"
        run_name = "limn-slm-v5-bpe"

    # Limn tokenizer produces much shorter sequences (median 14, max 51)
    max_seq_len = args.max_seq_len
    if use_limn_tokenizer and args.max_seq_len == 1024:
        max_seq_len = 128  # Limn sequences are very compact (p99=29)
        print(f"  Auto-adjusted max_seq_len to {max_seq_len} for Limn tokenizer")

    # Training config
    training_args = SFTConfig(
        output_dir=str(out_dir / "checkpoints"),
        num_train_epochs=args.epochs,
        per_device_train_batch_size=args.batch_size,
        per_device_eval_batch_size=args.batch_size,
        gradient_accumulation_steps=args.gradient_accumulation,
        learning_rate=lr,
        weight_decay=0.01,
        warmup_steps=50,
        lr_scheduler_type="cosine",
        logging_steps=10,
        eval_strategy="steps",
        eval_steps=50,
        save_strategy="steps",
        save_steps=100,
        save_total_limit=3,
        load_best_model_at_end=args.early_stopping > 0,
        metric_for_best_model="eval_loss",
        greater_is_better=False,
        bf16=True,
        gradient_checkpointing=True,
        max_length=max_seq_len,
        dataset_text_field="text",
        report_to="wandb" if _wandb_enabled else "none",
        run_name=run_name if _wandb_enabled else None,
        seed=42,
    )

    # Callbacks
    callbacks = []
    if args.early_stopping > 0:
        callbacks.append(EarlyStoppingCallback(
            early_stopping_patience=args.early_stopping
        ))
        print(f"  Early stopping enabled (patience={args.early_stopping})")

    # Trainer
    trainer = SFTTrainer(
        model=model,
        args=training_args,
        train_dataset=train_dataset,
        eval_dataset=eval_dataset,
        processing_class=tokenizer,
        callbacks=callbacks,
    )

    # Train
    print("\n=== Starting Training ===")
    if args.resume:
        print(f"Resuming from {args.resume}")
        trainer.train(resume_from_checkpoint=args.resume)
    else:
        trainer.train()

    # Save
    print("\nSaving model...")
    mode = "full" if args.full_finetune else "lora"
    final_dir = out_dir / f"limn-slm-{version_tag}-{mode}"
    if args.full_finetune:
        trainer.save_model(str(final_dir))
    else:
        # Save LoRA adapters
        model.save_pretrained(str(final_dir))
    tokenizer.save_pretrained(str(final_dir))

    print(f"\n=== Training Complete ===")
    print(f"Model saved to: {final_dir}")
    print(f"To serve: python serve.py --model {final_dir}")


if __name__ == "__main__":
    main()
