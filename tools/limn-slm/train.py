#!/usr/bin/env python3
"""
Fine-tune a small language model on Limn.

Uses Qwen2.5-0.5B-Instruct as base with QLoRA fine-tuning.
RTX 4090 (24GB) can handle this easily.

Usage:
    python train.py [--model MODEL] [--epochs N] [--batch-size N]
"""

import argparse
import json
import os
from pathlib import Path

os.environ.setdefault("WANDB_PROJECT", "limn-slm")

import torch
import wandb
from datasets import Dataset
from transformers import (
    AutoModelForCausalLM,
    AutoTokenizer,
    BitsAndBytesConfig,
    TrainingArguments,
)
from peft import LoraConfig, get_peft_model, prepare_model_for_kbit_training
from trl import SFTTrainer, SFTConfig

DATA_DIR = Path(__file__).resolve().parent / "data"
OUTPUT_DIR = Path(__file__).resolve().parent / "output"


def load_dataset(path):
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


def main():
    parser = argparse.ArgumentParser(description="Fine-tune LLM on Limn")
    parser.add_argument("--model", default="Qwen/Qwen2.5-0.5B-Instruct",
                        help="Base model (default: Qwen2.5-0.5B-Instruct)")
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
    args = parser.parse_args()

    OUTPUT_DIR.mkdir(exist_ok=True)

    print(f"=== Limn SLM Training ===")
    print(f"Base model: {args.model}")
    print(f"Device: {torch.cuda.get_device_name(0) if torch.cuda.is_available() else 'CPU'}")
    if torch.cuda.is_available():
        print(f"VRAM: {torch.cuda.get_device_properties(0).total_memory / 1e9:.1f}GB")

    # Load tokenizer
    print("\nLoading tokenizer...")
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

        # LoRA config
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
    train_examples = load_dataset(DATA_DIR / "train.jsonl")
    eval_examples = load_dataset(DATA_DIR / "eval.jsonl")
    print(f"  Train: {len(train_examples)} examples")
    print(f"  Eval: {len(eval_examples)} examples")

    # Format as text
    train_texts = [format_chat(ex, tokenizer) for ex in train_examples]
    eval_texts = [format_chat(ex, tokenizer) for ex in eval_examples]

    train_dataset = Dataset.from_dict({"text": train_texts})
    eval_dataset = Dataset.from_dict({"text": eval_texts})

    # Training config
    training_args = SFTConfig(
        output_dir=str(OUTPUT_DIR / "checkpoints"),
        num_train_epochs=args.epochs,
        per_device_train_batch_size=args.batch_size,
        per_device_eval_batch_size=args.batch_size,
        gradient_accumulation_steps=args.gradient_accumulation,
        learning_rate=args.lr,
        weight_decay=0.01,
        warmup_steps=50,
        lr_scheduler_type="cosine",
        logging_steps=10,
        eval_strategy="steps",
        eval_steps=50,
        save_strategy="steps",
        save_steps=100,
        save_total_limit=3,
        bf16=True,
        gradient_checkpointing=True,
        max_length=args.max_seq_len,
        dataset_text_field="text",
        report_to="wandb",
        run_name="limn-slm-qwen05b",
        seed=42,
    )

    # Trainer
    trainer = SFTTrainer(
        model=model,
        args=training_args,
        train_dataset=train_dataset,
        eval_dataset=eval_dataset,
        processing_class=tokenizer,
    )

    # Train
    print("\n=== Starting Training ===")
    trainer.train()

    # Save
    print("\nSaving model...")
    final_dir = OUTPUT_DIR / "limn-slm-final"
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
