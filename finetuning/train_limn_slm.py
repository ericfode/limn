#!/usr/bin/env python3
"""
Train Limn Small Language Model
================================

Finetune a small language model on Limn using LoRA/QLoRA for efficiency.

Requirements:
    pip install transformers datasets peft accelerate bitsandbytes torch

Author: Polecat now
Date: 2026-02-02
"""

import os
import json
from pathlib import Path
from typing import Dict, List, Optional
from dataclasses import dataclass, field

import torch
from datasets import load_dataset, Dataset
from transformers import (
    AutoTokenizer,
    AutoModelForCausalLM,
    TrainingArguments,
    Trainer,
    DataCollatorForLanguageModeling,
    EarlyStoppingCallback,
)
from peft import LoraConfig, get_peft_model, TaskType, PeftModel


@dataclass
class TrainingConfig:
    """Configuration for training."""

    # Model
    model_name: str = "gpt2"  # Small base model
    # Alternative: "distilgpt2", "facebook/opt-125m", "EleutherAI/pythia-70m"

    # Dataset
    train_file: str = "limn_train.jsonl"
    val_file: str = "limn_val.jsonl"

    # LoRA config
    lora_r: int = 8  # Rank
    lora_alpha: int = 16  # Scaling factor
    lora_dropout: float = 0.1
    target_modules: List[str] = field(default_factory=lambda: ["c_attn", "c_proj"])

    # Training hyperparameters
    output_dir: str = "./limn_model"
    num_train_epochs: int = 3
    per_device_train_batch_size: int = 8
    per_device_eval_batch_size: int = 8
    gradient_accumulation_steps: int = 4
    learning_rate: float = 2e-4
    weight_decay: float = 0.01
    warmup_steps: int = 100
    logging_steps: int = 10
    eval_steps: int = 100
    save_steps: int = 500
    max_seq_length: int = 128

    # Early stopping
    early_stopping_patience: int = 3
    early_stopping_threshold: float = 0.01

    # Hardware
    use_fp16: bool = True
    use_8bit: bool = False  # Set True for QLoRA


class LimnTrainer:
    """Trainer for Limn language model."""

    def __init__(self, config: TrainingConfig):
        """Initialize trainer.

        Args:
            config: Training configuration
        """
        self.config = config
        self.tokenizer = None
        self.model = None
        self.train_dataset = None
        self.val_dataset = None

    def load_tokenizer(self):
        """Load and configure tokenizer."""
        print(f"Loading tokenizer: {self.config.model_name}")

        self.tokenizer = AutoTokenizer.from_pretrained(self.config.model_name)

        # Add special tokens for Limn operators
        special_tokens = {
            "additional_special_tokens": ["∎", "~", "∿", "|", "→", "←", "↔"]
        }
        self.tokenizer.add_special_tokens(special_tokens)

        # Set padding token
        if self.tokenizer.pad_token is None:
            self.tokenizer.pad_token = self.tokenizer.eos_token

    def load_model(self):
        """Load base model and apply LoRA."""
        print(f"Loading model: {self.config.model_name}")

        # Load base model
        model_kwargs = {}
        if self.config.use_8bit:
            model_kwargs["load_in_8bit"] = True
            model_kwargs["device_map"] = "auto"

        self.model = AutoModelForCausalLM.from_pretrained(
            self.config.model_name,
            **model_kwargs
        )

        # Resize embeddings for new tokens
        self.model.resize_token_embeddings(len(self.tokenizer))

        # Apply LoRA
        print("Applying LoRA...")
        lora_config = LoraConfig(
            task_type=TaskType.CAUSAL_LM,
            r=self.config.lora_r,
            lora_alpha=self.config.lora_alpha,
            lora_dropout=self.config.lora_dropout,
            target_modules=self.config.target_modules,
            bias="none",
        )

        self.model = get_peft_model(self.model, lora_config)
        self.model.print_trainable_parameters()

    def load_datasets(self):
        """Load train and validation datasets."""
        print("Loading datasets...")

        # Load JSONL files
        data_files = {
            "train": self.config.train_file,
            "validation": self.config.val_file,
        }

        dataset = load_dataset("json", data_files=data_files)

        # Preprocess
        def preprocess_function(examples):
            """Preprocess examples into input format."""
            # Format: input + output
            texts = []
            for inp, out in zip(examples["input"], examples["output"]):
                # Format as instruction-following
                text = f"Input: {inp}\nOutput: {out}"
                texts.append(text)

            # Tokenize
            tokenized = self.tokenizer(
                texts,
                max_length=self.config.max_seq_length,
                truncation=True,
                padding="max_length",
            )

            # Set labels (same as input_ids for causal LM)
            tokenized["labels"] = tokenized["input_ids"].copy()

            return tokenized

        # Preprocess datasets
        tokenized_dataset = dataset.map(
            preprocess_function,
            batched=True,
            remove_columns=dataset["train"].column_names,
        )

        self.train_dataset = tokenized_dataset["train"]
        self.val_dataset = tokenized_dataset["validation"]

        print(f"Train size: {len(self.train_dataset)}")
        print(f"Validation size: {len(self.val_dataset)}")

    def train(self):
        """Train the model."""
        print("Starting training...")

        # Training arguments
        training_args = TrainingArguments(
            output_dir=self.config.output_dir,
            num_train_epochs=self.config.num_train_epochs,
            per_device_train_batch_size=self.config.per_device_train_batch_size,
            per_device_eval_batch_size=self.config.per_device_eval_batch_size,
            gradient_accumulation_steps=self.config.gradient_accumulation_steps,
            learning_rate=self.config.learning_rate,
            weight_decay=self.config.weight_decay,
            warmup_steps=self.config.warmup_steps,
            logging_steps=self.config.logging_steps,
            eval_steps=self.config.eval_steps,
            save_steps=self.config.save_steps,
            evaluation_strategy="steps",
            save_strategy="steps",
            load_best_model_at_end=True,
            fp16=self.config.use_fp16,
            report_to="none",  # Disable wandb/tensorboard for simplicity
        )

        # Data collator
        data_collator = DataCollatorForLanguageModeling(
            tokenizer=self.tokenizer,
            mlm=False,  # Causal LM, not masked LM
        )

        # Early stopping
        early_stopping = EarlyStoppingCallback(
            early_stopping_patience=self.config.early_stopping_patience,
            early_stopping_threshold=self.config.early_stopping_threshold,
        )

        # Initialize trainer
        trainer = Trainer(
            model=self.model,
            args=training_args,
            train_dataset=self.train_dataset,
            eval_dataset=self.val_dataset,
            data_collator=data_collator,
            callbacks=[early_stopping],
        )

        # Train
        trainer.train()

        # Save final model
        print("Saving final model...")
        trainer.save_model(self.config.output_dir)
        self.tokenizer.save_pretrained(self.config.output_dir)

        print(f"Training complete! Model saved to {self.config.output_dir}")

    def run(self):
        """Run full training pipeline."""
        self.load_tokenizer()
        self.load_model()
        self.load_datasets()
        self.train()


def main():
    """Main training script."""
    import argparse

    parser = argparse.ArgumentParser(description="Train Limn SLM")
    parser.add_argument("--model", default="gpt2", help="Base model name")
    parser.add_argument("--train-file", default="limn_train.jsonl", help="Training data file")
    parser.add_argument("--val-file", default="limn_val.jsonl", help="Validation data file")
    parser.add_argument("--output-dir", default="./limn_model", help="Output directory")
    parser.add_argument("--epochs", type=int, default=3, help="Number of epochs")
    parser.add_argument("--batch-size", type=int, default=8, help="Batch size per device")
    parser.add_argument("--learning-rate", type=float, default=2e-4, help="Learning rate")
    parser.add_argument("--use-8bit", action="store_true", help="Use 8-bit quantization (QLoRA)")
    parser.add_argument("--max-seq-length", type=int, default=128, help="Max sequence length")

    args = parser.parse_args()

    # Create config
    config = TrainingConfig(
        model_name=args.model,
        train_file=args.train_file,
        val_file=args.val_file,
        output_dir=args.output_dir,
        num_train_epochs=args.epochs,
        per_device_train_batch_size=args.batch_size,
        per_device_eval_batch_size=args.batch_size,
        learning_rate=args.learning_rate,
        use_8bit=args.use_8bit,
        max_seq_length=args.max_seq_length,
    )

    print("=== Limn SLM Training ===")
    print(f"Base model: {config.model_name}")
    print(f"Training file: {config.train_file}")
    print(f"Validation file: {config.val_file}")
    print(f"Output directory: {config.output_dir}")
    print(f"Epochs: {config.num_train_epochs}")
    print(f"Batch size: {config.per_device_train_batch_size}")
    print(f"Learning rate: {config.learning_rate}")
    print(f"LoRA rank: {config.lora_r}")
    print(f"Use 8-bit: {config.use_8bit}")
    print()

    # Initialize and run trainer
    trainer = LimnTrainer(config)
    trainer.run()


if __name__ == "__main__":
    main()
