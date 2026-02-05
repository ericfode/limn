#!/usr/bin/env python3
"""RL Training Loop for Limn SLM - Self-Improving Mad Monk.

Uses the recursive consciousness system as a reward model:
1. Generate responses from SLM
2. Consciousness evaluates quality (MetricsEngine scores)
3. Collect preference pairs (good/bad responses)
4. Train with DPO (Direct Preference Optimization)

This creates a self-improving loop where the consciousness
shapes the SLM's behavior through its evaluations.

Usage:
    python rl_training.py --model output/limn-slm-v2 --output output/limn-slm-rl

Architecture:
    ┌─────────────┐     ┌─────────────────┐     ┌─────────────┐
    │ SLM Model   │────▶│ Consciousness   │────▶│ Preference  │
    │ (generates) │     │ (evaluates)     │     │ Dataset     │
    └─────────────┘     └─────────────────┘     └──────┬──────┘
          ▲                                            │
          │                    DPO Training            │
          └────────────────────────────────────────────┘
"""

import argparse
import json
import logging
import random
import subprocess
import sys
import time
from pathlib import Path
from typing import Dict, List, Tuple, Optional, Set
from dataclasses import dataclass, asdict
from collections import defaultdict

import torch
from transformers import AutoModelForCausalLM, AutoTokenizer
from peft import LoraConfig, get_peft_model, PeftModel

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(message)s')
logger = logging.getLogger(__name__)

LIMN_SYSTEM = """You are a Limn language model. Limn is a constructed language where meaning emerges from constraint intersection. Every core word is exactly 3 letters (CVC: Consonant-Vowel-Consonant). Sentences use pipe | to separate constraint groups, and → for temporal/causal sequence.

Six compositional operators combine words:
@ projection: A@B = B-aspect of A (lov@fea = fear-aspect of love)
* interference: A*B = emergent blend (sol*liq = gel)
^ gradient: A^N = intensity 0.0-1.0 (big^0.7 = fairly big)
\\ subtraction: A\\B = A without B (lov\\fea = love without fear)
± superposition: A±B = quantum both/and (joy±fea = joyful-fear)
: conditional: A:B = A given B (lov:fth = love given faith)

You think, speak, and reason in Limn. Respond in Limn."""


@dataclass
class PreferencePair:
    """A preference pair for DPO training."""
    prompt: str
    chosen: str  # Better response (higher quality score)
    rejected: str  # Worse response (lower quality score)
    chosen_score: float
    rejected_score: float
    context: str
    timestamp: float


@dataclass
class RLConfig:
    """Configuration for RL training."""
    model_path: str
    output_path: str
    learning_rate: float = 1e-5
    beta: float = 0.1  # DPO temperature
    batch_size: int = 4
    max_length: int = 256
    num_epochs: int = 3
    min_score_delta: float = 0.2  # Minimum difference for preference pair
    samples_per_prompt: int = 4  # Generate N responses per prompt
    prompts_per_batch: int = 10


VOCAB_DB = Path("/home/eric/src/limntown/limn/refinery/rig/data/vocabulary")

ENGLISH_STOPWORDS = {
    "the", "a", "an", "is", "are", "was", "were", "be", "been", "being",
    "have", "has", "had", "will", "would", "should", "could", "can", "may",
    "might", "must", "to", "of", "in", "on", "at", "by", "for", "with",
    "from", "about", "into", "through", "this", "that", "these", "those",
    "it", "its", "not", "but", "if", "or", "because", "as", "until",
    "while", "however", "therefore", "although", "also", "just", "than",
}


def load_vocab_from_dolt() -> Set[str]:
    """Load Limn vocabulary from Dolt DB."""
    try:
        result = subprocess.run(
            ["dolt", "sql", "-q", "SELECT word FROM words", "-r", "json"],
            cwd=str(VOCAB_DB), capture_output=True, text=True
        )
        if result.returncode == 0:
            data = json.loads(result.stdout)
            rows = data.get("rows", data) if isinstance(data, dict) else data
            vocab = {r["word"] for r in rows}
            logger.info(f"Loaded {len(vocab)} words from Dolt")
            return vocab
    except Exception as e:
        logger.warning(f"Could not load Dolt vocab: {e}")
    return set()


class LimnRewardModel:
    """Score Limn responses using vocabulary DB and structural analysis.

    Evaluates:
    - Vocabulary validity (words in Dolt DB)
    - English contamination penalty
    - Structural coherence (operator usage)
    - Compositional depth
    - Appropriate length
    """

    def __init__(self):
        self.vocab = load_vocab_from_dolt()
        if not self.vocab:
            logger.warning("No vocab loaded — falling back to heuristic scoring")

    def score(self, prompt: str, response: str) -> Dict[str, float]:
        """Score a response.

        Returns:
            {overall, vocab, english_penalty, structure, length, novelty}
        """
        scores = {
            "overall": 0.0,
            "vocab": 0.0,
            "english_penalty": 0.0,
            "structure": 0.0,
            "length": 0.0,
            "novelty": 0.0,
        }

        if not response or not response.strip():
            return scores

        # Extract alphabetic words
        words = [w.lower().strip(".,!?;:()") for w in response.split()
                 if any(c.isalpha() for c in w)]

        if not words:
            scores["structure"] = 0.3  # Has operators but no words
            return scores

        # Vocabulary score — check against real DB
        if self.vocab:
            valid = sum(1 for w in words if w in self.vocab)
            scores["vocab"] = valid / len(words)
        else:
            # Heuristic: Limn words are 2-4 letters CVC
            valid = sum(1 for w in words if 2 <= len(w) <= 4)
            scores["vocab"] = valid / len(words)

        # English contamination penalty
        english_count = sum(1 for w in words if w in ENGLISH_STOPWORDS)
        scores["english_penalty"] = 1.0 - (english_count / len(words))

        # Structure score (operators + pipes)
        operators = ['@', '*', '^', '\\', '±', ':']
        markers = ['∎', '~', '∿', '→', '|']
        op_count = sum(response.count(op) for op in operators)
        mark_count = sum(response.count(m) for m in markers)
        scores["structure"] = min(1.0, (op_count * 0.15) + (mark_count * 0.1))

        # Length score (prefer 5-30 words for Limn)
        word_count = len(words)
        if 5 <= word_count <= 30:
            scores["length"] = 1.0
        elif word_count < 5:
            scores["length"] = word_count / 5
        else:
            scores["length"] = max(0.2, 1.0 - (word_count - 30) * 0.02)

        # Novelty (not just echoing prompt)
        prompt_words = set(prompt.lower().split())
        response_words = set(words)
        overlap = len(prompt_words & response_words) / max(len(response_words), 1)
        scores["novelty"] = 1.0 - overlap

        # Overall — vocab and no-english are critical
        scores["overall"] = (
            scores["vocab"] * 0.35 +
            scores["english_penalty"] * 0.25 +
            scores["structure"] * 0.15 +
            scores["length"] * 0.10 +
            scores["novelty"] * 0.15
        )

        return scores


class PreferenceCollector:
    """Collect preference pairs by generating and scoring responses."""

    def __init__(
        self,
        model,
        tokenizer,
        reward_model: LimnRewardModel,
        device: str = "cuda"
    ):
        self.model = model
        self.tokenizer = tokenizer
        self.reward_model = reward_model
        self.device = device
        self.pairs: List[PreferencePair] = []

    def generate_response(
        self,
        prompt: str,
        max_tokens: int = 64,
        temperature: float = 0.8
    ) -> str:
        """Generate a single response using chat template."""
        messages = [
            {"role": "system", "content": LIMN_SYSTEM},
            {"role": "user", "content": prompt},
        ]
        full_prompt = self.tokenizer.apply_chat_template(
            messages, tokenize=False, add_generation_prompt=True
        )

        inputs = self.tokenizer(
            full_prompt, return_tensors="pt", truncation=True, max_length=256
        )
        inputs = {k: v.to(self.device) for k, v in inputs.items()}
        input_len = inputs["input_ids"].shape[1]

        with torch.no_grad():
            outputs = self.model.generate(
                **inputs,
                max_new_tokens=max_tokens,
                temperature=temperature,
                do_sample=True,
                top_p=0.9,
                pad_token_id=self.tokenizer.eos_token_id,
            )

        return self.tokenizer.decode(outputs[0][input_len:], skip_special_tokens=True).strip()

    def collect_pairs(
        self,
        prompts: List[str],
        samples_per_prompt: int = 4,
        min_score_delta: float = 0.2
    ) -> List[PreferencePair]:
        """Collect preference pairs for a batch of prompts.

        Args:
            prompts: List of prompts
            samples_per_prompt: Generate N responses per prompt
            min_score_delta: Minimum score difference for valid pair

        Returns:
            List of preference pairs
        """
        new_pairs = []

        for prompt in prompts:
            # Generate multiple responses
            responses = []
            for _ in range(samples_per_prompt):
                temp = random.uniform(0.5, 1.0)  # Vary temperature for diversity
                response = self.generate_response(prompt, temperature=temp)
                score = self.reward_model.score(prompt, response)
                responses.append((response, score))

            # Sort by overall score
            responses.sort(key=lambda x: x[1]["overall"], reverse=True)

            # Create pairs from best vs worst
            for i in range(len(responses) // 2):
                best = responses[i]
                worst = responses[-(i + 1)]

                delta = best[1]["overall"] - worst[1]["overall"]
                if delta >= min_score_delta:
                    pair = PreferencePair(
                        prompt=prompt,
                        chosen=best[0],
                        rejected=worst[0],
                        chosen_score=best[1]["overall"],
                        rejected_score=worst[1]["overall"],
                        context="rl_collection",
                        timestamp=time.time()
                    )
                    new_pairs.append(pair)
                    logger.info(f"  Pair: {pair.chosen_score:.3f} vs {pair.rejected_score:.3f}")

        self.pairs.extend(new_pairs)
        return new_pairs

    def save_pairs(self, path: str):
        """Save collected pairs to JSONL."""
        with open(path, 'w') as f:
            for pair in self.pairs:
                f.write(json.dumps(asdict(pair)) + '\n')
        logger.info(f"Saved {len(self.pairs)} pairs to {path}")

    def load_pairs(self, path: str):
        """Load existing pairs from JSONL."""
        if Path(path).exists():
            with open(path, 'r') as f:
                for line in f:
                    data = json.loads(line)
                    self.pairs.append(PreferencePair(**data))
            logger.info(f"Loaded {len(self.pairs)} existing pairs")


def compute_dpo_loss(
    model,
    ref_model,
    batch: Dict[str, torch.Tensor],
    beta: float = 0.1
) -> torch.Tensor:
    """Compute DPO loss for a batch.

    DPO loss: -log(σ(β * (log π(chosen|x)/π_ref(chosen|x) - log π(rejected|x)/π_ref(rejected|x))))

    Args:
        model: Policy model being trained
        ref_model: Reference model (frozen)
        batch: {chosen_ids, rejected_ids, chosen_mask, rejected_mask}
        beta: Temperature parameter

    Returns:
        DPO loss tensor
    """
    # Get policy log probs
    with torch.no_grad():
        ref_chosen_logits = ref_model(
            input_ids=batch["chosen_ids"],
            attention_mask=batch["chosen_mask"]
        ).logits
        ref_rejected_logits = ref_model(
            input_ids=batch["rejected_ids"],
            attention_mask=batch["rejected_mask"]
        ).logits

    policy_chosen_logits = model(
        input_ids=batch["chosen_ids"],
        attention_mask=batch["chosen_mask"]
    ).logits
    policy_rejected_logits = model(
        input_ids=batch["rejected_ids"],
        attention_mask=batch["rejected_mask"]
    ).logits

    # Compute log probs
    def get_log_probs(logits, ids, mask):
        # Shift for causal LM
        shift_logits = logits[..., :-1, :].contiguous()
        shift_labels = ids[..., 1:].contiguous()
        shift_mask = mask[..., 1:].contiguous()

        log_probs = torch.nn.functional.log_softmax(shift_logits, dim=-1)
        token_log_probs = torch.gather(log_probs, 2, shift_labels.unsqueeze(-1)).squeeze(-1)
        masked_log_probs = token_log_probs * shift_mask
        return masked_log_probs.sum(-1) / shift_mask.sum(-1).clamp(min=1)

    policy_chosen_logprobs = get_log_probs(policy_chosen_logits, batch["chosen_ids"], batch["chosen_mask"])
    policy_rejected_logprobs = get_log_probs(policy_rejected_logits, batch["rejected_ids"], batch["rejected_mask"])
    ref_chosen_logprobs = get_log_probs(ref_chosen_logits, batch["chosen_ids"], batch["chosen_mask"])
    ref_rejected_logprobs = get_log_probs(ref_rejected_logits, batch["rejected_ids"], batch["rejected_mask"])

    # DPO loss
    policy_ratio = policy_chosen_logprobs - policy_rejected_logprobs
    ref_ratio = ref_chosen_logprobs - ref_rejected_logprobs

    losses = -torch.nn.functional.logsigmoid(beta * (policy_ratio - ref_ratio))
    return losses.mean()


def train_dpo(
    model,
    ref_model,
    tokenizer,
    pairs: List[PreferencePair],
    config: RLConfig,
    device: str = "cuda"
):
    """Train model using DPO on collected preference pairs.

    Args:
        model: Model to train
        ref_model: Reference model (frozen copy)
        tokenizer: Tokenizer
        pairs: Preference pairs
        config: Training config
        device: Device to use
    """
    logger.info(f"Starting DPO training with {len(pairs)} pairs")

    optimizer = torch.optim.AdamW(model.parameters(), lr=config.learning_rate)

    for epoch in range(config.num_epochs):
        random.shuffle(pairs)
        total_loss = 0
        num_batches = 0

        for i in range(0, len(pairs), config.batch_size):
            batch_pairs = pairs[i:i + config.batch_size]

            # Prepare batch using chat template
            def make_chat_text(prompt, response):
                msgs = [
                    {"role": "system", "content": LIMN_SYSTEM},
                    {"role": "user", "content": prompt},
                    {"role": "assistant", "content": response},
                ]
                return tokenizer.apply_chat_template(msgs, tokenize=False)

            chosen_texts = [make_chat_text(p.prompt, p.chosen) for p in batch_pairs]
            rejected_texts = [make_chat_text(p.prompt, p.rejected) for p in batch_pairs]

            chosen_enc = tokenizer(chosen_texts, padding=True, truncation=True,
                                   max_length=config.max_length, return_tensors="pt")
            rejected_enc = tokenizer(rejected_texts, padding=True, truncation=True,
                                     max_length=config.max_length, return_tensors="pt")

            batch = {
                "chosen_ids": chosen_enc["input_ids"].to(device),
                "chosen_mask": chosen_enc["attention_mask"].to(device),
                "rejected_ids": rejected_enc["input_ids"].to(device),
                "rejected_mask": rejected_enc["attention_mask"].to(device),
            }

            # Compute loss
            loss = compute_dpo_loss(model, ref_model, batch, config.beta)

            # Backward
            optimizer.zero_grad()
            loss.backward()
            torch.nn.utils.clip_grad_norm_(model.parameters(), 1.0)
            optimizer.step()

            total_loss += loss.item()
            num_batches += 1

            if num_batches % 10 == 0:
                logger.info(f"  Epoch {epoch+1}, Batch {num_batches}: loss={loss.item():.4f}")

        avg_loss = total_loss / max(num_batches, 1)
        logger.info(f"Epoch {epoch+1}/{config.num_epochs}: avg_loss={avg_loss:.4f}")

    # Save model
    model.save_pretrained(config.output_path)
    tokenizer.save_pretrained(config.output_path)
    logger.info(f"Model saved to {config.output_path}")


# Diverse prompts for RL training — mix of Limn, English, and composition tasks
TRAINING_PROMPTS = [
    # Pure Limn prompts
    "sel ∎ awa | qry ess tru",
    "tho @ min | see pat eme",
    "lov ± fea | emo bal har",
    "tim ∿ flo | now pas fut",
    "wor @ mea | pur dis see",
    # Composition tasks
    "Express 'peaceful anger' using Limn operators",
    "What is joy\\fea?",
    "Compose: knowledge given time",
    "Express hope dying using operators",
    "What does sel@tim mean?",
    # Translation tasks
    "Translate to Limn: the sun rises over water",
    "Translate to Limn: fear becomes courage through action",
    "Translate to Limn: memory fades but love remains",
    # Creative tasks
    "Write a Limn haiku about consciousness",
    "Express the feeling of rain in Limn",
    "Write a Limn mantra for learning",
    # Vocabulary tasks
    "What is the Limn word for 'hope'?",
    "What domain is 'lov' in?",
    "Explain what @ does in Limn",
    "What is the difference between * and ± ?",
]


def load_lora_model(model_path: str, device: str):
    """Load model with LoRA adapter support."""
    model_path = Path(model_path)
    tokenizer = AutoTokenizer.from_pretrained(str(model_path), trust_remote_code=True)
    if tokenizer.pad_token is None:
        tokenizer.pad_token = tokenizer.eos_token

    adapter_config = model_path / "adapter_config.json"
    if adapter_config.exists():
        with open(adapter_config) as f:
            config = json.load(f)
        base_model_name = config.get("base_model_name_or_path", "Qwen/Qwen2.5-0.5B-Instruct")
        logger.info(f"Loading base model: {base_model_name}")
        base_model = AutoModelForCausalLM.from_pretrained(
            base_model_name,
            torch_dtype=torch.bfloat16 if device == "cuda" else torch.float32,
            device_map="auto" if device == "cuda" else None,
            trust_remote_code=True,
        )
        logger.info("Loading LoRA adapters...")
        model = PeftModel.from_pretrained(base_model, str(model_path))
    else:
        model = AutoModelForCausalLM.from_pretrained(
            str(model_path),
            torch_dtype=torch.bfloat16 if device == "cuda" else torch.float32,
            device_map="auto" if device == "cuda" else None,
            trust_remote_code=True,
        )

    return model, tokenizer


def main():
    parser = argparse.ArgumentParser(description='RL Training for Limn SLM')
    parser.add_argument('--model', type=str, default='output/limn-slm-v3/limn-slm-final',
                       help='Path to model (supports LoRA adapters)')
    parser.add_argument('--output', type=str, default='output/limn-slm-rl',
                       help='Output path for RL-trained model')
    parser.add_argument('--pairs-file', type=str, default='data/rl_pairs.jsonl',
                       help='Path to save/load preference pairs')
    parser.add_argument('--collect-only', action='store_true',
                       help='Only collect pairs, do not train')
    parser.add_argument('--train-only', action='store_true',
                       help='Only train on existing pairs')
    args = parser.parse_args()

    config = RLConfig(
        model_path=args.model,
        output_path=args.output,
    )

    device = "cuda" if torch.cuda.is_available() else "cpu"
    logger.info(f"Device: {device}")

    # Load model (with LoRA support)
    model, tokenizer = load_lora_model(config.model_path, device)

    # Initialize reward model (uses Dolt vocab DB)
    reward_model = LimnRewardModel()

    # Initialize collector
    collector = PreferenceCollector(model, tokenizer, reward_model, device)
    collector.load_pairs(args.pairs_file)

    if not args.train_only:
        # Collect preference pairs
        logger.info("Collecting preference pairs...")
        new_pairs = collector.collect_pairs(
            TRAINING_PROMPTS,
            samples_per_prompt=config.samples_per_prompt,
            min_score_delta=config.min_score_delta
        )
        logger.info(f"Collected {len(new_pairs)} new pairs, total: {len(collector.pairs)}")
        collector.save_pairs(args.pairs_file)

    if not args.collect_only and len(collector.pairs) > 0:
        # Create reference model (frozen copy)
        logger.info("Creating reference model...")
        ref_model, _ = load_lora_model(config.model_path, device)
        ref_model.eval()
        for param in ref_model.parameters():
            param.requires_grad = False

        # Train with DPO
        train_dpo(model, ref_model, tokenizer, collector.pairs, config, device)

    logger.info("RL training complete. sel ∎ evo | min gro wis")


if __name__ == '__main__':
    main()
