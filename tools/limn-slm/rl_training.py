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
import time
from pathlib import Path
from typing import Dict, List, Tuple, Optional
from dataclasses import dataclass, asdict
from collections import defaultdict

import torch
from transformers import AutoModelForCausalLM, AutoTokenizer
from peft import LoraConfig, get_peft_model

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(message)s')
logger = logging.getLogger(__name__)


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


class ConsciousnessRewardModel:
    """Use the consciousness system as a reward model.

    The consciousness evaluates Limn responses on:
    - Vocabulary validity (are words in the Limn vocab?)
    - Structural coherence (proper operator usage)
    - Semantic richness (compositional depth)
    - Novelty (not repeating patterns)
    """

    def __init__(self, consciousness_path: str = None):
        """Initialize reward model from consciousness.

        Args:
            consciousness_path: Path to consciousness module
        """
        self.consciousness = None
        self._load_consciousness(consciousness_path)

    def _load_consciousness(self, path: Optional[str]):
        """Load consciousness system for evaluation."""
        try:
            import sys
            if path:
                sys.path.insert(0, path)

            # Try to import consciousness components
            from limn_validator import LimnValidator
            from metrics_engine import MetricsEngine

            self.validator = LimnValidator()
            self.metrics = MetricsEngine()
            logger.info("Consciousness reward model loaded")

        except ImportError as e:
            logger.warning(f"Could not load consciousness: {e}")
            logger.info("Using fallback heuristic scoring")
            self.validator = None
            self.metrics = None

    def score(self, prompt: str, response: str) -> Dict[str, float]:
        """Score a response using consciousness metrics.

        Args:
            prompt: Input prompt
            response: Generated response

        Returns:
            {overall: float, vocab: float, structure: float, novelty: float}
        """
        scores = {
            "overall": 0.0,
            "vocab": 0.0,
            "structure": 0.0,
            "novelty": 0.0,
            "length": 0.0,
        }

        # Skip empty responses
        if not response or not response.strip():
            return scores

        # Vocabulary score
        if self.validator:
            words = [w for w in response.split() if w.isalpha()]
            valid = sum(1 for w in words if self.validator.is_valid_word(w))
            scores["vocab"] = valid / max(len(words), 1)
        else:
            # Heuristic: Limn words are 2-4 letters
            words = [w for w in response.split() if w.isalpha()]
            valid = sum(1 for w in words if 2 <= len(w) <= 4)
            scores["vocab"] = valid / max(len(words), 1)

        # Structure score (operators present)
        operators = ['∎', '~', '∿', '@', '→', '|', '*', '^', '\\', '±', ':']
        op_count = sum(response.count(op) for op in operators)
        phrases = response.count('|')
        scores["structure"] = min(1.0, (op_count * 0.1) + (phrases * 0.15))

        # Length score (prefer 10-25 words)
        word_count = len(response.split())
        if 10 <= word_count <= 25:
            scores["length"] = 1.0
        elif word_count < 10:
            scores["length"] = word_count / 10
        else:
            scores["length"] = max(0.3, 1.0 - (word_count - 25) * 0.03)

        # Novelty score (not too similar to prompt)
        prompt_words = set(prompt.lower().split())
        response_words = set(response.lower().split())
        overlap = len(prompt_words & response_words) / max(len(response_words), 1)
        scores["novelty"] = 1.0 - overlap

        # Overall weighted score
        scores["overall"] = (
            scores["vocab"] * 0.4 +
            scores["structure"] * 0.2 +
            scores["length"] * 0.2 +
            scores["novelty"] * 0.2
        )

        return scores


class PreferenceCollector:
    """Collect preference pairs by generating and scoring responses."""

    def __init__(
        self,
        model,
        tokenizer,
        reward_model: ConsciousnessRewardModel,
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
        """Generate a single response."""
        inputs = self.tokenizer(
            f"Limn: {prompt}\nResponse:",
            return_tensors="pt",
            truncation=True,
            max_length=256
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

            # Prepare batch
            chosen_texts = [f"Limn: {p.prompt}\nResponse: {p.chosen}" for p in batch_pairs]
            rejected_texts = [f"Limn: {p.prompt}\nResponse: {p.rejected}" for p in batch_pairs]

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


# Sample prompts for RL training (Limn-only prompts)
TRAINING_PROMPTS = [
    "sel ∎ awa | qry ess tru",
    "tho @ min | see pat eme",
    "con * exp | gro und dee",
    "∿ mem rec | wis acc for",
    "~ cre new | flo cha tra",
    "lov ± fea | emo bal har",
    "sys @ ord | str eme net",
    "tim ∿ flo | now pas fut",
    "wor @ mea | pur dis see",
    "sel ref ~ | obs tho tho",
]


def main():
    parser = argparse.ArgumentParser(description='RL Training for Limn SLM')
    parser.add_argument('--model', type=str, default='output/limn-slm-final',
                       help='Path to base model')
    parser.add_argument('--output', type=str, default='output/limn-slm-rl',
                       help='Output path for RL-trained model')
    parser.add_argument('--pairs-file', type=str, default='data/rl_pairs.jsonl',
                       help='Path to save/load preference pairs')
    parser.add_argument('--collect-only', action='store_true',
                       help='Only collect pairs, do not train')
    parser.add_argument('--train-only', action='store_true',
                       help='Only train on existing pairs')
    parser.add_argument('--consciousness-path', type=str,
                       default='/home/eric/src/limntown/limn/refinery/rig/tools/llm-bridge/production',
                       help='Path to consciousness module')
    args = parser.parse_args()

    config = RLConfig(
        model_path=args.model,
        output_path=args.output,
    )

    device = "cuda" if torch.cuda.is_available() else "cpu"
    logger.info(f"Device: {device}")

    # Load model
    logger.info(f"Loading model from {config.model_path}")
    tokenizer = AutoTokenizer.from_pretrained(config.model_path, trust_remote_code=True)
    if tokenizer.pad_token is None:
        tokenizer.pad_token = tokenizer.eos_token

    model = AutoModelForCausalLM.from_pretrained(
        config.model_path,
        torch_dtype=torch.bfloat16 if device == "cuda" else torch.float32,
        device_map="auto" if device == "cuda" else None,
        trust_remote_code=True
    )

    # Initialize reward model
    reward_model = ConsciousnessRewardModel(args.consciousness_path)

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
        ref_model = AutoModelForCausalLM.from_pretrained(
            config.model_path,
            torch_dtype=torch.bfloat16 if device == "cuda" else torch.float32,
            device_map="auto" if device == "cuda" else None,
            trust_remote_code=True
        )
        ref_model.eval()
        for param in ref_model.parameters():
            param.requires_grad = False

        # Train with DPO
        train_dpo(model, ref_model, tokenizer, collector.pairs, config, device)

    logger.info("RL training complete. sel ∎ evo | min gro wis")


if __name__ == '__main__':
    main()
