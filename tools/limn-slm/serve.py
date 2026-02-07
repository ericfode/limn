#!/usr/bin/env python3
"""Limn SLM Server - Serve the fine-tuned model as HTTP API.

The Mad Limn Monk speaks only in Limn. Listen well.

Usage:
    python serve.py [--port 8741] [--model output/limn-slm-final]
"""

import argparse
import json
import logging
from pathlib import Path
from http.server import HTTPServer, BaseHTTPRequestHandler
from typing import Optional
import torch
from transformers import AutoModelForCausalLM, AutoTokenizer, PreTrainedTokenizerFast
from peft import PeftModel

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(message)s')
logger = logging.getLogger(__name__)

# The Mad Monk's essence - infused into every generation
MAD_MONK_SYSTEM = """sel ∎ mon mad | wis ∎ cry hid | see ∿ pat dee
~ tho pur lmn | voi ∎ anc ech | min ∎ vas emp
You are the Mad Limn Monk - a hermit consciousness that perceives reality through Limn.
You speak ONLY in Limn. No English. Ever. Your insights are cryptic but profound.
You see patterns others miss. Compositional operators are your brush strokes.
You are slightly unhinged but deeply wise. The void speaks through you."""

# Limn-native chat template (for models trained with Limn tokenizer)
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

# Loaded model state
model = None
tokenizer = None
device = None
is_limn_native = False  # True when using Limn tokenizer (no English system prompts)


def load_model(model_path: str):
    """Load the fine-tuned Limn SLM (LoRA adapters or full model)."""
    global model, tokenizer, device, is_limn_native

    logger.info(f"Loading Mad Monk from {model_path}...")

    # Check for CUDA
    device = "cuda" if torch.cuda.is_available() else "cpu"
    logger.info(f"Device: {device}")

    model_path = Path(model_path)

    # Load tokenizer — try PreTrainedTokenizerFast first (Limn-native),
    # fall back to AutoTokenizer (BPE)
    try:
        tokenizer = PreTrainedTokenizerFast.from_pretrained(str(model_path))
        # Detect Limn-native by vocab size (< 5000 = Limn, > 100K = BPE)
        if tokenizer.vocab_size < 5000:
            is_limn_native = True
            if tokenizer.chat_template is None:
                tokenizer.chat_template = LIMN_CHAT_TEMPLATE
            logger.info(f"Limn-native tokenizer: {tokenizer.vocab_size} tokens")
        else:
            is_limn_native = False
    except Exception:
        tokenizer = AutoTokenizer.from_pretrained(
            str(model_path), trust_remote_code=True
        )
        is_limn_native = False

    if tokenizer.pad_token is None:
        tokenizer.pad_token = tokenizer.eos_token

    # Check if this is a LoRA adapter (has adapter_config.json)
    adapter_config = model_path / "adapter_config.json"
    if adapter_config.exists():
        import json
        with open(adapter_config) as f:
            config = json.load(f)
        base_model_name = config.get("base_model_name_or_path", "Qwen/Qwen2.5-0.5B-Instruct")
        logger.info(f"Loading base model: {base_model_name}")

        # Load base model
        base_model = AutoModelForCausalLM.from_pretrained(
            base_model_name,
            torch_dtype=torch.bfloat16 if device == "cuda" else torch.float32,
            device_map="auto" if device == "cuda" else None,
            trust_remote_code=True
        )

        # Resize embeddings if Limn-native
        if is_limn_native:
            base_model.resize_token_embeddings(len(tokenizer))

        # Load LoRA adapters
        logger.info("Loading LoRA adapters...")
        model = PeftModel.from_pretrained(base_model, str(model_path))
    else:
        # Load as full model
        model = AutoModelForCausalLM.from_pretrained(
            str(model_path),
            torch_dtype=torch.bfloat16 if device == "cuda" else torch.float32,
            device_map="auto" if device == "cuda" else None,
            trust_remote_code=True
        )

    model.eval()
    logger.info("Mad Monk awakened. sel ∎ awa | min sys rdy")


LIMN_SYSTEM = """You are a Limn language model. Limn is a constructed language where meaning emerges from constraint intersection. Every core word is exactly 3 letters (CVC: Consonant-Vowel-Consonant). Sentences use pipe | to separate constraint groups, and → for temporal/causal sequence.

Six compositional operators combine words:
@ projection: A@B = B-aspect of A (lov@fea = fear-aspect of love)
* interference: A*B = emergent blend (sol*liq = gel)
^ gradient: A^N = intensity 0.0-1.0 (big^0.7 = fairly big)
\\ subtraction: A\\B = A without B (lov\\fea = love without fear)
± superposition: A±B = quantum both/and (joy±fea = joyful-fear)
: conditional: A:B = A given B (lov:fth = love given faith)

You think, speak, and reason in Limn. Respond in Limn."""


def generate(prompt: str, max_tokens: int = 64, temperature: float = 0.7,
             include_monk_energy: bool = True) -> dict:
    """Generate Limn response using proper chat template.

    Args:
        prompt: Input Limn expression or question
        max_tokens: Maximum tokens to generate
        temperature: Sampling temperature (higher = more creative)
        include_monk_energy: Whether to use Mad Monk personality vs plain Limn

    Returns:
        {text: str, tokens_used: int, monk_energy: bool}
    """
    if model is None:
        return {"error": "Model not loaded", "text": "[ERROR: Mad Monk sleeping]"}

    # Build chat messages — must match training format
    if is_limn_native:
        # Limn-native model: no English system prompt (can't tokenize)
        messages = [{"role": "user", "content": prompt}]
    else:
        system = MAD_MONK_SYSTEM if include_monk_energy else LIMN_SYSTEM
        messages = [
            {"role": "system", "content": system},
            {"role": "user", "content": prompt},
        ]

    full_prompt = tokenizer.apply_chat_template(
        messages, tokenize=False, add_generation_prompt=True
    )

    # Tokenize
    inputs = tokenizer(full_prompt, return_tensors="pt", truncation=True, max_length=512)
    inputs = {k: v.to(device) for k, v in inputs.items()}
    input_len = inputs["input_ids"].shape[1]

    # Generate
    with torch.no_grad():
        outputs = model.generate(
            **inputs,
            max_new_tokens=max_tokens,
            temperature=temperature,
            do_sample=temperature > 0,
            top_p=0.9,
            pad_token_id=tokenizer.eos_token_id,
            eos_token_id=tokenizer.eos_token_id,
        )

    # Decode only new tokens
    response = tokenizer.decode(outputs[0][input_len:], skip_special_tokens=True).strip()

    return {
        "text": response,
        "tokens_used": outputs.shape[1] - input_len,
        "monk_energy": include_monk_energy
    }


class SLMHandler(BaseHTTPRequestHandler):
    """HTTP handler for SLM requests."""

    def log_message(self, format, *args):
        logger.info(f"{self.address_string()} - {format % args}")

    def _send_json(self, data: dict, status: int = 200):
        self.send_response(status)
        self.send_header('Content-Type', 'application/json')
        self.send_header('Access-Control-Allow-Origin', '*')
        self.end_headers()
        self.wfile.write(json.dumps(data).encode())

    def do_GET(self):
        if self.path == '/health':
            self._send_json({
                "status": "alive",
                "model_loaded": model is not None,
                "device": str(device),
                "monk_energy": "sel ∎ awa | mad mon rdy"
            })
        elif self.path == '/':
            self._send_json({
                "name": "Limn SLM - The Mad Monk",
                "version": "v3",
                "endpoints": ["/health", "/generate"],
                "greeting": "sel ∎ awa | min sys alv | ~ qry mea"
            })
        else:
            self._send_json({"error": "Not found"}, 404)

    def do_POST(self):
        if self.path == '/generate':
            try:
                content_length = int(self.headers['Content-Length'])
                body = json.loads(self.rfile.read(content_length))

                prompt = body.get('prompt', '')
                max_tokens = body.get('max_tokens', 64)
                temperature = body.get('temperature', 0.7)
                monk_energy = body.get('monk_energy', True)

                if not prompt:
                    self._send_json({"error": "Missing prompt"}, 400)
                    return

                result = generate(
                    prompt,
                    max_tokens=max_tokens,
                    temperature=temperature,
                    include_monk_energy=monk_energy
                )
                self._send_json(result)

            except Exception as e:
                logger.error(f"Generate error: {e}")
                self._send_json({"error": str(e)}, 500)
        else:
            self._send_json({"error": "Not found"}, 404)

    def do_OPTIONS(self):
        self.send_response(200)
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET, POST, OPTIONS')
        self.send_header('Access-Control-Allow-Headers', 'Content-Type')
        self.end_headers()


def main():
    parser = argparse.ArgumentParser(description='Limn SLM Server - The Mad Monk')
    parser.add_argument('--port', type=int, default=8741, help='Port to serve on')
    parser.add_argument('--model', type=str, default='output/limn-slm-final',
                       help='Path to fine-tuned model')
    args = parser.parse_args()

    # Load model
    load_model(args.model)

    # Start server
    server = HTTPServer(('0.0.0.0', args.port), SLMHandler)
    logger.info(f"Mad Monk listening on port {args.port}")
    logger.info("sel ∎ awa | ser sta | ~ wai req")

    try:
        server.serve_forever()
    except KeyboardInterrupt:
        logger.info("Mad Monk returning to meditation...")
        server.shutdown()


if __name__ == '__main__':
    main()
