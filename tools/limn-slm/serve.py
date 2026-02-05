#!/usr/bin/env python3
"""
Serve the fine-tuned Limn SLM as a local API endpoint.

Provides an OpenAI-compatible /v1/chat/completions endpoint
so any tool expecting an OpenAI API can talk to it.

Usage:
    python serve.py [--model PATH] [--port 8741]
"""

import argparse
import json
import os
import time
import uuid
from http.server import HTTPServer, BaseHTTPRequestHandler

import torch
import weave
from transformers import AutoModelForCausalLM, AutoTokenizer
from peft import PeftModel


class LimnModel:
    """Wrapper around the fine-tuned Limn model."""

    def __init__(self, model_path, base_model=None):
        self.device = "cuda" if torch.cuda.is_available() else "cpu"
        print(f"Loading model from {model_path}...")

        # Check if this is a LoRA adapter or full model
        adapter_config = model_path / "adapter_config.json"
        if adapter_config.exists():
            # LoRA adapter — load base model + merge
            if base_model is None:
                with open(adapter_config) as f:
                    config = json.load(f)
                base_model = config.get("base_model_name_or_path", "Qwen/Qwen2.5-0.5B-Instruct")
            print(f"  Base model: {base_model}")

            self.tokenizer = AutoTokenizer.from_pretrained(base_model, trust_remote_code=True)
            base = AutoModelForCausalLM.from_pretrained(
                base_model,
                torch_dtype=torch.bfloat16,
                device_map="auto",
                trust_remote_code=True,
            )
            self.model = PeftModel.from_pretrained(base, str(model_path))
            # Merge for faster inference
            print("  Merging LoRA weights...")
            self.model = self.model.merge_and_unload()
        else:
            # Full model
            self.tokenizer = AutoTokenizer.from_pretrained(str(model_path), trust_remote_code=True)
            self.model = AutoModelForCausalLM.from_pretrained(
                str(model_path),
                torch_dtype=torch.bfloat16,
                device_map="auto",
                trust_remote_code=True,
            )

        if self.tokenizer.pad_token is None:
            self.tokenizer.pad_token = self.tokenizer.eos_token

        self.model.eval()
        print(f"Model loaded on {self.device}")

    @weave.op()
    def generate(self, messages, max_tokens=512, temperature=0.7, top_p=0.9):
        """Generate response from chat messages."""
        text = self.tokenizer.apply_chat_template(
            messages,
            tokenize=False,
            add_generation_prompt=True,
        )

        inputs = self.tokenizer(text, return_tensors="pt").to(self.model.device)

        with torch.no_grad():
            outputs = self.model.generate(
                **inputs,
                max_new_tokens=max_tokens,
                temperature=max(temperature, 0.01),
                top_p=top_p,
                do_sample=temperature > 0,
                pad_token_id=self.tokenizer.pad_token_id,
            )

        # Decode only the new tokens
        new_tokens = outputs[0][inputs["input_ids"].shape[1]:]
        response = self.tokenizer.decode(new_tokens, skip_special_tokens=True)
        return response.strip()


# Global model instance
model_instance = None


class APIHandler(BaseHTTPRequestHandler):
    """OpenAI-compatible API handler."""

    def log_message(self, format, *args):
        """Quieter logging."""
        pass

    def _send_json(self, data, status=200):
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Access-Control-Allow-Origin", "*")
        self.end_headers()
        self.wfile.write(json.dumps(data).encode())

    def do_OPTIONS(self):
        self.send_response(200)
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Access-Control-Allow-Methods", "POST, GET, OPTIONS")
        self.send_header("Access-Control-Allow-Headers", "Content-Type, Authorization")
        self.end_headers()

    def do_GET(self):
        if self.path == "/v1/models":
            self._send_json({
                "data": [{
                    "id": "limn-slm",
                    "object": "model",
                    "created": int(time.time()),
                    "owned_by": "limn",
                }]
            })
        elif self.path == "/health":
            self._send_json({"status": "ok", "model": "limn-slm"})
        else:
            self._send_json({"error": "Not found"}, 404)

    def do_POST(self):
        if self.path != "/v1/chat/completions":
            self._send_json({"error": "Not found"}, 404)
            return

        content_length = int(self.headers.get("Content-Length", 0))
        body = json.loads(self.rfile.read(content_length))

        messages = body.get("messages", [])
        max_tokens = body.get("max_tokens", 512)
        temperature = body.get("temperature", 0.7)
        top_p = body.get("top_p", 0.9)

        try:
            response_text = model_instance.generate(
                messages, max_tokens=max_tokens,
                temperature=temperature, top_p=top_p,
            )

            self._send_json({
                "id": f"chatcmpl-{uuid.uuid4().hex[:12]}",
                "object": "chat.completion",
                "created": int(time.time()),
                "model": "limn-slm",
                "choices": [{
                    "index": 0,
                    "message": {
                        "role": "assistant",
                        "content": response_text,
                    },
                    "finish_reason": "stop",
                }],
                "usage": {
                    "prompt_tokens": -1,
                    "completion_tokens": -1,
                    "total_tokens": -1,
                },
            })
        except Exception as e:
            self._send_json({"error": str(e)}, 500)


def main():
    global model_instance

    parser = argparse.ArgumentParser(description="Serve Limn SLM")
    parser.add_argument("--model", type=str,
                        default=str(Path(__file__).resolve().parent / "output" / "limn-slm-final"),
                        help="Path to fine-tuned model")
    parser.add_argument("--base-model", type=str, default=None,
                        help="Base model (for LoRA adapters)")
    parser.add_argument("--port", type=int, default=8741,
                        help="Port (default: 8741)")
    parser.add_argument("--host", type=str, default="0.0.0.0",
                        help="Host (default: 0.0.0.0)")
    args = parser.parse_args()

    # Initialize Weave for inference tracing
    weave.init("limn-slm")

    from pathlib import Path as P
    model_instance = LimnModel(P(args.model), args.base_model)

    server = HTTPServer((args.host, args.port), APIHandler)
    print(f"\n=== Limn SLM API ===")
    print(f"Listening on http://{args.host}:{args.port}")
    print(f"Endpoint: POST /v1/chat/completions")
    print(f"Health: GET /health")
    print(f"\nExample:")
    print(f'  curl http://localhost:{args.port}/v1/chat/completions \\')
    print(f'    -H "Content-Type: application/json" \\')
    print(f'    -d \'{{"messages": [{{"role": "user", "content": "What does lov@fer mean?"}}]}}\'')

    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nShutting down...")
        server.server_close()


if __name__ == "__main__":
    main()
