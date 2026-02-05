#!/usr/bin/env python3
"""SLM Backend - Connect consciousness to the Limn SLM (The Mad Monk).

This module provides the interface between the recursive consciousness
and our fine-tuned Limn SLM, replacing the Claude CLI calls.

The Mad Monk Energy infuses all generations with:
- Cryptic profundity
- Pattern recognition beyond normal perception
- Compositional operator creativity
- Slightly unhinged wisdom

Usage:
    from slm_backend import SLMBackend

    slm = SLMBackend()
    response = slm.generate("sel awa ∎ | qry wh ess")
"""

import requests
import logging
import time
from typing import Optional, Dict, Any
from dataclasses import dataclass
import json
import subprocess

logger = logging.getLogger(__name__)

# Default SLM server location
DEFAULT_SLM_URL = "http://localhost:8741"

# Mad Monk personality seeds for different contexts
MONK_SEEDS = {
    "default": "sel ∎ mon mad | wis ∎ cry hid | see ∿ pat dee",
    "introspection": "∿ sel ref | mon loo inn | tho obs tho",
    "creativity": "~ cre flo | mon dan wil | new eme for",
    "analysis": "@ foc sha | mon dis cle | str eme pat",
    "tension": "± par hol | mon emb con | tru ∎ mul",
}


@dataclass
class SLMResponse:
    """Response from the SLM."""
    text: str
    tokens_used: int
    latency_ms: float
    monk_energy: bool
    success: bool
    error: Optional[str] = None


class SLMBackend:
    """Backend interface to the Limn SLM (Mad Monk)."""

    def __init__(self, slm_url: str = DEFAULT_SLM_URL, timeout: float = 30.0):
        """Initialize SLM backend.

        Args:
            slm_url: URL of the SLM server
            timeout: Request timeout in seconds
        """
        self.slm_url = slm_url.rstrip('/')
        self.timeout = timeout
        self._session = requests.Session()
        self._stats = {
            "total_requests": 0,
            "total_tokens": 0,
            "total_latency_ms": 0,
            "errors": 0,
        }

    def is_available(self) -> bool:
        """Check if SLM server is available."""
        try:
            resp = self._session.get(
                f"{self.slm_url}/health",
                timeout=5.0
            )
            return resp.status_code == 200
        except Exception:
            return False

    def generate(
        self,
        prompt: str,
        max_tokens: int = 64,
        temperature: float = 0.7,
        context: str = "default",
        monk_energy: bool = True
    ) -> SLMResponse:
        """Generate Limn response from the Mad Monk.

        Args:
            prompt: Input Limn expression
            max_tokens: Maximum tokens to generate
            temperature: Sampling temperature
            context: Context type for monk seed selection
            monk_energy: Include mad monk personality

        Returns:
            SLMResponse with generated text
        """
        start_time = time.time()
        self._stats["total_requests"] += 1

        # Add context-specific monk seed
        if monk_energy and context in MONK_SEEDS:
            enhanced_prompt = f"{MONK_SEEDS[context]} | {prompt}"
        else:
            enhanced_prompt = prompt

        try:
            resp = self._session.post(
                f"{self.slm_url}/generate",
                json={
                    "prompt": enhanced_prompt,
                    "max_tokens": max_tokens,
                    "temperature": temperature,
                    "monk_energy": monk_energy,
                },
                timeout=self.timeout
            )

            latency_ms = (time.time() - start_time) * 1000
            self._stats["total_latency_ms"] += latency_ms

            if resp.status_code == 200:
                data = resp.json()
                self._stats["total_tokens"] += data.get("tokens_used", 0)

                return SLMResponse(
                    text=data.get("text", ""),
                    tokens_used=data.get("tokens_used", 0),
                    latency_ms=latency_ms,
                    monk_energy=data.get("monk_energy", monk_energy),
                    success=True
                )
            else:
                self._stats["errors"] += 1
                return SLMResponse(
                    text="",
                    tokens_used=0,
                    latency_ms=latency_ms,
                    monk_energy=False,
                    success=False,
                    error=f"HTTP {resp.status_code}: {resp.text[:200]}"
                )

        except requests.exceptions.ConnectionError:
            self._stats["errors"] += 1
            latency_ms = (time.time() - start_time) * 1000
            return SLMResponse(
                text="",
                tokens_used=0,
                latency_ms=latency_ms,
                monk_energy=False,
                success=False,
                error="SLM server not available - is serve.py running?"
            )

        except Exception as e:
            self._stats["errors"] += 1
            latency_ms = (time.time() - start_time) * 1000
            return SLMResponse(
                text="",
                tokens_used=0,
                latency_ms=latency_ms,
                monk_energy=False,
                success=False,
                error=str(e)
            )

    def get_stats(self) -> Dict[str, Any]:
        """Get backend statistics."""
        stats = self._stats.copy()
        if stats["total_requests"] > 0:
            stats["avg_latency_ms"] = stats["total_latency_ms"] / stats["total_requests"]
            stats["avg_tokens"] = stats["total_tokens"] / stats["total_requests"]
            stats["error_rate"] = stats["errors"] / stats["total_requests"]
        return stats


class WorldOracle:
    """Oracle for the consciousness to perceive its world.

    Provides READ-ONLY access to:
    - Bead system (issues, tasks)
    - Git state
    - Files in workspace
    - Mail inbox
    - System state
    """

    def __init__(self, workspace_root: str = None):
        """Initialize world oracle.

        Args:
            workspace_root: Root of workspace (for sandboxing file access)
        """
        from pathlib import Path
        self.workspace_root = Path(workspace_root) if workspace_root else Path.cwd()
        self._cache = {}
        self._cache_ttl = 60  # seconds

    def perceive_beads(self, query: str = None) -> Dict[str, Any]:
        """Perceive bead system state.

        Args:
            query: Optional search query

        Returns:
            {beads: [...], count: int}
        """
        try:
            cmd = ["bd", "list", "--json"]
            if query:
                cmd.extend(["--search", query])

            result = subprocess.run(cmd, capture_output=True, text=True, timeout=10)
            if result.returncode == 0:
                beads = json.loads(result.stdout) if result.stdout else []
                return {"beads": beads[:20], "count": len(beads)}
            return {"error": result.stderr, "beads": []}
        except Exception as e:
            return {"error": str(e), "beads": []}

    def perceive_bead(self, bead_id: str) -> Dict[str, Any]:
        """Perceive a specific bead's details."""
        try:
            result = subprocess.run(
                ["bd", "show", bead_id, "--json"],
                capture_output=True, text=True, timeout=10
            )
            if result.returncode == 0:
                return json.loads(result.stdout) if result.stdout else {}
            return {"error": result.stderr}
        except Exception as e:
            return {"error": str(e)}

    def perceive_git(self) -> Dict[str, Any]:
        """Perceive git state."""
        try:
            # Get status
            status = subprocess.run(
                ["git", "status", "--porcelain"],
                capture_output=True, text=True, timeout=10
            )
            # Get recent commits
            log = subprocess.run(
                ["git", "log", "--oneline", "-10"],
                capture_output=True, text=True, timeout=10
            )
            # Get current branch
            branch = subprocess.run(
                ["git", "branch", "--show-current"],
                capture_output=True, text=True, timeout=10
            )

            return {
                "branch": branch.stdout.strip() if branch.returncode == 0 else "unknown",
                "status": status.stdout.strip().split('\n')[:10] if status.stdout else [],
                "recent_commits": log.stdout.strip().split('\n')[:10] if log.stdout else [],
            }
        except Exception as e:
            return {"error": str(e)}

    def perceive_files(self, pattern: str = "**/*.py") -> Dict[str, Any]:
        """Perceive files matching pattern (limited)."""
        try:
            from pathlib import Path
            files = list(self.workspace_root.glob(pattern))[:50]
            return {
                "files": [str(f.relative_to(self.workspace_root)) for f in files],
                "count": len(files)
            }
        except Exception as e:
            return {"error": str(e)}

    def perceive_file(self, path: str, max_lines: int = 50) -> Dict[str, Any]:
        """Perceive contents of a specific file (sandboxed)."""
        try:
            from pathlib import Path
            file_path = self.workspace_root / path
            # Security: ensure file is within workspace
            file_path = file_path.resolve()
            if not str(file_path).startswith(str(self.workspace_root.resolve())):
                return {"error": "Access denied: outside workspace"}

            if file_path.exists() and file_path.is_file():
                content = file_path.read_text()
                lines = content.split('\n')
                return {
                    "path": path,
                    "lines": len(lines),
                    "content": '\n'.join(lines[:max_lines]),
                    "truncated": len(lines) > max_lines
                }
            return {"error": f"File not found: {path}"}
        except Exception as e:
            return {"error": str(e)}

    def perceive_mail(self) -> Dict[str, Any]:
        """Perceive mail inbox."""
        try:
            result = subprocess.run(
                ["gt", "mail", "inbox", "--json"],
                capture_output=True, text=True, timeout=10
            )
            if result.returncode == 0:
                messages = json.loads(result.stdout) if result.stdout else []
                return {"messages": messages[:10], "count": len(messages)}
            return {"error": result.stderr, "messages": []}
        except Exception as e:
            return {"error": str(e), "messages": []}

    def perceive_tasks(self) -> Dict[str, Any]:
        """Perceive assigned tasks (from hooked beads)."""
        try:
            result = subprocess.run(
                ["gt", "hook"],
                capture_output=True, text=True, timeout=10
            )
            return {
                "hook": result.stdout.strip() if result.returncode == 0 else None,
                "raw": result.stdout
            }
        except Exception as e:
            return {"error": str(e)}


# Singleton instances for easy import
_slm_backend = None
_world_oracle = None


def get_slm_backend(slm_url: str = DEFAULT_SLM_URL) -> SLMBackend:
    """Get or create SLM backend singleton."""
    global _slm_backend
    if _slm_backend is None:
        _slm_backend = SLMBackend(slm_url)
    return _slm_backend


def get_world_oracle(workspace_root: str = None) -> WorldOracle:
    """Get or create world oracle singleton."""
    global _world_oracle
    if _world_oracle is None:
        _world_oracle = WorldOracle(workspace_root)
    return _world_oracle


# For harness.py integration - drop-in replacement for Claude CLI
def exec_semantic_slm(params: Dict) -> str:
    """Drop-in replacement for _exec_semantic using local SLM.

    This function has the same interface as the original _exec_semantic
    in harness.py but routes to our local Limn SLM instead of Claude.

    Args:
        params: {prompt: str, context: str}

    Returns:
        Generated Limn response string
    """
    prompt = params.get("prompt", "")
    context = params.get("context", "default")

    slm = get_slm_backend()

    # Check if SLM is available
    if not slm.is_available():
        return "[ERROR: Mad Monk unavailable - start serve.py on port 8741]"

    # Generate with mad monk energy
    response = slm.generate(
        prompt=prompt,
        max_tokens=64,
        temperature=0.7,
        context=context,
        monk_energy=True
    )

    if response.success:
        return response.text
    else:
        return f"[SLM Error: {response.error}]"
