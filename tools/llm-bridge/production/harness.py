#!/usr/bin/env python3
"""
Production LMN Oracle Harness
==============================

The definitive implementation combining best features from all prototypes.

Features:
- JSON protocol (structured, extensible)
- Real Claude API integration
- Comprehensive oracle types
- Production error handling
- Performance optimization
- Response caching
- Async support ready

Author: Rex (Engineer)
Date: 2026-02-01
"""

import subprocess
import json
import time
import re
import os
import sqlite3
from pathlib import Path
from typing import Dict, Any, List, Optional, Union
from dataclasses import dataclass, asdict
from enum import Enum
from datetime import datetime
import requests


class OracleType(Enum):
    """All supported oracle types."""
    # Semantic
    SEMANTIC = "Semantic"

    # Arithmetic
    ARITH = "Arith"

    # Filesystem
    FILE_READ = "FileRead"
    FILE_WRITE = "FileWrite"
    FILE_EXISTS = "FileExists"

    # Temporal
    TIME_NOW = "TimeNow"
    TIME_AT = "TimeAt"
    TIME_DELTA = "TimeDelta"

    # Database
    DB_QUERY = "DbQuery"
    DB_WRITE = "DbWrite"

    # Network
    HTTP_GET = "HttpGet"
    HTTP_POST = "HttpPost"

    # Memory
    MEMORY_STORE = "MemoryStore"
    MEMORY_RETRIEVE = "MemoryRetrieve"


@dataclass
class OracleRequest:
    """Parsed oracle request."""
    type: OracleType
    params: Dict[str, Any]


@dataclass
class OracleResponse:
    """Oracle execution response."""
    success: bool
    result: Any
    error: Optional[str] = None
    duration_ms: float = 0
    cached: bool = False


class ProductionHarness:
    """
    Production-ready oracle harness.

    The Conscious layer - interfaces with all reality.
    """

    def __init__(
        self,
        bend_binary: str = "bend",
        enable_real_llm: bool = False,
        anthropic_api_key: Optional[str] = None,
        cache_dir: Optional[Path] = None
    ):
        """Initialize production harness.

        Args:
            bend_binary: Path to bend executable
            enable_real_llm: Use real Claude API
            anthropic_api_key: Claude API key (or from ANTHROPIC_API_KEY env)
            cache_dir: Directory for persistent cache
        """
        self.bend_binary = bend_binary
        self.enable_real_llm = enable_real_llm
        self.api_key = anthropic_api_key or os.environ.get("ANTHROPIC_API_KEY")

        # Cache setup
        self.cache_dir = cache_dir or Path("/tmp/lmn-oracle-cache")
        self.cache_dir.mkdir(exist_ok=True)
        self.cache_db = self.cache_dir / "oracle_cache.db"
        self._init_cache()

        # Memory store
        self.memory = {}

        # Performance stats
        self.stats = {
            "total_oracles": 0,
            "cache_hits": 0,
            "total_time_ms": 0
        }

    def _init_cache(self):
        """Initialize persistent cache database."""
        conn = sqlite3.connect(self.cache_db)
        conn.execute("""
            CREATE TABLE IF NOT EXISTS oracle_cache (
                key TEXT PRIMARY KEY,
                value TEXT,
                timestamp REAL
            )
        """)
        conn.commit()
        conn.close()

    def _cache_get(self, key: str) -> Optional[Any]:
        """Get from cache."""
        conn = sqlite3.connect(self.cache_db)
        cursor = conn.execute(
            "SELECT value FROM oracle_cache WHERE key = ?",
            (key,)
        )
        row = cursor.fetchone()
        conn.close()

        if row:
            return json.loads(row[0])
        return None

    def _cache_set(self, key: str, value: Any):
        """Set in cache."""
        conn = sqlite3.connect(self.cache_db)
        conn.execute(
            "INSERT OR REPLACE INTO oracle_cache (key, value, timestamp) VALUES (?, ?, ?)",
            (key, json.dumps(value), time.time())
        )
        conn.commit()
        conn.close()

    # =========================================================================
    # Bend Execution
    # =========================================================================

    def run_bend(self, bend_file: Path) -> Dict[str, Any]:
        """Execute Bend program (pure computation)."""
        try:
            result = subprocess.run(
                [self.bend_binary, "run-rs", str(bend_file)],
                capture_output=True,
                text=True,
                timeout=30
            )

            return {
                "success": result.returncode == 0,
                "stdout": result.stdout,
                "stderr": result.stderr
            }
        except subprocess.TimeoutExpired:
            return {"success": False, "error": "Execution timeout"}
        except Exception as e:
            return {"success": False, "error": str(e)}

    # =========================================================================
    # Oracle Parsing
    # =========================================================================

    def parse_oracles(self, output: str) -> List[OracleRequest]:
        """Parse oracle requests from Bend output."""
        oracles = []

        # Patterns for each oracle type
        patterns = {
            # Semantic
            OracleType.SEMANTIC: r'Oracle/Semantic[^{]*\{\s*prompt:\s*"([^"]+)",\s*context:\s*"([^"]+)"\s*\}',

            # Arithmetic
            OracleType.ARITH: r'Oracle/Arith[^{]*\{\s*op:\s*"([^"]+)",\s*a:\s*(\d+),\s*b:\s*(\d+)\s*\}',

            # Filesystem
            OracleType.FILE_READ: r'Oracle/FileRead[^{]*\{\s*path:\s*"([^"]+)"\s*\}',
            OracleType.FILE_WRITE: r'Oracle/FileWrite[^{]*\{\s*path:\s*"([^"]+)",\s*content:\s*"([^"]+)"\s*\}',
            OracleType.FILE_EXISTS: r'Oracle/FileExists[^{]*\{\s*path:\s*"([^"]+)"\s*\}',

            # Temporal
            OracleType.TIME_NOW: r'Oracle/TimeNow(?:\s|,|\))',
            OracleType.TIME_AT: r'Oracle/TimeAt[^{]*\{\s*timestamp:\s*(\d+)\s*\}',
            OracleType.TIME_DELTA: r'Oracle/TimeDelta[^{]*\{\s*seconds:\s*(\d+)\s*\}',

            # Database
            OracleType.DB_QUERY: r'Oracle/DbQuery[^{]*\{\s*sql:\s*"([^"]+)",\s*db:\s*"([^"]+)"\s*\}',
            OracleType.DB_WRITE: r'Oracle/DbWrite[^{]*\{\s*sql:\s*"([^"]+)",\s*db:\s*"([^"]+)"\s*\}',

            # Network
            OracleType.HTTP_GET: r'Oracle/HttpGet[^{]*\{\s*url:\s*"([^"]+)"\s*\}',
            OracleType.HTTP_POST: r'Oracle/HttpPost[^{]*\{\s*url:\s*"([^"]+)",\s*data:\s*"([^"]+)"\s*\}',

            # Memory
            OracleType.MEMORY_STORE: r'Oracle/MemoryStore[^{]*\{\s*key:\s*"([^"]+)",\s*value:\s*"([^"]+)"\s*\}',
            OracleType.MEMORY_RETRIEVE: r'Oracle/MemoryRetrieve[^{]*\{\s*key:\s*"([^"]+)"\s*\}',
        }

        for oracle_type, pattern in patterns.items():
            for match in re.finditer(pattern, output):
                params = self._extract_params(oracle_type, match)
                oracles.append(OracleRequest(type=oracle_type, params=params))

        return oracles

    def _extract_params(self, oracle_type: OracleType, match) -> Dict[str, Any]:
        """Extract parameters from regex match."""
        if oracle_type == OracleType.SEMANTIC:
            return {"prompt": match.group(1), "context": match.group(2)}
        elif oracle_type == OracleType.ARITH:
            return {"op": match.group(1), "a": int(match.group(2)), "b": int(match.group(3))}
        elif oracle_type == OracleType.FILE_READ:
            return {"path": match.group(1)}
        elif oracle_type == OracleType.FILE_WRITE:
            return {"path": match.group(1), "content": match.group(2)}
        elif oracle_type == OracleType.FILE_EXISTS:
            return {"path": match.group(1)}
        elif oracle_type == OracleType.TIME_NOW:
            return {}
        elif oracle_type == OracleType.TIME_AT:
            return {"timestamp": int(match.group(1))}
        elif oracle_type == OracleType.TIME_DELTA:
            return {"seconds": int(match.group(1))}
        elif oracle_type in [OracleType.DB_QUERY, OracleType.DB_WRITE]:
            return {"sql": match.group(1), "db": match.group(2)}
        elif oracle_type == OracleType.HTTP_GET:
            return {"url": match.group(1)}
        elif oracle_type == OracleType.HTTP_POST:
            return {"url": match.group(1), "data": match.group(2)}
        elif oracle_type == OracleType.MEMORY_STORE:
            return {"key": match.group(1), "value": match.group(2)}
        elif oracle_type == OracleType.MEMORY_RETRIEVE:
            return {"key": match.group(1)}
        return {}

    # =========================================================================
    # Oracle Execution
    # =========================================================================

    def execute_oracle(self, oracle: OracleRequest) -> OracleResponse:
        """Execute an oracle request (side effect happens here)."""
        start = time.time()
        self.stats["total_oracles"] += 1

        # Check cache
        cache_key = f"{oracle.type.value}:{json.dumps(oracle.params, sort_keys=True)}"
        cached_result = self._cache_get(cache_key)
        if cached_result is not None:
            duration = (time.time() - start) * 1000
            self.stats["cache_hits"] += 1
            return OracleResponse(
                success=True,
                result=cached_result,
                duration_ms=duration,
                cached=True
            )

        try:
            # Dispatch to handler
            handlers = {
                OracleType.SEMANTIC: self._exec_semantic,
                OracleType.ARITH: self._exec_arith,
                OracleType.FILE_READ: self._exec_file_read,
                OracleType.FILE_WRITE: self._exec_file_write,
                OracleType.FILE_EXISTS: self._exec_file_exists,
                OracleType.TIME_NOW: self._exec_time_now,
                OracleType.TIME_AT: self._exec_time_at,
                OracleType.TIME_DELTA: self._exec_time_delta,
                OracleType.DB_QUERY: self._exec_db_query,
                OracleType.DB_WRITE: self._exec_db_write,
                OracleType.HTTP_GET: self._exec_http_get,
                OracleType.HTTP_POST: self._exec_http_post,
                OracleType.MEMORY_STORE: self._exec_memory_store,
                OracleType.MEMORY_RETRIEVE: self._exec_memory_retrieve,
            }

            handler = handlers.get(oracle.type)
            if not handler:
                raise ValueError(f"Unknown oracle type: {oracle.type}")

            result = handler(oracle.params)
            duration = (time.time() - start) * 1000
            self.stats["total_time_ms"] += duration

            # Cache result
            self._cache_set(cache_key, result)

            return OracleResponse(
                success=True,
                result=result,
                duration_ms=duration
            )

        except Exception as e:
            duration = (time.time() - start) * 1000
            return OracleResponse(
                success=False,
                result=None,
                error=str(e),
                duration_ms=duration
            )

    # =========================================================================
    # Oracle Handlers (The Conscious Actions)
    # =========================================================================

    def _exec_semantic(self, params: Dict) -> str:
        """Semantic oracle - LLM reasoning (~ operator)."""
        prompt = params["prompt"]
        context = params.get("context", "general")

        if self.enable_real_llm and self.api_key:
            # Real Claude API call
            try:
                import anthropic
                client = anthropic.Anthropic(api_key=self.api_key)

                message = client.messages.create(
                    model="claude-sonnet-4-20250514",
                    max_tokens=1024,
                    messages=[{
                        "role": "user",
                        "content": f"Context: {context}\n\n{prompt}"
                    }]
                )

                return message.content[0].text
            except Exception as e:
                return f"[LLM Error: {e}]"
        else:
            # Mock LLM
            return self._mock_llm(prompt, context)

    def _mock_llm(self, prompt: str, context: str) -> str:
        """Mock LLM for testing."""
        prompt_lower = prompt.lower()

        # Arithmetic
        if any(op in prompt_lower for op in ["add", "plus", "+"]):
            nums = re.findall(r'\d+', prompt)
            if len(nums) >= 2:
                return str(int(nums[0]) + int(nums[1]))

        if any(op in prompt_lower for op in ["multiply", "times", "*"]):
            nums = re.findall(r'\d+', prompt)
            if len(nums) >= 2:
                return str(int(nums[0]) * int(nums[1]))

        # Limn translation
        if "cod flo log" in prompt:
            return "code flows clearly"

        # General
        return f"[Mock LLM: {prompt[:50]}...]"

    def _exec_arith(self, params: Dict) -> Union[int, float]:
        """Arithmetic oracle - fast computation."""
        op = params["op"]
        a = params["a"]
        b = params["b"]

        ops = {
            "add": lambda x, y: x + y,
            "sub": lambda x, y: x - y,
            "mul": lambda x, y: x * y,
            "div": lambda x, y: x / y if y != 0 else float('inf')
        }

        return ops[op](a, b)

    def _exec_file_read(self, params: Dict) -> str:
        """File read oracle (∎ ground truth)."""
        path = Path(params["path"])
        if not path.exists():
            raise FileNotFoundError(f"File not found: {path}")
        return path.read_text()

    def _exec_file_write(self, params: Dict) -> str:
        """File write oracle (∎ ground truth)."""
        path = Path(params["path"])
        content = params["content"]
        path.write_text(content)
        return f"Wrote {len(content)} bytes to {path}"

    def _exec_file_exists(self, params: Dict) -> bool:
        """File exists oracle (∎ ground truth)."""
        return Path(params["path"]).exists()

    def _exec_time_now(self, params: Dict) -> Dict[str, Any]:
        """Time now oracle (∿ temporal)."""
        now = time.time()
        return {
            "timestamp": int(now),
            "iso": datetime.fromtimestamp(now).isoformat(),
            "unix": now
        }

    def _exec_time_at(self, params: Dict) -> str:
        """Time at oracle (∿ temporal)."""
        timestamp = params["timestamp"]
        return datetime.fromtimestamp(timestamp).isoformat()

    def _exec_time_delta(self, params: Dict) -> str:
        """Time delta oracle (∿ temporal)."""
        seconds = params["seconds"]
        time.sleep(seconds)
        return f"Waited {seconds} seconds"

    def _exec_db_query(self, params: Dict) -> List[Dict]:
        """Database query oracle (∎ persisted state)."""
        # Simple SQLite implementation
        db_path = Path(params["db"])
        sql = params["sql"]

        conn = sqlite3.connect(db_path)
        conn.row_factory = sqlite3.Row
        cursor = conn.execute(sql)
        rows = [dict(row) for row in cursor.fetchall()]
        conn.close()

        return rows

    def _exec_db_write(self, params: Dict) -> str:
        """Database write oracle (∎ persisted state)."""
        db_path = Path(params["db"])
        sql = params["sql"]

        conn = sqlite3.connect(db_path)
        conn.execute(sql)
        conn.commit()
        affected = conn.total_changes
        conn.close()

        return f"Affected {affected} rows"

    def _exec_http_get(self, params: Dict) -> str:
        """HTTP GET oracle (∎ external data)."""
        url = params["url"]
        response = requests.get(url, timeout=10)
        response.raise_for_status()
        return response.text

    def _exec_http_post(self, params: Dict) -> str:
        """HTTP POST oracle (∎ external data)."""
        url = params["url"]
        data = json.loads(params["data"])
        response = requests.post(url, json=data, timeout=10)
        response.raise_for_status()
        return response.text

    def _exec_memory_store(self, params: Dict) -> str:
        """Memory store oracle (∿ context)."""
        key = params["key"]
        value = params["value"]
        self.memory[key] = value
        return f"Stored: {key}"

    def _exec_memory_retrieve(self, params: Dict) -> Optional[str]:
        """Memory retrieve oracle (∿ context)."""
        key = params["key"]
        return self.memory.get(key)

    # =========================================================================
    # Main Execution
    # =========================================================================

    def execute(self, bend_file: Path, verbose: bool = True) -> Dict[str, Any]:
        """Execute Bend program with full oracle support."""
        if verbose:
            print("=" * 70)
            print("Production LMN Oracle Harness")
            print("=" * 70)
            print(f"\n[Conscious] Executing: {bend_file.name}")
            print("[Conscious] Running subconscious (Bend/HVM)...\n")

        # Run pure Bend program
        result = self.run_bend(bend_file)

        if not result["success"]:
            if verbose:
                print(f"[Conscious] ✗ Execution failed: {result.get('error', 'Unknown')}")
            return result

        if verbose:
            print(f"[Conscious] ✓ Subconscious completed\n")

        # Parse oracles
        oracles = self.parse_oracles(result["stdout"])

        if not oracles:
            if verbose:
                print("[Conscious] No oracles (pure deterministic)")
            return result

        if verbose:
            print(f"[Conscious] Found {len(oracles)} oracle(s)\n")

        # Execute each oracle
        responses = []
        for i, oracle in enumerate(oracles, 1):
            if verbose:
                print(f"[Conscious] Oracle {i}/{len(oracles)}: {oracle.type.value}")
                print(f"[Conscious]   Params: {oracle.params}")

            response = self.execute_oracle(oracle)

            if verbose:
                if response.success:
                    cache_marker = " (cached)" if response.cached else ""
                    print(f"[Conscious]   ✓ Result: {response.result}{cache_marker}")
                    print(f"[Conscious]   ⏱ {response.duration_ms:.2f}ms")
                else:
                    print(f"[Conscious]   ✗ Error: {response.error}")
                print()

            responses.append(response)

        # Stats
        if verbose:
            print("=" * 70)
            print("Statistics")
            print("=" * 70)
            print(f"Total oracles: {self.stats['total_oracles']}")
            print(f"Cache hits: {self.stats['cache_hits']}")
            print(f"Cache rate: {self.stats['cache_hits']/self.stats['total_oracles']*100:.1f}%")
            print(f"Total time: {self.stats['total_time_ms']:.2f}ms")
            print("=" * 70)

        result["oracles"] = [
            {
                "type": r.success,
                "result": r.result,
                "error": r.error,
                "duration_ms": r.duration_ms,
                "cached": r.cached
            }
            for r in responses
        ]

        return result


def main():
    """Main entry point."""
    script_dir = Path(__file__).parent

    # Setup
    bend_binary = script_dir.parent.parent / "lmn-bend" / "bend"
    if not bend_binary.exists():
        bend_binary = "bend"

    harness = ProductionHarness(
        bend_binary=str(bend_binary),
        enable_real_llm=False  # Set to True to use real Claude API
    )

    # Run example
    oracle_file = script_dir / "oracle.bend"
    if not oracle_file.exists():
        print(f"Error: {oracle_file} not found")
        return 1

    result = harness.execute(oracle_file)

    return 0 if result["success"] else 1


if __name__ == "__main__":
    exit(main())
