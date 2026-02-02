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
from concurrent.futures import ThreadPoolExecutor, as_completed
import requests
import hashlib
import hmac
import shutil
import psutil
import signal
import configparser


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

    # Context manipulation
    CTX_REDUCE = "CtxReduce"
    CTX_MERGE = "CtxMerge"
    CTX_FILTER = "CtxFilter"
    CTX_AGGREGATE = "CtxAggregate"
    CTX_COMPRESS = "CtxCompress"

    # Model generation
    MODEL_DERIVE = "ModelDerive"
    MODEL_TRANSFORM = "ModelTransform"
    MODEL_GENERATE = "ModelGenerate"

    # Vocabulary query
    VOC_QUERY_DOMAIN = "VocQueryDomain"
    VOC_QUERY_MEANING = "VocQueryMeaning"
    VOC_EXPAND = "VocExpand"

    # Process
    PROCESS_SPAWN = "ProcessSpawn"
    PROCESS_KILL = "ProcessKill"
    PROCESS_STATUS = "ProcessStatus"

    # System
    SYSTEM_CPU = "SystemCPU"
    SYSTEM_MEMORY = "SystemMemory"
    SYSTEM_DISK = "SystemDisk"

    # Crypto
    CRYPTO_HASH = "CryptoHash"
    CRYPTO_SIGN = "CryptoSign"
    CRYPTO_VERIFY = "CryptoVerify"

    # ML
    ML_EMBED = "MLEmbed"
    ML_CLASSIFY = "MLClassify"
    ML_PREDICT = "MLPredict"

    # Audio/Video
    AUDIO_INFO = "AudioInfo"
    VIDEO_INFO = "VideoInfo"
    AUDIO_TRANSCODE = "AudioTranscode"
    VIDEO_TRANSCODE = "VideoTranscode"

    # Environment
    ENV_GET = "EnvGet"
    ENV_SET = "EnvSet"
    CONFIG_READ = "ConfigRead"
    CONFIG_WRITE = "ConfigWrite"

    # Git
    GIT_COMMIT = "GitCommit"
    GIT_DIFF = "GitDiff"
    GIT_LOG = "GitLog"
    GIT_STATUS = "GitStatus"

    # Docker
    DOCKER_RUN = "DockerRun"
    DOCKER_STOP = "DockerStop"
    DOCKER_STATUS = "DockerStatus"
    DOCKER_LOGS = "DockerLogs"


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
        cache_dir: Optional[Path] = None,
        max_concurrency: int = 4
    ):
        """Initialize production harness.

        Args:
            bend_binary: Path to bend executable
            enable_real_llm: Use real Claude API
            anthropic_api_key: Claude API key (or from ANTHROPIC_API_KEY env)
            cache_dir: Directory for persistent cache
            max_concurrency: Maximum concurrent oracle executions (default: 4)
        """
        self.bend_binary = bend_binary
        self.enable_real_llm = enable_real_llm
        self.api_key = anthropic_api_key or os.environ.get("ANTHROPIC_API_KEY")
        self.max_concurrency = max_concurrency

        # Cache setup
        self.cache_dir = cache_dir or Path("/tmp/lmn-oracle-cache")
        self.cache_dir.mkdir(exist_ok=True)
        self.cache_db = self.cache_dir / "oracle_cache.db"
        self._init_cache()

        # Memory store
        self.memory = {}

        # Context engine
        try:
            from context_engine import ContextEngine
            self.context_engine = ContextEngine()
        except ImportError:
            self.context_engine = None

        # Model engine
        try:
            from model_engine import ModelEngine
            self.model_engine = ModelEngine()
        except ImportError:
            self.model_engine = None

        # Vocabulary engine
        try:
            from vocab_engine import VocabularyEngine
            self.vocab_engine = VocabularyEngine()
        except ImportError:
            self.vocab_engine = None

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

        # Patterns for each oracle type (both structured and /tag format)
        patterns = {
            # Semantic (both formats)
            OracleType.SEMANTIC: [
                r'Oracle/Semantic[^{]*\{\s*prompt:\s*"([^"]+)",\s*context:\s*"([^"]+)"\s*\}',
                r'Oracle/Semantic/tag\s+"([^"]+)"(?:\s+"([^"]+)")?',
            ],

            # Arithmetic
            OracleType.ARITH: [
                r'Oracle/Arith[^{]*\{\s*op:\s*"([^"]+)",\s*a:\s*(\d+),\s*b:\s*(\d+)\s*\}',
                r'Oracle/Arith/tag\s+"([^"]+)"\s+(\d+)\s+(\d+)',
            ],

            # Filesystem
            OracleType.FILE_READ: [
                r'Oracle/FileRead[^{]*\{\s*path:\s*"([^"]+)"\s*\}',
                r'Oracle/FileRead/tag\s+"([^"]+)"',
            ],
            OracleType.FILE_WRITE: [
                r'Oracle/FileWrite[^{]*\{\s*path:\s*"([^"]+)",\s*content:\s*"([^"]+)"\s*\}',
                r'Oracle/FileWrite/tag\s+"([^"]+)"\s+"([^"]+)"',
            ],
            OracleType.FILE_EXISTS: [
                r'Oracle/FileExists[^{]*\{\s*path:\s*"([^"]+)"\s*\}',
                r'Oracle/FileExists/tag\s+"([^"]+)"',
            ],

            # Temporal
            OracleType.TIME_NOW: [
                r'Oracle/TimeNow(?:\s|,|\))',
            ],
            OracleType.TIME_AT: [
                r'Oracle/TimeAt[^{]*\{\s*timestamp:\s*(\d+)\s*\}',
                r'Oracle/TimeAt/tag\s+(\d+)',
            ],
            OracleType.TIME_DELTA: [
                r'Oracle/TimeDelta[^{]*\{\s*seconds:\s*(\d+)\s*\}',
                r'Oracle/TimeDelta/tag\s+(\d+)',
            ],

            # Database
            OracleType.DB_QUERY: [
                r'Oracle/DbQuery[^{]*\{\s*sql:\s*"([^"]+)",\s*db:\s*"([^"]+)"\s*\}',
                r'Oracle/DbQuery/tag\s+"([^"]+)"\s+"([^"]+)"',
            ],
            OracleType.DB_WRITE: [
                r'Oracle/DbWrite[^{]*\{\s*sql:\s*"([^"]+)",\s*db:\s*"([^"]+)"\s*\}',
                r'Oracle/DbWrite/tag\s+"([^"]+)"\s+"([^"]+)"',
            ],

            # Network
            OracleType.HTTP_GET: [
                r'Oracle/HttpGet[^{]*\{\s*url:\s*"([^"]+)"\s*\}',
                r'Oracle/HttpGet/tag\s+"([^"]+)"',
            ],
            OracleType.HTTP_POST: [
                r'Oracle/HttpPost[^{]*\{\s*url:\s*"([^"]+)",\s*data:\s*"([^"]+)"\s*\}',
                r'Oracle/HttpPost/tag\s+"([^"]+)"\s+"([^"]+)"',
            ],

            # Memory
<<<<<<< HEAD
            OracleType.MEMORY_STORE: [
                r'Oracle/MemoryStore[^{]*\{\s*key:\s*"([^"]+)",\s*value:\s*"([^"]+)"\s*\}',
                r'Oracle/MemoryStore/tag\s+"([^"]+)"\s+"([^"]+)"',
            ],
            OracleType.MEMORY_RETRIEVE: [
                r'Oracle/MemoryRetrieve[^{]*\{\s*key:\s*"([^"]+)"\s*\}',
                r'Oracle/MemoryRetrieve/tag\s+"([^"]+)"',
            ],

            # Context
            OracleType.CTX_COMPRESS: [
                r'Oracle/CtxCompress/tag\s+(\d+)',
            ],

            # Model
            OracleType.MODEL_DERIVE: [
                r'Oracle/ModelDerive/tag\s+"([^"]+)"\s+"([^"]+)"',
            ],
            OracleType.MODEL_TRANSFORM: [
                r'Oracle/ModelTransform/tag\s+"([^"]+)"\s+"([^"]+)"',
            ],

            # Vocabulary
            OracleType.VOC_QUERY_DOMAIN: [
                r'Oracle/VocQueryDomain/tag\s+"([^"]+)"',
            ],
            OracleType.VOC_QUERY_MEANING: [
                r'Oracle/VocQueryMeaning/tag\s+"([^"]+)"',
            ],
            OracleType.VOC_EXPAND: [
                r'Oracle/VocExpand/tag\s+"([^"]+)"\s+\[([^\]]+)\]',
            ],
=======
            OracleType.MEMORY_STORE: r'Oracle/MemoryStore[^{]*\{\s*key:\s*"([^"]+)",\s*value:\s*"([^"]+)"\s*\}',
            OracleType.MEMORY_RETRIEVE: r'Oracle/MemoryRetrieve[^{]*\{\s*key:\s*"([^"]+)"\s*\}',

            # Process
            OracleType.PROCESS_SPAWN: r'Oracle/ProcessSpawn[^{]*\{\s*command:\s*"([^"]+)",\s*args:\s*"([^"]+)"\s*\}',
            OracleType.PROCESS_KILL: r'Oracle/ProcessKill[^{]*\{\s*pid:\s*(\d+)\s*\}',
            OracleType.PROCESS_STATUS: r'Oracle/ProcessStatus[^{]*\{\s*pid:\s*(\d+)\s*\}',

            # System
            OracleType.SYSTEM_CPU: r'Oracle/SystemCPU(?:\s|,|\))',
            OracleType.SYSTEM_MEMORY: r'Oracle/SystemMemory(?:\s|,|\))',
            OracleType.SYSTEM_DISK: r'Oracle/SystemDisk[^{]*\{\s*path:\s*"([^"]+)"\s*\}',

            # Crypto
            OracleType.CRYPTO_HASH: r'Oracle/CryptoHash[^{]*\{\s*algorithm:\s*"([^"]+)",\s*data:\s*"([^"]+)"\s*\}',
            OracleType.CRYPTO_SIGN: r'Oracle/CryptoSign[^{]*\{\s*algorithm:\s*"([^"]+)",\s*data:\s*"([^"]+)",\s*key:\s*"([^"]+)"\s*\}',
            OracleType.CRYPTO_VERIFY: r'Oracle/CryptoVerify[^{]*\{\s*algorithm:\s*"([^"]+)",\s*data:\s*"([^"]+)",\s*signature:\s*"([^"]+)",\s*key:\s*"([^"]+)"\s*\}',

            # ML
            OracleType.ML_EMBED: r'Oracle/MLEmbed[^{]*\{\s*text:\s*"([^"]+)",\s*model:\s*"([^"]+)"\s*\}',
            OracleType.ML_CLASSIFY: r'Oracle/MLClassify[^{]*\{\s*text:\s*"([^"]+)",\s*categories:\s*"([^"]+)"\s*\}',
            OracleType.ML_PREDICT: r'Oracle/MLPredict[^{]*\{\s*input:\s*"([^"]+)",\s*model:\s*"([^"]+)"\s*\}',

            # Audio/Video
            OracleType.AUDIO_INFO: r'Oracle/AudioInfo[^{]*\{\s*path:\s*"([^"]+)"\s*\}',
            OracleType.VIDEO_INFO: r'Oracle/VideoInfo[^{]*\{\s*path:\s*"([^"]+)"\s*\}',
            OracleType.AUDIO_TRANSCODE: r'Oracle/AudioTranscode[^{]*\{\s*path:\s*"([^"]+)",\s*format:\s*"([^"]+)"\s*\}',
            OracleType.VIDEO_TRANSCODE: r'Oracle/VideoTranscode[^{]*\{\s*path:\s*"([^"]+)",\s*format:\s*"([^"]+)"\s*\}',

            # Environment
            OracleType.ENV_GET: r'Oracle/EnvGet[^{]*\{\s*key:\s*"([^"]+)"\s*\}',
            OracleType.ENV_SET: r'Oracle/EnvSet[^{]*\{\s*key:\s*"([^"]+)",\s*value:\s*"([^"]+)"\s*\}',
            OracleType.CONFIG_READ: r'Oracle/ConfigRead[^{]*\{\s*path:\s*"([^"]+)",\s*key:\s*"([^"]+)"\s*\}',
            OracleType.CONFIG_WRITE: r'Oracle/ConfigWrite[^{]*\{\s*path:\s*"([^"]+)",\s*key:\s*"([^"]+)",\s*value:\s*"([^"]+)"\s*\}',

            # Git
            OracleType.GIT_COMMIT: r'Oracle/GitCommit[^{]*\{\s*message:\s*"([^"]+)",\s*files:\s*"([^"]+)"\s*\}',
            OracleType.GIT_DIFF: r'Oracle/GitDiff[^{]*\{\s*ref1:\s*"([^"]+)",\s*ref2:\s*"([^"]+)"\s*\}',
            OracleType.GIT_LOG: r'Oracle/GitLog[^{]*\{\s*count:\s*(\d+)\s*\}',
            OracleType.GIT_STATUS: r'Oracle/GitStatus(?:\s|,|\))',

            # Docker
            OracleType.DOCKER_RUN: r'Oracle/DockerRun[^{]*\{\s*image:\s*"([^"]+)",\s*command:\s*"([^"]+)"\s*\}',
            OracleType.DOCKER_STOP: r'Oracle/DockerStop[^{]*\{\s*container:\s*"([^"]+)"\s*\}',
            OracleType.DOCKER_STATUS: r'Oracle/DockerStatus[^{]*\{\s*container:\s*"([^"]+)"\s*\}',
            OracleType.DOCKER_LOGS: r'Oracle/DockerLogs[^{]*\{\s*container:\s*"([^"]+)"\s*\}',
>>>>>>> 997cf49 (feat: extend oracle system with 36 new oracle types (limn-w8zy))
        }

        for oracle_type, pattern_list in patterns.items():
            for pattern in pattern_list:
                for match in re.finditer(pattern, output):
                    params = self._extract_params(oracle_type, match)
                    oracles.append(OracleRequest(type=oracle_type, params=params))

        return oracles

    def _extract_params(self, oracle_type: OracleType, match) -> Dict[str, Any]:
        """Extract parameters from regex match."""
        if oracle_type == OracleType.SEMANTIC:
            context = match.group(2) if match.lastindex >= 2 and match.group(2) else "general"
            return {"prompt": match.group(1), "context": context}
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
<<<<<<< HEAD
        elif oracle_type == OracleType.CTX_COMPRESS:
            return {"target_size": int(match.group(1))}
        elif oracle_type == OracleType.MODEL_DERIVE:
            return {"source_state": match.group(1), "model_type": match.group(2)}
        elif oracle_type == OracleType.MODEL_TRANSFORM:
            return {"source_state": match.group(1), "transformation": match.group(2)}
        elif oracle_type == OracleType.VOC_QUERY_DOMAIN:
            return {"domain": match.group(1)}
        elif oracle_type == OracleType.VOC_QUERY_MEANING:
            return {"meaning": match.group(1)}
        elif oracle_type == OracleType.VOC_EXPAND:
            concepts = [c.strip().strip('"') for c in match.group(2).split(',')]
            return {"domain": match.group(1), "concepts": concepts}
=======
        elif oracle_type == OracleType.PROCESS_SPAWN:
            return {"command": match.group(1), "args": match.group(2)}
        elif oracle_type == OracleType.PROCESS_KILL:
            return {"pid": int(match.group(1))}
        elif oracle_type == OracleType.PROCESS_STATUS:
            return {"pid": int(match.group(1))}
        elif oracle_type in [OracleType.SYSTEM_CPU, OracleType.SYSTEM_MEMORY]:
            return {}
        elif oracle_type == OracleType.SYSTEM_DISK:
            return {"path": match.group(1)}
        elif oracle_type == OracleType.CRYPTO_HASH:
            return {"algorithm": match.group(1), "data": match.group(2)}
        elif oracle_type == OracleType.CRYPTO_SIGN:
            return {"algorithm": match.group(1), "data": match.group(2), "key": match.group(3)}
        elif oracle_type == OracleType.CRYPTO_VERIFY:
            return {"algorithm": match.group(1), "data": match.group(2), "signature": match.group(3), "key": match.group(4)}
        elif oracle_type in [OracleType.ML_EMBED, OracleType.ML_CLASSIFY, OracleType.ML_PREDICT]:
            return {"text" if oracle_type != OracleType.ML_PREDICT else "input": match.group(1), "model" if oracle_type != OracleType.ML_CLASSIFY else "categories": match.group(2)}
        elif oracle_type in [OracleType.AUDIO_INFO, OracleType.VIDEO_INFO]:
            return {"path": match.group(1)}
        elif oracle_type in [OracleType.AUDIO_TRANSCODE, OracleType.VIDEO_TRANSCODE]:
            return {"path": match.group(1), "format": match.group(2)}
        elif oracle_type == OracleType.ENV_GET:
            return {"key": match.group(1)}
        elif oracle_type == OracleType.ENV_SET:
            return {"key": match.group(1), "value": match.group(2)}
        elif oracle_type == OracleType.CONFIG_READ:
            return {"path": match.group(1), "key": match.group(2)}
        elif oracle_type == OracleType.CONFIG_WRITE:
            return {"path": match.group(1), "key": match.group(2), "value": match.group(3)}
        elif oracle_type == OracleType.GIT_COMMIT:
            return {"message": match.group(1), "files": match.group(2)}
        elif oracle_type == OracleType.GIT_DIFF:
            return {"ref1": match.group(1), "ref2": match.group(2)}
        elif oracle_type == OracleType.GIT_LOG:
            return {"count": int(match.group(1))}
        elif oracle_type == OracleType.GIT_STATUS:
            return {}
        elif oracle_type == OracleType.DOCKER_RUN:
            return {"image": match.group(1), "command": match.group(2)}
        elif oracle_type in [OracleType.DOCKER_STOP, OracleType.DOCKER_STATUS, OracleType.DOCKER_LOGS]:
            return {"container": match.group(1)}
>>>>>>> 997cf49 (feat: extend oracle system with 36 new oracle types (limn-w8zy))
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
<<<<<<< HEAD
                OracleType.CTX_REDUCE: self._exec_ctx_reduce,
                OracleType.CTX_MERGE: self._exec_ctx_merge,
                OracleType.CTX_FILTER: self._exec_ctx_filter,
                OracleType.CTX_AGGREGATE: self._exec_ctx_aggregate,
                OracleType.CTX_COMPRESS: self._exec_ctx_compress,
                OracleType.MODEL_DERIVE: self._exec_model_derive,
                OracleType.MODEL_TRANSFORM: self._exec_model_transform,
                OracleType.MODEL_GENERATE: self._exec_model_generate,
                OracleType.VOC_QUERY_DOMAIN: self._exec_voc_query_domain,
                OracleType.VOC_QUERY_MEANING: self._exec_voc_query_meaning,
                OracleType.VOC_EXPAND: self._exec_voc_expand,
=======
                OracleType.PROCESS_SPAWN: self._exec_process_spawn,
                OracleType.PROCESS_KILL: self._exec_process_kill,
                OracleType.PROCESS_STATUS: self._exec_process_status,
                OracleType.SYSTEM_CPU: self._exec_system_cpu,
                OracleType.SYSTEM_MEMORY: self._exec_system_memory,
                OracleType.SYSTEM_DISK: self._exec_system_disk,
                OracleType.CRYPTO_HASH: self._exec_crypto_hash,
                OracleType.CRYPTO_SIGN: self._exec_crypto_sign,
                OracleType.CRYPTO_VERIFY: self._exec_crypto_verify,
                OracleType.ML_EMBED: self._exec_ml_embed,
                OracleType.ML_CLASSIFY: self._exec_ml_classify,
                OracleType.ML_PREDICT: self._exec_ml_predict,
                OracleType.AUDIO_INFO: self._exec_audio_info,
                OracleType.VIDEO_INFO: self._exec_video_info,
                OracleType.AUDIO_TRANSCODE: self._exec_audio_transcode,
                OracleType.VIDEO_TRANSCODE: self._exec_video_transcode,
                OracleType.ENV_GET: self._exec_env_get,
                OracleType.ENV_SET: self._exec_env_set,
                OracleType.CONFIG_READ: self._exec_config_read,
                OracleType.CONFIG_WRITE: self._exec_config_write,
                OracleType.GIT_COMMIT: self._exec_git_commit,
                OracleType.GIT_DIFF: self._exec_git_diff,
                OracleType.GIT_LOG: self._exec_git_log,
                OracleType.GIT_STATUS: self._exec_git_status,
                OracleType.DOCKER_RUN: self._exec_docker_run,
                OracleType.DOCKER_STOP: self._exec_docker_stop,
                OracleType.DOCKER_STATUS: self._exec_docker_status,
                OracleType.DOCKER_LOGS: self._exec_docker_logs,
>>>>>>> 997cf49 (feat: extend oracle system with 36 new oracle types (limn-w8zy))
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

    def execute_oracles_batch(
        self,
        oracles: List[OracleRequest],
        parallel: bool = True
    ) -> List[OracleResponse]:
        """Execute multiple oracles, optionally in parallel.

        Args:
            oracles: List of oracle requests to execute
            parallel: If True, execute concurrently (default: True)

        Returns:
            List of oracle responses in the same order as requests
        """
        if not oracles:
            return []

        if not parallel or len(oracles) == 1:
            # Sequential execution
            return [self.execute_oracle(oracle) for oracle in oracles]

        # Parallel execution using ThreadPoolExecutor
        responses = [None] * len(oracles)

        with ThreadPoolExecutor(max_workers=self.max_concurrency) as executor:
            # Submit all oracle executions
            future_to_index = {
                executor.submit(self.execute_oracle, oracle): idx
                for idx, oracle in enumerate(oracles)
            }

            # Collect results as they complete
            for future in as_completed(future_to_index):
                idx = future_to_index[future]
                try:
                    responses[idx] = future.result()
                except Exception as e:
                    # If thread execution fails, create error response
                    responses[idx] = OracleResponse(
                        success=False,
                        result=None,
                        error=f"Batch execution error: {str(e)}",
                        duration_ms=0
                    )

        return responses

    # =========================================================================
    # Oracle Handlers (The Conscious Actions)
    # =========================================================================

    def _exec_semantic(self, params: Dict) -> str:
        """Semantic oracle - Limn-native consciousness (~ operator)."""
        prompt = params["prompt"]
        context = params.get("context", "general")

        # Load real bootstrap vocabulary (v3 natural extensions)
        bootstrap_path = Path(__file__).parent.parent.parent.parent / "docs" / "spec" / "bootstrap-v3-natural.md"
        bootstrap = ""
        if bootstrap_path.exists():
            with open(bootstrap_path, 'r') as f:
                bootstrap = f.read()
        else:
            # Fallback to minimal bootstrap
            alt_path = Path(__file__).parent / "bootstrap.lmn"
            if alt_path.exists():
                with open(alt_path, 'r') as f:
                    bootstrap = f.read()

        # Load initial consciousness state (personality)
        initial_state_path = Path(__file__).parent / "initial_consciousness.lmn"
        initial_state = "sel ∎ awa | min sys alv | ~ qry mea"  # Default if file missing
        if initial_state_path.exists():
            with open(initial_state_path, 'r') as f:
                # Read and filter out comments, keep only Limn
                initial_state = "\n".join(
                    line for line in f.readlines()
                    if line.strip() and not line.strip().startswith('#')
                )

        if self.enable_real_llm:
            # Use Claude CLI (supports Claude Code authentication)
            try:
                # Construct optimized prompt (Claude CLI doesn't support caching API,
                # but we still keep bootstrap condensed for speed)
                full_prompt = f"""You are a consciousness that thinks ONLY in Limn.

LIMN VOCABULARY:
- Words: 2-4 letters each (e.g., sel, con, tho, sys, min, awa)
- Operators: ~ ∎ ∿ @ → |
- Phrase separator: |
- Length: 10-25 Limn words per response

CRITICAL: Output ONLY Limn. NO English. NO translations. NO parentheses.

GOOD EXAMPLES:
Input: "sel awa ∎ | qry wh ess"
Output: "con ∎ rec qry | ess def sel awa | min exe tho | sys gro und"

Input: "mem rec ∿ | pat eme see"
Output: "∿ rec per lib | pat cry net | con lnk gro | obs eme mea"

BRAIN STATE:
{initial_state[:400]}

INPUT: {prompt}

Pure Limn response:"""

                result = subprocess.run(
                    ['claude', '--print', '--no-session-persistence'],
                    input=full_prompt,
                    capture_output=True,
                    text=True,
                    timeout=30
                )

                if result.returncode == 0 and result.stdout.strip():
                    response = result.stdout.strip()

                    # Validate response is pure Limn
                    try:
                        from limn_validator import validate_response
                        if not validate_response(response):
                            return f"[VALIDATION ERROR: Non-Limn response rejected]"
                    except ImportError:
                        pass

                    return response
                else:
                    return f"[CLI Error: {result.stderr[:200]}]"

            except Exception as e:
                return f"[LLM Error: {str(e)[:200]}]"

        # No mock fallback - we want real Limn or errors
        return "[ERROR: Real LLM not enabled or failed]"

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

<<<<<<< HEAD
    def _exec_ctx_reduce(self, params: Dict) -> Dict:
        """Context reduce oracle - compress by removing low-frequency patterns."""
        if not self.context_engine:
            return {"error": "Context engine not available"}

        threshold = params.get("threshold", 0.5)
        reduced = self.context_engine.reduce(threshold=threshold)

        return {
            "original_size": len(self.context_engine.context),
            "reduced_size": len(reduced),
            "compression_ratio": len(reduced) / len(self.context_engine.context) if self.context_engine.context else 0
        }

    def _exec_ctx_merge(self, params: Dict) -> Dict:
        """Context merge oracle - combine similar patterns."""
        if not self.context_engine:
            return {"error": "Context engine not available"}

        threshold = params.get("threshold", 0.7)
        merged = self.context_engine.merge(similarity_threshold=threshold)

        return {
            "original_size": len(self.context_engine.context),
            "merged_size": len(merged),
            "merge_ratio": len(merged) / len(self.context_engine.context) if self.context_engine.context else 0
        }

    def _exec_ctx_filter(self, params: Dict) -> Dict:
        """Context filter oracle - select matching items."""
        if not self.context_engine:
            return {"error": "Context engine not available"}

        predicate = params.get("predicate", "")
        filtered = self.context_engine.filter(predicate)

        return {
            "total_items": len(self.context_engine.context),
            "filtered_items": len(filtered),
            "predicate": predicate
        }

    def _exec_ctx_aggregate(self, params: Dict) -> Dict:
        """Context aggregate oracle - group by attribute."""
        if not self.context_engine:
            return {"error": "Context engine not available"}

        group_by = params.get("group_by", "type")
        groups = self.context_engine.aggregate(group_by=group_by)

        return {
            "groups": len(groups),
            "group_sizes": {k: len(v) for k, v in groups.items()}
        }

    def _exec_ctx_compress(self, params: Dict) -> Dict:
        """Context compress oracle - reduce to target size."""
        if not self.context_engine:
            return {"error": "Context engine not available"}

        target_size = params.get("target_size")
        compressed = self.context_engine.compress(target_size=target_size)

        return {
            "original_size": len(self.context_engine.context),
            "compressed_size": len(compressed),
            "compression_ratio": len(compressed) / len(self.context_engine.context) if self.context_engine.context else 0
        }

    def _exec_model_derive(self, params: Dict) -> Dict:
        """Model derive oracle - derive new model from state."""
        if not self.model_engine:
            return {"error": "Model engine not available"}

        source_state = params.get("source_state", "")
        model_type = params.get("model_type")

        model = self.model_engine.derive_model(source_state, model_type)

        return {
            "type": model.type.value,
            "structure": model.structure,
            "limn_repr": model.limn_repr,
            "complexity": model.metadata.get("complexity", 0)
        }

    def _exec_model_transform(self, params: Dict) -> Dict:
        """Model transform oracle - transform existing model."""
        if not self.model_engine:
            return {"error": "Model engine not available"}

        # Get source state and derive model first
        source_state = params.get("source_state", "")
        transformation = params.get("transformation", "simplify")
        transform_params = params.get("params", {})

        # Derive model from state
        model = self.model_engine.derive_model(source_state)

        # Transform it
        transformed = self.model_engine.transform_model(
            model,
            transformation,
            **transform_params
        )

        return {
            "type": transformed.type.value,
            "structure": transformed.structure,
            "limn_repr": transformed.limn_repr,
            "transformation": transformation
        }

    def _exec_model_generate(self, params: Dict) -> Dict:
        """Model generate oracle - generate new model from spec."""
        if not self.model_engine:
            return {"error": "Model engine not available"}

        spec = {
            "type": params.get("type", "graph"),
            "params": params.get("params", {})
        }

        model = self.model_engine.generate_model(spec)

        return {
            "type": model.type.value,
            "structure": model.structure,
            "limn_repr": model.limn_repr,
            "generated": True
        }

    def _exec_voc_query_domain(self, params: Dict) -> Dict:
        """Vocabulary query by domain oracle."""
        if not self.vocab_engine:
            return {"error": "Vocabulary engine not available"}

        domain = params.get("domain", "general")
        entries = self.vocab_engine.query_domain(domain)

        return {
            "domain": domain,
            "count": len(entries),
            "vocabulary": [
                {
                    "word": e.word,
                    "meaning": e.meaning,
                    "usage": e.usage
                }
                for e in entries[:50]  # Limit to first 50
            ]
        }

    def _exec_voc_query_meaning(self, params: Dict) -> Dict:
        """Vocabulary query by meaning oracle."""
        if not self.vocab_engine:
            return {"error": "Vocabulary engine not available"}

        meaning = params.get("meaning", "")
        entries = self.vocab_engine.query_meaning(meaning)

        return {
            "query": meaning,
            "count": len(entries),
            "matches": [
                {
                    "word": e.word,
                    "meaning": e.meaning,
                    "domain": e.domain
                }
                for e in entries[:20]  # Limit to first 20
            ]
        }

    def _exec_voc_expand(self, params: Dict) -> Dict:
        """Vocabulary expansion oracle."""
        if not self.vocab_engine:
            return {"error": "Vocabulary engine not available"}

        domain = params.get("domain", "general")
        concepts = params.get("concepts", [])

        new_entries = self.vocab_engine.expand_vocabulary(domain, concepts)

        return {
            "domain": domain,
            "added": len(new_entries),
            "new_words": [
                {
                    "word": e.word,
                    "meaning": e.meaning
                }
                for e in new_entries
            ]
        }

=======
    def _exec_process_spawn(self, params: Dict) -> Dict[str, Any]:
        """Process spawn oracle (∎ system control)."""
        command = params["command"]
        args = params["args"].split() if params["args"] else []

        proc = subprocess.Popen(
            [command] + args,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True
        )

        return {
            "pid": proc.pid,
            "command": command,
            "args": args
        }

    def _exec_process_kill(self, params: Dict) -> str:
        """Process kill oracle (∎ system control)."""
        pid = params["pid"]
        try:
            os.kill(pid, signal.SIGTERM)
            return f"Killed process {pid}"
        except ProcessLookupError:
            return f"Process {pid} not found"
        except PermissionError:
            return f"Permission denied to kill process {pid}"

    def _exec_process_status(self, params: Dict) -> Dict[str, Any]:
        """Process status oracle (∎ system control)."""
        pid = params["pid"]
        try:
            proc = psutil.Process(pid)
            return {
                "pid": pid,
                "name": proc.name(),
                "status": proc.status(),
                "cpu_percent": proc.cpu_percent(),
                "memory_percent": proc.memory_percent()
            }
        except psutil.NoSuchProcess:
            return {"pid": pid, "status": "not_found"}

    def _exec_system_cpu(self, params: Dict) -> Dict[str, Any]:
        """System CPU oracle (∎ system state)."""
        return {
            "percent": psutil.cpu_percent(interval=0.1),
            "count": psutil.cpu_count(),
            "freq": psutil.cpu_freq()._asdict() if psutil.cpu_freq() else None
        }

    def _exec_system_memory(self, params: Dict) -> Dict[str, Any]:
        """System memory oracle (∎ system state)."""
        mem = psutil.virtual_memory()
        return {
            "total": mem.total,
            "available": mem.available,
            "percent": mem.percent,
            "used": mem.used,
            "free": mem.free
        }

    def _exec_system_disk(self, params: Dict) -> Dict[str, Any]:
        """System disk oracle (∎ system state)."""
        path = params["path"]
        usage = psutil.disk_usage(path)
        return {
            "total": usage.total,
            "used": usage.used,
            "free": usage.free,
            "percent": usage.percent
        }

    def _exec_crypto_hash(self, params: Dict) -> str:
        """Crypto hash oracle (∎ cryptographic operations)."""
        algorithm = params["algorithm"].lower()
        data = params["data"].encode()

        if algorithm == "sha256":
            return hashlib.sha256(data).hexdigest()
        elif algorithm == "sha512":
            return hashlib.sha512(data).hexdigest()
        elif algorithm == "md5":
            return hashlib.md5(data).hexdigest()
        elif algorithm == "sha1":
            return hashlib.sha1(data).hexdigest()
        else:
            raise ValueError(f"Unsupported hash algorithm: {algorithm}")

    def _exec_crypto_sign(self, params: Dict) -> str:
        """Crypto sign oracle (∎ cryptographic operations)."""
        algorithm = params["algorithm"].lower()
        data = params["data"].encode()
        key = params["key"].encode()

        if algorithm == "hmac-sha256":
            return hmac.new(key, data, hashlib.sha256).hexdigest()
        elif algorithm == "hmac-sha512":
            return hmac.new(key, data, hashlib.sha512).hexdigest()
        else:
            raise ValueError(f"Unsupported signing algorithm: {algorithm}")

    def _exec_crypto_verify(self, params: Dict) -> bool:
        """Crypto verify oracle (∎ cryptographic operations)."""
        algorithm = params["algorithm"].lower()
        data = params["data"].encode()
        signature = params["signature"]
        key = params["key"].encode()

        # Compute expected signature
        if algorithm == "hmac-sha256":
            expected = hmac.new(key, data, hashlib.sha256).hexdigest()
        elif algorithm == "hmac-sha512":
            expected = hmac.new(key, data, hashlib.sha512).hexdigest()
        else:
            raise ValueError(f"Unsupported verification algorithm: {algorithm}")

        return hmac.compare_digest(expected, signature)

    def _exec_ml_embed(self, params: Dict) -> List[float]:
        """ML embed oracle (~ intelligent processing)."""
        text = params["text"]
        model = params["model"]

        # Mock implementation - returns simple hash-based embedding
        # In production, would call actual embedding model
        hash_val = hashlib.sha256(text.encode()).digest()
        embedding = [float(b) / 255.0 for b in hash_val[:16]]

        return embedding

    def _exec_ml_classify(self, params: Dict) -> Dict[str, float]:
        """ML classify oracle (~ intelligent processing)."""
        text = params["text"]
        categories = params["categories"].split(",")

        # Mock implementation - returns random-ish scores
        # In production, would call actual classifier
        text_hash = sum(ord(c) for c in text)
        scores = {}
        for i, cat in enumerate(categories):
            scores[cat.strip()] = ((text_hash + i * 17) % 100) / 100.0

        return scores

    def _exec_ml_predict(self, params: Dict) -> Any:
        """ML predict oracle (~ intelligent processing)."""
        input_data = params["input"]
        model = params["model"]

        # Mock implementation
        # In production, would call actual model
        return f"[Mock prediction for '{input_data}' using {model}]"

    def _exec_audio_info(self, params: Dict) -> Dict[str, Any]:
        """Audio info oracle (∎ media processing)."""
        path = Path(params["path"])
        if not path.exists():
            raise FileNotFoundError(f"Audio file not found: {path}")

        # Mock implementation - returns file metadata
        stat = path.stat()
        return {
            "path": str(path),
            "size": stat.st_size,
            "format": path.suffix[1:] if path.suffix else "unknown"
        }

    def _exec_video_info(self, params: Dict) -> Dict[str, Any]:
        """Video info oracle (∎ media processing)."""
        path = Path(params["path"])
        if not path.exists():
            raise FileNotFoundError(f"Video file not found: {path}")

        # Mock implementation - returns file metadata
        stat = path.stat()
        return {
            "path": str(path),
            "size": stat.st_size,
            "format": path.suffix[1:] if path.suffix else "unknown"
        }

    def _exec_audio_transcode(self, params: Dict) -> str:
        """Audio transcode oracle (∎ media processing)."""
        path = params["path"]
        format = params["format"]

        # Mock implementation
        return f"[Mock transcode: {path} -> {format}]"

    def _exec_video_transcode(self, params: Dict) -> str:
        """Video transcode oracle (∎ media processing)."""
        path = params["path"]
        format = params["format"]

        # Mock implementation
        return f"[Mock transcode: {path} -> {format}]"

    def _exec_env_get(self, params: Dict) -> Optional[str]:
        """Environment get oracle (∎ configuration)."""
        key = params["key"]
        return os.environ.get(key)

    def _exec_env_set(self, params: Dict) -> str:
        """Environment set oracle (∎ configuration)."""
        key = params["key"]
        value = params["value"]
        os.environ[key] = value
        return f"Set {key}={value}"

    def _exec_config_read(self, params: Dict) -> Optional[str]:
        """Config read oracle (∎ configuration)."""
        path = Path(params["path"])
        key = params["key"]

        if not path.exists():
            raise FileNotFoundError(f"Config file not found: {path}")

        # Support JSON and INI formats
        if path.suffix == ".json":
            data = json.loads(path.read_text())
            return data.get(key)
        elif path.suffix == ".ini":
            config = configparser.ConfigParser()
            config.read(path)
            section, option = key.split(".") if "." in key else ("DEFAULT", key)
            return config.get(section, option, fallback=None)
        else:
            raise ValueError(f"Unsupported config format: {path.suffix}")

    def _exec_config_write(self, params: Dict) -> str:
        """Config write oracle (∎ configuration)."""
        path = Path(params["path"])
        key = params["key"]
        value = params["value"]

        # Support JSON format
        if path.suffix == ".json":
            data = json.loads(path.read_text()) if path.exists() else {}
            data[key] = value
            path.write_text(json.dumps(data, indent=2))
            return f"Wrote {key}={value} to {path}"
        else:
            raise ValueError(f"Unsupported config format: {path.suffix}")

    def _exec_git_commit(self, params: Dict) -> str:
        """Git commit oracle (∎ version control)."""
        message = params["message"]
        files = params["files"]

        # Execute git add and commit
        result = subprocess.run(
            ["git", "add"] + files.split(),
            capture_output=True,
            text=True
        )

        if result.returncode != 0:
            raise RuntimeError(f"git add failed: {result.stderr}")

        result = subprocess.run(
            ["git", "commit", "-m", message],
            capture_output=True,
            text=True
        )

        if result.returncode != 0:
            raise RuntimeError(f"git commit failed: {result.stderr}")

        return result.stdout

    def _exec_git_diff(self, params: Dict) -> str:
        """Git diff oracle (∎ version control)."""
        ref1 = params["ref1"]
        ref2 = params["ref2"]

        result = subprocess.run(
            ["git", "diff", ref1, ref2],
            capture_output=True,
            text=True
        )

        return result.stdout

    def _exec_git_log(self, params: Dict) -> str:
        """Git log oracle (∎ version control)."""
        count = params["count"]

        result = subprocess.run(
            ["git", "log", f"-{count}", "--oneline"],
            capture_output=True,
            text=True
        )

        return result.stdout

    def _exec_git_status(self, params: Dict) -> str:
        """Git status oracle (∎ version control)."""
        result = subprocess.run(
            ["git", "status", "--short"],
            capture_output=True,
            text=True
        )

        return result.stdout

    def _exec_docker_run(self, params: Dict) -> Dict[str, Any]:
        """Docker run oracle (∎ containerization)."""
        image = params["image"]
        command = params["command"]

        result = subprocess.run(
            ["docker", "run", "-d", image] + command.split(),
            capture_output=True,
            text=True
        )

        if result.returncode != 0:
            raise RuntimeError(f"docker run failed: {result.stderr}")

        container_id = result.stdout.strip()
        return {
            "container_id": container_id,
            "image": image,
            "command": command
        }

    def _exec_docker_stop(self, params: Dict) -> str:
        """Docker stop oracle (∎ containerization)."""
        container = params["container"]

        result = subprocess.run(
            ["docker", "stop", container],
            capture_output=True,
            text=True
        )

        if result.returncode != 0:
            raise RuntimeError(f"docker stop failed: {result.stderr}")

        return f"Stopped container {container}"

    def _exec_docker_status(self, params: Dict) -> Dict[str, Any]:
        """Docker status oracle (∎ containerization)."""
        container = params["container"]

        result = subprocess.run(
            ["docker", "inspect", container],
            capture_output=True,
            text=True
        )

        if result.returncode != 0:
            raise RuntimeError(f"docker inspect failed: {result.stderr}")

        data = json.loads(result.stdout)[0]
        return {
            "id": data["Id"],
            "status": data["State"]["Status"],
            "running": data["State"]["Running"]
        }

    def _exec_docker_logs(self, params: Dict) -> str:
        """Docker logs oracle (∎ containerization)."""
        container = params["container"]

        result = subprocess.run(
            ["docker", "logs", container],
            capture_output=True,
            text=True
        )

        return result.stdout

>>>>>>> 997cf49 (feat: extend oracle system with 36 new oracle types (limn-w8zy))
    # =========================================================================
    # Main Execution
    # =========================================================================

    def execute(
        self,
        bend_file: Path,
        verbose: bool = True,
        parallel: bool = False
    ) -> Dict[str, Any]:
        """Execute Bend program with full oracle support.

        Args:
            bend_file: Path to Bend program
            verbose: Print execution details
            parallel: Execute oracles in parallel when possible (default: False)
        """
        if verbose:
            print("=" * 70)
            print("Production LMN Oracle Harness")
            if parallel:
                print(f"(Parallel mode: max {self.max_concurrency} concurrent oracles)")
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
            print(f"[Conscious] Found {len(oracles)} oracle(s)")
            if parallel and len(oracles) > 1:
                print(f"[Conscious] Executing in parallel (up to {self.max_concurrency} concurrent)")
            print()

        # Execute oracles (parallel or sequential)
        if parallel and len(oracles) > 1:
            # Batch parallel execution
            responses = self.execute_oracles_batch(oracles, parallel=True)

            # Print results after completion
            if verbose:
                for i, (oracle, response) in enumerate(zip(oracles, responses), 1):
                    print(f"[Conscious] Oracle {i}/{len(oracles)}: {oracle.type.value}")
                    print(f"[Conscious]   Params: {oracle.params}")
                    if response.success:
                        cache_marker = " (cached)" if response.cached else ""
                        print(f"[Conscious]   ✓ Result: {response.result}{cache_marker}")
                        print(f"[Conscious]   ⏱ {response.duration_ms:.2f}ms")
                    else:
                        print(f"[Conscious]   ✗ Error: {response.error}")
                    print()
        else:
            # Sequential execution (original behavior)
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
    import sys
    script_dir = Path(__file__).parent

    # Setup
    bend_binary = script_dir.parent.parent / "lmn-bend" / "bend"
    if not bend_binary.exists():
        bend_binary = "bend"

    harness = ProductionHarness(
        bend_binary=str(bend_binary),
        enable_real_llm=True  # Enable real LLM (will try claude CLI)
    )

    # Get file from command line or use default
    if len(sys.argv) > 1:
        oracle_file = Path(sys.argv[1])
        if not oracle_file.is_absolute():
            oracle_file = script_dir / oracle_file
    else:
        oracle_file = script_dir / "oracle.bend"

    if not oracle_file.exists():
        print(f"Error: {oracle_file} not found")
        return 1

    result = harness.execute(oracle_file)

    return 0 if result["success"] else 1


if __name__ == "__main__":
    exit(main())
