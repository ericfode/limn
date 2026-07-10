from __future__ import annotations

import json
from pathlib import Path

import limn_zero


ROOT = Path(__file__).resolve().parents[1]


def test_package_version_is_explicit() -> None:
    assert limn_zero.__version__ == "0.0.0"


def test_controlling_documents_exist() -> None:
    required = {
        "AGENTS.md",
        "docs/THESIS.md",
        "docs/ARCHITECTURE.md",
        "docs/EXPERIMENT-CONTRACT.md",
        "docs/GO-NO-GO.md",
        "docs/REPRODUCIBILITY.md",
        "docs/ROADMAP.md",
    }

    missing = sorted(path for path in required if not (ROOT / path).is_file())
    assert not missing, f"missing controlling documents: {missing}"


def test_result_schema_is_json() -> None:
    schema = json.loads((ROOT / "results/schema.json").read_text(encoding="utf-8"))
    assert schema["title"] == "Limn Zero Run Result"
    assert "run_id" in schema["required"]
    assert schema["properties"]["status"]["enum"] == ["completed", "failed", "aborted"]
