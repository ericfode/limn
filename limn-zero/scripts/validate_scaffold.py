"""Validate the Limn Zero seed without third-party dependencies."""

from __future__ import annotations

import ast
import json
import re
import sys
import tomllib
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
MARKDOWN_LINK = re.compile(r"\[[^\]]+\]\(([^)]+)\)")
REQUIRED_PATHS = {
    "AGENTS.md",
    "README.md",
    "NEW-REPO-CHECKLIST.md",
    "pyproject.toml",
    "docs/THESIS.md",
    "docs/ARCHITECTURE.md",
    "docs/EXPERIMENT-CONTRACT.md",
    "docs/GO-NO-GO.md",
    "docs/REPRODUCIBILITY.md",
    "docs/ROADMAP.md",
    "configs/limn-jepa-0.example.yaml",
    "results/schema.json",
}


def validate_required_paths(errors: list[str]) -> None:
    for relative in sorted(REQUIRED_PATHS):
        if not (ROOT / relative).is_file():
            errors.append(f"missing required file: {relative}")


def validate_structured_files(errors: list[str]) -> None:
    try:
        with (ROOT / "pyproject.toml").open("rb") as handle:
            project = tomllib.load(handle)
        if project.get("project", {}).get("name") != "limn-zero":
            errors.append("pyproject.toml project.name must be 'limn-zero'")
    except (OSError, tomllib.TOMLDecodeError) as exc:
        errors.append(f"invalid pyproject.toml: {exc}")

    try:
        schema = json.loads((ROOT / "results/schema.json").read_text(encoding="utf-8"))
        required = set(schema.get("required", []))
        for field in {"run_id", "experiment_id", "variant", "seed", "status", "metrics"}:
            if field not in required:
                errors.append(f"result schema does not require {field!r}")
    except (OSError, json.JSONDecodeError) as exc:
        errors.append(f"invalid results/schema.json: {exc}")


def validate_python(errors: list[str]) -> None:
    for path in sorted(ROOT.rglob("*.py")):
        try:
            ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
        except (OSError, SyntaxError, UnicodeError) as exc:
            errors.append(f"invalid Python file {path.relative_to(ROOT)}: {exc}")


def validate_markdown_links(errors: list[str]) -> None:
    for path in sorted(ROOT.rglob("*.md")):
        text = path.read_text(encoding="utf-8")
        for match in MARKDOWN_LINK.finditer(text):
            raw_target = match.group(1).strip().strip("<>")
            if not raw_target or raw_target.startswith(("#", "http://", "https://", "mailto:")):
                continue
            target_without_anchor = raw_target.split("#", maxsplit=1)[0]
            target = (path.parent / target_without_anchor).resolve()
            if not target.exists():
                errors.append(
                    f"broken link in {path.relative_to(ROOT)}: {raw_target!r}"
                )


def main() -> int:
    errors: list[str] = []
    validate_required_paths(errors)
    validate_structured_files(errors)
    validate_python(errors)
    validate_markdown_links(errors)

    if errors:
        print("Limn Zero scaffold validation failed:")
        for error in errors:
            print(f"- {error}")
        return 1

    markdown_count = len(list(ROOT.rglob("*.md")))
    python_count = len(list(ROOT.rglob("*.py")))
    print(
        "Limn Zero scaffold valid: "
        f"{len(REQUIRED_PATHS)} required files, {markdown_count} Markdown files, "
        f"{python_count} Python files"
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
