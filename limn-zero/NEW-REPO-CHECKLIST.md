# New Repository Checklist

This directory is a seed. Copy its **contents** into a new repository; do not copy the parent
repository's `.git/`, `.beads/`, generated artifacts, or model files.

## 1. Copy the Seed

PowerShell example:

```powershell
Copy-Item -Recurse -LiteralPath Z:\limn\limn-zero -Destination Z:\limn-zero
Set-Location Z:\limn-zero
```

Choose a destination appropriate for your machine.

## 2. Initialize Version Control

```bash
git init
git branch -M main
```

Add a remote only after the destination repository exists:

```bash
git remote add origin <new-repository-url>
```

## 3. Initialize Work Tracking

If this project will use Beads:

```bash
bd init
bd onboard
```

Confirm that `.beads/` was created inside the new repository before continuing.

## 4. Create the Environment

```bash
python -m venv .venv
python -m pip install --upgrade pip
python -m pip install -e ".[dev]"
```

Activate the virtual environment using the convention for your shell.

## 5. Verify the Scaffold

```bash
python scripts/validate_scaffold.py
python -m pytest
ruff check .
ruff format --check .
mypy src
```

## 6. Make the Initial Commit

```bash
git add .
git commit -m "chore: initialize Limn Zero research scaffold"
git push -u origin main
```

## 7. First Tracked Work

Create issues for:

1. Implement deterministic object-world generation.
2. Implement temporal-causal graph generation.
3. Implement and validate the continuous JEPA ceiling.
4. Implement the no-bypass test harness.
5. Freeze the exact `E000` configuration before primary runs.

Do not begin graph-language emergence experiments until the continuous ceiling and dataset manifests
are reproducible.
