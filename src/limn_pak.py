#!/usr/bin/env python3
"""
Limn Package Manager (LPMS)
IPFS-based decentralized package distribution for Limn

Usage:
    limn pak init <name>       Initialize new package
    limn pak check             Validate package structure
    limn pak build             Build package for distribution
    limn pak pub               Publish to IPFS
    limn pak install           Install dependencies
    limn pak add <pkg>         Add dependency
    limn pak search <query>    Search registry
    limn pak info <pkg>        Show package info
    limn pak cache info        Show cache info
"""

import os
import sys
import json
import hashlib
import shutil
import subprocess
import argparse
from pathlib import Path
from typing import Dict, List, Optional, Any
from dataclasses import dataclass, field

# Add src to path
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))


# ============================================================================
# Configuration
# ============================================================================

LIMN_HOME = Path.home() / ".limn"
PAK_CACHE = LIMN_HOME / "pak"
REGISTRY_CACHE = LIMN_HOME / "registry"
DEFAULT_REGISTRY = "ipns://k51..."  # Would be real IPNS in production

PAK_MANIFEST = "pak.limn"
PAK_LOCK = "pak.lock.limn"


# ============================================================================
# Data Structures
# ============================================================================

@dataclass
class PakVersion:
    """Semantic version."""
    major: int = 0
    minor: int = 0
    patch: int = 0

    def __str__(self):
        return f"{self.major}.{self.minor}.{self.patch}"

    @classmethod
    def parse(cls, s: str) -> "PakVersion":
        parts = s.split(".")
        return cls(
            major=int(parts[0]) if len(parts) > 0 else 0,
            minor=int(parts[1]) if len(parts) > 1 else 0,
            patch=int(parts[2]) if len(parts) > 2 else 0,
        )


@dataclass
class PakManifest:
    """Package manifest."""
    name: str = ""
    version: PakVersion = field(default_factory=PakVersion)
    author: str = ""
    description: str = ""
    entry: str = "src/main.limn"
    dependencies: Dict[str, str] = field(default_factory=dict)  # name -> CID or version


@dataclass
class PakInfo:
    """Package info from registry."""
    name: str
    latest: str  # CID
    versions: Dict[str, str]  # version -> CID
    author: str
    description: str


# ============================================================================
# IPFS Integration
# ============================================================================

def ipfs_available() -> bool:
    """Check if IPFS daemon is running."""
    try:
        result = subprocess.run(
            ["ipfs", "version"],
            capture_output=True,
            timeout=5
        )
        return result.returncode == 0
    except (FileNotFoundError, subprocess.TimeoutExpired):
        return False


def ipfs_add(path: Path, recursive: bool = True) -> Optional[str]:
    """Add file/directory to IPFS, return CID."""
    try:
        cmd = ["ipfs", "add", "-Q"]  # -Q for quiet (just CID)
        if recursive:
            cmd.append("-r")
        cmd.append(str(path))

        result = subprocess.run(cmd, capture_output=True, text=True, timeout=60)
        if result.returncode == 0:
            return result.stdout.strip()
        else:
            print(f"IPFS error: {result.stderr}", file=sys.stderr)
            return None
    except Exception as e:
        print(f"IPFS error: {e}", file=sys.stderr)
        return None


def ipfs_get(cid: str, output_path: Path) -> bool:
    """Get content from IPFS."""
    try:
        result = subprocess.run(
            ["ipfs", "get", cid, "-o", str(output_path)],
            capture_output=True,
            timeout=120
        )
        return result.returncode == 0
    except Exception as e:
        print(f"IPFS error: {e}", file=sys.stderr)
        return False


def ipfs_cat(cid: str) -> Optional[str]:
    """Get file content from IPFS."""
    try:
        result = subprocess.run(
            ["ipfs", "cat", cid],
            capture_output=True,
            text=True,
            timeout=30
        )
        if result.returncode == 0:
            return result.stdout
        return None
    except Exception:
        return None


# ============================================================================
# Manifest Parsing
# ============================================================================

def parse_manifest(path: Path) -> Optional[PakManifest]:
    """Parse pak.limn manifest file."""
    if not path.exists():
        return None

    manifest = PakManifest()

    try:
        with open(path, 'r', encoding='utf-8') as f:
            content = f.read()

        # Simple parser for pak.limn format
        # Looks for patterns like: pak_nom sa "value"
        import re

        # Extract name
        match = re.search(r'pak_nom\s+sa\s+"([^"]+)"', content)
        if match:
            manifest.name = match.group(1)

        # Extract version parts
        for part, attr in [('maj', 'major'), ('min', 'minor'), ('pat', 'patch')]:
            match = re.search(rf'pak_ver_{part}\s+sa\s+(\d+)', content)
            if match:
                setattr(manifest.version, attr, int(match.group(1)))

        # Extract author
        match = re.search(r'pak_aut\s+sa\s+"([^"]+)"', content)
        if match:
            manifest.author = match.group(1)

        # Extract description
        match = re.search(r'pak_des\s+sa\s+"([^"]+)"', content)
        if match:
            manifest.description = match.group(1)

        # Extract entry point
        match = re.search(r'pak_ent\s+sa\s+"([^"]+)"', content)
        if match:
            manifest.entry = match.group(1)

        # TODO: Parse dependencies

        return manifest

    except Exception as e:
        print(f"Error parsing manifest: {e}", file=sys.stderr)
        return None


def generate_manifest(manifest: PakManifest) -> str:
    """Generate pak.limn content from manifest."""
    return f'''# pak.limn - Package manifest
# pro (program): package definition

# nom (name): package name
whe pak_nom
pak_nom sa "{manifest.name}"

# ver (version): semantic version
whe pak_ver_maj
whe pak_ver_min
whe pak_ver_pat
pak_ver_maj sa {manifest.version.major}
pak_ver_min sa {manifest.version.minor}
pak_ver_pat sa {manifest.version.patch}

# aut (author): package author
whe pak_aut
pak_aut sa "{manifest.author}"

# des (description): package description
whe pak_des
pak_des sa "{manifest.description}"

# ent (entry): main entry file
whe pak_ent
pak_ent sa "{manifest.entry}"

---
# key: (metadata only)
'''


# ============================================================================
# Package Commands
# ============================================================================

def cmd_init(name: str, path: Path = None):
    """Initialize a new package."""
    if path is None:
        path = Path.cwd() / name

    if path.exists():
        print(f"Error: Directory already exists: {path}", file=sys.stderr)
        return 1

    # Create directory structure
    path.mkdir(parents=True)
    (path / "src").mkdir()
    (path / "test").mkdir()

    # Create manifest
    manifest = PakManifest(
        name=name,
        version=PakVersion(1, 0, 0),
        author=os.environ.get("USER", "anonymous"),
        description=f"A Limn package: {name}",
        entry="src/main.limn"
    )

    with open(path / PAK_MANIFEST, 'w') as f:
        f.write(generate_manifest(manifest))

    # Create main entry file
    with open(path / "src" / "main.limn", 'w') as f:
        f.write(f'''# {name} - Main entry point
# pro (program): {name}

# Example: add two numbers
whe a
whe b
whe result

a joi b sa result

---
# key: example
a sa 1
b sa 2
''')

    # Create test file
    with open(path / "test" / "test_main.limn", 'w') as f:
        f.write(f'''# {name} - Tests
# pro (program): test_{name}

whe test_a
whe test_b
whe test_result

test_a joi test_b sa test_result

---
# key: test
test_a sa 5
test_b sa 3
''')

    # Create README
    with open(path / "README.md", 'w') as f:
        f.write(f'''# {name}

A Limn package.

## Installation

```bash
limn pak add {name}
```

## Usage

```limn
use nom "{name}"
```
''')

    print(f"Initialized package: {name}")
    print(f"  {path}/")
    print(f"  ├── pak.limn")
    print(f"  ├── src/main.limn")
    print(f"  ├── test/test_main.limn")
    print(f"  └── README.md")
    return 0


def cmd_check(path: Path = None):
    """Validate package structure."""
    if path is None:
        path = Path.cwd()

    errors = []
    warnings = []

    # Check manifest exists
    manifest_path = path / PAK_MANIFEST
    if not manifest_path.exists():
        errors.append(f"Missing manifest: {PAK_MANIFEST}")
    else:
        manifest = parse_manifest(manifest_path)
        if manifest is None:
            errors.append("Failed to parse manifest")
        else:
            if not manifest.name:
                errors.append("Missing package name (pak_nom)")
            if manifest.version.major == 0 and manifest.version.minor == 0:
                warnings.append("Version is 0.0.x - consider releasing 1.0.0")

            # Check entry point exists
            entry_path = path / manifest.entry
            if not entry_path.exists():
                errors.append(f"Entry point not found: {manifest.entry}")

    # Check src directory
    if not (path / "src").exists():
        warnings.append("Missing src/ directory")

    # Report
    if errors:
        print("Errors:")
        for e in errors:
            print(f"  ✗ {e}")
    if warnings:
        print("Warnings:")
        for w in warnings:
            print(f"  ⚠ {w}")

    if not errors:
        print("✓ Package structure valid")
        return 0
    return 1


def cmd_build(path: Path = None):
    """Build package for distribution."""
    if path is None:
        path = Path.cwd()

    # Validate first
    if cmd_check(path) != 0:
        return 1

    manifest = parse_manifest(path / PAK_MANIFEST)
    if manifest is None:
        return 1

    # Create build directory
    build_dir = path / ".limn-pkg"
    if build_dir.exists():
        shutil.rmtree(build_dir)
    build_dir.mkdir()

    # Copy files
    shutil.copy(path / PAK_MANIFEST, build_dir)

    # Copy src
    if (path / "src").exists():
        shutil.copytree(path / "src", build_dir / "src")

    # Copy README if exists
    if (path / "README.md").exists():
        shutil.copy(path / "README.md", build_dir)

    print(f"Built package: {manifest.name}@{manifest.version}")
    print(f"  Output: {build_dir}")
    return 0


def cmd_pub(path: Path = None):
    """Publish package to IPFS."""
    if path is None:
        path = Path.cwd()

    # Check IPFS
    if not ipfs_available():
        print("Error: IPFS daemon not running", file=sys.stderr)
        print("Start with: ipfs daemon", file=sys.stderr)
        return 1

    # Build first
    if cmd_build(path) != 0:
        return 1

    build_dir = path / ".limn-pkg"
    manifest = parse_manifest(path / PAK_MANIFEST)

    # Add to IPFS
    print("Publishing to IPFS...")
    cid = ipfs_add(build_dir)

    if cid:
        print(f"✓ Published: {manifest.name}@{manifest.version}")
        print(f"  CID: {cid}")
        print(f"")
        print(f"To use this package:")
        print(f"  use cid {cid}")
        return 0
    else:
        print("Error: Failed to publish to IPFS", file=sys.stderr)
        return 1


def cmd_install(path: Path = None):
    """Install dependencies from pak.limn."""
    if path is None:
        path = Path.cwd()

    manifest_path = path / PAK_MANIFEST
    if not manifest_path.exists():
        print(f"Error: No {PAK_MANIFEST} found", file=sys.stderr)
        return 1

    manifest = parse_manifest(manifest_path)
    if not manifest.dependencies:
        print("No dependencies to install")
        return 0

    # Ensure cache directory exists
    PAK_CACHE.mkdir(parents=True, exist_ok=True)

    for dep_name, dep_spec in manifest.dependencies.items():
        print(f"Installing {dep_name}...")

        # Check if it's a CID or name
        if dep_spec.startswith("bafybei") or dep_spec.startswith("Qm"):
            cid = dep_spec
        else:
            # TODO: Lookup in registry
            print(f"  Warning: Registry lookup not implemented for {dep_spec}")
            continue

        # Check cache
        cache_path = PAK_CACHE / cid
        if cache_path.exists():
            print(f"  ✓ {dep_name} (cached)")
            continue

        # Fetch from IPFS
        if not ipfs_available():
            print("  Error: IPFS not available", file=sys.stderr)
            continue

        if ipfs_get(cid, cache_path):
            print(f"  ✓ {dep_name} installed")
        else:
            print(f"  ✗ Failed to install {dep_name}")

    return 0


def cmd_add(pkg_spec: str, path: Path = None):
    """Add a dependency to pak.limn."""
    if path is None:
        path = Path.cwd()

    manifest_path = path / PAK_MANIFEST
    if not manifest_path.exists():
        print(f"Error: No {PAK_MANIFEST} found", file=sys.stderr)
        return 1

    # Parse pkg spec (name or name@version or CID)
    if pkg_spec.startswith("bafybei") or pkg_spec.startswith("Qm"):
        dep_type = "cid"
        dep_value = pkg_spec
    elif "@" in pkg_spec:
        name, version = pkg_spec.split("@", 1)
        dep_type = "version"
        dep_value = version
    else:
        dep_type = "name"
        dep_value = pkg_spec

    print(f"Adding dependency: {pkg_spec}")
    print(f"  Type: {dep_type}")

    # TODO: Actually add to manifest
    print("  Note: Manual edit of pak.limn required for now")

    return 0


def cmd_cache_info():
    """Show cache information."""
    if not PAK_CACHE.exists():
        print("Cache is empty")
        return 0

    total_size = 0
    count = 0

    for item in PAK_CACHE.iterdir():
        if item.is_dir():
            size = sum(f.stat().st_size for f in item.rglob('*') if f.is_file())
            total_size += size
            count += 1
            print(f"  {item.name}: {size / 1024:.1f} KB")

    print(f"\nTotal: {count} packages, {total_size / 1024:.1f} KB")
    return 0


def cmd_search(query: str):
    """Search registry for packages."""
    print(f"Searching for: {query}")
    print("  Note: Registry search not yet implemented")
    print("  Use 'limn pak info <cid>' for direct IPFS lookup")
    return 0


def cmd_info(pkg_spec: str):
    """Show package information."""
    # Check if it's a CID
    if pkg_spec.startswith("bafybei") or pkg_spec.startswith("Qm"):
        cid = pkg_spec

        # Check local cache first
        cache_path = PAK_CACHE / cid
        if cache_path.exists():
            manifest = parse_manifest(cache_path / PAK_MANIFEST)
            if manifest:
                print(f"Package: {manifest.name}")
                print(f"Version: {manifest.version}")
                print(f"Author:  {manifest.author}")
                print(f"CID:     {cid}")
                print(f"Description: {manifest.description}")
                return 0

        # Try IPFS
        if ipfs_available():
            content = ipfs_cat(f"{cid}/pak.limn")
            if content:
                print(f"CID: {cid}")
                print(f"Content:\n{content[:500]}...")
                return 0

        print(f"Package not found: {cid}")
        return 1

    # Name lookup
    print(f"Looking up: {pkg_spec}")
    print("  Note: Registry lookup not yet implemented")
    return 1


# ============================================================================
# Main CLI
# ============================================================================

def main():
    parser = argparse.ArgumentParser(
        description='Limn Package Manager (IPFS-based)',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Commands:
    init <name>     Initialize new package
    check           Validate package structure
    build           Build package for distribution
    pub             Publish to IPFS
    install         Install dependencies
    add <pkg>       Add dependency
    search <query>  Search registry
    info <pkg>      Show package info
    cache info      Show cache info
        """
    )

    parser.add_argument('command', nargs='?', default='help')
    parser.add_argument('args', nargs='*')

    args = parser.parse_args()

    if args.command == 'init':
        if not args.args:
            print("Usage: limn pak init <name>", file=sys.stderr)
            return 1
        return cmd_init(args.args[0])

    elif args.command == 'check':
        return cmd_check()

    elif args.command == 'build':
        return cmd_build()

    elif args.command == 'pub':
        return cmd_pub()

    elif args.command == 'install':
        return cmd_install()

    elif args.command == 'add':
        if not args.args:
            print("Usage: limn pak add <package>", file=sys.stderr)
            return 1
        return cmd_add(args.args[0])

    elif args.command == 'search':
        if not args.args:
            print("Usage: limn pak search <query>", file=sys.stderr)
            return 1
        return cmd_search(args.args[0])

    elif args.command == 'info':
        if not args.args:
            print("Usage: limn pak info <package>", file=sys.stderr)
            return 1
        return cmd_info(args.args[0])

    elif args.command == 'cache':
        if args.args and args.args[0] == 'info':
            return cmd_cache_info()
        print("Usage: limn pak cache info", file=sys.stderr)
        return 1

    else:
        parser.print_help()
        return 0


if __name__ == '__main__':
    sys.exit(main())
