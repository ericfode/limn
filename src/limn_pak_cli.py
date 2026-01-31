#!/usr/bin/env python3
"""
Limn Package Manager CLI - Unified interface for IPFS-based package management.

Commands:
    init        Initialize a new package
    check       Validate package structure
    build       Build package for distribution
    pub         Publish package to IPFS
    install     Install dependencies
    add         Add a dependency
    remove      Remove a dependency
    search      Search the registry
    info        Show package info
    tree        Show dependency tree

    registry    Registry management
    sign        Sign a package
    verify      Verify package signature
    pin         Remote pinning services
    key         Key management
    trust       Trusted publisher management
    cache       Cache management
    config      Configuration
"""

import os
import sys
import json
import shutil
import subprocess
from pathlib import Path
from typing import Dict, List, Optional, Any

# Add src to path
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from limn_registry import (
    RegistryManager, Registry, SemVer,
    ipfs_available, ipfs_add_json, LIMN_HOME
)
from limn_resolver import (
    DependencyResolver, Dependency, parse_manifest, generate_manifest,
    LockFile, PAK_MANIFEST, PAK_LOCK, PAK_CACHE
)
from limn_import import ImportManager, preprocess_imports
from limn_security import KeyManager, PackageSigner, TrustedPublisherManager
from limn_pinning import PinningManager


# ============================================================================
# Configuration
# ============================================================================

CONFIG_FILE = LIMN_HOME / "config.json"


def get_config() -> dict:
    """Load configuration."""
    if CONFIG_FILE.exists():
        return json.loads(CONFIG_FILE.read_text())
    return {"registry": "", "default_pinning": []}


def set_config(key: str, value: Any):
    """Set configuration value."""
    config = get_config()
    config[key] = value
    CONFIG_FILE.parent.mkdir(parents=True, exist_ok=True)
    CONFIG_FILE.write_text(json.dumps(config, indent=2))


# ============================================================================
# Package Commands
# ============================================================================

def cmd_init(args):
    """Initialize a new package."""
    if not args:
        print("Usage: limn pak init <name> [directory]")
        return 1

    name = args[0]
    directory = Path(args[1]) if len(args) > 1 else Path.cwd() / name

    if directory.exists():
        print(f"Error: Directory already exists: {directory}")
        return 1

    # Create structure
    directory.mkdir(parents=True)
    (directory / "src").mkdir()
    (directory / "test").mkdir()

    # Get author from git or env
    author = ""
    try:
        result = subprocess.run(
            ["git", "config", "user.name"],
            capture_output=True, text=True, timeout=5
        )
        if result.returncode == 0:
            author = result.stdout.strip()
    except Exception:
        pass
    if not author:
        author = os.environ.get("USER", os.environ.get("USERNAME", "anonymous"))

    # Create manifest
    metadata = {
        "name": name,
        "version_major": 1,
        "version_minor": 0,
        "version_patch": 0,
        "author": author,
        "description": f"A Limn package",
        "entry": "src/main.limn",
    }
    manifest_content = generate_manifest(metadata, [])
    (directory / PAK_MANIFEST).write_text(manifest_content)

    # Create entry file
    entry_content = f'''# {name} - Main entry point
# pro (program): {name}

# Example: a simple greeting
whe greeting
whe name

greeting sa "Hello from " joi name

---
# key: example
name sa "{name}"
'''
    (directory / "src" / "main.limn").write_text(entry_content)

    # Create test file
    test_content = f'''# {name} - Tests
# pro (program): test_{name}

whe test_input
whe test_expected
whe test_actual
whe test_pass

# Test: 1 + 1 = 2
test_input sa 1
test_expected sa 2
test_input joi test_input sa test_actual
test_actual sa test_expected cau test_pass sa tru

---
# key: run tests
'''
    (directory / "test" / "test_main.limn").write_text(test_content)

    # Create .gitignore
    gitignore_content = '''# Limn
.limn_deps/
.limn-pkg/
pak.lock.limn

# OS
.DS_Store
Thumbs.db
'''
    (directory / ".gitignore").write_text(gitignore_content)

    print(f"Created package: {name}")
    print(f"  {directory}/")
    print(f"  ├── pak.limn          # Package manifest")
    print(f"  ├── src/")
    print(f"  │   └── main.limn     # Entry point")
    print(f"  ├── test/")
    print(f"  │   └── test_main.limn")
    print(f"  └── .gitignore")
    print()
    print(f"Next steps:")
    print(f"  cd {name}")
    print(f"  limn run src/main.limn")
    return 0


def cmd_check(args):
    """Validate package structure."""
    directory = Path(args[0]) if args else Path.cwd()

    errors = []
    warnings = []

    # Check manifest
    manifest_path = directory / PAK_MANIFEST
    if not manifest_path.exists():
        errors.append(f"Missing {PAK_MANIFEST}")
    else:
        metadata, deps = parse_manifest(manifest_path)
        if not metadata.get("name"):
            errors.append("Missing package name (pak_nom)")
        if not metadata.get("entry"):
            warnings.append("Missing entry point (pak_ent)")
        else:
            entry_path = directory / metadata["entry"]
            if not entry_path.exists():
                errors.append(f"Entry point not found: {metadata['entry']}")

    # Check src directory
    if not (directory / "src").exists():
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


def cmd_build(args):
    """Build package for distribution."""
    directory = Path(args[0]) if args else Path.cwd()

    # Validate first
    if cmd_check([str(directory)]) != 0:
        return 1

    metadata, _ = parse_manifest(directory / PAK_MANIFEST)

    # Create build directory
    build_dir = directory / ".limn-pkg"
    if build_dir.exists():
        shutil.rmtree(build_dir)
    build_dir.mkdir()

    # Copy essential files
    shutil.copy(directory / PAK_MANIFEST, build_dir)

    # Copy src
    if (directory / "src").exists():
        shutil.copytree(directory / "src", build_dir / "src")

    # Copy README if exists
    for readme in ["README.md", "README", "readme.md"]:
        if (directory / readme).exists():
            shutil.copy(directory / readme, build_dir)
            break

    # Copy LICENSE if exists
    for license_file in ["LICENSE", "LICENSE.md", "license"]:
        if (directory / license_file).exists():
            shutil.copy(directory / license_file, build_dir)
            break

    version = metadata.get('version', '0.0.0')
    print(f"Built: {metadata.get('name', 'package')}@{version}")
    print(f"  Output: {build_dir}")
    return 0


def cmd_pub(args):
    """Publish package to IPFS."""
    directory = Path(args[0]) if args else Path.cwd()

    # Parse options
    sign_key = None
    pin_services = []
    for i, arg in enumerate(args):
        if arg == "--sign" and i + 1 < len(args):
            sign_key = args[i + 1]
        if arg == "--pin" and i + 1 < len(args):
            pin_services.append(args[i + 1])

    # Check IPFS
    if not ipfs_available():
        print("Error: IPFS daemon not running")
        print("Start with: ipfs daemon")
        return 1

    # Build first
    if cmd_build([str(directory)]) != 0:
        return 1

    build_dir = directory / ".limn-pkg"
    metadata, _ = parse_manifest(directory / PAK_MANIFEST)

    # Sign if requested
    if sign_key:
        print(f"Signing with key: {sign_key}")
        signer = PackageSigner()
        sig = signer.sign_package(build_dir, sign_key)
        if not sig:
            print("Warning: Signing failed, continuing without signature")

    # Publish to IPFS
    print("Publishing to IPFS...")
    try:
        result = subprocess.run(
            ["ipfs", "add", "-r", "-Q", str(build_dir)],
            capture_output=True, text=True, timeout=300
        )
        if result.returncode != 0:
            print(f"IPFS error: {result.stderr}")
            return 1

        cid = result.stdout.strip()
    except Exception as e:
        print(f"Error: {e}")
        return 1

    version = metadata.get('version', '0.0.0')
    print()
    print(f"✓ Published: {metadata.get('name', 'package')}@{version}")
    print(f"  CID: {cid}")
    print()
    print("To use this package:")
    print(f"  use cid {cid}")
    print()
    print("Or add to pak.limn:")
    print(f'  dep_{metadata.get("name", "pkg").replace("-", "_")} sa cid "{cid}"')

    # Pin to remote services if requested
    if pin_services:
        print()
        print("Pinning to remote services...")
        manager = PinningManager()
        manager.pin(cid, f"{metadata.get('name')}@{version}", pin_services)

    return 0


def cmd_install(args):
    """Install dependencies."""
    directory = Path(args[0]) if args else Path.cwd()

    manifest_path = directory / PAK_MANIFEST
    if not manifest_path.exists():
        print(f"Error: No {PAK_MANIFEST} found")
        return 1

    metadata, dependencies = parse_manifest(manifest_path)

    if not dependencies:
        print("No dependencies to install")
        return 0

    print(f"Installing dependencies for {metadata.get('name', 'package')}...")

    # Check for lock file
    lock_path = directory / PAK_LOCK
    lock_file = None
    if lock_path.exists():
        print("Using lock file")
        lock_file = LockFile.from_limn(lock_path.read_text())

    # Resolve dependencies
    registry = RegistryManager()
    registry.fetch()

    resolver = DependencyResolver(registry)
    graph = resolver.resolve(dependencies, lock_file)

    # Report errors
    if graph.warnings:
        for w in graph.warnings:
            print(f"  ⚠ {w}")

    if graph.errors:
        print("\nErrors:")
        for e in graph.errors:
            print(f"  ✗ {e}")
        return 1

    # Install
    if resolver.install_resolved(graph, directory):
        # Generate lock file
        lock = LockFile(version=1, dependencies=graph.resolved)
        lock_path.write_text(lock.to_limn())
        print(f"Generated {PAK_LOCK}")
        return 0

    return 1


def cmd_add(args):
    """Add a dependency."""
    if not args:
        print("Usage: limn pak add <package> [--cid <cid>] [--version <ver>]")
        return 1

    package = args[0]
    cid = None
    version = None

    for i, arg in enumerate(args):
        if arg == "--cid" and i + 1 < len(args):
            cid = args[i + 1]
        if arg == "--version" and i + 1 < len(args):
            version = args[i + 1]

    directory = Path.cwd()
    manifest_path = directory / PAK_MANIFEST

    if not manifest_path.exists():
        print(f"Error: No {PAK_MANIFEST} found")
        return 1

    metadata, dependencies = parse_manifest(manifest_path)

    # Check if already exists
    for dep in dependencies:
        if dep.name == package:
            print(f"Dependency already exists: {package}")
            print("Use --version or --cid to update")
            return 1

    # Create new dependency
    if cid:
        new_dep = Dependency(name=package, constraint=cid, is_cid=True)
    elif version:
        new_dep = Dependency(name=package, constraint=version, is_cid=False)
    else:
        # Lookup latest from registry
        registry = RegistryManager()
        registry.fetch()
        result = registry.lookup(package)
        if result:
            version, cid, _ = result
            new_dep = Dependency(name=package, constraint=f"^{version}", is_cid=False)
            print(f"Found: {package}@{version}")
        else:
            print(f"Package not found in registry: {package}")
            print("Specify --cid for direct CID import")
            return 1

    dependencies.append(new_dep)

    # Update manifest
    new_manifest = generate_manifest(metadata, dependencies)
    manifest_path.write_text(new_manifest)

    print(f"Added dependency: {package}")
    print()
    print("Run 'limn pak install' to install")
    return 0


def cmd_remove(args):
    """Remove a dependency."""
    if not args:
        print("Usage: limn pak remove <package>")
        return 1

    package = args[0]
    directory = Path.cwd()
    manifest_path = directory / PAK_MANIFEST

    if not manifest_path.exists():
        print(f"Error: No {PAK_MANIFEST} found")
        return 1

    metadata, dependencies = parse_manifest(manifest_path)

    # Find and remove
    new_deps = [d for d in dependencies if d.name != package]

    if len(new_deps) == len(dependencies):
        print(f"Dependency not found: {package}")
        return 1

    # Update manifest
    new_manifest = generate_manifest(metadata, new_deps)
    manifest_path.write_text(new_manifest)

    # Remove from deps directory
    deps_dir = directory / ".limn_deps" / package
    if deps_dir.exists():
        shutil.rmtree(deps_dir)

    print(f"Removed dependency: {package}")
    return 0


def cmd_search(args):
    """Search the registry."""
    if not args:
        print("Usage: limn pak search <query>")
        return 1

    query = args[0]

    registry = RegistryManager()
    registry.fetch()

    results = registry.search(query)

    if not results:
        print(f"No packages found for: {query}")
        return 0

    print(f"Found {len(results)} packages:")
    for pkg, reg_name in results:
        print(f"  {pkg.name}@{pkg.latest}")
        if pkg.description:
            print(f"    {pkg.description[:60]}")
    return 0


def cmd_info(args):
    """Show package info."""
    if not args:
        # Show current package info
        manifest_path = Path.cwd() / PAK_MANIFEST
        if manifest_path.exists():
            metadata, deps = parse_manifest(manifest_path)
            print(f"Package: {metadata.get('name', 'unknown')}")
            print(f"Version: {metadata.get('version', '0.0.0')}")
            print(f"Author:  {metadata.get('author', '')}")
            print(f"Entry:   {metadata.get('entry', '')}")
            if deps:
                print(f"\nDependencies ({len(deps)}):")
                for dep in deps:
                    print(f"  {dep.name}: {dep.constraint}")
        else:
            print("Usage: limn pak info <package>")
        return 0

    package = args[0]

    # Check if it's a CID
    if package.startswith("bafybei") or package.startswith("Qm"):
        # Direct IPFS lookup
        print(f"CID: {package}")
        cache_path = PAK_CACHE / package
        if cache_path.exists():
            print("Status: Cached")
            metadata, _ = parse_manifest(cache_path / PAK_MANIFEST)
            if metadata:
                print(f"Name: {metadata.get('name', 'unknown')}")
                print(f"Version: {metadata.get('version', '0.0.0')}")
        else:
            print("Status: Not cached")
            print("Fetch with: limn pak install")
        return 0

    # Registry lookup
    registry = RegistryManager()
    registry.fetch()

    result = registry.lookup(package)
    if result:
        version, cid, reg_name = result
        print(f"Package:  {package}")
        print(f"Version:  {version}")
        print(f"CID:      {cid}")
        print(f"Registry: {reg_name}")

        # Get full package info if available
        if reg_name in registry.registries:
            reg = registry.registries[reg_name]
            if package in reg.packages:
                pkg = reg.packages[package]
                print(f"Author:   {pkg.author}")
                if pkg.description:
                    print(f"Description: {pkg.description}")
                print(f"\nVersions:")
                for v in sorted(pkg.versions.keys(), key=lambda x: SemVer.parse(x), reverse=True):
                    ver_info = pkg.versions[v]
                    yanked = " (yanked)" if ver_info.yanked else ""
                    latest = " (latest)" if v == pkg.latest else ""
                    print(f"  {v}{latest}{yanked}")
    else:
        print(f"Package not found: {package}")
    return 0


def cmd_tree(args):
    """Show dependency tree."""
    directory = Path(args[0]) if args else Path.cwd()
    manifest_path = directory / PAK_MANIFEST

    if not manifest_path.exists():
        print(f"Error: No {PAK_MANIFEST} found")
        return 1

    metadata, dependencies = parse_manifest(manifest_path)
    print(f"{metadata.get('name', 'package')}@{metadata.get('version', '0.0.0')}")

    if not dependencies:
        print("  (no dependencies)")
        return 0

    lock_path = directory / PAK_LOCK
    if lock_path.exists():
        lock = LockFile.from_limn(lock_path.read_text())

        def print_dep(name: str, indent: int = 0, seen: set = None):
            if seen is None:
                seen = set()

            is_last = False  # Would need parent context
            prefix = "  " * indent + "├── "

            if name in lock.dependencies:
                dep = lock.dependencies[name]
                circular = name in seen
                marker = " (circular)" if circular else ""
                print(f"{prefix}{name}@{dep.version}{marker}")

                if not circular:
                    seen = seen | {name}
                    for trans in dep.dependencies:
                        print_dep(trans.name, indent + 1, seen)
            else:
                print(f"{prefix}{name} (not resolved)")

        for dep in dependencies:
            print_dep(dep.name)
    else:
        for dep in dependencies:
            constraint = f"cid:{dep.constraint[:12]}..." if dep.is_cid else dep.constraint
            print(f"├── {dep.name} ({constraint})")
        print()
        print("Run 'limn pak install' to resolve dependencies")

    return 0


def cmd_cache(args):
    """Cache management."""
    if not args:
        print("Usage: limn pak cache <command>")
        print("Commands: info, clear, gc")
        return 0

    subcmd = args[0]

    if subcmd == "info":
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

        print(f"Cache location: {PAK_CACHE}")
        print(f"Packages: {count}")
        print(f"Total size: {total_size / 1024 / 1024:.1f} MB")

    elif subcmd == "clear":
        if PAK_CACHE.exists():
            shutil.rmtree(PAK_CACHE)
            print("Cache cleared")
        else:
            print("Cache already empty")

    elif subcmd == "gc":
        # TODO: Implement garbage collection
        print("Garbage collection not yet implemented")

    return 0


def cmd_config(args):
    """Configuration management."""
    if not args:
        config = get_config()
        print("Configuration:")
        for key, value in config.items():
            print(f"  {key}: {value}")
        return 0

    if args[0] == "set" and len(args) >= 3:
        key = args[1]
        value = args[2]
        set_config(key, value)
        print(f"Set {key} = {value}")
    elif args[0] == "get" and len(args) >= 2:
        key = args[1]
        config = get_config()
        print(config.get(key, "(not set)"))
    else:
        print("Usage: limn pak config [set <key> <value> | get <key>]")

    return 0


# ============================================================================
# Main Entry Point
# ============================================================================

def print_help():
    """Print help message."""
    print("Limn Package Manager (IPFS-based)")
    print()
    print("Package commands:")
    print("  init <name>       Initialize a new package")
    print("  check [dir]       Validate package structure")
    print("  build [dir]       Build package for distribution")
    print("  pub [dir]         Publish to IPFS [--sign <key>] [--pin <service>]")
    print("  install [dir]     Install dependencies")
    print("  add <pkg>         Add dependency [--cid <cid>] [--version <ver>]")
    print("  remove <pkg>      Remove dependency")
    print("  tree [dir]        Show dependency tree")
    print()
    print("Discovery:")
    print("  search <query>    Search registry")
    print("  info [pkg]        Show package info")
    print()
    print("Security:")
    print("  sign <dir> <key>  Sign a package")
    print("  verify <dir>      Verify package signature")
    print("  key <cmd>         Key management (gen, list, export)")
    print("  trust <cmd>       Trusted publishers (add, list, remove)")
    print()
    print("Infrastructure:")
    print("  registry <cmd>    Registry management")
    print("  pin <cmd>         Remote pinning services")
    print("  cache <cmd>       Cache management (info, clear, gc)")
    print("  config [cmd]      Configuration")


def main():
    if len(sys.argv) < 2:
        print_help()
        return 0

    cmd = sys.argv[1]
    args = sys.argv[2:]

    # Package commands
    if cmd == "init":
        return cmd_init(args)
    elif cmd == "check":
        return cmd_check(args)
    elif cmd == "build":
        return cmd_build(args)
    elif cmd == "pub" or cmd == "publish":
        return cmd_pub(args)
    elif cmd == "install" or cmd == "i":
        return cmd_install(args)
    elif cmd == "add":
        return cmd_add(args)
    elif cmd == "remove" or cmd == "rm":
        return cmd_remove(args)
    elif cmd == "search":
        return cmd_search(args)
    elif cmd == "info":
        return cmd_info(args)
    elif cmd == "tree":
        return cmd_tree(args)
    elif cmd == "cache":
        return cmd_cache(args)
    elif cmd == "config":
        return cmd_config(args)

    # Security commands - delegate to limn_security
    elif cmd == "sign":
        from limn_security import cmd_sign
        cmd_sign(args)
    elif cmd == "verify":
        from limn_security import cmd_verify
        cmd_verify(args)
    elif cmd == "key":
        from limn_security import main as security_main
        sys.argv = ["limn", "key"] + args
        security_main()
    elif cmd == "trust":
        from limn_security import main as security_main
        sys.argv = ["limn", "trust"] + args
        security_main()

    # Registry commands - delegate to limn_registry
    elif cmd == "registry":
        from limn_registry import main as registry_main
        sys.argv = ["limn"] + args
        registry_main()

    # Pinning commands - delegate to limn_pinning
    elif cmd == "pin":
        from limn_pinning import main as pinning_main
        sys.argv = ["limn"] + args
        pinning_main()

    elif cmd == "help" or cmd == "--help" or cmd == "-h":
        print_help()

    else:
        print(f"Unknown command: {cmd}")
        print("Run 'limn pak help' for usage")
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(main() or 0)
