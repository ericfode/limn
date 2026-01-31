#!/usr/bin/env python3
"""
Limn Package Registry - Decentralized package registry on IPFS/IPNS.

The registry is a JSON document stored on IPFS, with IPNS providing
a mutable pointer to the latest version. Multiple registries can
coexist, and packages can be imported by CID (bypassing registry).

Registry Structure:
{
    "version": 1,
    "updated": "2026-01-25T12:00:00Z",
    "packages": {
        "math-utils": {
            "latest": "1.2.0",
            "author": "alice",
            "description": "Math utilities for Limn",
            "versions": {
                "1.0.0": {"cid": "bafybei...", "published": "..."},
                "1.2.0": {"cid": "bafybei...", "published": "..."}
            }
        }
    }
}
"""

import os
import sys
import json
import hashlib
import subprocess
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass, field, asdict
from enum import Enum
import re


# ============================================================================
# Configuration
# ============================================================================

LIMN_HOME = Path.home() / ".limn"
REGISTRY_CACHE = LIMN_HOME / "registry"
DEFAULT_REGISTRY_IPNS = "k51qzi5uqu5dgu8ej8k9cj2wpx7h94g8kxvqvjrz9g7lv9fvmfzpxqvx8vz"  # Placeholder

REGISTRY_VERSION = 1


# ============================================================================
# Data Structures
# ============================================================================

@dataclass
class SemVer:
    """Semantic version."""
    major: int = 0
    minor: int = 0
    patch: int = 0
    prerelease: str = ""
    build: str = ""

    def __str__(self):
        ver = f"{self.major}.{self.minor}.{self.patch}"
        if self.prerelease:
            ver += f"-{self.prerelease}"
        if self.build:
            ver += f"+{self.build}"
        return ver

    @classmethod
    def parse(cls, s: str) -> "SemVer":
        """Parse semver string like '1.2.3-alpha+build'."""
        match = re.match(
            r'^(\d+)\.(\d+)\.(\d+)(?:-([a-zA-Z0-9.-]+))?(?:\+([a-zA-Z0-9.-]+))?$',
            s.strip()
        )
        if not match:
            # Try simple format
            parts = s.strip().split('.')
            return cls(
                major=int(parts[0]) if len(parts) > 0 else 0,
                minor=int(parts[1]) if len(parts) > 1 else 0,
                patch=int(parts[2].split('-')[0].split('+')[0]) if len(parts) > 2 else 0,
            )

        return cls(
            major=int(match.group(1)),
            minor=int(match.group(2)),
            patch=int(match.group(3)),
            prerelease=match.group(4) or "",
            build=match.group(5) or "",
        )

    def __lt__(self, other: "SemVer") -> bool:
        if (self.major, self.minor, self.patch) != (other.major, other.minor, other.patch):
            return (self.major, self.minor, self.patch) < (other.major, other.minor, other.patch)
        # Prerelease versions have lower precedence
        if self.prerelease and not other.prerelease:
            return True
        if not self.prerelease and other.prerelease:
            return False
        return self.prerelease < other.prerelease

    def __eq__(self, other: "SemVer") -> bool:
        return (self.major, self.minor, self.patch, self.prerelease) == \
               (other.major, other.minor, other.patch, other.prerelease)

    def __hash__(self):
        return hash((self.major, self.minor, self.patch, self.prerelease))

    def satisfies(self, constraint: str) -> bool:
        """Check if this version satisfies a constraint like '^1.2.0' or '>=1.0.0'."""
        constraint = constraint.strip()

        if not constraint or constraint == "*":
            return True

        # Caret range: ^1.2.3 allows >=1.2.3 <2.0.0
        if constraint.startswith('^'):
            base = SemVer.parse(constraint[1:])
            if self.major != base.major:
                return False
            return self >= base

        # Tilde range: ~1.2.3 allows >=1.2.3 <1.3.0
        if constraint.startswith('~'):
            base = SemVer.parse(constraint[1:])
            if self.major != base.major or self.minor != base.minor:
                return False
            return self >= base

        # Range operators
        if constraint.startswith('>='):
            return self >= SemVer.parse(constraint[2:])
        if constraint.startswith('<='):
            return self <= SemVer.parse(constraint[2:])
        if constraint.startswith('>'):
            return self > SemVer.parse(constraint[1:])
        if constraint.startswith('<'):
            return self < SemVer.parse(constraint[1:])
        if constraint.startswith('='):
            return self == SemVer.parse(constraint[1:])

        # Exact match
        return self == SemVer.parse(constraint)

    def __ge__(self, other):
        return self > other or self == other

    def __le__(self, other):
        return self < other or self == other

    def __gt__(self, other):
        return not (self <= other)


@dataclass
class PackageVersion:
    """A specific version of a package."""
    cid: str
    published: str  # ISO timestamp
    signature: str = ""  # Optional Ed25519 signature
    yanked: bool = False  # Whether this version is yanked

    def to_dict(self) -> dict:
        d = {"cid": self.cid, "published": self.published}
        if self.signature:
            d["signature"] = self.signature
        if self.yanked:
            d["yanked"] = True
        return d

    @classmethod
    def from_dict(cls, d: dict) -> "PackageVersion":
        return cls(
            cid=d.get("cid", ""),
            published=d.get("published", ""),
            signature=d.get("signature", ""),
            yanked=d.get("yanked", False),
        )


@dataclass
class PackageEntry:
    """A package in the registry."""
    name: str
    latest: str  # Latest version string
    author: str
    description: str
    homepage: str = ""
    repository: str = ""
    license: str = ""
    keywords: List[str] = field(default_factory=list)
    versions: Dict[str, PackageVersion] = field(default_factory=dict)

    def to_dict(self) -> dict:
        return {
            "latest": self.latest,
            "author": self.author,
            "description": self.description,
            "homepage": self.homepage,
            "repository": self.repository,
            "license": self.license,
            "keywords": self.keywords,
            "versions": {v: ver.to_dict() for v, ver in self.versions.items()},
        }

    @classmethod
    def from_dict(cls, name: str, d: dict) -> "PackageEntry":
        versions = {}
        for v, ver_data in d.get("versions", {}).items():
            if isinstance(ver_data, str):
                # Legacy format: version -> CID
                versions[v] = PackageVersion(cid=ver_data, published="")
            else:
                versions[v] = PackageVersion.from_dict(ver_data)

        return cls(
            name=name,
            latest=d.get("latest", ""),
            author=d.get("author", ""),
            description=d.get("description", ""),
            homepage=d.get("homepage", ""),
            repository=d.get("repository", ""),
            license=d.get("license", ""),
            keywords=d.get("keywords", []),
            versions=versions,
        )

    def get_version(self, constraint: str = "latest") -> Optional[Tuple[str, PackageVersion]]:
        """Get version matching constraint. Returns (version_str, PackageVersion)."""
        if constraint == "latest":
            if self.latest and self.latest in self.versions:
                return (self.latest, self.versions[self.latest])
            # Find highest non-yanked version
            valid = [(v, ver) for v, ver in self.versions.items() if not ver.yanked]
            if not valid:
                return None
            valid.sort(key=lambda x: SemVer.parse(x[0]), reverse=True)
            return valid[0]

        # Find matching version
        for v, ver in sorted(self.versions.items(),
                             key=lambda x: SemVer.parse(x[0]), reverse=True):
            if not ver.yanked and SemVer.parse(v).satisfies(constraint):
                return (v, ver)

        return None


@dataclass
class Registry:
    """The package registry."""
    version: int = REGISTRY_VERSION
    updated: str = ""
    packages: Dict[str, PackageEntry] = field(default_factory=dict)
    ipns_name: str = ""  # IPNS name for this registry

    def to_dict(self) -> dict:
        return {
            "version": self.version,
            "updated": self.updated,
            "packages": {name: pkg.to_dict() for name, pkg in self.packages.items()},
        }

    @classmethod
    def from_dict(cls, d: dict, ipns_name: str = "") -> "Registry":
        packages = {}
        for name, pkg_data in d.get("packages", {}).items():
            packages[name] = PackageEntry.from_dict(name, pkg_data)

        return cls(
            version=d.get("version", REGISTRY_VERSION),
            updated=d.get("updated", ""),
            packages=packages,
            ipns_name=ipns_name,
        )

    def to_json(self, indent: int = 2) -> str:
        return json.dumps(self.to_dict(), indent=indent)

    @classmethod
    def from_json(cls, s: str, ipns_name: str = "") -> "Registry":
        return cls.from_dict(json.loads(s), ipns_name)

    def lookup(self, name: str, constraint: str = "latest") -> Optional[Tuple[str, str]]:
        """Lookup package. Returns (version, CID) or None."""
        if name not in self.packages:
            return None

        result = self.packages[name].get_version(constraint)
        if result:
            return (result[0], result[1].cid)
        return None

    def search(self, query: str, limit: int = 20) -> List[PackageEntry]:
        """Search packages by name, description, keywords."""
        query = query.lower()
        results = []

        for name, pkg in self.packages.items():
            score = 0

            # Name match (highest priority)
            if query in name.lower():
                score += 100
                if name.lower().startswith(query):
                    score += 50

            # Description match
            if query in pkg.description.lower():
                score += 30

            # Keyword match
            for kw in pkg.keywords:
                if query in kw.lower():
                    score += 20

            # Author match
            if query in pkg.author.lower():
                score += 10

            if score > 0:
                results.append((score, pkg))

        results.sort(key=lambda x: x[0], reverse=True)
        return [pkg for _, pkg in results[:limit]]

    def add_package(self, name: str, cid: str, version: str,
                    author: str, description: str,
                    signature: str = "", **kwargs) -> bool:
        """Add or update a package version."""
        now = datetime.utcnow().isoformat() + "Z"

        if name not in self.packages:
            self.packages[name] = PackageEntry(
                name=name,
                latest=version,
                author=author,
                description=description,
                homepage=kwargs.get("homepage", ""),
                repository=kwargs.get("repository", ""),
                license=kwargs.get("license", ""),
                keywords=kwargs.get("keywords", []),
                versions={},
            )

        pkg = self.packages[name]

        # Add version
        pkg.versions[version] = PackageVersion(
            cid=cid,
            published=now,
            signature=signature,
        )

        # Update latest if newer
        if not pkg.latest or SemVer.parse(version) > SemVer.parse(pkg.latest):
            pkg.latest = version

        # Update metadata if provided
        if kwargs.get("description"):
            pkg.description = kwargs["description"]
        if kwargs.get("homepage"):
            pkg.homepage = kwargs["homepage"]
        if kwargs.get("repository"):
            pkg.repository = kwargs["repository"]
        if kwargs.get("license"):
            pkg.license = kwargs["license"]
        if kwargs.get("keywords"):
            pkg.keywords = kwargs["keywords"]

        self.updated = now
        return True

    def yank_version(self, name: str, version: str) -> bool:
        """Yank (soft-delete) a package version."""
        if name not in self.packages:
            return False
        if version not in self.packages[name].versions:
            return False

        self.packages[name].versions[version].yanked = True

        # Update latest if we yanked it
        if self.packages[name].latest == version:
            # Find new latest
            valid = [(v, ver) for v, ver in self.packages[name].versions.items()
                     if not ver.yanked]
            if valid:
                valid.sort(key=lambda x: SemVer.parse(x[0]), reverse=True)
                self.packages[name].latest = valid[0][0]
            else:
                self.packages[name].latest = ""

        self.updated = datetime.utcnow().isoformat() + "Z"
        return True


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


def ipfs_add_json(data: dict) -> Optional[str]:
    """Add JSON data to IPFS, return CID."""
    try:
        json_str = json.dumps(data, indent=2)
        result = subprocess.run(
            ["ipfs", "add", "-Q", "--stdin-name", "registry.json"],
            input=json_str.encode(),
            capture_output=True,
            timeout=60
        )
        if result.returncode == 0:
            return result.stdout.decode().strip()
        return None
    except Exception:
        return None


def ipfs_cat(cid: str) -> Optional[str]:
    """Get content from IPFS."""
    try:
        result = subprocess.run(
            ["ipfs", "cat", cid],
            capture_output=True,
            timeout=30
        )
        if result.returncode == 0:
            return result.stdout.decode()
        return None
    except Exception:
        return None


def ipns_resolve(name: str) -> Optional[str]:
    """Resolve IPNS name to CID."""
    try:
        result = subprocess.run(
            ["ipfs", "name", "resolve", f"/ipns/{name}"],
            capture_output=True,
            text=True,
            timeout=30
        )
        if result.returncode == 0:
            resolved = result.stdout.strip()
            # Returns /ipfs/Qm... format
            if resolved.startswith("/ipfs/"):
                return resolved[6:]
            return resolved
        return None
    except Exception:
        return None


def ipns_publish(key: str, cid: str) -> bool:
    """Publish CID to IPNS key."""
    try:
        result = subprocess.run(
            ["ipfs", "name", "publish", f"--key={key}", f"/ipfs/{cid}"],
            capture_output=True,
            timeout=120  # IPNS publish can be slow
        )
        return result.returncode == 0
    except Exception:
        return False


def ipfs_key_list() -> List[str]:
    """List IPFS keys."""
    try:
        result = subprocess.run(
            ["ipfs", "key", "list"],
            capture_output=True,
            text=True,
            timeout=10
        )
        if result.returncode == 0:
            return result.stdout.strip().split('\n')
        return []
    except Exception:
        return []


def ipfs_key_gen(name: str) -> Optional[str]:
    """Generate new IPFS key, return peer ID."""
    try:
        result = subprocess.run(
            ["ipfs", "key", "gen", "--type=ed25519", name],
            capture_output=True,
            text=True,
            timeout=10
        )
        if result.returncode == 0:
            return result.stdout.strip()
        return None
    except Exception:
        return None


# ============================================================================
# Registry Manager
# ============================================================================

class RegistryManager:
    """Manages package registries."""

    def __init__(self, cache_dir: Path = None):
        self.cache_dir = cache_dir or REGISTRY_CACHE
        self.cache_dir.mkdir(parents=True, exist_ok=True)
        self.registries: Dict[str, Registry] = {}

    def get_cache_path(self, ipns_or_name: str) -> Path:
        """Get cache path for a registry."""
        # Hash the name for safe filesystem path
        name_hash = hashlib.sha256(ipns_or_name.encode()).hexdigest()[:16]
        return self.cache_dir / f"{name_hash}.json"

    def load_cached(self, ipns_or_name: str) -> Optional[Registry]:
        """Load registry from cache."""
        cache_path = self.get_cache_path(ipns_or_name)
        if cache_path.exists():
            try:
                content = cache_path.read_text()
                return Registry.from_json(content, ipns_or_name)
            except Exception:
                pass
        return None

    def save_cache(self, registry: Registry, ipns_or_name: str):
        """Save registry to cache."""
        cache_path = self.get_cache_path(ipns_or_name)
        cache_path.write_text(registry.to_json())

    def fetch(self, ipns_name: str = None, force: bool = False) -> Optional[Registry]:
        """Fetch registry from IPFS/IPNS."""
        ipns_name = ipns_name or DEFAULT_REGISTRY_IPNS

        # Check cache first (unless forced)
        if not force:
            cached = self.load_cached(ipns_name)
            if cached:
                self.registries[ipns_name] = cached
                return cached

        if not ipfs_available():
            print("Warning: IPFS not available, using cached registry")
            return self.load_cached(ipns_name)

        # Resolve IPNS to get current CID
        print(f"Resolving registry: {ipns_name}...")
        cid = ipns_resolve(ipns_name)
        if not cid:
            print("Failed to resolve registry IPNS")
            return self.load_cached(ipns_name)

        # Fetch content
        print(f"Fetching registry: {cid}...")
        content = ipfs_cat(cid)
        if not content:
            print("Failed to fetch registry content")
            return self.load_cached(ipns_name)

        try:
            registry = Registry.from_json(content, ipns_name)
            self.registries[ipns_name] = registry
            self.save_cache(registry, ipns_name)
            return registry
        except Exception as e:
            print(f"Failed to parse registry: {e}")
            return self.load_cached(ipns_name)

    def publish(self, registry: Registry, key_name: str = "limn-registry") -> Optional[str]:
        """Publish registry to IPFS/IPNS."""
        if not ipfs_available():
            print("Error: IPFS not available")
            return None

        # Ensure key exists
        keys = ipfs_key_list()
        if key_name not in keys:
            print(f"Creating IPNS key: {key_name}")
            peer_id = ipfs_key_gen(key_name)
            if not peer_id:
                print("Failed to create IPNS key")
                return None
            print(f"Created key with peer ID: {peer_id}")

        # Update timestamp
        registry.updated = datetime.utcnow().isoformat() + "Z"

        # Add to IPFS
        print("Adding registry to IPFS...")
        cid = ipfs_add_json(registry.to_dict())
        if not cid:
            print("Failed to add registry to IPFS")
            return None

        print(f"Registry CID: {cid}")

        # Publish to IPNS
        print(f"Publishing to IPNS (key={key_name})...")
        if ipns_publish(key_name, cid):
            print("Registry published successfully!")
            registry.ipns_name = key_name
            self.save_cache(registry, key_name)
            return cid
        else:
            print("Failed to publish to IPNS")
            print(f"You can manually reference by CID: {cid}")
            return cid

    def create_empty(self) -> Registry:
        """Create a new empty registry."""
        return Registry(
            version=REGISTRY_VERSION,
            updated=datetime.utcnow().isoformat() + "Z",
            packages={},
        )

    def lookup(self, name: str, constraint: str = "latest",
               registry_name: str = None) -> Optional[Tuple[str, str, str]]:
        """
        Lookup package across registries.
        Returns (version, cid, registry_name) or None.
        """
        registries_to_check = []

        if registry_name:
            if registry_name in self.registries:
                registries_to_check.append((registry_name, self.registries[registry_name]))
            else:
                # Try to fetch
                reg = self.fetch(registry_name)
                if reg:
                    registries_to_check.append((registry_name, reg))
        else:
            # Check all loaded registries
            registries_to_check = list(self.registries.items())

            # Also check default if not loaded
            if DEFAULT_REGISTRY_IPNS not in self.registries:
                reg = self.fetch(DEFAULT_REGISTRY_IPNS)
                if reg:
                    registries_to_check.append((DEFAULT_REGISTRY_IPNS, reg))

        for reg_name, registry in registries_to_check:
            result = registry.lookup(name, constraint)
            if result:
                return (result[0], result[1], reg_name)

        return None

    def search(self, query: str, limit: int = 20) -> List[Tuple[PackageEntry, str]]:
        """Search across all registries. Returns (PackageEntry, registry_name) tuples."""
        results = []

        for reg_name, registry in self.registries.items():
            for pkg in registry.search(query, limit):
                results.append((pkg, reg_name))

        return results[:limit]


# ============================================================================
# CLI Interface
# ============================================================================

def cmd_registry_init(args):
    """Initialize a new registry."""
    manager = RegistryManager()
    registry = manager.create_empty()

    # Save locally
    local_path = Path("registry.json")
    local_path.write_text(registry.to_json())
    print(f"Created empty registry: {local_path}")

    if "--publish" in args:
        key_name = "limn-registry"
        for i, arg in enumerate(args):
            if arg == "--key" and i + 1 < len(args):
                key_name = args[i + 1]

        cid = manager.publish(registry, key_name)
        if cid:
            print(f"\nRegistry published!")
            print(f"  CID: {cid}")
            print(f"  IPNS key: {key_name}")


def cmd_registry_add(args):
    """Add a package to registry."""
    if len(args) < 3:
        print("Usage: limn registry add <name> <version> <cid> [--author <author>] [--desc <desc>]")
        return

    name = args[0]
    version = args[1]
    cid = args[2]

    # Parse optional args
    author = ""
    description = ""
    for i, arg in enumerate(args):
        if arg == "--author" and i + 1 < len(args):
            author = args[i + 1]
        if arg == "--desc" and i + 1 < len(args):
            description = args[i + 1]

    # Load or create registry
    registry_path = Path("registry.json")
    if registry_path.exists():
        registry = Registry.from_json(registry_path.read_text())
    else:
        registry = Registry()

    # Add package
    registry.add_package(name, cid, version, author, description)

    # Save
    registry_path.write_text(registry.to_json())
    print(f"Added {name}@{version} -> {cid}")


def cmd_registry_publish(args):
    """Publish registry to IPNS."""
    registry_path = Path("registry.json")
    if not registry_path.exists():
        print("Error: No registry.json found")
        return

    registry = Registry.from_json(registry_path.read_text())

    key_name = "limn-registry"
    for i, arg in enumerate(args):
        if arg == "--key" and i + 1 < len(args):
            key_name = args[i + 1]

    manager = RegistryManager()
    cid = manager.publish(registry, key_name)

    if cid:
        print(f"\nTo use this registry:")
        print(f"  limn config set registry ipns://{key_name}")


def main():
    import sys

    if len(sys.argv) < 2:
        print("Limn Registry Manager")
        print()
        print("Commands:")
        print("  init [--publish]           Create empty registry")
        print("  add <name> <ver> <cid>     Add package to local registry")
        print("  publish [--key <name>]     Publish registry to IPNS")
        print("  fetch [ipns_name]          Fetch registry from IPNS")
        print("  search <query>             Search packages")
        print("  info <package>             Show package info")
        return

    cmd = sys.argv[1]
    args = sys.argv[2:]

    if cmd == "init":
        cmd_registry_init(args)
    elif cmd == "add":
        cmd_registry_add(args)
    elif cmd == "publish":
        cmd_registry_publish(args)
    elif cmd == "fetch":
        manager = RegistryManager()
        ipns = args[0] if args else DEFAULT_REGISTRY_IPNS
        registry = manager.fetch(ipns, force=True)
        if registry:
            print(f"Fetched registry with {len(registry.packages)} packages")
    elif cmd == "search":
        if not args:
            print("Usage: limn registry search <query>")
            return
        manager = RegistryManager()
        manager.fetch()  # Load default
        results = manager.search(args[0])
        if results:
            print(f"Found {len(results)} packages:")
            for pkg, reg in results:
                print(f"  {pkg.name}@{pkg.latest} - {pkg.description[:50]}")
        else:
            print("No packages found")
    elif cmd == "info":
        if not args:
            print("Usage: limn registry info <package>")
            return
        manager = RegistryManager()
        manager.fetch()
        result = manager.lookup(args[0])
        if result:
            ver, cid, reg = result
            print(f"Package: {args[0]}")
            print(f"Version: {ver}")
            print(f"CID: {cid}")
            print(f"Registry: {reg}")
        else:
            print(f"Package not found: {args[0]}")
    else:
        print(f"Unknown command: {cmd}")


if __name__ == "__main__":
    main()
