#!/usr/bin/env python3
"""
Limn Import System - Imports packages into the interpreter.

Handles:
- use cid <cid>                    Direct CID import
- use nom "<name>"                 Name-based import (latest)
- use nom "<name>" ver <M> <m> <p> Versioned import
- use ... | get <export>           Selective imports
- Namespace management
- Export resolution
"""

import os
import sys
import re
from pathlib import Path
from typing import Dict, List, Optional, Any, Set, Tuple
from dataclasses import dataclass, field

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from limn_registry import (
    RegistryManager, SemVer,
    ipfs_available, ipfs_cat, LIMN_HOME
)
from limn_resolver import (
    Dependency, DependencyResolver, parse_manifest, PAK_CACHE
)


# ============================================================================
# Data Structures
# ============================================================================

@dataclass
class Export:
    """An exported definition from a package."""
    name: str
    kind: str  # "variable", "function", "constraint"
    value: Any = None
    source: str = ""  # Source code for functions


@dataclass
class ImportedPackage:
    """A package that has been imported."""
    name: str
    version: str
    cid: str
    path: Path
    exports: Dict[str, Export]
    source: str = ""  # Full source code


@dataclass
class ImportSpec:
    """Parsed import specification."""
    import_type: str  # "cid" or "nom"
    cid: str = ""
    name: str = ""
    version: Optional[Tuple[int, int, int]] = None
    selective: List[str] = field(default_factory=list)  # Specific exports to import
    alias: str = ""  # Import alias (e.g., use nom "math" as m)


@dataclass
class Namespace:
    """Namespace for imported definitions."""
    packages: Dict[str, ImportedPackage] = field(default_factory=dict)
    definitions: Dict[str, Export] = field(default_factory=dict)


# ============================================================================
# Import Parser
# ============================================================================

class ImportParser:
    """Parses use statements."""

    def parse(self, source: str) -> List[ImportSpec]:
        """Parse all use statements from source."""
        imports = []

        # Pattern: use (cid|nom) <value> [ver M m p] [| get <name>]* [as <alias>]
        # Split by pipe and handle
        lines = source.split('\n')

        for line in lines:
            line = line.strip()
            if not line.startswith('use '):
                continue

            spec = self._parse_use_line(line)
            if spec:
                imports.append(spec)

        return imports

    def _parse_use_line(self, line: str) -> Optional[ImportSpec]:
        """Parse a single use line."""
        # Remove 'use ' prefix
        rest = line[4:].strip()

        # Split by | for selective imports
        parts = [p.strip() for p in rest.split('|')]
        main_part = parts[0]
        get_parts = [p for p in parts[1:] if p.startswith('get ')]

        spec = ImportSpec(import_type="")

        # Parse main import
        if main_part.startswith('cid '):
            spec.import_type = "cid"
            cid_part = main_part[4:].strip()
            # Handle alias
            if ' as ' in cid_part:
                cid_part, spec.alias = cid_part.split(' as ', 1)
                spec.alias = spec.alias.strip()
            spec.cid = cid_part.strip()

        elif main_part.startswith('nom '):
            spec.import_type = "nom"
            nom_part = main_part[4:].strip()

            # Extract name (quoted)
            match = re.match(r'"([^"]+)"(.*)$', nom_part)
            if match:
                spec.name = match.group(1)
                rest_after_name = match.group(2).strip()

                # Check for version
                ver_match = re.match(r'ver\s+(\d+)\s+(\d+)\s+(\d+)(.*)$', rest_after_name)
                if ver_match:
                    spec.version = (
                        int(ver_match.group(1)),
                        int(ver_match.group(2)),
                        int(ver_match.group(3)),
                    )
                    rest_after_name = ver_match.group(4).strip()

                # Check for alias
                if rest_after_name.startswith('as '):
                    spec.alias = rest_after_name[3:].strip()

        else:
            return None  # Invalid import

        # Parse selective imports
        for get_part in get_parts:
            export_name = get_part[4:].strip()  # Remove 'get '
            spec.selective.append(export_name)

        return spec if spec.import_type else None


# ============================================================================
# Package Loader
# ============================================================================

class PackageLoader:
    """Loads packages from IPFS/cache."""

    def __init__(self, cache_dir: Path = None):
        self.cache_dir = cache_dir or PAK_CACHE
        self.cache_dir.mkdir(parents=True, exist_ok=True)
        self.registry = RegistryManager()
        self.resolver = DependencyResolver(self.registry)

    def load(self, spec: ImportSpec) -> Optional[ImportedPackage]:
        """Load a package based on import spec."""
        if spec.import_type == "cid":
            return self._load_by_cid(spec.cid, spec.alias or spec.cid[:12])
        elif spec.import_type == "nom":
            return self._load_by_name(spec.name, spec.version, spec.alias)
        return None

    def _load_by_cid(self, cid: str, name: str) -> Optional[ImportedPackage]:
        """Load package by direct CID."""
        # Check cache
        cache_path = self.cache_dir / cid
        if not cache_path.exists():
            # Fetch from IPFS
            if not self._fetch_to_cache(cid):
                print(f"Failed to fetch package: {cid}")
                return None

        return self._load_from_path(name, "", cid, cache_path)

    def _load_by_name(self, name: str, version: Optional[Tuple[int, int, int]],
                      alias: str = None) -> Optional[ImportedPackage]:
        """Load package by name from registry."""
        # Fetch registry if needed
        self.registry.fetch()

        # Build constraint
        if version:
            constraint = f"={version[0]}.{version[1]}.{version[2]}"
        else:
            constraint = "latest"

        # Lookup
        result = self.registry.lookup(name, constraint)
        if not result:
            print(f"Package not found: {name} ({constraint})")
            return None

        ver_str, cid, _ = result

        # Check cache
        cache_path = self.cache_dir / cid
        if not cache_path.exists():
            if not self._fetch_to_cache(cid):
                print(f"Failed to fetch package: {name}@{ver_str}")
                return None

        return self._load_from_path(alias or name, ver_str, cid, cache_path)

    def _fetch_to_cache(self, cid: str) -> bool:
        """Fetch package from IPFS to cache."""
        if not ipfs_available():
            print("IPFS not available")
            return False

        import subprocess
        cache_path = self.cache_dir / cid
        try:
            result = subprocess.run(
                ["ipfs", "get", "-o", str(cache_path), cid],
                capture_output=True,
                timeout=120
            )
            return result.returncode == 0
        except Exception as e:
            print(f"IPFS error: {e}")
            return False

    def _load_from_path(self, name: str, version: str, cid: str,
                        path: Path) -> Optional[ImportedPackage]:
        """Load package from local path."""
        exports = {}
        source = ""

        # Check for manifest
        manifest_path = path / "pak.limn"
        if manifest_path.exists():
            metadata, _ = parse_manifest(manifest_path)
            version = version or metadata.get('version', '0.0.0')

            # Load entry file
            entry = metadata.get('entry', 'src/main.limn')
            entry_path = path / entry
            if entry_path.exists():
                source = entry_path.read_text(encoding='utf-8')
                exports = self._extract_exports(source)
        elif path.is_file():
            # Single file package
            source = path.read_text(encoding='utf-8')
            exports = self._extract_exports(source)
        else:
            # Try main.limn
            main_path = path / "main.limn"
            if main_path.exists():
                source = main_path.read_text(encoding='utf-8')
                exports = self._extract_exports(source)

        return ImportedPackage(
            name=name,
            version=version,
            cid=cid,
            path=path,
            exports=exports,
            source=source,
        )

    def _extract_exports(self, source: str) -> Dict[str, Export]:
        """Extract exported definitions from source."""
        exports = {}

        # Split into program and key sections
        if '---' in source:
            program_section = source.split('---')[0]
        else:
            program_section = source

        # Find variable declarations (potential exports)
        for match in re.finditer(r'whe\s+([a-z_][a-z0-9_]*)', program_section):
            var_name = match.group(1)
            exports[var_name] = Export(
                name=var_name,
                kind="variable",
                source=f"whe {var_name}",
            )

        # Find function definitions (constraints that define functions)
        # Pattern: func_name cau arg1 arg2 ... eff result
        for match in re.finditer(
            r'([a-z_][a-z0-9_]*)\s+cau\s+([^\n]+?)\s+eff\s+([a-z_][a-z0-9_]*)',
            program_section
        ):
            func_name = match.group(1)
            args = match.group(2)
            result = match.group(3)

            exports[func_name] = Export(
                name=func_name,
                kind="function",
                source=match.group(0),
            )

        # Find constraints that could be reused
        for line in program_section.split('\n'):
            line = line.strip()
            if line.startswith('#') or not line:
                continue
            if line.startswith('whe '):
                continue

            # Named constraints (constraints that include a clear definition)
            # e.g., "circle_area sa pi exp r exp r"
            match = re.match(r'([a-z_][a-z0-9_]*)\s+sa\s+(.+)', line)
            if match:
                name = match.group(1)
                if name not in exports:
                    exports[name] = Export(
                        name=name,
                        kind="constraint",
                        source=line,
                    )

        return exports


# ============================================================================
# Import Manager
# ============================================================================

class ImportManager:
    """Manages imports for a Limn program."""

    def __init__(self):
        self.parser = ImportParser()
        self.loader = PackageLoader()
        self.namespace = Namespace()

    def process_imports(self, source: str) -> Tuple[str, Dict[str, Any]]:
        """
        Process all imports in source.
        Returns (modified_source, bindings_to_add).
        """
        imports = self.parser.parse(source)

        bindings = {}
        additional_vars = []
        additional_constraints = []

        for spec in imports:
            pkg = self.loader.load(spec)
            if not pkg:
                continue

            # Store in namespace
            self.namespace.packages[pkg.name] = pkg

            # Handle selective imports
            if spec.selective:
                exports_to_add = {
                    name: exp for name, exp in pkg.exports.items()
                    if name in spec.selective
                }
            else:
                exports_to_add = pkg.exports

            # Add to namespace with optional prefix
            prefix = f"{pkg.name}_" if len(imports) > 1 else ""

            for name, export in exports_to_add.items():
                full_name = prefix + name
                self.namespace.definitions[full_name] = export

                # Add variable declaration
                if export.kind == "variable":
                    additional_vars.append(f"whe {full_name}")
                elif export.kind in ("function", "constraint"):
                    # Include the constraint source
                    if prefix:
                        # Rename variables in the constraint
                        renamed = self._rename_in_constraint(export.source, prefix, pkg.exports)
                        additional_constraints.append(renamed)
                    else:
                        additional_constraints.append(export.source)

        # Remove use statements from source
        modified_lines = []
        for line in source.split('\n'):
            stripped = line.strip()
            if not stripped.startswith('use '):
                modified_lines.append(line)

        # Insert imported definitions after variable declarations
        modified_source = '\n'.join(modified_lines)

        # Find where to insert imports
        if additional_vars or additional_constraints:
            # Add after existing whe declarations
            lines = modified_source.split('\n')
            insert_idx = 0

            for i, line in enumerate(lines):
                if line.strip().startswith('whe '):
                    insert_idx = i + 1

            # Insert additional vars
            for var in additional_vars:
                lines.insert(insert_idx, var)
                insert_idx += 1

            # Insert additional constraints before ---
            key_idx = None
            for i, line in enumerate(lines):
                if line.strip() == '---':
                    key_idx = i
                    break

            if key_idx:
                for constraint in additional_constraints:
                    lines.insert(key_idx, constraint)
                    key_idx += 1

            modified_source = '\n'.join(lines)

        return modified_source, bindings

    def _rename_in_constraint(self, source: str, prefix: str,
                              exports: Dict[str, Export]) -> str:
        """Rename variables in a constraint with prefix."""
        result = source
        for name in exports:
            # Replace whole word occurrences
            result = re.sub(
                rf'\b{re.escape(name)}\b',
                prefix + name,
                result
            )
        return result

    def get_export(self, name: str) -> Optional[Export]:
        """Get an export by name."""
        return self.namespace.definitions.get(name)

    def list_imports(self) -> List[str]:
        """List all imported packages."""
        return list(self.namespace.packages.keys())


# ============================================================================
# Integration with Interpreter
# ============================================================================

def preprocess_imports(source: str) -> str:
    """
    Preprocess source to handle imports.
    This is called before parsing.
    """
    manager = ImportManager()
    modified, _ = manager.process_imports(source)
    return modified


def resolve_import(spec: ImportSpec) -> Optional[ImportedPackage]:
    """Resolve a single import spec to a package."""
    loader = PackageLoader()
    return loader.load(spec)


# ============================================================================
# CLI Interface
# ============================================================================

def cmd_import_info(args):
    """Show info about what a use statement would import."""
    if not args:
        print("Usage: limn import info <use-statement>")
        print("Example: limn import info 'use nom \"math-utils\"'")
        return

    statement = ' '.join(args)
    parser = ImportParser()
    specs = parser.parse(statement)

    if not specs:
        print("Could not parse import statement")
        return

    loader = PackageLoader()

    for spec in specs:
        print(f"\nImport: {spec.import_type}")
        if spec.import_type == "cid":
            print(f"  CID: {spec.cid}")
        else:
            print(f"  Name: {spec.name}")
            if spec.version:
                print(f"  Version: {spec.version[0]}.{spec.version[1]}.{spec.version[2]}")

        if spec.selective:
            print(f"  Selective: {', '.join(spec.selective)}")
        if spec.alias:
            print(f"  Alias: {spec.alias}")

        # Try to load
        pkg = loader.load(spec)
        if pkg:
            print(f"\nPackage loaded:")
            print(f"  Version: {pkg.version}")
            print(f"  Path: {pkg.path}")
            print(f"  Exports ({len(pkg.exports)}):")
            for name, export in sorted(pkg.exports.items()):
                print(f"    {export.kind}: {name}")


def main():
    if len(sys.argv) < 2:
        print("Limn Import System")
        print()
        print("Commands:")
        print("  info <use-statement>   Show what would be imported")
        print("  test <file>            Test import processing on a file")
        return

    cmd = sys.argv[1]
    args = sys.argv[2:]

    if cmd == "info":
        cmd_import_info(args)
    elif cmd == "test":
        if not args:
            print("Usage: limn import test <file.limn>")
            return
        path = Path(args[0])
        if not path.exists():
            print(f"File not found: {path}")
            return
        source = path.read_text()
        modified = preprocess_imports(source)
        print("Modified source:")
        print("=" * 60)
        print(modified)
    else:
        print(f"Unknown command: {cmd}")


if __name__ == "__main__":
    main()
