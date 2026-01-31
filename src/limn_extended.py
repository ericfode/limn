"""
Limn Extended Interpreter
Extends Limn-PL with I/O and IPFS primitives for package management.

Host primitives (implemented in Python, called from Limn):
- rea fil nom eff con    - read file content
- wri fil nom con        - write file content
- exi pat eff res        - check path exists
- lis dir eff gro        - list directory
- cre dir nom            - create directory
- exe cmd eff out        - execute command
- ipfs add pat eff cid   - add to IPFS
- ipfs get cid eff pat   - get from IPFS
"""

import os
import subprocess
import json
from pathlib import Path
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Callable

# Import base interpreter
from limn_pl_interpreter import (
    LimnPLParser, LimnPLSolver, Environment, Program, Variable,
    Literal, BinaryOp, Constraint, FunctionCall, Node, run_limn_pl, parse_key
)


# ============================================================================
# Host Primitives (I/O Operations)
# ============================================================================

class LimnHostPrimitives:
    """Host primitives for Limn I/O operations."""

    def __init__(self, base_dir: str = "."):
        self.base_dir = Path(base_dir)
        self.cache_dir = Path.home() / ".limn"
        self.pak_cache = self.cache_dir / "pak"
        self.reg_cache = self.cache_dir / "reg"

    def ensure_cache_dirs(self):
        """Ensure cache directories exist."""
        self.pak_cache.mkdir(parents=True, exist_ok=True)
        self.reg_cache.mkdir(parents=True, exist_ok=True)

    # --- File Operations ---

    def rea_fil(self, path: str) -> Optional[str]:
        """Read file content: rea fil nom eff con"""
        try:
            filepath = self.base_dir / path if not os.path.isabs(path) else Path(path)
            return filepath.read_text(encoding='utf-8')
        except Exception:
            return None

    def wri_fil(self, path: str, content: str) -> bool:
        """Write file content: wri fil nom con"""
        try:
            filepath = self.base_dir / path if not os.path.isabs(path) else Path(path)
            filepath.parent.mkdir(parents=True, exist_ok=True)
            filepath.write_text(content, encoding='utf-8')
            return True
        except Exception:
            return False

    def exi_pat(self, path: str) -> bool:
        """Check path exists: exi pat eff res"""
        filepath = self.base_dir / path if not os.path.isabs(path) else Path(path)
        return filepath.exists()

    def lis_dir(self, path: str) -> List[str]:
        """List directory: lis dir eff gro"""
        try:
            dirpath = self.base_dir / path if not os.path.isabs(path) else Path(path)
            return [f.name for f in dirpath.iterdir()]
        except Exception:
            return []

    def cre_dir(self, path: str) -> bool:
        """Create directory: cre dir nom"""
        try:
            dirpath = self.base_dir / path if not os.path.isabs(path) else Path(path)
            dirpath.mkdir(parents=True, exist_ok=True)
            return True
        except Exception:
            return False

    def exe_cmd(self, cmd: str) -> Dict[str, Any]:
        """Execute command: exe cmd eff out"""
        try:
            result = subprocess.run(
                cmd, shell=True, capture_output=True, text=True, timeout=60
            )
            return {
                "out": result.stdout,
                "err": result.stderr,
                "cod": result.returncode
            }
        except subprocess.TimeoutExpired:
            return {"out": "", "err": "Timeout", "cod": -1}
        except Exception as e:
            return {"out": "", "err": str(e), "cod": -1}

    # --- IPFS Operations ---

    def ipfs_det(self) -> Dict[str, Any]:
        """Detect IPFS daemon: ipfs det eff res"""
        result = self.exe_cmd("ipfs id")
        if result["cod"] == 0:
            try:
                info = json.loads(result["out"])
                return {
                    "run": True,
                    "id": info.get("ID", ""),
                    "ver": info.get("AgentVersion", "")
                }
            except:
                return {"run": True, "id": "", "ver": ""}
        return {"run": False, "err": result["err"]}

    def ipfs_add(self, path: str) -> Optional[str]:
        """Add to IPFS: ipfs add pat eff cid"""
        filepath = self.base_dir / path if not os.path.isabs(path) else Path(path)
        if filepath.is_dir():
            result = self.exe_cmd(f'ipfs add -r -Q "{filepath}"')
        else:
            result = self.exe_cmd(f'ipfs add -Q "{filepath}"')

        if result["cod"] == 0:
            return result["out"].strip()
        return None

    def ipfs_get(self, cid: str, dest: Optional[str] = None) -> Optional[str]:
        """Get from IPFS: ipfs get cid eff pat"""
        self.ensure_cache_dirs()

        if dest is None:
            dest = str(self.pak_cache / cid)

        if os.path.exists(dest):
            return dest  # Already cached

        result = self.exe_cmd(f'ipfs get -o "{dest}" {cid}')
        if result["cod"] == 0:
            return dest
        return None

    def ipfs_pin(self, cid: str) -> bool:
        """Pin content: ipfs pin cid"""
        result = self.exe_cmd(f'ipfs pin add {cid}')
        return result["cod"] == 0

    def ipns_res(self, name: str) -> Optional[str]:
        """Resolve IPNS: ipns res nam eff cid"""
        result = self.exe_cmd(f'ipfs name resolve /ipns/{name}')
        if result["cod"] == 0:
            # Returns /ipfs/Qm... format
            resolved = result["out"].strip()
            if resolved.startswith("/ipfs/"):
                return resolved[6:]
            return resolved
        return None

    def ipns_pub(self, name: str, cid: str) -> bool:
        """Publish to IPNS: ipns pub nam cid"""
        result = self.exe_cmd(f'ipfs name publish --key={name} /ipfs/{cid}')
        return result["cod"] == 0


# ============================================================================
# Extended Solver with Host Primitives
# ============================================================================

class LimnExtendedSolver(LimnPLSolver):
    """Limn solver extended with host primitive support."""

    def __init__(self, primitives: Optional[LimnHostPrimitives] = None, **kwargs):
        super().__init__(**kwargs)
        self.primitives = primitives or LimnHostPrimitives()

        # Register built-in primitive functions
        self.builtin_funcs: Dict[str, Callable] = {
            # File operations
            "rea": self._rea,
            "wri": self._wri,
            "exi": self._exi,
            "lis": self._lis,
            "cre": self._cre,
            "exe": self._exe,
            # IPFS operations
            "ipfs_det": self._ipfs_det,
            "ipfs_add": self._ipfs_add,
            "ipfs_get": self._ipfs_get,
            "ipfs_pin": self._ipfs_pin,
            "ipns_res": self._ipns_res,
            "ipns_pub": self._ipns_pub,
            # Package operations
            "pak_par": self._pak_par,
            "pak_val": self._pak_val,
        }

    def _rea(self, args: List[Any]) -> Any:
        """Read file."""
        if len(args) >= 1:
            return self.primitives.rea_fil(str(args[0]))
        return None

    def _wri(self, args: List[Any]) -> Any:
        """Write file."""
        if len(args) >= 2:
            return self.primitives.wri_fil(str(args[0]), str(args[1]))
        return False

    def _exi(self, args: List[Any]) -> Any:
        """Check exists."""
        if len(args) >= 1:
            return self.primitives.exi_pat(str(args[0]))
        return False

    def _lis(self, args: List[Any]) -> Any:
        """List directory."""
        if len(args) >= 1:
            return self.primitives.lis_dir(str(args[0]))
        return []

    def _cre(self, args: List[Any]) -> Any:
        """Create directory."""
        if len(args) >= 1:
            return self.primitives.cre_dir(str(args[0]))
        return False

    def _exe(self, args: List[Any]) -> Any:
        """Execute command."""
        if len(args) >= 1:
            return self.primitives.exe_cmd(str(args[0]))
        return {"out": "", "err": "", "cod": -1}

    def _ipfs_det(self, args: List[Any]) -> Any:
        """Detect IPFS."""
        return self.primitives.ipfs_det()

    def _ipfs_add(self, args: List[Any]) -> Any:
        """Add to IPFS."""
        if len(args) >= 1:
            return self.primitives.ipfs_add(str(args[0]))
        return None

    def _ipfs_get(self, args: List[Any]) -> Any:
        """Get from IPFS."""
        if len(args) >= 1:
            dest = str(args[1]) if len(args) >= 2 else None
            return self.primitives.ipfs_get(str(args[0]), dest)
        return None

    def _ipfs_pin(self, args: List[Any]) -> Any:
        """Pin content."""
        if len(args) >= 1:
            return self.primitives.ipfs_pin(str(args[0]))
        return False

    def _ipns_res(self, args: List[Any]) -> Any:
        """Resolve IPNS."""
        if len(args) >= 1:
            return self.primitives.ipns_res(str(args[0]))
        return None

    def _ipns_pub(self, args: List[Any]) -> Any:
        """Publish to IPNS."""
        if len(args) >= 2:
            return self.primitives.ipns_pub(str(args[0]), str(args[1]))
        return False

    def _pak_par(self, args: List[Any]) -> Any:
        """Parse pak.limn manifest."""
        if len(args) >= 1:
            content = str(args[0])
            return self._parse_manifest(content)
        return None

    def _parse_manifest(self, content: str) -> Dict[str, Any]:
        """Parse pak.limn manifest content."""
        manifest = {
            "nom": "",
            "ver_maj": 0,
            "ver_min": 0,
            "ver_pat": 0,
            "aut": "",
            "des": "",
            "ent": "",
            "dep": []
        }

        parser = LimnPLParser()
        try:
            program = parser.parse(content)
            solver = LimnPLSolver()
            result = solver.solve(program, {})

            # Extract manifest fields
            for key in ["pak_nom", "pak_ver_maj", "pak_ver_min", "pak_ver_pat",
                       "pak_aut", "pak_des", "pak_ent", "pak_dep"]:
                if key in result:
                    field_name = key.replace("pak_", "")
                    manifest[field_name] = result[key]

        except Exception:
            pass

        return manifest

    def _pak_val(self, args: List[Any]) -> Any:
        """Validate package directory."""
        if len(args) >= 1:
            path = Path(str(args[0]))
            errors = []

            # Check pak.limn exists
            if not (path / "pak.limn").exists():
                errors.append("Missing pak.limn manifest")

            # Check src/ exists
            if not (path / "src").exists():
                errors.append("Missing src/ directory")

            # Parse manifest and check entry
            manifest_path = path / "pak.limn"
            if manifest_path.exists():
                content = manifest_path.read_text()
                manifest = self._parse_manifest(content)

                if not manifest.get("nom"):
                    errors.append("Missing package name in manifest")

                entry = manifest.get("ent", "")
                if entry and not (path / entry).exists():
                    errors.append(f"Entry file not found: {entry}")

            return {
                "val": len(errors) == 0,
                "err": errors
            }
        return {"val": False, "err": ["No path provided"]}

    def call_function(self, func_name: str, args: List[Any], env: Environment) -> Optional[Any]:
        """Override to handle built-in primitives."""
        # Check for built-in primitive
        if func_name in self.builtin_funcs:
            return self.builtin_funcs[func_name](args)

        # Fall back to user-defined functions
        return super().call_function(func_name, args, env)


# ============================================================================
# Package Manager CLI (Written in Limn, executed by extended solver)
# ============================================================================

PAK_INIT_LIMN = """
# pak ini (init) - Initialize new package
# Creates package structure with manifest

whe pak_nom       # package name (input)
whe pak_dir       # directory (input or derived)
whe man_con       # manifest content
whe ent_con       # entry file content
whe res_suc       # result success
whe res_err       # result error

# Generate manifest content
whe man_tem
man_tem sa "# pak.limn - Package manifest

whe pak_nom
whe pak_ver_maj
whe pak_ver_min
whe pak_ver_pat
whe pak_aut
whe pak_des
whe pak_ent

---
pak_nom sa \\"{pak_nom}\\"
pak_ver_maj sa 1
pak_ver_min sa 0
pak_ver_pat sa 0
pak_aut sa \\"\\"
pak_des sa \\"\\"
pak_ent sa \\"src/main.limn\\"
"

# Generate entry file content
ent_con sa "# main.limn - Package entry point

whe greeting
greeting sa \\"Hello from {pak_nom}\\"

---
# key: run
"

# Create directory structure
cre cau pak_dir eff dir_res
cre cau pak_dir joi "/src" eff src_res
wri cau pak_dir joi "/pak.limn" man_tem eff man_res
wri cau pak_dir joi "/src/main.limn" ent_con eff ent_res

res_suc sa 1
"""

PAK_CHECK_LIMN = """
# pak che (check) - Validate package structure

whe pak_dir       # directory to check
whe res_val       # validation result
whe res_err       # validation errors

pak_val cau pak_dir eff val_res

res_val sa val_res
"""

PAK_BUILD_LIMN = """
# pak bui (build) - Build package bundle

whe pak_dir       # package directory
whe bui_out       # build output directory
whe res_cid       # result CID
whe res_suc       # success

# First validate
pak_val cau pak_dir eff val_res

# Build if valid (just copy to .limn-pkg for now)
# In future: bundle, minify, etc.
res_suc sa val_res
"""

PAK_PUBLISH_LIMN = """
# pak pub (publish) - Publish package to IPFS

whe pak_dir       # package directory
whe res_cid       # result CID
whe res_suc       # success
whe res_err       # error

# Check IPFS daemon
ipfs_det cau eff det_res

# Add to IPFS if daemon running
ipfs_add cau pak_dir eff add_cid

res_cid sa add_cid
res_suc sa 1
"""

PAK_INSTALL_LIMN = """
# pak ins (install) - Install dependencies

whe pak_dir       # package directory
whe res_dep       # installed dependencies
whe res_suc       # success

# Read manifest
rea cau pak_dir joi "/pak.limn" eff man_con

# Parse manifest
pak_par cau man_con eff man_res

# Install each dependency
# (For now, just return success)
res_suc sa 1
"""


def run_extended_limn(source: str, key: Dict[str, Any],
                       base_dir: str = ".") -> Dict[str, Any]:
    """Run Limn program with extended primitives."""
    parser = LimnPLParser()
    program = parser.parse(source)

    primitives = LimnHostPrimitives(base_dir)
    solver = LimnExtendedSolver(primitives=primitives)

    return solver.solve(program, key)


# ============================================================================
# CLI Interface
# ============================================================================

def pak_init(name: str, directory: Optional[str] = None):
    """Initialize a new package."""
    pak_dir = directory or name

    primitives = LimnHostPrimitives()

    # Create directories
    primitives.cre_dir(pak_dir)
    primitives.cre_dir(f"{pak_dir}/src")
    primitives.cre_dir(f"{pak_dir}/test")

    # Create manifest
    manifest = f'''# pak.limn - Package manifest

whe pak_nom
whe pak_ver_maj
whe pak_ver_min
whe pak_ver_pat
whe pak_aut
whe pak_des
whe pak_ent

---
pak_nom sa "{name}"
pak_ver_maj sa 1
pak_ver_min sa 0
pak_ver_pat sa 0
pak_aut sa ""
pak_des sa ""
pak_ent sa "src/main.limn"
'''
    primitives.wri_fil(f"{pak_dir}/pak.limn", manifest)

    # Create entry file
    entry = f'''# main.limn - Package entry point

whe greeting

---
greeting sa "Hello from {name}"
'''
    primitives.wri_fil(f"{pak_dir}/src/main.limn", entry)

    # Create README
    readme = f'''# {name}

A Limn package.

## Installation

```bash
limn pak add {name}
```

## Usage

```limn
use nom "{name}"
```
'''
    primitives.wri_fil(f"{pak_dir}/README.md", readme)

    print(f"Created package: {pak_dir}/")
    print(f"  pak.limn")
    print(f"  src/main.limn")
    print(f"  README.md")


def pak_check(directory: str = "."):
    """Check/validate a package."""
    primitives = LimnHostPrimitives(directory)
    solver = LimnExtendedSolver(primitives=primitives)

    result = solver._pak_val([directory])

    if result["val"]:
        print("Package valid!")
    else:
        print("Package validation failed:")
        for err in result["err"]:
            print(f"  - {err}")


def pak_publish(directory: str = "."):
    """Publish package to IPFS."""
    primitives = LimnHostPrimitives(directory)

    # Check IPFS
    det = primitives.ipfs_det()
    if not det.get("run"):
        print("Error: IPFS daemon not running")
        print("Start it with: ipfs daemon")
        return

    # Validate first
    solver = LimnExtendedSolver(primitives=primitives)
    val_result = solver._pak_val([directory])

    if not val_result["val"]:
        print("Error: Package validation failed")
        for err in val_result["err"]:
            print(f"  - {err}")
        return

    # Add to IPFS
    cid = primitives.ipfs_add(directory)
    if cid:
        print(f"Published: {cid}")
        print(f"\nAdd as dependency:")
        print(f"  use cid {cid}")
    else:
        print("Error: Failed to publish to IPFS")


def main():
    import sys

    if len(sys.argv) < 2:
        print("Limn Package Manager")
        print()
        print("Commands:")
        print("  limn pak init <name>     Initialize new package")
        print("  limn pak check [dir]     Validate package")
        print("  limn pak pub [dir]       Publish to IPFS")
        print("  limn pak install [dir]   Install dependencies")
        return

    cmd = sys.argv[1]

    if cmd == "init":
        if len(sys.argv) < 3:
            print("Error: Package name required")
            print("Usage: limn pak init <name>")
            return
        pak_init(sys.argv[2])

    elif cmd == "check":
        directory = sys.argv[2] if len(sys.argv) > 2 else "."
        pak_check(directory)

    elif cmd == "pub":
        directory = sys.argv[2] if len(sys.argv) > 2 else "."
        pak_publish(directory)

    else:
        print(f"Unknown command: {cmd}")


if __name__ == "__main__":
    main()
