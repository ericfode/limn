#!/usr/bin/env python3
"""
Limn CLI - Run Limn programs and package management commands.

Commands:
    run <file.limn>     Run a Limn program
    pak <command>       Package management (IPFS-based)
    repl                Start interactive REPL
    version             Show version info
"""

import sys
import os
from pathlib import Path

# Add src directory to path
src_dir = Path(__file__).parent
sys.path.insert(0, str(src_dir))

from limn_pl_interpreter import LimnPLParser, parse_key
from limn_extended import LimnExtendedSolver, LimnHostPrimitives


# ============================================================================
# Version
# ============================================================================

__version__ = "0.2.0"


# ============================================================================
# File Loading and Parsing
# ============================================================================

def load_limn_file(filepath: Path) -> str:
    """Load a .limn file content."""
    return filepath.read_text(encoding='utf-8')


def preprocess_source(content: str, base_dir: Path = None) -> str:
    """
    Preprocess source code to handle imports.
    Expands 'use' statements to inline imported definitions.
    """
    # Check if there are any use statements
    has_imports = any(line.strip().startswith('use ') for line in content.split('\n'))

    if has_imports:
        try:
            from limn_import import preprocess_imports
            return preprocess_imports(content)
        except ImportError:
            # Import system not available, return as-is
            pass
        except Exception as e:
            print(f"Warning: Import preprocessing failed: {e}")

    return content


def parse_limn_file(content: str):
    """Parse a .limn file into program and key sections."""
    # Split on --- separator
    if '---' in content:
        parts = content.split('---', 1)
        program_text = parts[0].strip()
        key_text = parts[1].strip() if len(parts) > 1 else ""
    else:
        program_text = content.strip()
        key_text = ""

    # Build proper program structure
    var_lines = []
    cns_lines = []

    for line in program_text.split('\n'):
        line = line.strip()
        if line.startswith('#') or not line:
            continue
        if line.startswith('whe '):
            var_lines.append(line)
        else:
            cns_lines.append(line)

    # Build program string
    program_parts = ["pro limn_program", "var"] + var_lines + ["cns"] + cns_lines
    program = ' | '.join(program_parts)

    # Parse key
    key_bindings = {}
    if key_text:
        # Add 'yo' prefix to key lines
        key_lines = []
        for line in key_text.split('\n'):
            line = line.strip()
            if line.startswith('#') or not line:
                continue
            if not line.startswith('yo '):
                line = 'yo ' + line
            key_lines.append(line)

        key_source = ' | '.join(key_lines)
        key_bindings = parse_key(key_source)

    return program, key_bindings


def run_limn_program(filepath: Path, additional_bindings: dict = None,
                     verbose: bool = False, preprocess: bool = True):
    """Run a .limn program file."""
    content = load_limn_file(filepath)

    # Preprocess imports
    if preprocess:
        content = preprocess_source(content, filepath.parent)

    program_source, key_bindings = parse_limn_file(content)

    # Merge additional bindings
    if additional_bindings:
        key_bindings.update(additional_bindings)

    if verbose:
        print(f"Program: {program_source[:200]}...")
        print(f"Key: {key_bindings}")

    # Parse and solve
    parser = LimnPLParser()
    program = parser.parse(program_source)

    # Use extended solver for I/O primitives
    primitives = LimnHostPrimitives(str(filepath.parent))
    solver = LimnExtendedSolver(primitives=primitives)

    try:
        result = solver.solve(program, key_bindings)
        return result
    except Exception as e:
        if verbose:
            import traceback
            traceback.print_exc()
        print(f"Error: {e}")
        return None


# ============================================================================
# Commands
# ============================================================================

def cmd_run(args):
    """Run a .limn file."""
    if len(args) < 1:
        print("Usage: limn run <file.limn> [key=value ...] [--verbose]")
        return 1

    filepath = Path(args[0])
    if not filepath.exists():
        print(f"Error: File not found: {filepath}")
        return 1

    # Parse options and key bindings
    bindings = {}
    verbose = False

    for arg in args[1:]:
        if arg == "--verbose" or arg == "-v":
            verbose = True
        elif '=' in arg:
            key, value = arg.split('=', 1)
            # Try to parse as number
            try:
                if '.' in value:
                    bindings[key] = float(value)
                else:
                    bindings[key] = int(value)
            except ValueError:
                # Remove quotes if present
                if value.startswith('"') and value.endswith('"'):
                    value = value[1:-1]
                bindings[key] = value

    result = run_limn_program(filepath, bindings, verbose=verbose)
    if result:
        print("\nResult:")
        for key, value in sorted(result.items()):
            print(f"  {key} = {value}")
        return 0
    return 1


def cmd_pak(args):
    """Package management - delegate to full pak CLI."""
    # Import and run the full pak CLI
    from limn_pak_cli import main as pak_main

    # Reconstruct argv for pak CLI
    sys.argv = ["limn-pak"] + args
    return pak_main()


def cmd_repl(args):
    """Start interactive REPL."""
    print("Limn REPL v" + __version__)
    print("Enter Limn constraints. Use 'exit' to quit.")
    print("Enter key bindings after '---'")
    print()

    primitives = LimnHostPrimitives()
    solver = LimnExtendedSolver(primitives=primitives)
    parser = LimnPLParser()

    program_lines = []
    key_lines = []
    in_key = False

    while True:
        try:
            prompt = "key> " if in_key else ">>> "
            line = input(prompt)

            if line.strip().lower() == 'exit':
                break

            if line.strip() == '---':
                in_key = True
                continue

            if line.strip() == 'run':
                # Execute accumulated program
                if not program_lines:
                    print("No program to run")
                    continue

                # Build program
                var_lines = [l for l in program_lines if l.strip().startswith('whe ')]
                cns_lines = [l for l in program_lines if not l.strip().startswith('whe ')]

                program_parts = ["pro repl", "var"] + var_lines + ["cns"] + cns_lines
                program_source = ' | '.join(program_parts)

                # Parse key
                key_bindings = {}
                if key_lines:
                    key_source = ' | '.join(['yo ' + l if not l.startswith('yo ') else l
                                             for l in key_lines])
                    key_bindings = parse_key(key_source)

                try:
                    program = parser.parse(program_source)
                    result = solver.solve(program, key_bindings)
                    print("Result:")
                    for k, v in sorted(result.items()):
                        print(f"  {k} = {v}")
                except Exception as e:
                    print(f"Error: {e}")

                continue

            if line.strip() == 'clear':
                program_lines = []
                key_lines = []
                in_key = False
                print("Cleared")
                continue

            if line.strip() == 'show':
                print("Program:")
                for l in program_lines:
                    print(f"  {l}")
                if key_lines:
                    print("---")
                    print("Key:")
                    for l in key_lines:
                        print(f"  {l}")
                continue

            # Add line to program or key
            if line.strip():
                if in_key:
                    key_lines.append(line.strip())
                else:
                    program_lines.append(line.strip())

        except EOFError:
            break
        except KeyboardInterrupt:
            print("\nUse 'exit' to quit")

    print("Goodbye!")
    return 0


def cmd_version(args):
    """Show version info."""
    print(f"Limn v{__version__}")
    print("  Constraint-based programming language")
    print("  IPFS-based package management")
    print()

    # Check IPFS
    try:
        import subprocess
        result = subprocess.run(["ipfs", "version"], capture_output=True, text=True, timeout=5)
        if result.returncode == 0:
            print(f"  IPFS: {result.stdout.strip()}")
        else:
            print("  IPFS: not running")
    except Exception:
        print("  IPFS: not available")

    return 0


def print_help():
    """Print help message."""
    print(f"Limn v{__version__} - Constraint-Based Programming Language")
    print()
    print("Usage:")
    print("  limn <file.limn>              Run a Limn program directly")
    print("  limn run <file.limn>          Run a Limn program")
    print("  limn pak <command>            Package management (IPFS-based)")
    print("  limn repl                     Start interactive REPL")
    print("  limn version                  Show version info")
    print()
    print("Running programs:")
    print("  limn run example.limn                 Run with default key")
    print("  limn run example.limn x=5 y=10       Override key bindings")
    print("  limn run example.limn --verbose      Show debug info")
    print()
    print("Package management:")
    print("  limn pak init <name>          Create new package")
    print("  limn pak install              Install dependencies")
    print("  limn pak pub                  Publish to IPFS")
    print("  limn pak search <query>       Search registry")
    print()
    print("Run 'limn pak help' for full package manager documentation.")


def main():
    if len(sys.argv) < 2:
        print_help()
        return 0

    cmd = sys.argv[1]
    args = sys.argv[2:]

    if cmd == "run":
        return cmd_run(args)
    elif cmd == "pak":
        return cmd_pak(args)
    elif cmd == "repl":
        return cmd_repl(args)
    elif cmd == "version" or cmd == "--version" or cmd == "-V":
        return cmd_version(args)
    elif cmd == "help" or cmd == "--help" or cmd == "-h":
        print_help()
        return 0
    elif cmd.endswith(".limn"):
        # Direct file execution: limn myfile.limn
        return cmd_run([cmd] + args)
    else:
        print(f"Unknown command: {cmd}")
        print("Run 'limn help' for usage.")
        return 1


if __name__ == "__main__":
    sys.exit(main() or 0)
