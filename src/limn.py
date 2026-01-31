#!/usr/bin/env python3
"""
Limn-PL File Runner

Usage:
    python limn.py <file.limn> [--key "a sa 5 | b sa 3"]
    python limn.py <file.limn> --input input.json

File Format (.limn):
    # Comments start with #
    # Empty lines are ignored

    # Variables with whe
    whe x | whe y | whe result

    # Constraints
    x joi y sa result

    # Key (input values) with ---
    ---
    x sa 10
    y sa 20
"""

import sys
import os
import json
import argparse

# Add src directory to path
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from limn_pl_interpreter import LimnPLParser, LimnPLSolver, parse_key, run_limn_pl


def parse_limn_file(filepath):
    """Parse a .limn file and return program text and key text."""
    with open(filepath, 'r', encoding='utf-8') as f:
        content = f.read()

    # Split on key separator
    if '---' in content:
        parts = content.split('---', 1)
        program_text = parts[0].strip()
        key_text = parts[1].strip() if len(parts) > 1 else ""
    else:
        program_text = content.strip()
        key_text = ""

    # Parse program lines
    var_lines = []
    cns_lines = []

    for line in program_text.split('\n'):
        line = line.split('#')[0].strip()  # Remove comments
        if not line:
            continue
        # Check if it's a variable declaration
        if line.startswith('whe '):
            var_lines.append(line)
        elif line:
            cns_lines.append(line)

    # Build proper program structure
    # Format: pro name | var | whe x | whe y | cns | constraints
    parts = []

    # Get filename as program name
    import os
    prog_name = os.path.splitext(os.path.basename(filepath))[0].replace('_', '-')
    parts.append(f"pro {prog_name}")

    # Add variables
    if var_lines:
        parts.append("var")
        parts.extend(var_lines)

    # Add constraints
    if cns_lines:
        parts.append("cns")
        parts.extend(cns_lines)

    program = ' | '.join(parts)

    # Parse key lines and convert to yo format
    # Input format: a sa 10
    # Output format: yo a sa 10
    key_lines = []
    for line in key_text.split('\n'):
        line = line.split('#')[0].strip()
        if line:
            # Add 'yo' prefix if not present
            if not line.startswith('yo '):
                line = 'yo ' + line
            key_lines.append(line)
    key = ' | '.join(key_lines) if key_lines else ""

    return program, key


def run_limn_file(filepath, key_override=None, input_file=None, verbose=False):
    """Run a .limn file and return the result."""
    # Parse the file
    program, file_key = parse_limn_file(filepath)

    # Use key override if provided
    if key_override:
        # Add 'yo' prefix to each binding if not present
        key_parts = []
        for part in key_override.split('|'):
            part = part.strip()
            if part and not part.startswith('yo '):
                part = 'yo ' + part
            if part:
                key_parts.append(part)
        key = ' | '.join(key_parts)
    elif input_file:
        with open(input_file, 'r') as f:
            input_data = json.load(f)
        # Convert JSON to Limn key format with 'yo' prefix
        key_parts = [f"yo {k} sa {v}" for k, v in input_data.items()]
        key = ' | '.join(key_parts)
    else:
        key = file_key

    if verbose:
        print(f"# pro (program):")
        print(f"  {program}")
        print(f"# key (input):")
        print(f"  {key if key else '(nu key)'}")
        print()

    # Parse key if provided
    key_dict = parse_key(key) if key else {}

    try:
        result = run_limn_pl(program, key_dict)
        return result
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        return None


def format_result(result):
    """Format result dictionary for output."""
    if not result:
        return "# nu sol (no solution)"

    lines = ["# sol (solution):"]
    for var, val in result.items():
        lines.append(f"  {var} sa {val}")
    return '\n'.join(lines)


def main():
    parser = argparse.ArgumentParser(
        description='Limn-PL File Runner',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
    python limn.py program.limn
    python limn.py program.limn --key "x sa 5"
    python limn.py program.limn --input values.json
    python limn.py program.limn -v
        """
    )
    parser.add_argument('file', help='.limn file to run')
    parser.add_argument('--key', '-k', help='Key (input) as Limn sentence')
    parser.add_argument('--input', '-i', help='JSON file with input values')
    parser.add_argument('--verbose', '-v', action='store_true', help='Show program and key')
    parser.add_argument('--json', '-j', action='store_true', help='Output as JSON')

    args = parser.parse_args()

    # Check file exists
    if not os.path.exists(args.file):
        print(f"Error: File not found: {args.file}", file=sys.stderr)
        sys.exit(1)

    # Run the file
    result = run_limn_file(
        args.file,
        key_override=args.key,
        input_file=args.input,
        verbose=args.verbose
    )

    # Output result
    if args.json:
        print(json.dumps(result, indent=2))
    else:
        print(format_result(result))


if __name__ == '__main__':
    main()
