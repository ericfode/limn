#!/usr/bin/env python3
"""Replace 'given' and 'without' operators with single characters."""

import csv
import sys
from pathlib import Path

def fix_expression(expr):
    """Replace operator keywords with symbols."""
    # Replace 'without' with backslash
    expr = expr.replace(' without ', '\\')
    expr = expr.replace('without ', '\\')
    expr = expr.replace(' without', '\\')

    # Replace 'given' with colon
    expr = expr.replace(' given ', ':')
    expr = expr.replace('given ', ':')
    expr = expr.replace(' given', ':')

    return expr

def fix_csv_file(filepath):
    """Fix operators in a CSV file."""
    print(f"Processing {filepath.name}...")

    rows = []
    changes = 0

    with open(filepath, 'r') as f:
        reader = csv.DictReader(f)
        fieldnames = reader.fieldnames

        for row in reader:
            original_expr = row['expression']
            fixed_expr = fix_expression(original_expr)

            if original_expr != fixed_expr:
                changes += 1
                print(f"  {original_expr} → {fixed_expr}")

            row['expression'] = fixed_expr
            row['meaning'] = fix_expression(row['meaning'])
            row['example'] = fix_expression(row.get('example', ''))

            rows.append(row)

    # Write back
    with open(filepath, 'w', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)

    print(f"  Fixed {changes} expressions\n")
    return changes

def main():
    """Fix all CSV files."""
    base_path = Path('/home/eric/src/limntown/limn/crew/linguist')
    generated_path = base_path / 'generated'

    csv_files = sorted(generated_path.glob('*.csv'))

    if not csv_files:
        print("No CSV files found")
        return 1

    print(f"Found {len(csv_files)} CSV files\n")

    total_changes = 0
    for csv_file in csv_files:
        changes = fix_csv_file(csv_file)
        total_changes += changes

    print("=" * 60)
    print(f"Total expressions fixed: {total_changes}")

    return 0

if __name__ == '__main__':
    sys.exit(main())
