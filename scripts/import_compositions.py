#!/usr/bin/env python3
"""Import compositional expressions from CSV files into Dolt database."""

import csv
import subprocess
import sys
from pathlib import Path

# Domain name to ID mapping
DOMAIN_MAP = {
    'physical': 1,
    'spatial': 2,
    'temporal': 3,
    'living': 4,
    'cognitive': 5,
    'communication': 6,
    'social': 7,
    'abstract': 8,
    'operators': 9,
    'metalinguistic': 10,
}

def escape_sql(text):
    """Escape single quotes for SQL."""
    if text is None:
        return 'NULL'
    return text.replace("'", "''")

def import_csv_file(csv_path, db_path):
    """Import a single CSV file into the database."""
    print(f"Processing {csv_path.name}...")

    rows_added = 0
    rows_skipped = 0

    with open(csv_path, 'r') as f:
        reader = csv.DictReader(f)

        for row in reader:
            expression = row['expression'].strip()
            meaning = row['meaning'].strip()[:100]  # Truncate to 100 chars
            example = row.get('example', expression).strip()
            domain_name = row['domain'].strip().lower()

            # Map domain name to ID
            domain_id = DOMAIN_MAP.get(domain_name, 5)  # Default to cognitive

            # Build SQL insert
            sql = f"""INSERT IGNORE INTO words (word, source, meaning, examples, domain_id)
                      VALUES ('{escape_sql(expression)}', 'compositional',
                              '{escape_sql(meaning)}', '{escape_sql(example)}', {domain_id})"""

            # Execute via dolt sql
            result = subprocess.run(
                ['dolt', 'sql', '-q', sql],
                cwd=db_path,
                capture_output=True,
                text=True
            )

            if result.returncode == 0:
                rows_added += 1
            else:
                if 'duplicate' not in result.stderr.lower():
                    print(f"  Error on {expression}: {result.stderr}")
                rows_skipped += 1

            # Progress indicator
            if (rows_added + rows_skipped) % 100 == 0:
                print(f"  Processed {rows_added + rows_skipped} rows...")

    print(f"  Added: {rows_added}, Skipped: {rows_skipped}")
    return rows_added, rows_skipped

def main():
    """Main import function."""
    # Paths
    base_path = Path('/home/eric/src/limntown/limn/crew/linguist')
    generated_path = base_path / 'generated'
    db_path = base_path / 'data' / 'vocabulary'

    # Find all CSV files
    csv_files = sorted(generated_path.glob('*.csv'))

    if not csv_files:
        print("No CSV files found in generated/ directory")
        return 1

    print(f"Found {len(csv_files)} CSV files to import")
    print()

    total_added = 0
    total_skipped = 0

    for csv_file in csv_files:
        added, skipped = import_csv_file(csv_file, db_path)
        total_added += added
        total_skipped += skipped
        print()

    print("=" * 60)
    print(f"Import complete!")
    print(f"Total added: {total_added}")
    print(f"Total skipped: {total_skipped}")
    print()

    # Check final count
    result = subprocess.run(
        ['dolt', 'sql', '-q', 'SELECT COUNT(*) FROM words'],
        cwd=db_path,
        capture_output=True,
        text=True
    )
    print(result.stdout)

    return 0

if __name__ == '__main__':
    sys.exit(main())
