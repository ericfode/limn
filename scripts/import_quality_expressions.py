#!/usr/bin/env python3
"""Import compositional expressions with quality filtering."""

import csv
import subprocess
import sys
from pathlib import Path
from collections import defaultdict

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
}

def escape_sql(text):
    """Escape single quotes for SQL."""
    if text is None:
        return 'NULL'
    return text.replace("'", "''").replace('\\', '\\\\')

def is_quality_example(expression, example, meaning):
    """Check if example is meaningful (not just repetition)."""
    if not example or example.strip() == '':
        return False

    # Example should not just be the expression repeated
    if example.strip() == expression.strip():
        return False

    # Example should be longer than just the expression
    if len(example.strip()) <= len(expression.strip()) + 5:
        return False

    # Should have some meaningful context
    if ' ' not in example:  # No spaces = likely just the expression
        return False

    return True

def enhance_example(expression, meaning):
    """Create a better example if needed."""
    # For simple cases, create a minimal contextual example
    return f"{expression} = {meaning.split(',')[0]}"

def import_csv_file(csv_path, db_path, stats):
    """Import a single CSV file with quality filtering."""
    print(f"\nProcessing {csv_path.name}...")

    rows_processed = 0
    rows_imported = 0
    rows_skipped = 0

    expressions = []

    with open(csv_path, 'r') as f:
        reader = csv.DictReader(f)

        for row in reader:
            rows_processed += 1

            expression = row['expression'].strip()
            meaning = row['meaning'].strip()[:100]  # Truncate to 100 chars
            example = row.get('example', '').strip()
            domain_name = row['domain'].strip().lower()

            # Skip if expression already exists in stats
            if expression in stats['seen']:
                rows_skipped += 1
                continue

            # Validate and enhance example
            if not is_quality_example(expression, example, meaning):
                example = enhance_example(expression, meaning)

            # Map domain
            domain_id = DOMAIN_MAP.get(domain_name, 5)  # Default to cognitive

            expressions.append({
                'expression': expression,
                'meaning': meaning,
                'example': example,
                'domain_id': domain_id
            })

            stats['seen'].add(expression)
            rows_imported += 1

            # Batch import every 500 expressions
            if len(expressions) >= 500:
                batch_import(expressions, db_path, stats)
                expressions = []

    # Import remaining
    if expressions:
        batch_import(expressions, db_path, stats)

    print(f"  Processed: {rows_processed}, Imported: {rows_imported}, Skipped: {rows_skipped}")
    return rows_imported, rows_skipped

def batch_import(expressions, db_path, stats):
    """Import a batch of expressions."""
    if not expressions:
        return

    # Build batch SQL
    values = []
    for expr in expressions:
        values.append(
            f"('{escape_sql(expr['expression'])}', 'compositional', "
            f"'{escape_sql(expr['meaning'])}', '{escape_sql(expr['example'])}', "
            f"{expr['domain_id']})"
        )

    sql = f"INSERT IGNORE INTO words (word, source, meaning, examples, domain_id) VALUES {', '.join(values)}"

    result = subprocess.run(
        ['dolt', 'sql', '-q', sql],
        cwd=db_path,
        capture_output=True,
        text=True
    )

    if result.returncode != 0 and 'duplicate' not in result.stderr.lower():
        print(f"    Warning: {result.stderr[:200]}")
        stats['errors'] += 1

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

    stats = {
        'seen': set(),
        'errors': 0
    }

    total_imported = 0
    total_skipped = 0

    for csv_file in csv_files:
        imported, skipped = import_csv_file(csv_file, db_path, stats)
        total_imported += imported
        total_skipped += skipped

    print()
    print("=" * 60)
    print(f"Import complete!")
    print(f"Total imported: {total_imported}")
    print(f"Total skipped: {total_skipped}")
    print(f"Errors: {stats['errors']}")
    print()

    # Check final count
    result = subprocess.run(
        ['dolt', 'sql', '-q', 'SELECT COUNT(*) as total FROM words'],
        cwd=db_path,
        capture_output=True,
        text=True
    )
    print(result.stdout)

    return 0

if __name__ == '__main__':
    sys.exit(main())
