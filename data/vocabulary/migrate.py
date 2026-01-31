#!/usr/bin/env python3
"""
Migrate Limn vocabulary from markdown to Dolt database.
Parses vocabulary-v3-natural.md and inserts into words table.
"""

import re
import subprocess
import sys

VOCAB_FILE = "../../docs/spec/vocabulary-v3-natural.md"

# Domain name to ID mapping (must match schema.sql)
DOMAIN_MAP = {
    "Physical World": 1,
    "Matter States": 1,
    "Elements & Materials": 1,
    "Physical Properties": 1,
    "Physical Actions": 1,
    "Space & Position": 2,
    "Directions": 2,
    "Positions": 2,
    "Regions & Scales": 2,
    "Time & Change": 3,
    "Time Points": 3,
    "Durations": 3,
    "Change Patterns": 3,
    "Living Things": 4,
    "Life & Death": 4,
    "Body & Health": 4,
    "Organisms": 4,
    "Mind & Cognition": 5,
    "Mental States": 5,
    "Senses": 5,
    "Emotions": 5,
    "Cognition": 5,
    "Communication": 6,
    "Speech Acts": 6,
    "Social": 7,
    "Relationships": 7,
    "Social Actions": 7,
    "Groups": 7,
    "Abstract": 8,
    "Quantities": 8,
    "Logic": 8,
    "Values": 8,
    "Meta": 8,
    "Operators": 9,
    "Unary Modifiers": 9,
    "Quantifiers": 9,
    "References": 9,
    "Comparators": 9,
    "Metalinguistic": 10,
    "Agent/AI": 11,
}

def parse_vocab_line(line):
    """Parse a markdown table row into word data."""
    # Format: | `word` | source | meaning | examples |
    match = re.match(r'\|\s*`(\w+)`\s*\|\s*([^|]+)\s*\|\s*([^|]+)\s*\|\s*([^|]*)\s*\|', line)
    if match:
        return {
            'word': match.group(1).strip(),
            'source': match.group(2).strip(),
            'meaning': match.group(3).strip(),
            'examples': match.group(4).strip(),
        }
    return None

def dolt_sql(query):
    """Execute a dolt SQL query."""
    result = subprocess.run(
        ['dolt', 'sql', '-q', query],
        capture_output=True,
        text=True
    )
    if result.returncode != 0:
        print(f"SQL Error: {result.stderr}", file=sys.stderr)
        return False
    return True

def escape_sql(s):
    """Escape single quotes for SQL."""
    return s.replace("'", "''")

def main():
    current_domain = None
    word_count = 0
    errors = []

    with open(VOCAB_FILE, 'r') as f:
        for line in f:
            line = line.strip()

            # Track current domain from headers
            if line.startswith('## Domain'):
                match = re.search(r'Domain \d+: (.+)', line)
                if match:
                    current_domain = match.group(1)
                    print(f"Processing domain: {current_domain}")
            elif line.startswith('### '):
                subsection = line[4:].strip()
                # Map subsection to domain
                if subsection in DOMAIN_MAP:
                    current_domain = subsection

            # Parse word entries
            word_data = parse_vocab_line(line)
            if word_data and current_domain:
                domain_id = DOMAIN_MAP.get(current_domain, 8)  # Default to Abstract

                query = f"""INSERT INTO words (word, source, meaning, examples, domain_id)
                           VALUES ('{word_data['word']}',
                                   '{escape_sql(word_data['source'])}',
                                   '{escape_sql(word_data['meaning'])}',
                                   '{escape_sql(word_data['examples'])}',
                                   {domain_id})
                           ON DUPLICATE KEY UPDATE
                           source='{escape_sql(word_data['source'])}',
                           meaning='{escape_sql(word_data['meaning'])}',
                           examples='{escape_sql(word_data['examples'])}',
                           domain_id={domain_id}"""

                if dolt_sql(query):
                    word_count += 1
                else:
                    errors.append(word_data['word'])

    print(f"\nMigration complete: {word_count} words inserted")
    if errors:
        print(f"Errors: {errors}")

    # Commit the changes
    subprocess.run(['dolt', 'add', '.'])
    subprocess.run(['dolt', 'commit', '-m', 'Import vocabulary from v3-natural'])

if __name__ == '__main__':
    main()
