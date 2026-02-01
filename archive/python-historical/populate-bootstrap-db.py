#!/usr/bin/env python3
"""
Populate bootstrap database from limn-dictionary.limn

Parses Limn-in-Limn definitions and populates Dolt tables.
"""

import re
import json
from datetime import date
from pathlib import Path

def parse_limn_dictionary(file_path):
    """Parse limn-dictionary.limn into structured data."""

    definitions = []
    gaps = []
    axiom_candidates = []

    current_layer = 0
    current_layer_name = ""

    with open(file_path, 'r') as f:
        lines = f.readlines()

    i = 0
    while i < len(lines):
        line = lines[i].strip()

        # Parse layer headers
        if line.startswith('## Layer'):
            match = re.match(r'## Layer (\d+): (.+)', line)
            if match:
                current_layer = int(match.group(1))
                current_layer_name = match.group(2)

        # Parse gap headers
        elif line.startswith('# GAP'):
            gap_match = re.match(r'# GAP (\d+): (.+)', line)
            if gap_match:
                gap_num = int(gap_match.group(1))
                gap_title = gap_match.group(2)

                # Collect gap description (next few lines starting with #)
                gap_lines = []
                i += 1
                while i < len(lines) and lines[i].strip().startswith('#'):
                    gap_lines.append(lines[i].strip('# \n'))
                    i += 1

                gaps.append({
                    'gap_num': gap_num,
                    'title': gap_title,
                    'description': '\n'.join(gap_lines)
                })
                continue

        # Parse definitions (format: word: definition)
        elif ':' in line and not line.startswith('#'):
            parts = line.split(':', 1)
            if len(parts) == 2:
                word = parts[0].strip()
                definition_limn = parts[1].strip()

                # Skip if empty or comment
                if not word or not definition_limn:
                    i += 1
                    continue

                # Extract words used in definition
                depends_on = extract_dependencies(definition_limn)

                # Check for circular reference indicators
                circular_with = None
                if '←' in definition_limn or 'CIRCULAR' in lines[i]:
                    circular_with = extract_circular_deps(lines, i)

                # Check for collision
                is_collision = 'COLLISION' in lines[i] or 'USE DIFFERENT WORD' in lines[i]

                definitions.append({
                    'word': word,
                    'definition_limn': definition_limn,
                    'bootstrap_layer': current_layer,
                    'layer_name': current_layer_name,
                    'depends_on': depends_on,
                    'circular_with': circular_with,
                    'is_collision': is_collision,
                    'definable': not is_collision
                })

        i += 1

    return definitions, gaps

def extract_dependencies(definition):
    """Extract 3-letter Limn words from definition."""
    # Match 3-letter words (Limn vocabulary pattern)
    words = re.findall(r'\b[a-z]{3}\b', definition)
    # Remove duplicates, keep order
    seen = set()
    unique_words = []
    for w in words:
        if w not in seen:
            seen.add(w)
            unique_words.append(w)
    return unique_words

def extract_circular_deps(lines, current_idx):
    """Extract circular dependency chain from context."""
    # Look for patterns like: kno ← und ← kno
    for offset in range(-2, 3):
        idx = current_idx + offset
        if idx < 0 or idx >= len(lines):
            continue
        line = lines[idx]
        if '←' in line:
            # Extract words in chain
            chain = re.findall(r'\b[a-z]{3,4}\b', line.replace('←', ' '))
            if len(chain) >= 2:
                return chain
    return None

def generate_sql_inserts(definitions, gaps):
    """Generate SQL INSERT statements."""

    sql = []

    # Insert definitions
    sql.append("-- Limn Definitions (from bootstrap experiment)")
    sql.append("-- Generated from limn-dictionary.limn\n")

    for d in definitions:
        # Escape single quotes
        def_limn = d['definition_limn'].replace("'", "''")

        depends_json = json.dumps(d['depends_on']) if d['depends_on'] else 'NULL'
        circular_json = json.dumps(d['circular_with']) if d['circular_with'] else 'NULL'

        gap_blocker = "'limn-ehy5'" if d['is_collision'] else 'NULL'

        sql.append(f"""
INSERT INTO limn_definitions (
  word, definition_limn, bootstrap_layer, depends_on, circular_with,
  definable, gap_blocker, created_at, updated_by
) VALUES (
  '{d['word']}',
  '{def_limn}',
  {d['bootstrap_layer']},
  '{depends_json}',
  {circular_json},
  {1 if d['definable'] else 0},
  {gap_blocker},
  '{date.today().isoformat()}',
  'translator/bootstrap-experiment'
);""")

    # Insert gaps
    sql.append("\n-- Vocabulary Gaps\n")

    gap_mappings = [
        {
            'gap_id': 'limn-ehy5',
            'gap_type': 'collision',
            'title': "COLLISION: 'thi' means both 'thing' and 'think'",
            'severity': 'critical',
            'blocked_words': ['per', 'min', 'rea', 'bel', 'dou', 'ide', 'tho'],
            'domains_blocked': ['cognitive', 'social'],
            'description': "thi is used for both 'thing' (noun) and 'think' (verb), preventing definition of mental/social concepts.",
            'proposed_solutions': [
                {'type': 'add_word', 'word': 'cog', 'meaning': 'think (from cognition)'},
                {'type': 'add_word', 'word': 'pon', 'meaning': 'think (from ponder)'}
            ]
        },
        {
            'gap_id': 'limn-dnxo',
            'gap_type': 'circular',
            'title': 'Circular definitions block abstract concepts',
            'severity': 'high',
            'blocked_words': ['kno', 'und', 'lov', 'car', 'tru', 'rea'],
            'domains_blocked': ['cognitive', 'emotional', 'epistemic'],
            'description': "Abstract concepts form circular definition chains (kno ← und ← kno). Cannot bootstrap from physical primitives alone.",
            'proposed_solutions': [
                {'type': 'accept_axioms', 'axioms': ['kno', 'fee', 'wan', 'goo', 'per']},
                {'type': 'embrace_circularity', 'reason': 'Natural languages are inherently circular'}
            ]
        },
        {
            'gap_id': 'limn-8nu6',
            'gap_type': 'missing',
            'title': 'Missing critical vocab: able, choice, suffer, purpose',
            'severity': 'high',
            'blocked_words': ['can', 'mus', 'sho', 'may', 'kar', 'nir', 'duk'],
            'domains_blocked': ['modal', 'buddhist', 'teleological'],
            'description': "8-10 critical words missing: abl (able), cho (choice), suf (suffer), pur (purpose), pos (possible), pai (pain), des (desire).",
            'proposed_solutions': [
                {'type': 'add_words', 'priority1': ['abl', 'cho', 'suf'], 'priority2': ['pur', 'pos', 'pai']}
            ]
        }
    ]

    for gap in gap_mappings:
        sql.append(f"""
INSERT INTO vocabulary_gaps (
  gap_id, gap_type, title, description, severity,
  blocked_words, domains_blocked, proposed_solutions,
  discovered_date, status
) VALUES (
  '{gap['gap_id']}',
  '{gap['gap_type']}',
  '{gap['title']}',
  '{gap['description']}',
  '{gap['severity']}',
  '{json.dumps(gap['blocked_words'])}',
  '{json.dumps(gap['domains_blocked'])}',
  '{json.dumps(gap['proposed_solutions'])}',
  '{date.today().isoformat()}',
  'open'
);""")

    # Insert axiom candidates
    sql.append("\n-- Bootstrap Axiom Candidates\n")

    axioms = [
        {
            'word': 'kno',
            'type': 'cognitive',
            'reason': 'Cannot define without circular reference to "understand". Mental state primitive.',
            'unlocks': ['und', 'lea', 'bel', 'dou', 'cer', 'wis']
        },
        {
            'word': 'fee',
            'type': 'cognitive',
            'reason': 'Cannot define sensation/emotion from physical primitives. Subjective experience is irreducible.',
            'unlocks': ['lov', 'hat', 'joy', 'sad', 'fea', 'ang', 'pai']
        },
        {
            'word': 'wan',
            'type': 'volitional',
            'reason': 'Cannot define desire from non-intentional primitives. Circular with "desire".',
            'unlocks': ['nee', 'des', 'hop', 'wis', 'pre']
        },
        {
            'word': 'goo',
            'type': 'value',
            'reason': 'Cannot derive "ought" from "is" (Hume\'s guillotine). Value judgment is primitive.',
            'unlocks': ['bad', 'rig', 'wro', 'vir', 'evi', 'mor', 'eth']
        },
        {
            'word': 'per',
            'type': 'social',
            'reason': 'Cannot define person without "think" (collision). Requires intentional stance.',
            'unlocks': ['fri', 'foe', 'fam', 'tea', 'stu', 'lea', 'fol']
        }
    ]

    for axiom in axioms:
        sql.append(f"""
INSERT INTO bootstrap_axioms (
  axiom_word, axiom_type, reason, unlocks, accepted
) VALUES (
  '{axiom['word']}',
  '{axiom['type']}',
  '{axiom['reason']}',
  '{json.dumps(axiom['unlocks'])}',
  FALSE
);""")

    # Insert initial metrics
    sql.append("\n-- Initial Bootstrap Metrics\n")
    sql.append(f"""
INSERT INTO bootstrap_metrics (
  metric_date,
  total_vocabulary,
  defined_in_limn,
  definable_count,
  axiom_count,
  open_gaps,
  critical_gaps,
  blocked_word_count,
  notes
) VALUES (
  '{date.today().isoformat()}',
  808,  -- From vocabulary-v3-natural.md
  {len(definitions)},
  {sum(1 for d in definitions if d['definable'])},
  0,  -- No axioms accepted yet
  3,  -- limn-ehy5, limn-dnxo, limn-8nu6
  2,  -- ehy5 and 8nu6 are critical
  {len(set(['per', 'min', 'rea', 'bel', 'dou', 'can', 'mus', 'sho', 'kar', 'nir']))},
  'Initial bootstrap experiment (Experiment 011). Successfully defined ~100 words before hitting fundamental gaps.'
);""")

    return '\n'.join(sql)

def main():
    """Parse dictionary and generate SQL."""

    dict_path = Path(__file__).parent / "limn-dictionary.limn"
    output_path = Path(__file__).parent / "bootstrap-data.sql"

    print(f"Parsing {dict_path}...")
    definitions, gaps = parse_limn_dictionary(dict_path)

    print(f"Found {len(definitions)} definitions")
    print(f"Found {len(gaps)} gaps documented")

    # Count by layer
    by_layer = {}
    for d in definitions:
        layer = d['bootstrap_layer']
        by_layer[layer] = by_layer.get(layer, 0) + 1

    print("\nDefinitions by layer:")
    for layer in sorted(by_layer.keys()):
        print(f"  Layer {layer}: {by_layer[layer]} words")

    print(f"\nGenerating SQL...")
    sql = generate_sql_inserts(definitions, gaps)

    with open(output_path, 'w') as f:
        f.write("-- Limn Bootstrap Data\n")
        f.write("-- Generated from limn-dictionary.limn\n")
        f.write(f"-- Date: {date.today().isoformat()}\n")
        f.write("-- Experiment: 011 (Limn-in-Limn bootstrap)\n\n")
        f.write(sql)

    print(f"Wrote SQL to {output_path}")
    print("\nTo load into Dolt:")
    print("  cd <dolt-repo>")
    print(f"  dolt sql < {output_path}")
    print("  dolt add .")
    print("  dolt commit -m 'Add bootstrap experiment data'")

if __name__ == "__main__":
    main()
