#!/usr/bin/env python3
"""Validate beads have proper specifications before assignment."""

import subprocess
import json
import sys
from typing import Dict, List, Tuple

def check_bead_quality(bead: Dict) -> Tuple[bool, List[str]]:
    """Check if a bead has sufficient specification for work."""
    issues = []

    # Check description
    description = bead.get('description', '').strip()
    if not description:
        issues.append("❌ Missing description")
    elif len(description) < 50:
        issues.append(f"⚠️  Description too short ({len(description)} chars, recommend 50+)")

    # Check if description has structure
    if description and not any(marker in description for marker in ['##', '**', '-', '1.', '*']):
        issues.append("⚠️  Description lacks structure (no headers, lists, or formatting)")

    # Check for task-specific requirements
    task_type = bead.get('type', '')
    if task_type in ['task', 'bug', 'feature']:
        # Should have deliverables or acceptance criteria
        if not any(term in description.lower() for term in [
            'deliverable', 'acceptance', 'success criteria', 'expected',
            'should', 'must', 'requirement'
        ]):
            issues.append("⚠️  Missing clear deliverables or acceptance criteria")

    # Check assignee
    assignee = bead.get('assignee', '')
    if assignee and issues:
        issues.insert(0, f"⚠️  Bead assigned to {assignee} but has quality issues:")

    return len(issues) == 0, issues

def validate_ready_beads(limit: int = 20):
    """Check all ready-to-work beads for quality."""
    print("🔍 BEAD QUALITY VALIDATOR")
    print("=" * 70)

    # Get ready beads
    result = subprocess.run(
        ['bd', 'ready', '-u', f'--limit={limit}', '--json'],
        capture_output=True,
        text=True
    )

    if result.returncode != 0:
        print(f"Error fetching ready beads: {result.stderr}")
        return

    try:
        beads = json.loads(result.stdout)
    except json.JSONDecodeError:
        print("Error parsing JSON")
        return

    if not beads:
        print("✅ No unassigned ready beads to validate")
        return

    print(f"\nChecking {len(beads)} unassigned ready beads...\n")

    good_count = 0
    warning_count = 0
    bad_count = 0

    for bead in beads:
        bead_id = bead.get('id', 'unknown')
        title = bead.get('title', 'untitled')

        is_good, issues = check_bead_quality(bead)

        if is_good:
            print(f"✅ {bead_id}: {title[:50]}")
            good_count += 1
        elif any('❌' in issue for issue in issues):
            print(f"\n❌ {bead_id}: {title[:50]}")
            for issue in issues:
                print(f"   {issue}")
            bad_count += 1
        else:
            print(f"\n⚠️  {bead_id}: {title[:50]}")
            for issue in issues:
                print(f"   {issue}")
            warning_count += 1

    print("\n" + "=" * 70)
    print(f"Summary: {good_count} good, {warning_count} warnings, {bad_count} critical issues")

    if bad_count > 0:
        print(f"\n💡 Recommendation: Fix {bad_count} beads with critical issues before assignment")

def main():
    """Main entry point."""
    if len(sys.argv) > 1:
        # Validate specific bead
        bead_id = sys.argv[1]
        result = subprocess.run(
            ['bd', 'show', bead_id, '--json'],
            capture_output=True,
            text=True
        )
        if result.returncode != 0:
            print(f"Error fetching bead: {result.stderr}")
            sys.exit(1)

        try:
            bead = json.loads(result.stdout)
        except json.JSONDecodeError:
            print("Error parsing JSON")
            sys.exit(1)

        is_good, issues = check_bead_quality(bead)

        if is_good:
            print(f"✅ {bead_id} is ready for assignment")
        else:
            print(f"\n{bead_id} has quality issues:")
            for issue in issues:
                print(f"  {issue}")
            sys.exit(1)
    else:
        # Validate all ready beads
        validate_ready_beads()

if __name__ == "__main__":
    main()
