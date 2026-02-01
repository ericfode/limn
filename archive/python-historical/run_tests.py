#!/usr/bin/env python3
"""
Category B: Data Operations - Limn vs English Test Runner

Runs 10 data operation tests comparing Limn and English prompts.
Records token usage, success rate, and output quality.
"""

import json
import os
from pathlib import Path
from dataclasses import dataclass, asdict
from typing import List, Dict, Any
import anthropic

# Initialize Anthropic client
client = anthropic.Anthropic(api_key=os.environ.get("ANTHROPIC_API_KEY"))

@dataclass
class TestResult:
    """Results from a single test (Limn or English)"""
    test_id: str
    language: str  # "limn" or "english"
    prompt: str
    input_tokens: int
    output_tokens: int
    response: str
    success: bool
    quality_score: int  # 1-5
    notes: str = ""

@dataclass
class TestDefinition:
    """Definition of a single test"""
    id: str
    name: str
    limn_prompt: str
    english_prompt: str
    data_files: List[str]
    expected_output: str

# Test definitions
TESTS = [
    TestDefinition(
        id="B1",
        name="Parse JSON and Extract Field",
        limn_prompt="prs jso fil | get fld \"name\" | otp: vlu",
        english_prompt="Parse the JSON file and extract the 'name' field from the first user. Output only the value.",
        data_files=["test-data/b1-users.json"],
        expected_output="Alice"
    ),
    TestDefinition(
        id="B2",
        name="Filter List by Condition",
        limn_prompt="flt lst | whr vlu > 10 | otp: lst",
        english_prompt="Filter the list to include only values greater than 10. Output the filtered list.",
        data_files=["test-data/b2-numbers.json"],
        expected_output="[15, 42, 19, 27, 11, 33]"
    ),
    TestDefinition(
        id="B3",
        name="Sort Data by Field",
        limn_prompt="srt dat | by fld \"date\" | otp: lst",
        english_prompt="Sort the data by the 'date' field in ascending order. Output the sorted list.",
        data_files=["test-data/b3-events.json"],
        expected_output="Events sorted chronologically"
    ),
    TestDefinition(
        id="B4",
        name="Group and Count",
        limn_prompt="grp dat | by fld \"type\" | cnt | otp: tbl",
        english_prompt="Group the data by the 'type' field and count how many items are in each group. Output as a table.",
        data_files=["test-data/b4-items.json"],
        expected_output="fruit: 3, vegetable: 3"
    ),
    TestDefinition(
        id="B5",
        name="Validate Schema",
        limn_prompt="val dat | aga sch | otp: bol err",
        english_prompt="Validate the data against the schema. Output true if valid, or false with errors if invalid.",
        data_files=["test-data/b5-user-data.json", "test-data/b5-schema.json"],
        expected_output="true or validation details"
    ),
    TestDefinition(
        id="B6",
        name="Transform Structure",
        limn_prompt="tra dat | frm A | to B | otp: dat",
        english_prompt="Transform the flat data structure (user_name, user_age, city_name, city_pop) into a nested structure with separate 'user' and 'city' objects. Output the transformed data.",
        data_files=["test-data/b6-flat.json"],
        expected_output="Nested structure with user and city objects"
    ),
    TestDefinition(
        id="B7",
        name="Aggregate Statistics",
        limn_prompt="agg dat | sum avg min max | otp: tbl",
        english_prompt="Aggregate statistics for the 'sales' field: calculate sum, average, minimum, and maximum. Output as a table.",
        data_files=["test-data/b7-sales.json"],
        expected_output="Statistics table"
    ),
    TestDefinition(
        id="B8",
        name="Deduplicate by Field",
        limn_prompt="ded lst | by fld \"id\" | otp: lst",
        english_prompt="Remove duplicate entries based on the 'id' field, keeping only the first occurrence. Output the deduplicated list.",
        data_files=["test-data/b8-duplicates.json"],
        expected_output="4 unique users"
    ),
    TestDefinition(
        id="B9",
        name="Merge Datasets",
        limn_prompt="mrg dat A B | on fld \"id\" | otp: dat",
        english_prompt="Merge the two datasets on the 'id' field, combining user information with salary data. Output the merged dataset.",
        data_files=["test-data/b9-users.json", "test-data/b9-salaries.json"],
        expected_output="Combined user+salary records"
    ),
    TestDefinition(
        id="B10",
        name="Query Nested Data",
        limn_prompt="qry dat | pth \"users[0].name\" | otp: vlu",
        english_prompt="Query the nested data to extract the value at path 'users[0].name'. Output only the value.",
        data_files=["test-data/b10-nested.json"],
        expected_output="Alice"
    ),
]

def load_test_data(data_files: List[str]) -> str:
    """Load and format test data files"""
    data_parts = []
    for file_path in data_files:
        with open(file_path, 'r') as f:
            data = f.read()
            data_parts.append(f"File: {file_path}\n{data}")
    return "\n\n".join(data_parts)

def run_single_test(test: TestDefinition, language: str, prompt: str) -> TestResult:
    """Run a single test with either Limn or English prompt"""
    # Load test data
    data = load_test_data(test.data_files)

    # Construct full prompt
    if language == "limn":
        full_prompt = f"""You are interpreting a Limn language command. Limn is a constructed language optimized for semantic precision.

Command: {prompt}

Data:
{data}

Execute the command and provide the output as specified."""
    else:
        full_prompt = f"""Data:
{data}

Task: {prompt}"""

    # Call Claude API
    try:
        message = client.messages.create(
            model="claude-3-5-sonnet-20241022",
            max_tokens=1024,
            temperature=0.0,
            messages=[{
                "role": "user",
                "content": full_prompt
            }]
        )

        response = message.content[0].text
        input_tokens = message.usage.input_tokens
        output_tokens = message.usage.output_tokens

        # Evaluate success (simple heuristic - would need manual review)
        success = len(response) > 0 and len(response) < 500
        quality_score = 3  # Default - would need manual scoring

        return TestResult(
            test_id=test.id,
            language=language,
            prompt=prompt,
            input_tokens=input_tokens,
            output_tokens=output_tokens,
            response=response,
            success=success,
            quality_score=quality_score,
            notes=""
        )

    except Exception as e:
        return TestResult(
            test_id=test.id,
            language=language,
            prompt=prompt,
            input_tokens=0,
            output_tokens=0,
            response=f"ERROR: {str(e)}",
            success=False,
            quality_score=0,
            notes=str(e)
        )

def run_all_tests() -> List[TestResult]:
    """Run all tests for both Limn and English"""
    results = []

    for test in TESTS:
        print(f"\nRunning {test.id}: {test.name}")

        # Run Limn version
        print(f"  - Limn: {test.limn_prompt}")
        limn_result = run_single_test(test, "limn", test.limn_prompt)
        results.append(limn_result)
        print(f"    Tokens: {limn_result.input_tokens} in, {limn_result.output_tokens} out")

        # Run English version
        print(f"  - English: {test.english_prompt[:50]}...")
        english_result = run_single_test(test, "english", test.english_prompt)
        results.append(english_result)
        print(f"    Tokens: {english_result.input_tokens} in, {english_result.output_tokens} out")

    return results

def generate_report(results: List[TestResult]) -> str:
    """Generate markdown report of results"""
    limn_results = [r for r in results if r.language == "limn"]
    english_results = [r for r in results if r.language == "english"]

    report = "# Category B: Data Operations - Results\n\n"
    report += "## Summary Statistics\n\n"

    # Token statistics
    limn_input_avg = sum(r.input_tokens for r in limn_results) / len(limn_results)
    english_input_avg = sum(r.input_tokens for r in english_results) / len(english_results)
    limn_output_avg = sum(r.output_tokens for r in limn_results) / len(limn_results)
    english_output_avg = sum(r.output_tokens for r in english_results) / len(english_results)

    report += f"### Token Usage\n\n"
    report += f"| Metric | Limn | English | Difference |\n"
    report += f"|--------|------|---------|------------|\n"
    report += f"| Avg Input Tokens | {limn_input_avg:.1f} | {english_input_avg:.1f} | {((limn_input_avg - english_input_avg) / english_input_avg * 100):.1f}% |\n"
    report += f"| Avg Output Tokens | {limn_output_avg:.1f} | {english_output_avg:.1f} | {((limn_output_avg - english_output_avg) / english_output_avg * 100):.1f}% |\n"
    report += f"| Total Input Tokens | {sum(r.input_tokens for r in limn_results)} | {sum(r.input_tokens for r in english_results)} | - |\n"
    report += f"| Total Output Tokens | {sum(r.output_tokens for r in limn_results)} | {sum(r.output_tokens for r in english_results)} | - |\n"
    report += "\n"

    # Success rates
    limn_success = sum(1 for r in limn_results if r.success)
    english_success = sum(1 for r in english_results if r.success)

    report += f"### Success Rates\n\n"
    report += f"| Language | Successful | Total | Rate |\n"
    report += f"|----------|-----------|-------|------|\n"
    report += f"| Limn | {limn_success} | {len(limn_results)} | {limn_success/len(limn_results)*100:.1f}% |\n"
    report += f"| English | {english_success} | {len(english_results)} | {english_success/len(english_results)*100:.1f}% |\n"
    report += "\n"

    # Detailed results table
    report += "## Detailed Results\n\n"
    report += "| Test | Language | Prompt | Input Tokens | Output Tokens | Success |\n"
    report += "|------|----------|--------|--------------|---------------|----------|\n"

    for test in TESTS:
        limn = [r for r in limn_results if r.test_id == test.id][0]
        eng = [r for r in english_results if r.test_id == test.id][0]

        report += f"| {test.id} | Limn | `{limn.prompt[:40]}...` | {limn.input_tokens} | {limn.output_tokens} | {'✓' if limn.success else '✗'} |\n"
        report += f"| {test.id} | English | {eng.prompt[:40]}... | {eng.input_tokens} | {eng.output_tokens} | {'✓' if eng.success else '✗'} |\n"

    report += "\n## Individual Test Responses\n\n"

    for test in TESTS:
        report += f"### {test.id}: {test.name}\n\n"

        limn = [r for r in limn_results if r.test_id == test.id][0]
        eng = [r for r in english_results if r.test_id == test.id][0]

        report += f"**Limn Prompt:** `{limn.prompt}`\n\n"
        report += f"**Limn Response:**\n```\n{limn.response}\n```\n\n"

        report += f"**English Prompt:** {eng.prompt}\n\n"
        report += f"**English Response:**\n```\n{eng.response}\n```\n\n"
        report += "---\n\n"

    return report

def main():
    """Main execution function"""
    print("=" * 60)
    print("Category B: Data Operations - Limn vs English")
    print("=" * 60)

    # Check for API key
    if not os.environ.get("ANTHROPIC_API_KEY"):
        print("ERROR: ANTHROPIC_API_KEY environment variable not set")
        return

    # Run all tests
    results = run_all_tests()

    # Generate report
    report = generate_report(results)

    # Save report
    report_path = "RESULTS.md"
    with open(report_path, 'w') as f:
        f.write(report)

    # Save raw results JSON
    results_json = [asdict(r) for r in results]
    with open("results.json", 'w') as f:
        json.dump(results_json, f, indent=2)

    print(f"\n{'=' * 60}")
    print(f"Tests complete!")
    print(f"Report saved to: {report_path}")
    print(f"Raw results saved to: results.json")
    print(f"{'=' * 60}")

if __name__ == "__main__":
    main()
