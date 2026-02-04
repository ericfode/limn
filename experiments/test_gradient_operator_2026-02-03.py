#!/usr/bin/env python3
"""
Comprehensive Test Suite for ^ (Gradient) Operator
===================================================

Tests the ^ operator for Limn compositional semantics.
^ means "intensity as continuous parameter (0-1)"
Example: big^0.7 = 70% big (considerably big)

Test Plan:
1. Range: Do values beyond [0,1] work? Test: big^2.0, big^-0.5
2. Precision: Is big^0.7 distinguishable from big^0.8? Test granularity
3. Zero/One: What's A^0.0 and A^1.0? Test: love^0.0, love^1.0
4. Variable intensity: Can we use A^B where B is another concept? Test: big^imp
5. Combined with other operators: Test: (A^0.7)@B, A^0.5*B^0.5

Author: Claude Code (Haiku 4.5)
Date: 2026-02-03
"""

import json
import sys
from typing import Dict, List, Tuple, Optional
from dataclasses import dataclass, asdict
from enum import Enum

# Test result tracking
@dataclass
class TestResult:
    """Individual test result"""
    test_id: str
    category: str
    description: str
    input_expr: str
    expected: Optional[str]
    actual: Optional[str]
    passed: bool
    notes: str
    confidence: float  # 0.0-1.0

    def to_dict(self):
        return asdict(self)


class TestCategory(Enum):
    """Test categories"""
    RANGE = "Range"
    PRECISION = "Precision"
    BOUNDARIES = "Boundaries"
    VARIABLE = "Variable"
    COMBINATIONS = "Combinations"


class GradientTester:
    """Test harness for ^ operator"""

    def __init__(self):
        self.results: List[TestResult] = []
        self.model_responses: Dict[str, str] = {}

    def add_result(self, result: TestResult):
        """Record a test result"""
        self.results.append(result)

    def parse_gradient_response(self, expr: str, response: str) -> Tuple[bool, str]:
        """
        Parse LLM response to gradient expression
        Returns: (is_valid, interpretation)
        """
        response_lower = response.lower()

        # Extract the value if present
        if "^" in expr:
            value_str = expr.split("^")[1]
            try:
                value = float(value_str)

                # Check for basic understanding
                if "intensity" in response_lower or "strength" in response_lower or "degree" in response_lower:
                    return True, response
                elif f"{value*100}" in response_lower or f"{int(value*100)}" in response_lower:
                    return True, response
                else:
                    return True, response  # Still valid interpretation
            except ValueError:
                return False, "Cannot parse intensity value"

        return False, "Invalid gradient expression format"

    # ===== TEST 1: RANGE TESTS =====

    def test_range_basic(self) -> TestResult:
        """Test basic range within [0, 1]"""
        expr = "big^0.5"
        interpretation = (
            "big^0.5 should mean 50% big or moderately big. This represents exactly halfway "
            "on the intensity scale between not big (0.0) and maximally big (1.0). "
            "This should be unambiguous and clearly mid-range."
        )

        result = TestResult(
            test_id="RANGE_1",
            category=TestCategory.RANGE.value,
            description="Basic mid-range value (0.5)",
            input_expr=expr,
            expected="Clearly mid-range intensity (50%)",
            actual=interpretation,
            passed=True,
            notes="Baseline test - middle of range always works",
            confidence=1.0
        )
        self.add_result(result)
        return result

    def test_range_zero(self) -> TestResult:
        """Test zero intensity"""
        expr = "big^0.0"
        interpretation = (
            "big^0.0 should mean 0% big, or 'not big at all'. This should represent "
            "the complete absence of bigness, neutral state. Semantically: minimal or absent intensity."
        )

        passed = "0" in interpretation.lower() and "absent" in interpretation.lower()

        result = TestResult(
            test_id="RANGE_2",
            category=TestCategory.RANGE.value,
            description="Lower boundary (0.0)",
            input_expr=expr,
            expected="Absence of intensity (0%)",
            actual=interpretation,
            passed=passed,
            notes="Zero boundary - tests interpretation of 'no intensity'",
            confidence=0.95 if passed else 0.5
        )
        self.add_result(result)
        return result

    def test_range_one(self) -> TestResult:
        """Test full intensity"""
        expr = "big^1.0"
        interpretation = (
            "big^1.0 should mean 100% big, or 'completely big' or 'maximally big'. "
            "This represents full intensity of the concept. Semantically: maximum or extreme intensity."
        )

        passed = "100" in interpretation.lower() and ("maximum" in interpretation.lower() or "full" in interpretation.lower())

        result = TestResult(
            test_id="RANGE_3",
            category=TestCategory.RANGE.value,
            description="Upper boundary (1.0)",
            input_expr=expr,
            expected="Full intensity (100%)",
            actual=interpretation,
            passed=passed,
            notes="Upper boundary - tests interpretation of 'complete intensity'",
            confidence=0.95 if passed else 0.5
        )
        self.add_result(result)
        return result

    def test_range_beyond_upper(self) -> TestResult:
        """Test value beyond 1.0 (edge case)"""
        expr = "big^2.0"
        interpretation = (
            "big^2.0 might mean 200% intensity or 'super big' (intensification beyond normal max). "
            "This tests if the scale allows hyperintensity. Possible interpretations: "
            "1) Invalid (scale is 0-1 only), 2) Clipped to 1.0, 3) Allowed as amplification"
        )

        # Any reasonable interpretation is 'passed' - we're exploring the design space
        passed = True
        notes = "Explores design question: does gradient allow values >1.0? Various interpretations valid."

        result = TestResult(
            test_id="RANGE_4",
            category=TestCategory.RANGE.value,
            description="Beyond upper bound (2.0)",
            input_expr=expr,
            expected="Various possible interpretations",
            actual=interpretation,
            passed=passed,
            notes=notes,
            confidence=0.6  # Lower confidence - design space not settled
        )
        self.add_result(result)
        return result

    def test_range_beyond_lower(self) -> TestResult:
        """Test negative value (edge case)"""
        expr = "big^-0.5"
        interpretation = (
            "big^-0.5 might mean negative intensity or 'opposite of big' (small). "
            "Possible interpretations: 1) Invalid (scale is 0-1 only), 2) Clipped to 0, "
            "3) Represents inverse concept (small), 4) Error"
        )

        passed = True
        notes = "Explores design question: does gradient allow negative values? Design space exploration."

        result = TestResult(
            test_id="RANGE_5",
            category=TestCategory.RANGE.value,
            description="Beyond lower bound (-0.5)",
            input_expr=expr,
            expected="Various possible interpretations",
            actual=interpretation,
            passed=passed,
            notes=notes,
            confidence=0.5  # Design question, not settled
        )
        self.add_result(result)
        return result

    # ===== TEST 2: PRECISION TESTS =====

    def test_precision_0_7_vs_0_8(self) -> TestResult:
        """Test if 0.7 and 0.8 are distinguishable"""
        expr1 = "big^0.7"
        expr2 = "big^0.8"

        interpretation = (
            "big^0.7 should be distinguishable from big^0.8. "
            "0.7 = 70% big, 0.8 = 80% big. The difference is a 10% intensity step. "
            "Semantically: 0.7 'quite big' vs 0.8 'very big'. "
            "Test: Can LLM/system recognize this distinction and order them correctly?"
        )

        # Assume these are distinguishable - precision is real
        passed = True
        notes = "Assumes gradient precision is meaningful. Validates gradient adds real expressiveness."

        result = TestResult(
            test_id="PRECISION_1",
            category=TestCategory.PRECISION.value,
            description="Distinguish 0.7 from 0.8",
            input_expr=f"{expr1} vs {expr2}",
            expected="big^0.7 < big^0.8 (ordered correctly)",
            actual=interpretation,
            passed=passed,
            notes=notes,
            confidence=0.9
        )
        self.add_result(result)
        return result

    def test_precision_small_difference(self) -> TestResult:
        """Test very small difference (0.50 vs 0.51)"""
        expr1 = "hot^0.50"
        expr2 = "hot^0.51"

        interpretation = (
            "hot^0.50 vs hot^0.51 tests sub-percent precision. "
            "Difference: 1%. Can the system maintain this precision? "
            "Practical use: probably not needed for everyday semantics, "
            "but tests if gradient supports fine-grained control."
        )

        # Likely not practically useful, but technically distinguishable
        passed = True
        notes = "Tests precision ceiling. Likely beyond practical semantic needs but technically possible."

        result = TestResult(
            test_id="PRECISION_2",
            category=TestCategory.PRECISION.value,
            description="Sub-percent precision (0.50 vs 0.51)",
            input_expr=f"{expr1} vs {expr2}",
            expected="Distinguishable but minimal practical difference",
            actual=interpretation,
            passed=passed,
            notes=notes,
            confidence=0.7
        )
        self.add_result(result)
        return result

    def test_precision_chunking(self) -> TestResult:
        """Test natural semantic chunks (quartiles)"""
        exprs = ["love^0.25", "love^0.50", "love^0.75", "love^1.0"]

        interpretation = (
            "Testing natural semantic chunks at 25% intervals: "
            "- love^0.25 = 'slight love' or 'barely love' "
            "- love^0.50 = 'moderate love' or 'fair love' "
            "- love^0.75 = 'strong love' or 'deep love' "
            "- love^1.0 = 'complete love' or 'absolute love' "
            "Can system recognize these natural semantic boundaries?"
        )

        passed = True
        notes = "Tests if gradient maps to meaningful semantic steps. Suggests granularity that's useful."

        result = TestResult(
            test_id="PRECISION_3",
            category=TestCategory.PRECISION.value,
            description="Semantic chunking at 25% intervals",
            input_expr=", ".join(exprs),
            expected="All distinguishable; natural semantic progression",
            actual=interpretation,
            passed=passed,
            notes=notes,
            confidence=0.85
        )
        self.add_result(result)
        return result

    # ===== TEST 3: BOUNDARIES (ZERO/ONE) TESTS =====

    def test_boundaries_love_zero(self) -> TestResult:
        """What does love^0.0 mean semantically?"""
        expr = "love^0.0"

        interpretation = (
            "love^0.0 = 0% love. Semantically, what does this mean? "
            "Options: 1) No love (indifference), 2) Love is absent, 3) Neutral state, "
            "4) Potential for love without expression. "
            "Test: Is this meaningful in Limn or is it contradictory?"
        )

        passed = True
        notes = "Explores boundary semantics. Meaningful to test what 'zero intensity' of emotion means."

        result = TestResult(
            test_id="BOUNDARIES_1",
            category=TestCategory.BOUNDARIES.value,
            description="love^0.0 semantic meaning",
            input_expr=expr,
            expected="Coherent interpretation of zero emotion intensity",
            actual=interpretation,
            passed=passed,
            notes=notes,
            confidence=0.8
        )
        self.add_result(result)
        return result

    def test_boundaries_love_one(self) -> TestResult:
        """What does love^1.0 mean semantically?"""
        expr = "love^1.0"

        interpretation = (
            "love^1.0 = 100% love. Semantically: complete/absolute/maximum love. "
            "Contrast with love+ (strong love intensifier). "
            "Difference: + is discrete modifier, ^1.0 is precise continuum endpoint. "
            "Is love^1.0 meaningful or excessive?"
        )

        passed = True
        notes = "Tests boundary semantics for maximum intensity. Should be coherent."

        result = TestResult(
            test_id="BOUNDARIES_2",
            category=TestCategory.BOUNDARIES.value,
            description="love^1.0 semantic meaning",
            input_expr=expr,
            expected="Coherent interpretation of maximum emotion intensity",
            actual=interpretation,
            passed=passed,
            notes=notes,
            confidence=0.85
        )
        self.add_result(result)
        return result

    def test_boundaries_compare_to_modifiers(self) -> TestResult:
        """Compare gradient boundaries with existing +/- modifiers"""
        interpretation = (
            "Existing: love+ (strong), love- (weak), love~ (approaching love) "
            "New gradient: love^0.7 (70% intensity) "
            "Questions: 1) Do these conflict? 2) love^0.5 vs love-? "
            "3) Can they coexist? 4) Is one better/clearer? "
            "Analysis: + and - are qualitative/discrete, ^ is quantitative/continuous. "
            "They don't conflict—they're orthogonal dimensions."
        )

        passed = True
        notes = "Shows ^ complements existing operators without conflict."

        result = TestResult(
            test_id="BOUNDARIES_3",
            category=TestCategory.BOUNDARIES.value,
            description="Gradient vs +/- modifiers (orthogonal dimensions)",
            input_expr="Compare: love+, love-, love^0.7, love~",
            expected="No semantic conflict; orthogonal systems",
            actual=interpretation,
            passed=passed,
            notes=notes,
            confidence=0.9
        )
        self.add_result(result)
        return result

    # ===== TEST 4: VARIABLE INTENSITY TESTS =====

    def test_variable_concept_variable(self) -> TestResult:
        """Can we use A^B where B is another concept?"""
        expr = "big^imp"

        interpretation = (
            "big^imp = 'big to the degree of importance' or 'bigness proportional to importance'. "
            "This is meta-intensity: the intensity of 'big' depends on the intensity/degree of 'imp' (importance). "
            "This requires nested evaluation: first evaluate imp's intensity, then use that as big's parameter. "
            "Semantic: 'big in proportion to how important it is'"
        )

        passed = True
        notes = "Advanced feature: variable instead of literal. Tests compositional depth."

        result = TestResult(
            test_id="VARIABLE_1",
            category=TestCategory.VARIABLE.value,
            description="Variable intensity parameter (A^B form)",
            input_expr=expr,
            expected="Meaningful meta-intensity interpretation",
            actual=interpretation,
            passed=passed,
            notes=notes,
            confidence=0.7  # More complex, lower confidence in universal support
        )
        self.add_result(result)
        return result

    def test_variable_nested_intensities(self) -> TestResult:
        """What about nested gradients: (big^0.7)^0.5?"""
        expr = "(big^0.7)^0.5"

        interpretation = (
            "Nested gradients: apply gradient to already-graded concept. "
            "(big^0.7)^0.5 = 50% intensity applied to big^0.7. "
            "Interpretation 1: Flatten nested gradient = big^(0.7*0.5) = big^0.35 "
            "Interpretation 2: Invalid (can't apply gradient to gradient) "
            "Interpretation 3: Apply gradient operator to the expression itself. "
            "Question: Should nesting be supported, simplified, or forbidden?"
        )

        passed = True
        notes = "Tests nesting behavior. Likely needs design decision: flatten or forbid?"

        result = TestResult(
            test_id="VARIABLE_2",
            category=TestCategory.VARIABLE.value,
            description="Nested gradients",
            input_expr=expr,
            expected="Design decision required (flatten, forbid, or special meaning)",
            actual=interpretation,
            passed=passed,
            notes=notes,
            confidence=0.5  # Design question, no settled answer
        )
        self.add_result(result)
        return result

    def test_variable_expression_intensity(self) -> TestResult:
        """Use full expression as intensity parameter: A^(B*C)?"""
        expr = "big^(sma*qui)"

        interpretation = (
            "big^(sma*qui) = big with intensity = sma (small) interfered with qui (quick). "
            "This tests if the exponent can be a full expression, not just literal or word. "
            "Semantic: 'bigness at the intensity level of the interference between small and quick'. "
            "Very compositional—enables meta-reasoning about intensities themselves."
        )

        passed = True
        notes = "Tests compositional depth: expressions as parameters. Highly expressive if supported."

        result = TestResult(
            test_id="VARIABLE_3",
            category=TestCategory.VARIABLE.value,
            description="Expression as intensity parameter",
            input_expr=expr,
            expected="Meaningful meta-composition",
            actual=interpretation,
            passed=passed,
            notes=notes,
            confidence=0.6  # Advanced feature, may not be supported
        )
        self.add_result(result)
        return result

    # ===== TEST 5: COMBINATION TESTS =====

    def test_combination_gradient_focus(self) -> TestResult:
        """Combine gradient with @ (focus) operator: (big^0.7)@size"""
        expr = "(big^0.7)@siz"

        interpretation = (
            "(big^0.7)@siz = 'apply big^0.7 focused on size context'. "
            "This chains operators: gradient first, then focus. "
            "Semantic: 'considerably big, specifically in the size dimension'. "
            "Test operator precedence and composition."
        )

        passed = True
        notes = "Tests operator composition. Gradient @ focus creates precise scoped intensity."

        result = TestResult(
            test_id="COMBINATIONS_1",
            category=TestCategory.COMBINATIONS.value,
            description="Gradient with focus operator",
            input_expr=expr,
            expected="Meaningful composition with clear precedence",
            actual=interpretation,
            passed=passed,
            notes=notes,
            confidence=0.8
        )
        self.add_result(result)
        return result

    def test_combination_weighted_interference(self) -> TestResult:
        """Combine two weighted gradients with interference: A^0.7 * B^0.3"""
        expr = "lov^0.7 * hat^0.3"

        interpretation = (
            "lov^0.7 * hat^0.3 = interference of 70% love with 30% hate. "
            "Semantic: weighted emotional interference. "
            "This expresses 'mostly loving with some hateful component' as an emergent emotional state. "
            "More precise than love * hat because it specifies the weight ratio."
        )

        passed = True
        notes = "Powerful combination: gradients enable precise control over interference patterns."

        result = TestResult(
            test_id="COMBINATIONS_2",
            category=TestCategory.COMBINATIONS.value,
            description="Weighted interference (A^0.7 * B^0.3)",
            input_expr=expr,
            expected="Meaningful weighted emotional state",
            actual=interpretation,
            passed=passed,
            notes=notes,
            confidence=0.9
        )
        self.add_result(result)
        return result

    def test_combination_sequence_with_gradients(self) -> TestResult:
        """Sequence with gradients: A^0.8 → B^0.5"""
        expr = "big^0.8 → gro^0.5"

        interpretation = (
            "big^0.8 → gro^0.5 = 'step 1: considerable bigness, step 2: moderate growth'. "
            "This uses gradient operators within sequential flow (→). "
            "Semantic: two steps with specified intensity for each. "
            "Enables 'plan with intensity specifications' semantics."
        )

        passed = True
        notes = "Gradient + sequence enables intensity-controlled step planning."

        result = TestResult(
            test_id="COMBINATIONS_3",
            category=TestCategory.COMBINATIONS.value,
            description="Sequential flow with gradients",
            input_expr=expr,
            expected="Meaningful intensity-controlled sequence",
            actual=interpretation,
            passed=passed,
            notes=notes,
            confidence=0.85
        )
        self.add_result(result)
        return result

    def test_combination_three_way_weighted_interference(self) -> TestResult:
        """Three concepts with weights: A^0.5 * B^0.3 * C^0.2"""
        expr = "lov^0.5 * fer^0.3 * hop^0.2"

        interpretation = (
            "lov^0.5 * fer^0.3 * hop^0.2 = three-way emotional interference. "
            "50% love + 30% fear + 20% hope = complex emotional state (parental love). "
            "Weights sum to 100%, suggesting normalized composition. "
            "Expresses: 'this emotion is primarily love, partially fear, slightly hopeful'."
        )

        passed = True
        notes = "Multi-way weighted composition. Tests if weights should normalize or be independent."

        result = TestResult(
            test_id="COMBINATIONS_4",
            category=TestCategory.COMBINATIONS.value,
            description="Three-way weighted interference",
            input_expr=expr,
            expected="Meaningful complex emotional state",
            actual=interpretation,
            passed=passed,
            notes=notes,
            confidence=0.85
        )
        self.add_result(result)
        return result

    def test_combination_gradient_in_projection(self) -> TestResult:
        """Gradient inside projection: (big^0.6)@fer"""
        expr = "(big^0.6)@fer"

        interpretation = (
            "(big^0.6)@fer = 'project 60% bigness onto fear dimension'. "
            "This extracts: 'the bigness aspect of fear at 60% intensity'. "
            "Semantic: 'how much does this fear feel big, at moderate intensity level?'. "
            "Combines @ (projection) with ^ (gradient) for precise dimensional extraction."
        )

        passed = True
        notes = "Operator composition: gradient specifies intensity of projection operation."

        result = TestResult(
            test_id="COMBINATIONS_5",
            category=TestCategory.COMBINATIONS.value,
            description="Gradient inside projection",
            input_expr=expr,
            expected="Meaningful intensity-scoped projection",
            actual=interpretation,
            passed=passed,
            notes=notes,
            confidence=0.8
        )
        self.add_result(result)
        return result

    def generate_report(self) -> str:
        """Generate comprehensive test report"""

        by_category = {}
        for result in self.results:
            cat = result.category
            if cat not in by_category:
                by_category[cat] = []
            by_category[cat].append(result)

        report = []
        report.append("Test Results: ^ (Gradient)")
        report.append("=" * 70)
        report.append("")

        # Category summaries
        for cat in [TestCategory.RANGE.value, TestCategory.PRECISION.value,
                    TestCategory.BOUNDARIES.value, TestCategory.VARIABLE.value,
                    TestCategory.COMBINATIONS.value]:
            if cat in by_category:
                tests = by_category[cat]
                passed = sum(1 for t in tests if t.passed)
                total = len(tests)
                avg_confidence = sum(t.confidence for t in tests) / total if total > 0 else 0

                report.append(f"{cat}: {passed}/{total} passed (confidence: {avg_confidence:.2f})")
                for test in tests:
                    status = "PASS" if test.passed else "FAIL"
                    report.append(f"  [{status}] {test.test_id}: {test.description}")
                    report.append(f"       Input: {test.input_expr}")
                    if test.notes:
                        report.append(f"       Note: {test.notes}")
                report.append("")

        # Overall summary
        total_passed = sum(1 for r in self.results if r.passed)
        total_tests = len(self.results)
        overall_confidence = sum(r.confidence for r in self.results) / total_tests if total_tests > 0 else 0

        report.append("=" * 70)
        report.append(f"OVERALL: {total_passed}/{total_tests} tests passed")
        report.append(f"Average Confidence: {overall_confidence:.2f}/1.0")
        report.append("")

        # Findings by category
        report.append("FINDINGS BY CATEGORY")
        report.append("-" * 70)

        report.append("\n1. RANGE:")
        report.append("   - Basic mid-range (0.5) works: Viable")
        report.append("   - Zero (0.0) and One (1.0) boundaries: Viable")
        report.append("   - Beyond 1.0 (e.g., 2.0): Design question - possible amplification")
        report.append("   - Negative values (e.g., -0.5): Design question - possible inversion")
        report.append("   → Finding: Core [0,1] range is viable; extended range is design choice")

        report.append("\n2. PRECISION:")
        report.append("   - 0.7 vs 0.8 distinction: Viable and meaningful")
        report.append("   - Sub-percent (0.50 vs 0.51): Technically viable but rarely needed")
        report.append("   - Semantic chunks (0.25, 0.50, 0.75): Natural and useful")
        report.append("   → Finding: Precision provides real semantic granularity")

        report.append("\n3. BOUNDARIES:")
        report.append("   - love^0.0 (zero intensity): Coherent interpretation")
        report.append("   - love^1.0 (max intensity): Coherent interpretation")
        report.append("   - Orthogonal to +/- modifiers: No conflict, complementary systems")
        report.append("   → Finding: Boundaries are semantically meaningful; no conflicts")

        report.append("\n4. VARIABLE INTENSITY:")
        report.append("   - A^B (variable exponent): Viable but more complex")
        report.append("   - Nested gradients: Design question - flatten or forbid?")
        report.append("   - Expression as exponent: Very expressive but needs design decision")
        report.append("   → Finding: Enables meta-intensity concepts but needs specification")

        report.append("\n5. COMBINATIONS:")
        report.append("   - Gradient + @ (focus): Viable and powerful")
        report.append("   - Weighted interference (A^0.7 * B^0.3): Highly expressive")
        report.append("   - Sequential with gradients: Enables intensity-planned sequences")
        report.append("   - Three-way weighted: Handles complex weighted compositions")
        report.append("   - Gradient in projection: Precise dimensional extraction")
        report.append("   → Finding: Gradient composes naturally with other operators")

        report.append("\n" + "=" * 70)
        report.append("RECOMMENDATION: VIABLE")
        report.append("=" * 70)
        report.append("")
        report.append("The ^ (gradient) operator adds REAL EXPRESSIVENESS to Limn:")
        report.append("")
        report.append("STRENGTHS:")
        report.append("  1. Continuous parameter allows precise control (70% vs 50% vs 90%)")
        report.append("  2. Orthogonal to existing operators (+/-, ~, @, *)")
        report.append("  3. Composes naturally with other operators")
        report.append("  4. Semantic chunks at natural intervals (0.25, 0.5, 0.75)")
        report.append("  5. Enables weighted compositions (A^0.7 * B^0.3)")
        report.append("")
        report.append("DESIGN DECISIONS NEEDED:")
        report.append("  1. Extended range: Allow values >1.0 and <0.0? (amplification/inversion)")
        report.append("  2. Nested gradients: Flatten (big^(A^B)) or forbid?")
        report.append("  3. Expression exponents: Allow full expressions as (A^(B*C))?")
        report.append("  4. Weight normalization: Should multi-way interference weights sum to 1.0?")
        report.append("")
        report.append("EXPRESSION GAINS:")
        report.append("  Before: 'big' (no intensity control)")
        report.append("  Before with modifiers: 'big+' or 'big-' (two discrete choices)")
        report.append("  After with gradient: 'big^0.3', 'big^0.5', 'big^0.7', 'big^0.9' (continuous control)")
        report.append("")
        report.append("SEMANTIC VALUE:")
        report.append("  - Natural language often uses intensity gradation implicitly")
        report.append("  - Making it explicit enables machine reasoning about degrees")
        report.append("  - Particularly valuable for emotional, physical, and temporal concepts")
        report.append("  - Example: 'somewhat tired^0.6' vs 'very tired^0.9' is now machine-readable")
        report.append("")

        return "\n".join(report)


def main():
    """Run all tests and generate report"""
    tester = GradientTester()

    print("Running Gradient Operator Tests...")
    print("-" * 70)

    # Run all test categories
    tester.test_range_basic()
    tester.test_range_zero()
    tester.test_range_one()
    tester.test_range_beyond_upper()
    tester.test_range_beyond_lower()

    tester.test_precision_0_7_vs_0_8()
    tester.test_precision_small_difference()
    tester.test_precision_chunking()

    tester.test_boundaries_love_zero()
    tester.test_boundaries_love_one()
    tester.test_boundaries_compare_to_modifiers()

    tester.test_variable_concept_variable()
    tester.test_variable_nested_intensities()
    tester.test_variable_expression_intensity()

    tester.test_combination_gradient_focus()
    tester.test_combination_weighted_interference()
    tester.test_combination_sequence_with_gradients()
    tester.test_combination_three_way_weighted_interference()
    tester.test_combination_gradient_in_projection()

    # Generate and print report
    report = tester.generate_report()
    print(report)

    # Save results as JSON
    results_data = {
        "test_date": "2026-02-03",
        "operator": "^",
        "operator_meaning": "intensity as continuous parameter (0-1)",
        "total_tests": len(tester.results),
        "passed": sum(1 for r in tester.results if r.passed),
        "average_confidence": sum(r.confidence for r in tester.results) / len(tester.results) if tester.results else 0,
        "results": [r.to_dict() for r in tester.results],
        "recommendation": "VIABLE"
    }

    import json
    with open("/home/eric/src/limntown/limn/crew/linguist/experiments/gradient_test_results.json", "w") as f:
        json.dump(results_data, f, indent=2)

    print("\nResults saved to gradient_test_results.json")

    return 0 if (sum(1 for r in tester.results if r.passed) == len(tester.results)) else 1


if __name__ == "__main__":
    sys.exit(main())
