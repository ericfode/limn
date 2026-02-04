#!/usr/bin/env python3
"""Compositional Limn Parser - Parse and evaluate compositional expressions.

Uses Lark (EBNF grammar parser) for robust parsing with proper precedence.

Operators (precedence high to low):
  ^   gradient      A^0.7     intensity scaling (0.0-1.0)
  @   projection    A@B       extract B-component of A
  *   interference  A*B       emergent meaning from simultaneous A and B
  \\   subtraction   A\\B      A with B-component removed
  :   conditional   A:B       A given context B
  ±   superposition A±B       quantum both/and (pre-collapse)

Supports:
  - Parenthesized nesting: (A@B)*C^0.7
  - Multi-level: ((A@B)*C)^0.5
  - Mixed operators: A@B*C:D
  - 'without' keyword as subtraction alias

Author: Rex (Engineer)
Date: 2026-02-03
"""

from dataclasses import dataclass
from typing import List, Union

from lark import Lark, Transformer, v_args


# ── EBNF Grammar ──

LIMN_GRAMMAR = r"""
    ?start: superposition

    ?superposition: conditional
        | superposition "±" conditional  -> superposition

    ?conditional: subtraction
        | conditional ":" subtraction    -> conditional

    ?subtraction: interference
        | subtraction "\\" interference  -> subtraction
        | subtraction "without" interference -> subtraction

    ?interference: projection
        | interference "*" projection    -> interference

    ?projection: gradient
        | projection "@" gradient        -> projection

    ?gradient: atom
        | gradient "^" FLOAT             -> gradient

    ?atom: WORD                          -> word
        | "(" superposition ")"

    WORD: /[a-z]{2,4}/
    FLOAT: /\d+\.\d+/

    %import common.WS
    %ignore WS
"""

parser = Lark(LIMN_GRAMMAR, parser='lalr', maybe_placeholders=False)


# ── AST Nodes ──

@dataclass
class Word:
    """A single Limn word (leaf node)."""
    value: str

    def __str__(self):
        return self.value


@dataclass
class Gradient:
    """A^intensity - gradient/intensity scaling."""
    operand: 'Expr'
    intensity: float

    def __str__(self):
        return f"{self.operand}^{self.intensity}"


@dataclass
class Projection:
    """A@B - extract B-component of A."""
    left: 'Expr'
    right: 'Expr'

    def __str__(self):
        return f"{self.left}@{self.right}"


@dataclass
class Interference:
    """A*B - emergent meaning from simultaneous A and B."""
    left: 'Expr'
    right: 'Expr'

    def __str__(self):
        return f"{self.left}*{self.right}"


@dataclass
class Subtraction:
    """A\\B - A with B-component removed."""
    left: 'Expr'
    right: 'Expr'

    def __str__(self):
        return f"{self.left}\\{self.right}"


@dataclass
class Conditional:
    """A:B - A given context B."""
    left: 'Expr'
    right: 'Expr'

    def __str__(self):
        return f"{self.left}:{self.right}"


@dataclass
class Superposition:
    """A±B - quantum both/and."""
    left: 'Expr'
    right: 'Expr'

    def __str__(self):
        return f"{self.left}±{self.right}"


Expr = Union[Word, Gradient, Projection, Interference, Subtraction, Conditional, Superposition]


# ── Lark AST Transformer ──

@v_args(inline=True)
class LimnTransformer(Transformer):
    """Transform Lark parse tree into typed AST nodes."""

    def word(self, token):
        return Word(str(token))

    def gradient(self, operand, intensity):
        return Gradient(operand, float(intensity))

    def projection(self, left, right):
        return Projection(left, right)

    def interference(self, left, right):
        return Interference(left, right)

    def subtraction(self, left, right):
        return Subtraction(left, right)

    def conditional(self, left, right):
        return Conditional(left, right)

    def superposition(self, left, right):
        return Superposition(left, right)


_transformer = LimnTransformer()


# ── Public API ──

class ParseError(Exception):
    """Error during expression parsing."""
    pass


def parse(text: str) -> Expr:
    """Parse a compositional Limn expression into an AST.

    Examples:
        parse("lov@fer")        → Projection(Word("lov"), Word("fer"))
        parse("hot^0.7")        → Gradient(Word("hot"), 0.7)
        parse("(lov@fer)*joy")  → Interference(Projection(...), Word("joy"))
    """
    try:
        tree = parser.parse(text)
        return _transformer.transform(tree)
    except Exception as e:
        raise ParseError(str(e)) from e


def is_compositional(text: str) -> bool:
    """Check if text contains compositional operators."""
    import re
    return bool(re.search(r'[@*^\\±:]', text)) or 'without' in text


def extract_base_words(expr: Expr) -> List[str]:
    """Extract all base Limn words from a parsed expression."""
    if isinstance(expr, Word):
        return [expr.value]
    elif isinstance(expr, Gradient):
        return extract_base_words(expr.operand)
    else:
        return extract_base_words(expr.left) + extract_base_words(expr.right)


def describe_expr(expr: Expr) -> str:
    """Generate a human-readable description of a compositional expression."""
    if isinstance(expr, Word):
        return expr.value
    elif isinstance(expr, Gradient):
        pct = int(expr.intensity * 100)
        return f"{describe_expr(expr.operand)} at {pct}% intensity"
    elif isinstance(expr, Projection):
        return f"{describe_expr(expr.right)}-aspect of {describe_expr(expr.left)}"
    elif isinstance(expr, Interference):
        return f"{describe_expr(expr.left)} interfering with {describe_expr(expr.right)}"
    elif isinstance(expr, Subtraction):
        return f"{describe_expr(expr.left)} without {describe_expr(expr.right)}"
    elif isinstance(expr, Conditional):
        return f"{describe_expr(expr.left)} given {describe_expr(expr.right)}"
    elif isinstance(expr, Superposition):
        return f"{describe_expr(expr.left)} and/or {describe_expr(expr.right)}"
    return str(expr)


# ── Self-test ──

if __name__ == "__main__":
    tests = [
        "lov@fer",
        "hot^0.7",
        "(lov@fer)*joy",
        "dar without lig",
        "was±now",
        "lov:war",
        "((lov@fer)*joy)^0.5",
        "sel@awa*con^0.8",
        "(sel@awa)*con:tim",
        "lov±fer\\sor",
    ]

    print("Compositional Limn Parser (Lark LALR)")
    print("=" * 60)
    for t in tests:
        try:
            expr = parse(t)
            words = extract_base_words(expr)
            desc = describe_expr(expr)
            print(f"  {t:30s} → {desc}")
            print(f"  {'':30s}   words: {words}")
        except ParseError as e:
            print(f"  {t:30s} → ERROR: {e}")
    print()

    # Show parse tree for a complex expression
    print("Parse tree for '((lov@fer)*joy)^0.5':")
    tree = parser.parse("((lov@fer)*joy)^0.5")
    print(tree.pretty())
