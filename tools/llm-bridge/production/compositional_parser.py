#!/usr/bin/env python3
"""Compositional Limn Parser - Parse and evaluate compositional expressions.

Operators (precedence high to low):
  ^   gradient    A^0.7     intensity scaling (0.0-1.0)
  @   projection  A@B       extract B-component of A
  *   interference A*B      emergent meaning from simultaneous A and B
  \\   subtraction A\\B      A with B-component removed
  :   conditional A:B       A given context B
  ±   superposition A±B     quantum both/and (pre-collapse)

Supports:
  - Parenthesized nesting: (A@B)*C^0.7
  - Multi-level: ((A@B)*C)^0.5
  - Mixed operators: A@B*C:D

Author: Rex (Engineer)
Date: 2026-02-03
"""

import re
from dataclasses import dataclass
from typing import List, Optional, Union


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


# ── Tokenizer ──

TOKEN_PATTERNS = [
    ('FLOAT', r'\d+\.\d+'),
    ('WITHOUT', r'without'),
    ('WORD', r'[a-z]{2,4}'),
    ('CARET', r'\^'),
    ('AT', r'@'),
    ('STAR', r'\*'),
    ('BACKSLASH', r'\\'),
    ('COLON', r':'),
    ('PLUSMINUS', r'±'),
    ('LPAREN', r'\('),
    ('RPAREN', r'\)'),
    ('SPACE', r'\s+'),
]

TOKEN_RE = re.compile('|'.join(f'(?P<{name}>{pat})' for name, pat in TOKEN_PATTERNS))


@dataclass
class Token:
    type: str
    value: str


def tokenize(text: str) -> List[Token]:
    """Tokenize a compositional expression."""
    tokens = []
    for m in TOKEN_RE.finditer(text):
        kind = m.lastgroup
        value = m.group()
        if kind == 'SPACE':
            continue
        if kind == 'WITHOUT':
            kind = 'BACKSLASH'
            value = '\\'
        tokens.append(Token(kind, value))
    return tokens


# ── Recursive Descent Parser ──
# Precedence (low to high): ± < : < \ < * < @ < ^

class Parser:
    """Recursive descent parser for compositional Limn expressions."""

    def __init__(self, tokens: List[Token]):
        self.tokens = tokens
        self.pos = 0

    def peek(self) -> Optional[Token]:
        if self.pos < len(self.tokens):
            return self.tokens[self.pos]
        return None

    def advance(self) -> Token:
        tok = self.tokens[self.pos]
        self.pos += 1
        return tok

    def expect(self, type: str) -> Token:
        tok = self.peek()
        if tok is None or tok.type != type:
            raise ParseError(f"Expected {type}, got {tok}")
        return self.advance()

    def parse(self) -> Expr:
        expr = self.parse_superposition()
        if self.pos < len(self.tokens):
            raise ParseError(f"Unexpected token: {self.tokens[self.pos]}")
        return expr

    def parse_superposition(self) -> Expr:
        """Lowest precedence: ±"""
        left = self.parse_conditional()
        while self.peek() and self.peek().type == 'PLUSMINUS':
            self.advance()
            right = self.parse_conditional()
            left = Superposition(left, right)
        return left

    def parse_conditional(self) -> Expr:
        """: operator"""
        left = self.parse_subtraction()
        while self.peek() and self.peek().type == 'COLON':
            self.advance()
            right = self.parse_subtraction()
            left = Conditional(left, right)
        return left

    def parse_subtraction(self) -> Expr:
        """\\ operator"""
        left = self.parse_interference()
        while self.peek() and self.peek().type == 'BACKSLASH':
            self.advance()
            right = self.parse_interference()
            left = Subtraction(left, right)
        return left

    def parse_interference(self) -> Expr:
        """* operator"""
        left = self.parse_projection()
        while self.peek() and self.peek().type == 'STAR':
            self.advance()
            right = self.parse_projection()
            left = Interference(left, right)
        return left

    def parse_projection(self) -> Expr:
        """@ operator"""
        left = self.parse_gradient()
        while self.peek() and self.peek().type == 'AT':
            self.advance()
            right = self.parse_gradient()
            left = Projection(left, right)
        return left

    def parse_gradient(self) -> Expr:
        """^ operator (highest binary precedence)"""
        left = self.parse_primary()
        while self.peek() and self.peek().type == 'CARET':
            self.advance()
            intensity_tok = self.expect('FLOAT')
            intensity = float(intensity_tok.value)
            left = Gradient(left, intensity)
        return left

    def parse_primary(self) -> Expr:
        """Word or parenthesized expression."""
        tok = self.peek()
        if tok is None:
            raise ParseError("Unexpected end of expression")

        if tok.type == 'LPAREN':
            self.advance()
            expr = self.parse_superposition()
            self.expect('RPAREN')
            return expr

        if tok.type == 'WORD':
            self.advance()
            return Word(tok.value)

        raise ParseError(f"Unexpected token: {tok}")


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
    tokens = tokenize(text)
    if not tokens:
        raise ParseError("Empty expression")
    return Parser(tokens).parse()


def is_compositional(text: str) -> bool:
    """Check if text contains compositional operators."""
    return bool(re.search(r'[@*^\\±:]', text)) or 'without' in text


def extract_base_words(expr: Expr) -> List[str]:
    """Extract all base Limn words from a parsed expression."""
    if isinstance(expr, Word):
        return [expr.value]
    elif isinstance(expr, Gradient):
        return extract_base_words(expr.operand)
    else:
        # All binary operators have left/right
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
    ]

    for t in tests:
        try:
            expr = parse(t)
            words = extract_base_words(expr)
            desc = describe_expr(expr)
            print(f"  {t:30s} → {desc}")
            print(f"  {'':30s}   base words: {words}")
        except ParseError as e:
            print(f"  {t:30s} → ERROR: {e}")
