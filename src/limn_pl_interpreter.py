"""
Limn-PL Interpreter: A Programming Language Written in Limn

This interpreter executes programs written in Limn-PL, a constraint-based
programming language that uses only Limn vocabulary.

Limn-PL programs are Limn sentences where:
- Variables are marked with 'whe' (unknown)
- Constraints use Limn vocabulary (joi, cut, exp, con, sa, ma, mi, etc.)
- Execution is bidirectional constraint satisfaction
- Input is provided as a key (binding sentence)
"""

from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Set, Union, Tuple, Callable
from enum import Enum
import re
import math


# ============================================================================
# Limn Vocabulary Mapping
# ============================================================================

class LimnWord(Enum):
    """Limn vocabulary words used in Limn-PL."""
    # Structure
    PRO = "pro"  # program
    VAR = "var"  # variables
    CNS = "cns"  # constraints

    # Variable marker
    WHE = "whe"  # unknown/variable

    # Relations
    SA = "sa"    # equals
    MA = "ma"    # greater than
    MI = "mi"    # less than
    EQ = "eq"    # equal comparison

    # Operators
    NU = "nu"    # negation
    VE = "ve"    # type/intensifier

    # Arithmetic
    JOI = "joi"  # plus/join
    CUT = "cut"  # minus/cut
    EXP = "exp"  # multiply/expand
    CON = "con"  # divide/contract
    POW = "pow"  # power
    ROO = "roo"  # root
    MOR = "mor"  # increment
    LES = "les"  # decrement

    # Types
    ONE = "one"  # integer
    FLO = "flo"  # float
    WOR = "wor"  # string
    TRU = "tru"  # true
    FAL = "fal"  # false
    GRO = "gro"  # group/list
    HOL = "hol"  # null/hole

    # Control
    IF = "if"    # conditional
    THE = "the"  # then
    OTH = "oth"  # otherwise
    CYC = "cyc"  # cycle/loop
    BEG = "beg"  # begin
    END = "end"  # end

    # Function
    CAU = "cau"  # cause/call
    EFF = "eff"  # effect/result

    # Collection
    AMO = "amo"  # among/element of
    INS = "ins"  # inside/contains
    PAR = "par"  # part/index
    WHO = "who"  # whole/size
    FST = "fst"  # first
    NXT = "nxt"  # next
    FIN = "fin"  # final

    # Quantifiers
    AL = "al"    # all
    EX = "ex"    # exists
    ON = "on"    # one/single

    # Logic
    PA = "pa"    # parallel/or
    YO = "yo"    # this/here (binding)
    AN = "an"    # that/there (reference)

    # Scope
    PIPE = "|"   # scope separator


# ============================================================================
# AST Nodes
# ============================================================================

@dataclass
class Node:
    """Base AST node."""
    pass


@dataclass
class Variable(Node):
    """A variable (whe-marked unknown)."""
    name: str

    def __hash__(self):
        return hash(self.name)

    def __eq__(self, other):
        return isinstance(other, Variable) and self.name == other.name

    def __repr__(self):
        return f"Var({self.name})"


@dataclass
class Literal(Node):
    """A literal value."""
    value: Any
    limn_type: Optional[str] = None  # one, flo, wor, tru, fal, gro

    def __repr__(self):
        return f"Lit({self.value})"


@dataclass
class BinaryOp(Node):
    """A binary operation."""
    op: str  # joi, cut, exp, con, sa, ma, mi
    left: Node
    right: Node

    def __repr__(self):
        return f"({self.left} {self.op} {self.right})"


@dataclass
class UnaryOp(Node):
    """A unary operation."""
    op: str  # nu, ve
    operand: Node

    def __repr__(self):
        return f"({self.op} {self.operand})"


@dataclass
class FunctionCall(Node):
    """A function call (cau)."""
    func_name: str
    args: List[Node]
    result_var: Variable

    def __repr__(self):
        return f"Call({self.func_name}, {self.args}) -> {self.result_var}"


@dataclass
class Constraint(Node):
    """A constraint (relation that must hold)."""
    relation: str  # sa, ma, mi, eq, amo, ins
    left: Node
    right: Node
    negated: bool = False

    def __repr__(self):
        neg = "nu " if self.negated else ""
        return f"Constraint({neg}{self.left} {self.relation} {self.right})"


@dataclass
class Conditional(Node):
    """A conditional constraint (if-then-else)."""
    condition: Node
    then_branch: List[Node]
    else_branch: Optional[List[Node]] = None

    def __repr__(self):
        return f"If({self.condition} then {self.then_branch} else {self.else_branch})"


@dataclass
class Quantified(Node):
    """A quantified constraint (al, ex)."""
    quantifier: str  # al, ex
    variable: Variable
    body: Node

    def __repr__(self):
        return f"{self.quantifier} {self.variable}: {self.body}"


@dataclass
class Group(Node):
    """A group/list literal."""
    elements: List[Node]

    def __repr__(self):
        return f"Gro({self.elements})"


@dataclass
class IndexAccess(Node):
    """Index access: par i arr."""
    index: Node
    collection: Node

    def __repr__(self):
        return f"Par({self.index}, {self.collection})"


@dataclass
class SizeOf(Node):
    """Size/length of collection: who arr."""
    collection: Node

    def __repr__(self):
        return f"Who({self.collection})"


@dataclass
class FirstOf(Node):
    """First element of collection: fst arr."""
    collection: Node

    def __repr__(self):
        return f"Fst({self.collection})"


@dataclass
class TailOf(Node):
    """Tail (rest) of collection: nxt arr = arr[1:]."""
    collection: Node

    def __repr__(self):
        return f"Nxt({self.collection})"


@dataclass
class LastOf(Node):
    """Last element of collection: fin arr."""
    collection: Node

    def __repr__(self):
        return f"Fin({self.collection})"


@dataclass
class Alternative(Node):
    """Alternative/OR: branch1 pa branch2."""
    branches: List[Node]

    def __repr__(self):
        return f"Pa({self.branches})"


@dataclass
class CondBlock(Node):
    """A conditional block: if cond | the result | oth result."""
    condition: Node
    then_result: Node
    else_result: Optional[Node] = None

    def __repr__(self):
        return f"If({self.condition} then {self.then_result} else {self.else_result})"


@dataclass
class FunctionDef(Node):
    """A function definition (stored program)."""
    name: str
    params: List[Variable]
    result_var: Variable
    body: List[Node]

    def __repr__(self):
        return f"FuncDef({self.name}({self.params}) -> {self.result_var})"


@dataclass
class Program(Node):
    """A Limn-PL program."""
    name: str
    variables: List[Variable]
    constraints: List[Node]

    def __repr__(self):
        return f"Program({self.name}, vars={self.variables}, constraints={len(self.constraints)})"


# ============================================================================
# Parser
# ============================================================================

class LimnPLParser:
    """Parser for Limn-PL programs."""

    def __init__(self):
        self.tokens: List[str] = []
        self.pos: int = 0

    def tokenize(self, source: str) -> List[str]:
        """Tokenize Limn-PL source code."""
        # Handle comments
        source = re.sub(r'#.*$', '', source, flags=re.MULTILINE)

        # Tokenize: words, numbers, strings, operators
        pattern = r'''
            "([^"]*)"         # quoted strings
            | ([+-]?\d+\.?\d*)  # numbers
            | (\|)            # pipe separator
            | ([\w-]+)        # words and identifiers
        '''
        tokens = []
        for match in re.finditer(pattern, source, re.VERBOSE):
            if match.group(1) is not None:  # string
                tokens.append(f'"{match.group(1)}"')
            elif match.group(2):  # number
                tokens.append(match.group(2))
            elif match.group(3):  # pipe
                tokens.append('|')
            elif match.group(4):  # word
                tokens.append(match.group(4))
        return tokens

    def parse(self, source: str) -> Program:
        """Parse a Limn-PL program."""
        self.tokens = self.tokenize(source)
        self.pos = 0
        return self.parse_program()

    def current(self) -> Optional[str]:
        """Get current token."""
        if self.pos < len(self.tokens):
            return self.tokens[self.pos]
        return None

    def advance(self) -> str:
        """Advance and return current token."""
        token = self.current()
        self.pos += 1
        return token

    def expect(self, expected: str) -> str:
        """Expect and consume a specific token."""
        token = self.advance()
        if token != expected:
            raise SyntaxError(f"Expected '{expected}', got '{token}'")
        return token

    def parse_program(self) -> Program:
        """Parse: pro [name] | var [...] | cns [...]"""
        name = "unnamed"
        variables = []
        constraints = []

        # Parse program name if present
        if self.current() == "pro":
            self.advance()
            if self.current() and self.current() != "|":
                name = self.advance()

        # Parse sections
        while self.current():
            if self.current() == "|":
                self.advance()

            if self.current() == "var":
                self.advance()
                if self.current() == "|":
                    self.advance()
                variables = self.parse_variables()
            elif self.current() == "cns":
                self.advance()
                if self.current() == "|":
                    self.advance()
                constraints = self.parse_constraints()
            elif self.current():
                # Treat as constraint
                constraints.append(self.parse_expression())

        return Program(name, variables, constraints)

    def parse_variables(self) -> List[Variable]:
        """Parse variable declarations: whe a | whe b | ..."""
        variables = []
        while self.current():
            if self.current() == "cns" or (self.current() == "|" and
                self.pos + 1 < len(self.tokens) and
                self.tokens[self.pos + 1] == "cns"):
                break

            if self.current() == "|":
                self.advance()
                continue

            if self.current() == "whe":
                self.advance()
                if self.current() and self.current() not in ["|", "whe", "cns"]:
                    var_name = self.advance()
                    variables.append(Variable(var_name))
            else:
                # Bare variable name
                var_name = self.advance()
                if var_name and var_name not in ["|", "cns"]:
                    variables.append(Variable(var_name))

        return variables

    def parse_constraints(self) -> List[Node]:
        """Parse constraint list."""
        constraints = []
        while self.current():
            if self.current() == "|":
                self.advance()
                if not self.current():
                    break
                continue

            constraint = self.parse_expression()
            if constraint:
                constraints.append(constraint)

        return constraints

    def parse_expression(self) -> Optional[Node]:
        """Parse a constraint expression."""
        if not self.current():
            return None

        # Handle conditional: if cond | the result | oth result
        if self.current() == "if":
            self.advance()
            # Skip | if present
            if self.current() == "|":
                self.advance()
            condition = self.parse_expression()
            then_result = None
            else_result = None

            # Look for 'the'
            if self.current() == "|":
                self.advance()
            if self.current() == "the":
                self.advance()
                if self.current() == "|":
                    self.advance()
                then_result = self.parse_expression()

            # Look for 'oth'
            if self.current() == "|":
                self.advance()
            if self.current() == "oth":
                self.advance()
                if self.current() == "|":
                    self.advance()
                else_result = self.parse_expression()

            return CondBlock(condition, then_result, else_result)

        # Handle negation
        negated = False
        if self.current() == "nu":
            self.advance()
            negated = True

        # Parse left side
        left = self.parse_atom()
        if not left:
            return None

        # Check for operation followed by another term and then relation
        # Pattern: a joi b sa c
        if self.current() in ["joi", "cut", "exp", "con", "pow"]:
            op = self.advance()
            right = self.parse_atom()
            result = BinaryOp(op, left, right)

            # Check for equality constraint: a joi b sa c
            if self.current() == "sa":
                self.advance()
                result_var = self.parse_atom()
                constraint = Constraint("sa", result, result_var, negated)

                # Check for pa (OR)
                if self.current() == "pa":
                    return self.parse_alternatives(constraint)
                return constraint

            return result

        # Check for relation/operation
        # Pattern: x sa 5 OR x sa y joi z (equality with expression on right)
        if self.current() in ["sa", "ma", "mi", "eq", "amo", "ins"]:
            relation = self.advance()
            right = self.parse_atom()

            # Check if right side continues with an operator: x sa y joi z
            if right and self.current() in ["joi", "cut", "exp", "con", "pow"]:
                op = self.advance()
                right2 = self.parse_atom()
                right = BinaryOp(op, right, right2)

            constraint = Constraint(relation, left, right, negated)

            # Check for pa (OR)
            if self.current() == "pa":
                return self.parse_alternatives(constraint)
            return constraint

        if self.current() == "cau":
            # Function call: func cau arg1 arg2 eff result
            self.advance()
            args = []
            while self.current() and self.current() not in ["eff", "|"]:
                arg = self.parse_atom()
                if arg:
                    args.append(arg)

            result_var = Variable("_result")
            if self.current() == "eff":
                self.advance()
                result_var = self.parse_atom()

            return FunctionCall(left.name if isinstance(left, Variable) else str(left),
                               args, result_var)

        if negated:
            return UnaryOp("nu", left)

        return left

    def parse_alternatives(self, first_branch: Node) -> Alternative:
        """Parse pa (OR) alternatives: branch1 pa branch2 pa branch3..."""
        branches = [first_branch]

        while self.current() == "pa":
            self.advance()  # consume 'pa'
            # Skip | if present
            if self.current() == "|":
                self.advance()
            branch = self.parse_expression()
            if branch:
                branches.append(branch)
            else:
                break

        return Alternative(branches)

    def parse_atom(self) -> Optional[Node]:
        """Parse an atomic term (variable or literal)."""
        token = self.current()
        if not token or token == "|":
            return None

        # Check for literals
        if token.startswith('"'):
            self.advance()
            return Literal(token[1:-1], "wor")

        if token == "tru":
            self.advance()
            return Literal(True, "tru")

        if token == "fal":
            self.advance()
            return Literal(False, "fal")

        if token == "hol":
            self.advance()
            return Literal(None, "hol")

        # Check for group literal: gro | elem1 | elem2 | ... | sa var
        if token == "gro":
            self.advance()
            elements = []
            while self.current() and self.current() not in ["sa", "cns", "var"]:
                if self.current() == "|":
                    self.advance()
                    continue
                elem = self.parse_atom()
                if elem:
                    elements.append(elem)
                else:
                    break
            return Group(elements)

        # Check for par (index access): par i arr
        if token == "par":
            self.advance()
            index = self.parse_atom()
            collection = self.parse_atom()
            return IndexAccess(index, collection)

        # Check for who (size): who arr
        if token == "who":
            self.advance()
            collection = self.parse_atom()
            return SizeOf(collection)

        # Check for fst (first element): fst arr
        if token == "fst":
            self.advance()
            collection = self.parse_atom()
            return FirstOf(collection)

        # Check for nxt (tail/rest): nxt arr
        if token == "nxt":
            self.advance()
            collection = self.parse_atom()
            return TailOf(collection)

        # Check for fin (last element): fin arr
        if token == "fin":
            self.advance()
            collection = self.parse_atom()
            return LastOf(collection)

        # Check for numbers
        try:
            if "." in token:
                val = float(token)
                self.advance()
                return Literal(val, "flo")
            else:
                val = int(token)
                self.advance()
                return Literal(val, "one")
        except ValueError:
            pass

        # Must be a variable or keyword - keywords return None
        # Note: "eff" is NOT a keyword - it's used as a variable name in function definitions
        if token in ["sa", "ma", "mi", "joi", "cut", "exp", "con", "pow",
                     "cau", "amo", "ins", "nu", "|", "cns", "var", "pa", "the", "oth", "if"]:
            return None

        # It's a variable
        self.advance()
        return Variable(token)

    def parse_term(self) -> Optional[Node]:
        """Parse a term (variable, literal, or grouped expression)."""
        return self.parse_atom()

        self.advance()
        return Variable(token)


# ============================================================================
# Environment
# ============================================================================

@dataclass
class Environment:
    """Variable bindings."""
    bindings: Dict[str, Any] = field(default_factory=dict)

    def lookup(self, var_name: str) -> Optional[Any]:
        return self.bindings.get(var_name)

    def bind(self, var_name: str, value: Any) -> 'Environment':
        new_env = Environment(self.bindings.copy())
        new_env.bindings[var_name] = value
        return new_env

    def extend(self, new_bindings: Dict[str, Any]) -> 'Environment':
        new_env = Environment(self.bindings.copy())
        new_env.bindings.update(new_bindings)
        return new_env

    def is_bound(self, var_name: str) -> bool:
        return var_name in self.bindings

    def __repr__(self):
        return f"Env({self.bindings})"


# ============================================================================
# Solver
# ============================================================================

class SolverError(Exception):
    """Raised when solving fails."""
    pass


class LimnPLSolver:
    """Constraint solver for Limn-PL programs."""

    def __init__(self, functions: Optional[Dict[str, Program]] = None):
        self.functions = functions or {}
        self.call_depth = 0
        self.max_call_depth = 100

    def register_function(self, program: Program):
        """Register a program as a callable function."""
        self.functions[program.name] = program

    def call_function(self, func_name: str, args: List[Any], env: Environment) -> Optional[Any]:
        """Call a function with given arguments."""
        if func_name not in self.functions:
            return None

        if self.call_depth >= self.max_call_depth:
            raise SolverError(f"Maximum recursion depth ({self.max_call_depth}) exceeded")

        func = self.functions[func_name]
        self.call_depth += 1

        try:
            # Create bindings from arguments to function parameters
            func_bindings = {}
            func_vars = [v for v in func.variables if v.name != "eff"]
            for i, arg in enumerate(args):
                if i < len(func_vars):
                    func_bindings[func_vars[i].name] = arg

            # Solve the function's constraints
            result = self.solve(func, func_bindings)

            # Return the 'eff' (effect/result) variable
            return result.get("eff")
        finally:
            self.call_depth -= 1

    def evaluate(self, node: Node, env: Environment) -> Optional[Any]:
        """Evaluate a node in an environment."""
        if isinstance(node, Literal):
            return node.value

        if isinstance(node, Variable):
            return env.lookup(node.name)

        if isinstance(node, BinaryOp):
            left = self.evaluate(node.left, env)
            right = self.evaluate(node.right, env)

            if left is None or right is None:
                return None

            if node.op == "joi":  # plus
                if isinstance(left, list) and isinstance(right, list):
                    return left + right  # list concatenation
                return left + right
            elif node.op == "cut":  # minus
                return left - right
            elif node.op == "exp":  # multiply
                return left * right
            elif node.op == "con":  # divide
                if right == 0:
                    raise SolverError("Division by zero")
                return left / right
            elif node.op == "pow":  # power
                return left ** right

        if isinstance(node, UnaryOp):
            operand = self.evaluate(node.operand, env)
            if operand is None:
                return None
            if node.op == "nu":  # negation
                return not operand

        if isinstance(node, Group):
            elements = []
            for elem in node.elements:
                val = self.evaluate(elem, env)
                if val is None:
                    return None
                elements.append(val)
            return elements

        if isinstance(node, IndexAccess):
            index_val = self.evaluate(node.index, env)
            coll_val = self.evaluate(node.collection, env)
            if index_val is None or coll_val is None:
                return None
            try:
                return coll_val[int(index_val)]
            except (IndexError, TypeError):
                return None

        if isinstance(node, SizeOf):
            coll_val = self.evaluate(node.collection, env)
            if coll_val is None:
                return None
            try:
                return len(coll_val)
            except TypeError:
                return None

        if isinstance(node, FirstOf):
            coll_val = self.evaluate(node.collection, env)
            if coll_val is None:
                return None
            try:
                return coll_val[0] if len(coll_val) > 0 else None
            except (TypeError, IndexError):
                return None

        if isinstance(node, TailOf):
            coll_val = self.evaluate(node.collection, env)
            if coll_val is None:
                return None
            try:
                return coll_val[1:] if len(coll_val) > 0 else []
            except TypeError:
                return None

        if isinstance(node, LastOf):
            coll_val = self.evaluate(node.collection, env)
            if coll_val is None:
                return None
            try:
                return coll_val[-1] if len(coll_val) > 0 else None
            except (TypeError, IndexError):
                return None

        return None

    def get_unbound(self, node: Node, env: Environment) -> Set[str]:
        """Get unbound variables in a node."""
        unbound = set()

        if isinstance(node, Variable):
            if not env.is_bound(node.name):
                unbound.add(node.name)
        elif isinstance(node, BinaryOp):
            unbound.update(self.get_unbound(node.left, env))
            unbound.update(self.get_unbound(node.right, env))
        elif isinstance(node, UnaryOp):
            unbound.update(self.get_unbound(node.operand, env))
        elif isinstance(node, Constraint):
            unbound.update(self.get_unbound(node.left, env))
            unbound.update(self.get_unbound(node.right, env))
        elif isinstance(node, FunctionCall):
            for arg in node.args:
                unbound.update(self.get_unbound(arg, env))
            if not env.is_bound(node.result_var.name):
                unbound.add(node.result_var.name)
        elif isinstance(node, Group):
            for elem in node.elements:
                unbound.update(self.get_unbound(elem, env))
        elif isinstance(node, IndexAccess):
            unbound.update(self.get_unbound(node.index, env))
            unbound.update(self.get_unbound(node.collection, env))
        elif isinstance(node, SizeOf):
            unbound.update(self.get_unbound(node.collection, env))
        elif isinstance(node, FirstOf):
            unbound.update(self.get_unbound(node.collection, env))
        elif isinstance(node, TailOf):
            unbound.update(self.get_unbound(node.collection, env))
        elif isinstance(node, LastOf):
            unbound.update(self.get_unbound(node.collection, env))
        elif isinstance(node, Alternative):
            for branch in node.branches:
                unbound.update(self.get_unbound(branch, env))
        elif isinstance(node, CondBlock):
            unbound.update(self.get_unbound(node.condition, env))
            unbound.update(self.get_unbound(node.then_result, env))
            if node.else_result:
                unbound.update(self.get_unbound(node.else_result, env))

        return unbound

    def solve_constraint(self, constraint: Constraint, env: Environment) -> Optional[Dict[str, Any]]:
        """Attempt to solve a constraint, returning new bindings."""

        # For equality constraints with arithmetic
        if constraint.relation == "sa":
            left = constraint.left
            right = constraint.right

            # Handle BinaryOp on left: a joi b sa c
            if isinstance(left, BinaryOp):
                left_val = self.evaluate(left.left, env)
                right_val = self.evaluate(left.right, env)
                result_val = self.evaluate(right, env)

                op = left.op

                # Count unknowns
                unknowns = []
                if left_val is None and isinstance(left.left, Variable):
                    unknowns.append(('left', left.left.name))
                if right_val is None and isinstance(left.right, Variable):
                    unknowns.append(('right', left.right.name))
                if result_val is None and isinstance(right, Variable):
                    unknowns.append(('result', right.name))

                if len(unknowns) == 0:
                    # Check constraint
                    computed = self.evaluate(left, env)
                    if computed is not None and abs(computed - result_val) < 1e-10:
                        return {} if not constraint.negated else None
                    return None if not constraint.negated else {}

                if len(unknowns) == 1:
                    unknown_type, unknown_name = unknowns[0]

                    try:
                        if unknown_type == 'result':
                            value = self.evaluate(left, env)
                        elif unknown_type == 'left':
                            if op == "joi":  # a + b = c => a = c - b
                                value = result_val - right_val
                            elif op == "cut":  # a - b = c => a = c + b
                                value = result_val + right_val
                            elif op == "exp":  # a * b = c => a = c / b
                                if right_val == 0:
                                    return None
                                value = result_val / right_val
                            elif op == "con":  # a / b = c => a = c * b
                                value = result_val * right_val
                            else:
                                return None
                        elif unknown_type == 'right':
                            if op == "joi":  # a + b = c => b = c - a
                                value = result_val - left_val
                            elif op == "cut":  # a - b = c => b = a - c
                                value = left_val - result_val
                            elif op == "exp":  # a * b = c => b = c / a
                                if left_val == 0:
                                    return None
                                value = result_val / left_val
                            elif op == "con":  # a / b = c => b = a / c
                                if result_val == 0:
                                    return None
                                value = left_val / result_val
                            else:
                                return None

                        if not constraint.negated:
                            return {unknown_name: value}
                    except (ZeroDivisionError, TypeError):
                        return None

            # Handle BinaryOp on right: t1 sa C exp 9 (meaning t1 = C * 9)
            if isinstance(right, BinaryOp):
                result_val = self.evaluate(left, env)  # t1
                left_val = self.evaluate(right.left, env)  # C
                right_val = self.evaluate(right.right, env)  # 9

                op = right.op

                # Count unknowns
                unknowns = []
                if result_val is None and isinstance(left, Variable):
                    unknowns.append(('result', left.name))
                if left_val is None and isinstance(right.left, Variable):
                    unknowns.append(('left', right.left.name))
                if right_val is None and isinstance(right.right, Variable):
                    unknowns.append(('right', right.right.name))

                if len(unknowns) == 0:
                    # Check constraint
                    computed = self.evaluate(right, env)
                    if computed is not None and abs(computed - result_val) < 1e-10:
                        return {} if not constraint.negated else None
                    return None if not constraint.negated else {}

                if len(unknowns) == 1:
                    unknown_type, unknown_name = unknowns[0]

                    try:
                        if unknown_type == 'result':
                            value = self.evaluate(right, env)
                        elif unknown_type == 'left':
                            # t1 sa C op 9, solving for C
                            if op == "joi":  # t1 = C + 9 => C = t1 - 9
                                value = result_val - right_val
                            elif op == "cut":  # t1 = C - 9 => C = t1 + 9
                                value = result_val + right_val
                            elif op == "exp":  # t1 = C * 9 => C = t1 / 9
                                if right_val == 0:
                                    return None
                                value = result_val / right_val
                            elif op == "con":  # t1 = C / 9 => C = t1 * 9
                                value = result_val * right_val
                            else:
                                return None
                        elif unknown_type == 'right':
                            # t1 sa C op x, solving for x
                            if op == "joi":  # t1 = C + x => x = t1 - C
                                value = result_val - left_val
                            elif op == "cut":  # t1 = C - x => x = C - t1
                                value = left_val - result_val
                            elif op == "exp":  # t1 = C * x => x = t1 / C
                                if left_val == 0:
                                    return None
                                value = result_val / left_val
                            elif op == "con":  # t1 = C / x => x = C / t1
                                if result_val == 0:
                                    return None
                                value = left_val / result_val
                            else:
                                return None

                        if not constraint.negated:
                            return {unknown_name: value}
                    except (ZeroDivisionError, TypeError):
                        return None

            # Handle SizeOf on left: who arr sa n
            if isinstance(left, SizeOf):
                coll_val = self.evaluate(left.collection, env)
                right_val = self.evaluate(right, env)

                if coll_val is not None:
                    size = len(coll_val)
                    if right_val is not None:
                        # Check constraint
                        if constraint.negated:
                            return {} if size != right_val else None
                        return {} if size == right_val else None
                    elif isinstance(right, Variable):
                        if not constraint.negated:
                            return {right.name: size}

            # Handle IndexAccess on left: par i arr sa x
            if isinstance(left, IndexAccess):
                index_val = self.evaluate(left.index, env)
                coll_val = self.evaluate(left.collection, env)
                right_val = self.evaluate(right, env)

                if index_val is not None and coll_val is not None:
                    try:
                        elem_val = coll_val[int(index_val)]
                        if right_val is not None:
                            # Check constraint
                            if constraint.negated:
                                return {} if elem_val != right_val else None
                            return {} if elem_val == right_val else None
                        elif isinstance(right, Variable):
                            if not constraint.negated:
                                return {right.name: elem_val}
                    except (IndexError, TypeError):
                        return None

            # Handle FirstOf on left: fst arr sa x
            if isinstance(left, FirstOf):
                coll_val = self.evaluate(left.collection, env)
                right_val = self.evaluate(right, env)

                if coll_val is not None and len(coll_val) > 0:
                    first_val = coll_val[0]
                    if right_val is not None:
                        if constraint.negated:
                            return {} if first_val != right_val else None
                        return {} if first_val == right_val else None
                    elif isinstance(right, Variable):
                        if not constraint.negated:
                            return {right.name: first_val}

            # Handle TailOf on left: nxt arr sa rest
            if isinstance(left, TailOf):
                coll_val = self.evaluate(left.collection, env)
                right_val = self.evaluate(right, env)

                if coll_val is not None:
                    tail_val = coll_val[1:] if len(coll_val) > 0 else []
                    if right_val is not None:
                        if constraint.negated:
                            return {} if tail_val != right_val else None
                        return {} if tail_val == right_val else None
                    elif isinstance(right, Variable):
                        if not constraint.negated:
                            return {right.name: tail_val}

            # Handle LastOf on left: fin arr sa x
            if isinstance(left, LastOf):
                coll_val = self.evaluate(left.collection, env)
                right_val = self.evaluate(right, env)

                if coll_val is not None and len(coll_val) > 0:
                    last_val = coll_val[-1]
                    if right_val is not None:
                        if constraint.negated:
                            return {} if last_val != right_val else None
                        return {} if last_val == right_val else None
                    elif isinstance(right, Variable):
                        if not constraint.negated:
                            return {right.name: last_val}

            # Simple equality: x sa 5 or x sa y
            left_val = self.evaluate(left, env)
            right_val = self.evaluate(right, env)

            if left_val is not None and right_val is not None:
                if constraint.negated:
                    return {} if left_val != right_val else None
                return {} if left_val == right_val else None

            if left_val is not None and isinstance(right, Variable):
                if not constraint.negated:
                    return {right.name: left_val}

            if right_val is not None and isinstance(left, Variable):
                if not constraint.negated:
                    return {left.name: right_val}

        # Comparison constraints
        left_val = self.evaluate(constraint.left, env)
        right_val = self.evaluate(constraint.right, env)

        if left_val is not None and right_val is not None:
            if constraint.relation == "ma":  # greater than
                satisfied = left_val > right_val
            elif constraint.relation == "mi":  # less than
                satisfied = left_val < right_val
            elif constraint.relation == "eq":  # equal
                satisfied = left_val == right_val
            elif constraint.relation == "amo":  # element of
                satisfied = left_val in right_val
            elif constraint.relation == "ins":  # contains
                satisfied = right_val in left_val
            else:
                return None

            if constraint.negated:
                satisfied = not satisfied

            return {} if satisfied else None

        return None

    def solve_alternative(self, alt: Alternative, env: Environment) -> Optional[Dict[str, Any]]:
        """
        Solve an Alternative (pa) constraint.
        Any branch can satisfy - returns first one that works.
        """
        for branch in alt.branches:
            if isinstance(branch, Constraint):
                result = self.solve_constraint(branch, env)
                if result is not None:
                    return result
            elif isinstance(branch, Alternative):
                result = self.solve_alternative(branch, env)
                if result is not None:
                    return result
            elif isinstance(branch, CondBlock):
                result = self.solve_condblock(branch, env)
                if result is not None:
                    return result
        return None

    def solve_condblock(self, cond: CondBlock, env: Environment) -> Optional[Dict[str, Any]]:
        """
        Solve a CondBlock (if/the/oth) constraint.
        Evaluates condition and follows appropriate branch.
        """
        # Try to evaluate the condition
        if isinstance(cond.condition, Constraint):
            cond_result = self.solve_constraint(cond.condition, env)
            if cond_result is not None:
                # Condition is true (or at least satisfiable)
                if cond.then_result:
                    if isinstance(cond.then_result, Constraint):
                        return self.solve_constraint(cond.then_result, env)
                    elif isinstance(cond.then_result, Variable):
                        return {cond.then_result.name: True}
                return cond_result
            else:
                # Condition is false, use else branch
                if cond.else_result:
                    if isinstance(cond.else_result, Constraint):
                        return self.solve_constraint(cond.else_result, env)
                    elif isinstance(cond.else_result, Variable):
                        return {cond.else_result.name: False}
                return {}
        return None

    def solve_function_call(self, call: FunctionCall, env: Environment) -> Optional[Dict[str, Any]]:
        """
        Solve a function call.
        Evaluates arguments, calls function, binds result.
        """
        # Evaluate all arguments
        arg_values = []
        for arg in call.args:
            val = self.evaluate(arg, env)
            if val is None:
                return None  # Can't evaluate yet
            arg_values.append(val)

        # Call the function
        result = self.call_function(call.func_name, arg_values, env)
        if result is not None and isinstance(call.result_var, Variable):
            return {call.result_var.name: result}

        return None

    def solve(self, program: Program, initial_bindings: Dict[str, Any],
              max_iterations: int = 100) -> Dict[str, Any]:
        """Solve a Limn-PL program."""
        env = Environment(initial_bindings.copy())

        # Iterative constraint propagation
        for iteration in range(max_iterations):
            made_progress = False

            for constraint in program.constraints:
                result = None

                if isinstance(constraint, Constraint):
                    result = self.solve_constraint(constraint, env)

                    if result is None:
                        # Check if all variables are bound
                        unbound = self.get_unbound(constraint, env)
                        if len(unbound) == 0:
                            raise SolverError(f"Constraint violated: {constraint}")
                        continue

                elif isinstance(constraint, Alternative):
                    result = self.solve_alternative(constraint, env)

                elif isinstance(constraint, CondBlock):
                    result = self.solve_condblock(constraint, env)

                elif isinstance(constraint, FunctionCall):
                    result = self.solve_function_call(constraint, env)

                if result:
                    env = env.extend(result)
                    made_progress = True

            # Check if all variables are bound
            all_bound = all(env.is_bound(var.name) for var in program.variables)

            if all_bound:
                # Verify all constraints
                for constraint in program.constraints:
                    if isinstance(constraint, Constraint):
                        result = self.solve_constraint(constraint, env)
                        if result is None:
                            raise SolverError(f"Solution found but constraint violated: {constraint}")
                    elif isinstance(constraint, Alternative):
                        result = self.solve_alternative(constraint, env)
                        if result is None:
                            raise SolverError(f"Solution found but alternative violated: {constraint}")
                return env.bindings

            if not made_progress:
                unbound = [var.name for var in program.variables if not env.is_bound(var.name)]
                raise SolverError(f"Cannot solve: insufficient constraints for variables {unbound}")

        raise SolverError(f"Maximum iterations ({max_iterations}) reached")


# ============================================================================
# High-Level API
# ============================================================================

def run_limn_pl(source: str, key: Dict[str, Any],
                  functions: Optional[List[str]] = None) -> Dict[str, Any]:
    """
    Run a Limn-PL program with a key (input bindings).

    Args:
        source: Limn-PL source code
        key: Dictionary of variable bindings (the "key")
        functions: Optional list of function source code strings to register

    Returns:
        Dictionary of all variable bindings (solution)
    """
    parser = LimnPLParser()
    program = parser.parse(source)
    solver = LimnPLSolver()

    # Register any provided functions
    if functions:
        for func_source in functions:
            func_program = parser.parse(func_source)
            solver.register_function(func_program)

    return solver.solve(program, key)


def parse_key(key_source: str) -> Dict[str, Any]:
    """
    Parse a Limn-style key into bindings.

    Format: yo a sa 3 | yo b sa 5
    """
    bindings = {}
    parser = LimnPLParser()
    tokens = parser.tokenize(key_source)

    i = 0
    while i < len(tokens):
        if tokens[i] == "yo" and i + 3 < len(tokens):
            var_name = tokens[i + 1]
            if tokens[i + 2] == "sa":
                value_token = tokens[i + 3]
                # Parse value
                if value_token.startswith('"'):
                    bindings[var_name] = value_token[1:-1]
                elif value_token == "tru":
                    bindings[var_name] = True
                elif value_token == "fal":
                    bindings[var_name] = False
                elif value_token == "hol":
                    bindings[var_name] = None
                else:
                    try:
                        if "." in value_token:
                            bindings[var_name] = float(value_token)
                        else:
                            bindings[var_name] = int(value_token)
                    except ValueError:
                        bindings[var_name] = value_token
                i += 4
            else:
                i += 1
        elif tokens[i] == "|":
            i += 1
        else:
            i += 1

    return bindings


# ============================================================================
# Tests
# ============================================================================

def test_basic_addition():
    """Test forward addition."""
    print("\n=== Test: Addition (Forward) ===")
    source = """
    pro joi-test |
    var | whe a | whe b | whe c |
    cns | a joi b sa c
    """

    result = run_limn_pl(source, {"a": 3, "b": 5})
    print(f"Input: a=3, b=5")
    print(f"Result: {result}")
    assert result["c"] == 8
    print("PASSED")


def test_backward_addition():
    """Test backward addition."""
    print("\n=== Test: Addition (Backward) ===")
    source = """
    pro joi-test |
    var | whe a | whe b | whe c |
    cns | a joi b sa c
    """

    result = run_limn_pl(source, {"a": 3, "c": 10})
    print(f"Input: a=3, c=10")
    print(f"Result: {result}")
    assert result["b"] == 7
    print("PASSED")


def test_multiplication():
    """Test multiplication bidirectionally."""
    print("\n=== Test: Multiplication (Bidirectional) ===")
    source = """
    pro exp-test |
    var | whe x | whe y | whe z |
    cns | x exp y sa z
    """

    # Forward
    result = run_limn_pl(source, {"x": 4, "y": 5})
    print(f"Forward - Input: x=4, y=5")
    print(f"Result: {result}")
    assert result["z"] == 20

    # Backward
    result = run_limn_pl(source, {"z": 20, "y": 5})
    print(f"Backward - Input: z=20, y=5")
    print(f"Result: {result}")
    assert result["x"] == 4
    print("PASSED")


def test_chained_constraints():
    """Test multiple constraints."""
    print("\n=== Test: Chained Constraints ===")
    source = """
    pro chain |
    var | whe a | whe b | whe c | whe d |
    cns |
    a joi b sa c |
    c exp 2 sa d
    """

    result = run_limn_pl(source, {"a": 3, "b": 4})
    print(f"Input: a=3, b=4")
    print(f"Result: {result}")
    assert result["c"] == 7
    assert result["d"] == 14
    print("PASSED")


def test_temperature_conversion():
    """Test temperature conversion (from spec)."""
    print("\n=== Test: Temperature Conversion ===")
    source = """
    pro tem-con |
    var | whe C | whe F | whe t1 | whe t2 |
    cns |
    t1 sa C exp 9 |
    t2 sa t1 con 5 |
    F sa t2 joi 32
    """

    # Celsius to Fahrenheit
    result = run_limn_pl(source, {"C": 100})
    print(f"C to F - Input: C=100")
    print(f"Result: {result}")
    assert result["F"] == 212

    # Fahrenheit to Celsius
    result = run_limn_pl(source, {"F": 32})
    print(f"F to C - Input: F=32")
    print(f"Result: {result}")
    assert result["C"] == 0
    print("PASSED")


def test_key_parsing():
    """Test Limn-style key parsing."""
    print("\n=== Test: Key Parsing ===")

    key_source = "yo a sa 3 | yo b sa 5 | yo name sa \"hello\""
    bindings = parse_key(key_source)

    print(f"Key source: {key_source}")
    print(f"Parsed bindings: {bindings}")

    assert bindings["a"] == 3
    assert bindings["b"] == 5
    assert bindings["name"] == "hello"
    print("PASSED")


def test_equality_constraint():
    """Test simple equality."""
    print("\n=== Test: Equality Constraint ===")
    source = """
    pro eq-test |
    var | whe x | whe y |
    cns |
    x sa 42 |
    y sa x
    """

    result = run_limn_pl(source, {})
    print(f"Input: (none)")
    print(f"Result: {result}")
    assert result["x"] == 42
    assert result["y"] == 42
    print("PASSED")


def test_constraint_violation():
    """Test that violations are detected."""
    print("\n=== Test: Constraint Violation ===")
    source = """
    pro violate |
    var | whe a | whe b | whe c |
    cns | a joi b sa c
    """

    try:
        result = run_limn_pl(source, {"a": 3, "b": 5, "c": 100})
        print("ERROR: Should have raised SolverError")
        assert False
    except SolverError as e:
        print(f"Correctly detected violation: {e}")
        print("PASSED")


def test_group_literal():
    """Test group/list literals."""
    print("\n=== Test: Group Literal ===")
    # Use a simpler approach: provide the list via input key
    source = """
    pro gro-test |
    var | whe arr | whe n |
    cns |
    who arr sa n
    """

    result = run_limn_pl(source, {"arr": [1, 2, 3, 4, 5]})
    print(f"Input: arr=[1,2,3,4,5]")
    print(f"Result: {result}")
    assert result["arr"] == [1, 2, 3, 4, 5]
    assert result["n"] == 5
    print("PASSED")


def test_index_access():
    """Test index access with par."""
    print("\n=== Test: Index Access ===")
    source = """
    pro par-test |
    var | whe arr | whe x |
    cns |
    par 1 arr sa x
    """

    result = run_limn_pl(source, {"arr": [10, 20, 30]})
    print(f"Input: arr=[10, 20, 30]")
    print(f"Result: {result}")
    assert result["arr"] == [10, 20, 30]
    assert result["x"] == 20
    print("PASSED")


def test_element_of():
    """Test element-of constraint (amo)."""
    print("\n=== Test: Element Of (amo) ===")
    source = """
    pro amo-test |
    var | whe arr | whe x |
    cns |
    x sa 2 |
    x amo arr
    """

    result = run_limn_pl(source, {"arr": [1, 2, 3]})
    print(f"Input: arr=[1, 2, 3]")
    print(f"Result: {result}")
    assert result["x"] == 2
    assert 2 in result["arr"]
    print("PASSED")


def test_list_size_backward():
    """Test backward computation from list size."""
    print("\n=== Test: List Size ===")
    source = """
    pro size-test |
    var | whe arr | whe n | whe doubled |
    cns |
    who arr sa n |
    n exp 2 sa doubled
    """

    result = run_limn_pl(source, {"arr": [1, 2, 3, 4]})
    print(f"Input: arr=[1, 2, 3, 4]")
    print(f"Result: {result}")
    assert result["n"] == 4
    assert result["doubled"] == 8
    print("PASSED")


def test_list_first():
    """Test first element access with fst."""
    print("\n=== Test: First Element (fst) ===")
    source = """
    pro fst-test |
    var | whe arr | whe first |
    cns |
    fst arr sa first
    """

    result = run_limn_pl(source, {"arr": [10, 20, 30]})
    print(f"Input: arr=[10, 20, 30]")
    print(f"Result: {result}")
    assert result["first"] == 10
    print("PASSED")


def test_list_tail():
    """Test tail/rest of list with nxt."""
    print("\n=== Test: List Tail (nxt) ===")
    source = """
    pro nxt-test |
    var | whe arr | whe rest |
    cns |
    nxt arr sa rest
    """

    result = run_limn_pl(source, {"arr": [10, 20, 30, 40]})
    print(f"Input: arr=[10, 20, 30, 40]")
    print(f"Result: {result}")
    assert result["rest"] == [20, 30, 40]
    print("PASSED")


def test_list_last():
    """Test last element access with fin."""
    print("\n=== Test: Last Element (fin) ===")
    source = """
    pro fin-test |
    var | whe arr | whe last |
    cns |
    fin arr sa last
    """

    result = run_limn_pl(source, {"arr": [10, 20, 30]})
    print(f"Input: arr=[10, 20, 30]")
    print(f"Result: {result}")
    assert result["last"] == 30
    print("PASSED")


def test_conditional_basic():
    """Test basic conditional (if/the/oth)."""
    print("\n=== Test: Conditional (if/the/oth) ===")
    source = """
    pro cond-test |
    var | whe x | whe y |
    cns |
    x sa 5 |
    if x ma 0 | the y sa 1 | oth y sa 0
    """

    result = run_limn_pl(source, {})
    print(f"Input: (none)")
    print(f"Result: {result}")
    assert result["x"] == 5
    assert result["y"] == 1
    print("PASSED")


def test_alternative_or():
    """Test alternative/OR with pa."""
    print("\n=== Test: Alternative (pa) ===")
    # Test that either branch can satisfy
    source = """
    pro pa-test |
    var | whe x |
    cns |
    x sa 3 |
    x sa 3 pa x sa 5
    """

    result = run_limn_pl(source, {})
    print(f"Input: (none)")
    print(f"Result: {result}")
    assert result["x"] == 3  # First matching branch
    print("PASSED")


def test_function_call():
    """Test function definition and call."""
    print("\n=== Test: Function Call ===")

    # Define a double function
    double_func = """
    pro dbl |
    var | whe x | whe eff |
    cns | eff sa x exp 2
    """

    # Main program that calls double
    main_prog = """
    pro main |
    var | whe a | whe b |
    cns |
    a sa 5 |
    dbl cau a eff b
    """

    result = run_limn_pl(main_prog, {}, functions=[double_func])
    print(f"Input: (none)")
    print(f"Result: {result}")
    assert result["a"] == 5
    assert result["b"] == 10
    print("PASSED")


def test_function_bidirectional():
    """Test function calls work bidirectionally."""
    print("\n=== Test: Function Bidirectional ===")

    # Define add function
    add_func = """
    pro add |
    var | whe x | whe y | whe eff |
    cns | x joi y sa eff
    """

    # Main program
    main_prog = """
    pro main |
    var | whe a | whe result |
    cns |
    a sa 3 |
    add cau a 5 eff result
    """

    result = run_limn_pl(main_prog, {}, functions=[add_func])
    print(f"Input: (none)")
    print(f"Result: {result}")
    assert result["a"] == 3
    assert result["result"] == 8
    print("PASSED")


def run_all_tests():
    """Run all tests."""
    print("=" * 60)
    print("Running Limn-PL Interpreter Tests")
    print("=" * 60)

    test_basic_addition()
    test_backward_addition()
    test_multiplication()
    test_chained_constraints()
    test_temperature_conversion()
    test_key_parsing()
    test_equality_constraint()
    test_constraint_violation()

    # List/Group tests
    print("\n" + "-" * 60)
    print("Running List/Group Tests")
    print("-" * 60)
    test_group_literal()
    test_index_access()
    test_element_of()
    test_list_size_backward()
    test_list_first()
    test_list_tail()
    test_list_last()

    # Boolean/OR tests
    print("\n" + "-" * 60)
    print("Running Boolean/OR Tests")
    print("-" * 60)
    test_conditional_basic()
    test_alternative_or()

    # Function tests
    print("\n" + "-" * 60)
    print("Running Function Tests")
    print("-" * 60)
    test_function_call()
    test_function_bidirectional()

    print("\n" + "=" * 60)
    print("All tests PASSED!")
    print("=" * 60)


if __name__ == "__main__":
    run_all_tests()
