//! Abstract Syntax Tree for Limn programs

use crate::value::Value;

/// A Limn program
#[derive(Debug, Clone)]
pub struct Program {
    pub name: String,
    pub variables: Vec<Variable>,
    pub constraints: Vec<Constraint>,
    pub imports: Vec<Import>,
}

impl Program {
    pub fn new(name: impl Into<String>) -> Self {
        Program {
            name: name.into(),
            variables: Vec::new(),
            constraints: Vec::new(),
            imports: Vec::new(),
        }
    }
}

/// A variable declaration
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variable {
    pub name: String,
}

impl Variable {
    pub fn new(name: impl Into<String>) -> Self {
        Variable { name: name.into() }
    }
}

/// An import statement
#[derive(Debug, Clone)]
pub struct Import {
    pub kind: ImportKind,
    pub selective: Vec<String>,
    pub alias: Option<String>,
}

#[derive(Debug, Clone)]
pub enum ImportKind {
    /// Import by CID: use cid bafybei...
    Cid(String),
    /// Import by name: use nom "package-name"
    Name {
        name: String,
        version: Option<(u32, u32, u32)>,
        registry: Option<String>,
    },
}

/// A constraint in the program
#[derive(Debug, Clone)]
pub struct Constraint {
    pub kind: ConstraintKind,
    pub negated: bool,
    pub line: usize,
}

#[derive(Debug, Clone)]
pub enum ConstraintKind {
    /// Equality: left sa right
    Equals { left: Expr, right: Expr },

    /// Greater than: left ma right
    GreaterThan { left: Expr, right: Expr },

    /// Less than: left mi right
    LessThan { left: Expr, right: Expr },

    /// Conditional: if cond cau consequent
    Conditional {
        condition: Expr,
        consequent: Vec<Constraint>,
        alternative: Option<Vec<Constraint>>,
    },

    /// Function call: func cau args eff result
    FunctionCall {
        function: String,
        args: Vec<Expr>,
        result: String,
    },

    /// Element of: elem amo collection
    ElementOf { element: Expr, collection: Expr },
}

/// An expression
#[derive(Debug, Clone)]
pub enum Expr {
    /// Variable reference
    Variable(String),

    /// Literal value
    Literal(Value),

    /// Binary operation
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },

    /// Unary operation
    Unary {
        op: UnaryOp,
        operand: Box<Expr>,
    },

    /// Group literal: gro | elem1 | elem2 |
    Group(Vec<Expr>),

    /// Index access: par index collection
    Index {
        index: Box<Expr>,
        collection: Box<Expr>,
    },

    /// Size of collection: who collection
    Size(Box<Expr>),

    /// First element: fst collection
    First(Box<Expr>),

    /// Rest of collection: nxt collection
    Rest(Box<Expr>),

    /// Last element: fin collection
    Last(Box<Expr>),

    /// Conditional expression: if cond | the then_val | oth else_val
    Conditional {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Option<Box<Expr>>,
    },

    /// Function call expression: func cau args eff result
    Call {
        function: String,
        args: Vec<Expr>,
    },
}

impl Expr {
    pub fn var(name: impl Into<String>) -> Self {
        Expr::Variable(name.into())
    }

    pub fn int(n: i64) -> Self {
        Expr::Literal(Value::Integer(n))
    }

    pub fn float(f: f64) -> Self {
        Expr::Literal(Value::Float(f))
    }

    pub fn string(s: impl Into<String>) -> Self {
        Expr::Literal(Value::String(s.into()))
    }

    pub fn bool(b: bool) -> Self {
        Expr::Literal(Value::Boolean(b))
    }

    pub fn binary(op: BinaryOp, left: Expr, right: Expr) -> Self {
        Expr::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
        }
    }

    pub fn unary(op: UnaryOp, operand: Expr) -> Self {
        Expr::Unary {
            op,
            operand: Box::new(operand),
        }
    }

    /// Get all variable names referenced in this expression
    pub fn variables(&self) -> Vec<String> {
        let mut vars = Vec::new();
        self.collect_variables(&mut vars);
        vars
    }

    fn collect_variables(&self, vars: &mut Vec<String>) {
        match self {
            Expr::Variable(name) => vars.push(name.clone()),
            Expr::Literal(_) => {}
            Expr::Binary { left, right, .. } => {
                left.collect_variables(vars);
                right.collect_variables(vars);
            }
            Expr::Unary { operand, .. } => {
                operand.collect_variables(vars);
            }
            Expr::Group(elements) => {
                for elem in elements {
                    elem.collect_variables(vars);
                }
            }
            Expr::Index { index, collection } => {
                index.collect_variables(vars);
                collection.collect_variables(vars);
            }
            Expr::Size(collection) | Expr::First(collection) | Expr::Rest(collection) | Expr::Last(collection) => {
                collection.collect_variables(vars);
            }
            Expr::Conditional { condition, then_branch, else_branch } => {
                condition.collect_variables(vars);
                then_branch.collect_variables(vars);
                if let Some(else_branch) = else_branch {
                    else_branch.collect_variables(vars);
                }
            }
            Expr::Call { args, .. } => {
                for arg in args {
                    arg.collect_variables(vars);
                }
            }
        }
    }
}

/// Binary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    /// Addition: joi
    Add,
    /// Subtraction: cut
    Sub,
    /// Multiplication: exp
    Mul,
    /// Division: con
    Div,
    /// Power: pow
    Pow,
    /// Root: roo
    Root,
    /// String concatenation
    Concat,
}

impl BinaryOp {
    pub fn as_str(&self) -> &'static str {
        match self {
            BinaryOp::Add => "joi",
            BinaryOp::Sub => "cut",
            BinaryOp::Mul => "exp",
            BinaryOp::Div => "con",
            BinaryOp::Pow => "pow",
            BinaryOp::Root => "roo",
            BinaryOp::Concat => "joi",
        }
    }

    /// Get the inverse operation for bidirectional solving
    pub fn inverse(&self) -> Option<BinaryOp> {
        match self {
            BinaryOp::Add => Some(BinaryOp::Sub),
            BinaryOp::Sub => Some(BinaryOp::Add),
            BinaryOp::Mul => Some(BinaryOp::Div),
            BinaryOp::Div => Some(BinaryOp::Mul),
            BinaryOp::Pow => Some(BinaryOp::Root),
            BinaryOp::Root => Some(BinaryOp::Pow),
            BinaryOp::Concat => None,
        }
    }
}

/// Unary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    /// Negation: nu
    Negate,
}

impl UnaryOp {
    pub fn as_str(&self) -> &'static str {
        match self {
            UnaryOp::Negate => "nu",
        }
    }
}

/// A key (input bindings)
#[derive(Debug, Clone)]
pub struct Key {
    pub bindings: Vec<KeyBinding>,
}

impl Key {
    pub fn new() -> Self {
        Key { bindings: Vec::new() }
    }

    pub fn with_binding(mut self, name: impl Into<String>, value: Value) -> Self {
        self.bindings.push(KeyBinding {
            variable: name.into(),
            value,
        });
        self
    }
}

impl Default for Key {
    fn default() -> Self {
        Key::new()
    }
}

/// A single key binding
#[derive(Debug, Clone)]
pub struct KeyBinding {
    pub variable: String,
    pub value: Value,
}

/// A Limn source file containing program and optional key
#[derive(Debug, Clone)]
pub struct SourceFile {
    pub program: Program,
    pub key: Option<Key>,
}
