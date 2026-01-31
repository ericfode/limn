//! Error types for the Limn compiler

use thiserror::Error;

/// Main error type for Limn operations
#[derive(Error, Debug)]
pub enum LimnError {
    #[error("Lexer error at line {line}, column {column}: {message}")]
    Lexer {
        line: usize,
        column: usize,
        message: String,
    },

    #[error("Parser error at line {line}: {message}")]
    Parser {
        line: usize,
        message: String,
    },

    #[error("Solver error: {0}")]
    Solver(String),

    #[error("Constraint violation: {0}")]
    ConstraintViolation(String),

    #[error("Undefined variable: {0}")]
    UndefinedVariable(String),

    #[error("Type error: {0}")]
    TypeError(String),

    #[error("Division by zero")]
    DivisionByZero,

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("IPFS error: {0}")]
    Ipfs(String),

    #[error("Package error: {0}")]
    Package(String),

    #[error("Registry error: {0}")]
    Registry(String),

    #[error("Dependency resolution error: {0}")]
    Resolution(String),

    #[error("Signature error: {0}")]
    Signature(String),

    #[error("Configuration error: {0}")]
    Config(String),

    #[error("{0}")]
    Other(String),
}

impl LimnError {
    pub fn lexer(line: usize, column: usize, message: impl Into<String>) -> Self {
        LimnError::Lexer {
            line,
            column,
            message: message.into(),
        }
    }

    pub fn parser(line: usize, message: impl Into<String>) -> Self {
        LimnError::Parser {
            line,
            message: message.into(),
        }
    }

    pub fn solver(message: impl Into<String>) -> Self {
        LimnError::Solver(message.into())
    }

    pub fn package(message: impl Into<String>) -> Self {
        LimnError::Package(message.into())
    }

    pub fn ipfs(message: impl Into<String>) -> Self {
        LimnError::Ipfs(message.into())
    }
}

pub type Result<T> = std::result::Result<T, LimnError>;
