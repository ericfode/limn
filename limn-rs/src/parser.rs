//! Parser for Limn source code
//!
//! Converts a stream of tokens into an AST.

use crate::ast::*;
use crate::error::{LimnError, Result};
use crate::lexer::{Token, TokenKind};
use crate::value::Value;

/// Parser for Limn programs
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    /// Parse a complete source file (program + optional key)
    pub fn parse_file(&mut self) -> Result<SourceFile> {
        let program = self.parse_program()?;

        // Check for key separator
        let key = if self.check(&TokenKind::KeySep) {
            self.advance();
            Some(self.parse_key()?)
        } else {
            None
        };

        Ok(SourceFile { program, key })
    }

    /// Parse a program section
    fn parse_program(&mut self) -> Result<Program> {
        self.skip_newlines();

        // Optional program name
        let name = if self.check(&TokenKind::Pro) {
            self.advance();
            if let Some(Token { kind: TokenKind::Identifier(name), .. }) = self.peek() {
                let name = name.clone();
                self.advance();
                name
            } else {
                "unnamed".to_string()
            }
        } else {
            "unnamed".to_string()
        };

        let mut program = Program::new(name);

        // Parse imports, variables, and constraints
        while !self.is_at_end() && !self.check(&TokenKind::KeySep) {
            self.skip_newlines();

            if self.is_at_end() || self.check(&TokenKind::KeySep) {
                break;
            }

            // Import statement
            if self.check(&TokenKind::Use) {
                program.imports.push(self.parse_import()?);
                continue;
            }

            // Variable declaration
            if self.check(&TokenKind::Whe) {
                self.advance();
                let var = self.parse_variable_name()?;
                program.variables.push(var);
                continue;
            }

            // Section markers (for structured format)
            if self.check(&TokenKind::Var) {
                self.advance();
                self.skip_pipes();
                continue;
            }
            if self.check(&TokenKind::Cns) {
                self.advance();
                self.skip_pipes();
                continue;
            }

            // Pipe separator
            if self.check(&TokenKind::Pipe) {
                self.advance();
                continue;
            }

            // Comment
            if let Some(Token { kind: TokenKind::Comment(_), .. }) = self.peek() {
                self.advance();
                continue;
            }

            // Constraint
            let constraint = self.parse_constraint()?;
            program.constraints.push(constraint);
        }

        Ok(program)
    }

    /// Parse an import statement
    fn parse_import(&mut self) -> Result<Import> {
        self.expect(&TokenKind::Use)?;

        let kind = if self.check(&TokenKind::Cid) {
            self.advance();
            let cid = self.expect_identifier()?;
            ImportKind::Cid(cid)
        } else if self.check(&TokenKind::Nom) {
            self.advance();
            let name = self.expect_string()?;

            let version = if self.check(&TokenKind::Ver) {
                self.advance();
                let major = self.expect_integer()? as u32;
                let minor = self.expect_integer()? as u32;
                let patch = self.expect_integer()? as u32;
                Some((major, minor, patch))
            } else {
                None
            };

            let registry = if self.check(&TokenKind::From) {
                self.advance();
                Some(self.expect_string()?)
            } else {
                None
            };

            ImportKind::Name { name, version, registry }
        } else {
            return Err(self.error("Expected 'cid' or 'nom' after 'use'"));
        };

        // Parse selective imports
        let mut selective = Vec::new();
        while self.check(&TokenKind::Pipe) {
            self.advance();
            if self.check(&TokenKind::Get) {
                self.advance();
                let name = self.expect_identifier()?;
                selective.push(name);
            } else {
                break;
            }
        }

        // Parse alias
        let alias = if self.check(&TokenKind::As) {
            self.advance();
            Some(self.expect_identifier()?)
        } else {
            None
        };

        Ok(Import { kind, selective, alias })
    }

    /// Parse a key section
    fn parse_key(&mut self) -> Result<Key> {
        let mut key = Key::new();

        while !self.is_at_end() {
            self.skip_newlines();

            if self.is_at_end() {
                break;
            }

            // Comment
            if let Some(Token { kind: TokenKind::Comment(_), .. }) = self.peek() {
                self.advance();
                continue;
            }

            // Pipe
            if self.check(&TokenKind::Pipe) {
                self.advance();
                continue;
            }

            // Key binding: var sa value
            // or: yo var sa value
            if self.check(&TokenKind::Yo) {
                self.advance();
            }

            if let Some(Token { kind: TokenKind::Identifier(name), .. }) = self.peek() {
                let name = name.clone();
                self.advance();

                if self.check(&TokenKind::Sa) {
                    self.advance();
                    let value = self.parse_value()?;
                    key.bindings.push(KeyBinding { variable: name, value });
                } else {
                    // Not a binding, skip
                    continue;
                }
            } else {
                // Skip unknown tokens in key section
                self.advance();
            }
        }

        Ok(key)
    }

    /// Parse a constraint
    fn parse_constraint(&mut self) -> Result<Constraint> {
        let line = self.current_line();

        // Check for negation
        let negated = if self.check(&TokenKind::Nu) {
            self.advance();
            true
        } else {
            false
        };

        // Check for conditional
        if self.check(&TokenKind::If) {
            return self.parse_conditional(negated, line);
        }

        // Parse left expression
        let left = self.parse_expression()?;

        // Relation
        let kind = if self.check(&TokenKind::Sa) {
            self.advance();
            let right = self.parse_expression()?;
            ConstraintKind::Equals { left, right }
        } else if self.check(&TokenKind::Ma) {
            self.advance();
            let right = self.parse_expression()?;
            ConstraintKind::GreaterThan { left, right }
        } else if self.check(&TokenKind::Mi) {
            self.advance();
            let right = self.parse_expression()?;
            ConstraintKind::LessThan { left, right }
        } else if self.check(&TokenKind::Amo) {
            self.advance();
            let collection = self.parse_expression()?;
            ConstraintKind::ElementOf { element: left, collection }
        } else if self.check(&TokenKind::Cau) {
            // Function call: func cau args eff result
            self.advance();
            let func_name = if let Expr::Variable(name) = left {
                name
            } else {
                return Err(self.error("Expected function name before 'cau'"));
            };

            let mut args = Vec::new();
            while !self.check(&TokenKind::Eff) && !self.is_at_end() &&
                  !self.check(&TokenKind::Newline) && !self.check(&TokenKind::Pipe) {
                args.push(self.parse_expression()?);
            }

            let result = if self.check(&TokenKind::Eff) {
                self.advance();
                self.expect_identifier()?
            } else {
                return Err(self.error("Expected 'eff' after function arguments"));
            };

            ConstraintKind::FunctionCall {
                function: func_name,
                args,
                result,
            }
        } else {
            // Just an expression - treat as equals to itself (identity)
            ConstraintKind::Equals { left: left.clone(), right: left }
        };

        Ok(Constraint { kind, negated, line })
    }

    /// Parse a conditional constraint
    fn parse_conditional(&mut self, negated: bool, line: usize) -> Result<Constraint> {
        self.expect(&TokenKind::If)?;
        let condition = self.parse_expression()?;

        // Expect 'cau' or pipe
        if self.check(&TokenKind::Cau) {
            self.advance();
        }

        let mut consequent = Vec::new();
        while !self.check(&TokenKind::Oth) && !self.check(&TokenKind::Newline) &&
              !self.check(&TokenKind::Pipe) && !self.is_at_end() {
            consequent.push(self.parse_constraint()?);
        }

        let alternative = if self.check(&TokenKind::Oth) {
            self.advance();
            let mut alt = Vec::new();
            while !self.check(&TokenKind::Newline) && !self.check(&TokenKind::Pipe) && !self.is_at_end() {
                alt.push(self.parse_constraint()?);
            }
            Some(alt)
        } else {
            None
        };

        Ok(Constraint {
            kind: ConstraintKind::Conditional {
                condition,
                consequent,
                alternative,
            },
            negated,
            line,
        })
    }

    /// Parse an expression
    fn parse_expression(&mut self) -> Result<Expr> {
        self.parse_additive()
    }

    /// Parse additive expressions: joi, cut
    fn parse_additive(&mut self) -> Result<Expr> {
        let mut left = self.parse_multiplicative()?;

        while self.check(&TokenKind::Joi) || self.check(&TokenKind::Cut) {
            let op = if self.check(&TokenKind::Joi) {
                self.advance();
                BinaryOp::Add
            } else {
                self.advance();
                BinaryOp::Sub
            };

            let right = self.parse_multiplicative()?;
            left = Expr::binary(op, left, right);
        }

        Ok(left)
    }

    /// Parse multiplicative expressions: exp, con
    fn parse_multiplicative(&mut self) -> Result<Expr> {
        let mut left = self.parse_power()?;

        while self.check(&TokenKind::Exp) || self.check(&TokenKind::Con) {
            let op = if self.check(&TokenKind::Exp) {
                self.advance();
                BinaryOp::Mul
            } else {
                self.advance();
                BinaryOp::Div
            };

            let right = self.parse_power()?;
            left = Expr::binary(op, left, right);
        }

        Ok(left)
    }

    /// Parse power expressions: pow, roo
    fn parse_power(&mut self) -> Result<Expr> {
        let mut left = self.parse_unary()?;

        while self.check(&TokenKind::Pow) || self.check(&TokenKind::Roo) {
            let op = if self.check(&TokenKind::Pow) {
                self.advance();
                BinaryOp::Pow
            } else {
                self.advance();
                BinaryOp::Root
            };

            let right = self.parse_unary()?;
            left = Expr::binary(op, left, right);
        }

        Ok(left)
    }

    /// Parse unary expressions: nu
    fn parse_unary(&mut self) -> Result<Expr> {
        if self.check(&TokenKind::Nu) {
            self.advance();
            let operand = self.parse_unary()?;
            return Ok(Expr::unary(UnaryOp::Negate, operand));
        }

        self.parse_collection_ops()
    }

    /// Parse collection operations: par, who, fst, nxt, fin
    fn parse_collection_ops(&mut self) -> Result<Expr> {
        if self.check(&TokenKind::Par) {
            self.advance();
            let index = self.parse_primary()?;
            let collection = self.parse_primary()?;
            return Ok(Expr::Index {
                index: Box::new(index),
                collection: Box::new(collection),
            });
        }

        if self.check(&TokenKind::Who) {
            self.advance();
            let collection = self.parse_primary()?;
            return Ok(Expr::Size(Box::new(collection)));
        }

        if self.check(&TokenKind::Fst) {
            self.advance();
            let collection = self.parse_primary()?;
            return Ok(Expr::First(Box::new(collection)));
        }

        if self.check(&TokenKind::Nxt) {
            self.advance();
            let collection = self.parse_primary()?;
            return Ok(Expr::Rest(Box::new(collection)));
        }

        if self.check(&TokenKind::Fin) {
            self.advance();
            let collection = self.parse_primary()?;
            return Ok(Expr::Last(Box::new(collection)));
        }

        self.parse_primary()
    }

    /// Parse primary expressions: literals, variables, groups
    fn parse_primary(&mut self) -> Result<Expr> {
        // Group literal: gro | elem1 | elem2 |
        if self.check(&TokenKind::Gro) {
            self.advance();
            let mut elements = Vec::new();

            if self.check(&TokenKind::Pipe) {
                self.advance();
                while !self.check(&TokenKind::Pipe) && !self.is_at_end() {
                    elements.push(self.parse_expression()?);
                    if self.check(&TokenKind::Pipe) {
                        self.advance();
                        if self.check(&TokenKind::Pipe) {
                            // End of group
                            break;
                        }
                    }
                }
            }

            return Ok(Expr::Group(elements));
        }

        // Literals
        if let Some(Token { kind: TokenKind::Integer(n), .. }) = self.peek() {
            let n = *n;
            self.advance();
            return Ok(Expr::int(n));
        }

        if let Some(Token { kind: TokenKind::Float(f), .. }) = self.peek() {
            let f = *f;
            self.advance();
            return Ok(Expr::float(f));
        }

        if let Some(Token { kind: TokenKind::String(s), .. }) = self.peek() {
            let s = s.clone();
            self.advance();
            return Ok(Expr::string(s));
        }

        if self.check(&TokenKind::Tru) {
            self.advance();
            return Ok(Expr::bool(true));
        }

        if self.check(&TokenKind::Fal) {
            self.advance();
            return Ok(Expr::bool(false));
        }

        if self.check(&TokenKind::Hol) {
            self.advance();
            return Ok(Expr::Literal(Value::Null));
        }

        // Variable
        if let Some(Token { kind: TokenKind::Identifier(name), .. }) = self.peek() {
            let name = name.clone();
            self.advance();
            return Ok(Expr::var(name));
        }

        Err(self.error("Expected expression"))
    }

    /// Parse a simple value (for key section)
    fn parse_value(&mut self) -> Result<Value> {
        if let Some(Token { kind: TokenKind::Integer(n), .. }) = self.peek() {
            let n = *n;
            self.advance();
            return Ok(Value::Integer(n));
        }

        if let Some(Token { kind: TokenKind::Float(f), .. }) = self.peek() {
            let f = *f;
            self.advance();
            return Ok(Value::Float(f));
        }

        if let Some(Token { kind: TokenKind::String(s), .. }) = self.peek() {
            let s = s.clone();
            self.advance();
            return Ok(Value::String(s));
        }

        if self.check(&TokenKind::Tru) {
            self.advance();
            return Ok(Value::Boolean(true));
        }

        if self.check(&TokenKind::Fal) {
            self.advance();
            return Ok(Value::Boolean(false));
        }

        if self.check(&TokenKind::Hol) {
            self.advance();
            return Ok(Value::Null);
        }

        // Variable reference in key - treat as string for now
        if let Some(Token { kind: TokenKind::Identifier(name), .. }) = self.peek() {
            let name = name.clone();
            self.advance();
            return Ok(Value::String(name));
        }

        Err(self.error("Expected value"))
    }

    // Helper methods

    fn parse_variable_name(&mut self) -> Result<Variable> {
        let name = self.expect_identifier()?;
        Ok(Variable::new(name))
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn advance(&mut self) -> Option<&Token> {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.tokens.get(self.current - 1)
    }

    fn check(&self, kind: &TokenKind) -> bool {
        if let Some(token) = self.peek() {
            std::mem::discriminant(&token.kind) == std::mem::discriminant(kind)
        } else {
            false
        }
    }

    fn expect(&mut self, kind: &TokenKind) -> Result<&Token> {
        if self.check(kind) {
            Ok(self.advance().unwrap())
        } else {
            Err(self.error(format!("Expected {:?}", kind)))
        }
    }

    fn expect_identifier(&mut self) -> Result<String> {
        if let Some(Token { kind: TokenKind::Identifier(name), .. }) = self.peek() {
            let name = name.clone();
            self.advance();
            Ok(name)
        } else {
            Err(self.error("Expected identifier"))
        }
    }

    fn expect_string(&mut self) -> Result<String> {
        if let Some(Token { kind: TokenKind::String(s), .. }) = self.peek() {
            let s = s.clone();
            self.advance();
            Ok(s)
        } else {
            Err(self.error("Expected string"))
        }
    }

    fn expect_integer(&mut self) -> Result<i64> {
        if let Some(Token { kind: TokenKind::Integer(n), .. }) = self.peek() {
            let n = *n;
            self.advance();
            Ok(n)
        } else {
            Err(self.error("Expected integer"))
        }
    }

    fn skip_newlines(&mut self) {
        while self.check(&TokenKind::Newline) {
            self.advance();
        }
    }

    fn skip_pipes(&mut self) {
        while self.check(&TokenKind::Pipe) || self.check(&TokenKind::Newline) {
            self.advance();
        }
    }

    fn is_at_end(&self) -> bool {
        matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Eof) | None)
    }

    fn current_line(&self) -> usize {
        self.peek().map(|t| t.line).unwrap_or(0)
    }

    fn error(&self, message: impl Into<String>) -> LimnError {
        let line = self.current_line();
        LimnError::parser(line, message)
    }
}

/// Parse source code into a SourceFile
pub fn parse(source: &str) -> Result<SourceFile> {
    let mut lexer = crate::lexer::Lexer::new(source);
    let tokens = lexer.tokenize()?;
    let mut parser = Parser::new(tokens);
    parser.parse_file()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple() {
        let source = "whe x\nwhe y\nwhe result\nx joi y sa result";
        let file = parse(source).unwrap();

        assert_eq!(file.program.variables.len(), 3);
        assert_eq!(file.program.constraints.len(), 1);
    }

    #[test]
    fn test_parse_with_key() {
        let source = "whe x\nwhe y\nx joi y sa result\n---\nx sa 5\ny sa 3";
        let file = parse(source).unwrap();

        assert!(file.key.is_some());
        let key = file.key.unwrap();
        assert_eq!(key.bindings.len(), 2);
    }
}
