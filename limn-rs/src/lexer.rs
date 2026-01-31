//! Lexer for Limn source code
//!
//! Converts source text into a stream of tokens.

use crate::error::{LimnError, Result};
use std::iter::Peekable;
use std::str::Chars;

/// Token types in Limn
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Structure
    Pro,    // program
    Var,    // variables section
    Cns,    // constraints section

    // Variable marker
    Whe,    // unknown/variable declaration

    // Relations
    Sa,     // equals (same as)
    Ma,     // greater than (more)
    Mi,     // less than (minimum)
    Eq,     // equal comparison

    // Arithmetic operators
    Joi,    // plus/join
    Cut,    // minus/cut
    Exp,    // multiply/expand
    Con,    // divide/contract
    Pow,    // power
    Roo,    // root

    // Unary
    Nu,     // negation

    // Types/Literals
    One,    // integer type
    Flo,    // float type
    Wor,    // string type
    Tru,    // true
    Fal,    // false
    Gro,    // group/list
    Hol,    // null/hole

    // Control
    If,     // conditional
    The,    // then
    Oth,    // otherwise
    Cyc,    // cycle/loop
    Beg,    // begin
    End,    // end

    // Functions
    Cau,    // cause/call
    Eff,    // effect/result

    // Collections
    Amo,    // among/element of
    Ins,    // inside/contains
    Par,    // part/index
    Who,    // whole/size
    Fst,    // first
    Nxt,    // next
    Fin,    // final

    // Quantifiers
    Al,     // all
    Ex,     // exists
    On,     // one/single

    // Logic
    Pa,     // parallel/or
    Yo,     // this/here (binding/key)
    An,     // that/there (reference)

    // Package/Import
    Use,    // import
    Cid,    // content identifier
    Nom,    // name
    Ver,    // version
    Get,    // selective import
    As,     // alias
    From,   // from registry

    // Separators
    Pipe,   // |
    KeySep, // ---

    // Literals
    Integer(i64),
    Float(f64),
    String(String),
    Identifier(String),

    // Special
    Newline,
    Comment(String),
    Eof,
}

/// A token with position information
#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub column: usize,
    pub lexeme: String,
}

impl Token {
    pub fn new(kind: TokenKind, line: usize, column: usize, lexeme: String) -> Self {
        Token { kind, line, column, lexeme }
    }
}

/// Lexer for Limn source code
pub struct Lexer<'a> {
    source: &'a str,
    chars: Peekable<Chars<'a>>,
    line: usize,
    column: usize,
    start_column: usize,
    current_lexeme: String,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            source,
            chars: source.chars().peekable(),
            line: 1,
            column: 1,
            start_column: 1,
            current_lexeme: String::new(),
        }
    }

    /// Tokenize the entire source
    pub fn tokenize(&mut self) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();

        loop {
            let token = self.next_token()?;
            let is_eof = token.kind == TokenKind::Eof;
            tokens.push(token);
            if is_eof {
                break;
            }
        }

        Ok(tokens)
    }

    /// Get the next token
    pub fn next_token(&mut self) -> Result<Token> {
        self.skip_whitespace();
        self.current_lexeme.clear();
        self.start_column = self.column;

        match self.peek() {
            None => Ok(self.make_token(TokenKind::Eof)),

            Some('#') => {
                // Comment
                self.advance();
                let mut comment = String::new();
                while let Some(&c) = self.peek() {
                    if c == '\n' {
                        break;
                    }
                    comment.push(self.advance().unwrap());
                }
                Ok(self.make_token(TokenKind::Comment(comment)))
            }

            Some('\n') => {
                self.advance();
                let token = self.make_token(TokenKind::Newline);
                self.line += 1;
                self.column = 1;
                Ok(token)
            }

            Some('|') => {
                self.advance();
                Ok(self.make_token(TokenKind::Pipe))
            }

            Some('-') => {
                self.advance();
                // Check for --- (key separator) or negative number
                if self.peek() == Some(&'-') {
                    self.advance();
                    if self.peek() == Some(&'-') {
                        self.advance();
                        return Ok(self.make_token(TokenKind::KeySep));
                    }
                    // Oops, just two dashes - treat as identifier?
                    return Err(LimnError::lexer(self.line, self.column, "Unexpected '--'"));
                }
                // Could be negative number
                if let Some(&c) = self.peek() {
                    if c.is_ascii_digit() {
                        return self.scan_number(true);
                    }
                }
                // Just a minus sign - could be an operator
                // But in Limn, 'cut' is the minus operator
                Err(LimnError::lexer(self.line, self.column, "Unexpected '-'"))
            }

            Some('"') => self.scan_string(),

            Some(c) if c.is_ascii_digit() => self.scan_number(false),

            Some(c) if c.is_alphabetic() || *c == '_' => self.scan_identifier(),

            Some(c) => {
                let c = *c;
                self.advance();
                Err(LimnError::lexer(self.line, self.column, format!("Unexpected character: '{}'", c)))
            }
        }
    }

    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        self.current_lexeme.push(c);
        self.column += 1;
        Some(c)
    }

    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.peek() {
            if c == ' ' || c == '\t' || c == '\r' {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn make_token(&self, kind: TokenKind) -> Token {
        Token::new(kind, self.line, self.start_column, self.current_lexeme.clone())
    }

    fn scan_string(&mut self) -> Result<Token> {
        self.advance(); // consume opening quote
        let mut value = String::new();

        loop {
            match self.peek() {
                None | Some('\n') => {
                    return Err(LimnError::lexer(self.line, self.column, "Unterminated string"));
                }
                Some('"') => {
                    self.advance(); // consume closing quote
                    break;
                }
                Some('\\') => {
                    self.advance();
                    match self.advance() {
                        Some('n') => value.push('\n'),
                        Some('t') => value.push('\t'),
                        Some('r') => value.push('\r'),
                        Some('\\') => value.push('\\'),
                        Some('"') => value.push('"'),
                        Some(c) => value.push(c),
                        None => return Err(LimnError::lexer(self.line, self.column, "Unterminated escape")),
                    }
                }
                Some(_) => {
                    value.push(self.advance().unwrap());
                }
            }
        }

        Ok(self.make_token(TokenKind::String(value)))
    }

    fn scan_number(&mut self, negative: bool) -> Result<Token> {
        let mut has_dot = false;

        while let Some(&c) = self.peek() {
            if c.is_ascii_digit() {
                self.advance();
            } else if c == '.' && !has_dot {
                // Look ahead to make sure it's a decimal point, not a method call
                has_dot = true;
                self.advance();
            } else {
                break;
            }
        }

        let mut lexeme = self.current_lexeme.clone();
        if negative {
            lexeme = format!("-{}", lexeme);
        }

        if has_dot {
            match lexeme.parse::<f64>() {
                Ok(f) => Ok(self.make_token(TokenKind::Float(f))),
                Err(_) => Err(LimnError::lexer(self.line, self.column, "Invalid float")),
            }
        } else {
            match lexeme.parse::<i64>() {
                Ok(n) => Ok(self.make_token(TokenKind::Integer(n))),
                Err(_) => Err(LimnError::lexer(self.line, self.column, "Invalid integer")),
            }
        }
    }

    fn scan_identifier(&mut self) -> Result<Token> {
        while let Some(&c) = self.peek() {
            if c.is_alphanumeric() || c == '_' || c == '-' {
                self.advance();
            } else {
                break;
            }
        }

        let kind = self.keyword_or_identifier(&self.current_lexeme);
        Ok(self.make_token(kind))
    }

    fn keyword_or_identifier(&self, s: &str) -> TokenKind {
        match s {
            // Structure
            "pro" => TokenKind::Pro,
            "var" => TokenKind::Var,
            "cns" => TokenKind::Cns,

            // Variable
            "whe" => TokenKind::Whe,

            // Relations
            "sa" => TokenKind::Sa,
            "ma" => TokenKind::Ma,
            "mi" => TokenKind::Mi,
            "eq" => TokenKind::Eq,

            // Arithmetic
            "joi" => TokenKind::Joi,
            "cut" => TokenKind::Cut,
            "exp" => TokenKind::Exp,
            "con" => TokenKind::Con,
            "pow" => TokenKind::Pow,
            "roo" => TokenKind::Roo,

            // Unary
            "nu" => TokenKind::Nu,

            // Types
            "one" => TokenKind::One,
            "flo" => TokenKind::Flo,
            "wor" => TokenKind::Wor,
            "tru" => TokenKind::Tru,
            "fal" => TokenKind::Fal,
            "gro" => TokenKind::Gro,
            "hol" => TokenKind::Hol,

            // Control
            "if" => TokenKind::If,
            "the" => TokenKind::The,
            "oth" => TokenKind::Oth,
            "cyc" => TokenKind::Cyc,
            "beg" => TokenKind::Beg,
            "end" => TokenKind::End,

            // Functions
            "cau" => TokenKind::Cau,
            "eff" => TokenKind::Eff,

            // Collections
            "amo" => TokenKind::Amo,
            "ins" => TokenKind::Ins,
            "par" => TokenKind::Par,
            "who" => TokenKind::Who,
            "fst" => TokenKind::Fst,
            "nxt" => TokenKind::Nxt,
            "fin" => TokenKind::Fin,

            // Quantifiers
            "al" => TokenKind::Al,
            "ex" => TokenKind::Ex,
            "on" => TokenKind::On,

            // Logic
            "pa" => TokenKind::Pa,
            "yo" => TokenKind::Yo,
            "an" => TokenKind::An,

            // Package/Import
            "use" => TokenKind::Use,
            "cid" => TokenKind::Cid,
            "nom" => TokenKind::Nom,
            "ver" => TokenKind::Ver,
            "get" => TokenKind::Get,
            "as" => TokenKind::As,
            "from" => TokenKind::From,

            // Identifier
            _ => TokenKind::Identifier(s.to_string()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_tokens() {
        let source = "whe x\nwhe y\nx joi y sa z";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();

        assert!(matches!(tokens[0].kind, TokenKind::Whe));
        assert!(matches!(tokens[1].kind, TokenKind::Identifier(_)));
    }

    #[test]
    fn test_numbers() {
        let source = "42 3.14";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();

        assert!(matches!(tokens[0].kind, TokenKind::Integer(42)));
        assert!(matches!(tokens[1].kind, TokenKind::Float(f) if (f - 3.14).abs() < 0.001));
    }

    #[test]
    fn test_string() {
        let source = r#""hello world""#;
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();

        assert!(matches!(&tokens[0].kind, TokenKind::String(s) if s == "hello world"));
    }

    #[test]
    fn test_key_separator() {
        let source = "whe x\n---\nx sa 5";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();

        let has_keysep = tokens.iter().any(|t| matches!(t.kind, TokenKind::KeySep));
        assert!(has_keysep);
    }
}
