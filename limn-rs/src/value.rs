//! Value types for Limn runtime

use std::fmt;
use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use serde::{Serialize, Deserialize};

/// A Limn value - the runtime representation of data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Value {
    /// Integer value
    Integer(i64),

    /// Floating point value
    Float(f64),

    /// String value
    String(String),

    /// Boolean value
    Boolean(bool),

    /// Group (list/array)
    Group(Vec<Value>),

    /// Null/hole value
    Null,

    /// Unbound variable (during solving)
    Unbound,
}

impl Value {
    /// Check if value is numeric
    pub fn is_numeric(&self) -> bool {
        matches!(self, Value::Integer(_) | Value::Float(_))
    }

    /// Check if value is bound (not Unbound)
    pub fn is_bound(&self) -> bool {
        !matches!(self, Value::Unbound)
    }

    /// Check if value is truthy
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(b) => *b,
            Value::Integer(n) => *n != 0,
            Value::Float(f) => *f != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::Group(g) => !g.is_empty(),
            Value::Null => false,
            Value::Unbound => false,
        }
    }

    /// Convert to f64 if numeric
    pub fn as_f64(&self) -> Option<f64> {
        match self {
            Value::Integer(n) => Some(*n as f64),
            Value::Float(f) => Some(*f),
            _ => None,
        }
    }

    /// Convert to i64 if integer
    pub fn as_i64(&self) -> Option<i64> {
        match self {
            Value::Integer(n) => Some(*n),
            Value::Float(f) => Some(*f as i64),
            _ => None,
        }
    }

    /// Convert to string
    pub fn as_string(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Float(f) => {
                // Format nicely - remove trailing zeros
                let s = format!("{}", f);
                if s.contains('.') {
                    s.trim_end_matches('0').trim_end_matches('.').to_string()
                } else {
                    s
                }
            }
            Value::String(s) => s.clone(),
            Value::Boolean(b) => if *b { "tru".to_string() } else { "fal".to_string() },
            Value::Group(g) => {
                let items: Vec<String> = g.iter().map(|v| v.as_string()).collect();
                format!("gro | {} |", items.join(" | "))
            }
            Value::Null => "hol".to_string(),
            Value::Unbound => "?".to_string(),
        }
    }

    /// Parse from string
    pub fn parse(s: &str) -> Value {
        let s = s.trim();

        // Boolean
        if s == "tru" || s == "true" {
            return Value::Boolean(true);
        }
        if s == "fal" || s == "false" {
            return Value::Boolean(false);
        }

        // Null
        if s == "hol" || s == "null" {
            return Value::Null;
        }

        // Integer
        if let Ok(n) = s.parse::<i64>() {
            return Value::Integer(n);
        }

        // Float
        if let Ok(f) = s.parse::<f64>() {
            return Value::Float(f);
        }

        // String (remove quotes if present)
        let s = s.trim_matches('"');
        Value::String(s.to_string())
    }

    /// Arithmetic operations with automatic type promotion
    pub fn add(&self, other: &Value) -> Option<Value> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Some(Value::Integer(a + b)),
            (Value::Float(a), Value::Float(b)) => Some(Value::Float(a + b)),
            (Value::Integer(a), Value::Float(b)) => Some(Value::Float(*a as f64 + b)),
            (Value::Float(a), Value::Integer(b)) => Some(Value::Float(a + *b as f64)),
            (Value::String(a), Value::String(b)) => Some(Value::String(format!("{}{}", a, b))),
            (Value::String(a), other) => Some(Value::String(format!("{}{}", a, other.as_string()))),
            (other, Value::String(b)) => Some(Value::String(format!("{}{}", other.as_string(), b))),
            _ => None,
        }
    }

    pub fn sub(&self, other: &Value) -> Option<Value> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Some(Value::Integer(a - b)),
            (Value::Float(a), Value::Float(b)) => Some(Value::Float(a - b)),
            (Value::Integer(a), Value::Float(b)) => Some(Value::Float(*a as f64 - b)),
            (Value::Float(a), Value::Integer(b)) => Some(Value::Float(a - *b as f64)),
            _ => None,
        }
    }

    pub fn mul(&self, other: &Value) -> Option<Value> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Some(Value::Integer(a * b)),
            (Value::Float(a), Value::Float(b)) => Some(Value::Float(a * b)),
            (Value::Integer(a), Value::Float(b)) => Some(Value::Float(*a as f64 * b)),
            (Value::Float(a), Value::Integer(b)) => Some(Value::Float(a * *b as f64)),
            _ => None,
        }
    }

    pub fn div(&self, other: &Value) -> Option<Value> {
        let b = other.as_f64()?;
        if b == 0.0 {
            return None;
        }
        let a = self.as_f64()?;
        let result = a / b;

        // Return integer if both were integers and result is whole
        if matches!(self, Value::Integer(_)) && matches!(other, Value::Integer(_)) {
            if result.fract() == 0.0 {
                return Some(Value::Integer(result as i64));
            }
        }
        Some(Value::Float(result))
    }

    pub fn pow(&self, other: &Value) -> Option<Value> {
        let a = self.as_f64()?;
        let b = other.as_f64()?;
        let result = a.powf(b);

        // Return integer if appropriate
        if matches!(self, Value::Integer(_)) && matches!(other, Value::Integer(_)) {
            if result.fract() == 0.0 && result >= i64::MIN as f64 && result <= i64::MAX as f64 {
                return Some(Value::Integer(result as i64));
            }
        }
        Some(Value::Float(result))
    }

    pub fn root(&self, other: &Value) -> Option<Value> {
        let a = self.as_f64()?;
        let b = other.as_f64()?;
        if b == 0.0 {
            return None;
        }
        Some(Value::Float(a.powf(1.0 / b)))
    }

    /// Comparison operations
    pub fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => (a - b).abs() < f64::EPSILON,
            (Value::Integer(a), Value::Float(b)) => (*a as f64 - b).abs() < f64::EPSILON,
            (Value::Float(a), Value::Integer(b)) => (a - *b as f64).abs() < f64::EPSILON,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Null, Value::Null) => true,
            (Value::Group(a), Value::Group(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.eq(y))
            }
            _ => false,
        }
    }

    pub fn lt(&self, other: &Value) -> Option<bool> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Some(a < b),
            (Value::Float(a), Value::Float(b)) => Some(a < b),
            (Value::Integer(a), Value::Float(b)) => Some((*a as f64) < *b),
            (Value::Float(a), Value::Integer(b)) => Some(*a < (*b as f64)),
            (Value::String(a), Value::String(b)) => Some(a < b),
            _ => None,
        }
    }

    pub fn gt(&self, other: &Value) -> Option<bool> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Some(a > b),
            (Value::Float(a), Value::Float(b)) => Some(a > b),
            (Value::Integer(a), Value::Float(b)) => Some((*a as f64) > *b),
            (Value::Float(a), Value::Integer(b)) => Some(*a > (*b as f64)),
            (Value::String(a), Value::String(b)) => Some(a > b),
            _ => None,
        }
    }

    /// Negation
    pub fn negate(&self) -> Option<Value> {
        match self {
            Value::Integer(n) => Some(Value::Integer(-n)),
            Value::Float(f) => Some(Value::Float(-f)),
            Value::Boolean(b) => Some(Value::Boolean(!b)),
            _ => None,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_string())
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        Value::eq(self, other)
    }
}

impl Eq for Value {}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.eq(other) {
            return Some(Ordering::Equal);
        }
        if self.lt(other) == Some(true) {
            return Some(Ordering::Less);
        }
        if self.gt(other) == Some(true) {
            return Some(Ordering::Greater);
        }
        None
    }
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Integer(n) => {
                0u8.hash(state);
                n.hash(state);
            }
            Value::Float(f) => {
                1u8.hash(state);
                f.to_bits().hash(state);
            }
            Value::String(s) => {
                2u8.hash(state);
                s.hash(state);
            }
            Value::Boolean(b) => {
                3u8.hash(state);
                b.hash(state);
            }
            Value::Group(g) => {
                4u8.hash(state);
                for v in g {
                    v.hash(state);
                }
            }
            Value::Null => {
                5u8.hash(state);
            }
            Value::Unbound => {
                6u8.hash(state);
            }
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Value::Unbound
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arithmetic() {
        let a = Value::Integer(10);
        let b = Value::Integer(3);

        assert_eq!(a.add(&b), Some(Value::Integer(13)));
        assert_eq!(a.sub(&b), Some(Value::Integer(7)));
        assert_eq!(a.mul(&b), Some(Value::Integer(30)));
        assert_eq!(a.div(&b), Some(Value::Float(10.0 / 3.0)));
    }

    #[test]
    fn test_comparison() {
        let a = Value::Integer(5);
        let b = Value::Integer(10);

        assert!(a.lt(&b) == Some(true));
        assert!(b.gt(&a) == Some(true));
        assert!(a.eq(&Value::Integer(5)));
    }

    #[test]
    fn test_parse() {
        assert_eq!(Value::parse("42"), Value::Integer(42));
        assert_eq!(Value::parse("3.14"), Value::Float(3.14));
        assert_eq!(Value::parse("tru"), Value::Boolean(true));
        assert_eq!(Value::parse("hol"), Value::Null);
    }
}
