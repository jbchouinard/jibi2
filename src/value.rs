use std::fmt;

use crate::error::Result;
use crate::ops::math::Number;

pub type IntType = i64;
pub type FloatType = f64;

#[derive(Debug, Clone)]
pub enum Value {
    Symbol(String),
    Float(FloatType),
    Int(IntType),
    Bool(bool),
    Nil,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Float(x) => write!(f, "{}", x),
            Self::Int(n) => write!(f, "{}", n),
            Self::Bool(b) => write!(f, "{}", if *b { "true" } else { "false" }),
            Self::Nil => write!(f, "nil"),
            Self::Symbol(s) => write!(f, "{}", s),
        }
    }
}

impl Value {
    pub fn to_number(self) -> Result<Number> {
        Number::from_val(self)
    }
    pub fn to_symbol(self) -> Result<String> {
        match self {
            Value::Symbol(s) => Ok(s),
            _ => Err(TypeError::new("symbol".to_string()).into()),
        }
    }
}

#[derive(Debug)]
pub struct TypeError {
    etype: String,
}

impl TypeError {
    pub fn new(etype: String) -> Self {
        Self { etype }
    }
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "expected value of type {}", self.etype)
    }
}
