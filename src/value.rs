use std::fmt;

use crate::error::Result;
use crate::ops::math::Number;

pub type IntType = i64;
pub type FloatType = f64;

#[derive(Debug, Clone)]
pub enum Value {
    Float(FloatType),
    Int(IntType),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Float(x) => write!(f, "{}", x),
            Self::Int(n) => write!(f, "{}", n),
        }
    }
}

impl Value {
    pub fn to_number(self) -> Result<Number> {
        Number::from_val(self)
    }
}
