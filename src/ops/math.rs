#![allow(dead_code)]
use std::str::FromStr;

use crate::error::{Result, RuntimeError};
use crate::value::{FloatType, IntType, TypeError, Value};
use crate::vararg_op;

vararg_op!(op_add, args, {
    let mut acc = Number::Int(0);
    for n in args {
        acc = acc.add(&n.to_number()?)?;
    }
    acc.to_val()
});

vararg_op!(op_sub, args, {
    let mut acc = match args.pop() {
        Some(n) => n.to_number()?,
        None => return Err(RuntimeError::new("need at least 1 arg".to_string()).into()),
    };
    for n in args {
        acc = acc.sub(&n.to_number()?)?;
    }
    acc.to_val()
});

vararg_op!(op_mul, args, {
    let mut acc = Number::Int(1);
    for n in args {
        acc = acc.mul(&n.to_number()?)?;
    }
    acc.to_val()
});

vararg_op!(op_div, args, {
    let mut acc = match args.pop() {
        Some(n) => n.to_number()?,
        None => return Err(RuntimeError::new("need at least 1 arg".to_string()).into()),
    };
    for n in args {
        acc = acc.div(&n.to_number()?)?;
    }
    acc.to_val()
});

pub enum Number {
    Int(IntType),
    Float(FloatType),
}

impl Number {
    pub fn from_val(val: Value) -> Result<Self> {
        match val {
            Value::Int(n) => Ok(Self::Int(n)),
            Value::Float(x) => Ok(Self::Float(x)),
            _ => Err(TypeError::new("number".to_string()).into()),
        }
    }
    pub fn to_val(&self) -> Value {
        match self {
            Self::Int(n) => Value::Int(*n),
            Self::Float(x) => Value::Float(*x),
        }
    }
    fn as_float(&self) -> Result<FloatType> {
        match self {
            Self::Float(x) => Ok(*x),
            Self::Int(n) => {
                let x = *n as FloatType;
                if x as IntType != *n {
                    return Err(
                        RuntimeError::new(format!("cannot convert int {} to float", n)).into(),
                    );
                }
                Ok(x)
            }
        }
    }
    fn as_int(&self) -> Result<IntType> {
        match self {
            Self::Int(n) => Ok(*n),
            Self::Float(x) => Ok(*x as IntType),
        }
    }
    #[inline(always)]
    fn add(&self, other: &Self) -> Result<Self> {
        if let (Self::Int(n), Self::Int(m)) = (self, other) {
            let s = n
                .checked_add(*m)
                .ok_or_else(|| RuntimeError::from_str("int overflow").unwrap())?;
            Ok(Self::Int(s))
        } else {
            Ok(Self::Float(self.as_float()? + other.as_float()?))
        }
    }
    #[inline(always)]
    fn sub(&self, other: &Self) -> Result<Self> {
        if let (Self::Int(n), Self::Int(m)) = (self, other) {
            let s = n
                .checked_sub(*m)
                .ok_or_else(|| RuntimeError::from_str("int overflow").unwrap())?;
            Ok(Self::Int(s))
        } else {
            Ok(Self::Float(self.as_float()? - other.as_float()?))
        }
    }
    #[inline(always)]
    fn mul(&self, other: &Self) -> Result<Self> {
        if let (Self::Int(n), Self::Int(m)) = (self, other) {
            let s = n
                .checked_mul(*m)
                .ok_or_else(|| RuntimeError::from_str("int overflow").unwrap())?;
            Ok(Self::Int(s))
        } else {
            Ok(Self::Float(self.as_float()? * other.as_float()?))
        }
    }
    #[inline(always)]
    fn div(&self, other: &Self) -> Result<Self> {
        if let (Self::Int(n), Self::Int(m)) = (self, other) {
            let s = n
                .checked_div(*m)
                .ok_or_else(|| RuntimeError::from_str("int overflow").unwrap())?;
            Ok(Self::Int(s))
        } else {
            Ok(Self::Float(self.as_float()? / other.as_float()?))
        }
    }
    fn eq(&self, other: &Self) -> Result<bool> {
        if let (Self::Int(n), Self::Int(m)) = (self, other) {
            Ok(n == m)
        } else {
            Ok(self.as_float()? == other.as_float()?)
        }
    }
    fn lt(&self, other: &Self) -> Result<bool> {
        if let (Self::Int(n), Self::Int(m)) = (self, other) {
            Ok(n < m)
        } else {
            Ok(self.as_float()? < other.as_float()?)
        }
    }
    fn lte(&self, other: &Self) -> Result<bool> {
        if let (Self::Int(n), Self::Int(m)) = (self, other) {
            Ok(n <= m)
        } else {
            Ok(self.as_float()? <= other.as_float()?)
        }
    }
    fn gt(&self, other: &Self) -> Result<bool> {
        if let (Self::Int(n), Self::Int(m)) = (self, other) {
            Ok(n > m)
        } else {
            Ok(self.as_float()? > other.as_float()?)
        }
    }
    fn gte(&self, other: &Self) -> Result<bool> {
        if let (Self::Int(n), Self::Int(m)) = (self, other) {
            Ok(n >= m)
        } else {
            Ok(self.as_float()? >= other.as_float()?)
        }
    }
}
