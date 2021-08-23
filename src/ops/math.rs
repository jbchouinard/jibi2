#![allow(dead_code)]

use crate::error::{ArgumentError, Result};
use crate::object::{FloatType, IntType, Object, TypeError};
use crate::{binary_op, vararg_op};

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
        None => return Err(ArgumentError::new("need at least 1 arg".to_string()).into()),
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
        None => return Err(ArgumentError::new("need at least 1 arg".to_string()).into()),
    };
    for n in args {
        acc = acc.div(&n.to_number()?)?;
    }
    acc.to_val()
});

binary_op!(op_num_eq, x, y, {
    Object::Bool(x.to_number()?.eq(&y.to_number()?)?)
});

binary_op!(op_num_neq, x, y, {
    Object::Bool(x.to_number()?.neq(&y.to_number()?)?)
});

binary_op!(op_num_lt, x, y, {
    Object::Bool(x.to_number()?.lt(&y.to_number()?)?)
});

binary_op!(op_num_lte, x, y, {
    Object::Bool(x.to_number()?.lte(&y.to_number()?)?)
});

binary_op!(op_num_gt, x, y, {
    Object::Bool(x.to_number()?.gte(&y.to_number()?)?)
});

binary_op!(op_num_gte, x, y, {
    Object::Bool(x.to_number()?.gte(&y.to_number()?)?)
});

pub enum Number {
    Int(IntType),
    Float(FloatType),
}

impl Number {
    pub fn from_val(val: Object) -> Result<Self> {
        match val {
            Object::Int(n) => Ok(Self::Int(n)),
            Object::Float(x) => Ok(Self::Float(x)),
            _ => Err(TypeError::new("number".to_string()).into()),
        }
    }
    pub fn to_val(&self) -> Object {
        match self {
            Self::Int(n) => Object::Int(*n),
            Self::Float(x) => Object::Float(*x),
        }
    }
    fn as_float(&self) -> Result<FloatType> {
        match self {
            Self::Float(x) => Ok(*x),
            Self::Int(n) => {
                let x = *n as FloatType;
                if x as IntType != *n {
                    return Err(TypeError::new(format!("cannot convert int {} to float", n)).into());
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
                .ok_or_else(|| TypeError::new("int overflow".to_string()))?;
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
                .ok_or_else(|| ArgumentError::new("int overflow".to_string()))?;
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
                .ok_or_else(|| ArgumentError::new("int overflow".to_string()))?;
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
                .ok_or_else(|| ArgumentError::new("int overflow".to_string()))?;
            Ok(Self::Int(s))
        } else {
            Ok(Self::Float(self.as_float()? / other.as_float()?))
        }
    }
    #[inline(always)]
    fn eq(&self, other: &Self) -> Result<bool> {
        if let (Self::Int(n), Self::Int(m)) = (self, other) {
            Ok(n == m)
        } else {
            Ok(self.as_float()? == other.as_float()?)
        }
    }
    #[inline(always)]
    fn neq(&self, other: &Self) -> Result<bool> {
        if let (Self::Int(n), Self::Int(m)) = (self, other) {
            Ok(n != m)
        } else {
            #[allow(clippy::float_cmp)]
            Ok(self.as_float()? != other.as_float()?)
        }
    }
    #[inline(always)]
    fn lt(&self, other: &Self) -> Result<bool> {
        if let (Self::Int(n), Self::Int(m)) = (self, other) {
            Ok(n < m)
        } else {
            Ok(self.as_float()? < other.as_float()?)
        }
    }
    #[inline(always)]
    fn lte(&self, other: &Self) -> Result<bool> {
        if let (Self::Int(n), Self::Int(m)) = (self, other) {
            Ok(n <= m)
        } else {
            Ok(self.as_float()? <= other.as_float()?)
        }
    }
    #[inline(always)]
    fn gt(&self, other: &Self) -> Result<bool> {
        if let (Self::Int(n), Self::Int(m)) = (self, other) {
            Ok(n > m)
        } else {
            Ok(self.as_float()? > other.as_float()?)
        }
    }
    #[inline(always)]
    fn gte(&self, other: &Self) -> Result<bool> {
        if let (Self::Int(n), Self::Int(m)) = (self, other) {
            Ok(n >= m)
        } else {
            Ok(self.as_float()? >= other.as_float()?)
        }
    }
}
