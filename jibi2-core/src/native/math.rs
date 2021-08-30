use std::rc::Rc;

use crate::error::{ArgumentError, Result, RuntimeError};
use crate::native::expect_n_args;
use crate::object::{FloatType, IntType, Object, TypeError};
use crate::vm::VM;
use crate::{binary_native_fn, native_fn};

native_fn!(native_add, argv, argc, {
    let mut acc = Number::Int(0);
    for arg in argv.iter().take(argc) {
        acc = acc.add(&arg.as_number()?)?;
    }
    Ok(acc.to_object())
});

native_fn!(native_sub, argv, argc, {
    if argc < 1 {
        return Err(ArgumentError::new("need at least 1 arg".to_string()).into());
    }
    let mut acc = argv[0].as_number()?;
    if argc == 1 {
        acc = Number::Int(0).sub(&acc)?;
    }
    for arg in argv.iter().take(argc).skip(1) {
        acc = acc.sub(&arg.as_number()?)?;
    }
    Ok(acc.to_object())
});

native_fn!(native_mul, argv, argc, {
    let mut acc = Number::Int(1);
    for arg in argv.iter().take(argc) {
        acc = acc.mul(&arg.as_number()?)?;
    }
    Ok(acc.to_object())
});

native_fn!(native_div, argv, argc, {
    if argc < 1 {
        return Err(ArgumentError::new("need at least 1 arg".to_string()).into());
    }
    let mut acc = argv[0].as_number()?;
    if argc == 1 {
        acc = Number::Int(1).div(&acc)?;
    }
    for arg in argv.iter().take(argc).skip(1) {
        acc = acc.div(&arg.as_number()?)?;
    }
    Ok(acc.to_object())
});

binary_native_fn!(native_num_eq, x, y, {
    Object::Bool(x.as_number()?.eq(&y.as_number()?)?)
});

binary_native_fn!(native_num_neq, x, y, {
    Object::Bool(x.as_number()?.neq(&y.as_number()?)?)
});

binary_native_fn!(native_num_lt, x, y, {
    Object::Bool(x.as_number()?.lt(&y.as_number()?)?)
});

binary_native_fn!(native_num_lte, x, y, {
    Object::Bool(x.as_number()?.lte(&y.as_number()?)?)
});

binary_native_fn!(native_num_gt, x, y, {
    Object::Bool(x.as_number()?.gt(&y.as_number()?)?)
});

binary_native_fn!(native_num_gte, x, y, {
    Object::Bool(x.as_number()?.gte(&y.as_number()?)?)
});

pub fn add_native_functions(vm: &mut VM) {
    vm.define_native("+", Rc::new(native_add));
    vm.define_native("-", Rc::new(native_sub));
    vm.define_native("*", Rc::new(native_mul));
    vm.define_native("/", Rc::new(native_div));
    vm.define_native("=", Rc::new(native_num_eq));
    vm.define_native("!=", Rc::new(native_num_neq));
    vm.define_native("<", Rc::new(native_num_lt));
    vm.define_native("<=", Rc::new(native_num_lte));
    vm.define_native(">", Rc::new(native_num_gt));
    vm.define_native(">=", Rc::new(native_num_gte));
}

pub enum Number {
    Int(IntType),
    Float(FloatType),
}

fn int_to_float(n: &IntType) -> Result<FloatType> {
    let x = *n as FloatType;
    if x as IntType != *n {
        return Err(RuntimeError::new(format!("cannot convert int {} to float", n)).into());
    }
    Ok(x)
}

impl Number {
    pub fn from_object(val: &Object) -> Result<Self> {
        match val {
            Object::Int(n) => Ok(Self::Int(*n)),
            Object::Float(x) => Ok(Self::Float(*x)),
            _ => Err(TypeError::new("number".to_string()).into()),
        }
    }
    pub fn to_object(&self) -> Object {
        match self {
            Self::Int(n) => Object::Int(*n),
            Self::Float(x) => Object::Float(*x),
        }
    }
    fn as_float(&self) -> Result<FloatType> {
        match self {
            Self::Float(x) => Ok(*x),
            Self::Int(n) => int_to_float(n),
        }
    }
    #[inline(always)]
    fn add(&self, other: &Self) -> Result<Self> {
        if let (Self::Int(n), Self::Int(m)) = (self, other) {
            let s = n
                .checked_add(*m)
                .ok_or_else(|| RuntimeError::new("int overflow".to_string()))?;
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
                .ok_or_else(|| RuntimeError::new("int overflow".to_string()))?;
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
                .ok_or_else(|| RuntimeError::new("int overflow".to_string()))?;
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
                .ok_or_else(|| RuntimeError::new("int overflow".to_string()))?;
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
