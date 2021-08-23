use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::chunk::Chunk;
use crate::error::Result;
use crate::ops::math::Number;

pub type IntType = i64;
pub type FloatType = f64;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Nil,
    Bool(bool),
    Symbol(Rc<String>),
    String(Rc<String>),
    Int(IntType),
    Float(FloatType),
    Function(FunctionRef),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Float(x) => write!(f, "{}", x),
            Self::Int(n) => write!(f, "{}", n),
            Self::Bool(b) => write!(f, "{}", if *b { "true" } else { "false" }),
            Self::Nil => write!(f, "nil"),
            Self::Symbol(s) => write!(f, "{}", s),
            Self::String(s) => write!(f, "\"{}\"", s),
            Self::Function(func) => write!(f, "{}", func.borrow()),
        }
    }
}

impl Object {
    pub fn to_number(self) -> Result<Number> {
        Number::from_val(self)
    }
    pub fn as_bool(&self) -> Result<bool> {
        match self {
            Object::Bool(b) => Ok(*b),
            _ => Err(TypeError::new("boolean".to_string()).into()),
        }
    }
    pub fn as_symbol(&self) -> Result<&str> {
        match self {
            Object::Symbol(s) => Ok(s),
            _ => Err(TypeError::new("symbol".to_string()).into()),
        }
    }
    pub fn as_string(&self) -> Result<&str> {
        match self {
            Object::String(s) => Ok(s),
            _ => Err(TypeError::new("string".to_string()).into()),
        }
    }
    pub fn as_function(&self) -> Result<Rc<RefCell<Function>>> {
        match self {
            Object::Function(ref f) => Ok(Rc::clone(f)),
            _ => Err(TypeError::new("function".to_string()).into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Rc<String>,
    pub arity: usize,
    pub code: Chunk,
}

impl Function {
    pub fn new() -> Self {
        Self {
            name: Rc::new("unnamed".to_string()),
            arity: 0,
            code: Chunk::new(),
        }
    }
    pub fn to_ref(self) -> FunctionRef {
        Rc::new(RefCell::new(self))
    }
}

pub type FunctionRef = Rc<RefCell<Function>>;

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#[fn {}({})]", self.name, self.arity)
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
