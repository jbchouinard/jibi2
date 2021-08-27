use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::chunk::Chunk;
use crate::error::Result;
use crate::ops::math::Number;
use crate::vm::{CallFrame, CallStack, Stack};

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
    Closure(ClosureRef),
    NativeFunction(NativeFunctionRef),
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
            Self::Closure(clos) => write!(f, "{}", clos.function),
            Self::NativeFunction(func) => write!(f, "{}", func),
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

static FUNCTION_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn function_id() -> usize {
    FUNCTION_COUNTER.fetch_add(1, Ordering::SeqCst)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    id: usize,
    pub name: Rc<String>,
    pub arity: usize,
    pub code: Chunk,
}

pub type FunctionRef = Rc<RefCell<Function>>;

impl Function {
    pub fn new(name: Rc<String>) -> Self {
        Self {
            id: function_id(),
            name,
            arity: 0,
            code: Chunk::new(),
        }
    }
    pub fn into_ref(self) -> FunctionRef {
        Rc::new(RefCell::new(self))
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#[fn{}({}) {}]", self.id, self.arity, self.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub function: Function,
}

pub type ClosureRef = Rc<Closure>;

impl Closure {
    pub fn new(func: Function) -> Self {
        Self { function: func }
    }
    pub fn into_ref(self) -> ClosureRef {
        Rc::new(self)
    }
}

pub type NativeFn = Rc<dyn Fn(usize, &mut Stack) -> Result<()>>;

static NATIVE_FUNCTION_COUNTER: AtomicUsize = AtomicUsize::new(1);

fn native_function_id() -> usize {
    NATIVE_FUNCTION_COUNTER.fetch_add(1, Ordering::SeqCst)
}

#[derive(Clone)]
pub struct NativeFunction {
    id: usize,
    pub name: String,
    f: NativeFn,
}

impl NativeFunction {
    pub fn new(name: String, f: NativeFn) -> Self {
        Self {
            id: native_function_id(),
            name,
            f,
        }
    }
    pub fn call(&self, nargs: usize, stack: &mut Stack) -> Result<()> {
        (self.f)(nargs, stack)
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &NativeFunction) -> bool {
        self.id == other.id
    }
}

impl fmt::Display for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#[builtin{} {}]", self.id, self.name)
    }
}

impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "NativeFunction({}, {})", self.id, self.name)
    }
}

pub type NativeFunctionRef = Rc<NativeFunction>;

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

fn printsize<T>(name: &str) {
    println!("size of {}: {} bytes", name, std::mem::size_of::<T>());
}

pub fn debug_print_object_sizes() {
    printsize::<bool>("Bool");
    printsize::<IntType>("Int");
    printsize::<FloatType>("Float");
    printsize::<String>("String");
    printsize::<Rc<String>>("StringRef");
    printsize::<Function>("Function");
    printsize::<FunctionRef>("FunctionRef");
    printsize::<NativeFunction>("NativeFunction");
    printsize::<NativeFunctionRef>("NativeFunctionRef");
    printsize::<Object>("Value");
    printsize::<Stack>("Stack");
    printsize::<CallFrame>("CallFrame");
    printsize::<CallStack>("CallStack");
}
