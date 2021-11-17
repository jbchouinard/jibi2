use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::chunk::Chunk;
use crate::compiler::Variable;
use crate::error::Result;
use crate::native::math::Number;
use crate::vm::{CallFrame, CallStack, Stack, VM};

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
    Pair(PairRef),
    Function(FunctionRef),
    Closure(ClosureRef),
    NativeFunction(NativeFunctionRef),
}

fn display_pair(pair: &Pair) -> String {
    match pair.iter() {
        Ok(iterator) => {
            format!(
                "({})",
                iterator
                    .map(|v| v.to_string())
                    .collect::<Vec<String>>()
                    .join(" ")
            )
        }
        // Not a list
        Err(_) => format!("({} . {})", &pair.car(), &pair.cdr()),
    }
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
            Self::Pair(pair) => write!(f, "{}", display_pair(pair)),
            Self::Function(func) => write!(f, "{}", func.borrow()),
            Self::Closure(clos) => write!(f, "{}", clos.function),
            Self::NativeFunction(func) => write!(f, "{}", func),
        }
    }
}

impl Object {
    pub fn as_number(&self) -> Result<Number> {
        Number::from_object(self)
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
    pub fn as_function(&self) -> Result<FunctionRef> {
        match self {
            Object::Function(ref f) => Ok(Rc::clone(f)),
            _ => Err(TypeError::new("function".to_string()).into()),
        }
    }
    pub fn as_closure(&self) -> Result<ClosureRef> {
        match self {
            Object::Closure(ref clos) => Ok(Rc::clone(clos)),
            _ => Err(TypeError::new("closure".to_string()).into()),
        }
    }
    pub fn as_pair(&self) -> Result<PairRef> {
        match self {
            Object::Pair(ref pair) => Ok(Rc::clone(pair)),
            _ => Err(TypeError::new("pair".to_string()).into()),
        }
    }
    pub fn make_list(mut v: Vec<Object>) -> Object {
        let mut cur = Object::Nil;
        v.reverse();
        for val in v {
            cur = Object::Pair(Pair::cons(val, cur).into_ref());
        }
        cur
    }
    pub fn iter_list(&self) -> Result<ListIterator> {
        match self {
            Object::Nil => Ok(ListIterator { head: None }),
            Object::Pair(p) => p.iter(),
            _ => Err(TypeError::new("can only iter lists".to_string()).into()),
        }
    }
}

// The cdr of the Pair is an Option just to implement iterative dropping.
// It is always Some(Object) until the Pair is dropped.
#[derive(Debug, PartialEq, Clone)]
pub struct Pair(Object, Option<Object>);

pub type PairRef = Rc<Pair>;

// The default drop is called recursively when dropping a list. This causes
// stack overflow when dropping a large list (~180k elements in my tests).
// This implementation iteratively drops the list instead.
// This requires making the cdr of the pair an Option - since drop is a &mut self
// function, the cdr cannot be moved out of self - but it can be take()'d.
impl Drop for Pair {
    fn drop(&mut self) {
        let mut next = self.1.take();
        while let Some(Object::Pair(pref)) = next {
            if let Ok(mut pair) = Rc::try_unwrap(pref) {
                next = pair.1.take();
            } else {
                break;
            }
        }
    }
}

impl Pair {
    pub fn into_ref(self) -> PairRef {
        Rc::new(self)
    }
    pub fn cons(x: Object, y: Object) -> Self {
        Self(x, Some(y))
    }
    pub fn car(&self) -> Object {
        self.0.clone()
    }
    pub fn cdr(&self) -> Object {
        self.1.as_ref().unwrap().clone()
    }
    pub fn is_list(&self) -> bool {
        let mut next = self.1.as_ref().unwrap();
        loop {
            match next {
                Object::Nil => return true,
                Object::Pair(pair) => next = pair.1.as_ref().unwrap(),
                _ => return false,
            }
        }
    }
    pub fn iter(&self) -> Result<ListIterator> {
        if !self.is_list() {
            return Err(TypeError::new("can only iter lists".to_string()).into());
        }
        Ok(ListIterator { head: Some(self) })
    }
}

pub struct ListIterator<'a> {
    head: Option<&'a Pair>,
}

impl Iterator for ListIterator<'_> {
    type Item = Object;
    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        match self.head {
            None => None,
            Some(Pair(x, Some(y))) => {
                match y {
                    Object::Pair(c) => self.head = Some(c),
                    _ => self.head = None,
                };
                Some(x.clone())
            }
            Some(Pair(_, None)) => panic!(),
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
    pub upvalues: Vec<Variable>,
}

pub type FunctionRef = Rc<RefCell<Function>>;

impl Function {
    pub fn new(name: Rc<String>) -> Self {
        Self {
            id: function_id(),
            name,
            arity: 0,
            code: Chunk::new(),
            upvalues: vec![],
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
    pub enclosing: Option<ClosureRef>,
    pub captured: RefCell<Vec<Option<Rc<RefCell<Object>>>>>,
}

pub type ClosureRef = Rc<Closure>;

impl Closure {
    pub fn new(func: Function, enclosing: Option<ClosureRef>) -> Self {
        Self {
            function: func,
            captured: RefCell::new(vec![]),
            enclosing,
        }
    }
    pub fn into_ref(self) -> ClosureRef {
        Rc::new(self)
    }
    pub fn get_upvalue(&self, n: usize) -> Object {
        match self.function.upvalues[n] {
            Variable::Local(i) => self.captured.borrow()[i - 1]
                .as_ref()
                .unwrap()
                .borrow()
                .clone(),
            Variable::Upvalue(i) => self.enclosing.as_ref().unwrap().get_upvalue(i),
            _ => panic!(),
        }
    }
    pub fn set_upvalue(&self, n: usize, val: Object) {
        match self.function.upvalues[n] {
            Variable::Local(i) => {
                self.captured.borrow()[i - 1].as_ref().unwrap().replace(val);
            }
            Variable::Upvalue(i) => self.enclosing.as_ref().unwrap().set_upvalue(i, val),
            _ => panic!(),
        }
    }
}

pub type NativeFn = Rc<dyn Fn(&[Object], usize) -> Result<Object>>;

static NATIVE_FUNCTION_COUNTER: AtomicUsize = AtomicUsize::new(1);

fn native_function_id() -> usize {
    NATIVE_FUNCTION_COUNTER.fetch_add(1, Ordering::SeqCst)
}

#[derive(Clone)]
pub struct NativeFunction {
    id: usize,
    pub name: String,
    pub f: NativeFn,
}

impl NativeFunction {
    pub fn new(name: String, f: NativeFn) -> Self {
        Self {
            id: native_function_id(),
            name,
            f,
        }
    }
    pub fn call(&self, argv: &[Object], argc: usize) -> Result<Object> {
        (self.f)(argv, argc)
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

fn normbytes(n: usize) -> String {
    if n >= 1024 {
        format!("{} KiB", n / 1024)
    } else {
        format!("{} B", n)
    }
}

fn printsize<T>(name: &str) {
    println!("size of {}: {}", name, normbytes(std::mem::size_of::<T>()));
}

pub fn debug_print_object_sizes() {
    printsize::<Object>("Object");
    printsize::<Stack>("Stack");
    printsize::<CallFrame>("CallFrame");
    printsize::<CallStack>("CallStack");
    printsize::<VM>("VM");
}
