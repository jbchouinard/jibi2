use std::fmt;

pub use crate::compiler::CompileError;
pub use crate::object::TypeError;
pub use crate::reader::parser::SyntaxError;
pub use crate::reader::tokenizer::LexError;

#[derive(Debug)]
pub enum Error {
    Lex(LexError),
    Syntax(SyntaxError),
    Compile(CompileError),
    Type(TypeError),
    Argument(ArgumentError),
    Runtime(RuntimeError),
}

pub type Result<T> = std::result::Result<T, Error>;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Lex(e) => write!(f, "{}", e),
            Self::Syntax(e) => write!(f, "{}", e),
            Self::Compile(e) => write!(f, "{}", e),
            Self::Type(e) => write!(f, "{}", e),
            Self::Argument(e) => write!(f, "{}", e),
            Self::Runtime(e) => write!(f, "{}", e),
        }
    }
}

impl From<LexError> for Error {
    fn from(le: LexError) -> Self {
        Self::Lex(le)
    }
}

impl From<SyntaxError> for Error {
    fn from(se: SyntaxError) -> Self {
        Self::Syntax(se)
    }
}

impl From<CompileError> for Error {
    fn from(e: CompileError) -> Self {
        Self::Compile(e)
    }
}

impl From<TypeError> for Error {
    fn from(te: TypeError) -> Self {
        Self::Type(te)
    }
}

impl From<ArgumentError> for Error {
    fn from(e: ArgumentError) -> Self {
        Self::Argument(e)
    }
}

impl From<RuntimeError> for Error {
    fn from(e: RuntimeError) -> Self {
        Self::Runtime(e)
    }
}

#[derive(Debug)]
pub struct ArgumentError {
    reason: String,
}

impl ArgumentError {
    pub fn new(reason: String) -> Self {
        Self { reason }
    }
}

impl fmt::Display for ArgumentError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ArgumentError: {}", self.reason)
    }
}

#[derive(Debug)]
pub struct RuntimeError {
    reason: String,
}

impl RuntimeError {
    pub fn new(reason: String) -> Self {
        Self { reason }
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "RuntimeError: {}", self.reason)
    }
}
