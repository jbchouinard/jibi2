use std::fmt;

pub use crate::compiler::CompileError;
pub use crate::compiler::SyntaxError;
pub use crate::reader::tokenizer::LexError;
pub use crate::value::TypeError;
pub use crate::vm::RuntimeError;

#[derive(Debug)]
pub enum Error {
    Lex(LexError),
    Syntax(SyntaxError),
    Compile(CompileError),
    Runtime(RuntimeError),
    Type(TypeError),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Compile(e) => write!(f, "{}", e),
            Self::Syntax(e) => write!(f, "{}", e),
            Self::Lex(e) => write!(f, "{}", e),
            Self::Runtime(e) => write!(f, "{}", e),
            Self::Type(e) => write!(f, "{}", e),
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
    fn from(ce: CompileError) -> Self {
        Self::Compile(ce)
    }
}

impl From<RuntimeError> for Error {
    fn from(re: RuntimeError) -> Self {
        Self::Runtime(re)
    }
}

impl From<TypeError> for Error {
    fn from(te: TypeError) -> Self {
        Self::Type(te)
    }
}

pub type Result<T> = std::result::Result<T, Error>;
