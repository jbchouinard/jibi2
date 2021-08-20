use std::fmt;

pub use crate::compiler::CompileError;
pub use crate::reader::tokenizer::LexError;
pub use crate::vm::RuntimeError;

#[derive(Debug)]
pub enum JibiError {
    Compile(CompileError),
    Lex(LexError),
    Runtime(RuntimeError),
}

impl fmt::Display for JibiError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Compile(e) => write!(f, "{}", e),
            Self::Lex(e) => write!(f, "{}", e),
            Self::Runtime(e) => write!(f, "{}", e),
        }
    }
}

impl From<CompileError> for JibiError {
    fn from(ce: CompileError) -> Self {
        Self::Compile(ce)
    }
}

impl From<LexError> for JibiError {
    fn from(le: LexError) -> Self {
        Self::Lex(le)
    }
}

impl From<RuntimeError> for JibiError {
    fn from(re: RuntimeError) -> Self {
        Self::Runtime(re)
    }
}

pub type Result<T> = std::result::Result<T, JibiError>;
