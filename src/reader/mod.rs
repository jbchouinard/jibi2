use std::fmt;

pub mod tokenizer;

pub use tokenizer::{LexError, Token, TokenToIter, TokenValidator, Tokenizer};

#[derive(Debug, PartialEq, Clone)]
pub struct PositionTag {
    pub filename: String,
    pub lineno: usize,
    pub col: usize,
}

impl PositionTag {
    pub fn new(filename: &str, lineno: usize, col: usize) -> Self {
        Self {
            filename: filename.to_string(),
            lineno,
            col,
        }
    }
}

impl fmt::Display for PositionTag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}:{}:{}", self.filename, self.lineno, self.col)
    }
}
