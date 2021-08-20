use std::convert::TryInto;
use std::fmt;

use crate::chunk::Chunk;
use crate::error::Result;
use crate::instruction::Instruction;
use crate::reader::{LexError, Token, TokenToIter, Tokenizer};
use crate::value::Value;

pub fn compile_tokens(tokens: Vec<Token>) -> Result<Chunk> {
    let mut chunk = Chunk::new();
    chunk.write_constant(Value::Int(tokens.len().try_into().unwrap()), 1);
    chunk.write(Instruction::op_return(), 1);
    Ok(chunk)
}

pub fn compile_source(filename: &str, source: &str) -> Result<Chunk> {
    let tokens = Tokenizer::new(filename.to_string(), source.to_string())
        .to_iter()
        .collect::<std::result::Result<Vec<Token>, LexError>>()?;

    compile_tokens(tokens)
}

#[derive(Debug)]
pub struct CompileError;

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "CompileError")
    }
}
