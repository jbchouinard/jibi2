use std::convert::Infallible;
use std::fmt;
use std::str::FromStr;

use crate::chunk::Chunk;
use crate::compiler::{compile_source, compile_tokens};
use crate::error::Result;
use crate::instruction::Op;
use crate::ops::math::*;
use crate::reader::Token;
use crate::value::Value;

const INITIAL_STACK_SIZE: usize = 256;
const MAX_STACK_SIZE: usize = 16 * 1024;

pub type Stack = Vec<Value>;

pub struct VM {
    chunk: Option<Chunk>,
    pos: usize,
    stack: Stack,
}

impl VM {
    pub fn new() -> Self {
        Self {
            chunk: None,
            pos: 0,
            stack: Vec::with_capacity(INITIAL_STACK_SIZE),
        }
    }
    pub fn interpret_source(&mut self, filename: &str, source: &str) -> Result<()> {
        self.chunk = Some(compile_source(filename, source)?);
        self.pos = 0;
        self.run()
    }
    pub fn interpret_tokens(&mut self, tokens: Vec<Token>) -> Result<()> {
        self.chunk = Some(compile_tokens(tokens)?);
        self.pos = 0;
        self.run()
    }
    pub fn run(&mut self) -> Result<()> {
        let chunk = self.chunk.as_ref().unwrap();

        #[cfg(debug_trace_execution)]
        let mut offset = 0;
        loop {
            #[cfg(debug_trace_execution)]
            {
                for v in self.stack.iter() {
                    println!("{}", v);
                }
                offset += chunk.disassemble_instruction(self.pos, offset);
            }
            if self.stack.len() > MAX_STACK_SIZE {
                panic!("stack too large");
            }
            let ins = &chunk.code[self.pos];
            match ins.op() {
                Op::Negate => op_negate(&mut self.stack)?,
                Op::Add => op_add(&mut self.stack)?,
                Op::Sub => op_sub(&mut self.stack)?,
                Op::Mul => op_mul(&mut self.stack)?,
                Op::Div => op_div(&mut self.stack)?,
                Op::Return => {
                    println!("{}", self.stack.pop().expect("stack empty"));
                    return Ok(());
                }
                Op::Constant => self
                    .stack
                    .push(chunk.constants[ins.get_constant_n()].clone()),
                Op::ConstantLong => self
                    .stack
                    .push(chunk.constants[ins.get_constant_n()].clone()),
            }
            self.pos += 1;
        }
    }
    pub fn reset_stack(&mut self) {
        self.stack = Vec::with_capacity(INITIAL_STACK_SIZE);
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

impl FromStr for RuntimeError {
    type Err = Infallible;
    fn from_str(reason: &str) -> std::result::Result<Self, Infallible> {
        Ok(Self::new(reason.to_string()))
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "RuntimeError: {}", self.reason)
    }
}
