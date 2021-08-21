use std::collections::HashMap;
use std::convert::Infallible;
use std::fmt;
use std::str::FromStr;

use crate::chunk::Chunk;
use crate::compiler::{compile_source, compile_tokens};
use crate::error::Result;
use crate::instruction::Op;
use crate::ops::math::*;
use crate::reader::TokenProducer;
use crate::value::Value;

const INI_STACK_SIZE: usize = 256;
const MAX_STACK_SIZE: usize = 16 * 1024;

pub type Stack = Vec<Value>;

pub struct VM {
    chunk: Option<Chunk>,
    pos: usize,
    stack: Stack,
    globals: HashMap<String, Value>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            chunk: None,
            pos: 0,
            stack: Vec::with_capacity(INI_STACK_SIZE),
            globals: HashMap::new(),
        }
    }
    pub fn interpret_source(&mut self, filename: &str, source: &str) -> Result<()> {
        let mut chunk = Chunk::new();
        compile_source(filename, source, &mut chunk)?;
        self.chunk = Some(chunk);
        self.pos = 0;
        self.run()
    }
    pub fn interpret_tokens(&mut self, producer: Box<dyn TokenProducer>) -> Result<()> {
        let mut chunk = Chunk::new();
        compile_tokens(producer, &mut chunk)?;
        self.chunk = Some(chunk);
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
                println!("GLOBALS: {:?}", self.globals);
                println!(
                    "STACK: {}",
                    self.stack
                        .iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<String>>()
                        .join(" ")
                );
                offset += chunk.disassemble_instruction(self.pos, offset);
            }
            if self.stack.len() > MAX_STACK_SIZE {
                panic!("stack too large");
            }
            let ins = &chunk.code[self.pos];
            match ins.op() {
                Op::Add => op_add(&mut self.stack, ins.get_operand(0))?,
                Op::Sub => op_sub(&mut self.stack, ins.get_operand(0))?,
                Op::Mul => op_mul(&mut self.stack, ins.get_operand(0))?,
                Op::Div => op_div(&mut self.stack, ins.get_operand(0))?,
                Op::Pop => {
                    self.stack.pop().unwrap();
                }
                Op::DefGlobal => {
                    let sym = self.stack.pop().unwrap().to_symbol()?;
                    let val = self.stack.pop().unwrap();
                    self.globals.insert(sym, val);
                    self.stack.push(Value::Nil);
                }
                Op::GetGlobal => {
                    let sym = self.stack.pop().unwrap().to_symbol()?;
                    match self.globals.get(&sym) {
                        Some(val) => self.stack.push(val.clone()),
                        None => return Err(RuntimeError::new("Undefined".to_string()).into()),
                    }
                }
                Op::Return => {
                    println!("{}", self.stack.pop().unwrap());
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
        self.stack = Vec::with_capacity(INI_STACK_SIZE);
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
