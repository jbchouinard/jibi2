use std::collections::HashMap;
use std::convert::Infallible;
use std::fmt;
use std::str::FromStr;

use crate::chunk::Chunk;
use crate::compiler::{compile_source, compile_tokens};
use crate::error::Result;
use crate::instruction::{AnyInstruction, Op};
use crate::ops::*;
use crate::reader::TokenProducer;
use crate::value::Value;

pub type Stack = Vec<Value>;

const INI_STACK_SIZE: usize = 32;
const MAX_STACK_SIZE: usize = 16 * 1024;

pub struct VM {
    chunk: Option<Chunk>,
    ip: usize,
    stack: Stack,
    globals: HashMap<String, Value>,
    register0: Option<Value>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            chunk: None,
            ip: 0,
            stack: Vec::with_capacity(INI_STACK_SIZE),
            register0: None,
            globals: HashMap::new(),
        }
    }
    pub fn interpret(&mut self, code: Chunk) -> Result<()> {
        self.chunk = Some(code);
        self.ip = 0;
        let res = self.run();
        if res.is_err() {
            self.clear_stack();
        }
        res
    }
    pub fn interpret_source(&mut self, filename: &str, source: &str) -> Result<()> {
        let mut chunk = Chunk::new();
        compile_source(filename, source, &mut chunk)?;
        self.interpret(chunk)
    }
    pub fn interpret_tokens(&mut self, producer: Box<dyn TokenProducer>) -> Result<()> {
        let mut chunk = Chunk::new();
        compile_tokens(producer, &mut chunk)?;
        self.interpret(chunk)
    }
    fn clear_stack(&mut self) {
        self.stack = Vec::with_capacity(INI_STACK_SIZE);
    }
    pub fn run(&mut self) -> Result<()> {
        let chunk = self.chunk.as_ref().unwrap();
        loop {
            if self.stack.len() > MAX_STACK_SIZE {
                panic!("stack too large");
            }

            #[cfg(debug_trace_execution)]
            self.trace_execution(chunk);

            let (ins, newpos) = AnyInstruction::read(&chunk.code, self.ip);
            match ins.op() {
                Op::Add | Op::AddLong => op_add(&mut self.stack, ins.get_usize())?,
                Op::Sub | Op::SubLong => op_sub(&mut self.stack, ins.get_usize())?,
                Op::Mul | Op::MulLong => op_mul(&mut self.stack, ins.get_usize())?,
                Op::Div | Op::DivLong => op_div(&mut self.stack, ins.get_usize())?,
                Op::NumEq => op_num_eq(&mut self.stack)?,
                Op::NumNeq => op_num_neq(&mut self.stack)?,
                Op::NumLt => op_num_lt(&mut self.stack)?,
                Op::NumLte => op_num_lte(&mut self.stack)?,
                Op::NumGt => op_num_gt(&mut self.stack)?,
                Op::NumGte => op_num_gte(&mut self.stack)?,
                Op::Equal => op_equal(&mut self.stack)?,
                Op::PopR0 => {
                    self.register0 = Some(self.stack.pop().unwrap());
                }
                Op::PushR0 => self.stack.push(self.register0.take().unwrap()),
                Op::Pop => {
                    self.stack.pop().unwrap();
                }
                Op::PopN => {
                    for _ in 0..ins.get_operand(0) {
                        self.stack.pop().unwrap();
                    }
                }
                Op::DefGlobal => {
                    let sym = self.stack.pop().unwrap().to_symbol()?;
                    let val = self.stack.pop().unwrap();
                    self.globals.insert(sym, val);
                }
                Op::GetGlobal => {
                    let sym = self.stack.pop().unwrap().to_symbol()?;
                    match self.globals.get(&sym) {
                        Some(val) => self.stack.push(val.clone()),
                        None => {
                            return Err(RuntimeError::new(format!("Undefined name {}", sym)).into())
                        }
                    }
                }
                Op::SetGlobal => {
                    let sym = self.stack.pop().unwrap().to_symbol()?;
                    let val = self.stack.pop().unwrap();
                    match self.globals.get(&sym) {
                        Some(_) => {
                            self.globals.insert(sym, val);
                            self.stack.push(Value::Nil)
                        }
                        None => {
                            return Err(RuntimeError::new(format!("Undefined name {}", sym)).into())
                        }
                    }
                }
                Op::GetLocal | Op::GetLocalLong => {
                    self.stack.push(self.stack[ins.get_usize()].clone())
                }
                Op::SetLocal | Op::SetLocalLong => {
                    let val = self.stack.pop().unwrap();
                    self.stack[ins.get_usize()] = val;
                    self.stack.push(Value::Nil)
                }
                Op::Constant | Op::ConstantLong => {
                    self.stack.push(chunk.constants[ins.get_usize()].clone())
                }
                Op::Return => {
                    println!("{}", self.register0.as_ref().unwrap());
                    return Ok(());
                }
            }
            self.ip = newpos;
        }
    }
    pub fn reset_stack(&mut self) {
        self.stack = Vec::with_capacity(INI_STACK_SIZE);
    }
    #[cfg(debug_trace_execution)]
    fn trace_execution(&self, chunk: &Chunk) {
        println!("------------------------------------------------");
        println!("GLOBALS: {:?}", self.globals);
        print!("REGISTER0: {:?}", self.register0);
        println!(
            "\nSTACK: {}",
            self.stack
                .iter()
                .map(|v| v.to_string())
                .collect::<Vec<String>>()
                .join(" ")
        );
        chunk.disassemble_instruction(self.ip, self.ip);
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
