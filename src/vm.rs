use std::collections::HashMap;
use std::convert::Infallible;
use std::fmt;
use std::rc::Rc;
use std::str::FromStr;

use crate::compiler::{compile_source, compile_tokens, FunctionType};
use crate::error::Result;
use crate::instruction::{AnyInstruction, Op};
use crate::object::{FunctionRef, Object};
use crate::ops::*;
use crate::reader::TokenProducer;

// TODO: use fixed sized arrays to avoid heap allocations
pub type Stack = Vec<Object>;
pub type CallStack = Vec<CallFrame>;

const INI_STACK_SIZE: usize = 32;
const MAX_STACK_SIZE: usize = 16 * 1024;

pub struct VM {
    pub register0: Option<Object>,
    pub stack: Stack,
    pub frames: CallStack,
    pub globals: HashMap<String, Object>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Vec::with_capacity(INI_STACK_SIZE),
            register0: None,
            globals: HashMap::new(),
            frames: vec![],
        }
    }
    pub fn interpret(&mut self, function: FunctionRef) -> Result<()> {
        self.stack.push(Object::Function(Rc::clone(&function)));
        self.frames.push(CallFrame::new(Rc::clone(&function), 0, 0));
        let res = self.run();
        if res.is_err() {
            self.clear_stack();
        }
        res
    }
    pub fn interpret_source(&mut self, filename: &str, source: &str) -> Result<()> {
        let function = compile_source(filename, source, FunctionType::Script)?;
        self.interpret(function)
    }
    pub fn interpret_tokens(&mut self, producer: Box<dyn TokenProducer>) -> Result<()> {
        let function = compile_tokens(producer, FunctionType::Script)?;
        self.interpret(function)
    }
    fn peek(&self, n: usize) -> &Object {
        &self.stack[self.stack.len() - 1 - n]
    }
    fn clear_stack(&mut self) {
        self.stack = Vec::with_capacity(INI_STACK_SIZE);
    }
    pub fn run(&mut self) -> Result<()> {
        macro_rules! frame {
            () => {
                self.frames.last().unwrap()
            };
        }
        macro_rules! mutframe {
            () => {
                self.frames.last_mut().unwrap()
            };
        }

        loop {
            if self.stack.len() > MAX_STACK_SIZE {
                panic!("stack too large");
            }

            #[cfg(debug_trace_execution)]
            self.trace_execution(chunk);

            let (ins, mut newpos) =
                AnyInstruction::read(&frame!().function.borrow().code.code, frame!().ip);
            match ins.op() {
                Op::Jump => {
                    let offset = ins.get_usize();
                    newpos += offset;
                }
                Op::JumpTrue => {
                    if self.peek(0).as_bool()? {
                        newpos += ins.get_usize();
                    }
                }
                Op::JumpFalse => {
                    if !self.peek(0).as_bool()? {
                        newpos += ins.get_usize();
                    }
                }
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
                    let sym = self.stack.pop().unwrap().as_symbol()?.to_string();
                    let val = self.stack.pop().unwrap();
                    self.globals.insert(sym, val);
                }
                Op::GetGlobal => {
                    let sym = self.stack.pop().unwrap().as_symbol()?.to_string();
                    match self.globals.get(&sym) {
                        Some(val) => self.stack.push(val.clone()),
                        None => {
                            return Err(RuntimeError::new(format!("Undefined name {}", sym)).into())
                        }
                    }
                }
                Op::SetGlobal => {
                    let sym = self.stack.pop().unwrap().as_symbol()?.to_string();
                    let val = self.stack.pop().unwrap();
                    match self.globals.get(&sym) {
                        Some(_) => {
                            self.globals.insert(sym, val);
                        }
                        None => {
                            return Err(RuntimeError::new(format!("Undefined name {}", sym)).into())
                        }
                    }
                }
                Op::GetLocal | Op::GetLocalLong => self
                    .stack
                    .push(self.stack[frame!().fp + ins.get_usize()].clone()),
                Op::SetLocal | Op::SetLocalLong => {
                    let val = self.stack.pop().unwrap();
                    self.stack[frame!().fp + ins.get_usize()] = val;
                    self.stack.push(Object::Nil)
                }
                Op::Constant | Op::ConstantLong => self
                    .stack
                    .push(frame!().function.borrow().code.constants[ins.get_usize()].clone()),
                Op::Return => {
                    return Ok(());
                }
            }
            mutframe!().ip = newpos;
        }
    }
    pub fn reset_stack(&mut self) {
        self.stack = Vec::with_capacity(INI_STACK_SIZE);
    }
    #[cfg(debug_trace_execution)]
    fn trace_execution(&self, frame: &CallFrame) {
        println!("------------------------------------------------");
        println!("IP: {}", frame.ip);
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

pub struct CallFrame {
    function: FunctionRef,
    ip: usize,
    fp: usize,
}

impl CallFrame {
    pub fn new(function: FunctionRef, ip: usize, fp: usize) -> Self {
        Self { function, ip, fp }
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
