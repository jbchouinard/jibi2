use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::compiler::{compile_source, compile_tokens, FunctionType};
use crate::error::{ArgumentError, Error, Result};
use crate::instruction::{AnyInstruction, Op};
use crate::object::{FunctionRef, Object};
use crate::ops::*;
use crate::reader::TokenProducer;
use crate::stack::ArrayStack;

// 256KiB / 16 bytes per Object
pub type Stack = ArrayStack<Object, { 256 * 1024 / 16 }>;
pub type CallStack = ArrayStack<CallFrame, 1024>;

pub struct VM {
    pub stack: Stack,
    pub frames: CallStack,
    pub register0: Option<Object>,
    pub globals: HashMap<String, Object>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Stack::new(),
            register0: None,
            globals: HashMap::new(),
            frames: CallStack::new(),
        }
    }
    pub fn load(&mut self, function: FunctionRef) -> Result<()> {
        self.stack.push(Object::Function(Rc::clone(&function)));
        self.frames.push(CallFrame::new(Rc::clone(&function), 0, 0));
        Ok(())
    }
    pub fn load_source(&mut self, filename: &str, source: &str) -> Result<()> {
        let function = compile_source(filename, source, FunctionType::Script)?;
        self.load(function)
    }
    pub fn load_tokens(&mut self, producer: Box<dyn TokenProducer>) -> Result<()> {
        let function = compile_tokens(producer, FunctionType::Script)?;
        self.load(function)
    }
    fn peek(&self, n: usize) -> &Object {
        self.stack.peek_ref(n)
    }
    pub fn reset(&mut self) {
        self.register0 = None;
        self.stack = Stack::new();
        self.frames = CallStack::new();
    }
    pub fn run(&mut self) -> RunResult<()> {
        match self._run() {
            Ok(()) => Ok(()),
            Err(e) => {
                let offset = self.frames.peek_ref(0).ip;
                let function = self.frames.peek_ref(0).function.borrow();
                let err = VMError {
                    err: e,
                    offset,
                    function: function.name.to_string(),
                    lineno: function.code.get_line(offset),
                };
                Err(err)
            }
        }
    }

    fn _run(&mut self) -> Result<()> {
        macro_rules! frame {
            () => {
                self.frames.peek_ref(0)
            };
        }
        macro_rules! mutframe {
            () => {
                self.frames.peek_mut(0)
            };
        }

        loop {
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
                    self.register0 = Some(self.stack.pop());
                }
                Op::PushR0 => self.stack.push(self.register0.take().unwrap()),
                Op::Pop => {
                    self.stack.pop();
                }
                Op::PopN => {
                    for _ in 0..ins.get_operand(0) {
                        self.stack.pop();
                    }
                }
                Op::DefGlobal => {
                    let sym = self.stack.pop().as_symbol()?.to_string();
                    let val = self.stack.pop();
                    self.globals.insert(sym, val);
                }
                Op::GetGlobal => {
                    let sym = self.stack.pop().as_symbol()?.to_string();
                    match self.globals.get(&sym) {
                        Some(val) => self.stack.push(val.clone()),
                        None => {
                            return Err(ArgumentError::new(format!("Undefined name {}", sym)).into())
                        }
                    }
                }
                Op::SetGlobal => {
                    let sym = self.stack.pop().as_symbol()?.to_string();
                    let val = self.stack.pop();
                    match self.globals.get(&sym) {
                        Some(_) => {
                            self.globals.insert(sym, val);
                        }
                        None => {
                            return Err(ArgumentError::new(format!("Undefined name {}", sym)).into())
                        }
                    }
                }
                Op::GetLocal | Op::GetLocalLong => self
                    .stack
                    .push(self.stack.get_ref(frame!().fp + ins.get_usize()).clone()),
                Op::SetLocal | Op::SetLocalLong => {
                    let val = self.stack.pop();
                    *self.stack.get_mut(frame!().fp + ins.get_usize()) = val;
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

type RunResult<T> = std::result::Result<T, VMError>;

#[derive(Debug)]
pub struct VMError {
    err: Error,
    offset: usize,
    lineno: usize,
    function: String,
}

impl fmt::Display for VMError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} (in {} at line {}, offset 0x{:04x})",
            self.err, self.function, self.lineno, self.offset
        )
    }
}
