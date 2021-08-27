use std::fmt;
use std::rc::Rc;

use hashbrown::HashMap;

use crate::compiler::{compile_source, compile_tokens, Variable};
use crate::error::{ArgumentError, Error, Result, RuntimeError};
use crate::instruction::*;
use crate::native::add_native_functions;
use crate::object::{Closure, ClosureRef, NativeFn, NativeFunction, Object, TypeError};
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

impl Default for VM {
    fn default() -> Self {
        let mut vm = VM::new();
        add_native_functions(&mut vm);
        vm
    }
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
    pub fn load(&mut self, closure: ClosureRef) -> Result<()> {
        self.frames.push(CallFrame::new(Rc::clone(&closure), 0, 0));
        self.stack.push(Object::Closure(Rc::clone(&closure)));
        Ok(())
    }
    pub fn load_source(&mut self, filename: &str, source: &str) -> Result<()> {
        let function = compile_source(filename, source)?.borrow().clone();
        self.load(Closure::new(function, None).into_ref())
    }
    pub fn load_tokens(&mut self, name: &str, producer: Box<dyn TokenProducer>) -> Result<()> {
        let function = compile_tokens(name, producer)?.borrow().clone();
        self.load(Closure::new(function, None).into_ref())
    }
    pub fn define_native(&mut self, name: &str, f: NativeFn) {
        let nf = NativeFunction::new(name.to_string(), f);
        let obj = Object::NativeFunction(Rc::new(nf));
        self.globals.insert(name.to_string(), obj);
    }
    fn peek(&self, n: usize) -> &Object {
        self.stack.peek_ref(n)
    }
    pub fn reset(&mut self) {
        self.register0 = None;
        self.stack = Stack::new();
        self.frames = CallStack::new();
    }
    pub fn run(&mut self) -> RunResult<Option<Object>> {
        let res = self._run();

        #[cfg(debug_trace_execution)]
        {
            self.trace_state();
            Self::printline();
        }

        match res {
            Ok(res) => Ok(res),
            Err(e) => {
                let trace = self.stacktrace();
                let offset = self.frames.peek_ref(0).ip;
                let function = &self.frames.peek_ref(0).closure.borrow().function;
                let err = VMError {
                    err: e,
                    offset,
                    function: function.name.to_string(),
                    lineno: function.code.get_line(offset),
                    trace,
                };
                Err(err)
            }
        }
    }
    fn stacktrace(&mut self) -> Vec<String> {
        let mut trace = vec![];
        for i in 0..self.frames.size {
            let frame = self.frames.get_ref(i);
            let offset = frame.ip - 1;
            let function = &frame.closure.borrow().function;
            trace.push(format!(
                "Line {}, in {}",
                function.code.get_line(offset),
                &function.name
            ));
        }
        trace
    }
    fn _run(&mut self) -> Result<Option<Object>> {
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
        macro_rules! read_op {
            () => {{
                let frame = mutframe!();
                let op = frame.closure.borrow().function.code.code[frame.ip];
                frame.ip += 1;
                op
            }};
        }
        macro_rules! read_short {
            () => {{
                let frame = mutframe!();
                let op = frame.closure.borrow().function.code.code[frame.ip];
                frame.ip += 1;
                op
            }};
        }
        macro_rules! read_long {
            () => {{
                let frame = mutframe!();
                let op = (frame.closure.borrow().function.code.code[frame.ip] as u16) << 8
                    | (frame.closure.borrow().function.code.code[frame.ip + 1] as u16);
                frame.ip += 2;
                op
            }};
        }
        macro_rules! read_constant_short {
            () => {{
                let n = read_short!() as usize;
                frame!().closure.borrow().function.code.constants[n].clone()
            }};
        }
        macro_rules! read_constant_long {
            () => {{
                let n = read_long!() as usize;
                frame!().closure.borrow().function.code.constants[n].clone()
            }};
        }

        loop {
            #[cfg(debug_trace_execution)]
            {
                self.trace_state();
                self.trace_instruction(frame!());
            }

            match read_op!() {
                OP_JUMP => {
                    let jmp = read_long!() as usize;
                    mutframe!().ip += jmp;
                }
                OP_JUMP_TRUE => {
                    let jmp = read_long!() as usize;
                    if self.peek(0).as_bool()? {
                        mutframe!().ip += jmp;
                    }
                }
                OP_JUMP_FALSE => {
                    let jmp = read_long!() as usize;
                    if !self.peek(0).as_bool()? {
                        mutframe!().ip += jmp;
                    }
                }
                OP_REPR => {
                    let val = self.stack.pop();
                    self.stack.push(Object::String(Rc::new(val.to_string())));
                }
                OP_PRINT => {
                    let val = self.stack.pop();
                    let s = match val {
                        Object::String(s) => s.to_string(),
                        _ => val.to_string(),
                    };
                    println!("{}", s);
                    self.stack.push(Object::Nil);
                }
                OP_ADD => op_add(&mut self.stack, read_short!() as usize)?,
                OP_ADD_LONG => op_add(&mut self.stack, read_long!() as usize)?,
                OP_SUB => op_sub(&mut self.stack, read_short!() as usize)?,
                OP_SUB_LONG => op_sub(&mut self.stack, read_long!() as usize)?,
                OP_MUL => op_mul(&mut self.stack, read_short!() as usize)?,
                OP_MUL_LONG => op_mul(&mut self.stack, read_short!() as usize)?,
                OP_DIV => op_div(&mut self.stack, read_short!() as usize)?,
                OP_DIV_LONG => op_div(&mut self.stack, read_long!() as usize)?,
                OP_NUM_EQ => op_num_eq(&mut self.stack)?,
                OP_NUM_NEQ => op_num_neq(&mut self.stack)?,
                OP_NUM_LT => op_num_lt(&mut self.stack)?,
                OP_NUM_LTE => op_num_lte(&mut self.stack)?,
                OP_NUM_GT => op_num_gt(&mut self.stack)?,
                OP_NUM_GTE => op_num_gte(&mut self.stack)?,
                OP_EQUAL => op_equal(&mut self.stack)?,
                OP_POP_R0 => {
                    self.register0 = Some(self.stack.pop());
                }
                OP_PUSH_R0 => self.stack.push(self.register0.take().unwrap()),
                OP_POP => {
                    self.stack.pop();
                }
                OP_POP_N => {
                    for _ in 0..read_short!() {
                        self.stack.pop();
                    }
                }
                OP_DEF_GLOBAL => {
                    let sym = self.stack.pop().as_symbol()?.to_string();
                    let val = self.stack.pop();
                    self.globals.insert(sym, val);
                }
                OP_GET_GLOBAL => {
                    let sym = self.stack.pop().as_symbol()?.to_string();
                    match self.globals.get(&sym) {
                        Some(val) => self.stack.push(val.clone()),
                        None => {
                            return Err(RuntimeError::new(format!("Undefined name {}", sym)).into())
                        }
                    }
                }
                OP_SET_GLOBAL => {
                    let sym = self.stack.pop().as_symbol()?.to_string();
                    let val = self.stack.pop();
                    match self.globals.get(&sym) {
                        Some(_) => {
                            self.globals.insert(sym, val);
                        }
                        None => {
                            return Err(RuntimeError::new(format!("Undefined name {}", sym)).into())
                        }
                    }
                }
                OP_GET_LOCAL => self.stack.push(
                    self.stack
                        .get_ref(frame!().fp + read_short!() as usize)
                        .clone(),
                ),
                OP_GET_LOCAL_LONG => self.stack.push(
                    self.stack
                        .get_ref(frame!().fp + read_long!() as usize)
                        .clone(),
                ),
                OP_SET_LOCAL => {
                    let val = self.stack.pop();
                    *self.stack.get_mut(frame!().fp + read_short!() as usize) = val;
                }
                OP_SET_LOCAL_LONG => {
                    let val = self.stack.pop();
                    *self.stack.get_mut(frame!().fp + read_short!() as usize) = val;
                }
                OP_GET_UPVALUE => {
                    let n = read_short!();
                    let closure = self.stack.get_ref(frame!().fp).as_closure()?;
                    self.stack.push(closure.borrow().get_upvalue(n as usize));
                }
                OP_SET_UPVALUE => {
                    let n = read_short!();
                    let val = self.stack.pop();
                    let closure = self.stack.get_ref(frame!().fp).as_closure()?;
                    closure.borrow_mut().set_upvalue(n as usize, val);
                }
                OP_CONSTANT => {
                    self.stack.push(read_constant_short!());
                }
                OP_CONSTANT_LONG => {
                    self.stack.push(read_constant_long!());
                }
                OP_CLOSURE => {
                    let func = read_constant_short!().as_function()?;
                    let enclosing = self.stack.get_ref(frame!().fp).as_closure()?;
                    let mut closure = Closure::new(func.borrow().clone(), Some(enclosing));
                    for uv in func.borrow().upvalues.iter() {
                        closure.captured.push(match uv {
                            Variable::Local(i) => Some(self.stack.get_ref(frame!().fp + i).clone()),
                            Variable::Upvalue(_) => None,
                            _ => panic!(),
                        })
                    }
                    self.stack.push(Object::Closure(closure.into_ref()));
                }
                OP_CLOSURE_LONG => {
                    let func = read_constant_long!().as_function()?;
                    let enclosing = self.stack.get_ref(frame!().fp).as_closure()?;
                    let mut closure = Closure::new(func.borrow().clone(), Some(enclosing));
                    for uv in func.borrow().upvalues.iter() {
                        closure.captured.push(match uv {
                            Variable::Local(i) => Some(self.stack.get_ref(frame!().fp + i).clone()),
                            Variable::Upvalue(_) => None,
                            _ => panic!(),
                        })
                    }
                    self.stack.push(Object::Closure(closure.into_ref()));
                }
                OP_HALT => {
                    if self.stack.size == 0 {
                        return Ok(None);
                    } else {
                        let res = self.stack.pop();
                        return Ok(Some(res));
                    }
                }
                OP_RETURN => {
                    let returning = self.frames.pop();
                    let result = self.stack.pop();
                    // Pop off arguments and function
                    for _ in 0..returning.closure.borrow().function.arity + 1 {
                        self.stack.pop();
                    }
                    if self.frames.size == 0 {
                        return Ok(Some(result));
                    } else {
                        self.stack.push(result);
                    }
                }
                OP_APPLY => {
                    let nargs = read_short!() as usize;
                    let obj = self.stack.peek_ref(nargs).clone();
                    match obj {
                        Object::Closure(clos) => {
                            let function = &clos.borrow().function;
                            if function.arity != nargs {
                                return Err(ArgumentError::new(format!(
                                    "expected {} arguments, got {}",
                                    function.arity, nargs
                                ))
                                .into());
                            }
                            let fp = self.stack.size - function.arity - 1;
                            self.frames.push(CallFrame::new(Rc::clone(&clos), 0, fp));
                        }
                        Object::NativeFunction(function) => {
                            function.call(nargs, &mut self.stack)?;
                            let result = self.stack.pop();
                            // Pop off function value
                            self.stack.pop();
                            self.stack.push(result);
                        }
                        _ => return Err(TypeError::new("expected callable".to_string()).into()),
                    }
                }
                opcode => panic!("invalid opcode {}", opcode),
            }
        }
    }
    #[cfg(debug_trace_execution)]
    fn printline() {
        println!("------------------------------------------------");
    }
    #[cfg(debug_trace_execution)]
    fn trace_state(&self) {
        Self::printline();
        print!("GLOBALS: ");
        for (k, v) in self.globals.iter() {
            print!("{}: {}, ", k, v)
        }
        println!();
        println!(
            "REGISTER0: {}",
            self.register0.as_ref().unwrap_or(&Object::Nil)
        );
        print!("STACK:");
        for i in 0..self.stack.size {
            print!("  {}", self.stack.get_ref(i));
        }
        println!("\nCALL STACK: {}", self.frames.size);
    }
    #[cfg(debug_trace_execution)]
    fn trace_instruction(&self, frame: &CallFrame) {
        Self::printline();
        frame
            .closure
            .borrow()
            .function
            .code
            .disassemble_instruction(frame.ip, frame.ip);
    }
}

pub struct CallFrame {
    closure: ClosureRef,
    ip: usize,
    fp: usize,
}

impl CallFrame {
    pub fn new(closure: ClosureRef, ip: usize, fp: usize) -> Self {
        Self { closure, ip, fp }
    }
}

type RunResult<T> = std::result::Result<T, VMError>;

#[derive(Debug)]
pub struct VMError {
    err: Error,
    offset: usize,
    lineno: usize,
    function: String,
    trace: Vec<String>,
}

impl VMError {
    pub fn print_trace(&self) {
        println!("Traceback:");
        for line in &self.trace {
            println!("  {}", line);
        }
    }
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
