use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use hashbrown::HashMap;

use crate::compiler::{compile_source, compile_tokens, Variable};
use crate::error::{ArgumentError, Error, Result, RuntimeError};
use crate::instruction::OP;
use crate::native::add_native_functions;
use crate::native::base::*;
use crate::native::math::*;
use crate::object::{Closure, ClosureRef, NativeFn, NativeFunction, Object, Pair, TypeError};
use crate::reader::TokenProducer;
use crate::stack::ArrayStack;

// 32 * 1024 * 16 bytes = 256KiB
pub type Stack = ArrayStack<Object, { 16 * 1024 }>;
// 4 * 1024 * 48 bytes = 192KiB
pub type CallStack = ArrayStack<CallFrame, { 4 * 1024 }>;

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
        macro_rules! read_operand {
            ($op:ident) => {{
                let frame = mutframe!();
                if ($op & OP::LONG) == OP::LONG {
                    let op = (frame.closure.borrow().function.code.code[frame.ip] as u16) << 8
                        | (frame.closure.borrow().function.code.code[frame.ip + 1] as u16);
                    frame.ip += 2;
                    op as usize
                } else {
                    let op = frame.closure.borrow().function.code.code[frame.ip];
                    frame.ip += 1;
                    op as usize
                }
            }};
        }
        macro_rules! read_constant {
            ($op:ident) => {{
                let n = read_operand!($op);
                frame!().closure.borrow().function.code.constants[n].clone()
            }};
        }
        macro_rules! call_native_fn {
            ($argc:expr, $native:expr) => {{
                let argc = $argc;
                let argv = self.stack.peek_slice(argc);
                let result = $native(argv, argc)?;
                self.stack.popfree_n(argc + 1);
                self.stack.push(result);
            }};
        }
        macro_rules! call_native_op {
            ($argc:expr, $native:expr) => {{
                let argc = $argc;
                let argv = self.stack.peek_slice(argc);
                let result = $native(argv, argc)?;
                self.stack.popfree_n(argc);
                self.stack.push(result);
            }};
        }

        loop {
            if self.frames.size == 0 {
                return Ok(Some(self.stack.pop()));
            }

            #[cfg(debug_trace_execution)]
            {
                self.trace_state();
                self.trace_instruction(frame!());
            }

            let op = read_op!();
            match op {
                OP::NOP => (),
                OP::HALT => return Ok(self.stack.maybe_pop()),
                OP::JUMP => {
                    let jmp = read_operand!(op);
                    mutframe!().ip += jmp;
                }
                OP::JUMP_TRUE => {
                    let jmp = read_operand!(op);
                    if self.peek(0).as_bool()? {
                        mutframe!().ip += jmp;
                    }
                }
                OP::JUMP_FALSE => {
                    let jmp = read_operand!(op);
                    if !self.peek(0).as_bool()? {
                        mutframe!().ip += jmp;
                    }
                }
                OP::CONSTANT | OP::CONSTANT_LONG => {
                    self.stack.push(read_constant!(op));
                }
                OP::POP => self.stack.popfree(),
                OP::POP_N => self.stack.popfree_n(read_operand!(op)),
                OP::POP_R0 => {
                    self.register0 = Some(self.stack.pop());
                }
                OP::PUSH_R0 => self.stack.push(self.register0.take().unwrap()),
                OP::DEF_GLOBAL => {
                    let sym = self.stack.pop().as_symbol()?.to_string();
                    let val = self.stack.pop();
                    self.globals.insert(sym, val);
                }
                OP::GET_GLOBAL => {
                    let sym = self.stack.pop().as_symbol()?.to_string();
                    match self.globals.get(&sym) {
                        Some(val) => self.stack.push(val.clone()),
                        None => {
                            return Err(RuntimeError::new(format!("Undefined name {}", sym)).into())
                        }
                    }
                }
                OP::SET_GLOBAL => {
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
                OP::GET_LOCAL | OP::GET_LOCAL_LONG => self
                    .stack
                    .push(self.stack.get_ref(frame!().fp + read_operand!(op)).clone()),

                OP::SET_LOCAL | OP::SET_LOCAL_LONG => {
                    let val = self.stack.pop();
                    *self.stack.get_mut(frame!().fp + read_operand!(op)) = val;
                }
                OP::GET_UPVALUE => {
                    let n = read_operand!(op);
                    let closure = self.stack.get_ref(frame!().fp).as_closure()?;
                    self.stack.push(closure.borrow().get_upvalue(n));
                }
                OP::SET_UPVALUE => {
                    let n = read_operand!(op);
                    let val = self.stack.pop();
                    let closure = self.stack.get_ref(frame!().fp).as_closure()?;
                    closure.borrow_mut().set_upvalue(n, val);
                }
                OP::CLOSURE | OP::CLOSURE_LONG => {
                    let func = read_constant!(op).as_function()?;
                    let enclosing = self.stack.get_ref(frame!().fp).as_closure()?;
                    let mut closure = Closure::new(func.borrow().clone(), Some(enclosing));
                    for uv in func.borrow().upvalues.iter() {
                        closure.captured.push(match uv {
                            Variable::Local(i) => {
                                Some(mutframe!().get_upvalue(&mut self.stack, *i))
                            }
                            Variable::Upvalue(_) => None,
                            _ => panic!(),
                        })
                    }
                    self.stack.push(Object::Closure(closure.into_ref()));
                }
                OP::CALL => {
                    let nargs = read_operand!(op);
                    let func = self.stack.peek_ref(nargs).clone();
                    match func {
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
                            call_native_fn!(nargs, function.f);
                        }
                        _ => return Err(TypeError::new("callable".to_string()).into()),
                    }
                }
                OP::RETURN => {
                    let returning = self.frames.pop();
                    let result = self.stack.pop();
                    // Pop off arguments and function
                    self.stack
                        .popfree_n(returning.closure.borrow().function.arity + 1);
                    self.stack.push(result);
                }
                OP::TAIL_CALL => {
                    // Save the function being called and its arguments
                    let nargs = read_operand!(op);
                    let mut saved = vec![];
                    for _ in 0..nargs + 1 {
                        saved.push(self.stack.pop());
                    }

                    let frame = mutframe!();

                    // Pop off function object and locals of current function call
                    self.stack
                        .popfree_n(frame.closure.borrow().function.arity + 1);

                    // Push back the next function and its arguments
                    while !saved.is_empty() {
                        self.stack.push(saved.pop().unwrap());
                    }

                    // Call next function
                    let func = self.stack.peek_ref(nargs).clone();
                    match func {
                        Object::Closure(clos) => {
                            // Modify the CallFrame at the top of the stack instead of
                            // creating a new one.
                            let function = &clos.borrow().function;
                            if function.arity != nargs {
                                return Err(ArgumentError::new(format!(
                                    "expected {} arguments, got {}",
                                    function.arity, nargs
                                ))
                                .into());
                            }
                            frame.fp = self.stack.size - function.arity - 1;
                            frame.closure = Rc::clone(&clos);
                            frame.ip = 0;
                        }
                        Object::NativeFunction(function) => {
                            // Native functions don't use call frames, so pop it
                            self.frames.popfree();
                            call_native_fn!(nargs, function.f);
                        }
                        _ => return Err(TypeError::new("callable".to_string()).into()),
                    }
                }
                OP::PRINT => call_native_op!(1, native_print),
                OP::REPR => call_native_op!(1, native_repr),
                OP::EQUAL => call_native_op!(2, native_equal),
                OP::ADD | OP::ADD_LONG => call_native_op!(read_operand!(op), native_add),
                OP::SUB | OP::SUB_LONG => call_native_op!(read_operand!(op), native_sub),
                OP::MUL | OP::MUL_LONG => call_native_op!(read_operand!(op), native_mul),
                OP::DIV | OP::DIV_LONG => call_native_op!(read_operand!(op), native_div),
                OP::NUM_EQ => call_native_op!(2, native_num_eq),
                OP::NUM_NEQ => call_native_op!(2, native_num_neq),
                OP::NUM_LT => call_native_op!(2, native_num_lt),
                OP::NUM_LTE => call_native_op!(2, native_num_lte),
                OP::NUM_GT => call_native_op!(2, native_num_gt),
                OP::NUM_GTE => call_native_op!(2, native_num_gte),
                OP::CONS => {
                    let x = self.stack.pop();
                    let y = self.stack.pop();
                    self.stack.push(Object::Pair(Pair::cons(y, x).into_ref()));
                }
                OP::CAR => {
                    let pair = self.stack.pop().as_pair()?;
                    self.stack.push(pair.car());
                }
                OP::CDR => {
                    let pair = self.stack.pop().as_pair()?;
                    self.stack.push(pair.cdr());
                }
                OP::LIST | OP::LIST_LONG => {
                    let n = read_operand!(op);
                    let mut head = Object::Nil;
                    for _ in 0..n {
                        head = Object::Pair(Pair::cons(self.stack.pop(), head).into_ref());
                    }
                    self.stack.push(head);
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
    closed_upvalues: Vec<(usize, Rc<RefCell<Object>>)>,
}

impl CallFrame {
    pub fn new(closure: ClosureRef, ip: usize, fp: usize) -> Self {
        Self {
            closure,
            ip,
            fp,
            closed_upvalues: vec![],
        }
    }
    fn get_upvalue(&mut self, stack: &mut Stack, i: usize) -> Rc<RefCell<Object>> {
        for (j, objref) in &self.closed_upvalues {
            if i == *j {
                return Rc::clone(objref);
            }
        }
        let objref = Rc::new(RefCell::new(stack.get_ref(self.fp + i).clone()));
        self.closed_upvalues.push((i, Rc::clone(&objref)));
        objref
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
