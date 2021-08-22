use std::fmt;
use std::rc::Rc;

use crate::chunk::Chunk;
use crate::error::{Error, Result};
use crate::instruction::*;
use crate::reader::{PositionTag, Token, TokenProducer, TokenValue, Tokenizer};
use crate::value::Value;

pub fn compile_tokens(producer: Box<dyn TokenProducer>, chunk: &mut Chunk) -> Result<()> {
    let mut compiler = Compiler::new(chunk);
    let mut parser = Parser::new(&mut compiler, producer);
    parser.advance()?;
    parser.parse()?;
    parser.end();
    Ok(())
}

pub fn compile_source(filename: &str, source: &str, chunk: &mut Chunk) -> Result<()> {
    let tokens = Tokenizer::new(filename.to_string(), source.to_string());
    compile_tokens(Box::new(tokens), chunk)
}

#[derive(Debug)]
pub struct Local {
    name: String,
    depth: i32,
}

impl Local {
    pub fn new(name: String, depth: i32) -> Self {
        Self { name, depth }
    }
}

pub enum Variable {
    Global,
    Local(usize),
}

pub struct Compiler<'a> {
    current_chunk: &'a mut Chunk,
    scope_depth: i32,
    locals: Vec<Local>,
}

impl<'a> Compiler<'a> {
    pub fn new(chunk: &'a mut Chunk) -> Self {
        Self {
            current_chunk: chunk,
            scope_depth: 0,
            locals: Vec::with_capacity(16),
        }
    }
    fn emit_instruction<I: Into<AnyInstruction>>(&mut self, ins: I, lineno: usize) {
        self.current_chunk.write_instruction(ins, lineno)
    }
    fn emit_constant(&mut self, val: Value, lineno: usize) {
        self.current_chunk.write_constant(val, lineno)
    }
    fn define_variable(&mut self, name: String, pos: &PositionTag) -> Result<()> {
        // Global variable: store in globals
        if self.scope_depth == 0 {
            self.emit_constant(Value::Symbol(Rc::new(name)), pos.lineno);
            self.emit_instruction(instruction_def_global(), pos.lineno);
            Ok(())
        }
        // Local variable: register local, leave value on stack
        // since locals are saved on the stack, definitions can only happen at the
        // top of a new local scope
        else {
            if self.locals.len() >= u16::MAX.into() {
                panic!("too many local variables")
            }
            for local in self.locals.iter().rev() {
                if local.depth != -1 && local.depth < self.scope_depth {
                    break;
                }
                if local.name == name {
                    return Err(SyntaxError::new(
                        pos.clone(),
                        format!("cannot re-define local variable {}", name),
                    )
                    .into());
                }
            }
            self.locals.push(Local::new(name, self.scope_depth));
            Ok(())
        }
    }
    fn resolve_variable(&mut self, name: &str) -> Variable {
        if !self.locals.is_empty() {
            for i in (0..self.locals.len()).rev() {
                if self.locals[i].name == name {
                    return Variable::Local(i);
                }
            }
        }
        Variable::Global
    }
    fn get_variable(&mut self, name: String, lineno: usize) -> Result<()> {
        let var = self.resolve_variable(&name);
        match var {
            Variable::Global => {
                self.emit_constant(Value::Symbol(Rc::new(name)), lineno);
                self.emit_instruction(instruction_get_global(), lineno);
            }
            Variable::Local(n) => {
                self.emit_instruction(instruction_get_local(n), lineno);
            }
        }
        Ok(())
    }
    fn set_variable(&mut self, name: String, lineno: usize) -> Result<()> {
        let var = self.resolve_variable(&name);
        match var {
            Variable::Global => {
                self.emit_constant(Value::Symbol(Rc::new(name)), lineno);
                self.emit_instruction(instruction_set_global(), lineno);
            }
            Variable::Local(n) => {
                self.emit_instruction(instruction_set_local(n), lineno);
            }
        }
        Ok(())
    }
    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }
    fn end_scope(&mut self, lineno: usize) {
        self.scope_depth -= 1;
        let mut popped: u8 = 0;
        while !self.locals.is_empty() && self.locals[self.locals.len() - 1].depth > self.scope_depth
        {
            self.locals.pop().unwrap();
            popped += 1;
        }
        if popped > 0 {
            self.emit_instruction(instruction_pop_n(popped), lineno)
        }
    }
    pub fn end(&mut self, lineno: usize) {
        self.emit_instruction(instruction_return(), lineno);
        #[cfg(debug_trace_compile)]
        self.current_chunk.disassemble("debug_trace_compile");
    }
}

pub struct Parser<'a> {
    producer: Box<dyn TokenProducer>,
    peek: Token,
    stashed_next: Option<Token>,
    compiler: &'a mut Compiler<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(compiler: &'a mut Compiler<'a>, producer: Box<dyn TokenProducer>) -> Self {
        Self {
            producer,
            peek: Token::new(TokenValue::None, PositionTag::new("", 1, 0)),
            stashed_next: None,
            compiler,
        }
    }
    fn advance(&mut self) -> Result<Token> {
        let stashed_next = std::mem::take(&mut self.stashed_next);
        let next = match stashed_next {
            Some(next) => std::mem::replace(&mut self.peek, next),
            None => std::mem::replace(&mut self.peek, self.producer.next_token()?),
        };
        if next.value == TokenValue::Eof {
            return Err(self.error_at(&next, "reached EOF".to_string()));
        }
        Ok(next)
    }
    fn rewind(&mut self, tok: Token) {
        if self.stashed_next.is_some() {
            panic!("cannot rewind more than once");
        }
        self.stashed_next = Some(std::mem::replace(&mut self.peek, tok));
    }
    fn expect(&mut self, v: TokenValue) -> Result<Token> {
        if self.peek.value == v {
            self.advance()
        } else {
            Err(self.error(format!("expected {}, got {}", v, self.peek.value)))
        }
    }
    pub fn parse(&mut self) -> Result<()> {
        // Empty program, return nil
        if self.peek.value == TokenValue::Eof {
            self.emit_constant(Value::Nil);
            self.emit_instruction(instruction_pop_r0());
        }
        while self.peek.value != TokenValue::Eof {
            self.top_level_expression()?;
            self.emit_instruction(instruction_pop_r0());
        }
        Ok(())
    }
    pub fn top_level_expression(&mut self) -> Result<()> {
        if !self.definition()? {
            self.expression()?;
        }
        Ok(())
    }
    pub fn definition(&mut self) -> Result<bool> {
        let next = self.advance()?;
        if next.value == TokenValue::Char('(')
            && self.peek.value == TokenValue::Keyword("def".to_string())
        {
            self.advance()?;
            self.sform_def()?;
            self.expect(TokenValue::Char(')'))?;
            // Definitions are expressions, they evaluate to nil
            self.emit_constant(Value::Nil);
            return Ok(true);
        }
        self.rewind(next);
        Ok(false)
    }
    pub fn expression(&mut self) -> Result<()> {
        match self.peek.value {
            TokenValue::Char('(') => self.list(),
            _ => self.atom(),
        }
    }
    fn atom(&mut self) -> Result<()> {
        let current = self.advance()?;
        match current.value {
            TokenValue::Int(n) => self.emit_constant(Value::Int(n)),
            TokenValue::Float(x) => self.emit_constant(Value::Float(x)),
            TokenValue::String(s) => self.emit_constant(Value::String(Rc::new(s))),
            TokenValue::Ident(s) => self.compiler.get_variable(s, self.lineno())?,
            TokenValue::Keyword(k) => self.atom_const(k)?,
            _ => return Err(self.error_at(&current, format!("unexpected {}", current.value))),
        };
        Ok(())
    }
    pub fn atom_const(&mut self, sym: String) -> Result<()> {
        match &sym[..] {
            "nil" => self.emit_constant(Value::Nil),
            "true" => self.emit_constant(Value::Bool(true)),
            "false" => self.emit_constant(Value::Bool(false)),
            _ => return Err(self.error(format!("ill-formed special form {}", sym))),
        }
        Ok(())
    }
    fn list(&mut self) -> Result<()> {
        self.expect(TokenValue::Char('('))?;
        match &self.peek.value {
            TokenValue::Keyword(_) => self.special_form()?,
            TokenValue::Char(')') => self.emit_constant(Value::Nil),
            _ => return Err(self.error(format!("unexpected {}", self.peek.value))),
        };
        self.expect(TokenValue::Char(')'))?;
        Ok(())
    }
    fn ident(&mut self) -> Result<String> {
        let current = self.advance()?;
        let ident = match current.value {
            TokenValue::Ident(s) => s,
            _ => {
                return Err(
                    self.error_at(&current, format!("expected ident, got {}", current.value))
                )
            }
        };
        Ok(ident)
    }
    fn special_form(&mut self) -> Result<()> {
        let current = self.advance()?;
        let keyword = match current.value {
            TokenValue::Keyword(k) => k,
            _ => panic!("expected keyword"),
        };
        match &keyword[..] {
            "def" => return Err(self.error("ill-placed def".to_string())),
            "begin" => self.sform_begin()?,
            "set!" => self.sform_set()?,
            "let" => self.sform_let()?,
            "equal?" => self.sform_equal()?,
            "+" => self.sform_arith(keyword)?,
            "-" => self.sform_arith(keyword)?,
            "*" => self.sform_arith(keyword)?,
            "/" => self.sform_arith(keyword)?,
            "=" => self.sform_numcmp(keyword)?,
            "!=" => self.sform_numcmp(keyword)?,
            "<" => self.sform_numcmp(keyword)?,
            "<=" => self.sform_numcmp(keyword)?,
            ">" => self.sform_numcmp(keyword)?,
            ">=" => self.sform_numcmp(keyword)?,
            _ => return Err(self.error(format!("invalid special form keyword {}", keyword))),
        }
        Ok(())
    }
    fn sform_def(&mut self) -> Result<()> {
        let ident = self.ident()?;
        self.expression()?;
        self.compiler.define_variable(ident, &self.peek.pos)?;
        Ok(())
    }
    fn sform_set(&mut self) -> Result<()> {
        let ident = self.ident()?;
        self.expression()?;
        self.compiler.set_variable(ident, self.peek.pos.lineno)?;
        Ok(())
    }
    fn sform_equal(&mut self) -> Result<()> {
        self.expression()?;
        self.expression()?;
        self.emit_instruction(instruction_equal());
        Ok(())
    }
    fn sform_let(&mut self) -> Result<()> {
        self.compiler.begin_scope();
        self.expect(TokenValue::Char('('))?;
        while self.peek.value != TokenValue::Char(')') {
            self.expect(TokenValue::Char('('))?;
            let ident = self.ident()?;
            self.expression()?;
            self.compiler.define_variable(ident, &self.peek.pos)?;
            self.expect(TokenValue::Char(')'))?;
        }
        self.expect(TokenValue::Char(')'))?;
        while self.peek.value != TokenValue::Char(')') {
            self.expression()?;
            self.emit_instruction(instruction_pop_r0());
        }
        self.compiler.end_scope(self.lineno());
        self.emit_instruction(instruction_push_r0());
        Ok(())
    }
    fn sform_begin(&mut self) -> Result<()> {
        // (begin): don't even bother, just return nil
        if self.peek.value == TokenValue::Char(')') {
            self.emit_constant(Value::Nil);
            return Ok(());
        }
        // Save value of last expression in register, push it back at the end
        self.compiler.begin_scope();
        while self.definition()? {
            self.emit_instruction(instruction_pop_r0());
        }
        while self.peek.value != TokenValue::Char(')') {
            self.expression()?;
            self.emit_instruction(instruction_pop_r0());
        }
        self.compiler.end_scope(self.peek.pos.lineno);
        self.emit_instruction(instruction_push_r0());
        Ok(())
    }
    fn sform_arith(&mut self, op: String) -> Result<()> {
        let mut nargs: usize = 0;
        while self.peek.value != TokenValue::Char(')') {
            self.expression()?;
            nargs += 1;
        }
        self.emit_instruction(match &op[..] {
            "+" => instruction_add(nargs),
            "-" => instruction_sub(nargs),
            "*" => instruction_mul(nargs),
            "/" => instruction_div(nargs),
            _ => panic!("invalid arith keyword {}", op),
        });
        Ok(())
    }
    fn sform_numcmp(&mut self, op: String) -> Result<()> {
        self.expression()?;
        self.expression()?;
        self.emit_instruction(match &op[..] {
            "=" => instruction_num_eq(),
            "!=" => instruction_num_neq(),
            "<" => instruction_num_lt(),
            "<=" => instruction_num_lte(),
            ">" => instruction_num_gt(),
            ">=" => instruction_num_gte(),
            _ => panic!("invalid numcmp keyword {}", op),
        });
        Ok(())
    }
    fn error_at(&self, tok: &Token, reason: String) -> Error {
        SyntaxError::new(tok.pos.clone(), reason).into()
    }
    fn error(&self, reason: String) -> Error {
        self.error_at(&self.peek, reason)
    }
    pub fn lineno(&self) -> usize {
        self.peek.pos.lineno
    }
    fn emit_instruction<I: Into<AnyInstruction>>(&mut self, ins: I) {
        self.compiler.emit_instruction(ins, self.lineno())
    }
    fn emit_constant(&mut self, val: Value) {
        self.compiler.emit_constant(val, self.lineno())
    }
    pub fn end(&mut self) {
        self.compiler.end(self.peek.pos.lineno)
    }
}

#[derive(Debug)]
pub struct SyntaxError {
    pub pos: PositionTag,
    pub reason: String,
}

impl SyntaxError {
    pub fn new(pos: PositionTag, reason: String) -> Self {
        Self { pos, reason }
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        write!(f, "SyntaxError: {} at {}", self.reason, self.pos)
    }
}
