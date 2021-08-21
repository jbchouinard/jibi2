use std::fmt;

use crate::chunk::Chunk;
use crate::error::{Error, Result};
use crate::instruction::Instruction;
use crate::reader::{PositionTag, Token, TokenProducer, TokenValue, Tokenizer};
use crate::value::Value;

pub fn compile_tokens(producer: Box<dyn TokenProducer>, chunk: &mut Chunk) -> Result<()> {
    let mut compiler = Compiler::new(producer, chunk);
    compiler.next()?;
    compiler.expressions()?;
    compiler.end();
    Ok(())
}

pub fn compile_source(filename: &str, source: &str, chunk: &mut Chunk) -> Result<()> {
    let tokens = Tokenizer::new(filename.to_string(), source.to_string());
    compile_tokens(Box::new(tokens), chunk)
}

pub struct Compiler<'a> {
    producer: Box<dyn TokenProducer>,
    peek: Token,
    current_chunk: &'a mut Chunk,
}

impl<'a> Compiler<'a> {
    pub fn new(producer: Box<dyn TokenProducer>, chunk: &'a mut Chunk) -> Self {
        Self {
            producer,
            peek: Token::new(TokenValue::None, PositionTag::new("", 0, 0)),
            current_chunk: chunk,
        }
    }
    fn next(&mut self) -> Result<Token> {
        let next = std::mem::replace(&mut self.peek, self.producer.next_token()?);
        println!("{}", next);
        Ok(next)
    }
    fn expect(&mut self, v: TokenValue) -> Result<Token> {
        if self.peek.value == v {
            self.next()
        } else {
            Err(self.error(&format!("expected token {}, got {}", v, self.peek.value)))
        }
    }
    pub fn expressions(&mut self) -> Result<()> {
        if self.peek.value == TokenValue::Eof {
            self.emit_constant(Value::Nil);
        }
        loop {
            if self.peek.value == TokenValue::Eof {
                break Ok(());
            } else {
                self.expression()?;
                if self.peek.value != TokenValue::Eof {
                    self.emit_instruction(Instruction::op_pop());
                }
            }
        }
    }
    pub fn expression(&mut self) -> Result<()> {
        match self.peek.value {
            TokenValue::Char('(') => self.list(),
            _ => self.atom(),
        }
    }
    fn atom(&mut self) -> Result<()> {
        let current = self.next()?;
        match current.value {
            TokenValue::Int(n) => self.emit_constant(Value::Int(n)),
            TokenValue::Float(x) => self.emit_constant(Value::Float(x)),
            TokenValue::Ident(s) => {
                self.emit_constant(Value::Symbol(s));
                self.emit_instruction(Instruction::op_getglobal());
            }
            _ => return Err(self.error("unexpected token")),
        };
        Ok(())
    }
    fn list(&mut self) -> Result<()> {
        self.expect(TokenValue::Char('('))?;
        match &self.peek.value {
            TokenValue::Keyword(_) => self.special_form()?,
            TokenValue::Char(')') => self.emit_constant(Value::Nil),
            _ => return Err(self.error("unexpected token")),
        };
        self.expect(TokenValue::Char(')'))?;
        Ok(())
    }
    fn ident(&mut self) -> Result<String> {
        let current = self.next()?;
        let ident = match current.value {
            TokenValue::Ident(s) => s,
            _ => return Err(self.error("expected ident")),
        };
        Ok(ident)
    }
    fn special_form(&mut self) -> Result<()> {
        let current = self.next()?;
        let keyword = match current.value {
            TokenValue::Keyword(k) => k,
            _ => panic!("expected keyword"),
        };
        match &keyword[..] {
            "def" => self.sform_def()?,
            "+" => self.sform_arith(keyword)?,
            "-" => self.sform_arith(keyword)?,
            "*" => self.sform_arith(keyword)?,
            "/" => self.sform_arith(keyword)?,
            _ => panic!("invalid keyword"),
        }
        Ok(())
    }
    fn sform_def(&mut self) -> Result<()> {
        let ident = self.ident()?;
        self.expression()?;
        self.emit_constant(Value::Symbol(ident));
        self.emit_instruction(Instruction::op_defglobal());
        Ok(())
    }
    fn sform_arith(&mut self, op: String) -> Result<()> {
        let mut nargs: u8 = 0;
        while self.peek.value != TokenValue::Char(')') {
            self.expression()?;
            nargs += 1;
        }
        self.emit_instruction(match &op[..] {
            "+" => Instruction::op_add(nargs),
            "-" => Instruction::op_sub(nargs),
            "*" => Instruction::op_mul(nargs),
            "/" => Instruction::op_div(nargs),
            _ => panic!(),
        });
        Ok(())
    }
    pub fn end(&mut self) {
        self.emit_instruction(Instruction::op_return());
        #[cfg(debug_trace_compile)]
        self.current_chunk.disassemble("code");
    }
    fn emit_instruction(&mut self, ins: Instruction) {
        self.current_chunk.write(ins, self.peek.pos.lineno)
    }
    fn emit_constant(&mut self, val: Value) {
        self.current_chunk.write_constant(val, self.peek.pos.lineno)
    }
    fn error(&self, reason: &str) -> Error {
        SyntaxError::new(self.peek.pos.clone(), reason).into()
    }
}

#[derive(Debug)]
pub struct SyntaxError {
    pub pos: PositionTag,
    pub reason: String,
}

impl SyntaxError {
    pub fn new(pt: PositionTag, reason: &str) -> Self {
        Self {
            pos: pt,
            reason: reason.to_string(),
        }
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        write!(f, "SyntaxError: {} {}", self.pos, self.reason)
    }
}

#[derive(Debug)]
pub struct CompileError;

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "CompileError")
    }
}
