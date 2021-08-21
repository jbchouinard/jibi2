use std::fmt;
use std::rc::Rc;

use crate::chunk::Chunk;
use crate::error::{Error, Result};
use crate::instruction::*;
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
            peek: Token::new(TokenValue::None, PositionTag::new("", 1, 0)),
            current_chunk: chunk,
        }
    }
    fn next(&mut self) -> Result<Token> {
        let next = std::mem::replace(&mut self.peek, self.producer.next_token()?);
        Ok(next)
    }
    fn expect(&mut self, v: TokenValue) -> Result<Token> {
        if self.peek.value == v {
            self.next()
        } else {
            Err(self.error(format!("expected {}, got {}", v, self.peek.value)))
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
                    self.emit_instruction(instruction_pop());
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
            TokenValue::String(s) => self.emit_constant(Value::String(Rc::new(s))),
            TokenValue::Ident(s) => {
                self.emit_constant(Value::Symbol(Rc::new(s)));
                self.emit_instruction(instruction_get_global());
            }
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
            _ => return Err(self.error(format!("invalid special form {}", sym))),
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
        let current = self.next()?;
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
            "=" => self.sform_numcmp(keyword)?,
            "!=" => self.sform_numcmp(keyword)?,
            "<" => self.sform_numcmp(keyword)?,
            "<=" => self.sform_numcmp(keyword)?,
            ">" => self.sform_numcmp(keyword)?,
            ">=" => self.sform_numcmp(keyword)?,
            _ => panic!("invalid keyword"),
        }
        Ok(())
    }
    fn sform_def(&mut self) -> Result<()> {
        let ident = self.ident()?;
        self.expression()?;
        self.emit_constant(Value::Symbol(Rc::new(ident)));
        self.emit_instruction(instruction_def_global());
        Ok(())
    }
    fn sform_arith(&mut self, op: String) -> Result<()> {
        let mut nargs: u8 = 0;
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
    pub fn end(&mut self) {
        self.emit_instruction(instruction_return());
        #[cfg(debug_trace_compile)]
        self.current_chunk.disassemble("code");
    }
    fn emit_instruction<const N: usize>(&mut self, ins: Instruction<N>) {
        self.current_chunk
            .write_instruction(ins, self.peek.pos.lineno)
    }
    fn emit_constant(&mut self, val: Value) {
        self.current_chunk.write_constant(val, self.peek.pos.lineno)
    }
    fn error_at(&self, tok: &Token, reason: String) -> Error {
        SyntaxError::new(tok.pos.clone(), reason).into()
    }
    fn error(&self, reason: String) -> Error {
        self.error_at(&self.peek, reason)
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
