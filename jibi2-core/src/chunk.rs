use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::hash::Hash;

use crate::instruction::*;
use crate::object::{IntType, Object};

#[derive(Debug, Clone, PartialEq)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub lines: Vec<usize>,
    pub constants: Vec<Object>,
    pub constants_map: HashMap<ConstKey, usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: vec![],
            lines: vec![0],
            constants: vec![],
            constants_map: HashMap::new(),
        }
    }
    pub fn count(&self) -> usize {
        self.code.len()
    }
    pub fn get_line(&self, offset: usize) -> usize {
        let mut eol_offset = 0;
        for (n, line_len) in self.lines.iter().enumerate() {
            eol_offset += line_len;
            if eol_offset > offset {
                return n + 1;
            }
        }
        self.lines.len()
    }
    pub fn write(&mut self, byte: u8, line: usize) -> usize {
        self.code.push(byte);

        while self.lines.len() < line {
            self.lines.push(0);
        }
        self.lines[line - 1] += 1;

        self.code.len() - 1
    }
    pub fn write_at(&mut self, i: usize, byte: u8) {
        self.code[i] = byte;
    }
    pub fn write_op(&mut self, op: &Op, line: usize) -> usize {
        self.write(op.clone().as_byte(), line)
    }
    pub fn write_instruction<I: Into<AnyInstruction>>(&mut self, ins: I, line: usize) -> usize {
        let ins: AnyInstruction = ins.into();
        let start = self.write_op(ins.op(), line);
        for i in 0..ins.size() - 1 {
            self.write(ins.get_operand(i), line);
        }
        start
    }
    pub fn erase(&mut self, to_len: usize) {
        if to_len > self.code.len() {
            panic!("not that long");
        }
        self.code.resize(to_len, 0);
    }

    pub fn add_constant(&mut self, val: Object) -> usize {
        let key = ConstKey::from_val(&val);
        match key {
            Some(key) => match self.constants_map.entry(key) {
                Entry::Vacant(e) => {
                    self.constants.push(val);
                    let i = self.constants.len() - 1;
                    e.insert(i);
                    i
                }
                Entry::Occupied(e) => *e.get(),
            },
            None => {
                self.constants.push(val);
                self.constants.len() - 1
            }
        }
    }
    pub fn write_constant(&mut self, val: Object, line: usize) -> usize {
        let n = self.add_constant(val);
        self.write_instruction(instruction_constant(n), line)
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ConstKey {
    Nil,
    Bool(bool),
    Sym(String),
    Str(String),
    Int(IntType),
}

impl ConstKey {
    pub fn from_val(val: &Object) -> Option<Self> {
        match val {
            Object::Nil => Some(Self::Nil),
            Object::Bool(b) => Some(Self::Bool(*b)),
            Object::Symbol(ref s) => Some(Self::Str(s.to_string())),
            Object::String(ref s) => Some(Self::Sym(s.to_string())),
            Object::Int(n) => Some(Self::Int(*n)),
            _ => None,
        }
    }
}
