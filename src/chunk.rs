use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::hash::Hash;

use crate::instruction::*;
use crate::value::{IntType, Value};

const CONST_NIL: usize = 0;
const CONST_TRUE: usize = 1;
const CONST_FALSE: usize = 2;

pub struct Chunk {
    pub code: Vec<u8>,
    pub lines: Vec<usize>,
    pub constants: Vec<Value>,
    pub const_str_map: HashMap<String, usize>,
    pub const_sym_map: HashMap<String, usize>,
    pub const_int_map: HashMap<IntType, usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: vec![],
            lines: vec![0],
            constants: vec![Value::Nil, Value::Bool(true), Value::Bool(false)],
            const_str_map: HashMap::new(),
            const_sym_map: HashMap::new(),
            const_int_map: HashMap::new(),
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
                return n;
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
    fn reuse_constant<T: Eq + Hash>(
        constants: &mut Vec<Value>,
        map: &mut HashMap<T, usize>,
        key: T,
        val: Value,
    ) -> usize {
        match map.entry(key) {
            Entry::Vacant(e) => {
                constants.push(val);
                let i = constants.len() - 1;
                e.insert(i);
                i
            }
            Entry::Occupied(e) => *e.get(),
        }
    }
    pub fn add_constant(&mut self, val: Value) -> usize {
        match val {
            Value::Nil => CONST_NIL,
            Value::Bool(true) => CONST_TRUE,
            Value::Bool(false) => CONST_FALSE,
            Value::Int(n) => {
                Self::reuse_constant(&mut self.constants, &mut self.const_int_map, n, val)
            }
            Value::String(ref s) => Self::reuse_constant(
                &mut self.constants,
                &mut self.const_str_map,
                s.to_string(),
                val,
            ),
            Value::Symbol(ref s) => Self::reuse_constant(
                &mut self.constants,
                &mut self.const_sym_map,
                s.to_string(),
                val,
            ),
            _ => {
                self.constants.push(val);
                self.constants.len() - 1
            }
        }
    }
    pub fn write_constant(&mut self, val: Value, line: usize) -> usize {
        let n = self.add_constant(val);
        self.write_instruction(instruction_constant(n), line)
    }
}
