use crate::instruction::*;
use crate::value::Value;

pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
    pub lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: vec![],
            constants: vec![],
            lines: vec![0],
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
    pub fn write(&mut self, byte: u8, line: usize) {
        self.code.push(byte);

        while self.lines.len() < line {
            self.lines.push(0);
        }
        self.lines[line - 1] += 1;
    }
    pub fn write_op(&mut self, op: &Op, line: usize) {
        self.write(op.clone().as_byte(), line)
    }
    pub fn write_instruction<I: Into<AnyInstruction>>(&mut self, ins: I, line: usize) {
        let ins: AnyInstruction = ins.into();
        self.write_op(ins.op(), line);
        for i in 0..ins.size() - 1 {
            self.write(ins.get_operand(i), line);
        }
    }
    pub fn add_constant(&mut self, val: Value) -> usize {
        for (i, existing) in self.constants.iter().enumerate() {
            if &val == existing {
                return i;
            }
        }
        self.constants.push(val);
        self.constants.len() - 1
    }
    pub fn write_constant(&mut self, val: Value, line: usize) {
        let n = self.add_constant(val);
        self.write_instruction(instruction_constant(n), line);
    }
}
