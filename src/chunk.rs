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
            lines: vec![],
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
        self.lines.len() + 1
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
    pub fn write_instruction<const N: usize>(&mut self, ins: Instruction<N>, line: usize) {
        self.write_op(&ins.op, line);
        for n in 0..N {
            self.write(ins.operands[n as usize], line);
        }
    }
    pub fn add_constant(&mut self, val: Value) -> usize {
        self.constants.push(val);
        self.constants.len() - 1
    }
    pub fn write_constant(&mut self, val: Value, line: usize) {
        let n = self.add_constant(val);
        if n <= u8::MAX as usize {
            self.write_instruction(instruction_constant(n as u8), line);
        } else if n <= u16::MAX as usize {
            self.write_instruction(instruction_constant_long(n as u16), line);
        } else {
            panic!("too many constants");
        }
    }
}
