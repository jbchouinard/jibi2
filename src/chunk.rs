use crate::instruction::Instruction;
use crate::value::Value;

pub struct Chunk {
    pub code: Vec<Instruction>,
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

    pub fn get_line(&self, code_i: usize) -> usize {
        for (n, idx) in self.lines.iter().enumerate() {
            if *idx > code_i {
                return n;
            }
        }
        self.lines.len()
    }

    pub fn write(&mut self, ins: Instruction, line: usize) {
        self.code.push(ins);
        if line > self.lines.len() {
            for _ in self.lines.len()..line {
                self.lines.push(self.code.len() - 1)
            }
        }
    }

    pub fn add_constant(&mut self, val: Value) -> usize {
        self.constants.push(val);
        self.constants.len() - 1
    }

    pub fn write_constant(&mut self, val: Value, line: usize) {
        let n = self.add_constant(val);
        self.write(Instruction::op_constant(n), line);
    }
}
