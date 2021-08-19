use crate::chunk::{Chunk, OpCode};

impl Chunk {
    pub fn disassemble(&self, name: &str) {
        println!("=== {} ===", name);

        let mut offset = 0;
        while offset < self.count() {
            offset = disassemble_instruction(self, offset);
        }
    }
}

pub fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    print!("{:04} ", offset);

    let op = &chunk.code[offset];
    match op {
        OpCode::Return => simple_instruction(op, offset),
    }
}

pub fn simple_instruction(op: &OpCode, offset: usize) -> usize {
    println!("{}", op);
    offset + 1
}
