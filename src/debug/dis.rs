use std::fmt;

use crate::chunk::Chunk;
use crate::instruction::{Instruction, Op};

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{:16}",
            match self {
                Op::Negate => "OP_NEGATE",
                Op::Add => "OP_ADD",
                Op::Sub => "OP_SUB",
                Op::Mul => "OP_MUL",
                Op::Div => "OP_DIV",
                Op::Return => "OP_RETURN",
                Op::Constant => "OP_CONSTANT",
                Op::ConstantLong => "OP_CONSTANT_LONG",
            }
        )
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Op0(ins) => write!(f, "{}", ins.op),
            Self::Op1(ins) => write!(f, "{}  {:02x}", ins.op, ins.operands[0]),
            Self::Op2(ins) => write!(
                f,
                "{}  {:02x}  {:02x}",
                ins.op, ins.operands[0], ins.operands[1]
            ),
        }
    }
}

impl Chunk {
    pub fn disassemble_instruction(&self, pos: usize, offset: usize) -> usize {
        let ins = &self.code[pos];
        print!("{:04x}  ", offset);

        let prev_line = if pos > 0 { self.get_line(pos - 1) } else { 0 };
        let cur_line = self.get_line(pos);
        if pos == 0 || prev_line != cur_line {
            print!("{:4}  ", cur_line);
        } else {
            print!("   |  ");
        }

        match ins.op() {
            Op::Constant => {
                println!("{} ({})", ins, self.constants[ins.get_constant_n()]);
            }
            Op::ConstantLong => {
                println!("{} ({})", ins, self.constants[ins.get_constant_n()]);
            }
            _ => println!("{}", ins),
        };
        ins.size()
    }

    pub fn disassemble(&self, name: &str) {
        println!("=== {} ===", name);

        let mut offset = 0;

        for pos in 0..self.code.len() {
            offset += self.disassemble_instruction(pos, offset);
        }
    }
}
