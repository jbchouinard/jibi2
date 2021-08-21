use std::fmt;

use crate::chunk::Chunk;
use crate::instruction::{AnyInstruction, Instruction, Op};

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{:16}",
            match self {
                Op::Add => "OP_ADD",
                Op::Sub => "OP_SUB",
                Op::Mul => "OP_MUL",
                Op::Div => "OP_DIV",
                Op::Pop => "OP_POP",
                Op::NumEq => "OP_NUM_EQ",
                Op::NumNeq => "OP_NUM_NEQ",
                Op::NumLt => "OP_NUM_LT",
                Op::NumLte => "OP_NUM_LTE",
                Op::NumGt => "OP_NUM_GT",
                Op::NumGte => "OP_NUM_GTE",
                Op::DefGlobal => "OP_DEF_GLOBAL",
                Op::GetGlobal => "OP_GET_GLOBAL",
                Op::Return => "OP_RETURN",
                Op::Constant => "OP_CONSTANT",
                Op::ConstantLong => "OP_CONSTANT_LONG",
            }
        )
    }
}

impl<const N: usize> fmt::Display for Instruction<N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}  ", self.op)?;
        for n in 0..N {
            write!(f, "{:02x}  ", self.operands[n])?;
        }
        Ok(())
    }
}

impl fmt::Display for AnyInstruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Op0(ins) => write!(f, "{}", ins),
            Self::Op1(ins) => write!(f, "{}", ins),
            Self::Op2(ins) => write!(f, "{}", ins),
        }
    }
}

impl Chunk {
    pub fn disassemble_instruction(&self, prev_pos: usize, pos: usize) -> usize {
        print!("{:04x}  ", pos);
        let (ins, newpos) = AnyInstruction::read(&self.code, pos);

        let prev_line = self.get_line(prev_pos);
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
        newpos
    }

    pub fn disassemble(&self, name: &str) {
        println!("=== {} ===", name);

        let mut prev_offset = 0;
        let mut offset = 0;
        while offset < self.code.len() {
            let new_offset = self.disassemble_instruction(prev_offset, offset);
            prev_offset = offset;
            offset = new_offset;
        }
    }
}
