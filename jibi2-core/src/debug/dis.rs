use std::fmt;

use crate::chunk::Chunk;
use crate::instruction::{AnyInstruction, Instruction, Op};

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{:16}",
            match self {
                Op::Halt => "OP_HALT",
                Op::Add => "OP_ADD",
                Op::Sub => "OP_SUB",
                Op::Mul => "OP_MUL",
                Op::Div => "OP_DIV",
                Op::AddLong => "OP_ADD_LONG",
                Op::SubLong => "OP_SUB_LONG",
                Op::MulLong => "OP_MUL_LONG",
                Op::DivLong => "OP_DIV_LONG",
                Op::PopR0 => "OP_POP_R0",
                Op::PushR0 => "OP_PUSH_R0",
                Op::Pop => "OP_POP",
                Op::PopN => "OP_POP_N",
                Op::NumEq => "OP_NUM_EQ",
                Op::NumNeq => "OP_NUM_NEQ",
                Op::NumLt => "OP_NUM_LT",
                Op::NumLte => "OP_NUM_LTE",
                Op::NumGt => "OP_NUM_GT",
                Op::NumGte => "OP_NUM_GTE",
                Op::Equal => "OP_EQUAL",
                Op::DefGlobal => "OP_DEF_GLOBAL",
                Op::GetGlobal => "OP_GET_GLOBAL",
                Op::SetGlobal => "OP_SET_GLOBAL",
                Op::GetLocal => "OP_GET_LOCAL",
                Op::GetLocalLong => "OP_GET_LOCAL_LONG",
                Op::SetLocal => "OP_SET_LOCAL",
                Op::SetLocalLong => "OP_SET_LOCAL_LONG",
                Op::Return => "OP_RETURN",
                Op::Constant => "OP_CONSTANT",
                Op::ConstantLong => "OP_CONSTANT_LONG",
                Op::Jump => "OP_JUMP",
                Op::JumpTrue => "OP_JUMP_IF_TRUE",
                Op::JumpFalse => "OP_JUMP_IF_FALSE",
                Op::Apply => "OP_APPLY",
                Op::Repr => "OP_REPR",
                Op::Print => "OP_PRINT",
                Op::Closure => "OP_CLOSURE",
                Op::ClosureLong => "OP_CLOSURE_LONG",
            }
        )
    }
}

impl<const N: usize> fmt::Display for Instruction<N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}  ", self.op)?;
        for n in 0..N {
            write!(f, "0x{:02x}  ", self.operands[n])?;
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
        print!("0x{:04x}  ", pos);
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
                println!("{} ({})", ins, self.constants[ins.get_usize()]);
            }
            Op::ConstantLong => {
                println!("{} ({})", ins, self.constants[ins.get_usize()]);
            }
            _ => println!("{}", ins),
        };
        newpos
    }

    pub fn disassemble(&self, name: &str, starting_offset: usize) {
        println!("{:=^48}", format!(" {} ", name));
        let mut prev_offset = starting_offset;
        let mut offset = starting_offset;
        while offset < self.code.len() {
            let new_offset = self.disassemble_instruction(prev_offset, offset);
            prev_offset = offset;
            offset = new_offset;
        }
        println!("{:=^48}", "");
    }
}
