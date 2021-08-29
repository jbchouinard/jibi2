use std::fmt;

use crate::chunk::Chunk;
use crate::instruction::*;

pub fn op_string(op: u8) -> String {
    format!(
        "{:16}",
        match op {
            OP::NOP => "OP_NOP",
            OP::HALT => "OP_HALT",
            OP::ADD => "OP_ADD",
            OP::SUB => "OP_SUB",
            OP::MUL => "OP_MUL",
            OP::DIV => "OP_DIV",
            OP::ADD_LONG => "OP_ADD_LONG",
            OP::SUB_LONG => "OP_SUB_LONG",
            OP::MUL_LONG => "OP_MUL_LONG",
            OP::DIV_LONG => "OP_DIV_LONG",
            OP::POP_R0 => "OP_POP_R0",
            OP::PUSH_R0 => "OP_PUSH_R0",
            OP::POP => "OP_POP",
            OP::POP_N => "OP_POP_N",
            OP::NUM_EQ => "OP_NUM_EQ",
            OP::NUM_NEQ => "OP_NUM_NEQ",
            OP::NUM_LT => "OP_NUM_LT",
            OP::NUM_LTE => "OP_NUM_LTE",
            OP::NUM_GT => "OP_NUM_GT",
            OP::NUM_GTE => "OP_NUM_GTE",
            OP::EQUAL => "OP_EQUAL",
            OP::DEF_GLOBAL => "OP_DEF_GLOBAL",
            OP::GET_GLOBAL => "OP_GET_GLOBAL",
            OP::SET_GLOBAL => "OP_SET_GLOBAL",
            OP::GET_LOCAL => "OP_GET_LOCAL",
            OP::GET_LOCAL_LONG => "OP_GET_LOCAL_LONG",
            OP::SET_LOCAL => "OP_SET_LOCAL",
            OP::SET_LOCAL_LONG => "OP_SET_LOCAL_LONG",
            OP::GET_UPVALUE => "OP_GET_UPVALUE",
            OP::SET_UPVALUE => "OP_SET_UPVALUE",
            OP::RETURN => "OP_RETURN",
            OP::CONSTANT => "OP_CONSTANT",
            OP::CONSTANT_LONG => "OP_CONSTANT_LONG",
            OP::JUMP => "OP_JUMP",
            OP::JUMP_TRUE => "OP_JUMP_IF_TRUE",
            OP::JUMP_FALSE => "OP_JUMP_IF_FALSE",
            OP::CALL => "OP_CALL",
            OP::TAIL_CALL => "OP_TAIL_CALL",
            OP::REPR => "OP_REPR",
            OP::PRINT => "OP_PRINT",
            OP::CLOSURE => "OP_CLOSURE",
            OP::CLOSURE_LONG => "OP_CLOSURE_LONG",
            OP::CONS => "OP_CONS",
            OP::CAR => "OP_CAR",
            OP::CDR => "OP_CDR",
            OP::LIST => "OP_LIST",
            OP::LIST_LONG => "OP_LIST_LONG",
            n => panic!("invalid opcode {}", n),
        }
    )
}

impl<const N: usize> fmt::Display for Instruction<N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}  ", op_string(self.op))?;
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
            OP::CONSTANT => {
                println!("{} ({})", ins, self.constants[ins.get_usize()]);
            }
            OP::CONSTANT_LONG => {
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
