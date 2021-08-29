#[allow(non_snake_case)]
pub mod OP {
    pub const LONG: u8 = 0b10000000;

    pub const NOP: u8 = 0b00000000;

    pub const CALL: u8 = 0b00000001;
    pub const RETURN: u8 = 0b00000010;
    pub const TAIL_CALL: u8 = 0b00000011;

    pub const CLOSURE: u8 = 0b00000100;
    pub const CLOSURE_LONG: u8 = CLOSURE | LONG;

    pub const POP: u8 = 0b00001000;
    pub const POP_N: u8 = 0b00001001;
    pub const POP_R0: u8 = 0b00001010;
    pub const PUSH_R0: u8 = 0b00001011;

    pub const JUMP: u8 = 0b00001100 | LONG;
    pub const JUMP_TRUE: u8 = 0b00001101 | LONG;
    pub const JUMP_FALSE: u8 = 0b00001110 | LONG;

    pub const CONSTANT: u8 = 0b00010000;
    pub const DEF_GLOBAL: u8 = 0b00010001;
    pub const GET_GLOBAL: u8 = 0b00010010;
    pub const SET_GLOBAL: u8 = 0b00010011;
    pub const GET_LOCAL: u8 = 0b00010100;
    pub const SET_LOCAL: u8 = 0b00010101;
    pub const GET_UPVALUE: u8 = 0b00010110;
    pub const SET_UPVALUE: u8 = 0b00010111;
    pub const CONSTANT_LONG: u8 = CONSTANT | LONG;
    pub const GET_LOCAL_LONG: u8 = GET_LOCAL | LONG;
    pub const SET_LOCAL_LONG: u8 = SET_LOCAL | LONG;
    pub const GET_UPVALUE_LONG: u8 = GET_UPVALUE | LONG;
    pub const SET_UPVALUE_LONG: u8 = SET_UPVALUE | LONG;

    pub const EQUAL: u8 = 0b00011000;
    pub const REPR: u8 = 0b00011001;
    pub const PRINT: u8 = 0b00011010;

    pub const CONS: u8 = 0b00011011;
    pub const CAR: u8 = 0b00011100;
    pub const CDR: u8 = 0b00011101;
    pub const LIST: u8 = 0b00011110;
    pub const LIST_LONG: u8 = LIST | LONG;

    pub const ADD: u8 = 0b00100000;
    pub const SUB: u8 = 0b00100001;
    pub const MUL: u8 = 0b00100010;
    pub const DIV: u8 = 0b00100011;
    pub const ADD_LONG: u8 = ADD | LONG;
    pub const SUB_LONG: u8 = SUB | LONG;
    pub const MUL_LONG: u8 = MUL | LONG;
    pub const DIV_LONG: u8 = DIV | LONG;

    pub const NUM_EQ: u8 = 0b00100100;
    pub const NUM_NEQ: u8 = 0b00100101;
    pub const NUM_LT: u8 = 0b00100110;
    pub const NUM_LTE: u8 = 0b00100111;
    pub const NUM_GT: u8 = 0b00101000;
    pub const NUM_GTE: u8 = 0b00101001;

    pub const HALT: u8 = 0b11111111;
}

pub struct Instruction<const N: usize> {
    pub op: u8,
    pub operands: [u8; N],
}

impl<const N: usize> Instruction<N> {
    #[inline(always)]
    pub fn new(op: u8, operands: [u8; N]) -> Self {
        Self { op, operands }
    }
}

impl Instruction<1> {
    pub fn get_usize(&self) -> usize {
        self.operands[0] as usize
    }
}

impl Instruction<2> {
    pub fn get_usize(&self) -> usize {
        read_long_operands(&self.operands[0..2]) as usize
    }
}

pub fn make_long_operands(n: u16) -> [u8; 2] {
    [((n >> 8) & 0xff) as u8, (n & 0xff) as u8]
}

fn read_long_operands(operands: &[u8]) -> u16 {
    (operands[0] as u16) << 8 | (operands[1] as u16)
}

macro_rules! instruction_simple {
    ($f:ident, $op:expr) => {
        #[inline(always)]
        pub fn $f() -> $crate::instruction::Instruction<0> {
            $crate::instruction::Instruction::new($op, [])
        }
    };
}
macro_rules! instruction_short {
    ($f:ident, $op:expr) => {
        #[inline(always)]
        pub fn $f(n: u8) -> $crate::instruction::Instruction<1> {
            $crate::instruction::Instruction::new($op, [n])
        }
    };
}
macro_rules! instruction_long {
    ($f:ident, $op:expr) => {
        #[inline(always)]
        pub fn $f(n: u16) -> $crate::instruction::Instruction<2> {
            $crate::instruction::Instruction::new($op, make_long_operands(n))
        }
    };
}
macro_rules! instruction_var {
    ($f:ident, $op:expr) => {
        #[inline(always)]
        pub fn $f(n: usize) -> $crate::instruction::AnyInstruction {
            if n <= u8::MAX as usize {
                $crate::instruction::Instruction::<1>::new($op, [n as u8]).into()
            } else if n <= u16::MAX as usize {
                $crate::instruction::Instruction::<2>::new(
                    $op | $crate::instruction::OP::LONG,
                    make_long_operands(n as u16),
                )
                .into()
            } else {
                panic!("operand too large");
            }
        }
    };
}

instruction_simple!(instruction_nop, OP::NOP);
instruction_simple!(instruction_halt, OP::HALT);
instruction_short!(instruction_add_short, OP::ADD);
instruction_short!(instruction_sub_short, OP::SUB);
instruction_short!(instruction_mul_short, OP::MUL);
instruction_short!(instruction_div_short, OP::DIV);
instruction_long!(instruction_add_long, OP::ADD_LONG);
instruction_long!(instruction_sub_long, OP::SUB_LONG);
instruction_long!(instruction_mul_long, OP::MUL_LONG);
instruction_long!(instruction_div_long, OP::DIV_LONG);
instruction_var!(instruction_add, OP::ADD);
instruction_var!(instruction_sub, OP::SUB);
instruction_var!(instruction_mul, OP::MUL);
instruction_var!(instruction_div, OP::DIV);
instruction_simple!(instruction_num_eq, OP::NUM_EQ);
instruction_simple!(instruction_num_neq, OP::NUM_EQ);
instruction_simple!(instruction_num_lt, OP::NUM_LT);
instruction_simple!(instruction_num_lte, OP::NUM_LTE);
instruction_simple!(instruction_num_gt, OP::NUM_GT);
instruction_simple!(instruction_num_gte, OP::NUM_GTE);
instruction_simple!(instruction_pop_r0, OP::POP_R0);
instruction_simple!(instruction_push_r0, OP::PUSH_R0);
instruction_simple!(instruction_pop, OP::POP);
instruction_short!(instruction_pop_n, OP::POP_N);
instruction_simple!(instruction_def_global, OP::DEF_GLOBAL);
instruction_simple!(instruction_get_global, OP::GET_GLOBAL);
instruction_simple!(instruction_set_global, OP::SET_GLOBAL);
instruction_short!(instruction_get_local_short, OP::GET_LOCAL);
instruction_long!(instruction_get_local_long, OP::GET_LOCAL_LONG);
instruction_var!(instruction_get_local, OP::GET_LOCAL);
instruction_short!(instruction_set_local_short, OP::SET_LOCAL);
instruction_long!(instruction_set_local_long, OP::SET_LOCAL_LONG);
instruction_var!(instruction_set_local, OP::SET_LOCAL);
instruction_short!(instruction_get_upvalue, OP::GET_UPVALUE);
instruction_short!(instruction_set_upvalue, OP::SET_UPVALUE);
instruction_short!(instruction_constant_short, OP::CONSTANT);
instruction_long!(instruction_constant_long, OP::CONSTANT_LONG);
instruction_var!(instruction_constant, OP::CONSTANT);
instruction_simple!(instruction_return, OP::RETURN);
instruction_simple!(instruction_equal, OP::EQUAL);
instruction_long!(instruction_jump, OP::JUMP);
instruction_long!(instruction_jump_if_true, OP::JUMP_TRUE);
instruction_long!(instruction_jump_if_false, OP::JUMP_FALSE);
instruction_short!(instruction_apply, OP::CALL);
instruction_short!(instruction_tail_apply, OP::TAIL_CALL);
instruction_simple!(instruction_repr, OP::REPR);
instruction_simple!(instruction_print, OP::PRINT);
instruction_short!(instruction_closure_short, OP::CLOSURE);
instruction_long!(instruction_closure_long, OP::CLOSURE_LONG);
instruction_var!(instruction_closure, OP::CLOSURE);
instruction_simple!(instruction_cons, OP::CONS);
instruction_simple!(instruction_car, OP::CAR);
instruction_simple!(instruction_cdr, OP::CDR);
instruction_short!(instruction_list_short, OP::LIST);
instruction_long!(instruction_list_long, OP::LIST_LONG);
instruction_var!(instruction_list, OP::LIST);

macro_rules! op0 {
    ($f:ident, $c:expr, $p:expr) => {
        (Op0($f()), $p + 1)
    };
}
macro_rules! op1 {
    ($f:ident, $c:expr, $p:expr) => {
        (Op1($f($c[$p + 1])), $p + 2)
    };
}
macro_rules! op2 {
    ($f:ident, $c:expr, $p:expr) => {
        (Op2($f(read_long_operands(&$c[$p + 1..$p + 3]))), $p + 3)
    };
}

pub enum AnyInstruction {
    Op0(Instruction<0>),
    Op1(Instruction<1>),
    Op2(Instruction<2>),
}

impl AnyInstruction {
    pub fn op(&self) -> u8 {
        match self {
            Self::Op0(ins) => ins.op,
            Self::Op1(ins) => ins.op,
            Self::Op2(ins) => ins.op,
        }
    }
    pub fn get_operand(&self, n: usize) -> u8 {
        match self {
            Self::Op0(ins) => ins.operands[n],
            Self::Op1(ins) => ins.operands[n],
            Self::Op2(ins) => ins.operands[n],
        }
    }
    pub fn size(&self) -> usize {
        match self {
            Self::Op0(_) => 1,
            Self::Op1(_) => 2,
            Self::Op2(_) => 3,
        }
    }
    pub fn get_usize(&self) -> usize {
        match self {
            Self::Op0(_) => 0,
            Self::Op1(ins) => ins.get_usize(),
            Self::Op2(ins) => ins.get_usize(),
        }
    }
    pub fn is_static(&self) -> bool {
        matches!(
            self.op(),
            OP::POP
                | OP::POP_N
                | OP::PUSH_R0
                | OP::POP_R0
                | OP::CONSTANT
                | OP::CONSTANT_LONG
                | OP::EQUAL
                | OP::ADD
                | OP::SUB
                | OP::MUL
                | OP::DIV
                | OP::ADD_LONG
                | OP::SUB_LONG
                | OP::MUL_LONG
                | OP::DIV_LONG
                | OP::NUM_EQ
                | OP::NUM_NEQ
                | OP::NUM_LT
                | OP::NUM_LTE
                | OP::NUM_GT
                | OP::NUM_GTE
                | OP::JUMP
                | OP::JUMP_TRUE
                | OP::JUMP_FALSE
                | OP::CONS
                | OP::CAR
                | OP::CDR
                | OP::LIST
                | OP::LIST_LONG
        )
    }
    pub fn read_all(code: &[u8], start: usize, size: usize) -> Vec<Self> {
        let mut instructions = vec![];
        let mut pos = start;
        while pos < size {
            let (ins, newpos) = Self::read(code, pos);
            instructions.push(ins);
            pos = newpos;
        }
        instructions
    }
    pub fn read(code: &[u8], pos: usize) -> (Self, usize) {
        use AnyInstruction::*;
        match code[pos] {
            OP::NOP => op0!(instruction_nop, code, pos),
            OP::HALT => op0!(instruction_halt, code, pos),
            OP::ADD => op1!(instruction_add_short, code, pos),
            OP::SUB => op1!(instruction_sub_short, code, pos),
            OP::MUL => op1!(instruction_mul_short, code, pos),
            OP::DIV => op1!(instruction_div_short, code, pos),
            OP::ADD_LONG => op2!(instruction_add_long, code, pos),
            OP::SUB_LONG => op2!(instruction_sub_long, code, pos),
            OP::MUL_LONG => op2!(instruction_mul_long, code, pos),
            OP::DIV_LONG => op2!(instruction_div_long, code, pos),
            OP::NUM_EQ => op0!(instruction_num_eq, code, pos),
            OP::NUM_NEQ => op0!(instruction_num_neq, code, pos),
            OP::NUM_LT => op0!(instruction_num_lt, code, pos),
            OP::NUM_LTE => op0!(instruction_num_lte, code, pos),
            OP::NUM_GT => op0!(instruction_num_gt, code, pos),
            OP::NUM_GTE => op0!(instruction_num_gte, code, pos),
            OP::EQUAL => op0!(instruction_equal, code, pos),
            OP::POP_R0 => op0!(instruction_pop_r0, code, pos),
            OP::PUSH_R0 => op0!(instruction_push_r0, code, pos),
            OP::POP => op0!(instruction_pop, code, pos),
            OP::POP_N => op1!(instruction_pop_n, code, pos),
            OP::DEF_GLOBAL => op0!(instruction_def_global, code, pos),
            OP::GET_GLOBAL => op0!(instruction_get_global, code, pos),
            OP::SET_GLOBAL => op0!(instruction_set_global, code, pos),
            OP::GET_LOCAL => op1!(instruction_get_local_short, code, pos),
            OP::GET_LOCAL_LONG => op2!(instruction_get_local_long, code, pos),
            OP::SET_LOCAL => op1!(instruction_set_local_short, code, pos),
            OP::SET_LOCAL_LONG => op2!(instruction_set_local_long, code, pos),
            OP::GET_UPVALUE => op1!(instruction_get_upvalue, code, pos),
            OP::SET_UPVALUE => op1!(instruction_set_upvalue, code, pos),
            OP::CONSTANT => op1!(instruction_constant_short, code, pos),
            OP::CONSTANT_LONG => op2!(instruction_constant_long, code, pos),
            OP::JUMP => op2!(instruction_jump, code, pos),
            OP::JUMP_TRUE => op2!(instruction_jump_if_true, code, pos),
            OP::JUMP_FALSE => op2!(instruction_jump_if_false, code, pos),
            OP::CALL => op1!(instruction_apply, code, pos),
            OP::TAIL_CALL => op1!(instruction_tail_apply, code, pos),
            OP::RETURN => op0!(instruction_return, code, pos),
            OP::REPR => op0!(instruction_repr, code, pos),
            OP::PRINT => op0!(instruction_print, code, pos),
            OP::CLOSURE => op1!(instruction_closure_short, code, pos),
            OP::CLOSURE_LONG => op2!(instruction_closure_long, code, pos),
            OP::CONS => op0!(instruction_cons, code, pos),
            OP::CAR => op0!(instruction_car, code, pos),
            OP::CDR => op0!(instruction_cdr, code, pos),
            OP::LIST => op1!(instruction_list_short, code, pos),
            OP::LIST_LONG => op2!(instruction_list_long, code, pos),
            _ => panic!("invalid opcode"),
        }
    }
}

impl From<Instruction<0>> for AnyInstruction {
    fn from(ins: Instruction<0>) -> Self {
        Self::Op0(ins)
    }
}

impl From<Instruction<1>> for AnyInstruction {
    fn from(ins: Instruction<1>) -> Self {
        Self::Op1(ins)
    }
}

impl From<Instruction<2>> for AnyInstruction {
    fn from(ins: Instruction<2>) -> Self {
        Self::Op2(ins)
    }
}
