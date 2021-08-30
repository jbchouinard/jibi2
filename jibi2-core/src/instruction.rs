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
    pub const CONSTANT_LONG: u8 = CONSTANT | LONG;

    pub const DEF_GLOBAL: u8 = 0b00010001;
    pub const GET_GLOBAL: u8 = 0b00010010;
    pub const SET_GLOBAL: u8 = 0b00010011;

    pub const GET_LOCAL: u8 = 0b00010100;
    pub const SET_LOCAL: u8 = 0b00010101;
    pub const GET_LOCAL_LONG: u8 = GET_LOCAL | LONG;
    pub const SET_LOCAL_LONG: u8 = SET_LOCAL | LONG;

    pub const GET_UPVALUE: u8 = 0b00010110;
    pub const SET_UPVALUE: u8 = 0b00010111;
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

pub mod ins {
    use super::*;

    macro_rules! simple {
        ($f:ident, $op:expr) => {
            #[inline(always)]
            pub fn $f() -> $crate::instruction::Instruction<0> {
                $crate::instruction::Instruction::new($op, [])
            }
        };
    }
    macro_rules! short {
        ($f:ident, $op:expr) => {
            #[inline(always)]
            pub fn $f(n: u8) -> $crate::instruction::Instruction<1> {
                $crate::instruction::Instruction::new($op, [n])
            }
        };
    }
    macro_rules! long {
        ($f:ident, $op:expr) => {
            #[inline(always)]
            pub fn $f(n: u16) -> $crate::instruction::Instruction<2> {
                $crate::instruction::Instruction::new(
                    $op | $crate::instruction::OP::LONG,
                    make_long_operands(n),
                )
            }
        };
    }
    macro_rules! var {
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
    macro_rules! slv {
        ($fshort:ident, $flong:ident, $fvar:ident, $op:expr) => {
            short!($fshort, $op);
            long!($flong, $op);
            var!($fvar, $op);
        };
    }

    simple!(nop, OP::NOP);
    simple!(halt, OP::HALT);
    slv!(add_1, add_2, add, OP::ADD);
    slv!(sub_1, sub_2, sub, OP::SUB);
    slv!(mul_1, mul_2, mul, OP::MUL);
    slv!(div_1, div_2, div, OP::DIV);
    simple!(num_eq, OP::NUM_EQ);
    simple!(num_neq, OP::NUM_EQ);
    simple!(num_lt, OP::NUM_LT);
    simple!(num_lte, OP::NUM_LTE);
    simple!(num_gt, OP::NUM_GT);
    simple!(num_gte, OP::NUM_GTE);
    simple!(pop_r0, OP::POP_R0);
    simple!(push_r0, OP::PUSH_R0);
    simple!(pop, OP::POP);
    short!(pop_n, OP::POP_N);
    simple!(def_global, OP::DEF_GLOBAL);
    simple!(get_global, OP::GET_GLOBAL);
    simple!(set_global, OP::SET_GLOBAL);
    slv!(get_local_1, get_local_2, get_local, OP::GET_LOCAL);
    slv!(set_local_1, set_local_2, set_local, OP::SET_LOCAL_LONG);
    slv!(get_upvalue_1, get_upvalue_2, get_upvalue, OP::GET_UPVALUE);
    slv!(set_upvalue_1, set_upvalue_2, set_upvalue, OP::SET_UPVALUE);
    slv!(constant_1, constant_2, constant, OP::CONSTANT);
    simple!(r#return, OP::RETURN);
    simple!(equal, OP::EQUAL);
    long!(jump, OP::JUMP);
    long!(jump_if_true, OP::JUMP_TRUE);
    long!(jump_if_false, OP::JUMP_FALSE);
    short!(apply, OP::CALL);
    short!(tail_apply, OP::TAIL_CALL);
    simple!(repr, OP::REPR);
    simple!(print, OP::PRINT);
    slv!(closure_1, closure_2, closure, OP::CLOSURE);
    simple!(cons, OP::CONS);
    simple!(car, OP::CAR);
    simple!(cdr, OP::CDR);
    slv!(list_1, list_2, list, OP::LIST);
}

macro_rules! op0 {
    ($f:expr, $c:expr, $p:expr) => {
        (Op0($f()), $p + 1)
    };
}
macro_rules! op1 {
    ($f:expr, $c:expr, $p:expr) => {
        (Op1($f($c[$p + 1])), $p + 2)
    };
}
macro_rules! op2 {
    ($f:expr, $c:expr, $p:expr) => {
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
            OP::NOP => op0!(ins::nop, code, pos),
            OP::HALT => op0!(ins::halt, code, pos),
            OP::ADD => op1!(ins::add_1, code, pos),
            OP::SUB => op1!(ins::sub_1, code, pos),
            OP::MUL => op1!(ins::mul_1, code, pos),
            OP::DIV => op1!(ins::div_1, code, pos),
            OP::ADD_LONG => op2!(ins::add_2, code, pos),
            OP::SUB_LONG => op2!(ins::sub_2, code, pos),
            OP::MUL_LONG => op2!(ins::mul_2, code, pos),
            OP::DIV_LONG => op2!(ins::div_2, code, pos),
            OP::NUM_EQ => op0!(ins::num_eq, code, pos),
            OP::NUM_NEQ => op0!(ins::num_neq, code, pos),
            OP::NUM_LT => op0!(ins::num_lt, code, pos),
            OP::NUM_LTE => op0!(ins::num_lte, code, pos),
            OP::NUM_GT => op0!(ins::num_gt, code, pos),
            OP::NUM_GTE => op0!(ins::num_gte, code, pos),
            OP::EQUAL => op0!(ins::equal, code, pos),
            OP::POP_R0 => op0!(ins::pop_r0, code, pos),
            OP::PUSH_R0 => op0!(ins::push_r0, code, pos),
            OP::POP => op0!(ins::pop, code, pos),
            OP::POP_N => op1!(ins::pop_n, code, pos),
            OP::DEF_GLOBAL => op0!(ins::def_global, code, pos),
            OP::GET_GLOBAL => op0!(ins::get_global, code, pos),
            OP::SET_GLOBAL => op0!(ins::set_global, code, pos),
            OP::GET_LOCAL => op1!(ins::get_local_1, code, pos),
            OP::GET_LOCAL_LONG => op2!(ins::get_local_2, code, pos),
            OP::SET_LOCAL => op1!(ins::set_local_1, code, pos),
            OP::SET_LOCAL_LONG => op2!(ins::set_local_2, code, pos),
            OP::GET_UPVALUE => op1!(ins::get_upvalue_1, code, pos),
            OP::GET_UPVALUE_LONG => op2!(ins::get_upvalue_2, code, pos),
            OP::SET_UPVALUE => op1!(ins::set_upvalue_1, code, pos),
            OP::SET_UPVALUE_LONG => op2!(ins::set_upvalue_2, code, pos),
            OP::CONSTANT => op1!(ins::constant_1, code, pos),
            OP::CONSTANT_LONG => op2!(ins::constant_2, code, pos),
            OP::JUMP => op2!(ins::jump, code, pos),
            OP::JUMP_TRUE => op2!(ins::jump_if_true, code, pos),
            OP::JUMP_FALSE => op2!(ins::jump_if_false, code, pos),
            OP::CALL => op1!(ins::apply, code, pos),
            OP::TAIL_CALL => op1!(ins::tail_apply, code, pos),
            OP::RETURN => op0!(ins::r#return, code, pos),
            OP::REPR => op0!(ins::repr, code, pos),
            OP::PRINT => op0!(ins::print, code, pos),
            OP::CLOSURE => op1!(ins::closure_1, code, pos),
            OP::CLOSURE_LONG => op2!(ins::closure_2, code, pos),
            OP::CONS => op0!(ins::cons, code, pos),
            OP::CAR => op0!(ins::car, code, pos),
            OP::CDR => op0!(ins::cdr, code, pos),
            OP::LIST => op1!(ins::list_1, code, pos),
            OP::LIST_LONG => op2!(ins::list_2, code, pos),
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
