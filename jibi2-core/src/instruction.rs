pub struct Operand(u8);

#[derive(Debug, PartialEq, Clone)]
pub enum Op {
    Halt,
    Pop,
    PopN,
    PopR0,
    PushR0,
    DefGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    GetLocalLong,
    SetLocal,
    SetLocalLong,
    Constant,
    ConstantLong,
    Jump,
    JumpTrue,
    JumpFalse,
    Apply,
    Closure,
    ClosureLong,
    Return,
    Repr,
    Print,
    Equal,
    NumEq,
    NumNeq,
    NumLt,
    NumLte,
    NumGt,
    NumGte,
    Add,
    AddLong,
    Sub,
    SubLong,
    Mul,
    MulLong,
    Div,
    DivLong,
}

pub const OP_HALT: u8 = Op::Halt as u8;
pub const OP_POP: u8 = Op::Pop as u8;
pub const OP_POP_N: u8 = Op::PopN as u8;
pub const OP_POP_R0: u8 = Op::PopR0 as u8;
pub const OP_PUSH_R0: u8 = Op::PushR0 as u8;
pub const OP_DEF_GLOBAL: u8 = Op::DefGlobal as u8;
pub const OP_GET_GLOBAL: u8 = Op::GetGlobal as u8;
pub const OP_SET_GLOBAL: u8 = Op::SetGlobal as u8;
pub const OP_GET_LOCAL: u8 = Op::GetLocal as u8;
pub const OP_GET_LOCAL_LONG: u8 = Op::GetLocalLong as u8;
pub const OP_SET_LOCAL: u8 = Op::SetLocal as u8;
pub const OP_SET_LOCAL_LONG: u8 = Op::SetLocalLong as u8;
pub const OP_CONSTANT: u8 = Op::Constant as u8;
pub const OP_CONSTANT_LONG: u8 = Op::ConstantLong as u8;
pub const OP_RETURN: u8 = Op::Return as u8;
pub const OP_EQUAL: u8 = Op::Equal as u8;
pub const OP_ADD: u8 = Op::Add as u8;
pub const OP_SUB: u8 = Op::Sub as u8;
pub const OP_MUL: u8 = Op::Mul as u8;
pub const OP_DIV: u8 = Op::Div as u8;
pub const OP_ADD_LONG: u8 = Op::AddLong as u8;
pub const OP_SUB_LONG: u8 = Op::SubLong as u8;
pub const OP_MUL_LONG: u8 = Op::MulLong as u8;
pub const OP_DIV_LONG: u8 = Op::DivLong as u8;
pub const OP_NUM_EQ: u8 = Op::NumEq as u8;
pub const OP_NUM_NEQ: u8 = Op::NumNeq as u8;
pub const OP_NUM_LT: u8 = Op::NumLt as u8;
pub const OP_NUM_LTE: u8 = Op::NumLte as u8;
pub const OP_NUM_GT: u8 = Op::NumGt as u8;
pub const OP_NUM_GTE: u8 = Op::NumGte as u8;
pub const OP_JUMP: u8 = Op::Jump as u8;
pub const OP_JUMP_TRUE: u8 = Op::JumpTrue as u8;
pub const OP_JUMP_FALSE: u8 = Op::JumpFalse as u8;
pub const OP_APPLY: u8 = Op::Apply as u8;
pub const OP_REPR: u8 = Op::Repr as u8;
pub const OP_PRINT: u8 = Op::Print as u8;
pub const OP_CLOSURE: u8 = Op::Closure as u8;
pub const OP_CLOSURE_LONG: u8 = Op::ClosureLong as u8;

pub struct Instruction<const N: usize> {
    pub op: Op,
    pub operands: [u8; N],
}

impl<const N: usize> Instruction<N> {
    #[inline(always)]
    pub fn new(op: Op, operands: [u8; N]) -> Self {
        Self { op, operands }
    }
}

impl Instruction<1> {
    pub fn get_usize(&self) -> usize {
        self.operands[0] as usize
    }
}

impl Instruction<2> {
    pub fn get_long_usize(&self) -> usize {
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
    ($f:ident, $opshort:expr, $oplong:expr) => {
        #[inline(always)]
        pub fn $f(n: usize) -> $crate::instruction::AnyInstruction {
            if n <= u8::MAX as usize {
                $crate::instruction::Instruction::<1>::new($opshort, [n as u8]).into()
            } else if n <= u16::MAX as usize {
                $crate::instruction::Instruction::<2>::new($oplong, make_long_operands(n as u16))
                    .into()
            } else {
                panic!("operand too large");
            }
        }
    };
}

instruction_simple!(instruction_halt, Op::Halt);
instruction_short!(instruction_add_short, Op::Add);
instruction_short!(instruction_sub_short, Op::Sub);
instruction_short!(instruction_mul_short, Op::Mul);
instruction_short!(instruction_div_short, Op::Div);
instruction_long!(instruction_add_long, Op::AddLong);
instruction_long!(instruction_sub_long, Op::SubLong);
instruction_long!(instruction_mul_long, Op::MulLong);
instruction_long!(instruction_div_long, Op::DivLong);
instruction_var!(instruction_add, Op::Add, Op::AddLong);
instruction_var!(instruction_sub, Op::Sub, Op::SubLong);
instruction_var!(instruction_mul, Op::Mul, Op::MulLong);
instruction_var!(instruction_div, Op::Div, Op::DivLong);
instruction_simple!(instruction_num_eq, Op::NumEq);
instruction_simple!(instruction_num_neq, Op::NumNeq);
instruction_simple!(instruction_num_lt, Op::NumLt);
instruction_simple!(instruction_num_lte, Op::NumLte);
instruction_simple!(instruction_num_gt, Op::NumGt);
instruction_simple!(instruction_num_gte, Op::NumGte);
instruction_simple!(instruction_pop_r0, Op::PopR0);
instruction_simple!(instruction_push_r0, Op::PushR0);
instruction_simple!(instruction_pop, Op::Pop);
instruction_short!(instruction_pop_n, Op::PopN);
instruction_simple!(instruction_def_global, Op::DefGlobal);
instruction_simple!(instruction_get_global, Op::GetGlobal);
instruction_simple!(instruction_set_global, Op::SetGlobal);
instruction_short!(instruction_get_local_short, Op::GetLocal);
instruction_long!(instruction_get_local_long, Op::GetLocalLong);
instruction_var!(instruction_get_local, Op::GetLocal, Op::GetLocalLong);
instruction_short!(instruction_set_local_short, Op::SetLocal);
instruction_long!(instruction_set_local_long, Op::SetLocalLong);
instruction_var!(instruction_set_local, Op::SetLocal, Op::SetLocalLong);
instruction_short!(instruction_constant_short, Op::Constant);
instruction_long!(instruction_constant_long, Op::ConstantLong);
instruction_var!(instruction_constant, Op::Constant, Op::ConstantLong);
instruction_simple!(instruction_return, Op::Return);
instruction_simple!(instruction_equal, Op::Equal);
instruction_long!(instruction_jump, Op::Jump);
instruction_long!(instruction_jump_if_true, Op::JumpTrue);
instruction_long!(instruction_jump_if_false, Op::JumpFalse);
instruction_short!(instruction_apply, Op::Apply);
instruction_simple!(instruction_repr, Op::Repr);
instruction_simple!(instruction_print, Op::Print);
instruction_short!(instruction_closure_short, Op::Closure);
instruction_long!(instruction_closure_long, Op::ClosureLong);
instruction_var!(instruction_closure, Op::Closure, Op::ClosureLong);

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
    pub fn op(&self) -> &Op {
        match self {
            Self::Op0(ins) => &ins.op,
            Self::Op1(ins) => &ins.op,
            Self::Op2(ins) => &ins.op,
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
            Self::Op1(ins) => ins.get_usize(),
            Self::Op2(ins) => ins.get_long_usize(),
            _ => panic!("not a constant"),
        }
    }
    pub fn is_static(&self) -> bool {
        matches!(
            self.op(),
            Op::Pop
                | Op::PopN
                | Op::PushR0
                | Op::PopR0
                | Op::Constant
                | Op::ConstantLong
                | Op::Equal
                | Op::Add
                | Op::Sub
                | Op::Mul
                | Op::Div
                | Op::AddLong
                | Op::SubLong
                | Op::MulLong
                | Op::DivLong
                | Op::NumEq
                | Op::NumNeq
                | Op::NumLt
                | Op::NumLte
                | Op::NumGt
                | Op::NumGte
                | Op::Jump
                | Op::JumpTrue
                | Op::JumpFalse
        )
    }
    pub fn read(code: &[u8], pos: usize) -> (Self, usize) {
        use AnyInstruction::*;
        match code[pos] {
            OP_HALT => op0!(instruction_halt, code, pos),
            OP_ADD => op1!(instruction_add_short, code, pos),
            OP_SUB => op1!(instruction_sub_short, code, pos),
            OP_MUL => op1!(instruction_mul_short, code, pos),
            OP_DIV => op1!(instruction_div_short, code, pos),
            OP_ADD_LONG => op2!(instruction_add_long, code, pos),
            OP_SUB_LONG => op2!(instruction_sub_long, code, pos),
            OP_MUL_LONG => op2!(instruction_mul_long, code, pos),
            OP_DIV_LONG => op2!(instruction_div_long, code, pos),
            OP_NUM_EQ => op0!(instruction_num_eq, code, pos),
            OP_NUM_NEQ => op0!(instruction_num_neq, code, pos),
            OP_NUM_LT => op0!(instruction_num_lt, code, pos),
            OP_NUM_LTE => op0!(instruction_num_lte, code, pos),
            OP_NUM_GT => op0!(instruction_num_gt, code, pos),
            OP_NUM_GTE => op0!(instruction_num_gte, code, pos),
            OP_EQUAL => op0!(instruction_equal, code, pos),
            OP_POP_R0 => op0!(instruction_pop_r0, code, pos),
            OP_PUSH_R0 => op0!(instruction_push_r0, code, pos),
            OP_POP => op0!(instruction_pop, code, pos),
            OP_POP_N => op1!(instruction_pop_n, code, pos),
            OP_DEF_GLOBAL => op0!(instruction_def_global, code, pos),
            OP_GET_GLOBAL => op0!(instruction_get_global, code, pos),
            OP_SET_GLOBAL => op0!(instruction_set_global, code, pos),
            OP_GET_LOCAL => op1!(instruction_get_local_short, code, pos),
            OP_GET_LOCAL_LONG => op2!(instruction_get_local_long, code, pos),
            OP_SET_LOCAL => op1!(instruction_set_local_short, code, pos),
            OP_SET_LOCAL_LONG => op2!(instruction_set_local_long, code, pos),
            OP_CONSTANT => op1!(instruction_constant_short, code, pos),
            OP_CONSTANT_LONG => op2!(instruction_constant_long, code, pos),
            OP_RETURN => op0!(instruction_return, code, pos),
            OP_JUMP => op2!(instruction_jump, code, pos),
            OP_JUMP_TRUE => op2!(instruction_jump_if_true, code, pos),
            OP_JUMP_FALSE => op2!(instruction_jump_if_false, code, pos),
            OP_APPLY => op1!(instruction_apply, code, pos),
            OP_REPR => op0!(instruction_repr, code, pos),
            OP_PRINT => op0!(instruction_print, code, pos),
            OP_CLOSURE => op1!(instruction_closure_short, code, pos),
            OP_CLOSURE_LONG => op2!(instruction_closure_long, code, pos),
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
