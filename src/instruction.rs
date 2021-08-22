pub struct Operand(u8);

#[derive(Debug, PartialEq, Clone)]
pub enum Op {
    Pop,
    PopN,
    PushR0,
    PopR0,
    DefGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    GetLocalLong,
    SetLocal,
    SetLocalLong,
    Constant,
    ConstantLong,
    Return,
    Equal,
    Add,
    Sub,
    Mul,
    Div,
    AddLong,
    SubLong,
    MulLong,
    DivLong,
    NumEq,
    NumNeq,
    NumLt,
    NumLte,
    NumGt,
    NumGte,
}

impl Op {
    pub fn as_byte(self) -> u8 {
        self as u8
    }
    pub fn from_byte(n: u8) -> Self {
        match n {
            n if n == Op::Pop as u8 => Op::Pop,
            n if n == Op::PopN as u8 => Op::PopN,
            n if n == Op::PushR0 as u8 => Op::PushR0,
            n if n == Op::PopR0 as u8 => Op::PopR0,
            n if n == Op::DefGlobal as u8 => Op::DefGlobal,
            n if n == Op::GetGlobal as u8 => Op::GetGlobal,
            n if n == Op::SetGlobal as u8 => Op::SetGlobal,
            n if n == Op::GetLocal as u8 => Op::GetLocal,
            n if n == Op::GetLocalLong as u8 => Op::GetLocalLong,
            n if n == Op::SetLocal as u8 => Op::SetLocal,
            n if n == Op::SetLocalLong as u8 => Op::SetLocalLong,
            n if n == Op::Constant as u8 => Op::Constant,
            n if n == Op::ConstantLong as u8 => Op::ConstantLong,
            n if n == Op::Return as u8 => Op::Return,
            n if n == Op::Add as u8 => Op::Add,
            n if n == Op::Sub as u8 => Op::Sub,
            n if n == Op::Mul as u8 => Op::Mul,
            n if n == Op::Div as u8 => Op::Div,
            n if n == Op::AddLong as u8 => Op::AddLong,
            n if n == Op::SubLong as u8 => Op::SubLong,
            n if n == Op::MulLong as u8 => Op::MulLong,
            n if n == Op::DivLong as u8 => Op::DivLong,
            n if n == Op::NumEq as u8 => Op::NumEq,
            n if n == Op::NumNeq as u8 => Op::NumNeq,
            n if n == Op::NumLt as u8 => Op::NumLt,
            n if n == Op::NumLte as u8 => Op::NumLte,
            n if n == Op::NumGt as u8 => Op::NumGt,
            n if n == Op::NumGte as u8 => Op::NumGte,
            n if n == Op::Equal as u8 => Op::Equal,
            _ => panic!("invalid opcode {}", n),
        }
    }
}

pub struct Instruction<const N: usize> {
    pub op: Op,
    pub operands: [u8; N],
}

impl<const N: usize> Instruction<N> {
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
        ((self.operands[0] as usize) * (u8::MAX as usize)) + (self.operands[1] as usize)
    }
}

fn make_long_operands(n: u16) -> [u8; 2] {
    [(n / (u8::MAX as u16)) as u8, (n % (u8::MAX as u16)) as u8]
}

fn read_long_operands(operands: &[u8]) -> u16 {
    (u8::MAX as u16 * operands[0] as u16) + (operands[1] as u16)
}

macro_rules! instruction_simple {
    ($f:ident, $op:expr) => {
        pub fn $f() -> $crate::instruction::Instruction<0> {
            $crate::instruction::Instruction::new($op, [])
        }
    };
}
macro_rules! instruction_short {
    ($f:ident, $op:expr) => {
        pub fn $f(n: u8) -> $crate::instruction::Instruction<1> {
            $crate::instruction::Instruction::new($op, [n])
        }
    };
}
macro_rules! instruction_long {
    ($f:ident, $op:expr) => {
        pub fn $f(n: u16) -> $crate::instruction::Instruction<2> {
            $crate::instruction::Instruction::new($op, make_long_operands(n))
        }
    };
}
macro_rules! instruction_var {
    ($f:ident, $opshort:expr, $oplong:expr) => {
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
    pub fn read(code: &[u8], pos: usize) -> (Self, usize) {
        use AnyInstruction::*;
        let opcode = code[pos];
        let op = Op::from_byte(opcode);
        match op {
            Op::Add => op1!(instruction_add_short, code, pos),
            Op::Sub => op1!(instruction_sub_short, code, pos),
            Op::Mul => op1!(instruction_mul_short, code, pos),
            Op::Div => op1!(instruction_div_short, code, pos),
            Op::AddLong => op2!(instruction_add_long, code, pos),
            Op::SubLong => op2!(instruction_sub_long, code, pos),
            Op::MulLong => op2!(instruction_mul_long, code, pos),
            Op::DivLong => op2!(instruction_div_long, code, pos),
            Op::NumEq => op0!(instruction_num_eq, code, pos),
            Op::NumNeq => op0!(instruction_num_neq, code, pos),
            Op::NumLt => op0!(instruction_num_lt, code, pos),
            Op::NumLte => op0!(instruction_num_lte, code, pos),
            Op::NumGt => op0!(instruction_num_gt, code, pos),
            Op::NumGte => op0!(instruction_num_gte, code, pos),
            Op::Equal => op0!(instruction_equal, code, pos),
            Op::PopR0 => op0!(instruction_pop_r0, code, pos),
            Op::PushR0 => op0!(instruction_push_r0, code, pos),
            Op::Pop => op0!(instruction_pop, code, pos),
            Op::PopN => op1!(instruction_pop_n, code, pos),
            Op::DefGlobal => op0!(instruction_def_global, code, pos),
            Op::GetGlobal => op0!(instruction_get_global, code, pos),
            Op::SetGlobal => op0!(instruction_set_global, code, pos),
            Op::GetLocal => op1!(instruction_get_local_short, code, pos),
            Op::GetLocalLong => op2!(instruction_get_local_long, code, pos),
            Op::SetLocal => op1!(instruction_set_local_short, code, pos),
            Op::SetLocalLong => op2!(instruction_set_local_long, code, pos),
            Op::Constant => op1!(instruction_constant_short, code, pos),
            Op::ConstantLong => op2!(instruction_constant_long, code, pos),
            Op::Return => op0!(instruction_return, code, pos),
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
