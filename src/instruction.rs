pub struct Operand(u8);

#[derive(Debug, PartialEq, Clone)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    NumEq,
    NumNeq,
    NumLt,
    NumLte,
    NumGt,
    NumGte,
    Pop,
    DefGlobal,
    GetGlobal,
    Constant,
    ConstantLong,
    Return,
}

impl Op {
    pub fn as_byte(self) -> u8 {
        self as u8
    }
    pub fn from_byte(n: u8) -> Self {
        match n {
            n if n == Self::Add as u8 => Self::Add,
            n if n == Self::Sub as u8 => Self::Sub,
            n if n == Self::Mul as u8 => Self::Mul,
            n if n == Self::Div as u8 => Self::Div,
            n if n == Self::Pop as u8 => Self::Pop,
            n if n == Self::DefGlobal as u8 => Self::DefGlobal,
            n if n == Self::GetGlobal as u8 => Self::DefGlobal,
            n if n == Self::Constant as u8 => Self::Constant,
            n if n == Self::ConstantLong as u8 => Self::ConstantLong,
            n if n == Self::Return as u8 => Self::Return,
            n if n == Self::Constant as u8 => Self::Constant,
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
    pub fn get_constant_n(&self) -> usize {
        self.operands[0] as usize
    }
}

impl Instruction<2> {
    pub fn get_constant_n(&self) -> usize {
        ((self.operands[0] as usize) * (u8::MAX as usize)) + (self.operands[1] as usize)
    }
}

pub fn instruction_add(n: u8) -> Instruction<1> {
    Instruction::new(Op::Add, [n])
}
pub fn instruction_sub(n: u8) -> Instruction<1> {
    Instruction::new(Op::Sub, [n])
}
pub fn instruction_mul(n: u8) -> Instruction<1> {
    Instruction::new(Op::Mul, [n])
}
pub fn instruction_div(n: u8) -> Instruction<1> {
    Instruction::new(Op::Div, [n])
}
pub fn instruction_num_eq() -> Instruction<0> {
    Instruction::new(Op::NumEq, [])
}
pub fn instruction_num_neq() -> Instruction<0> {
    Instruction::new(Op::NumNeq, [])
}
pub fn instruction_num_lt() -> Instruction<0> {
    Instruction::new(Op::NumLt, [])
}
pub fn instruction_num_lte() -> Instruction<0> {
    Instruction::new(Op::NumLte, [])
}
pub fn instruction_num_gt() -> Instruction<0> {
    Instruction::new(Op::NumGt, [])
}
pub fn instruction_num_gte() -> Instruction<0> {
    Instruction::new(Op::NumGte, [])
}
pub fn instruction_pop() -> Instruction<0> {
    Instruction::new(Op::Pop, [])
}
pub fn instruction_def_global() -> Instruction<0> {
    Instruction::new(Op::DefGlobal, [])
}
pub fn instruction_get_global() -> Instruction<0> {
    Instruction::new(Op::GetGlobal, [])
}
pub fn instruction_constant(n: u8) -> Instruction<1> {
    Instruction::new(Op::Constant, [n])
}
pub fn instruction_constant_long(n: u16) -> Instruction<2> {
    Instruction::new(
        Op::ConstantLong,
        [(n / (u8::MAX as u16)) as u8, (n % (u8::MAX as u16)) as u8],
    )
}
pub fn instruction_return() -> Instruction<0> {
    Instruction::new(Op::Return, [])
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
    pub fn get_constant_n(&self) -> usize {
        match self {
            Self::Op1(ins) => ins.get_constant_n(),
            Self::Op2(ins) => ins.get_constant_n(),
            _ => panic!("not a constant"),
        }
    }
    pub fn read(code: &[u8], pos: usize) -> (Self, usize) {
        use AnyInstruction::*;
        let opcode = code[pos];
        let op = Op::from_byte(opcode);
        match op {
            Op::Add => (Op1(instruction_add(code[pos + 1])), pos + 2),
            Op::Sub => (Op1(instruction_sub(code[pos + 1])), pos + 2),
            Op::Mul => (Op1(instruction_mul(code[pos + 1])), pos + 2),
            Op::Div => (Op1(instruction_div(code[pos + 1])), pos + 2),
            Op::NumEq => (Op0(instruction_num_eq()), pos + 1),
            Op::NumNeq => (Op0(instruction_num_neq()), pos + 1),
            Op::NumLt => (Op0(instruction_num_lt()), pos + 1),
            Op::NumLte => (Op0(instruction_num_lte()), pos + 1),
            Op::NumGt => (Op0(instruction_num_gt()), pos + 1),
            Op::NumGte => (Op0(instruction_num_gte()), pos + 1),
            Op::Pop => (Op0(instruction_pop()), pos + 1),
            Op::DefGlobal => (Op0(instruction_def_global()), pos + 1),
            Op::GetGlobal => (Op0(instruction_def_global()), pos + 1),
            Op::Constant => (Op1(instruction_constant(code[pos + 1])), pos + 2),
            Op::ConstantLong => (
                Op2(instruction_constant_long(
                    (u8::MAX as u16 * code[pos + 1] as u16) + (code[pos + 2] as u16),
                )),
                pos + 4,
            ),
            Op::Return => (Op0(instruction_return()), pos + 1),
        }
    }
}
