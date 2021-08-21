pub struct Operand(u8);

#[derive(Debug, PartialEq, Clone)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
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

pub struct Ins<const N: usize> {
    pub op: Op,
    pub operands: [u8; N],
}

impl<const N: usize> Ins<N> {
    pub fn new(op: Op) -> Self {
        Self {
            op,
            operands: [0; N],
        }
    }
}

pub enum Instruction {
    Op0(Ins<0>),
    Op1(Ins<1>),
    Op2(Ins<2>),
}

impl Instruction {
    pub fn from_op(op: Op) -> Self {
        match op {
            Op::Add => Self::Op1(Ins::new(Op::Add)),
            Op::Sub => Self::Op1(Ins::new(Op::Sub)),
            Op::Mul => Self::Op1(Ins::new(Op::Mul)),
            Op::Div => Self::Op1(Ins::new(Op::Div)),
            Op::Pop => Self::Op0(Ins::new(Op::Pop)),
            Op::DefGlobal => Self::Op0(Ins::new(Op::DefGlobal)),
            Op::GetGlobal => Self::Op0(Ins::new(Op::GetGlobal)),
            Op::Return => Self::Op0(Ins::new(Op::Return)),
            Op::Constant => Self::Op1(Ins::new(Op::Constant)),
            Op::ConstantLong => Self::Op2(Ins::new(Op::ConstantLong)),
        }
    }
    pub fn op(&self) -> Op {
        match self {
            Self::Op0(ins) => ins.op.clone(),
            Self::Op1(ins) => ins.op.clone(),
            Self::Op2(ins) => ins.op.clone(),
        }
    }
    pub fn get_operand(&self, n: usize) -> u8 {
        match self {
            Self::Op0(ins) => ins.operands[n],
            Self::Op1(ins) => ins.operands[n],
            Self::Op2(ins) => ins.operands[n],
        }
    }
    pub fn set_operand(&mut self, n: usize, val: u8) {
        match self {
            Self::Op0(ins) => ins.operands[n] = val,
            Self::Op1(ins) => ins.operands[n] = val,
            Self::Op2(ins) => ins.operands[n] = val,
        }
    }
    pub fn with_operand(mut self, n: usize, val: u8) -> Self {
        self.set_operand(n, val);
        self
    }
    pub fn size(&self) -> usize {
        match self {
            Self::Op0(_) => 1,
            Self::Op1(_) => 2,
            Self::Op2(_) => 3,
        }
    }
    pub fn op_return() -> Self {
        Self::from_op(Op::Return)
    }
    pub fn op_defglobal() -> Self {
        Self::from_op(Op::DefGlobal)
    }
    pub fn op_getglobal() -> Self {
        Self::from_op(Op::GetGlobal)
    }
    pub fn op_pop() -> Self {
        Self::from_op(Op::Pop)
    }
    pub fn op_add(n: u8) -> Self {
        Self::from_op(Op::Add).with_operand(0, n)
    }
    pub fn op_sub(n: u8) -> Self {
        Self::from_op(Op::Sub).with_operand(0, n)
    }
    pub fn op_mul(n: u8) -> Self {
        Self::from_op(Op::Mul).with_operand(0, n)
    }
    pub fn op_div(n: u8) -> Self {
        Self::from_op(Op::Div).with_operand(0, n)
    }
    pub fn op_constant(n: usize) -> Self {
        if n <= u8::MAX as usize {
            Self::from_op(Op::Constant).with_operand(0, n as u8)
        } else if n <= u16::MAX as usize {
            Self::from_op(Op::ConstantLong)
                .with_operand(0, (n / (u8::MAX as usize)) as u8)
                .with_operand(1, (n % (u8::MAX as usize)) as u8)
        } else {
            panic!("too many constants");
        }
    }
    pub fn get_constant_n(&self) -> usize {
        match self.op() {
            Op::Constant => self.get_operand(0) as usize,
            Op::ConstantLong => {
                (u8::MAX as usize) * (self.get_operand(0) as usize) + (self.get_operand(1) as usize)
            }
            _ => panic!("not a constant"),
        }
    }
}
