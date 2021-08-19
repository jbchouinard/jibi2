use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum OpCode {
    Return = 0,
}

impl OpCode {
    pub fn as_byte(self) -> u8 {
        self as u8
    }
    pub fn from_byte(n: u8) -> Self {
        match n {
            0 => Self::Return,
            _ => panic!("invalid opcode {}", n),
        }
    }
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                OpCode::Return => "OP_RETURN",
            }
        )
    }
}

pub struct Chunk {
    pub code: Vec<OpCode>,
}

impl Chunk {
    pub fn new() -> Self {
        Self { code: vec![] }
    }

    pub fn count(&self) -> usize {
        self.code.len()
    }

    pub fn write(&mut self, op: OpCode) {
        self.code.push(op)
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        self.code.iter().map(|c| c.clone().as_byte()).collect()
    }
}

impl fmt::Display for Chunk {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for op in self.code.iter() {
            writeln!(f, "{}", op)?
        }
        Ok(())
    }
}
