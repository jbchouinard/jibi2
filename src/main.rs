use jibi2::chunk::{Chunk, OpCode};

fn main() {
    let mut chunk = Chunk::new();
    chunk.write(OpCode::Return);
    chunk.disassemble("test chunk");
}
