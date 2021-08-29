use crate::vm::VM;

mod dis;
mod time;

pub fn add_native_functions(vm: &mut VM) {
    time::add_native_functions(vm);
    dis::add_native_functions(vm);
}
