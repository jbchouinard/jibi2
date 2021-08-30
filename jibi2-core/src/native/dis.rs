use std::rc::Rc;

use crate::error::Result;
use crate::native::expect_n_args;
use crate::object::Object;
use crate::vm::VM;

fn native_dis(argv: &[Object], argc: usize) -> Result<Object> {
    expect_n_args(argc, 1)?;
    let clos = argv[0].as_closure()?;
    let name = Rc::clone(&clos.borrow().function.name);
    clos.borrow().function.code.disassemble(&name, 0);
    Ok(Object::Nil)
}

pub fn add_native_functions(vm: &mut VM) {
    vm.define_native("dis", Rc::new(native_dis));
}
