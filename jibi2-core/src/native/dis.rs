use std::rc::Rc;

use crate::error::{ArgumentError, Result};
use crate::object::Object;
use crate::vm::{Stack, VM};

fn native_dis(nargs: usize, stack: &mut Stack) -> Result<()> {
    if nargs != 1 {
        return Err(ArgumentError::new("expected 1 argument".to_string()).into());
    }
    let clos = stack.pop().as_closure()?;
    let name = Rc::clone(&clos.borrow().function.name);
    clos.borrow().function.code.disassemble(&name, 0);
    stack.push(Object::Nil);
    Ok(())
}

pub fn add_native_functions(vm: &mut VM) {
    vm.define_native("dis", Rc::new(native_dis));
}
