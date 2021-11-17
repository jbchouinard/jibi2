use std::rc::Rc;

use crate::native_fn;
use crate::object::Object;
use crate::vm::VM;

native_fn!(native_dis, obj, {
    let clos = obj.as_closure()?;
    let name = Rc::clone(&clos.function.name);
    clos.function.code.disassemble(&name, 0);
    Object::Nil
});

pub fn add_native_functions(vm: &mut VM) {
    vm.define_native("dis", Rc::new(native_dis));
}
