use std::rc::Rc;

use crate::native_fn;
use crate::object::Object;
use crate::vm::VM;

native_fn!(native_equal, x, y, { Object::Bool(x == y) });

native_fn!(native_print, obj, {
    let s = match obj {
        Object::String(s) => s.to_string(),
        _ => obj.to_string(),
    };
    println!("{}", s);
    Object::Nil
});

native_fn!(native_repr, obj, {
    Object::String(Rc::new(obj.to_string()))
});

pub fn add_native_functions(vm: &mut VM) {
    vm.define_native("equal?", Rc::new(native_equal));
    vm.define_native("print", Rc::new(native_print));
    vm.define_native("repr", Rc::new(native_repr));
}
