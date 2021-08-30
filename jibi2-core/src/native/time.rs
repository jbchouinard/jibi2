use std::convert::TryInto;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::native_fn;
use crate::object::Object;
use crate::vm::VM;

native_fn!(native_now, {
    Object::Int(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_millis()
            .try_into()
            .unwrap(),
    )
});

pub fn add_native_functions(vm: &mut VM) {
    vm.define_native("now", Rc::new(native_now));
}
