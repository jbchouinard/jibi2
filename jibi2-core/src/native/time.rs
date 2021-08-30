use std::convert::TryInto;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::error::Result;
use crate::native::expect_n_args;
use crate::object::Object;
use crate::vm::VM;

fn native_now(_argv: &[Object], argc: usize) -> Result<Object> {
    expect_n_args(argc, 0)?;
    let now = SystemTime::now();
    let ts = now.duration_since(UNIX_EPOCH).unwrap();
    Ok(Object::Int(ts.as_millis().try_into().unwrap()))
}

pub fn add_native_functions(vm: &mut VM) {
    vm.define_native("now", Rc::new(native_now));
}
