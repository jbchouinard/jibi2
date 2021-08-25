use std::convert::TryInto;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::error::Result;
use crate::object::Object;
use crate::vm::{Stack, VM};

fn native_now(nargs: usize, stack: &mut Stack) -> Result<()> {
    for _ in 0..nargs {
        stack.pop();
    }
    let now = SystemTime::now();
    let ts = now.duration_since(UNIX_EPOCH).unwrap();
    stack.push(Object::Int(ts.as_millis().try_into().unwrap()));
    Ok(())
}

pub fn add_native_functions(vm: &mut VM) {
    vm.define_native("now", Rc::new(native_now));
}
