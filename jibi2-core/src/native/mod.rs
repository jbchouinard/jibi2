use std::rc::Rc;

use crate::error::{ArgumentError, Result};
use crate::object::Object;
use crate::vm::VM;

pub mod dis;
pub mod math;
pub mod time;

pub fn add_native_functions(vm: &mut VM) {
    vm.define_native("equal?", Rc::new(native_equal));
    time::add_native_functions(vm);
    dis::add_native_functions(vm);
    math::add_native_functions(vm);
}

#[macro_export]
macro_rules! native_fn {
    ($name:ident, $argv:ident, $argc:ident, $body:block) => {
        pub fn $name(
            $argv: &[$crate::object::Object],
            $argc: usize,
        ) -> $crate::error::Result<$crate::object::Object> {
            $body
        }
    };
}

#[macro_export]
macro_rules! binary_native_fn {
    ($name:ident, $x:ident, $y:ident, $body:block) => {
        pub fn $name(
            argv: &[$crate::object::Object],
            argc: usize,
        ) -> $crate::error::Result<$crate::object::Object> {
            expect_n_args(argc, 2)?;
            let $x = &argv[0];
            let $y = &argv[1];
            Ok($body)
        }
    };
}

pub fn expect_n_args(actual: usize, expected: usize) -> Result<()> {
    if actual == expected {
        Ok(())
    } else {
        Err(ArgumentError::new(format!("expected {} argument, got {}", expected, actual)).into())
    }
}

binary_native_fn!(native_equal, x, y, { Object::Bool(x == y) });
