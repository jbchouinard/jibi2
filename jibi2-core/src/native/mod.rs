use crate::error::{ArgumentError, Result};
use crate::vm::VM;

pub mod base;
pub mod dis;
pub mod math;
pub mod time;

pub fn add_native_functions(vm: &mut VM) {
    base::add_native_functions(vm);
    math::add_native_functions(vm);
    time::add_native_functions(vm);
    dis::add_native_functions(vm);
}

#[macro_export]
macro_rules! native_fn_varia {
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
macro_rules! native_fn {
    ($name:ident, $body:block) => {
        pub fn $name(
            _: &[$crate::object::Object],
            argc: usize,
        ) -> $crate::error::Result<$crate::object::Object> {
            $crate::native::expect_n_args(argc, 0)?;
            Ok($body)
        }
    };
    ($name:ident, $x:ident, $body:block) => {
        pub fn $name(
            argv: &[$crate::object::Object],
            argc: usize,
        ) -> $crate::error::Result<$crate::object::Object> {
            $crate::native::expect_n_args(argc, 1)?;
            let $x = &argv[0];
            Ok($body)
        }
    };
    ($name:ident, $x:ident, $y:ident, $body:block) => {
        pub fn $name(
            argv: &[$crate::object::Object],
            argc: usize,
        ) -> $crate::error::Result<$crate::object::Object> {
            $crate::native::expect_n_args(argc, 2)?;
            let $x = &argv[0];
            let $y = &argv[1];
            Ok($body)
        }
    };
    ($name:ident, $x:ident, $y:ident, $z:ident, $body:block) => {
        pub fn $name(
            argv: &[$crate::object::Object],
            argc: usize,
        ) -> $crate::error::Result<$crate::object::Object> {
            $crate::native::expect_n_args(argc, 3)?;
            let $x = &argv[0];
            let $y = &argv[1];
            let $z = &argv[2];
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
