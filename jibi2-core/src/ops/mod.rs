use crate::object::Object;

pub mod math;

pub use math::*;

#[macro_export]
macro_rules! vararg_op {
    ($name:ident, $nargs:ident, $stack:ident $b:block) => {
        #[inline(always)]
        pub fn $name($stack: &mut $crate::vm::Stack, $nargs: usize) -> crate::error::Result<()> {
            $b;
            Ok(())
        }
    };
}

#[macro_export]
macro_rules! binary_op {
    ($name:ident, $x:ident, $y:ident, $b:block) => {
        #[inline(always)]
        pub fn $name(stack: &mut $crate::vm::Stack) -> $crate::error::Result<()> {
            let $y = stack.pop();
            let $x = stack.pop();
            stack.push($b);
            Ok(())
        }
    };
}

binary_op!(op_equal, x, y, { Object::Bool(x == y) });