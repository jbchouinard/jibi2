use crate::value::Value;

pub mod math;

pub use math::*;

#[macro_export]
macro_rules! vararg_op {
    ($name:ident, $args:ident, $b:block) => {
        #[inline(always)]
        pub fn $name(stack: &mut $crate::vm::Stack, nargs: usize) -> crate::error::Result<()> {
            let mut $args: Vec<Value> = vec![];
            for _ in 0..nargs {
                $args.push(stack.pop().unwrap());
            }
            stack.push($b);
            Ok(())
        }
    };
}

#[macro_export]
macro_rules! binary_op {
    ($name:ident, $x:ident, $y:ident, $b:block) => {
        #[inline(always)]
        pub fn $name(stack: &mut $crate::vm::Stack) -> $crate::error::Result<()> {
            let $y = stack.pop().unwrap();
            let $x = stack.pop().unwrap();
            stack.push($b);
            Ok(())
        }
    };
}

binary_op!(op_equal, x, y, { Value::Bool(x == y) });
