pub mod math;

#[macro_export]
macro_rules! unary_op {
    ($name:ident, $x:ident, $b:block) => {
        #[inline(always)]
        pub fn $name(stack: &mut $crate::vm::Stack) -> Result<()> {
            let $x = stack.pop().unwrap();
            let res = $b;
            stack.push(res);
            Ok(())
        }
    };
}

#[macro_export]
macro_rules! binary_op {
    ($name:ident, $x:ident, $y:ident, $b:block) => {
        #[inline(always)]
        pub fn $name(stack: &mut $crate::vm::Stack) -> Result<()> {
            let $y = stack.pop().unwrap();
            let $x = stack.pop().unwrap();
            let res = $b;
            stack.push(res);
            Ok(())
        }
    };
}
