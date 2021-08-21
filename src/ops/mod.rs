pub mod math;

#[macro_export]
macro_rules! vararg_op {
    ($name:ident, $args:ident, $b:block) => {
        #[inline(always)]
        pub fn $name(stack: &mut $crate::vm::Stack, nargs: u8) -> Result<()> {
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
        pub fn $name(stack: &mut $crate::vm::Stack) -> Result<()> {
            let $y = stack.pop().unwrap();
            let $x = stack.pop().unwrap();
            stack.push($b);
            Ok(())
        }
    };
}
