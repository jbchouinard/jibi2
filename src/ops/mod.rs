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
            let res = $b;
            stack.push(res);
            Ok(())
        }
    };
}
