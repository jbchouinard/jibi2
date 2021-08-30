use std::mem::MaybeUninit;

pub struct ArrayStack<T, const N: usize> {
    pub size: usize,
    arr: [MaybeUninit<T>; N],
}

impl<T, const N: usize> ArrayStack<T, N> {
    pub fn new() -> Self {
        Self {
            size: 0,
            arr: unsafe { MaybeUninit::uninit().assume_init() },
        }
    }
    pub fn push(&mut self, val: T) {
        unsafe { self.arr[self.size].as_mut_ptr().write(val) };
        self.size += 1;
    }
    pub fn pop(&mut self) -> T {
        self.size -= 1;
        let val = std::mem::replace(&mut self.arr[self.size], MaybeUninit::uninit());
        unsafe { val.assume_init() }
    }
    pub fn pop_n(&mut self, n: usize) {
        for _ in 0..n {
            self.pop();
        }
    }
    pub fn peek_ref(&self, from_top: usize) -> &T {
        let n = self.size - from_top - 1;
        unsafe { self.arr[n].assume_init_ref() }
    }
    pub fn peek_mut(&mut self, from_top: usize) -> &mut T {
        let n = self.size - from_top - 1;
        unsafe { self.arr[n].assume_init_mut() }
    }
    pub fn peek_slice(&self, n: usize) -> &[T] {
        if n > self.size {
            panic!("out of bounds stack read");
        }
        unsafe { MaybeUninit::slice_assume_init_ref(&self.arr[self.size - n..self.size]) }
    }

    pub fn get_ref(&self, n: usize) -> &T {
        if n >= self.size {
            panic!("out of bounds stack read");
        }
        unsafe { self.arr[n].assume_init_ref() }
    }
    pub fn get_mut(&mut self, n: usize) -> &mut T {
        if n >= self.size {
            panic!("out of bounds stack read");
        }
        unsafe { self.arr[n].assume_init_mut() }
    }
}
