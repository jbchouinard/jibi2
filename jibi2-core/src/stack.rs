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
    pub fn popfree(&mut self) {
        self.size -= 1;
        self.arr[self.size] = MaybeUninit::uninit();
    }
    pub fn maybe_pop(&mut self) -> Option<T> {
        if self.size == 0 {
            None
        } else {
            Some(self.pop())
        }
    }
    pub fn pop_n(&mut self, n: usize) -> Vec<T> {
        let mut v = vec![];
        for _ in 0..n {
            v.push(self.pop());
        }
        v
    }
    pub fn popfree_n(&mut self, n: usize) {
        self.size -= n;
        for i in 0..n {
            self.arr[self.size + i] = MaybeUninit::uninit();
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
