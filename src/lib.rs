use std::fmt::{Display, Formatter};
use std::ops::Index;

mod lexer;
mod parser;
mod evaluator;
pub mod environment;
pub mod user_io;
pub mod errors;

pub struct Cursor {
    pub val: usize,
}

impl Display for Cursor {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&*format!("{}", self.val))
    }
}

impl Cursor {
    pub fn new() -> Cursor { Cursor { val: 0 } }
    pub fn get(&self) -> usize { self.val }
    pub fn mov(&mut self, steps: i32) { self.val += steps as usize }
    pub fn step_fwd(&mut self) { self.mov(1) }
}

pub trait HasLength { fn length(&self) -> usize; }

impl<T> HasLength for Vec<T> { fn length(&self) -> usize { self.len() } }


pub trait WalksCollection<'a, V: 'a, I: 'a>
    where V: FromIterator<I> + Index<usize, Output=I> + HasLength,
          I: Clone + PartialEq {
    fn cnt(&self) -> &Cursor;
    fn mut_cnt(&mut self) -> &mut Cursor;
    fn arr(&self) -> &V;
    fn peek(&'a self, amount: usize) -> &I { &self.arr()[self.cnt().get() + amount] }
    fn peek_back(&'a self, amount: usize) -> &I { &self.arr()[self.cnt().get() - amount] }
    fn read_prev(&'a self) -> &I { &self.arr()[self.cnt().get() - 1] }
    fn read_curr(&'a self) -> &I { self.peek(0) }
    fn read_next(&'a self) -> &I { self.peek(1) }
    fn can_peek(&'a self, amount: usize) -> bool { self.cnt().get() + amount < self.arr().length() }
    fn can_consume(&'a self) -> bool { self.can_peek(0) }
    fn consume(&'a mut self) -> &I {
        self.mut_cnt().step_fwd();
        self.read_prev()
    }
    fn try_read_curr(&'a self) -> Option<&I> {
        if self.arr().length() <= self.cnt().get() { None } else { Some(self.read_curr()) }
    }
}

#[cfg(test)]
mod tests {
    use crate::{Cursor, WalksCollection};

    struct TestVec {
        v: Vec<char>,
        c: Cursor,
    }

    impl TestVec {
        fn from_str(str: &String) -> TestVec { TestVec { v: str.chars().collect(), c: Cursor::new() } }
    }

    impl WalksCollection<'_, Vec<char>, char> for TestVec {
        fn cnt(&self) -> &Cursor {
            &self.c
        }

        fn mut_cnt(&mut self) -> &mut Cursor {
            &mut self.c
        }

        fn arr(&self) -> &Vec<char> {
            &self.v
        }
    }

    #[test]
    fn create_class() {
        let hi = String::from("hello");
        let mut tv = TestVec::from_str(&hi);
        assert_eq!(tv.can_peek(1), true);
        assert_eq!(tv.can_peek(10), false);
        assert_eq!(*tv.read_curr(), 'h');
        tv.c.mov(4);
        assert_eq!(*tv.read_curr(), 'o');
        assert_eq!(*tv.read_prev(), 'l');
    }
}
