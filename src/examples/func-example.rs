extern crate frunk;
use std::ops::Mul;
use frunk::kinder::lift::Functor;

fn squares<A: Mul<Output=A> + Copy, T: Functor<A, B=A, C=T>>(xs: &T) -> T {
    xs.fmap(|&x| x * x)
}

fn main() {
    println!("{:?}", squares(&vec!(1,2,3)));
}
