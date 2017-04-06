use lift::*;

pub enum Xor<L, R> {
    Left(L),
    Right(R)
}

lift2right!(Xor);


pub enum Ior<L, R> {
    Left(L),
    Right(R),
    Both(L,R)
}

lift2right!(Ior);

pub enum Lazy<A> {
    Lazy(Box<Fn() -> A>)
}

impl<A> Lazy<A> {
    pub fn apply(&self) -> A {
        match *self {
            Lazy::Lazy(ref f) => f()
        }
    }
}
