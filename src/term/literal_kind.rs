// this module is transparently re-exported by its parent `term`

use super::*;

#[derive(Clone,Debug,Eq,Hash)]
pub enum LiteralKind<T: Borrow<str>> {
    Lang(T),
    Datatype(IriTerm<T>),
}
pub use self::LiteralKind::*;

impl<T> LiteralKind<T> where
    T: Borrow<str>
{
    pub fn copy_with<'a, U, F> (other: &'a LiteralKind<U>, factory: &mut F) -> LiteralKind<T> where
        U: Borrow<str>,
        F: FnMut(&'a str) -> T,
    {
        match other {
            Lang(tag) => Lang(factory(tag.borrow())),
            Datatype(iri) => Datatype(IriTerm::copy_with(iri, factory)),
        }
    }

    pub fn copy<'a, U> (other: &'a LiteralKind<U>) -> LiteralKind<T> where
        T: From<&'a str>,
        U: Borrow<str>,
    {
        Self::copy_with(other, &mut T::from)
    }
}

impl<T,U> PartialEq<LiteralKind<U>> for LiteralKind<T> where
    T: Borrow<str>,
    U: Borrow<str>,
{
    fn eq(&self, other: &LiteralKind<U>) -> bool {
        match (self, other) {
            (Lang(tag1), Lang(tag2)) => tag1.borrow() == tag2.borrow(),
            (Datatype(iri1), Datatype(iri2)) => iri1 == iri2,
            _ => false,
        }
    }
}
