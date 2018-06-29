// this module is transparently re-exported by its parent `term`

use std::hash::{Hash, Hasher};

use super::*;

#[derive(Clone,Copy,Debug,Eq)]
pub enum LiteralKind<T: Borrow<str>> {
    Lang(T),
    Datatype(IriData<T>),
}
pub use self::LiteralKind::*;

impl<T> LiteralKind<T> where
    T: Borrow<str>
{
    pub fn from_with<'a, U, F> (other: &'a LiteralKind<U>, mut factory: F) -> LiteralKind<T> where
        U: Borrow<str>,
        F: FnMut(&'a str) -> T,
    {
        match other {
            Lang(tag) => Lang(factory(tag.borrow())),
            Datatype(iri) => Datatype(IriData::from_with(iri, factory)),
        }
    }

    pub fn normalized_with<'a, U, F> (other: &'a LiteralKind<U>, mut factory: F, norm: Normalization) -> LiteralKind<T> where
        U: Borrow<str>,
        F: FnMut(&str) -> T,
    {
        match other {
            Lang(tag) => Lang(factory(tag.borrow())),
            Datatype(iri) => Datatype(IriData::normalized_with(iri, factory, norm)),
        }
    }
}

impl<T> Hash for LiteralKind<T> where
    T: Borrow<str>,
{
    fn hash<H:Hasher> (&self, state: &mut H) {
        match self {
            Lang(tag) => tag.borrow().hash(state),
            Datatype(iri) => iri.hash(state),
        }
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
