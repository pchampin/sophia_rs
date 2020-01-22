// this module is transparently re-exported by its parent `term`

use std::fmt;
use std::hash::{Hash, Hasher};

use crate::term::{IriData, Normalization, TermData};

/// There are two kinds of literals: language-tagged, and typed.
#[derive(Clone, Copy, Debug, Eq)]
pub enum LiteralKind<T: TermData> {
    Lang(T),
    Datatype(IriData<T>),
}
pub use self::LiteralKind::*;

impl<T> LiteralKind<T>
where
    T: TermData,
{
    /// Copy another literal kind with the given factory.
    pub fn from_with<'a, U, F>(other: &'a LiteralKind<U>, mut factory: F) -> LiteralKind<T>
    where
        U: TermData,
        F: FnMut(&'a str) -> T,
    {
        match other {
            Lang(tag) => Lang(factory(tag.as_ref())),
            Datatype(iri) => Datatype(IriData::from_with(iri, factory)),
        }
    }

    /// Copy another literal kind with the given factory,
    /// applying the given normalization policy.
    pub fn normalized_with<U, F>(
        other: &'_ LiteralKind<U>,
        mut factory: F,
        norm: Normalization,
    ) -> LiteralKind<T>
    where
        U: TermData,
        F: FnMut(&str) -> T,
    {
        match other {
            Lang(tag) => Lang(factory(tag.as_ref())),
            Datatype(iri) => Datatype(IriData::normalized_with(iri, factory, norm)),
        }
    }
}

impl<T> Hash for LiteralKind<T>
where
    T: TermData,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Datatype(iri) => iri.hash(state),
            Lang(tag) => tag.as_ref().to_lowercase().hash(state),
        }
    }
}

impl<T, U> PartialEq<LiteralKind<U>> for LiteralKind<T>
where
    T: TermData,
    U: TermData,
{
    fn eq(&self, other: &LiteralKind<U>) -> bool {
        match (self, other) {
            (Lang(tag1), Lang(tag2)) => tag1.as_ref().eq_ignore_ascii_case(tag2.as_ref()),
            (Datatype(iri1), Datatype(iri2)) => iri1 == iri2,
            _ => false,
        }
    }
}

impl<T> fmt::Display for LiteralKind<T>
where
    T: TermData,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Lang(tag) => write!(f, "@{}", tag.as_ref()),
            Datatype(iri) => write!(f, "^^{}", iri),
        }
    }
}

#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially the ::term::test module).
}
