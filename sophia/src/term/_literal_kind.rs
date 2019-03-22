// this module is transparently re-exported by its parent `term`

use std::hash::{Hash, Hasher};

use super::*;

/// There are two kinds of literals: language-tagged, and typed.
#[derive(Clone, Copy, Debug, Eq)]
pub enum LiteralKind<T: AsRef<str>> {
    Lang(T),
    Datatype(IriData<T>),
}
pub use self::LiteralKind::*;

impl<T> LiteralKind<T>
where
    T: AsRef<str>,
{
    /// Copy another literal kind with the given factory.
    pub fn from_with<'a, U, F>(other: &'a LiteralKind<U>, mut factory: F) -> LiteralKind<T>
    where
        U: AsRef<str>,
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
        U: AsRef<str>,
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
    T: AsRef<str>,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Lang(tag) => tag.as_ref().hash(state),
            Datatype(iri) => iri.hash(state),
        }
    }
}

impl<T, U> PartialEq<LiteralKind<U>> for LiteralKind<T>
where
    T: AsRef<str>,
    U: AsRef<str>,
{
    fn eq(&self, other: &LiteralKind<U>) -> bool {
        match (self, other) {
            (Lang(tag1), Lang(tag2)) => tag1.as_ref() == tag2.as_ref(),
            (Datatype(iri1), Datatype(iri2)) => iri1 == iri2,
            _ => false,
        }
    }
}

#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially the ::term::test module).
}
