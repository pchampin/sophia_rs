// this module is transparently re-exported by its parent `term`

use super::*;
use std::hash::{Hash, Hasher};

/// There are two kinds of literals: language-tagged, and typed.
#[derive(Clone, Copy, Debug, Eq)]
pub enum LiteralKind<T: TermData> {
    Lang(T),
    Datatype(Iri<T>),
}
pub use self::LiteralKind::*;

impl<T> LiteralKind<T>
where
    T: TermData,
{
    /// Copy another literal kind with the given factory.
    pub fn from_with<'a, U, F>(other: &'a LiteralKind<U>, mut factory: F) -> Self
    where
        U: TermData,
        F: FnMut(&'a str) -> T,
    {
        match other {
            Lang(tag) => Lang(factory(tag.as_ref())),
            Datatype(iri) => Datatype(iri.clone_with(factory)),
        }
    }

    /// If the literal is typed transform the IRI according to the given
    /// policy.
    ///
    /// If the policy already applies or it is language tagged the literal is
    /// returned unchanged.
    pub fn clone_normalized_with<F, U>(&self, policy: Normalization, factory: F) -> LiteralKind<U>
    where
        F: FnMut(&str) -> U,
        U: TermData,
    {
        match self {
            Lang(_) => LiteralKind::from_with(&self, factory),
            Datatype(iri) => Datatype(iri.clone_normalized_with(policy, factory)),
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

#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially the ::term::test module).
}
