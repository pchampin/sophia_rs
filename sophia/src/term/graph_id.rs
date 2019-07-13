//! A graph identifier is the fourth component of a quad.
//! It identifies the graph of a dataset to which this quad belongs.

use crate::term::{Term, TermData};

/// A wrapper around a [`Term`] to identify a [`Graph`] in a [`Dataset`].
///
/// Used as the fourth component of a [`Quad`].
///
/// [`Term`]: ../enum.Term.html
/// [`Graph`]: ../../graph/trait.Graph.html
/// [`Dataset`]: ../../dataset/trait.Dataset.html
/// [`Quad`]: ../../quad/trait.Quad.html
#[derive(Clone, Debug, Eq, Hash)]
pub enum GraphId<T>
where
    T: TermData,
{
    /// Identifies the default graph of the [`dataset`](../../dataset/index.html).
    Default,
    /// Identifies a named graph of the [`dataset`](../../dataset/index.html).
    Name(Term<T>),
}

impl<T> GraphId<T>
where
    T: TermData,
{
    pub fn name(&self) -> Option<&Term<T>> {
        match self {
            GraphId::Default => None,
            GraphId::Name(t) => Some(t),
        }
    }

    pub fn in_default_graph(&self) -> bool {
        match self {
            GraphId::Default => true,
            _ => false,
        }
    }
}

impl<'a, T, U> From<&'a Term<U>> for GraphId<T>
where
    T: TermData + From<&'a str>,
    U: TermData,
{
    fn from(other: &'a Term<U>) -> GraphId<T> {
        GraphId::Name(other.into())
    }
}

impl<'a, T, U> From<&'a GraphId<U>> for GraphId<T>
where
    T: TermData + From<&'a str>,
    U: TermData,
{
    fn from(other: &'a GraphId<U>) -> GraphId<T> {
        match other {
            GraphId::Default => GraphId::Default,
            GraphId::Name(other) => GraphId::Name(other.into()),
        }
    }
}

impl<T, U> PartialEq<GraphId<U>> for GraphId<T>
where
    T: TermData,
    U: TermData,
{
    fn eq(&self, other: &GraphId<U>) -> bool {
        match (self, other) {
            (GraphId::Default, GraphId::Default) => true,
            (GraphId::Name(t1), GraphId::Name(t2)) => t1 == t2,
            _ => false,
        }
    }
}

impl<T, U> PartialEq<Term<U>> for GraphId<T>
where
    T: TermData,
    U: TermData,
{
    fn eq(&self, other: &Term<U>) -> bool {
        match self {
            GraphId::Default => false,
            GraphId::Name(t) => t == other,
        }
    }
}

impl<T, U> PartialEq<GraphId<U>> for Term<T>
where
    T: TermData,
    U: TermData,
{
    fn eq(&self, other: &GraphId<U>) -> bool {
        match other {
            GraphId::Default => false,
            GraphId::Name(t) => t == self,
        }
    }
}
