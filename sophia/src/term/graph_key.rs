/// RDF [datasets](../../dataset/index.html) contain multiple [graphs](../../graph/index.html),
/// the default graph, and zero or more named graphs indexed by [terms](../../term/index.html).
/// The [`GraphKey`](enum.GraphKey.html) type is a wrapper around
/// [`Term`](../../term/struct.Term.html) to identify a graph in a dataset.

use std::borrow::Borrow;
use std::hash::Hash;

use crate::term::Term;

/// A graph key is the fourth component of a [`Quad`](./trait.Quad.html).
/// It identifies the graph of a dataset to which this quad belongs.
#[derive(Clone, Debug, Eq, Hash)]
pub enum GraphKey<T>
where
    T: Borrow<str> + Clone + Eq + Hash,
{
    /// The default graph of the [`dataset`](../dataset/index.html).
    Default,
    /// A named graph of the [`dataset`](../dataset/index.html).
    Name(Term<T>),
}


impl<T> GraphKey<T>
where
    T: Borrow<str> + Clone + Eq + Hash,
{
    pub fn name(&self) -> Option<&Term<T>> {
        match self {
            GraphKey::Default => None,
            GraphKey::Name(t) => Some(t),
        }
    }

    pub fn in_default_graph(&self) -> bool {
        match self {
            GraphKey::Default => true,
            _ => false,
        }
    }
}

impl<'a, T, U> From<&'a Term<U>> for GraphKey<T> where
        T: Borrow<str> + Clone + Eq + Hash + From<&'a str>,
        U: Borrow<str> + Clone + Eq + Hash,
{
    fn from(other: &'a Term<U>) -> GraphKey<T> {
        GraphKey::Name(other.into())
    }
}

impl<'a, T, U> From<&'a GraphKey<U>> for GraphKey<T> where
        T: Borrow<str> + Clone + Eq + Hash + From<&'a str>,
        U: Borrow<str> + Clone + Eq + Hash,
{
    fn from(other: &'a GraphKey<U>) -> GraphKey<T> {
        match other {
            GraphKey::Default => GraphKey::Default,
            GraphKey::Name(other) => GraphKey::Name(other.into()),
        }
    }
}

impl<T, U> PartialEq<GraphKey<U>> for GraphKey<T> where
    T: Borrow<str> + Clone + Eq + Hash,
    U: Borrow<str> + Clone + Eq + Hash,
{
    fn eq(&self, other: &GraphKey<U>) -> bool {
        match (self, other) {
            (GraphKey::Default, GraphKey::Default) => true,
            (GraphKey::Name(t1), GraphKey::Name(t2)) => t1 == t2,
            _ => false,
        }
    }
}

impl<T, U> PartialEq<Term<U>> for GraphKey<T> where
    T: Borrow<str> + Clone + Eq + Hash,
    U: Borrow<str> + Clone + Eq + Hash,
{
    fn eq(&self, other: &Term<U>) -> bool {
        match self {
            GraphKey::Default => false,
            GraphKey::Name(t) => t == other,
        }
    }
}

impl<T, U> PartialEq<GraphKey<U>> for Term<T> where
    T: Borrow<str> + Clone + Eq + Hash,
    U: Borrow<str> + Clone + Eq + Hash,
{
    fn eq(&self, other: &GraphKey<U>) -> bool {
        match other {
            GraphKey::Default => false,
            GraphKey::Name(t) => t == self,
        }
    }
}
