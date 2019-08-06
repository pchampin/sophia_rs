//! An RDF triple expresses a single fact.
//! Its formed of three terms called *subject*, *predicate* and *object*.
//!
//! You can think of a triple as a sentence of the form
//! "subject verb complement"
//! (although the *predicate* is often better expressed as a relationship than a verb).
//! Examples :
//!
//! * John is a person.
//! * John was born in Paris.
//! * John knows Jane.
//! * John's family name is "Doe".
//!

use crate::quad::Quad;
use crate::term::*;

pub mod stream;

/// This trait represents an abstract RDF triple,
/// and provide convenient methods for working with triples.
pub trait Triple<'a> {
    type TermData: TermData + 'a;
    /// The subject of this triple.
    fn s(&self) -> &Term<Self::TermData>;
    /// The predicate of this triple.
    fn p(&self) -> &Term<Self::TermData>;
    /// The object of this triple.
    fn o(&self) -> &Term<Self::TermData>;

    /// [`Quad`](../quad/trait.Quad.html) adapter owning this triple,
    /// pretending to belong to the default graph.
    fn as_quad(self) -> TripleAsQuad<Self>
    where
        Self: Sized,
    {
        TripleAsQuad(self)
    }
    /// [`Quad`](../quad/trait.Quad.html) adapter owning this triple,
    /// pretending to belong to a named graph with the given name.
    fn as_quad_from(self, name: Term<Self::TermData>) -> TripleAsQuadFrom<'a, Self>
    where
        Self: Sized,
    {
        TripleAsQuadFrom(self, name)
    }
}

impl<'a, T> Triple<'a> for [Term<T>; 3]
where
    T: TermData + 'a,
{
    type TermData = T;
    #[inline]
    fn s(&self) -> &Term<T> {
        &self[0]
    }
    #[inline]
    fn p(&self) -> &Term<T> {
        &self[1]
    }
    #[inline]
    fn o(&self) -> &Term<T> {
        &self[2]
    }
}

impl<'a, T> Triple<'a> for [&'a Term<T>; 3]
where
    T: TermData + 'a,
{
    type TermData = T;
    #[inline]
    fn s(&self) -> &Term<T> {
        self[0]
    }
    #[inline]
    fn p(&self) -> &Term<T> {
        self[1]
    }
    #[inline]
    fn o(&self) -> &Term<T> {
        self[2]
    }
}

impl<'a, T: Triple<'a>> Triple<'a> for &'a T {
    type TermData = T::TermData;
    #[inline]
    fn s(&self) -> &Term<T::TermData> {
        (*self).s()
    }
    #[inline]
    fn p(&self) -> &Term<T::TermData> {
        (*self).p()
    }
    #[inline]
    fn o(&self) -> &Term<T::TermData> {
        (*self).o()
    }
}

/// The adapter returned by [`Triple::as_quad`](./trait.Triple.html#method.as_quad).
pub struct TripleAsQuad<T>(T);

impl<T> TripleAsQuad<T> {
    /// Unwrap this adapter to get the original triple back.
    pub fn unwrap(self) -> T {
        self.0
    }
}

impl<'a, T: Triple<'a>> Quad<'a> for TripleAsQuad<T> {
    type TermData = T::TermData;
    #[inline]
    fn s(&self) -> &Term<T::TermData> {
        self.0.s()
    }
    #[inline]
    fn p(&self) -> &Term<T::TermData> {
        self.0.p()
    }
    #[inline]
    fn o(&self) -> &Term<T::TermData> {
        self.0.o()
    }
    #[inline]
    fn g(&self) -> &GraphName<T::TermData> {
        &None
    }
}

/// The adapter returned by [`Triple::as_quad_from`](./trait.Triple.html#method.as_quad_from).
pub struct TripleAsQuadFrom<'a, T: Triple<'a>>(T, Term<T::TermData>);

impl<'a, T: Triple<'a>> TripleAsQuadFrom<'a, T> {
    /// Unwrap this adapter to get the original triple back.
    pub fn unwrap(self) -> T {
        self.0
    }
}

impl<'a, T: Triple<'a>> Quad<'a> for TripleAsQuadFrom<'a, T> {
    type TermData = T::TermData;
    #[inline]
    fn s(&self) -> &Term<T::TermData> {
        self.0.s()
    }
    #[inline]
    fn p(&self) -> &Term<T::TermData> {
        self.0.p()
    }
    #[inline]
    fn o(&self) -> &Term<T::TermData> {
        self.0.o()
    }
    #[inline]
    fn g(&self) -> &GraphName<T::TermData> {
        self.1.as_graph_id()
    }
}

#[cfg(test)]
mod test {
    // Nothing really worth testing here
}
