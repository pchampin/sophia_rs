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
use crate::term::TTerm;

pub mod stream;
pub mod streaming_mode;

/// This trait represents an abstract RDF triple,
/// and provide convenient methods for working with triples.
pub trait Triple {
    type Term: TTerm + ?Sized;
    /// The subject of this triple.
    fn s(&self) -> &Self::Term;
    /// The predicate of this triple.
    fn p(&self) -> &Self::Term;
    /// The object of this triple.
    fn o(&self) -> &Self::Term;

    /// [`Quad`](../quad/trait.Quad.html) adapter owning this triple,
    /// pretending to belong to the default graph.
    fn wrap_as_quad(self) -> TripleAsQuad<Self>
    where
        Self: Sized,
    {
        TripleAsQuad(self)
    }

    #[deprecated(since = "0.6.3", note = "has been renamed to wrap_as_quad")]
    #[allow(clippy::wrong_self_convention)]
    fn as_quad(self) -> TripleAsQuad<Self>
    where
        Self: Sized,
        Self::Term: Sized,
    {
        self.wrap_as_quad()
    }

    /// [`Quad`](../quad/trait.Quad.html) adapter owning this triple,
    /// pretending to belong to a named graph with the given name.
    fn wrap_as_quad_from(self, name: Self::Term) -> TripleAsQuadFrom<Self>
    where
        Self: Sized,
        Self::Term: Sized,
    {
        TripleAsQuadFrom(self, name)
    }

    #[deprecated(since = "0.6.3", note = "has been renamed to wrap_as_quad_from")]
    #[allow(clippy::wrong_self_convention)]
    fn as_quad_from(self, name: Self::Term) -> TripleAsQuadFrom<Self>
    where
        Self: Sized,
        Self::Term: Sized,
    {
        self.wrap_as_quad_from(name)
    }

    /// Iterator over the components of this triple
    fn components(&self) -> TripleIter<Self> {
        TripleIter(self, 0)
    }
}

/// Iterator over the components of a triple.
pub struct TripleIter<'a, T: ?Sized>(&'a T, u8);

impl<'a, T> Iterator for TripleIter<'a, T>
where
    T: Triple + ?Sized,
{
    type Item = &'a T::Term;
    fn next(&mut self) -> Option<Self::Item> {
        self.1 += 1;
        match self.1 {
            1 => Some(self.0.s()),
            2 => Some(self.0.p()),
            3 => Some(self.0.o()),
            _ => None,
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (3, Some(3))
    }
}

impl<T> Triple for [T; 3]
where
    T: TTerm + Sized,
{
    type Term = T;
    #[inline]
    fn s(&self) -> &Self::Term {
        &self[0]
    }
    #[inline]
    fn p(&self) -> &Self::Term {
        &self[1]
    }
    #[inline]
    fn o(&self) -> &Self::Term {
        &self[2]
    }
}

impl<'a, T> Triple for (&'a T, &'a T, &'a T)
where
    T: TTerm + ?Sized,
{
    type Term = T;
    #[inline]
    fn s(&self) -> &Self::Term {
        self.0
    }
    #[inline]
    fn p(&self) -> &Self::Term {
        self.1
    }
    #[inline]
    fn o(&self) -> &Self::Term {
        self.2
    }
}

impl<'a, T: Triple> Triple for &'a T {
    type Term = T::Term;
    #[inline]
    fn s(&self) -> &Self::Term {
        (*self).s()
    }
    #[inline]
    fn p(&self) -> &Self::Term {
        (*self).p()
    }
    #[inline]
    fn o(&self) -> &Self::Term {
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

impl<T: Triple> Quad for TripleAsQuad<T> {
    type Term = T::Term;
    #[inline]
    fn s(&self) -> &Self::Term {
        self.0.s()
    }
    #[inline]
    fn p(&self) -> &Self::Term {
        self.0.p()
    }
    #[inline]
    fn o(&self) -> &Self::Term {
        self.0.o()
    }
    #[inline]
    fn g(&self) -> Option<&Self::Term> {
        None
    }
}

/// The adapter returned by [`Triple::as_quad_from`](./trait.Triple.html#method.as_quad_from).
pub struct TripleAsQuadFrom<T: Triple>(T, T::Term);

impl<T> TripleAsQuadFrom<T>
where
    T: Triple,
    T::Term: Sized,
{
    /// Unwrap this adapter to get the original triple back.
    pub fn unwrap(self) -> T {
        self.0
    }
}

impl<T> Quad for TripleAsQuadFrom<T>
where
    T: Triple,
{
    type Term = T::Term;
    #[inline]
    fn s(&self) -> &Self::Term {
        self.0.s()
    }
    #[inline]
    fn p(&self) -> &Self::Term {
        self.0.p()
    }
    #[inline]
    fn o(&self) -> &Self::Term {
        self.0.o()
    }
    #[inline]
    fn g(&self) -> Option<&Self::Term> {
        Some(&self.1)
    }
}

#[cfg(test)]
mod test {
    // Nothing really worth testing here
}
