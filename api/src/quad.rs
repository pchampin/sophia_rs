//! A quad expresses a single fact within a context.
//! Quads are like RDF [`triple`](crate::triple)s
//! augmented with an optional graph name.
//!
//! They are the individual statements of an RDF `dataset`(../dataset/index.html).

use crate::term::TTerm;
use crate::triple::*;

pub mod stream;
pub mod streaming_mode;

/// This trait represents an abstract RDF quad,
/// and provides convenient methods for working with quads.
pub trait Quad {
    type Term: TTerm + ?Sized;
    /// The subject of this quad.
    fn s(&self) -> &Self::Term;
    /// The predicate of this quad.
    fn p(&self) -> &Self::Term;
    /// The object of this quad.
    fn o(&self) -> &Self::Term;
    /// The (optional) graph name
    fn g(&self) -> Option<&Self::Term>;

    /// [`Triple`] adapter owning this quad.
    fn wrap_as_triple(self) -> QuadAsTriple<Self>
    where
        Self: Sized,
    {
        QuadAsTriple(self)
    }

    #[deprecated(since = "0.6.3", note = "Has been renamed to wrap_as_triple")]
    #[allow(clippy::wrong_self_convention)]
    fn as_triple(self) -> QuadAsTriple<Self>
    where
        Self: Sized,
    {
        self.wrap_as_triple()
    }

    /// Iterator over the components of this triple
    fn components(&self) -> QuadIter<Self> {
        QuadIter(self, 0)
    }
}

/// Iterator over the components of a quad.
pub struct QuadIter<'a, Q: ?Sized>(&'a Q, u8);

impl<'a, Q> Iterator for QuadIter<'a, Q>
where
    Q: Quad + ?Sized,
{
    type Item = &'a Q::Term;
    fn next(&mut self) -> Option<Self::Item> {
        self.1 += 1;
        match self.1 {
            1 => Some(self.0.s()),
            2 => Some(self.0.p()),
            3 => Some(self.0.o()),
            4 => self.0.g(),
            _ => None,
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let n = match self.0.g() {
            Some(_) => 4,
            None => 3,
        };
        (n, Some(n))
    }
}

impl<T> Quad for [T; 4]
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
    #[inline]
    fn g(&self) -> Option<&Self::Term> {
        Some(&self[3])
    }
}

impl<'a, T> Quad for (&'a T, &'a T, &'a T, &'a T)
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
    #[inline]
    fn g(&self) -> Option<&Self::Term> {
        Some(self.3)
    }
}

impl<T> Quad for (T, Option<T::Term>)
where
    T: Triple,
    T::Term: Sized,
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
        self.1.as_ref()
    }
}

/// An owned `Quad` as a tuple of an array and an optional name.
pub type TupleQuad<T> = ([T; 3], Option<T>);

impl<'a, Q: Quad> Quad for &'a Q
where
    Q: Quad,
{
    type Term = Q::Term;
    #[inline]
    fn s(&self) -> &Q::Term {
        (*self).s()
    }
    #[inline]
    fn p(&self) -> &Q::Term {
        (*self).p()
    }
    #[inline]
    fn o(&self) -> &Q::Term {
        (*self).o()
    }
    #[inline]
    fn g(&self) -> Option<&Q::Term> {
        (*self).g()
    }
}

/// The adapter returned by [`Quad::as_triple`].
pub struct QuadAsTriple<Q: ?Sized>(Q);

impl<Q> QuadAsTriple<Q>
where
    Q: Sized,
{
    /// Unwrap this adapter to get the original quad back.
    pub fn unwrap(self) -> Q {
        self.0
    }
}

impl<Q> Triple for QuadAsTriple<Q>
where
    Q: Quad + ?Sized,
{
    type Term = Q::Term;
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
}

#[cfg(test)]
mod test {
    // Nothing really worth testing here
}
