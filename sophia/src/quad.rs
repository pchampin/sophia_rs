//! A quad expresses a single fact within a context.
//! Quads are like RDF `triples`(../triple/index.html)
//! augmented with an optional graph name.
//!
//! They are the individual statements of an RDF `dataset`(../dataset/index.html).

use crate::term::*;
use crate::triple::*;

pub mod stream;

/// This trait represents an abstract RDF quad,
/// and provides convenient methods for working with quads.
pub trait Quad {
    type TermData: TermData;
    /// The subject of this quad.
    fn s(&self) -> &Term<<Self as Quad>::TermData>;
    /// The predicate of this quad.
    fn p(&self) -> &Term<<Self as Quad>::TermData>;
    /// The object of this quad.
    fn o(&self) -> &Term<<Self as Quad>::TermData>;
    /// The (optional) graph name
    fn g(&self) -> Option<&Term<<Self as Quad>::TermData>>;

    /// [`Triple`](../triple/trait.Triple.html) adapter owning this quad.
    fn as_triple(self) -> QuadAsTriple<Self>
    where
        Self: Sized,
    {
        QuadAsTriple(self)
    }
}

impl<T> Quad for [Term<T>; 4]
where
    T: TermData,
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
    #[inline]
    fn g(&self) -> Option<&Term<T>> {
        Some(&self[3])
    }
}

impl<'a, T> Quad for [&'a Term<T>; 4]
where
    T: TermData,
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
    #[inline]
    fn g(&self) -> Option<&Term<T>> {
        Some(self[3])
    }
}

impl<T> Quad for (T, Option<Term<T::TermData>>)
where
    T: Triple,
{
    type TermData = T::TermData;
    #[inline]
    fn s(&self) -> &Term<T::TermData> {
        &self.0.s()
    }
    #[inline]
    fn p(&self) -> &Term<T::TermData> {
        &self.0.p()
    }
    #[inline]
    fn o(&self) -> &Term<T::TermData> {
        &self.0.o()
    }
    #[inline]
    fn g(&self) -> Option<&Term<T::TermData>> {
        self.1.as_ref()
    }
}

impl<'a, T> Quad for (T, Option<&'a Term<T::TermData>>)
where
    T: Triple,
{
    type TermData = T::TermData;
    #[inline]
    fn s(&self) -> &Term<T::TermData> {
        &self.0.s()
    }
    #[inline]
    fn p(&self) -> &Term<T::TermData> {
        &self.0.p()
    }
    #[inline]
    fn o(&self) -> &Term<T::TermData> {
        &self.0.o()
    }
    #[inline]
    fn g(&self) -> Option<&Term<T::TermData>> {
        self.1
    }
}

impl<'a, Q: Quad> Quad for &'a Q
where
    Q: Quad,
{
    type TermData = <Q as Quad>::TermData;
    #[inline]
    fn s(&self) -> &Term<<Q as Quad>::TermData> {
        (*self).s()
    }
    #[inline]
    fn p(&self) -> &Term<<Q as Quad>::TermData> {
        (*self).p()
    }
    #[inline]
    fn o(&self) -> &Term<<Q as Quad>::TermData> {
        (*self).o()
    }
    #[inline]
    fn g(&self) -> Option<&Term<<Q as Quad>::TermData>> {
        (*self).g()
    }
}

/// The adapter returned by [`Quad::as_triple`](./trait.Quad.html#method.as_triple).
pub struct QuadAsTriple<Q>(Q);

impl<Q> QuadAsTriple<Q> {
    /// Unwrap this adapter to get the original quad back.
    pub fn unwrap(self) -> Q {
        self.0
    }
}

impl<Q: Quad> Triple for QuadAsTriple<Q> {
    type TermData = <Q as Quad>::TermData;
    #[inline]
    fn s(&self) -> &Term<<Q as Quad>::TermData> {
        self.0.s()
    }
    #[inline]
    fn p(&self) -> &Term<<Q as Quad>::TermData> {
        self.0.p()
    }
    #[inline]
    fn o(&self) -> &Term<<Q as Quad>::TermData> {
        self.0.o()
    }
}

#[cfg(test)]
mod test {
    // Nothing really worth testing here
}
