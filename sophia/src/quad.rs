//! A quad expresses a single fact within a context.
//! Quads are like RDF `triples`(../triple/index.html)
//! augmented with an optional graph name.
//!
//! They are the individual statements of an RDF `dataset`(../dataset/index.html).

use std::borrow::Borrow;

use crate::term::graph_key::*;
use crate::term::*;
use crate::triple::*;

pub mod stream;

/// This trait represents an abstract RDF quad,
/// and provide convenient methods for working with quads.
pub trait Quad<'a> {
    type TermData: TermData + 'a;
    /// The subject of this quad.
    fn s(&self) -> &Term<<Self as Quad<'a>>::TermData>;
    /// The predicate of this quad.
    fn p(&self) -> &Term<<Self as Quad<'a>>::TermData>;
    /// The object of this quad.
    fn o(&self) -> &Term<<Self as Quad<'a>>::TermData>;
    /// The graph key (either a graph name or "default graph") of this quad.
    fn g(&self) -> &GraphKey<<Self as Quad<'a>>::TermData>;
}

impl<'a, T> Quad<'a> for [Term<T>; 4]
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
    #[inline]
    fn g(&self) -> &GraphKey<T> {
        self[3].as_graph_key()
    }
}

impl<'a, T> Quad<'a> for [&'a Term<T>; 4]
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
    #[inline]
    fn g(&self) -> &GraphKey<T> {
        self[3].as_graph_key()
    }
}

impl<'a, T, G> Quad<'a> for (T, G)
where
    T: Triple<'a>,
    G: Borrow<GraphKey<T::TermData>>,
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
    fn g(&self) -> &GraphKey<T::TermData> {
        self.1.borrow()
    }
}

impl<'a, Q: Quad<'a>> Quad<'a> for &'a Q
where
    Q: Quad<'a>,
{
    type TermData = <Q as Quad<'a>>::TermData;
    #[inline]
    fn s(&self) -> &Term<<Q as Quad<'a>>::TermData> {
        (*self).s()
    }
    #[inline]
    fn p(&self) -> &Term<<Q as Quad<'a>>::TermData> {
        (*self).p()
    }
    #[inline]
    fn o(&self) -> &Term<<Q as Quad<'a>>::TermData> {
        (*self).o()
    }
    #[inline]
    fn g(&self) -> &GraphKey<<Q as Quad<'a>>::TermData> {
        (*self).g()
    }
}

struct TripleWrapper<Q>(Q);

impl<'a, Q: Quad<'a>> Triple<'a> for TripleWrapper<Q> {
    type TermData = <Q as Quad<'a>>::TermData;
    #[inline]
    fn s(&self) -> &Term<<Q as Quad<'a>>::TermData> {
        self.0.s()
    }
    #[inline]
    fn p(&self) -> &Term<<Q as Quad<'a>>::TermData> {
        self.0.p()
    }
    #[inline]
    fn o(&self) -> &Term<<Q as Quad<'a>>::TermData> {
        self.0.o()
    }
}

/// Convert any quad into a triple
pub fn as_triple<'a, Q: Quad<'a>>(quad: Q) -> impl Triple<'a> {
    TripleWrapper(quad)
}


struct DGQuadWrapper<T>(T);

impl<'a, T: Triple<'a>> Quad<'a> for DGQuadWrapper<T> {
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
    fn g(&self) -> &GraphKey<T::TermData> {
        unimplemented!()
    }
}

/// Convert any triple into a quad from the default graph.
pub fn as_quad<'a, T: Triple<'a>>(triple: T) -> impl Quad<'a> {
    DGQuadWrapper(triple)
}

pub struct NGQuadWrapper<'a, T: Triple<'a>>(T, Term<T::TermData>);

impl<'a, T: Triple<'a>> Quad<'a> for NGQuadWrapper<'a, T> {
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
    fn g(&self) -> &GraphKey<T::TermData> {
        self.1.as_graph_key()
    }
}

/// Convert any triple into a quad from the default graph.
pub fn as_quad_from<'a, T: Triple<'a>>(triple: T, graph_key: Term<T::TermData>) -> impl Quad<'a> {
    NGQuadWrapper(triple, graph_key)
}

#[cfg(test)]
mod test {
    // Nothing really worth testing here
}
