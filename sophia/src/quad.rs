//! A quad expresses a single fact within a context.
//! Quads are RDF triples augmented with an optional graph name.
//!
//! They are the individual statements of an RDF dataset.

use std::hash::Hash;

use crate::term::*;
use crate::term::graph_key::*;
use crate::triple::*;

pub mod stream;

/// This trait represents an abstract RDF quad,
/// and provide convenient methods for working with quads.
pub trait Quad<'a>: Triple<'a> {
    /// The graph key (either a graph name or "default graph") of this quad.
    fn g(&self) -> &GraphKey<Self::TermData>;
}

impl<'a, T> Triple<'a> for [Term<T>;4]
where
    T: AsRef<str> + Clone + Eq + Hash + 'a,
{
    type TermData= T;
    #[inline] fn s(&self) -> &Term<T> { &self[0] }
    #[inline] fn p(&self) -> &Term<T> { &self[1] }
    #[inline] fn o(&self) -> &Term<T> { &self[2] }
}

impl<'a, T> Quad<'a> for [Term<T>;4]
where
    T: AsRef<str> + Clone + Eq + Hash + 'a,
{
    #[inline] fn g(&self) -> &GraphKey<T> { self[3].as_graph_key() }
}



impl<'a, T> Triple<'a> for [&'a Term<T>;4]
where
    T: AsRef<str> + Clone + Eq + Hash + 'a,
{
    type TermData= T;
    #[inline] fn s(&self) -> &Term<T> { self[0] }
    #[inline] fn p(&self) -> &Term<T> { self[1] }
    #[inline] fn o(&self) -> &Term<T> { self[2] }
}

impl<'a, T> Quad<'a> for [&'a Term<T>;4]
where
    T: AsRef<str> + Clone + Eq + Hash + 'a,
{
    #[inline] fn g(&self) -> &GraphKey<T> { self[3].as_graph_key() }
}



impl<'a, T> Triple<'a> for (T, GraphKey<T::TermData>)
where
    T: Triple<'a>,
{
    type TermData= T::TermData;
    #[inline] fn s(&self) -> &Term<Self::TermData> { &self.0.s() }
    #[inline] fn p(&self) -> &Term<Self::TermData> { &self.0.p() }
    #[inline] fn o(&self) -> &Term<Self::TermData> { &self.0.o() }
}

impl<'a, T> Quad<'a> for (T, GraphKey<T::TermData>)
where
    T: Triple<'a>,
{
    #[inline] fn g(&self) -> &GraphKey<T::TermData> { &self.1 }
}



impl<'a, T: Quad<'a>> Quad<'a> for &'a T {
    #[inline] fn g(&self) -> &GraphKey<T::TermData> { (*self).g() }
}


struct DGQuadWrapper<T> (T);

impl<'a, T: Triple<'a>> Triple<'a> for DGQuadWrapper<T> {
    type TermData = T::TermData;
    #[inline] fn s(&self) -> &Term<Self::TermData> { self.0.s() }
    #[inline] fn p(&self) -> &Term<Self::TermData> { self.0.p() }
    #[inline] fn o(&self) -> &Term<Self::TermData> { self.0.o() }
}

impl<'a, T: Triple<'a>> Quad<'a> for DGQuadWrapper<T> {
    #[inline] fn g(&self) -> &GraphKey<Self::TermData> { unimplemented!() }
}

/// Convert any triple into a quad from the default graph.
pub fn as_quad<'a, T: Triple<'a>>(triple: T) -> impl Quad<'a> {
    DGQuadWrapper(triple)
}


pub struct NGQuadWrapper<'a, T: Triple<'a>> (T, Term<T::TermData>);

impl<'a, T: Triple<'a>> Triple<'a> for NGQuadWrapper<'a, T> {
    type TermData = T::TermData;
    #[inline] fn s(&self) -> &Term<Self::TermData> { self.0.s() }
    #[inline] fn p(&self) -> &Term<Self::TermData> { self.0.p() }
    #[inline] fn o(&self) -> &Term<Self::TermData> { self.0.o() }
}

impl<'a, T: Triple<'a>> Quad<'a> for NGQuadWrapper<'a, T> {
    #[inline] fn g(&self) -> &GraphKey<Self::TermData> { self.1.as_graph_key() }
}

/// Convert any triple into a quad from the default graph.
pub fn as_quad_from<'a, T: Triple<'a>>(triple: T, graph_key: Term<T::TermData>) -> impl Quad<'a> {
    NGQuadWrapper(triple, graph_key)
}



#[cfg(test)]
mod test {
    // Nothing really worth testing here
}
