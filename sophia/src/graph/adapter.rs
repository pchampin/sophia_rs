//! Adapters for exposing `Graph` as other traits.

use std::borrow::{Borrow, BorrowMut};
use std::iter::empty;
use std::marker::PhantomData;

use resiter::Map;

use crate::dataset::*;
use crate::graph::{Graph, MutableGraph, SetGraph};
use crate::quad::streaming_mode::{FromTriple, StreamedQuad};
use sophia_term::{Term, TermData};

mod _error;
pub use self::_error::*;

/// The adapter returned by
/// * [`Graph::borrow_as_dataset`](../trait.Graph.html#method.borrow_as_dataset)
/// * [`Graph::borrow_mut_as_dataset`](../trait.Graph.html#method.borrow_mut_as_dataset)
/// * [`Graph::own_as_dataset`](../trait.Graph.html#method.own_as_dataset)
pub struct GraphAsDataset<G: ?Sized, H>(
    pub(in crate::graph) H,
    pub(in crate::graph) PhantomData<G>,
);

impl<G: ?Sized, H> GraphAsDataset<G, H> {
    /// Unwrap this adapter to get the original graph back.
    pub fn unwrap(self) -> H {
        self.0
    }
}

impl<G, H> Dataset for GraphAsDataset<G, H>
where
    G: Graph,
    H: Borrow<G>,
{
    type Quad = FromTriple<G::Triple>;
    type Error = G::Error;

    #[inline]
    fn quads(&self) -> DQuadSource<Self> {
        Box::new(self.0.borrow().triples().map_ok(StreamedQuad::from_triple))
    }
    #[inline]
    fn quads_with_s<'s, T>(&'s self, s: &'s Term<T>) -> DQuadSource<'s, Self>
    where
        T: TermData,
    {
        Box::new(
            self.0
                .borrow()
                .triples_with_s(s)
                .map_ok(StreamedQuad::from_triple),
        )
    }
    #[inline]
    fn quads_with_p<'s, T>(&'s self, p: &'s Term<T>) -> DQuadSource<'s, Self>
    where
        T: TermData,
    {
        Box::new(
            self.0
                .borrow()
                .triples_with_p(p)
                .map_ok(StreamedQuad::from_triple),
        )
    }
    #[inline]
    fn quads_with_o<'s, T>(&'s self, o: &'s Term<T>) -> DQuadSource<'s, Self>
    where
        T: TermData,
    {
        Box::new(
            self.0
                .borrow()
                .triples_with_o(o)
                .map_ok(StreamedQuad::from_triple),
        )
    }
    #[inline]
    fn quads_with_g<'s, T>(&'s self, g: Option<&'s Term<T>>) -> DQuadSource<'s, Self>
    where
        T: TermData,
    {
        if g.is_some() {
            return Box::new(empty());
        }
        self.quads()
    }
    #[inline]
    fn quads_with_sp<'s, T, U>(&'s self, s: &'s Term<T>, p: &'s Term<U>) -> DQuadSource<'s, Self>
    where
        T: TermData,
        U: TermData,
    {
        Box::new(
            self.0
                .borrow()
                .triples_with_sp(s, p)
                .map_ok(StreamedQuad::from_triple),
        )
    }
    #[inline]
    fn quads_with_so<'s, T, U>(&'s self, s: &'s Term<T>, o: &'s Term<U>) -> DQuadSource<'s, Self>
    where
        T: TermData,
        U: TermData,
    {
        Box::new(
            self.0
                .borrow()
                .triples_with_so(s, o)
                .map_ok(StreamedQuad::from_triple),
        )
    }
    #[inline]
    fn quads_with_sg<'s, T, U>(
        &'s self,
        s: &'s Term<T>,
        g: Option<&'s Term<U>>,
    ) -> DQuadSource<'s, Self>
    where
        T: TermData,
        U: TermData,
    {
        if g.is_some() {
            return Box::new(empty());
        }
        self.quads_with_s(s)
    }
    #[inline]
    fn quads_with_po<'s, T, U>(&'s self, p: &'s Term<T>, o: &'s Term<U>) -> DQuadSource<'s, Self>
    where
        T: TermData,
        U: TermData,
    {
        Box::new(
            self.0
                .borrow()
                .triples_with_po(p, o)
                .map_ok(StreamedQuad::from_triple),
        )
    }
    #[inline]
    fn quads_with_pg<'s, T, U>(
        &'s self,
        p: &'s Term<T>,
        g: Option<&'s Term<U>>,
    ) -> DQuadSource<'s, Self>
    where
        T: TermData,
        U: TermData,
    {
        if g.is_some() {
            return Box::new(empty());
        }
        self.quads_with_p(p)
    }
    #[inline]
    fn quads_with_og<'s, T, U>(
        &'s self,
        o: &'s Term<T>,
        g: Option<&'s Term<U>>,
    ) -> DQuadSource<'s, Self>
    where
        T: TermData,
        U: TermData,
    {
        if g.is_some() {
            return Box::new(empty());
        }
        self.quads_with_o(o)
    }
    #[inline]
    fn quads_with_spo<'s, T, U, V>(
        &'s self,
        s: &'s Term<T>,
        p: &'s Term<U>,
        o: &'s Term<V>,
    ) -> DQuadSource<'s, Self>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        Box::new(
            self.0
                .borrow()
                .triples_with_spo(s, p, o)
                .map_ok(StreamedQuad::from_triple),
        )
    }
    #[inline]
    fn quads_with_spg<'s, T, U, V>(
        &'s self,
        s: &'s Term<T>,
        p: &'s Term<U>,
        g: Option<&'s Term<V>>,
    ) -> DQuadSource<'s, Self>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        if g.is_some() {
            return Box::new(empty());
        }
        self.quads_with_sp(s, p)
    }
    #[inline]
    fn quads_with_sog<'s, T, U, V>(
        &'s self,
        s: &'s Term<T>,
        o: &'s Term<U>,
        g: Option<&'s Term<V>>,
    ) -> DQuadSource<'s, Self>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        if g.is_some() {
            return Box::new(empty());
        }
        self.quads_with_so(s, o)
    }
    #[inline]
    fn quads_with_pog<'s, T, U, V>(
        &'s self,
        p: &'s Term<T>,
        o: &'s Term<U>,
        g: Option<&'s Term<V>>,
    ) -> DQuadSource<'s, Self>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        if g.is_some() {
            return Box::new(empty());
        }
        self.quads_with_po(p, o)
    }
    #[inline]
    fn quads_with_spog<'s, T, U, V, W>(
        &'s self,
        s: &'s Term<T>,
        p: &'s Term<U>,
        o: &'s Term<V>,
        g: Option<&'s Term<W>>,
    ) -> DQuadSource<'s, Self>
    where
        T: TermData,
        U: TermData,
        V: TermData,
        W: TermData,
    {
        if g.is_some() {
            return Box::new(empty());
        }
        self.quads_with_spo(s, p, o)
    }

    #[inline]
    fn contains<T, U, V, W>(
        &self,
        s: &Term<T>,
        p: &Term<U>,
        o: &Term<V>,
        g: Option<&Term<W>>,
    ) -> DResult<Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
        W: TermData,
    {
        if g.is_some() {
            return Ok(false);
        }
        self.0.borrow().contains(s, p, o)
    }
    #[inline]
    fn subjects(&self) -> DResultTermSet<Self> {
        self.0.borrow().subjects()
    }
    #[inline]
    fn predicates(&self) -> DResultTermSet<Self> {
        self.0.borrow().predicates()
    }
    #[inline]
    fn objects(&self) -> DResultTermSet<Self> {
        self.0.borrow().objects()
    }
    #[inline]
    fn graph_names(&self) -> DResultTermSet<Self> {
        Ok(std::collections::HashSet::new())
    }
    #[inline]
    fn iris(&self) -> DResultTermSet<Self> {
        self.0.borrow().iris()
    }
    #[inline]
    fn bnodes(&self) -> DResultTermSet<Self> {
        self.0.borrow().bnodes()
    }
    #[inline]
    fn literals(&self) -> DResultTermSet<Self> {
        self.0.borrow().literals()
    }
    #[inline]
    fn variables(&self) -> DResultTermSet<Self> {
        self.0.borrow().variables()
    }
}

impl<G, H> MutableDataset for GraphAsDataset<G, H>
where
    G: MutableGraph,
    H: BorrowMut<G>,
{
    type MutationError = GraphAsDatasetError<G::MutationError>;

    fn insert<T, U, V, W>(
        &mut self,
        s: &Term<T>,
        p: &Term<U>,
        o: &Term<V>,
        g: Option<&Term<W>>,
    ) -> MDResult<Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
        W: TermData,
    {
        if g.is_some() {
            return Err(GraphAsDatasetError::GraphNamesNotSupported);
        };
        Ok(self.0.borrow_mut().insert(s, p, o)?)
    }

    fn remove<T, U, V, W>(
        &mut self,
        s: &Term<T>,
        p: &Term<U>,
        o: &Term<V>,
        g: Option<&Term<W>>,
    ) -> MDResult<Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
        W: TermData,
    {
        if g.is_some() {
            return Ok(false);
        };
        Ok(self.0.borrow_mut().remove(s, p, o)?)
    }
}

impl<G, H> SetDataset for GraphAsDataset<G, H>
where
    G: SetGraph,
    H: Borrow<G>,
{
}

#[cfg(test)]
mod test {
    use crate::dataset::{Dataset, MutableDataset};
    use crate::graph::*;
    use crate::ns::{rdf, rdfs};
    use sophia_term::{BoxTerm, StaticTerm};
    use std::collections::HashSet;
    use std::error::Error;

    const DG: Option<&'static StaticTerm> = None;

    type MyGraph = HashSet<[BoxTerm; 3]>;

    #[test]
    fn test_borrow() -> Result<(), Box<dyn Error>> {
        let mut g = MyGraph::new();
        <MyGraph as MutableGraph>::insert(&mut g, &rdfs::Resource, &rdf::type_, &rdfs::Class)?;

        let d = g.borrow_as_dataset();
        assert_eq!(d.quads().count(), 1);
        Ok(())
    }

    #[test]
    fn test_borrow_mut() -> Result<(), Box<dyn Error>> {
        let mut g = MyGraph::new();

        let mut d = g.borrow_mut_as_dataset();
        assert_eq!(d.quads().count(), 0);
        d.insert(&rdfs::Resource, &rdf::type_, &rdfs::Class, DG)?;
        assert_eq!(d.quads().count(), 1);
        // borrow stops here
        assert_eq!(g.len(), 1);

        let mut d = g.borrow_mut_as_dataset();
        assert_eq!(d.quads().count(), 1);
        d.remove(&rdfs::Resource, &rdf::type_, &rdfs::Class, DG)?;
        assert_eq!(d.quads().count(), 0);
        // borrow stops here
        assert_eq!(g.len(), 0);
        Ok(())
    }

    #[test]
    fn test_owned() -> Result<(), Box<dyn Error>> {
        let g = MyGraph::new();

        let mut d = g.as_dataset();
        assert_eq!(d.quads().count(), 0);
        d.insert(&rdfs::Resource, &rdf::type_, &rdfs::Class, DG)?;
        assert_eq!(d.quads().count(), 1);

        let g = d.unwrap();
        assert_eq!(g.len(), 1);

        let mut d = g.as_dataset();
        assert_eq!(d.quads().count(), 1);
        d.remove(&rdfs::Resource, &rdf::type_, &rdfs::Class, DG)?;
        assert_eq!(d.quads().count(), 0);

        let g = d.unwrap();
        assert_eq!(g.len(), 0);
        Ok(())
    }

    #[test]
    fn test_invalid_graph_name() {
        let mut g = MyGraph::new();

        let mut d = g.borrow_mut_as_dataset();
        let ret = d.insert(
            &rdfs::Class,
            &rdfs::subClassOf,
            &rdfs::Resource,
            Some(&rdfs::Class),
        );
        assert!(ret.is_err());
    }
}
