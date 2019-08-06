//! Adapters for exposing `Graph` as other traits.

use std::borrow::{Borrow, BorrowMut};
use std::iter::empty;
use std::marker::PhantomData;

use resiter::Map;

use crate::dataset::*;
use crate::error::*;
use crate::graph::{Graph, MutableGraph, SetGraph};
use crate::term::*;
use crate::triple::{Triple, TripleAsQuad};

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

impl<'a, G, H> Dataset<'a> for GraphAsDataset<G, H>
where
    G: Graph<'a>,
    H: Borrow<G>,
{
    type Quad = TripleAsQuad<G::Triple>;
    type Error = G::Error;

    #[inline]
    fn quads(&'a self) -> DQuadSource<'a, Self> {
        Box::new(self.0.borrow().triples().map_ok(Triple::as_quad))
    }
    #[inline]
    fn quads_with_s<T>(&'a self, s: &'a Term<T>) -> DQuadSource<'a, Self>
    where
        T: TermData,
    {
        Box::new(self.0.borrow().triples_with_s(s).map_ok(Triple::as_quad))
    }
    #[inline]
    fn quads_with_p<T>(&'a self, p: &'a Term<T>) -> DQuadSource<'a, Self>
    where
        T: TermData,
    {
        Box::new(self.0.borrow().triples_with_p(p).map_ok(Triple::as_quad))
    }
    #[inline]
    fn quads_with_o<T>(&'a self, o: &'a Term<T>) -> DQuadSource<'a, Self>
    where
        T: TermData,
    {
        Box::new(self.0.borrow().triples_with_o(o).map_ok(Triple::as_quad))
    }
    #[inline]
    fn quads_with_g<T>(&'a self, g: &'a GraphName<T>) -> DQuadSource<'a, Self>
    where
        T: TermData,
    {
        if let Some(_) = g {
            return Box::new(empty());
        }
        self.quads()
    }
    #[inline]
    fn quads_with_sp<T, U>(&'a self, s: &'a Term<T>, p: &'a Term<U>) -> DQuadSource<'a, Self>
    where
        T: TermData,
        U: TermData,
    {
        Box::new(
            self.0
                .borrow()
                .triples_with_sp(s, p)
                .map_ok(Triple::as_quad),
        )
    }
    #[inline]
    fn quads_with_so<T, U>(&'a self, s: &'a Term<T>, o: &'a Term<U>) -> DQuadSource<'a, Self>
    where
        T: TermData,
        U: TermData,
    {
        Box::new(
            self.0
                .borrow()
                .triples_with_so(s, o)
                .map_ok(Triple::as_quad),
        )
    }
    #[inline]
    fn quads_with_sg<T, U>(&'a self, s: &'a Term<T>, g: &'a GraphName<U>) -> DQuadSource<'a, Self>
    where
        T: TermData,
        U: TermData,
    {
        if let Some(_) = g {
            return Box::new(empty());
        }
        self.quads_with_s(s)
    }
    #[inline]
    fn quads_with_po<T, U>(&'a self, p: &'a Term<T>, o: &'a Term<U>) -> DQuadSource<'a, Self>
    where
        T: TermData,
        U: TermData,
    {
        Box::new(
            self.0
                .borrow()
                .triples_with_po(p, o)
                .map_ok(Triple::as_quad),
        )
    }
    #[inline]
    fn quads_with_pg<T, U>(&'a self, p: &'a Term<T>, g: &'a GraphName<U>) -> DQuadSource<'a, Self>
    where
        T: TermData,
        U: TermData,
    {
        if let Some(_) = g {
            return Box::new(empty());
        }
        self.quads_with_p(p)
    }
    #[inline]
    fn quads_with_og<T, U>(&'a self, o: &'a Term<T>, g: &'a GraphName<U>) -> DQuadSource<'a, Self>
    where
        T: TermData,
        U: TermData,
    {
        if let Some(_) = g {
            return Box::new(empty());
        }
        self.quads_with_o(o)
    }
    #[inline]
    fn quads_with_spo<T, U, V>(
        &'a self,
        s: &'a Term<T>,
        p: &'a Term<U>,
        o: &'a Term<V>,
    ) -> DQuadSource<'a, Self>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        Box::new(
            self.0
                .borrow()
                .triples_with_spo(s, p, o)
                .map_ok(Triple::as_quad),
        )
    }
    #[inline]
    fn quads_with_spg<T, U, V>(
        &'a self,
        s: &'a Term<T>,
        p: &'a Term<U>,
        g: &'a GraphName<V>,
    ) -> DQuadSource<'a, Self>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        if let Some(_) = g {
            return Box::new(empty());
        }
        self.quads_with_sp(s, p)
    }
    #[inline]
    fn quads_with_sog<T, U, V>(
        &'a self,
        s: &'a Term<T>,
        o: &'a Term<U>,
        g: &'a GraphName<V>,
    ) -> DQuadSource<'a, Self>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        if let Some(_) = g {
            return Box::new(empty());
        }
        self.quads_with_so(s, o)
    }
    #[inline]
    fn quads_with_pog<T, U, V>(
        &'a self,
        p: &'a Term<T>,
        o: &'a Term<U>,
        g: &'a GraphName<V>,
    ) -> DQuadSource<'a, Self>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        if let Some(_) = g {
            return Box::new(empty());
        }
        self.quads_with_po(p, o)
    }
    #[inline]
    fn quads_with_spog<T, U, V, W>(
        &'a self,
        s: &'a Term<T>,
        p: &'a Term<U>,
        o: &'a Term<V>,
        g: &'a GraphName<W>,
    ) -> DQuadSource<'a, Self>
    where
        T: TermData,
        U: TermData,
        V: TermData,
        W: TermData,
    {
        if let Some(_) = g {
            return Box::new(empty());
        }
        self.quads_with_spo(s, p, o)
    }

    #[inline]
    fn contains<T, U, V, W>(
        &'a self,
        s: &'a Term<T>,
        p: &'a Term<U>,
        o: &'a Term<V>,
        g: &'a GraphName<W>,
    ) -> DResult<'a, Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
        W: TermData,
    {
        if let Some(_) = g {
            return Ok(false);
        }
        self.0.borrow().contains(s, p, o)
    }
    #[inline]
    fn subjects(&'a self) -> DResultTermSet<'a, Self> {
        self.0.borrow().subjects()
    }
    #[inline]
    fn predicates(&'a self) -> DResultTermSet<'a, Self> {
        self.0.borrow().predicates()
    }
    #[inline]
    fn objects(&'a self) -> DResultTermSet<'a, Self> {
        self.0.borrow().objects()
    }
    #[inline]
    fn graph_names(&'a self) -> DResultTermSet<'a, Self> {
        Ok(std::collections::HashSet::new())
    }
    #[inline]
    fn iris(&'a self) -> DResultTermSet<'a, Self> {
        self.0.borrow().iris()
    }
    #[inline]
    fn bnodes(&'a self) -> DResultTermSet<'a, Self> {
        self.0.borrow().bnodes()
    }
    #[inline]
    fn literals(&'a self) -> DResultTermSet<'a, Self> {
        self.0.borrow().literals()
    }
    #[inline]
    fn variables(&'a self) -> DResultTermSet<'a, Self> {
        self.0.borrow().variables()
    }
}

impl<G, H> MutableDataset for GraphAsDataset<G, H>
where
    G: MutableGraph,
    H: BorrowMut<G>,
    Error: From<G::MutationError>,
{
    type MutationError = Error;

    fn insert<T, U, V, W>(
        &mut self,
        s: &Term<T>,
        p: &Term<U>,
        o: &Term<V>,
        g: &GraphName<W>,
    ) -> MDResult<Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
        W: TermData,
    {
        if let Some(graph_name) = g {
            return Err(ErrorKind::UnsupportedGraphName(graph_name.n3()).into());
        };
        Ok(self.0.borrow_mut().insert(s, p, o)?)
    }

    fn remove<T, U, V, W>(
        &mut self,
        s: &Term<T>,
        p: &Term<U>,
        o: &Term<V>,
        g: &GraphName<W>,
    ) -> MDResult<Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
        W: TermData,
    {
        if let Some(_) = g {
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
    use crate::error::Result;
    use crate::graph::inmem::LightGraph;
    use crate::graph::{Graph, MutableGraph};
    use crate::ns::{rdf, rdfs};
    use crate::term::GraphName;

    const DG: GraphName<&'static str> = None;

    #[test]
    fn test_borrow() -> Result<()> {
        let mut g = LightGraph::new();
        g.insert(&rdfs::Resource, &rdf::type_, &rdfs::Class)?;

        let d = g.borrow_as_dataset();
        assert_eq!(d.quads().count(), 1);
        Ok(())
    }

    #[test]
    fn test_borrow_mut() -> Result<()> {
        let mut g = LightGraph::new();

        let mut d = g.borrow_mut_as_dataset();
        assert_eq!(d.quads().count(), 0);
        d.insert(&rdfs::Resource, &rdf::type_, &rdfs::Class, &DG)?;
        assert_eq!(d.quads().count(), 1);
        // borrow stops here
        assert_eq!(g.len(), 1);

        let mut d = g.borrow_mut_as_dataset();
        assert_eq!(d.quads().count(), 1);
        d.remove(&rdfs::Resource, &rdf::type_, &rdfs::Class, &DG)?;
        assert_eq!(d.quads().count(), 0);
        // borrow stops here
        assert_eq!(g.len(), 0);
        Ok(())
    }

    #[test]
    fn test_owned() -> Result<()> {
        let g = LightGraph::new();

        let mut d = g.as_dataset();
        assert_eq!(d.quads().count(), 0);
        d.insert(&rdfs::Resource, &rdf::type_, &rdfs::Class, &DG)?;
        assert_eq!(d.quads().count(), 1);

        let g = d.unwrap();
        assert_eq!(g.len(), 1);

        let mut d = g.as_dataset();
        assert_eq!(d.quads().count(), 1);
        d.remove(&rdfs::Resource, &rdf::type_, &rdfs::Class, &DG)?;
        assert_eq!(d.quads().count(), 0);

        let g = d.unwrap();
        assert_eq!(g.len(), 0);
        Ok(())
    }

    #[test]
    fn test_invalid_graph_name() {
        let mut g = LightGraph::new();

        let mut d = g.borrow_mut_as_dataset();
        let ret = d.insert(
            &rdfs::Class,
            &rdfs::subClassOf,
            &rdfs::Resource,
            &rdfs::Class.as_graph_id(),
        );
        assert!(ret.is_err());
    }

    mod dataset {
        // moved here from ::dataset::adapter::test,
        // because test_graph_impl! seems to be only usable from ::graph
        use crate::dataset::adapter::test::{
            make_default_graph, make_named_graph, LightDatasetGraph,
        };
        test_graph_impl!(default_graph, LightDatasetGraph, true, make_default_graph);
        test_graph_impl!(named_graph, LightDatasetGraph, true, make_named_graph);
    }
}
