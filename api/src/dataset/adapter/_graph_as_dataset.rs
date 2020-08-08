// this module is transparently re-exported by its parent `adapter`

use std::borrow::{Borrow, BorrowMut};
use std::hash::Hash;
use std::iter::empty;
use std::marker::PhantomData;

use resiter::Map;

use crate::dataset::*;
use crate::graph::{Graph, MutableGraph, SetGraph};
use crate::quad::streaming_mode::{FromTriple, StreamedQuad};
use crate::term::TTerm;

use super::GraphAsDatasetError;

/// The adapter returned by
/// * [`Graph::as_dataset`](../../graph/trait.Graph.html#method.as_dataset)
/// * [`Graph::as_dataset_mut`](../../graph/trait.Graph.html#method.as_dataset_mut)
/// * [`Graph::into_dataset`](../../graph/trait.Graph.html#method.into_dataset)
pub struct GraphAsDataset<G: ?Sized, H = G>(H, PhantomData<G>);

impl<G: ?Sized, H> GraphAsDataset<G, H> {
    /// Wrap a graph as a dataset
    pub fn new(graph: H) -> GraphAsDataset<G, H> {
        GraphAsDataset(graph, PhantomData)
    }

    /// Unwrap this adapter to get the original graph.
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
    fn quads_with_s<'s, TS>(&'s self, s: &'s TS) -> DQuadSource<'s, Self>
    where
        TS: TTerm + ?Sized,
    {
        Box::new(
            self.0
                .borrow()
                .triples_with_s(s)
                .map_ok(StreamedQuad::from_triple),
        )
    }
    #[inline]
    fn quads_with_p<'s, TP>(&'s self, p: &'s TP) -> DQuadSource<'s, Self>
    where
        TP: TTerm + ?Sized,
    {
        Box::new(
            self.0
                .borrow()
                .triples_with_p(p)
                .map_ok(StreamedQuad::from_triple),
        )
    }
    #[inline]
    fn quads_with_o<'s, TO>(&'s self, o: &'s TO) -> DQuadSource<'s, Self>
    where
        TO: TTerm + ?Sized,
    {
        Box::new(
            self.0
                .borrow()
                .triples_with_o(o)
                .map_ok(StreamedQuad::from_triple),
        )
    }
    #[inline]
    fn quads_with_g<'s, TG>(&'s self, g: Option<&'s TG>) -> DQuadSource<'s, Self>
    where
        TG: TTerm + ?Sized,
    {
        if g.is_some() {
            return Box::new(empty());
        }
        self.quads()
    }
    #[inline]
    fn quads_with_sp<'s, TS, TP>(&'s self, s: &'s TS, p: &'s TP) -> DQuadSource<'s, Self>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
    {
        Box::new(
            self.0
                .borrow()
                .triples_with_sp(s, p)
                .map_ok(StreamedQuad::from_triple),
        )
    }
    #[inline]
    fn quads_with_so<'s, TS, TO>(&'s self, s: &'s TS, o: &'s TO) -> DQuadSource<'s, Self>
    where
        TS: TTerm + ?Sized,
        TO: TTerm + ?Sized,
    {
        Box::new(
            self.0
                .borrow()
                .triples_with_so(s, o)
                .map_ok(StreamedQuad::from_triple),
        )
    }
    #[inline]
    fn quads_with_sg<'s, TS, TG>(&'s self, s: &'s TS, g: Option<&'s TG>) -> DQuadSource<'s, Self>
    where
        TS: TTerm + ?Sized,
        TG: TTerm + ?Sized,
    {
        if g.is_some() {
            return Box::new(empty());
        }
        self.quads_with_s(s)
    }
    #[inline]
    fn quads_with_po<'s, TP, TO>(&'s self, p: &'s TP, o: &'s TO) -> DQuadSource<'s, Self>
    where
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
    {
        Box::new(
            self.0
                .borrow()
                .triples_with_po(p, o)
                .map_ok(StreamedQuad::from_triple),
        )
    }
    #[inline]
    fn quads_with_pg<'s, TP, TG>(&'s self, p: &'s TP, g: Option<&'s TG>) -> DQuadSource<'s, Self>
    where
        TP: TTerm + ?Sized,
        TG: TTerm + ?Sized,
    {
        if g.is_some() {
            return Box::new(empty());
        }
        self.quads_with_p(p)
    }
    #[inline]
    fn quads_with_og<'s, TO, TG>(&'s self, o: &'s TO, g: Option<&'s TG>) -> DQuadSource<'s, Self>
    where
        TO: TTerm + ?Sized,
        TG: TTerm + ?Sized,
    {
        if g.is_some() {
            return Box::new(empty());
        }
        self.quads_with_o(o)
    }
    #[inline]
    fn quads_with_spo<'s, TS, TP, TO>(
        &'s self,
        s: &'s TS,
        p: &'s TP,
        o: &'s TO,
    ) -> DQuadSource<'s, Self>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
    {
        Box::new(
            self.0
                .borrow()
                .triples_with_spo(s, p, o)
                .map_ok(StreamedQuad::from_triple),
        )
    }
    #[inline]
    fn quads_with_spg<'s, TS, TP, TG>(
        &'s self,
        s: &'s TS,
        p: &'s TP,
        g: Option<&'s TG>,
    ) -> DQuadSource<'s, Self>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TG: TTerm + ?Sized,
    {
        if g.is_some() {
            return Box::new(empty());
        }
        self.quads_with_sp(s, p)
    }
    #[inline]
    fn quads_with_sog<'s, TS, TO, TG>(
        &'s self,
        s: &'s TS,
        o: &'s TO,
        g: Option<&'s TG>,
    ) -> DQuadSource<'s, Self>
    where
        TS: TTerm + ?Sized,
        TO: TTerm + ?Sized,
        TG: TTerm + ?Sized,
    {
        if g.is_some() {
            return Box::new(empty());
        }
        self.quads_with_so(s, o)
    }
    #[inline]
    fn quads_with_pog<'s, TP, TO, TG>(
        &'s self,
        p: &'s TP,
        o: &'s TO,
        g: Option<&'s TG>,
    ) -> DQuadSource<'s, Self>
    where
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
        TG: TTerm + ?Sized,
    {
        if g.is_some() {
            return Box::new(empty());
        }
        self.quads_with_po(p, o)
    }
    #[inline]
    fn quads_with_spog<'s, TS, TP, TO, TG>(
        &'s self,
        s: &'s TS,
        p: &'s TP,
        o: &'s TO,
        g: Option<&'s TG>,
    ) -> DQuadSource<'s, Self>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
        TG: TTerm + ?Sized,
    {
        if g.is_some() {
            return Box::new(empty());
        }
        self.quads_with_spo(s, p, o)
    }

    #[inline]
    fn contains<'s, TS, TP, TO, TG>(
        &'s self,
        s: &'s TS,
        p: &'s TP,
        o: &'s TO,
        g: Option<&'s TG>,
    ) -> DResult<Self, bool>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
        TG: TTerm + ?Sized,
    {
        if g.is_some() {
            return Ok(false);
        }
        self.0.borrow().contains(s, p, o)
    }
    #[inline]
    fn subjects(&self) -> DResultTermSet<Self>
    where
        DTerm<Self>: Clone + Eq + Hash,
    {
        self.0.borrow().subjects()
    }
    #[inline]
    fn predicates(&self) -> DResultTermSet<Self>
    where
        DTerm<Self>: Clone + Eq + Hash,
    {
        self.0.borrow().predicates()
    }
    #[inline]
    fn objects(&self) -> DResultTermSet<Self>
    where
        DTerm<Self>: Clone + Eq + Hash,
    {
        self.0.borrow().objects()
    }
    #[inline]
    fn graph_names(&self) -> DResultTermSet<Self>
    where
        DTerm<Self>: Clone + Eq + Hash,
    {
        Ok(std::collections::HashSet::new())
    }
    #[inline]
    fn iris(&self) -> DResultTermSet<Self>
    where
        DTerm<Self>: Clone + Eq + Hash,
    {
        self.0.borrow().iris()
    }
    #[inline]
    fn bnodes(&self) -> DResultTermSet<Self>
    where
        DTerm<Self>: Clone + Eq + Hash,
    {
        self.0.borrow().bnodes()
    }
    #[inline]
    fn literals(&self) -> DResultTermSet<Self>
    where
        DTerm<Self>: Clone + Eq + Hash,
    {
        self.0.borrow().literals()
    }
    #[inline]
    fn variables(&self) -> DResultTermSet<Self>
    where
        DTerm<Self>: Clone + Eq + Hash,
    {
        self.0.borrow().variables()
    }
}

impl<G, H> MutableDataset for GraphAsDataset<G, H>
where
    G: MutableGraph,
    H: BorrowMut<G>,
{
    type MutationError = GraphAsDatasetError<G::MutationError>;

    fn insert<TS, TP, TO, TG>(
        &mut self,
        s: &TS,
        p: &TP,
        o: &TO,
        g: Option<&TG>,
    ) -> MDResult<Self, bool>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
        TG: TTerm + ?Sized,
    {
        if g.is_some() {
            return Err(GraphAsDatasetError::GraphNamesNotSupported);
        };
        Ok(self.0.borrow_mut().insert(s, p, o)?)
    }

    fn remove<TS, TP, TO, TG>(
        &mut self,
        s: &TS,
        p: &TP,
        o: &TO,
        g: Option<&TG>,
    ) -> MDResult<Self, bool>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
        TG: TTerm + ?Sized,
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
    use super::*;
    use crate::dataset::adapter::DatasetGraph;
    use crate::dataset::{Dataset, MutableDataset};
    use crate::graph::*;
    use crate::ns::{rdf, rdfs};
    use crate::triple::stream::TripleSource;
    use std::collections::HashSet;
    use std::convert::Infallible;
    use std::error::Error;

    type BoxTerm = crate::term::test::TestTerm<Box<str>>;
    type StaticTerm = crate::term::test::TestTerm<&'static str>;

    const DG: Option<&'static StaticTerm> = None;

    type MyGraph = HashSet<[BoxTerm; 3]>;

    #[test]
    fn as_dataset() -> Result<(), Box<dyn Error>> {
        let mut g = MyGraph::new();
        <MyGraph as MutableGraph>::insert(&mut g, &rdfs::Resource, &rdf::type_, &rdfs::Class)?;

        let d = g.as_dataset();
        assert_eq!(d.quads().count(), 1);
        Ok(())
    }

    #[test]
    fn as_dataset_mut() -> Result<(), Box<dyn Error>> {
        let mut g = MyGraph::new();

        let mut d = g.as_dataset_mut();
        assert_eq!(d.quads().count(), 0);
        d.insert(&rdfs::Resource, &rdf::type_, &rdfs::Class, DG)?;
        assert_eq!(d.quads().count(), 1);
        // borrow stops here
        assert_eq!(g.len(), 1);

        let mut d = g.as_dataset_mut();
        assert_eq!(d.quads().count(), 1);
        d.remove(&rdfs::Resource, &rdf::type_, &rdfs::Class, DG)?;
        assert_eq!(d.quads().count(), 0);
        // borrow stops here
        assert_eq!(g.len(), 0);
        Ok(())
    }

    #[test]
    fn into_dataset() -> Result<(), Box<dyn Error>> {
        let g = MyGraph::new();

        let mut d = g.into_dataset();
        assert_eq!(d.quads().count(), 0);
        d.insert(&rdfs::Resource, &rdf::type_, &rdfs::Class, DG)?;
        assert_eq!(d.quads().count(), 1);

        let g = d.unwrap();
        assert_eq!(g.len(), 1);

        let mut d = g.into_dataset();
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

        let mut d = g.as_dataset_mut();
        let ret = d.insert(
            &rdfs::Class,
            &rdfs::subClassOf,
            &rdfs::Resource,
            Some(&rdfs::Class),
        );
        assert!(ret.is_err());
    }

    /// A DatasetAsGraph wrapped as a graph so that we can test it
    type GDG =
        DatasetGraph<GraphAsDataset<MyGraph>, GraphAsDataset<MyGraph>, Option<&'static StaticTerm>>;

    fn make_gdg<TS: TripleSource>(ts: TS) -> Result<GDG, Infallible> {
        Ok(DatasetGraph::new(
            ts.collect_triples::<MyGraph>().unwrap().into_dataset(),
            None,
        ))
    }

    crate::test_immutable_graph_impl!(gdg, GDG, true, true, make_gdg);
}
