// this module is transparently re-exported by its parent `adapter`

use std::borrow::{Borrow, BorrowMut};
use std::marker::PhantomData;

use resiter::Map;

use crate::dataset::{Dataset, MutableDataset, SetDataset};
use crate::graph::*;
use crate::term::matcher::{GraphNameMatcher, ANY};
use crate::term::TTerm;
use crate::triple::streaming_mode::{FromQuad, StreamedTriple};

/// The adapter returned by
/// [`Dataset::graph`],
/// [`Dataset::graph_mut`], and
/// [`Dataset::union_graph`].
pub struct DatasetGraph<D: ?Sized, E, M: GraphNameMatcher> {
    dataset: E,
    gmatcher: M,
    _phantom: PhantomData<D>,
}

impl<D: ?Sized, E, M: GraphNameMatcher> DatasetGraph<D, E, M> {
    /// Wrap a dataset as a graph
    pub fn new(dataset: E, gmatcher: M) -> DatasetGraph<D, E, M> {
        DatasetGraph {
            dataset,
            gmatcher,
            _phantom: PhantomData,
        }
    }

    /// Unwrap this adapter to get the original graph.
    pub fn unwrap(self) -> E {
        self.dataset
    }
}

impl<D, E, M> Graph for DatasetGraph<D, E, M>
where
    D: Dataset + ?Sized,
    E: Borrow<D>,
    M: GraphNameMatcher,
{
    type Triple = FromQuad<D::Quad>;
    type Error = D::Error;

    fn triples(&self) -> GTripleSource<Self> {
        Box::new(
            self.dataset
                .borrow()
                .quads_matching(&ANY, &ANY, &ANY, &self.gmatcher)
                .map_ok(StreamedTriple::from_quad),
        )
    }
    fn triples_with_s<'s, TS>(&'s self, s: &'s TS) -> GTripleSource<'s, Self>
    where
        TS: TTerm + ?Sized,
    {
        Box::new(
            self.dataset
                .borrow()
                .quads_matching(s, &ANY, &ANY, &self.gmatcher)
                .map_ok(StreamedTriple::from_quad),
        )
    }
    fn triples_with_p<'s, TP>(&'s self, p: &'s TP) -> GTripleSource<'s, Self>
    where
        TP: TTerm + ?Sized,
    {
        Box::new(
            self.dataset
                .borrow()
                .quads_matching(&ANY, p, &ANY, &self.gmatcher)
                .map_ok(StreamedTriple::from_quad),
        )
    }
    fn triples_with_o<'s, TO>(&'s self, o: &'s TO) -> GTripleSource<'s, Self>
    where
        TO: TTerm + ?Sized,
    {
        Box::new(
            self.dataset
                .borrow()
                .quads_matching(&ANY, &ANY, o, &self.gmatcher)
                .map_ok(StreamedTriple::from_quad),
        )
    }
    fn triples_with_sp<'s, TS, TP>(&'s self, s: &'s TS, p: &'s TP) -> GTripleSource<'s, Self>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
    {
        Box::new(
            self.dataset
                .borrow()
                .quads_matching(s, p, &ANY, &self.gmatcher)
                .map_ok(StreamedTriple::from_quad),
        )
    }
    fn triples_with_so<'s, TS, TO>(&'s self, s: &'s TS, o: &'s TO) -> GTripleSource<'s, Self>
    where
        TS: TTerm + ?Sized,
        TO: TTerm + ?Sized,
    {
        Box::new(
            self.dataset
                .borrow()
                .quads_matching(s, &ANY, o, &self.gmatcher)
                .map_ok(StreamedTriple::from_quad),
        )
    }
    fn triples_with_po<'s, TP, TO>(&'s self, p: &'s TP, o: &'s TO) -> GTripleSource<'s, Self>
    where
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
    {
        Box::new(
            self.dataset
                .borrow()
                .quads_matching(&ANY, p, o, &self.gmatcher)
                .map_ok(StreamedTriple::from_quad),
        )
    }
    fn triples_with_spo<'s, TS, TP, TO>(
        &'s self,
        s: &'s TS,
        p: &'s TP,
        o: &'s TO,
    ) -> GTripleSource<'s, Self>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
    {
        Box::new(
            self.dataset
                .borrow()
                .quads_matching(s, p, o, &self.gmatcher)
                .map_ok(StreamedTriple::from_quad),
        )
    }
}

impl<D, E, T> MutableGraph for DatasetGraph<D, E, Option<&T>>
where
    D: MutableDataset,
    E: BorrowMut<D>,
    T: TTerm + ?Sized,
{
    type MutationError = D::MutationError;

    fn insert<TS, TP, TO>(&mut self, s: &TS, p: &TP, o: &TO) -> MgResult<Self, bool>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
    {
        self.dataset.borrow_mut().insert(s, p, o, self.gmatcher)
    }

    fn remove<TS, TP, TO>(&mut self, s: &TS, p: &TP, o: &TO) -> MgResult<Self, bool>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
    {
        self.dataset.borrow_mut().remove(s, p, o, self.gmatcher)
    }
}

impl<D, E, T> SetGraph for DatasetGraph<D, E, Option<&T>>
where
    D: Dataset + SetDataset,
    E: Borrow<D>,
    T: TTerm + ?Sized,
{
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::dataset::test::*;
    use crate::dataset::MdResult;
    use crate::dataset::*;
    use crate::quad::stream::QuadSource;
    use crate::term::{same_graph_name, SimpleIri, TTerm};
    use crate::triple::stream::TripleSource;
    use std::collections::HashSet;

    type BoxTerm = crate::term::test::TestTerm<Box<str>>;

    type MyQuad = ([BoxTerm; 3], Option<BoxTerm>);
    type MyDataset = HashSet<MyQuad>;
    type MyDatasetGraph = DatasetGraph<MyDataset, MyDataset, Option<&'static SimpleIri<'static>>>;

    fn make_default_graph<TS>(ts: TS) -> Result<MyDatasetGraph, ()>
    where
        TS: TripleSource,
    {
        let mut g = DatasetGraph {
            dataset: MyDataset::new(),
            gmatcher: None,
            _phantom: PhantomData,
        };
        g.insert_all(ts).unwrap();
        Ok(g)
    }

    #[cfg(feature = "all_tests")]
    fn make_named_graph<TS>(ts: TS) -> Result<MyDatasetGraph, ()>
    where
        TS: TripleSource,
    {
        let mut g = DatasetGraph {
            dataset: MyDataset::new(),
            gmatcher: Some(&crate::ns::rdfs::Resource),
            _phantom: PhantomData,
        };
        g.insert_all(ts).unwrap();
        Ok(g)
    }

    crate::test_graph_impl!(dflt_graph, MyDatasetGraph, true, true, make_default_graph);
    #[cfg(feature = "all_tests")]
    crate::test_graph_impl!(named_graph, MyDatasetGraph, true, true, make_named_graph);

    #[test]
    fn test_graph_default() -> MdResult<MyDataset, ()> {
        let d: MyDataset = some_quads().collect_quads().unwrap();
        assert_eq!(d.graph(DG.as_ref()).triples().count(), 4);
        assert_eq!(d.graph(GN1.as_ref()).triples().count(), 7);
        assert_eq!(d.graph(GN2.as_ref()).triples().count(), 7);
        assert_eq!(d.union_graph(ANY).triples().count(), 18);
        assert_eq!(
            d.union_graph(vec![DG.as_ref(), GN1.as_ref()])
                .triples()
                .count(),
            11
        );
        assert_eq!(
            d.union_graph([|x: Option<&dyn TTerm>| same_graph_name(x, DG.as_ref())
                || same_graph_name(x, GN2.as_ref())])
                .triples()
                .count(),
            11
        );
        Ok(())
    }
}
