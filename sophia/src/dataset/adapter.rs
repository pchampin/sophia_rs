//! Adapters for exposing `Datasets` as other traits.

use std::borrow::{Borrow, BorrowMut};
use std::marker::PhantomData;

use resiter::Map;

use crate::dataset::{Dataset, MutableDataset, SetDataset};
use crate::graph::*;
use crate::term::matcher::{GraphNameMatcher, ANY};
use crate::term::{Term, TermData};
use crate::triple::streaming_mode::{FromQuad, StreamedTriple};

/// The adapter returned by
/// [`Dataset::union_graph`](../trait.Dataset.html#method.union_graph)
pub struct DatasetGraph<D: ?Sized, E, M: GraphNameMatcher> {
    pub(in crate::dataset) dataset: E,
    pub(in crate::dataset) gmatcher: M,
    pub(in crate::dataset) _phantom: PhantomData<D>,
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
    fn triples_with_s<'s, T>(&'s self, s: &'s Term<T>) -> GTripleSource<'s, Self>
    where
        T: TermData,
    {
        Box::new(
            self.dataset
                .borrow()
                .quads_matching(s, &ANY, &ANY, &self.gmatcher)
                .map_ok(StreamedTriple::from_quad),
        )
    }
    fn triples_with_p<'s, T>(&'s self, p: &'s Term<T>) -> GTripleSource<'s, Self>
    where
        T: TermData,
    {
        Box::new(
            self.dataset
                .borrow()
                .quads_matching(&ANY, p, &ANY, &self.gmatcher)
                .map_ok(StreamedTriple::from_quad),
        )
    }
    fn triples_with_o<'s, T>(&'s self, o: &'s Term<T>) -> GTripleSource<'s, Self>
    where
        T: TermData,
    {
        Box::new(
            self.dataset
                .borrow()
                .quads_matching(&ANY, &ANY, o, &self.gmatcher)
                .map_ok(StreamedTriple::from_quad),
        )
    }
    fn triples_with_sp<'s, T, U>(
        &'s self,
        s: &'s Term<T>,
        p: &'s Term<U>,
    ) -> GTripleSource<'s, Self>
    where
        T: TermData,
        U: TermData,
    {
        Box::new(
            self.dataset
                .borrow()
                .quads_matching(s, p, &ANY, &self.gmatcher)
                .map_ok(StreamedTriple::from_quad),
        )
    }
    fn triples_with_so<'s, T, U>(
        &'s self,
        s: &'s Term<T>,
        o: &'s Term<U>,
    ) -> GTripleSource<'s, Self>
    where
        T: TermData,
        U: TermData,
    {
        Box::new(
            self.dataset
                .borrow()
                .quads_matching(s, &ANY, o, &self.gmatcher)
                .map_ok(StreamedTriple::from_quad),
        )
    }
    fn triples_with_po<'s, T, U>(
        &'s self,
        p: &'s Term<T>,
        o: &'s Term<U>,
    ) -> GTripleSource<'s, Self>
    where
        T: TermData,
        U: TermData,
    {
        Box::new(
            self.dataset
                .borrow()
                .quads_matching(&ANY, p, o, &self.gmatcher)
                .map_ok(StreamedTriple::from_quad),
        )
    }
    fn triples_with_spo<'s, T, U, V>(
        &'s self,
        s: &'s Term<T>,
        p: &'s Term<U>,
        o: &'s Term<V>,
    ) -> GTripleSource<'s, Self>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        Box::new(
            self.dataset
                .borrow()
                .quads_matching(s, p, o, &self.gmatcher)
                .map_ok(StreamedTriple::from_quad),
        )
    }
}

impl<D, E, F> MutableGraph for DatasetGraph<D, E, Option<Term<F>>>
where
    D: MutableDataset,
    E: BorrowMut<D>,
    F: TermData,
{
    type MutationError = D::MutationError;

    fn insert<T, U, V>(&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> MGResult<Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        self.dataset
            .borrow_mut()
            .insert(s, p, o, self.gmatcher.as_ref())
    }

    fn remove<T, U, V>(&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> MGResult<Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        self.dataset
            .borrow_mut()
            .remove(s, p, o, self.gmatcher.as_ref())
    }
}

impl<D, E, F> SetGraph for DatasetGraph<D, E, Option<Term<F>>>
where
    D: SetDataset,
    E: Borrow<D>,
    F: TermData,
{
}

#[cfg(test)]
pub(crate) mod test {
    // for some reason, test_graph_impl! only works in mod graph,
    // so this macro invocation is done in ::graph::adapter::test::dataset.
    //
    // the boiler plate code is still defined here, and made visible for that module.
    use super::*;
    use crate::dataset::test::*;
    use crate::dataset::MDResult;
    use crate::dataset::*;
    use crate::ns::rdfs;
    use crate::term::BoxTerm;
    use std::collections::HashSet;

    pub type MyQuad = ([BoxTerm; 3], Option<BoxTerm>);
    pub type MyDataset = HashSet<MyQuad>;
    pub type MyDatasetGraph = DatasetGraph<MyDataset, MyDataset, Option<BoxTerm>>;

    pub fn make_default_graph() -> MyDatasetGraph {
        DatasetGraph {
            dataset: MyDataset::new(),
            gmatcher: None,
            _phantom: PhantomData,
        }
    }

    pub fn make_named_graph() -> MyDatasetGraph {
        DatasetGraph {
            dataset: MyDataset::new(),
            gmatcher: Some(BoxTerm::from(&rdfs::Resource)),
            _phantom: PhantomData,
        }
    }

    // call to test_impl_graph! has been moved to ::graph::adapter::test::dataset,
    // because I couldn't call it from this module...

    #[test]
    fn test_graph_default() -> MDResult<MyDataset, ()> {
        let mut d = MyDataset::new();
        populate(&mut d)?;
        assert_eq!(d.graph(*DG).triples().count(), 4);
        assert_eq!(d.graph(*GN1).triples().count(), 6);
        assert_eq!(d.graph(*GN2).triples().count(), 7);
        assert_eq!(d.union_graph(ANY).triples().count(), 17);
        assert_eq!(
            d.union_graph(vec![DG.clone(), GN1.clone()])
                .triples()
                .count(),
            10
        );
        assert_eq!(
            d.union_graph(|x: Option<&Term<&str>>| (x == *DG || x == *GN2))
                .triples()
                .count(),
            11
        );
        Ok(())
    }
}
