//! Adapters for exposing `Datasets` as other traits.

use std::borrow::{Borrow, BorrowMut};
use std::marker::PhantomData;

use resiter::Map;

use crate::dataset::{Dataset, MutableDataset, SetDataset};
use crate::graph::*;
use crate::quad::{Quad, QuadAsTriple};
use crate::term::matcher::{GraphNameMatcher, ANY};
use crate::term::{Term, TermData};

/// The adapter returned by
/// [`Dataset::union_graph`](../trait.Dataset.html#method.union_graph)
pub struct DatasetGraph<D: ?Sized, E, M: GraphNameMatcher> {
    pub(in crate::dataset) dataset: E,
    pub(in crate::dataset) gmatcher: M,
    pub(in crate::dataset) _phantom: PhantomData<D>,
}

impl<'a, D, E, M> Graph<'a> for DatasetGraph<D, E, M>
where
    D: Dataset<'a> + ?Sized,
    E: Borrow<D>,
    M: GraphNameMatcher,
{
    type Triple = QuadAsTriple<D::Quad>;
    type Error = D::Error;

    fn triples(&'a self) -> GTripleSource<'a, Self> {
        Box::new(
            self.dataset
                .borrow()
                .quads_matching(&ANY, &ANY, &ANY, &self.gmatcher)
                .map_ok(Quad::as_triple),
        )
    }
    fn triples_with_s<T>(&'a self, s: &'a Term<T>) -> GTripleSource<'a, Self>
    where
        T: TermData,
    {
        Box::new(
            self.dataset
                .borrow()
                .quads_matching(s, &ANY, &ANY, &self.gmatcher)
                .map_ok(Quad::as_triple),
        )
    }
    fn triples_with_p<T>(&'a self, p: &'a Term<T>) -> GTripleSource<'a, Self>
    where
        T: TermData,
    {
        Box::new(
            self.dataset
                .borrow()
                .quads_matching(&ANY, p, &ANY, &self.gmatcher)
                .map_ok(Quad::as_triple),
        )
    }
    fn triples_with_o<T>(&'a self, o: &'a Term<T>) -> GTripleSource<'a, Self>
    where
        T: TermData,
    {
        Box::new(
            self.dataset
                .borrow()
                .quads_matching(&ANY, &ANY, o, &self.gmatcher)
                .map_ok(Quad::as_triple),
        )
    }
    fn triples_with_sp<T, U>(&'a self, s: &'a Term<T>, p: &'a Term<U>) -> GTripleSource<'a, Self>
    where
        T: TermData,
        U: TermData,
    {
        Box::new(
            self.dataset
                .borrow()
                .quads_matching(s, p, &ANY, &self.gmatcher)
                .map_ok(Quad::as_triple),
        )
    }
    fn triples_with_so<T, U>(&'a self, s: &'a Term<T>, o: &'a Term<U>) -> GTripleSource<'a, Self>
    where
        T: TermData,
        U: TermData,
    {
        Box::new(
            self.dataset
                .borrow()
                .quads_matching(s, &ANY, o, &self.gmatcher)
                .map_ok(Quad::as_triple),
        )
    }
    fn triples_with_po<T, U>(&'a self, p: &'a Term<T>, o: &'a Term<U>) -> GTripleSource<'a, Self>
    where
        T: TermData,
        U: TermData,
    {
        Box::new(
            self.dataset
                .borrow()
                .quads_matching(&ANY, p, o, &self.gmatcher)
                .map_ok(Quad::as_triple),
        )
    }
    fn triples_with_spo<T, U, V>(
        &'a self,
        s: &'a Term<T>,
        p: &'a Term<U>,
        o: &'a Term<V>,
    ) -> GTripleSource<'a, Self>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        Box::new(
            self.dataset
                .borrow()
                .quads_matching(s, p, o, &self.gmatcher)
                .map_ok(Quad::as_triple),
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

    fn insert<T, U, V>(&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> Result<bool, Self::MutationError>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        self.dataset
            .borrow_mut()
            .insert(s, p, o, self.gmatcher.as_ref())
    }

    fn remove<T, U, V>(&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> Result<bool, Self::MutationError>
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
    use crate::dataset::inmem::LightDataset;
    use crate::dataset::test::*;
    use crate::ns::rdfs;
    use crate::term::BoxTerm;
    use anyhow::Result;

    pub type LightDatasetGraph = DatasetGraph<LightDataset, LightDataset, Option<BoxTerm>>;

    pub fn make_default_graph() -> LightDatasetGraph {
        DatasetGraph {
            dataset: LightDataset::new(),
            gmatcher: None,
            _phantom: PhantomData,
        }
    }

    pub fn make_named_graph() -> LightDatasetGraph {
        DatasetGraph {
            dataset: LightDataset::new(),
            gmatcher: Some(BoxTerm::from(&rdfs::Resource)),
            _phantom: PhantomData,
        }
    }

    // call to test_impl_graph! has been moved to ::graph::adapter::test::dataset,
    // because I couldn't call it from this module...

    #[test]
    fn test_graph_default() -> Result<()> {
        let mut d = LightDataset::new();
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
