// this module is transparently re-exported by its parent `dataset::inmem`

use std::collections::{HashMap, HashSet};
use std::iter::empty;

use super::*;
use crate::graph::index::*;

/// A [`DatasetWrapper`](trait.DatasetWrapper.html)
/// indexing quads by graph identifier, then by subject, then by predicate, then by object.
///
/// Compared to its wrapped dataset,
/// it overrides the methods that can efficiently be implemented using this index.
///
/// Since it must be able to produce quads instead of the underlying datasets,
/// it is limited to wrapping datasets whose quads are `([&Term<H>;3], &GraphId<H>)`.
///
#[derive(Default)]
pub struct GspoWrapper<T>
where
    T: IndexedDataset,
{
    wrapped: T,
    g2s: HashMap<T::Index, Vec<T::Index>>,
    gs2p: HashMap<[T::Index; 2], Vec<T::Index>>,
    gsp2o: HashMap<[T::Index; 3], Vec<T::Index>>,
}

impl<T> GspoWrapper<T>
where
    T: IndexedDataset + Default,
    T::Index: Default,
{
    pub fn new() -> Self {
        Self::default()
    }
}

type MyQuad<'a, T> = ([&'a Term<T>; 3], &'a GraphId<T>);

impl<'a, T> DatasetWrapper<'a> for GspoWrapper<T>
where
    T: IndexedDataset + Dataset<'a, Quad = MyQuad<'a, <T as IndexedDataset>::TermData>>,
{
    type Wrapped = T;

    fn get_wrapped(&'a self) -> &'a T {
        &self.wrapped
    }

    fn get_wrapped_mut(&'a mut self) -> &'a mut T {
        &mut self.wrapped
    }

    fn dw_quads_with_g<U>(&'a self, g: &'a GraphId<U>) -> DQuadSource<'a, Self::Wrapped>
    where
        U: TermData,
    {
        if let Some(gi) = self.wrapped.get_index_for_graph_id(g) {
            if let Some(sis) = self.g2s.get(&gi) {
                let g = self.wrapped.get_graph_id(gi).unwrap();
                return Box::new(sis.iter().flat_map(move |si| {
                    let s = self.wrapped.get_term(*si).unwrap();
                    let pis = self.gs2p.get(&[gi, *si]).unwrap();
                    pis.iter().flat_map(move |pi| {
                        let p = self.wrapped.get_term(*pi).unwrap();
                        let ois = self.gsp2o.get(&[gi, *si, *pi]).unwrap();
                        ois.iter().map(move |oi| {
                            let o = self.wrapped.get_term(*oi).unwrap();
                            Ok(([s, p, o], g))
                        })
                    })
                }));
            }
        }
        Box::new(empty())
    }

    fn dw_quads_with_sg<U, V>(
        &'a self,
        s: &'a Term<U>,
        g: &'a GraphId<V>,
    ) -> DQuadSource<'a, Self::Wrapped>
    where
        U: TermData,
        V: TermData,
    {
        if let Some(gi) = self.wrapped.get_index_for_graph_id(g) {
            if let Some(si) = self.wrapped.get_index(s) {
                if let Some(pis) = self.gs2p.get(&[gi, si]) {
                    let g = self.wrapped.get_graph_id(gi).unwrap();
                    let s = self.wrapped.get_term(si).unwrap();
                    return Box::new(pis.iter().flat_map(move |pi| {
                        let p = self.wrapped.get_term(*pi).unwrap();
                        let ois = self.gsp2o.get(&[gi, si, *pi]).unwrap();
                        ois.iter().map(move |oi| {
                            let o = self.wrapped.get_term(*oi).unwrap();
                            Ok(([s, p, o], g))
                        })
                    }));
                }
            }
        }
        Box::new(empty())
    }

    fn dw_quads_with_spg<U, V, W>(
        &'a self,
        s: &'a Term<U>,
        p: &'a Term<V>,
        g: &'a GraphId<W>,
    ) -> DQuadSource<'a, Self::Wrapped>
    where
        U: TermData,
        V: TermData,
        W: TermData,
    {
        if let Some(gi) = self.wrapped.get_index_for_graph_id(g) {
            if let Some(si) = self.wrapped.get_index(s) {
                if let Some(pi) = self.wrapped.get_index(p) {
                    let g = self.wrapped.get_graph_id(gi).unwrap();
                    let s = self.wrapped.get_term(si).unwrap();
                    let p = self.wrapped.get_term(pi).unwrap();
                    let ois = self.gsp2o.get(&[gi, si, pi]).unwrap();
                    return Box::new(ois.iter().map(move |oi| {
                        let o = self.wrapped.get_term(*oi).unwrap();
                        Ok(([s, p, o], g))
                    }));
                }
            }
        }
        Box::new(empty())
    }

    fn dw_graph_names(&'a self) -> DResultTermSet<'a, Self::Wrapped> {
        let graph_names: HashSet<_> = self
            .g2s
            .keys()
            .filter_map(|i| self.wrapped.get_term(*i)) // NB: filters out GraphId::Default
            .cloned()
            .collect();
        Ok(graph_names)
    }
}

impl<T> IndexedDatasetWrapper<T> for GspoWrapper<T>
where
    T: IndexedDataset,
{
    #[allow(clippy::collapsible_if)] // it is more regular that way
    #[inline]
    fn idw_hook_insert_indexed(&mut self, modified: &Option<[T::Index; 4]>) {
        if let Some([si, pi, oi, gi]) = *modified {
            if insert_in_index(&mut self.gsp2o, [gi, si, pi], oi) {
                if insert_in_index(&mut self.gs2p, [gi, si], pi) {
                    insert_in_index(&mut self.g2s, gi, si);
                }
            }
        }
    }

    #[allow(clippy::collapsible_if)] // it is more regular that way
    #[inline]
    fn idw_hook_remove_indexed(&mut self, modified: &Option<[T::Index; 4]>) {
        if let Some([si, pi, oi, gi]) = *modified {
            if remove_from_index(&mut self.gsp2o, [gi, si, pi], oi) {
                if remove_from_index(&mut self.gs2p, [gi, si], pi) {
                    remove_from_index(&mut self.g2s, gi, si);
                }
            }
        }
    }

    #[inline]
    fn idw_hook_shrink_to_fit(&mut self) {
        self.g2s.shrink_to_fit();
        self.gs2p.shrink_to_fit();
        self.gsp2o.shrink_to_fit();
    }
}

impl<'a, T> Dataset<'a> for GspoWrapper<T>
where
    T: IndexedDataset + Dataset<'a, Quad = MyQuad<'a, <T as IndexedDataset>::TermData>>,
{
    impl_dataset_for_wrapper!();
}

impl<T> IndexedDataset for GspoWrapper<T>
where
    T: IndexedDataset + for<'a> Dataset<'a, Quad = MyQuad<'a, <T as IndexedDataset>::TermData>>,
{
    impl_indexed_dataset_for_wrapper!();
}

impl<T> MutableDataset for GspoWrapper<T>
where
    T: IndexedDataset + for<'a> Dataset<'a, Quad = MyQuad<'a, <T as IndexedDataset>::TermData>>,
{
    impl_mutable_dataset_for_indexed_mutable_dataset!();
}

impl<T> SetDataset for GspoWrapper<T> where T: IndexedDataset + SetDataset {}

#[cfg(test)]
type GspoDataset = GspoWrapper<LightDataset>;
#[cfg(test)]
test_dataset_impl!(GspoDataset);
