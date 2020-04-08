// this module is transparently re-exported by its parent `dataset::inmem`

use std::collections::{HashMap, HashSet};
use std::iter::empty;

use super::*;

use crate::graph::indexed::*;
use crate::quad::streaming_mode::{ByTermRefs, StreamedQuad};

/// A [`DatasetWrapper`](trait.DatasetWrapper.html)
/// indexing quads by graph name, then by subject, then by predicate, then by object.
///
/// Compared to its wrapped dataset,
/// it overrides the methods that can efficiently be implemented using this index.
///
/// Since it must be able to produce quads instead of the underlying datasets,
/// it is limited to wrapping datasets whose quads are `([&Term<H>;3], Option<&Term<H>>)`.
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

impl<T> DatasetWrapper for GspoWrapper<T>
where
    T: IndexedDataset + Dataset<Quad = ByTermRefs<<T as IndexedDataset>::TermData>>,
{
    type Wrapped = T;

    fn get_wrapped(&self) -> &T {
        &self.wrapped
    }

    fn get_wrapped_mut(&mut self) -> &mut T {
        &mut self.wrapped
    }

    fn dw_quads_with_g<'s, U>(&'s self, g: Option<&'s Term<U>>) -> DQuadSource<'s, Self::Wrapped>
    where
        U: TermData,
    {
        if let Some(gi) = self.wrapped.get_index_for_graph_name(g) {
            if let Some(sis) = self.g2s.get(&gi) {
                let g = self.wrapped.get_graph_name(gi).unwrap();
                return Box::new(sis.iter().flat_map(move |si| {
                    let s = self.wrapped.get_term(*si).unwrap();
                    let pis = self.gs2p.get(&[gi, *si]).unwrap();
                    pis.iter().flat_map(move |pi| {
                        let p = self.wrapped.get_term(*pi).unwrap();
                        let ois = self.gsp2o.get(&[gi, *si, *pi]).unwrap();
                        ois.iter().map(move |oi| {
                            let o = self.wrapped.get_term(*oi).unwrap();
                            Ok(StreamedQuad::by_term_refs(s, p, o, g))
                        })
                    })
                }));
            }
        }
        Box::new(empty())
    }

    fn dw_quads_with_sg<'s, U, V>(
        &'s self,
        s: &'s Term<U>,
        g: Option<&'s Term<V>>,
    ) -> DQuadSource<'s, Self::Wrapped>
    where
        U: TermData,
        V: TermData,
    {
        if let Some(gi) = self.wrapped.get_index_for_graph_name(g) {
            if let Some(si) = self.wrapped.get_index(s) {
                if let Some(pis) = self.gs2p.get(&[gi, si]) {
                    let g = self.wrapped.get_graph_name(gi).unwrap();
                    let s = self.wrapped.get_term(si).unwrap();
                    return Box::new(pis.iter().flat_map(move |pi| {
                        let p = self.wrapped.get_term(*pi).unwrap();
                        let ois = self.gsp2o.get(&[gi, si, *pi]).unwrap();
                        ois.iter().map(move |oi| {
                            let o = self.wrapped.get_term(*oi).unwrap();
                            Ok(StreamedQuad::by_term_refs(s, p, o, g))
                        })
                    }));
                }
            }
        }
        Box::new(empty())
    }

    fn dw_quads_with_spg<'s, U, V, W>(
        &'s self,
        s: &'s Term<U>,
        p: &'s Term<V>,
        g: Option<&'s Term<W>>,
    ) -> DQuadSource<'s, Self::Wrapped>
    where
        U: TermData,
        V: TermData,
        W: TermData,
    {
        if let Some(gi) = self.wrapped.get_index_for_graph_name(g) {
            if let Some(si) = self.wrapped.get_index(s) {
                if let Some(pi) = self.wrapped.get_index(p) {
                    let g = self.wrapped.get_graph_name(gi).unwrap();
                    let s = self.wrapped.get_term(si).unwrap();
                    let p = self.wrapped.get_term(pi).unwrap();
                    let ois = self.gsp2o.get(&[gi, si, pi]).unwrap();
                    return Box::new(ois.iter().map(move |oi| {
                        let o = self.wrapped.get_term(*oi).unwrap();
                        Ok(StreamedQuad::by_term_refs(s, p, o, g))
                    }));
                }
            }
        }
        Box::new(empty())
    }

    fn dw_graph_names(&self) -> DResultTermSet<Self::Wrapped> {
        let graph_names: HashSet<_> = self
            .g2s
            .keys()
            .filter_map(|i| self.wrapped.get_term(*i)) // NB: filters out None
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

impl<T> Dataset for GspoWrapper<T>
where
    T: IndexedDataset + Dataset<Quad = ByTermRefs<<T as IndexedDataset>::TermData>>,
{
    impl_dataset_for_wrapper!();
}

impl<T> IndexedDataset for GspoWrapper<T>
where
    T: IndexedDataset + Dataset<Quad = ByTermRefs<<T as IndexedDataset>::TermData>>,
{
    impl_indexed_dataset_for_wrapper!();
}

impl<T> MutableDataset for GspoWrapper<T>
where
    T: IndexedDataset + Dataset<Quad = ByTermRefs<<T as IndexedDataset>::TermData>>,
{
    impl_mutable_dataset_for_indexed_dataset!();
}

impl<T> SetDataset for GspoWrapper<T>
where
    T: IndexedDataset + Dataset<Quad = ByTermRefs<<T as IndexedDataset>::TermData>>,
    T: SetDataset,
{
}

#[cfg(test)]
type GspoDataset = GspoWrapper<LightDataset>;
#[cfg(test)]
test_dataset_impl!(GspoDataset);
