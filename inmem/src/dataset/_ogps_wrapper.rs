// this module is transparently re-exported by its parent `dataset::inmem`

use std::collections::{HashMap, HashSet};
use std::iter::empty;

use super::*;
use sophia_api::dataset::{DQuadSource, DResultTermSet};
use sophia_api::quad::streaming_mode::{ByTermRefs, StreamedQuad};
use sophia_api::term::TTerm;
use sophia_indexed::graph::*;

/// A [`DatasetWrapper`](trait.DatasetWrapper.html)
/// indexing quads by object, then by graph name, then by predicate, then by subject.
///
/// Compared to its wrapped dataset,
/// it overrides the methods that can efficiently be implemented using this index.
///
/// Since it must be able to produce quads instead of the underlying datasets,
/// it is limited to wrapping datasets whose quads are `([&Term<H>;3], Option<&Term<H>>)`.
///
#[derive(Default)]
pub struct OgpsWrapper<T>
where
    T: IndexedDataset,
{
    wrapped: T,
    o2g: HashMap<T::Index, Vec<T::Index>>,
    og2p: HashMap<[T::Index; 2], Vec<T::Index>>,
    ogp2s: HashMap<[T::Index; 3], Vec<T::Index>>,
}

impl<T> OgpsWrapper<T>
where
    T: IndexedDataset + Default,
    T::Index: Default,
{
    /// Build a new `DatasetWrapper` that indexes quads by object,
    /// then by graph name, then by predicate, then by subject.
    pub fn new() -> Self {
        Self::default()
    }
}

impl<T> DatasetWrapper for OgpsWrapper<T>
where
    T: IndexedDataset + Dataset<Quad = ByTermRefs<Term<<T as IndexedDataset>::TermData>>>,
{
    type Wrapped = T;

    fn get_wrapped(&self) -> &T {
        &self.wrapped
    }

    fn get_wrapped_mut(&mut self) -> &mut T {
        &mut self.wrapped
    }

    fn dw_quads_with_o<'s, TO>(&'s self, o: &'s TO) -> DQuadSource<'s, Self::Wrapped>
    where
        TO: TTerm + ?Sized,
    {
        if let Some(oi) = self.wrapped.get_index(o) {
            if let Some(gis) = self.o2g.get(&oi) {
                let o = self.wrapped.get_term(oi).unwrap();
                return Box::new(gis.iter().flat_map(move |gi| {
                    let g = self.wrapped.get_graph_name(*gi).unwrap();
                    let pis = self.og2p.get(&[oi, *gi]).unwrap();
                    pis.iter().flat_map(move |pi| {
                        let p = self.wrapped.get_term(*pi).unwrap();
                        let sis = self.ogp2s.get(&[oi, *gi, *pi]).unwrap();
                        sis.iter().map(move |si| {
                            let s = self.wrapped.get_term(*si).unwrap();
                            Ok(StreamedQuad::by_term_refs(s, p, o, g))
                        })
                    })
                }));
            }
        }
        Box::new(empty())
    }

    fn dw_quads_with_og<'s, TO, TG>(
        &'s self,
        o: &'s TO,
        g: Option<&'s TG>,
    ) -> DQuadSource<'s, Self::Wrapped>
    where
        TO: TTerm + ?Sized,
        TG: TTerm + ?Sized,
    {
        if let Some(oi) = self.wrapped.get_index(o) {
            if let Some(gi) = self.wrapped.get_index_for_graph_name(g) {
                if let Some(pis) = self.og2p.get(&[oi, gi]) {
                    let o = self.wrapped.get_term(oi).unwrap();
                    let g = self.wrapped.get_graph_name(gi).unwrap();
                    return Box::new(pis.iter().flat_map(move |pi| {
                        let p = self.wrapped.get_term(*pi).unwrap();
                        let sis = self.ogp2s.get(&[oi, gi, *pi]).unwrap();
                        sis.iter().map(move |si| {
                            let s = self.wrapped.get_term(*si).unwrap();
                            Ok(StreamedQuad::by_term_refs(s, p, o, g))
                        })
                    }));
                }
            }
        }
        Box::new(empty())
    }

    fn dw_quads_with_pog<'s, TP, TO, TG>(
        &'s self,
        p: &'s TP,
        o: &'s TO,
        g: Option<&'s TG>,
    ) -> DQuadSource<'s, Self::Wrapped>
    where
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
        TG: TTerm + ?Sized,
    {
        if let Some(pi) = self.wrapped.get_index(p) {
            if let Some(oi) = self.wrapped.get_index(o) {
                if let Some(gi) = self.wrapped.get_index_for_graph_name(g) {
                    if let Some(sis) = self.ogp2s.get(&[oi, gi, pi]) {
                        let p = self.wrapped.get_term(pi).unwrap();
                        let o = self.wrapped.get_term(oi).unwrap();
                        let g = self.wrapped.get_graph_name(gi).unwrap();
                        return Box::new(sis.iter().map(move |si| {
                            let s = self.wrapped.get_term(*si).unwrap();
                            Ok(StreamedQuad::by_term_refs(s, p, o, g))
                        }));
                    }
                }
            }
        }
        Box::new(empty())
    }

    fn dw_objects(&self) -> DResultTermSet<Self::Wrapped> {
        let objects: HashSet<_> = self
            .o2g
            .keys()
            .map(|i| self.wrapped.get_term(*i).unwrap().clone())
            .collect();
        Ok(objects)
    }
}

impl<T> IndexedDatasetWrapper<T> for OgpsWrapper<T>
where
    T: IndexedDataset,
{
    #[inline]
    fn idw_wrap_empty(dataset: T) -> Self {
        OgpsWrapper {
            wrapped: dataset,
            o2g: HashMap::default(),
            og2p: HashMap::default(),
            ogp2s: HashMap::default(),
        }
    }

    #[allow(clippy::collapsible_if)] // it is more regular that way
    #[inline]
    fn idw_hook_insert_indexed(&mut self, modified: &Option<[T::Index; 4]>) {
        if let Some([si, pi, oi, gi]) = *modified {
            if insert_in_index(&mut self.ogp2s, [oi, gi, pi], si) {
                if insert_in_index(&mut self.og2p, [oi, gi], pi) {
                    insert_in_index(&mut self.o2g, oi, gi);
                }
            }
        }
    }

    #[allow(clippy::collapsible_if)] // it is more regular that way
    #[inline]
    fn idw_hook_remove_indexed(&mut self, modified: &Option<[T::Index; 4]>) {
        if let Some([si, pi, oi, gi]) = *modified {
            if remove_from_index(&mut self.ogp2s, [oi, gi, pi], si) {
                if remove_from_index(&mut self.og2p, [oi, gi], pi) {
                    remove_from_index(&mut self.o2g, oi, gi);
                }
            }
        }
    }

    #[inline]
    fn idw_hook_shrink_to_fit(&mut self) {
        self.o2g.shrink_to_fit();
        self.og2p.shrink_to_fit();
        self.ogp2s.shrink_to_fit();
    }
}

impl<T> Dataset for OgpsWrapper<T>
where
    T: IndexedDataset + Dataset<Quad = ByTermRefs<Term<<T as IndexedDataset>::TermData>>>,
{
    impl_dataset_for_wrapper!();
}

impl<T> IndexedDataset for OgpsWrapper<T>
where
    T: IndexedDataset + Dataset<Quad = ByTermRefs<Term<<T as IndexedDataset>::TermData>>>,
{
    impl_indexed_dataset_for_wrapper!();
}

impl<T> CollectibleDataset for OgpsWrapper<T>
where
    T: IndexedDataset + Dataset<Quad = ByTermRefs<Term<<T as IndexedDataset>::TermData>>>,
{
    sophia_indexed::impl_collectible_dataset_for_indexed_dataset!();
}

impl<T> MutableDataset for OgpsWrapper<T>
where
    T: IndexedDataset + Dataset<Quad = ByTermRefs<Term<<T as IndexedDataset>::TermData>>>,
{
    sophia_indexed::impl_mutable_dataset_for_indexed_dataset!();
}

impl<T> SetDataset for OgpsWrapper<T>
where
    T: IndexedDataset + Dataset<Quad = ByTermRefs<Term<<T as IndexedDataset>::TermData>>>,
    T: IndexedDataset + SetDataset,
{
}

#[cfg(all(test, feature = "all_tests"))]
type GspoDataset = OgpsWrapper<LightDataset>;
#[cfg(all(test, feature = "all_tests"))]
sophia_api::test_dataset_impl!(GspoDataset);
