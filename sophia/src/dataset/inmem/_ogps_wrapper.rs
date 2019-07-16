// this module is transparently re-exported by its parent `dataset::inmem`

use std::collections::{HashMap, HashSet};
use std::iter::empty;

use super::*;
use crate::graph::indexed::*;

/// A [`DatasetWrapper`](trait.DatasetWrapper.html)
/// indexing quads by object, then by graph identifier, then by predicate, then by subject.
///
/// Compared to its wrapped dataset,
/// it overrides the methods that can efficiently be implemented using this index.
///
/// Since it must be able to produce quads instead of the underlying datasets,
/// it is limited to wrapping datasets whose quads are `([&Term<H>;3], &GraphId<H>)`.
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
    pub fn new() -> Self {
        Self::default()
    }
}

type MyQuad<'a, T> = ([&'a Term<T>; 3], &'a GraphId<T>);

impl<'a, T> DatasetWrapper<'a> for OgpsWrapper<T>
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

    fn dw_quads_with_o<U>(&'a self, o: &'a Term<U>) -> DQuadSource<'a, Self::Wrapped>
    where
        U: TermData,
    {
        if let Some(oi) = self.wrapped.get_index(o) {
            if let Some(gis) = self.o2g.get(&oi) {
                let o = self.wrapped.get_term(oi).unwrap();
                return Box::new(gis.iter().flat_map(move |gi| {
                    let g = self.wrapped.get_graph_id(*gi).unwrap();
                    let pis = self.og2p.get(&[oi, *gi]).unwrap();
                    pis.iter().flat_map(move |pi| {
                        let p = self.wrapped.get_term(*pi).unwrap();
                        let sis = self.ogp2s.get(&[oi, *gi, *pi]).unwrap();
                        sis.iter().map(move |si| {
                            let s = self.wrapped.get_term(*si).unwrap();
                            Ok(([s, p, o], g))
                        })
                    })
                }));
            }
        }
        Box::new(empty())
    }

    fn dw_quads_with_og<U, V>(
        &'a self,
        o: &'a Term<U>,
        g: &'a GraphId<V>,
    ) -> DQuadSource<'a, Self::Wrapped>
    where
        U: TermData,
        V: TermData,
    {
        if let Some(oi) = self.wrapped.get_index(o) {
            if let Some(gi) = self.wrapped.get_index_for_graph_id(g) {
                if let Some(pis) = self.og2p.get(&[oi, gi]) {
                    let o = self.wrapped.get_term(oi).unwrap();
                    let g = self.wrapped.get_graph_id(gi).unwrap();
                    return Box::new(pis.iter().flat_map(move |pi| {
                        let p = self.wrapped.get_term(*pi).unwrap();
                        let sis = self.ogp2s.get(&[oi, gi, *pi]).unwrap();
                        sis.iter().map(move |si| {
                            let s = self.wrapped.get_term(*si).unwrap();
                            Ok(([s, p, o], g))
                        })
                    }));
                }
            }
        }
        Box::new(empty())
    }

    fn dw_quads_with_pog<U, V, W>(
        &'a self,
        p: &'a Term<U>,
        o: &'a Term<V>,
        g: &'a GraphId<W>,
    ) -> DQuadSource<'a, Self::Wrapped>
    where
        U: TermData,
        V: TermData,
        W: TermData,
    {
        if let Some(pi) = self.wrapped.get_index(p) {
            if let Some(oi) = self.wrapped.get_index(o) {
                if let Some(gi) = self.wrapped.get_index_for_graph_id(g) {
                    if let Some(sis) = self.ogp2s.get(&[oi, gi, pi]) {
                        let p = self.wrapped.get_term(pi).unwrap();
                        let o = self.wrapped.get_term(oi).unwrap();
                        let g = self.wrapped.get_graph_id(gi).unwrap();
                        return Box::new(sis.iter().map(move |si| {
                            let s = self.wrapped.get_term(*si).unwrap();
                            Ok(([s, p, o], g))
                        }));
                    }
                }
            }
        }
        Box::new(empty())
    }

    fn dw_objects(&'a self) -> DResultTermSet<'a, Self::Wrapped> {
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

impl<'a, T> Dataset<'a> for OgpsWrapper<T>
where
    T: IndexedDataset + Dataset<'a, Quad = MyQuad<'a, <T as IndexedDataset>::TermData>>,
{
    impl_dataset_for_wrapper!();
}

impl<T> IndexedDataset for OgpsWrapper<T>
where
    T: IndexedDataset + for<'a> Dataset<'a, Quad = MyQuad<'a, <T as IndexedDataset>::TermData>>,
{
    impl_indexed_dataset_for_wrapper!();
}

impl<T> MutableDataset for OgpsWrapper<T>
where
    T: IndexedDataset + for<'a> Dataset<'a, Quad = MyQuad<'a, <T as IndexedDataset>::TermData>>,
{
    impl_mutable_dataset_for_indexed_dataset!();
}

impl<T> SetDataset for OgpsWrapper<T> where T: IndexedDataset + SetDataset {}

#[cfg(test)]
type GspoDataset = OgpsWrapper<LightDataset>;
#[cfg(test)]
test_dataset_impl!(GspoDataset);
