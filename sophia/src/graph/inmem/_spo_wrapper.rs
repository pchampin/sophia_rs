// this module is transparently re-exported by its parent `graph::inmem`

use std::collections::{HashMap, HashSet};
use std::iter::empty;

use super::*;
use crate::triple::streaming_mode::{ByTermRefs, StreamedTriple};
use sophia_api::term::TTerm;

/// A [`GraphWrapper`](trait.GraphWrapper.html)
/// indexing triples by subject, then by predicate, then by object.
///
/// Compared to its wrapped graph,
/// it overrides the methods that can efficiently be implemented using this index.
///
/// Since it must be able to produce triples instead of the underlying graphs,
/// it is limited to wrapping graphs whose triples are `[&Term<H>;3]`.
///
#[derive(Default)]
pub struct SpoWrapper<T>
where
    T: IndexedGraph,
{
    wrapped: T,
    s2p: HashMap<T::Index, Vec<T::Index>>,
    sp2o: HashMap<[T::Index; 2], Vec<T::Index>>,
}

impl<T> SpoWrapper<T>
where
    T: IndexedGraph + Default,
    T::Index: Default,
{
    pub fn new() -> Self {
        Self::default()
    }
}

impl<T> GraphWrapper for SpoWrapper<T>
where
    T: IndexedGraph + Graph<Triple = ByTermRefs<<T as IndexedGraph>::TermData>>,
{
    type Wrapped = T;

    fn get_wrapped(&self) -> &T {
        &self.wrapped
    }

    fn get_wrapped_mut(&mut self) -> &mut T {
        &mut self.wrapped
    }

    fn gw_triples_with_s<'s, TS>(&'s self, s: &'s TS) -> GTripleSource<'s, Self::Wrapped>
    where
        TS: TTerm + ?Sized,
    {
        if let Some(si) = self.wrapped.get_index(s) {
            if let Some(pis) = self.s2p.get(&si) {
                let s = self.wrapped.get_term(si).unwrap();
                return Box::new(pis.iter().flat_map(move |pi| {
                    let p = self.wrapped.get_term(*pi).unwrap();
                    let ois = &self.sp2o[&[si, *pi]];
                    ois.iter().map(move |oi| {
                        let o = self.wrapped.get_term(*oi).unwrap();
                        Ok(StreamedTriple::by_term_refs(s, p, o))
                    })
                }));
            }
        }
        Box::new(empty())
    }

    fn gw_triples_with_sp<'s, TS, TP>(
        &'s self,
        s: &'s TS,
        p: &'s TP,
    ) -> GTripleSource<'s, Self::Wrapped>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
    {
        if let Some(si) = self.wrapped.get_index(s) {
            if let Some(pi) = self.wrapped.get_index(p) {
                if let Some(ois) = self.sp2o.get(&[si, pi]) {
                    let s = self.wrapped.get_term(si).unwrap();
                    let p = self.wrapped.get_term(pi).unwrap();
                    return Box::new(ois.iter().map(move |oi| {
                        let o = self.wrapped.get_term(*oi).unwrap();
                        Ok(StreamedTriple::by_term_refs(s, p, o))
                    }));
                }
            }
        }
        Box::new(empty())
    }

    fn gw_subjects(&self) -> GResultTermSet<Self::Wrapped> {
        let subjects: HashSet<_> = self
            .s2p
            .keys()
            .map(|i| self.wrapped.get_term(*i).unwrap().clone())
            .collect();
        Ok(subjects)
    }
}

impl<T> IndexedGraphWrapper<T> for SpoWrapper<T>
where
    T: IndexedGraph,
{
    #[inline]
    fn igw_wrap_empty(graph: T) -> Self {
        SpoWrapper {
            wrapped: graph,
            s2p: HashMap::default(),
            sp2o: HashMap::default(),
        }
    }

    #[inline]
    fn igw_hook_insert_indexed(&mut self, modified: &Option<[T::Index; 3]>) {
        if let Some([si, pi, oi]) = *modified {
            if insert_in_index(&mut self.sp2o, [si, pi], oi) {
                insert_in_index(&mut self.s2p, si, pi);
            }
        }
    }

    #[inline]
    fn igw_hook_remove_indexed(&mut self, modified: &Option<[T::Index; 3]>) {
        if let Some([si, pi, oi]) = *modified {
            if remove_from_index(&mut self.sp2o, [si, pi], oi) {
                remove_from_index(&mut self.s2p, si, pi);
            }
        }
    }

    #[inline]
    fn igw_hook_shrink_to_fit(&mut self) {
        self.s2p.shrink_to_fit();
        self.sp2o.shrink_to_fit();
    }
}

impl<T> Graph for SpoWrapper<T>
where
    T: IndexedGraph + Graph<Triple = ByTermRefs<<T as IndexedGraph>::TermData>>,
{
    impl_graph_for_wrapper!();
}

impl<T> IndexedGraph for SpoWrapper<T>
where
    T: IndexedGraph + Graph<Triple = ByTermRefs<<T as IndexedGraph>::TermData>>,
{
    impl_indexed_graph_for_wrapper!();
}

impl<T> CollectibleGraph for SpoWrapper<T>
where
    T: IndexedGraph + Graph<Triple = ByTermRefs<<T as IndexedGraph>::TermData>>,
{
    impl_collectible_graph_for_indexed_graph!();
}

impl<T> MutableGraph for SpoWrapper<T>
where
    T: IndexedGraph + Graph<Triple = ByTermRefs<<T as IndexedGraph>::TermData>>,
{
    impl_mutable_graph_for_indexed_graph!();
}

impl<T> SetGraph for SpoWrapper<T>
where
    T: IndexedGraph + Graph<Triple = ByTermRefs<<T as IndexedGraph>::TermData>>,
    T: SetGraph,
{
}

#[cfg(all(test, feature = "all_tests"))]
type SpoGraph = super::SpoWrapper<super::LightGraph>;
#[cfg(all(test, feature = "all_tests"))]
test_graph_impl!(SpoGraph);
