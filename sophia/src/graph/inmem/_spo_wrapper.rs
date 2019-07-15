// this module is transparently re-exported by its parent `graph::inmem`

use std::collections::{HashMap, HashSet};
use std::iter::empty;

use super::*;

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
    sp2o: HashMap<[T::Index;2], Vec<T::Index>>,
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

impl<'a, T> GraphWrapper<'a> for SpoWrapper<T>
where
    T: IndexedGraph + Graph<'a, Triple = [&'a Term<<T as IndexedGraph>::TermData>; 3]>,
{
    type Wrapped = T;

    fn get_wrapped(&'a self) -> &'a T {
        &self.wrapped
    }

    fn get_wrapped_mut(&'a mut self) -> &'a mut T {
        &mut self.wrapped
    }

    fn gw_triples_with_s<U>(&'a self, s: &'a Term<U>) -> GTripleSource<'a, Self::Wrapped>
    where
        U: TermData,
    {
        if let Some(si) = self.wrapped.get_index(s) {
            if let Some(pis) = self.s2p.get(&si) {
                let s = self.wrapped.get_term(si).unwrap();
                return Box::new(
                    pis.iter()
                        .flat_map(move |pi| {
                            let p = self.wrapped.get_term(*pi).unwrap();
                            let ois = &self.sp2o[&[si, *pi]];
                            ois.iter()
                                .map(move |oi| {
                                    let o = self.wrapped.get_term(*oi).unwrap();
                                    Ok([s, p, o])
                                })
                        })
                );
            }
        }
        Box::new(empty())
    }

    fn gw_triples_with_sp<U, V>(
        &'a self,
        s: &'a Term<U>,
        p: &'a Term<V>,
    ) -> GTripleSource<'a, Self::Wrapped>
    where
        U: TermData,
        V: TermData,
    {
        if let Some(si) = self.wrapped.get_index(s) {
            if let Some(pi) = self.wrapped.get_index(p) {
                if let Some(ois) = self.sp2o.get(&[si, pi]) {
                    let s = self.wrapped.get_term(si).unwrap();
                    let p = self.wrapped.get_term(pi).unwrap();
                    return Box::new(
                        ois.iter()
                            .map(move |oi| {
                                let o = self.wrapped.get_term(*oi).unwrap();
                                Ok([s, p, o])
                            }),
                    );
                }
            }
        }
        Box::new(empty())
    }

    fn gw_subjects(&'a self) -> GResultTermSet<'a, Self::Wrapped> {
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


impl<'a, T> Graph<'a> for SpoWrapper<T>
where
    T: IndexedGraph + Graph<'a, Triple = [&'a Term<<T as IndexedGraph>::TermData>; 3]>,
{
    impl_graph_for_wrapper!();
}

impl<T> IndexedGraph for SpoWrapper<T>
where
    T: IndexedGraph + for <'a> Graph<'a, Triple = [&'a Term<<T as IndexedGraph>::TermData>; 3]>,
{
    impl_indexed_graph_for_wrapper!();
}

impl<T> MutableGraph for SpoWrapper<T>
where
    T: IndexedGraph + for<'a> Graph<'a, Triple = [&'a Term<<T as IndexedGraph>::TermData>; 3]>,
{
    impl_mutable_graph_for_indexed_mutable_graph!();
}

impl<T> SetGraph for SpoWrapper<T> where T: IndexedGraph + SetGraph {}

#[cfg(test)]
type SpoGraph = super::SpoWrapper<super::LightGraph>;
#[cfg(test)]
test_graph_impl!(SpoGraph);
