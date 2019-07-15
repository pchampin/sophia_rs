// this module is transparently re-exported by its parent `graph::inmem`

use std::collections::{HashMap, HashSet};
use std::iter::empty;

use super::*;

/// A [`GraphWrapper`](trait.GraphWrapper.html)
/// indexing triples by object, then by predicate, then by subject.
///
/// Compared to its wrapped graph,
/// it overrides the methods that can efficiently be implemented using this index.
///
/// Since it must be able to produce triples instead of the underlying graphs,
/// it is limited to wrapping graphs whose triples are `[&Term<H>;3]`.
///
#[derive(Default)]
pub struct OpsWrapper<T>
where
    T: IndexedGraph,
{
    wrapped: T,
    o2p: HashMap<T::Index, Vec<T::Index>>,
    po2s: HashMap<[T::Index;2], Vec<T::Index>>,
}

impl<T> OpsWrapper<T>
where
    T: IndexedGraph + Default,
    T::Index: Default,
{
    pub fn new() -> Self {
        Self::default()
    }
}

impl<'a, T> GraphWrapper<'a> for OpsWrapper<T>
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

    fn gw_triples_with_o<U>(&'a self, o: &'a Term<U>) -> GTripleSource<'a, Self::Wrapped>
    where
        U: TermData,
    {
        if let Some(oi) = self.wrapped.get_index(o) {
            if let Some(pis) = self.o2p.get(&oi) {
                let o = self.wrapped.get_term(oi).unwrap();
                return Box::new(
                    pis.iter()
                        .flat_map(move |pi| {
                            let p = self.wrapped.get_term(*pi).unwrap();
                            let sis = &self.po2s[&[*pi, oi]];
                            sis.iter()
                                .map(move |si| {
                                    let s = self.wrapped.get_term(*si).unwrap();
                                    Ok([s, p, o])
                                })
                        })
                );
            }
        }
        Box::new(empty())
    }

    fn gw_triples_with_po<U, V>(
        &'a self,
        p: &'a Term<U>,
        o: &'a Term<V>,
    ) -> GTripleSource<'a, Self::Wrapped>
    where
        U: TermData,
        V: TermData,
    {
        if let Some(pi) = self.wrapped.get_index(p) {
            if let Some(oi) = self.wrapped.get_index(o) {
                if let Some(sis) = self.po2s.get(&[pi, oi]) {
                    let p = self.wrapped.get_term(pi).unwrap();
                    let o = self.wrapped.get_term(oi).unwrap();
                    return Box::new(
                        sis.iter()
                            .map(move |si| {
                                let s = self.wrapped.get_term(*si).unwrap();
                                Ok([s, p, o])
                            }),
                    );
                }
            }
        }
        Box::new(empty())
    }

    fn gw_objects(&'a self) -> GResultTermSet<'a, Self::Wrapped> {
        let objects: HashSet<_> = self
            .o2p
            .keys()
            .map(|i| self.wrapped.get_term(*i).unwrap().clone())
            .collect();
        Ok(objects)
    }
}

impl<T> IndexedGraphWrapper<T> for OpsWrapper<T>
where
    T: IndexedGraph,
{
    #[inline]
    fn igw_hook_insert_indexed(&mut self, modified: &Option<[T::Index; 3]>) {
        if let Some([si, pi, oi]) = *modified {
            if insert_in_index(&mut self.po2s, [pi, oi], si) {
                insert_in_index(&mut self.o2p, oi, pi);
            }
        }
    }

    #[inline]
    fn igw_hook_remove_indexed(&mut self, modified: &Option<[T::Index; 3]>) {
        if let Some([si, pi, oi]) = *modified {
            if remove_from_index(&mut self.po2s, [pi, oi], si) {
                remove_from_index(&mut self.o2p, oi, pi);
            }
        }
    }

    #[inline]
    fn igw_hook_shrink_to_fit(&mut self) {
        self.o2p.shrink_to_fit();
        self.po2s.shrink_to_fit();
    }
}



impl<'a, T> Graph<'a> for OpsWrapper<T>
where
    T: IndexedGraph + Graph<'a, Triple = [&'a Term<<T as IndexedGraph>::TermData>; 3]>,
{
    impl_graph_for_wrapper!();
}

impl<T> IndexedGraph for OpsWrapper<T>
where
    T: IndexedGraph + for <'a> Graph<'a, Triple = [&'a Term<<T as IndexedGraph>::TermData>; 3]>,
{
    impl_indexed_graph_for_wrapper!();
}


impl<T> MutableGraph for OpsWrapper<T>
where
    T: IndexedGraph + for<'a> Graph<'a, Triple = [&'a Term<<T as IndexedGraph>::TermData>; 3]>,
{
    impl_mutable_graph_for_indexed_mutable_graph!();
}

impl<T> SetGraph for OpsWrapper<T> where T: IndexedGraph + SetGraph {}

#[cfg(test)]
type OpsGraph = OpsWrapper<LightGraph>;
#[cfg(test)]
test_graph_impl!(OpsGraph);
