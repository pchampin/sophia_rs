// this module is transparently re-exported by its parent `graphe`

use std::collections::HashMap;
use std::iter::empty;

use super::*;
use crate::graph::index::remove_one_val;

/// A [`GraphWrapper`](trait.GraphWrapper.html)
/// indexing triples by object, then by predicate, then by subject.
/// 
/// Compared to its wrapped graph,
/// it overrides the methods that can efficiently be implemented using this index.
/// 
/// Since it must be able to produce triples instead of the underlying graphs,
/// it is limited to wrapping graphs whose triples are `[&Triple<H>]`.
/// 
#[derive(Default)]
pub struct OpsWrapper<T> where
    T: IndexedGraph,
{
    wrapped: T,
    o2p: HashMap<T::Index, Vec<T::Index>>,
    po2s: HashMap<(T::Index, T::Index), Vec<T::Index>>,
}

impl<T> OpsWrapper<T> where
    T: IndexedGraph + Default,
    T::Index: Default,
{
    pub fn new() -> Self {
        Self::default()
    }
}

impl<'a, T> GraphWrapper<'a> for OpsWrapper<T> where
    T: IndexedGraph + Graph<'a, Triple=[&'a Term<<T as IndexedGraph>::Holder>;3]>,
{
    type Wrapped = T;

    fn get_wrapped(&'a self) -> &'a T {
        &self.wrapped
    }

    fn get_wrapped_mut(&'a mut self) -> &'a mut T {
        &mut self.wrapped
    }

    fn gw_triples_with_o<U> (&'a self, o: &'a Term<U>) -> GTripleSource<'a, Self::Wrapped>
    where
        U: Borrow<str>,
    {
        if let Some(oi) = self.wrapped.get_index(o) {
            if let Some(pis) = self.o2p.get(&oi) {
                let o = self.wrapped.get_term(oi).unwrap();
                return Box::new(
                    pis.iter()
                    .map(move |pi| (
                        self.po2s.get(&(*pi, oi)).unwrap(),
                        self.wrapped.get_term(*pi).unwrap(),
                        o,
                    ))
                    .flat_map(move |(sis,p,o)|
                        sis.iter()
                        .map(move |si| Ok([
                            self.wrapped.get_term(*si).unwrap(), p, o,
                        ]))
                    )
                )
            }
        }
        Box::new(empty())
    }

    fn gw_triples_with_po<U, V> (&'a self, p: &'a Term<U>, o: &'a Term<V>) -> GTripleSource<'a, Self::Wrapped>
    where
        U: Borrow<str>,
        V: Borrow<str>,
    {
        if let Some(pi) = self.wrapped.get_index(p) {
            if let Some(oi) = self.wrapped.get_index(o) {
                if let Some(sis) = self.po2s.get(&(pi, oi)) {
                    let p = self.wrapped.get_term(pi).unwrap();
                    let o = self.wrapped.get_term(oi).unwrap();
                    return Box::new(
                        sis.iter()
                        .map(move |si| Ok([
                            self.wrapped.get_term(*si).unwrap(), p, o,
                        ]))
                    )
                }
            }
        }
        Box::new(empty())
    }
}

impl<T> IndexedGraph for OpsWrapper<T> where
    T: IndexedGraph,
{
    type Index = T::Index;
    type Holder = T::Holder;

    #[inline]
    fn get_index<U> (&self, t: &Term<U>) -> Option<Self::Index> where
        U: Borrow<str>,
    {
        self.wrapped.get_index(t)
    }

    #[inline]
    fn get_term<'a>(&'a self, i: Self::Index) -> Option<&Term<Self::Holder>>
    {
        self.wrapped.get_term(i)
    }

    fn insert_indexed<U, V, W> (&mut self, s: &Term<U>, p: &Term<V>, o: &Term<W>) -> Option<[Self::Index;3]> where
        U: Borrow<str>,
        V: Borrow<str>,
        W: Borrow<str>,
    {
        let modified = self.wrapped.insert_indexed(s, p, o);
        if let Some([si, pi, oi]) = modified {
            self.o2p.entry(oi).or_insert_with(|| Vec::new()).push(pi);
            self.po2s.entry((pi, oi)).or_insert_with(|| Vec::new()).push(si);
        }
        modified
    }

    fn remove_indexed<U, V, W> (&mut self, s: &Term<U>, p: &Term<V>, o: &Term<W>) -> Option<[Self::Index;3]> where
        U: Borrow<str>,
        V: Borrow<str>,
        W: Borrow<str>,
    {
        let modified = self.wrapped.remove_indexed(s, p, o);
        if let Some([si, pi, oi]) = modified {
            remove_one_val(&mut self.o2p, oi, pi);
            remove_one_val(&mut self.po2s, (pi, oi), si);
        }
        modified
    }

    fn shrink_to_fit(&mut self) {
        self.wrapped.shrink_to_fit();
        self.o2p.shrink_to_fit();
        self.po2s.shrink_to_fit();
    }
}

impl<'a, T> Graph<'a> for OpsWrapper<T> where
    T: IndexedGraph + Graph<'a, Triple=[&'a Term<<T as IndexedGraph>::Holder>;3]>,
{
    impl_graph_for_wrapper!();
}

impl<T> MutableGraph for OpsWrapper<T> where
    T: IndexedGraph + for <'a> Graph<'a, Triple=[&'a Term<<T as IndexedGraph>::Holder>;3]>,
{
    impl_mutable_graph_for_indexed_mutable_graph!();
}

impl<T> SetGraph for OpsWrapper<T> where
    T: IndexedGraph + SetGraph,
{}


#[cfg(test)]
type OpsGraph = OpsWrapper<LightGraph>;
#[cfg(test)]
test_graph_impl!(OpsGraph);
