// this module is transparently re-exported by its parent `graph`

use std::collections::HashMap;
use std::iter::empty;

use super::*;
use crate::graph::index::remove_one_val;

/// A [`GraphWrapper`](trait.GraphWrapper.html)
/// indexing triples by subject, then by predicate, then by object.
/// 
/// Compared to its wrapped graph,
/// it overrides the methods that can efficiently be implemented using this index.
/// 
/// Since it must be able to produce triples instead of the underlying graphs,
/// it is limited to wrapping graphs whose triples are `[&Triple<H>]`.
/// 
#[derive(Default)]
pub struct SpoWrapper<T> where
    T: IndexedGraph,
{
    wrapped: T,
    s2p: HashMap<T::Index, Vec<T::Index>>,
    sp2o: HashMap<(T::Index, T::Index), Vec<T::Index>>,
}

impl<T> SpoWrapper<T> where
    T: IndexedGraph + Default,
    T::Index: Default,
{
    pub fn new() -> Self {
        Self::default()
    }
}

impl<'a, T> GraphWrapper<'a> for SpoWrapper<T> where
    T: IndexedGraph + Graph<'a, Triple=[&'a Term<<T as IndexedGraph>::Holder>;3]>,
{
    type Wrapped = T;

    fn get_wrapped(&'a self) -> &'a T {
        &self.wrapped
    }

    fn get_wrapped_mut(&'a mut self) -> &'a mut T {
        &mut self.wrapped
    }

    fn gw_triples_with_s<U> (&'a self, s: &'a Term<U>) -> GTripleSource<'a, Self::Wrapped>
    where
        U: Borrow<str> + Clone + Eq + Hash,
    {
        if let Some(si) = self.wrapped.get_index(s) {
            if let Some(pis) = self.s2p.get(&si) {
                let s = self.wrapped.get_term(si).unwrap();
                return Box::new(
                    pis.iter()
                    .map(move |pi| (
                        s,
                        self.wrapped.get_term(*pi).unwrap(),
                        self.sp2o.get(&(si, *pi)).unwrap(),
                    ))
                    .flat_map(move |(s,p,ois)|
                        ois.iter()
                        .map(move |oi| Ok([
                            s, p, self.wrapped.get_term(*oi).unwrap(),
                        ]))
                    )
                )
            }
        }
        Box::new(empty())
    }

    fn gw_triples_with_sp<U, V> (&'a self, s: &'a Term<U>, p: &'a Term<V>) -> GTripleSource<'a, Self::Wrapped>
    where
        U: Borrow<str> + Clone + Eq + Hash,
        V: Borrow<str> + Clone + Eq + Hash,
    {
        if let Some(si) = self.wrapped.get_index(s) {
            if let Some(pi) = self.wrapped.get_index(p) {
                if let Some(ois) = self.sp2o.get(&(si, pi)) {
                    let s = self.wrapped.get_term(si).unwrap();
                    let p = self.wrapped.get_term(pi).unwrap();
                    return Box::new(
                        ois.iter()
                        .map(move |oi| Ok([
                            s, p, self.wrapped.get_term(*oi).unwrap(),
                        ]))
                    )
                }
            }
        }
        Box::new(empty())
    }
}

impl<T> IndexedGraph for SpoWrapper<T> where
    T: IndexedGraph,
{
    type Index = T::Index;
    type Holder = T::Holder;

    #[inline]
    fn get_index<U> (&self, t: &Term<U>) -> Option<Self::Index> where
        U: Borrow<str> + Clone + Eq + Hash,
    {
        self.wrapped.get_index(t)
    }

    #[inline]
    fn get_term<'a>(&'a self, i: Self::Index) -> Option<&Term<Self::Holder>>
    {
        self.wrapped.get_term(i)
    }

    fn insert_indexed<U, V, W> (&mut self, s: &Term<U>, p: &Term<V>, o: &Term<W>) -> Option<[Self::Index;3]> where
        U: Borrow<str> + Clone + Eq + Hash,
        V: Borrow<str> + Clone + Eq + Hash,
        W: Borrow<str> + Clone + Eq + Hash,
    {
        let modified = self.wrapped.insert_indexed(s, p, o);
        if let Some([si, pi, oi]) = modified {
            self.s2p.entry(si).or_insert_with(|| Vec::new()).push(pi);
            self.sp2o.entry((si, pi)).or_insert_with(|| Vec::new()).push(oi);
        }
        modified
    }

    fn remove_indexed<U, V, W> (&mut self, s: &Term<U>, p: &Term<V>, o: &Term<W>) -> Option<([Self::Index;3])> where
        U: Borrow<str> + Clone + Eq + Hash,
        V: Borrow<str> + Clone + Eq + Hash,
        W: Borrow<str> + Clone + Eq + Hash,
    {
        let modified = self.wrapped.remove_indexed(s, p, o);
        if let Some([si, pi, oi]) = modified {
            remove_one_val(&mut self.s2p, si, pi);
            remove_one_val(&mut self.sp2o, (si, pi), oi);
        }
        modified
    }

    fn shrink_to_fit(&mut self) {
        self.wrapped.shrink_to_fit();
        self.s2p.shrink_to_fit();
        self.sp2o.shrink_to_fit();
    }
}

impl<'a, T> Graph<'a> for SpoWrapper<T> where
    T: IndexedGraph + Graph<'a, Triple=[&'a Term<<T as IndexedGraph>::Holder>;3]>,
{
    impl_graph_for_wrapper!();
}

impl<T> MutableGraph for SpoWrapper<T> where
    T: IndexedGraph + for <'a> Graph<'a, Triple=[&'a Term<<T as IndexedGraph>::Holder>;3]>,
{
    impl_mutable_graph_for_indexed_mutable_graph!();
}

impl<T> SetGraph for SpoWrapper<T> where
    T: IndexedGraph + SetGraph,
{}


#[cfg(test)]
type SpoGraph = SpoWrapper<LightGraph>;
#[cfg(test)]
test_graph_impl!(SpoGraph);
