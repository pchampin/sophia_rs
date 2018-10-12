// this module is transparently re-exported by its parent `graphe`

use std::collections::HashMap;
use std::iter::empty;

use super::*;
use ::graph::index::remove_one_val;

#[derive(Default)]
pub struct OpsWrapper<T> where
    T: IndexedMutableGraph,
{
    wrapped: T,
    o2p: HashMap<T::Index, Vec<T::Index>>,
    po2s: HashMap<(T::Index, T::Index), Vec<T::Index>>,
}

impl<T> OpsWrapper<T> where
    T: IndexedMutableGraph + Default,
    T::Index: Default,
{
    pub fn new() -> Self {
        Self::default()
    }
}

impl<T> GraphWrapper for OpsWrapper<T> where
    T: IndexedMutableGraph,
{
    type Wrapped = T;

    fn get_wrapped(&self) -> &T {
        &self.wrapped
    }

    fn get_wrapped_mut(&mut self) -> &mut T {
        &mut self.wrapped
    }

    fn gw_iter_for_o<'a, U> (&'a self, o: &'a Term<U>) -> FallibleTripleIterator<'a, Self::Wrapped>
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
                        .map(move |si| Ok((
                            self.wrapped.get_term(*si).unwrap(), p, o,
                        )))
                    )
                )
            }
        }
        Box::new(empty())
    }

    fn gw_iter_for_po<'a, U, V> (&'a self, p: &'a Term<U>, o: &'a Term<V>) -> FallibleTripleIterator<'a, Self::Wrapped>
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
                        .map(move |si| Ok((
                            self.wrapped.get_term(*si).unwrap(), p, o,
                        )))
                    )
                }
            }
        }
        Box::new(empty())
    }

    fn gw_hint_for_o<U> (&self, o: &Term<U>) -> (usize, Option<usize>) where
        U: Borrow<str>,
    {
        match self.get_index(o) {
            None => (0, Some(0)),
            Some(oi) => {
                let (_, max) = self.wrapped.hint();
                let min = self.o2p.get(&oi).unwrap().len();
                (min, max)
            }
        }
    }

    fn gw_hint_for_po<U, V> (&self, p: &Term<U>, o: &Term<V>) -> (usize, Option<usize>) where
        U: Borrow<str>,
        V: Borrow<str>,
    {
        if let Some(pi) = self.get_index(p) {
            if let Some(oi) = self.get_index(o) {
                let nb = self.po2s.get(&(pi, oi)).unwrap().len();
                return (nb, Some(nb));
            }
        }
        (0, Some(0))
    }
}

impl<T> Graph for OpsWrapper<T> where
    T: IndexedMutableGraph,
{
    impl_graph_for_wrapper!();
}

impl<T> IndexedMutableGraph for OpsWrapper<T> where
    T: IndexedMutableGraph,
{
    type Index = T::Index;

    #[inline]
    fn get_index<U> (&self, t: &Term<U>) -> Option<Self::Index> where
        U: Borrow<str>,
    {
        self.wrapped.get_index(t)
    }

    #[inline]
    fn get_term(&self, i: Self::Index) -> Option<&Term<Self::Holder>>
    {
        self.wrapped.get_term(i)
    }

    fn insert_indexed<U, V, W> (&mut self, s: &Term<U>, p: &Term<V>, o: &Term<W>) -> Option<(Self::Index, Self::Index, Self::Index)> where
        U: Borrow<str>,
        V: Borrow<str>,
        W: Borrow<str>,
    {
        let modified = self.wrapped.insert_indexed(s, p, o);
        if let Some((si, pi, oi)) = modified {
            self.o2p.entry(oi).or_insert_with(|| Vec::new()).push(pi);
            self.po2s.entry((pi, oi)).or_insert_with(|| Vec::new()).push(si);
        }
        modified
    }

    fn remove_indexed<U, V, W> (&mut self, s: &Term<U>, p: &Term<V>, o: &Term<W>) -> Option<(Self::Index, Self::Index, Self::Index)> where
        U: Borrow<str>,
        V: Borrow<str>,
        W: Borrow<str>,
    {
        let modified = self.wrapped.remove_indexed(s, p, o);
        if let Some((si, pi, oi)) = modified {    
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

impl<T> MutableGraph for OpsWrapper<T> where
    T: IndexedMutableGraph,
{
    impl_mutable_graph_for_indexed_mutable_graph!();
}

impl<T> SetGraph for OpsWrapper<T> where
    T: IndexedMutableGraph + SetGraph,
{}



#[cfg(test)]
type OpsGraph = OpsWrapper<LightGraph>;
#[cfg(test)]
test_graph_impl!(OpsGraph);
