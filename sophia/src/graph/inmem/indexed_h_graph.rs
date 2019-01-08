// this module is transparently re-exported by its parent `graphe`

use std::borrow::Borrow;
use std::collections::HashSet;
use std::hash::Hash;

use ::graph::*;
use ::graph::index::{IndexedMutableGraph, TermIndex};
use ::term::{RefTerm, Term, factory::TermFactory};

/// A generic implementation of [`Graph`] and [`MutableGraph`],
/// storing its terms in a [`TermIndex`],
/// and its triples in a [`HashSet`].
///
/// [`Graph`]: ../trait.Graph.html
/// [`MutableGraph`]: ../trait.MutableGraph.html
/// [`TermIndex`]: trait.TermIndex.html
/// [`HashSet`]: https://doc.rust-lang.org/std/collections/struct.HashSet.html
#[derive(Default)]
pub struct IndexedHGraph<I> where
    I: TermIndex,
    I::Index: Hash,
    <I::Factory as TermFactory>::Holder: 'static,
{
    terms: I,
    triples: HashSet<(I::Index, I::Index, I::Index)>,
}

impl<I> IndexedHGraph<I> where
    I: TermIndex,
    I::Index: Hash,
{
    pub fn new() -> IndexedHGraph<I> {
        IndexedHGraph {
            terms: I::default(),
            triples: HashSet::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.triples.len()
    }
}

impl<I> IndexedMutableGraph for IndexedHGraph<I> where
    I: TermIndex,
    I::Index: Hash,
    <I::Factory as TermFactory>::Holder: 'static,
{
    type Index = I::Index;
    type Holder = <I::Factory as TermFactory>::Holder;

    #[inline]
    fn get_index<T> (&self, t: &Term<T>) -> Option<Self::Index> where
        T: Borrow<str>,
    {
        self.terms.get_index(&RefTerm::from(t))
    }

    #[inline]
    fn get_term<'a>(&'a self, i: Self::Index) -> Option<&Term<Self::Holder>> {
        self.terms.get_term(i)
    }

    fn insert_indexed<T, U, V> (&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> Option<(I::Index, I::Index, I::Index)> where
        T: Borrow<str>,
        U: Borrow<str>,
        V: Borrow<str>,
    {
        let si = self.terms.make_index(&RefTerm::from(s));
        let pi = self.terms.make_index(&RefTerm::from(p));
        let oi = self.terms.make_index(&RefTerm::from(o));
        let modified = self.triples.insert((si, pi, oi));
        if modified {
            Some((si, pi, oi))
        } else {
            self.terms.dec_ref(si);
            self.terms.dec_ref(pi);
            self.terms.dec_ref(oi);
            None
        }
    }

    fn remove_indexed<T, U, V> (&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> Option<(I::Index, I::Index, I::Index)> where
        T: Borrow<str>,
        U: Borrow<str>,
        V: Borrow<str>,
    {
        let si = self.terms.get_index(&RefTerm::from(s));
        let pi = self.terms.get_index(&RefTerm::from(p));
        let oi = self.terms.get_index(&RefTerm::from(o));
        if let (Some(si), Some(pi), Some(oi)) = (si, pi, oi) {
            let modified = self.triples.remove(&(si, pi, oi));
            if modified {
                self.terms.dec_ref(si);
                self.terms.dec_ref(pi);
                self.terms.dec_ref(oi);
                return Some((si, pi, oi));
            }
        }
        None
    }

    fn shrink_to_fit(&mut self) {
        self.terms.shrink_to_fit();
        self.triples.shrink_to_fit();
    }
}

impl<'a, I> Graph<'a> for IndexedHGraph<I> where
    I: TermIndex,
    I::Index: Hash,
    <I::Factory as TermFactory>::Holder: 'static,
{
    type Triple = [&'a Term<<Self as IndexedMutableGraph>::Holder>;3];
    type Error = ::error::Never;

    fn iter(&'a self) -> GFallibleTripleIterator<'a, Self> {
        Box::from(
            self.triples.iter()
            .map(move |(si, pi, oi)| Ok([
                self.terms.get_term(*si).unwrap(),
                self.terms.get_term(*pi).unwrap(),
                self.terms.get_term(*oi).unwrap(),
            ]))
        )
    }
}

impl<I> MutableGraph for IndexedHGraph<I> where
    I: TermIndex,
    I::Index: Hash,
    <I::Factory as TermFactory>::Holder: 'static,
{
    impl_mutable_graph_for_indexed_mutable_graph!();
}

impl<I> SetGraph for IndexedHGraph<I> where
    I: TermIndex,
    I::Index: Hash,
{}



#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially in graph::inmem -- the mod.rs file).
}