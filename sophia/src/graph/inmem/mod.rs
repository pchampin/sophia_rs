// TODO properly document

use std::borrow::Borrow;
use std::collections::HashSet;
use std::hash::Hash;

use ::term::*;
use ::term::factory::*;
use super::index::*;
use super::traits::*;

#[macro_use]
mod wrapper; pub use self::wrapper::*;
mod spo_wrapper; pub use self::spo_wrapper::*;
mod ops_wrapper; pub use self::ops_wrapper::*;

/// A heaviy indexed graph.
/// Should be fast to query, but slower to load, and consuming more memory.
pub type FastGraph = OpsWrapper<SpoWrapper<GenericGraph<TermIndexU<u32, RcTermFactory>>>>;
/// A graph with no triple index.
///Should be fast to load and light in memory, but slower to query.
pub type LightGraph = GenericGraph<TermIndexU<u32, RcTermFactory>>;

#[cfg(test)] test_graph_impl!(test_fastg, FastGraph);
#[cfg(test)] test_graph_impl!(test_lightg, LightGraph);

/// Variants of Graph implementations with a smaller memory-footprint.
/// The trade-off is that these implementations can only contain a limited number of terms.
pub mod small {
    use super::*;

    /// A heaviy indexed graph.
    /// Should be fast to query, but slower to load, and consuming more memory.
    pub type FastGraph = OpsWrapper<SpoWrapper<GenericGraph<TermIndexU<u16, RcTermFactory>>>>;
    /// A graph with no triple index.
    ///Should be fast to load and light in memory, but slower to query.
    pub type LightGraph = GenericGraph<TermIndexU<u16, RcTermFactory>>;

    #[cfg(test)] test_graph_impl!(test_fastg, FastGraph);
    #[cfg(test)] test_graph_impl!(test_lightg, LightGraph);
}

/// Variants of Graph implementations which are safe to share across threads.
pub mod sync {
    use super::*;

    /// A heaviy indexed graph.
    /// Should be fast to query, but slower to load, and consuming more memory.
    pub type FastGraph = OpsWrapper<SpoWrapper<GenericGraph<TermIndexU<u32, ArcTermFactory>>>>;
    /// A graph with no triple index.
    ///Should be fast to load and light in memory, but slower to query.
    pub type LightGraph = GenericGraph<TermIndexU<u32, ArcTermFactory>>;

    #[cfg(test)] test_graph_impl!(test_fastg, FastGraph);
    #[cfg(test)] test_graph_impl!(test_lightg, LightGraph);
}


#[derive(Default)]
pub struct GenericGraph<I> where
    I: TermIndex,
    I::Index: Hash,
{
    terms: I,
    triples: HashSet<(I::Index, I::Index, I::Index)>,
}

impl<I> GenericGraph<I> where
    I: TermIndex,
    I::Index: Hash,
{
    pub fn new() -> GenericGraph<I> {
        GenericGraph {
            terms: I::default(),
            triples: HashSet::new(),
        }
    }

    fn len(&self) -> usize {
        self.triples.len()
    }
}

impl<I> IndexedMutableGraph for GenericGraph<I> where
    I: TermIndex,
    I::Index: Hash,
{
    type Index = I::Index;

    #[inline]
    fn get_index<T> (&self, t: &Term<T>) -> Option<Self::Index> where
        T: Borrow<str>,
    {
        self.terms.get_index(&RefTerm::from(t))
    }

    #[inline]
    fn get_term(&self, i: Self::Index) -> Option<&Term<Self::Holder>> {
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

impl<I> Graph for GenericGraph<I> where
    I: TermIndex,
    I::Index: Hash,
{
    type Holder = <I::Factory as TermFactory>::Holder;
    type Error = ::error::Never;

    fn iter<'a> (&'a self) -> FallibleTripleIterator<'a, Self> {
        Box::from(
            self.triples.iter()
            .map(move |(si, pi, oi)| Ok((
                self.terms.get_term(*si).unwrap(),
                self.terms.get_term(*pi).unwrap(),
                self.terms.get_term(*oi).unwrap(),
            )))
        )
    }

    fn hint(&self) -> (usize, Option<usize>) {
        (self.len(), Some(self.len()))
    }
}

impl<I> MutableGraph for GenericGraph<I> where
    I: TermIndex,
    I::Index: Hash,
{
    impl_mutable_graph_for_indexed_mutable_graph!();
}

impl<I> SetGraph for GenericGraph<I> where
    I: TermIndex,
    I::Index: Hash,
{}
