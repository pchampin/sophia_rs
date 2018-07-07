use std::borrow::Borrow;
use std::hash::Hash;
use std::iter::{empty, once};
use std::rc::Rc;
use std::sync::Arc;

use ::term::*;
use ::term::factory::*;
use super::traits::*;
use super::index::*;

pub type SimpleGraph = GenericGraph<RcTermFactory, TermIndexU<u32, Rc<str>>>;
pub type SyncSimpleGraph = GenericGraph<ArcTermFactory, TermIndexU<u32, Arc<str>>>;
/* TODO once "strategy wrappers are implemented"
pub type FastGraph = OpWrapper<PosWrapper<GenericGraph<Rc<RcTerm>, Rc<str>, RcTermFactory>>>;
*/


// TODO we are fooling the borrow checker by pretending that
// the keys in t2i are StaticTerm,
// while in fact they have a shorter lifetime.
// However, we ensure that keys do not exist longer than the data they borrow
// (inside i2t)...

#[derive(Default)]
pub struct GenericGraph<F, I> where
    F: TermFactory + Default,
    I: TermIndex<Holder=F::Holder>,
    I::Index: Hash,
{
    factory: F,
    terms: I,
    spo: TripleIndex<I::Index, ()>,
}

impl<F, I> GenericGraph<F, I> where
    F: TermFactory + Default,
    I: TermIndex<Holder=F::Holder>,
    I::Index: Hash,
{
    pub fn new() -> GenericGraph<F, I> {
        GenericGraph {
            factory: F::default(),
            terms: I::default(),
            spo: TripleIndex::new(),
        }
    }
}

impl<F, I> Graph for GenericGraph<F, I> where
    F: TermFactory + Default,
    I: TermIndex<Holder=F::Holder>,
    I::Index: Hash,
{
    type SHolder = F::Holder;

    fn iter<'a> (&'a self) -> TripleIterator<'a, Self::SHolder> {
        Box::from(
            self.spo.triples(&self.terms)
            .map(move |(s, p, o, _)| (s, p, o))
        )
    }

    fn iter_for_s<'a, T> (&'a self, s: &'a Term<T>) -> TripleIterator<'a, Self::SHolder> where
        T: Borrow<str>,
    {
        if let Some(si) = self.terms.get_index(&RefTerm::from(s)) {
            let s = self.terms.get_term(si).unwrap();
            if let Some(subindex) = self.spo.get(&si) {
                return Box::from(
                    subindex.pairs(&self.terms)
                    .map(move |(p, o, _)| (s, p, o))
                );
            }
        }
        return Box::from(empty())
    }

    fn iter_for_p<'a, T> (&'a self, p: &'a Term<T>) -> TripleIterator<'a, Self::SHolder> where
        T: Borrow<str>,
    {
        if let Some(pi) = self.terms.get_index(&RefTerm::from(p)) {
            let p = self.terms.get_term(pi).unwrap();
            return Box::new(
                self.spo.iter()
                .filter_map(move |(si, subindex)| {
                    let s = self.terms.get_term(*si).unwrap();
                    subindex.get(&pi).map(|subsubindex| (s, subsubindex))
                })
                .flat_map(move |(s, subsubindex)|
                    subsubindex.keys()
                    .map(move |oi| (s, p, self.terms.get_term(*oi).unwrap()))
                )
            )
        }
        return Box::from(empty())
    }

    fn iter_for_sp<'a, T, U> (&'a self, s: &'a Term<T>, p: &'a Term<U>) -> TripleIterator<'a, Self::SHolder> where
        T: Borrow<str>,
        U: Borrow<str>,
    {
        if let Some(si) = self.terms.get_index(&RefTerm::from(s)) {
            if let Some(subindex) = self.spo.get(&si) {
                if let Some(pi) = self.terms.get_index(&RefTerm::from(p)) {
                    if let Some(subsubindex) = subindex.get(&pi) {
                        let s = self.terms.get_term(si).unwrap();
                        let p = self.terms.get_term(pi).unwrap();
                        return Box::from(
                            subsubindex.keys()
                            .map(move |oi| (s, p, self.terms.get_term(*oi).unwrap()))
                        );
                    }
                }
            }
        }
        return Box::from(empty())
    }

    fn iter_for_spo<'a, T, U, V> (&'a self, s: &'a Term<T>, p: &'a Term<U>, o: &'a Term<V>) -> TripleIterator<'a, Self::SHolder> where
        T: Borrow<str>,
        U: Borrow<str>,
        V: Borrow<str>,
    {
        if let Some(si) = self.terms.get_index(&RefTerm::from(s)) {
            if let Some(subindex) = self.spo.get(&si) {
                if let Some(pi) = self.terms.get_index(&RefTerm::from(p)) {
                    if let Some(subsubindex) = subindex.get(&pi) {
                        if let Some(oi) = self.terms.get_index(&RefTerm::from(o)) {
                            if let Some(_) = subsubindex.get(&oi) {
                                let s = self.terms.get_term(si).unwrap();
                                let p = self.terms.get_term(pi).unwrap();
                                let o = self.terms.get_term(oi).unwrap();
                                return Box::from(
                                    once((s, p, o))
                                );
                            }
                        }
                    }
                }
            }
        }
        return Box::from(empty())
    }

    fn len(&self) -> usize {
        self.spo.len()
    }

    fn hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }

    fn hint_for_s<'a, T> (&'a self, s: &'a Term<T>) -> (usize, Option<usize>) where
        T: Borrow<str>,
    {
        if let Some(si) = self.terms.get_index(&RefTerm::from(s)) {
            if let Some(subindex) = self.spo.get(&si) {
                let len = subindex.len();
                return (len, Some(len));
            }
        }
        (0, Some(0))
    }

    fn hint_for_sp<'a, T, U> (&'a self, s: &'a Term<T>,p: &'a Term<U>) -> (usize, Option<usize>) where
        T: Borrow<str>,
        U: Borrow<str>,
    {
        if let Some(si) = self.terms.get_index(&RefTerm::from(s)) {
            if let Some(subindex) = self.spo.get(&si) {
                if let Some(pi) = self.terms.get_index(&RefTerm::from(p)) {
                    if let Some(subsubindex) = subindex.get(&pi) {
                        let len = subsubindex.len();
                        return (len, Some(len));
                    }
                }
            }
        }
        (0, Some(0))
    }
}

impl<F, I> MutableGraph for GenericGraph<F, I> where
    F: TermFactory + Default,
    I: TermIndex<Holder=F::Holder>,
    I::Index: Hash,
{
    fn insert<T, U, V> (&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> bool where
        T: Borrow<str>,
        U: Borrow<str>,
        V: Borrow<str>,
    {
        let si = self.terms.make_index(self.factory.copy(s));
        let pi = self.terms.make_index(self.factory.copy(p));
        let oi = self.terms.make_index(self.factory.copy(o));
        let modified = self.spo.insert(si, pi, oi, ()).is_none();
        if !modified {
            self.terms.dec_ref(si);
            self.terms.dec_ref(pi);
            self.terms.dec_ref(oi);
        }
        modified
    }
    fn remove<T, U, V> (&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> bool where
        T: Borrow<str>,
        U: Borrow<str>,
        V: Borrow<str>,
    {
        let si = self.terms.get_index(&RefTerm::from(s));
        let pi = self.terms.get_index(&RefTerm::from(p));
        let oi = self.terms.get_index(&RefTerm::from(o));
        if let (Some(si), Some(pi), Some(oi)) = (si, pi, oi) {
            let modified = self.spo.remove(&si, &pi, &oi).is_some();
            if modified {
                self.terms.dec_ref(si);
                self.terms.dec_ref(pi);
                self.terms.dec_ref(oi);
            }
            modified
        } else {
            false
        }
    }
}

impl<F, I> SetGraph for GenericGraph<F, I> where
    F: TermFactory + Default,
    I: TermIndex<Holder=F::Holder>,
    I::Index: Hash,
{}



/* TODO implement PosWrapper, OsWrappe : augments a GenericGraph with additional indexes

// NB: This wrapper is only usable around GenericGraph,
// because it requires the guarantees that GenericGraph provides,
// regarding the lifetime of RefTerms used in the index.
#[derive(Default)]
pub struct PosWrapper<S, T, U> where
    S: Borrow<str>,
    T: Borrow<Term<S>> + Clone + From<Term<S>>,
    U: TermFactory<Holder=S>,
{
    inner: GenericGraph<S, T, U>,
    pos: TripleIndex<'static, (T, T, T)>,
}

*/
