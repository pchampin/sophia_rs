use std::borrow::Borrow;
use std::iter::{empty, once};
use std::rc::Rc;
use std::sync::Arc;

use ::term::*;
use ::term::factory::*;
use super::traits::*;
use super::index::*;

pub type SimpleGraph = GenericGraph<Rc<str>, RcTerm, RcTermFactory>;
pub type SyncSimpleGraph = GenericGraph<Arc<str>, ArcTerm, ArcTermFactory>;
/* TODO once "strategy wrappers are implemented"
pub type FastGraph = OpWrapper<PosWrapper<GenericGraph<Rc<RcTerm>, Rc<str>, RcTermFactory>>>;
*/


// TODO we are fooling the borrow checker by pretending that
// the keys in the TripleIndex are StaticTerm,
// while in fact they have a shorter lifetime.
// However, we ensure that keys do not exist longer than the data they borrow
// (inside the value associated to that key)...
// NB: this is true only because GenericGraph.copy
//  * always reuses the same &str's for a given term, and
//  * uses a noramlized form for `IriData`s.
// Otherwise, several triples with e.g. the same subject could hold different versions of its IRI,
// and when the version used for the index-key is dropped, the index-key becomes dangling.

#[derive(Default)]
pub struct GenericGraph<S, T, U> where
    S: Borrow<str>,
    T: Borrow<Term<S>> + Clone + From<Term<S>>,
    U: TermFactory<Holder=S>,
{
    factory: U,
    spo: TripleIndex<'static, (T, T, T)>,
}

impl<S, T, U> GenericGraph<S, T, U> where
    S: Borrow<str>,
    T: Borrow<Term<S>> + Clone + From<Term<S>>,
    U: TermFactory<Holder=S> + Default,
{
    pub fn new() -> GenericGraph<S, T, U> {
        GenericGraph {
            factory: U::default(),
            spo: TripleIndex::new(),
        }
    }
}


impl<S, T, U> Graph for GenericGraph<S, T, U> where
    S: Borrow<str>,
    T: Borrow<Term<S>> + Clone + From<Term<S>>,
    U: TermFactory<Holder=S>,
{
    type SHolder = S;

    fn iter(&self) -> TripleIterator<Self::SHolder> {
        Box::from(
            self.spo.values_flat().map(holder_triple_as_ref)
        )
    }

    fn iter_for_s<'a, V> (&'a self, s: &'a Term<V>) -> TripleIterator<S> where
        V: Borrow<str>,
    {
        match self.spo.get(s) {
            Some(subindex) => Box::from(subindex.values_flat().map(holder_triple_as_ref)),
            None => Box::from(empty())
        }
    }

    fn iter_for_p<'a, V> (&'a self, p: &'a Term<V>) -> TripleIterator<S> where
        V: Borrow<str>,
    {
        Box::from(
            self.spo.values()
                .filter_map(move |subindex| subindex.get(p))
                .flat_map(|subsubindex| subsubindex.values())
                .map(holder_triple_as_ref)
        )
    }

    fn iter_for_sp<'a, V, W> (&'a self, s: &'a Term<V>, p: &'a Term<W>) -> TripleIterator<S> where
        V: Borrow<str>,
        W: Borrow<str>,
    {
        if let Some(subindex) = self.spo.get(s) {
            if let Some(subsubindex) = subindex.get(p) {
                return Box::from(
                    subsubindex.values().map(holder_triple_as_ref)
                );
            }
        }
        Box::from(empty())
    }

    fn iter_for_spo<'a, V, W, X> (&'a self, s: &'a Term<V>, p: &'a Term<W>, o: &'a Term<X>) -> TripleIterator<S> where
        V: Borrow<str>,
        W: Borrow<str>,
        X: Borrow<str>,
    {
        if let Some(subindex) = self.spo.get(s) {
            if let Some(sub2index) = subindex.get(p) {
                let o = RefTerm::from(o);
                if let Some(triple) = sub2index.get(&o) {
                    return Box::from(
                        once(holder_triple_as_ref(&triple))
                    );
                }
            }
        }
        Box::from(empty())
    }

    fn len(&self) -> usize {
        self.spo.len()
    }

    fn hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }

    fn hint_for_s<'a, V> (&'a self, s: &'a Term<V>) -> (usize, Option<usize>) where
        V: Borrow<str>,
    {
        let len = match self.spo.get(s) {
            Some(subindex) => subindex.len(),
            None           => 0,
        };
        (len, Some(len))
    }

    fn hint_for_sp<'a, V, W> (&'a self, s: &'a Term<V>, p: &'a Term<W>) -> (usize, Option<usize>) where
        V: Borrow<str>,
        W: Borrow<str>,
    {
        let len = match self.spo.get(s) {
            None           => 0,
            Some(subindex) => match subindex.get(p) {
                None              => 0,
                Some(subsubindex) => subsubindex.len(),
            }
        };
        (len, Some(len))
    }
}

impl<S, T, U> MutableGraph for GenericGraph<S, T, U> where
    S: Borrow<str>,
    T: Borrow<Term<S>> + Clone + From<Term<S>>,
    U: TermFactory<Holder=S>,
{
    type THolder = T;

    fn copy<V> (&mut self, t: &Term<V>) -> Self::THolder where
        V: Borrow<str>,
    {
        T::from(self.factory.copy_normalized(t, Normalization::LastHashOrSlash))
    }

    unsafe fn insert_as_is(&mut self, s: Self::THolder, p: Self::THolder, o: Self::THolder) -> bool {
        let ks = fake_static(&s);
        let kp = fake_static(&p);
        let ko = fake_static(&o);
        let t = (s, p, o);
        self.spo.insert(ks, kp, ko, t).is_none()
    }

    fn remove<V, W, X> (&mut self, s: &Term<V>, p: &Term<W>, o: &Term<X>) -> bool where
        V: Borrow<str>,
        W: Borrow<str>,
        X:Borrow<str>,
    {
        let ks = unsafe { fake_static(&s) };
        let kp = unsafe { fake_static(&p) };
        let ko = unsafe { fake_static(&o) };
        self.spo.remove(&ks, &kp, &ko).is_some()
    }
}

impl<S, T, U> SetGraph for GenericGraph<S, T, U> where
    S: Borrow<str>,
    T: Borrow<Term<S>> + Clone + From<Term<S>>,
    U: TermFactory<Holder=S>,
{}



#[inline]
fn holder_triple_as_ref<S, T> (t: &(T, T, T)) -> (&Term<S>, &Term<S>, &Term<S>) where
    S: Borrow<str>,
    T: Borrow<Term<S>>,
{
    (t.0.borrow(), t.1.borrow(), t.2.borrow())
}

/// Unsafely converts a term into a StaticTerm.
/// This is to be used *only* when we can guarantee that the produced StaticTerm
/// will not outlive the source term.
/// We use this for index keys, when the source term is stored in the value of the index.
#[inline]
unsafe fn fake_static<S, T> (t: &T) -> StaticTerm where
    S: Borrow<str>,
    T: Borrow<Term<S>>,
{
    StaticTerm::from_with(t.borrow(), |txt| &*(txt as *const str))
}


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
