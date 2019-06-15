// this module is transparently re-exported by its parent `graph`
use std::borrow::Borrow;
use std::collections::HashMap;

use crate::graph::index::TermIndexMap;
use crate::term::factory::{FTerm, TermFactory};
use crate::term::*;

/// An in-memory implemention of [`TermIndexMap`](../index/trait.TermIndexMap.html)
/// with unsigned integers as indices.
pub struct TermIndexMapU<I, F>
where
    F: TermFactory,
{
    factory: F,
    i2t: Vec<Option<FTerm<F>>>,
    i2c: Vec<I>,
    t2i: HashMap<StaticTerm, I>,

    // factory is used to make new terms (used by make_term)
    // i2t (index to term) maps
    // - each used index to Some(Term)
    // - each free index to None
    // i2c (index to count) maps
    // - each used index to its ref counter (used by inc_ref and dec_ref)
    // - each free index to the next free index
    // t2i (term to index) maps each term to its index
    //
    // 0 is the null_index (index not mapped to any Term), so
    // - i2t[0] is initialized to None but never used,
    // - i2c[0] is used to store the first free index
}

// Implementation note:
//
// We are fooling the borrow checker by pretending that
// the keys in t2i are StaticTerm,
// while in fact they have a shorter lifetime.
// However, we ensure that keys do not exist longer than the data they borrow
// (inside i2t)...

impl<I, F> TermIndexMapU<I, F>
where
    I: Unsigned,
    F: TermFactory + Default,
{
    pub fn new() -> TermIndexMapU<I, F> {
        Self::default()
    }
}

impl<I, F> Default for TermIndexMapU<I, F>
where
    I: Unsigned,
    F: TermFactory + Default,
{
    fn default() -> TermIndexMapU<I, F> {
        TermIndexMapU {
            factory: F::default(),
            i2c: vec![I::ONE],
            i2t: vec![None],
            t2i: HashMap::default(),
        }
    }
}

impl<I, F> TermIndexMapU<I, F>
where
    I: Unsigned,
    F: TermFactory + Default,
{
    #[inline]
    fn next_free(&self) -> I {
        self.i2c[0]
    }

    fn set_next_free(&mut self, new: I) {
        self.i2c[0] = new
    }

    fn inc_next_free(&mut self) {
        self.i2c[0].inc()
    }
}

/// This macro implements TermIndexMap for TermIndexMapU<uXX>,
/// where uXX is one of u16, u32...
/// I would prefer to define a generic implementation using traits,
/// but I found this to be non trivial.
impl<T, F> TermIndexMap for TermIndexMapU<T, F>
where
    T: Unsigned,
    F: TermFactory + Default,
{
    type Index = T;
    type Factory = F;

    const NULL_INDEX: Self::Index = T::ZERO;

    #[inline]
    fn get_index(&self, t: &RefTerm) -> Option<T> {
        self.t2i.get(t).cloned()
    }

    fn make_index(&mut self, t: &RefTerm) -> T {
        let t = self.factory.copy(&t);
        let rt = unsafe { fake_static(&t) };
        if let Some(i) = self.get_index(&rt) {
            self.i2c[i.as_usize()].inc();
            return i;
        }
        let i = self.next_free();
        self.t2i.insert(rt, i);
        let i = i.as_usize();
        if i == self.i2t.len() {
            self.inc_next_free();
            self.i2t.push(Some(t));
            self.i2c.push(T::ONE);
        } else {
            self.set_next_free(self.i2c[i]);
            self.i2t[i] = Some(t);
            self.i2c[i] = T::ONE;
        }
        T::from_usize(i)
    }

    fn get_term(&self, i: T) -> Option<&FTerm<F>> {
        let i = i.as_usize();
        if i < self.i2t.len() {
            self.i2t[i].as_ref()
        } else {
            None
        }
    }

    fn inc_ref(&mut self, i: T) {
        let i = i.as_usize();
        self.i2c[i].inc();
    }

    fn dec_ref(&mut self, i: T) {
        let i = i.as_usize();
        self.i2c[i].dec();
        if self.i2c[i] == T::ZERO {
            let t: FTerm<F> = self.i2t[i].take().unwrap();
            self.t2i.remove(unsafe { &fake_static(&t) });
            self.i2c[i] = self.next_free();
            self.set_next_free(T::from_usize(i));
        }
    }

    fn shrink_to_fit(&mut self) {
        self.factory.shrink_to_fit();
        self.i2c.shrink_to_fit();
        self.i2t.shrink_to_fit();
        self.t2i.shrink_to_fit();
        debug_assert_eq!(self.i2c.len(), self.i2t.len());
    }
}

/// This trait is used by [`TermIndexMapU`](struct.TermIndexMapU.html)
/// as an abstraction of all unsigned int types.
///
pub trait Unsigned: Copy + Eq + std::hash::Hash {
    const ZERO: Self;
    const ONE: Self;
    fn as_usize(&self) -> usize;
    fn from_usize(_: usize) -> Self;
    fn inc(&mut self);
    fn dec(&mut self);
}

macro_rules! impl_unsigned_for {
    ($uXX: ty) => {
        impl Unsigned for $uXX {
            const ZERO: Self = 0;
            const ONE: Self = 1;

            #[inline]
            fn as_usize(&self) -> usize {
                *self as usize
            }
            #[inline]
            fn from_usize(other: usize) -> Self {
                other as $uXX
            }
            #[inline]
            fn inc(&mut self) {
                *self += 1
            }
            #[inline]
            fn dec(&mut self) {
                *self -= 1
            }
        }
    };
}

impl_unsigned_for!(u16);
impl_unsigned_for!(u32);
impl_unsigned_for!(u64);

/// Unsafely converts a term into a StaticTerm.
/// This is to be used *only* when we can guarantee that the produced StaticTerm
/// will not outlive the source term.
/// We use this for keys in TermIndexMapU::t2i, when the owning term is in TermIndexMapU::i2t.
#[inline]
unsafe fn fake_static<S, T>(t: &T) -> StaticTerm
where
    S: TermData,
    T: Borrow<Term<S>>,
{
    StaticTerm::from_with(t.borrow(), |txt| &*(txt as *const str))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::graph::index::assert_term_index_works;
    use crate::term::factory::RcTermFactory;

    #[test]
    fn test_term_index() {
        let mut ti = TermIndexMapU::<u16, RcTermFactory>::default();
        assert_term_index_works(&mut ti);
    }

    #[test]
    fn test_term_index_inner() {
        let mut ti = TermIndexMapU::<u16, RcTermFactory>::default();
        assert_eq!(ti.next_free(), 1);
        assert_eq!(ti.i2t.len(), 1);

        use crate::ns::rdf;

        assert_eq!(ti.get_index(&rdf::subject), None);
        assert_eq!(ti.make_index(&rdf::subject), 1);
        assert_eq!(ti.get_index(&rdf::subject), Some(1));
        assert_eq!(ti.i2c[1], 1);
        assert_eq!(ti.next_free(), 2);
        assert_eq!(ti.i2t.len(), 2);

        assert_eq!(ti.get_index(&rdf::predicate), None);
        assert_eq!(ti.make_index(&rdf::predicate), 2);
        assert_eq!(ti.get_index(&rdf::predicate), Some(2));
        assert_eq!(ti.i2c[2], 1);
        assert_eq!(ti.next_free(), 3);
        assert_eq!(ti.i2t.len(), 3);

        assert_eq!(ti.get_index(&rdf::object), None);
        assert_eq!(ti.make_index(&rdf::object), 3);
        assert_eq!(ti.get_index(&rdf::object), Some(3));
        assert_eq!(ti.i2c[3], 1);
        assert_eq!(ti.next_free(), 4);
        assert_eq!(ti.i2t.len(), 4);

        assert_eq!(ti.make_index(&rdf::predicate), 2);
        assert_eq!(ti.i2c[2], 2);

        ti.inc_ref(2);
        assert_eq!(ti.i2c[2], 3);

        ti.dec_ref(2);
        assert_eq!(ti.i2c[2], 2);
        assert_eq!(ti.next_free(), 4);

        ti.dec_ref(2);
        assert_eq!(ti.i2c[2], 1);
        assert_eq!(ti.next_free(), 4);

        ti.dec_ref(2);
        assert_eq!(ti.get_index(&rdf::predicate), None);
        assert_eq!(ti.next_free(), 2);
        assert_eq!(ti.i2c[2], 4); // now the previous version of next_free()

        ti.dec_ref(1);
        assert_eq!(ti.get_index(&rdf::subject), None);
        assert_eq!(ti.next_free(), 1);
        assert_eq!(ti.i2c[1], 2); // now the previous version of next_free()

        assert_eq!(ti.make_index(&rdf::type_), 1);
        assert_eq!(ti.i2c[1], 1);
        assert_eq!(ti.i2t.len(), 4);
        assert_eq!(ti.next_free(), 2);

        // re-inserting rdf::subject, now ends up in a different place
        assert_eq!(ti.make_index(&rdf::subject), 2);
        assert_eq!(ti.i2c[2], 1);
        assert_eq!(ti.i2t.len(), 4);
        assert_eq!(ti.next_free(), 4);

        assert_eq!(ti.make_index(&rdf::Property), 4);
        assert_eq!(ti.i2c[4], 1);
        assert_eq!(ti.i2t.len(), 5);
        assert_eq!(ti.next_free(), 5);
    }
}
