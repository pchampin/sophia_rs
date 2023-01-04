use super::*;
use crate::source::SourceError;
use crate::term::FromTerm;
use crate::triple::TBorrowTerm;
use std::collections::{BTreeSet, HashSet};
use std::convert::Infallible;
use std::hash::{BuildHasher, Hash};

//
// foreign implementations
//

// reference to Graph

impl<'a, T: Graph + ?Sized> Graph for &'a T {
    type Triple<'x> = T::Triple<'x> where Self: 'x;

    type Error = T::Error;

    fn triples(&self) -> GTripleSource<Self> {
        T::triples(*self)
    }

    fn triples_matching<'s, S, P, O>(&'s self, sm: S, pm: P, om: O) -> GTripleSource<'s, Self>
    where
        S: TermMatcher + 's,
        P: TermMatcher + 's,
        O: TermMatcher + 's,
    {
        T::triples_matching(*self, sm, pm, om)
    }

    fn contains<TS, TP, TO>(&self, s: TS, p: TP, o: TO) -> GResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
    {
        T::contains(*self, s, p, o)
    }

    fn subjects(&self) -> GTermSource<Self> {
        T::subjects(*self)
    }

    fn predicates(&self) -> GTermSource<Self> {
        T::predicates(*self)
    }

    fn objects(&self) -> GTermSource<Self> {
        T::objects(*self)
    }

    fn iris(&self) -> GTermSource<Self> {
        T::iris(*self)
    }

    fn blank_nodes(&self) -> GTermSource<Self> {
        T::blank_nodes(*self)
    }

    fn literals(&self) -> GTermSource<Self> {
        T::literals(*self)
    }

    fn quoted_triples<'s>(&'s self) -> GTermSource<'s, Self>
    where
        GTerm<'s, Self>: Clone,
    {
        T::quoted_triples(*self)
    }

    fn variables(&self) -> GTermSource<Self> {
        T::variables(*self)
    }
}

// NB: this one is required so that &'a mut T can also implement MutableDataset
impl<'a, T: Graph + ?Sized> Graph for &'a mut T {
    type Triple<'x> = T::Triple<'x> where Self: 'x;

    type Error = T::Error;

    fn triples(&self) -> GTripleSource<Self> {
        T::triples(*self)
    }

    fn triples_matching<'s, S, P, O>(&'s self, sm: S, pm: P, om: O) -> GTripleSource<'s, Self>
    where
        S: TermMatcher + 's,
        P: TermMatcher + 's,
        O: TermMatcher + 's,
    {
        T::triples_matching(*self, sm, pm, om)
    }

    fn contains<TS, TP, TO>(&self, s: TS, p: TP, o: TO) -> GResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
    {
        T::contains(*self, s, p, o)
    }

    fn subjects(&self) -> GTermSource<Self> {
        T::subjects(*self)
    }

    fn predicates(&self) -> GTermSource<Self> {
        T::predicates(*self)
    }

    fn objects(&self) -> GTermSource<Self> {
        T::objects(*self)
    }

    fn iris(&self) -> GTermSource<Self> {
        T::iris(*self)
    }

    fn blank_nodes(&self) -> GTermSource<Self> {
        T::blank_nodes(*self)
    }

    fn literals(&self) -> GTermSource<Self> {
        T::literals(*self)
    }

    fn quoted_triples<'s>(&'s self) -> GTermSource<'s, Self>
    where
        GTerm<'s, Self>: Clone,
    {
        T::quoted_triples(*self)
    }

    fn variables(&self) -> GTermSource<Self> {
        T::variables(*self)
    }
}

impl<'a, T: MutableGraph + ?Sized> MutableGraph for &'a mut T {
    type MutationError = T::MutationError;

    fn insert<TS, TP, TO>(&mut self, s: TS, p: TP, o: TO) -> MgResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
    {
        T::insert(*self, s, p, o)
    }

    fn remove<TS, TP, TO>(&mut self, s: TS, p: TP, o: TO) -> MgResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
    {
        T::remove(*self, s, p, o)
    }

    fn insert_all<TS: TripleSource>(
        &mut self,
        src: TS,
    ) -> StreamResult<usize, TS::Error, Self::MutationError> {
        T::insert_all(*self, src)
    }

    fn remove_all<TS: TripleSource>(
        &mut self,
        src: TS,
    ) -> StreamResult<usize, TS::Error, Self::MutationError> {
        T::remove_all(*self, src)
    }

    fn remove_matching<S, P, O>(
        &mut self,
        ms: S,
        mp: P,
        mo: O,
    ) -> Result<usize, Self::MutationError>
    where
        S: TermMatcher,
        P: TermMatcher,
        O: TermMatcher,
        Self::MutationError: From<Self::Error>,
    {
        T::remove_matching(*self, ms, mp, mo)
    }

    fn retain_matching<S, P, O>(&mut self, ms: S, mp: P, mo: O) -> Result<(), Self::MutationError>
    where
        S: TermMatcher,
        P: TermMatcher,
        O: TermMatcher,
        Self::MutationError: From<Self::Error>,
    {
        T::retain_matching(*self, ms, mp, mo)
    }
}

// slice of triples

impl<T: Triple> Graph for [T] {
    type Error = Infallible;
    type Triple<'x> = [TBorrowTerm<'x, T>; 3] where Self: 'x;

    fn triples(&self) -> GTripleSource<Self> {
        Box::new(self.iter().map(Triple::spo).map(Ok))
    }
}

// Vec of triples

impl<T: Triple> Graph for Vec<T> {
    type Error = Infallible;
    type Triple<'x> = [TBorrowTerm<'x, T>; 3] where Self: 'x;

    fn triples(&self) -> GTripleSource<Self> {
        self[..].triples()
    }
}

impl<T> CollectibleGraph for Vec<[T; 3]>
where
    T: Term + FromTerm,
{
    fn from_triple_source<TS: TripleSource>(
        mut triples: TS,
    ) -> StreamResult<Self, TS::Error, Self::Error> {
        let min_cap = triples.size_hint_triples().0;
        let mut v = Vec::with_capacity(min_cap);
        triples
            .for_each_triple(|t| v.push([t.s().into_term(), t.p().into_term(), t.o().into_term()]))
            .map_err(SourceError)?;
        Ok(v)
    }
}

impl<T> MutableGraph for Vec<[T; 3]>
where
    T: Term + FromTerm,
{
    type MutationError = Infallible;

    fn insert<TS, TP, TO>(&mut self, s: TS, p: TP, o: TO) -> MgResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
    {
        self.push([s.into_term(), p.into_term(), o.into_term()]);
        Ok(true)
    }

    fn remove<TS, TP, TO>(&mut self, s: TS, p: TP, o: TO) -> MgResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
    {
        let s = s.borrow_term();
        let p = p.borrow_term();
        let o = o.borrow_term();
        match self.iter().position(|t| t.matched_by([s], [p], [o])) {
            None => Ok(false),
            Some(i) => {
                self.swap_remove(i);
                Ok(true)
            }
        }
    }
}

// HashSet of triples

impl<T: Triple, S> Graph for HashSet<T, S> {
    type Error = Infallible;
    type Triple<'x> = [TBorrowTerm<'x, T>; 3] where Self: 'x;

    fn triples(&self) -> GTripleSource<Self> {
        Box::new(self.iter().map(Triple::spo).map(Ok))
    }
}

impl<T, S> CollectibleGraph for HashSet<[T; 3], S>
where
    T: Term + Eq + FromTerm + Hash,
    S: BuildHasher + Default,
{
    fn from_triple_source<TS: TripleSource>(
        mut triples: TS,
    ) -> StreamResult<Self, TS::Error, Self::Error> {
        let min_cap = triples.size_hint_triples().0;
        let mut s = HashSet::<_, S>::with_capacity_and_hasher(min_cap, S::default());
        triples
            .for_each_triple(|t| {
                s.insert([t.s().into_term(), t.p().into_term(), t.o().into_term()]);
            })
            .map_err(SourceError)?;
        Ok(s)
    }
}

impl<T, S> MutableGraph for HashSet<[T; 3], S>
where
    T: Term + Eq + FromTerm + Hash,
    S: BuildHasher + Default,
{
    type MutationError = Infallible;

    fn insert<TS, TP, TO>(&mut self, s: TS, p: TP, o: TO) -> MgResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
    {
        Ok(self.insert([s.into_term(), p.into_term(), o.into_term()]))
    }

    fn remove<TS, TP, TO>(&mut self, s: TS, p: TP, o: TO) -> MgResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
    {
        Ok(self.remove(&[s.into_term(), p.into_term(), o.into_term()]))
    }
}

impl<T: Triple, S> SetGraph for HashSet<T, S> {}

// BTreeSet of triples

/// NB: This is a straighforward and minimal implementation,
/// not taking advantage of the order of terms to optimize [`Graph::triples_matching`]
/// nor other methods.
impl<T: Triple> Graph for BTreeSet<T> {
    type Error = Infallible;
    type Triple<'x> = [TBorrowTerm<'x, T>; 3] where Self: 'x;

    fn triples(&self) -> GTripleSource<Self> {
        Box::new(self.iter().map(Triple::spo).map(Ok))
    }
}

impl<T> CollectibleGraph for BTreeSet<[T; 3]>
where
    T: Term + FromTerm + Ord,
{
    fn from_triple_source<TS: TripleSource>(
        mut triples: TS,
    ) -> StreamResult<Self, TS::Error, Self::Error> {
        let mut s = BTreeSet::new();
        triples
            .for_each_triple(|t| {
                s.insert([t.s().into_term(), t.p().into_term(), t.o().into_term()]);
            })
            .map_err(SourceError)?;
        Ok(s)
    }
}

impl<T> MutableGraph for BTreeSet<[T; 3]>
where
    T: Term + FromTerm + Ord,
{
    type MutationError = Infallible;

    fn insert<TS, TP, TO>(&mut self, s: TS, p: TP, o: TO) -> MgResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
    {
        Ok(self.insert([s.into_term(), p.into_term(), o.into_term()]))
    }

    fn remove<TS, TP, TO>(&mut self, s: TS, p: TP, o: TO) -> MgResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
    {
        Ok(self.remove(&[s.into_term(), p.into_term(), o.into_term()]))
    }
}

impl<T: Triple> SetGraph for BTreeSet<T> {}

#[cfg(test)]
mod test {
    use super::*;

    // NB: implementation of Graph by &G and &mut G are not tested,
    // as the code is trivial to review.

    // NB: implementation of Graph by [T] is tested indirectly,
    // as the implementation of Graph by Vec<T> relies on it.

    type VecAsGraph = Vec<[SimpleTerm<'static>; 3]>;
    crate::test_graph_impl!(vec, VecAsGraph, false);

    // the following is only to test the test macro with is_gen=false
    #[cfg(feature = "all_tests")]
    crate::test_immutable_graph_impl!(vec_strict, VecAsGraph, false, false);

    #[cfg(feature = "all_tests")]
    type HashSetAsGraph = HashSet<[SimpleTerm<'static>; 3]>;
    #[cfg(feature = "all_tests")]
    crate::test_graph_impl!(hashset, HashSetAsGraph);

    #[cfg(feature = "all_tests")]
    type BTreeSetAsGraph = BTreeSet<[crate::term::CmpTerm<SimpleTerm<'static>>; 3]>;
    #[cfg(feature = "all_tests")]
    crate::test_graph_impl!(btreeset, BTreeSetAsGraph);
}
