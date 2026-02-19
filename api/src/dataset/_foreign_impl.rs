use super::*;
use crate::quad::{Gspo, Spog};
use crate::source::SourceError;
use crate::term::FromTerm;
use std::collections::{BTreeSet, HashSet};
use std::convert::Infallible;
use std::hash::{BuildHasher, Hash};

//
// foreign implementations
//

// reference to Dataset

impl<T: Dataset + ?Sized> Dataset for &T {
    type Quad<'x>
        = T::Quad<'x>
    where
        Self: 'x;

    type Error = T::Error;

    fn quads(&self) -> impl Iterator<Item = DResult<Self, Self::Quad<'_>>> + '_ {
        T::quads(*self)
    }

    fn quads_matching<'s, 't, S, P, O, G>(
        &'s self,
        sm: S,
        pm: P,
        om: O,
        gm: G,
    ) -> impl Iterator<Item = DResult<Self, Self::Quad<'s>>> + 't
    where
        's: 't,
        S: TermMatcher + 't,
        P: TermMatcher + 't,
        O: TermMatcher + 't,
        G: GraphNameMatcher + 't,
    {
        T::quads_matching(*self, sm, pm, om, gm)
    }

    fn contains<TS, TP, TO, TG>(&self, s: TS, p: TP, o: TO, g: GraphName<TG>) -> DResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
        TG: Term,
    {
        T::contains(*self, s, p, o, g)
    }

    fn subjects(&self) -> impl Iterator<Item = DResult<Self, DTerm<'_, Self>>> + '_ {
        T::subjects(*self)
    }

    fn predicates(&self) -> impl Iterator<Item = DResult<Self, DTerm<'_, Self>>> + '_ {
        T::predicates(*self)
    }

    fn objects(&self) -> impl Iterator<Item = DResult<Self, DTerm<'_, Self>>> + '_ {
        T::objects(*self)
    }

    fn graph_names(&self) -> impl Iterator<Item = DResult<Self, DTerm<'_, Self>>> + '_ {
        T::graph_names(*self)
    }

    fn iris(&self) -> impl Iterator<Item = DResult<Self, DTerm<'_, Self>>> + '_ {
        T::iris(*self)
    }

    fn blank_nodes(&self) -> impl Iterator<Item = DResult<Self, DTerm<'_, Self>>> + '_ {
        T::blank_nodes(*self)
    }

    fn literals(&self) -> impl Iterator<Item = DResult<Self, DTerm<'_, Self>>> + '_ {
        T::literals(*self)
    }

    fn triple_terms<'s>(&'s self) -> Box<dyn Iterator<Item = DResult<Self, DTerm<'s, Self>>> + 's>
    where
        DTerm<'s, Self>: Clone,
    {
        T::triple_terms(*self)
    }

    fn variables(&self) -> impl Iterator<Item = DResult<Self, DTerm<'_, Self>>> + '_ {
        T::variables(*self)
    }
}

// NB: this one is required so that &'a mut T can also implement MutableDataset
impl<T: Dataset + ?Sized> Dataset for &mut T {
    type Quad<'x>
        = T::Quad<'x>
    where
        Self: 'x;

    type Error = T::Error;

    fn quads(&self) -> impl Iterator<Item = DResult<Self, Self::Quad<'_>>> + '_ {
        T::quads(*self)
    }

    fn quads_matching<'s, 't, S, P, O, G>(
        &'s self,
        sm: S,
        pm: P,
        om: O,
        gm: G,
    ) -> impl Iterator<Item = DResult<Self, Self::Quad<'s>>> + 't
    where
        's: 't,
        S: TermMatcher + 't,
        P: TermMatcher + 't,
        O: TermMatcher + 't,
        G: GraphNameMatcher + 't,
    {
        T::quads_matching(*self, sm, pm, om, gm)
    }

    fn contains<TS, TP, TO, TG>(&self, s: TS, p: TP, o: TO, g: GraphName<TG>) -> DResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
        TG: Term,
    {
        T::contains(*self, s, p, o, g)
    }

    fn subjects(&self) -> impl Iterator<Item = DResult<Self, DTerm<'_, Self>>> + '_ {
        T::subjects(*self)
    }

    fn predicates(&self) -> impl Iterator<Item = DResult<Self, DTerm<'_, Self>>> + '_ {
        T::predicates(*self)
    }

    fn objects(&self) -> impl Iterator<Item = DResult<Self, DTerm<'_, Self>>> + '_ {
        T::objects(*self)
    }

    fn graph_names(&self) -> impl Iterator<Item = DResult<Self, DTerm<'_, Self>>> + '_ {
        T::graph_names(*self)
    }

    fn iris(&self) -> impl Iterator<Item = DResult<Self, DTerm<'_, Self>>> + '_ {
        T::iris(*self)
    }

    fn blank_nodes(&self) -> impl Iterator<Item = DResult<Self, DTerm<'_, Self>>> + '_ {
        T::blank_nodes(*self)
    }

    fn literals(&self) -> impl Iterator<Item = DResult<Self, DTerm<'_, Self>>> + '_ {
        T::literals(*self)
    }

    fn triple_terms<'s>(&'s self) -> Box<dyn Iterator<Item = DResult<Self, DTerm<'s, Self>>> + 's>
    where
        DTerm<'s, Self>: Clone,
    {
        T::triple_terms(*self)
    }

    fn variables(&self) -> impl Iterator<Item = DResult<Self, DTerm<'_, Self>>> + '_ {
        T::variables(*self)
    }
}

impl<T: MutableDataset + ?Sized> MutableDataset for &mut T {
    type MutationError = T::MutationError;

    fn insert<TS, TP, TO, TG>(
        &mut self,
        s: TS,
        p: TP,
        o: TO,
        g: GraphName<TG>,
    ) -> MdResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
        TG: Term,
    {
        T::insert(*self, s, p, o, g)
    }

    fn remove<TS, TP, TO, TG>(
        &mut self,
        s: TS,
        p: TP,
        o: TO,
        g: GraphName<TG>,
    ) -> MdResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
        TG: Term,
    {
        T::remove(*self, s, p, o, g)
    }

    fn insert_all<TS: QuadSource>(
        &mut self,
        src: TS,
    ) -> StreamResult<usize, TS::Error, Self::MutationError> {
        T::insert_all(*self, src)
    }

    fn remove_all<TS: QuadSource>(
        &mut self,
        src: TS,
    ) -> StreamResult<usize, TS::Error, Self::MutationError> {
        T::remove_all(*self, src)
    }

    fn remove_matching<S, P, O, G>(
        &mut self,
        ms: S,
        mp: P,
        mo: O,
        mg: G,
    ) -> Result<usize, Self::MutationError>
    where
        S: TermMatcher,
        P: TermMatcher,
        O: TermMatcher,
        G: GraphNameMatcher,
        Self::MutationError: From<Self::Error>,
    {
        T::remove_matching(*self, ms, mp, mo, mg)
    }

    fn retain_matching<S, P, O, G>(
        &mut self,
        ms: S,
        mp: P,
        mo: O,
        mg: G,
    ) -> Result<(), Self::MutationError>
    where
        S: TermMatcher,
        P: TermMatcher,
        O: TermMatcher,
        G: GraphNameMatcher,
        Self::MutationError: From<Self::Error>,
    {
        T::retain_matching(*self, ms, mp, mo, mg)
    }
}

//
// foreign implementations
//

// slice of quads

impl<Q: Quad> Dataset for [Q] {
    type Error = Infallible;
    type Quad<'x>
        = &'x Q
    where
        Self: 'x;

    fn quads(&self) -> impl Iterator<Item = DResult<Self, Self::Quad<'_>>> + '_ {
        self.iter().map(Ok)
    }
}

// Vec of quads

impl<Q: Quad> Dataset for Vec<Q> {
    type Error = Infallible;
    type Quad<'x>
        = &'x Q
    where
        Self: 'x;

    fn quads(&self) -> impl Iterator<Item = DResult<Self, Self::Quad<'_>>> + '_ {
        self[..].quads()
    }
}

impl<T> CollectibleDataset for Vec<Spog<T>>
where
    T: Term + FromTerm,
{
    type CollectError = Infallible;

    fn from_quad_source<TS: QuadSource>(
        mut quads: TS,
    ) -> StreamResult<Self, TS::Error, Self::CollectError> {
        let min_cap = quads.size_hint_quads().0;
        let mut v = Vec::with_capacity(min_cap);
        quads
            .for_each_quad(|q| {
                v.push((
                    [q.s().into_term(), q.p().into_term(), q.o().into_term()],
                    q.g().map(Term::into_term),
                ))
            })
            .map_err(SourceError)?;
        Ok(v)
    }
}

impl<T> MutableDataset for Vec<Spog<T>>
where
    T: Term + FromTerm,
{
    type MutationError = Infallible;

    fn insert<TS, TP, TO, TG>(
        &mut self,
        s: TS,
        p: TP,
        o: TO,
        g: GraphName<TG>,
    ) -> MdResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
        TG: Term,
    {
        self.push((
            [s.into_term(), p.into_term(), o.into_term()],
            g.map(Term::into_term),
        ));
        Ok(true)
    }

    fn remove<TS, TP, TO, TG>(
        &mut self,
        s: TS,
        p: TP,
        o: TO,
        g: GraphName<TG>,
    ) -> MdResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
        TG: Term,
    {
        let s = s.borrow_term();
        let p = p.borrow_term();
        let o = o.borrow_term();
        let g = g.as_ref().map(|gn| gn.borrow_term());
        let mut i = 0;
        while i < self.len() {
            if self[i].matched_by([s], [p], [o], [g]) {
                self.swap_remove(i);
            } else {
                i += 1;
            }
        }
        Ok(true)
    }
}

impl<T> CollectibleDataset for Vec<Gspo<T>>
where
    T: Term + FromTerm,
{
    type CollectError = Infallible;

    fn from_quad_source<TS: QuadSource>(
        mut quads: TS,
    ) -> StreamResult<Self, TS::Error, Self::CollectError> {
        let min_cap = quads.size_hint_quads().0;
        let mut v = Vec::with_capacity(min_cap);
        quads
            .for_each_quad(|q| {
                v.push((
                    q.g().map(Term::into_term),
                    [q.s().into_term(), q.p().into_term(), q.o().into_term()],
                ))
            })
            .map_err(SourceError)?;
        Ok(v)
    }
}

impl<T> MutableDataset for Vec<Gspo<T>>
where
    T: Term + FromTerm,
{
    type MutationError = Infallible;

    fn insert<TS, TP, TO, TG>(
        &mut self,
        s: TS,
        p: TP,
        o: TO,
        g: GraphName<TG>,
    ) -> MdResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
        TG: Term,
    {
        self.push((
            g.map(Term::into_term),
            [s.into_term(), p.into_term(), o.into_term()],
        ));
        Ok(true)
    }

    fn remove<TS, TP, TO, TG>(
        &mut self,
        s: TS,
        p: TP,
        o: TO,
        g: GraphName<TG>,
    ) -> MdResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
        TG: Term,
    {
        let s = s.borrow_term();
        let p = p.borrow_term();
        let o = o.borrow_term();
        let g = g.as_ref().map(|gn| gn.borrow_term());
        match self.iter().position(|q| q.matched_by([s], [p], [o], [g])) {
            None => Ok(false),
            Some(i) => {
                self.swap_remove(i);
                Ok(true)
            }
        }
    }
}

// HashSet of quads

impl<Q: Quad, S> Dataset for HashSet<Q, S> {
    type Error = Infallible;
    type Quad<'x>
        = &'x Q
    where
        Self: 'x;

    fn quads(&self) -> impl Iterator<Item = DResult<Self, Self::Quad<'_>>> + '_ {
        self.iter().map(Ok)
    }
}

impl<T, S> CollectibleDataset for HashSet<Spog<T>, S>
where
    T: Term + Eq + FromTerm + Hash,
    S: BuildHasher + Default,
{
    type CollectError = Infallible;

    fn from_quad_source<TS: QuadSource>(
        mut quads: TS,
    ) -> StreamResult<Self, TS::Error, Self::CollectError> {
        let min_cap = quads.size_hint_quads().0;
        let mut s = HashSet::<_, S>::with_capacity_and_hasher(min_cap, S::default());
        quads
            .for_each_quad(|q| {
                s.insert((
                    [q.s().into_term(), q.p().into_term(), q.o().into_term()],
                    q.g().map(Term::into_term),
                ));
            })
            .map_err(SourceError)?;
        Ok(s)
    }
}

impl<T, S> MutableDataset for HashSet<Spog<T>, S>
where
    T: Term + Eq + FromTerm + Hash,
    S: BuildHasher + Default,
{
    type MutationError = Infallible;

    fn insert<TS, TP, TO, TG>(
        &mut self,
        s: TS,
        p: TP,
        o: TO,
        g: GraphName<TG>,
    ) -> MdResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
        TG: Term,
    {
        Ok(self.insert((
            [s.into_term(), p.into_term(), o.into_term()],
            g.map(Term::into_term),
        )))
    }

    fn remove<TS, TP, TO, TG>(
        &mut self,
        s: TS,
        p: TP,
        o: TO,
        g: GraphName<TG>,
    ) -> MdResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
        TG: Term,
    {
        Ok(self.remove(&(
            [s.into_term(), p.into_term(), o.into_term()],
            g.map(Term::into_term),
        )))
    }
}

impl<T, S> CollectibleDataset for HashSet<Gspo<T>, S>
where
    T: Term + Eq + FromTerm + Hash,
    S: BuildHasher + Default,
{
    type CollectError = Infallible;

    fn from_quad_source<TS: QuadSource>(
        mut quads: TS,
    ) -> StreamResult<Self, TS::Error, Self::CollectError> {
        let min_cap = quads.size_hint_quads().0;
        let mut s = HashSet::<_, S>::with_capacity_and_hasher(min_cap, S::default());
        quads
            .for_each_quad(|q| {
                s.insert((
                    q.g().map(Term::into_term),
                    [q.s().into_term(), q.p().into_term(), q.o().into_term()],
                ));
            })
            .map_err(SourceError)?;
        Ok(s)
    }
}

impl<T, S> MutableDataset for HashSet<Gspo<T>, S>
where
    T: Term + Eq + FromTerm + Hash,
    S: BuildHasher + Default,
{
    type MutationError = Infallible;

    fn insert<TS, TP, TO, TG>(
        &mut self,
        s: TS,
        p: TP,
        o: TO,
        g: GraphName<TG>,
    ) -> MdResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
        TG: Term,
    {
        Ok(self.insert((
            g.map(Term::into_term),
            [s.into_term(), p.into_term(), o.into_term()],
        )))
    }

    fn remove<TS, TP, TO, TG>(
        &mut self,
        s: TS,
        p: TP,
        o: TO,
        g: GraphName<TG>,
    ) -> MdResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
        TG: Term,
    {
        Ok(self.remove(&(
            g.map(Term::into_term),
            [s.into_term(), p.into_term(), o.into_term()],
        )))
    }
}

impl<T: Quad, S> SetDataset for HashSet<T, S> {}

// BTreeSet of quads

/// NB: This is a straightforward and minimal implementation,
/// not taking advantage of the order of terms to optimize [`Dataset::quads_matching`]
/// nor other methods.
impl<Q: Quad> Dataset for BTreeSet<Q> {
    type Error = Infallible;
    type Quad<'x>
        = &'x Q
    where
        Self: 'x;

    fn quads(&self) -> impl Iterator<Item = DResult<Self, Self::Quad<'_>>> + '_ {
        self.iter().map(Ok)
    }
}

impl<T> CollectibleDataset for BTreeSet<Spog<T>>
where
    T: Term + FromTerm + Ord,
{
    type CollectError = Infallible;

    fn from_quad_source<TS: QuadSource>(
        mut quads: TS,
    ) -> StreamResult<Self, TS::Error, Self::CollectError> {
        let mut s = BTreeSet::new();
        quads
            .for_each_quad(|q| {
                s.insert((
                    [q.s().into_term(), q.p().into_term(), q.o().into_term()],
                    q.g().map(Term::into_term),
                ));
            })
            .map_err(SourceError)?;
        Ok(s)
    }
}

impl<T> MutableDataset for BTreeSet<Spog<T>>
where
    T: Term + FromTerm + Ord,
{
    type MutationError = Infallible;

    fn insert<TS, TP, TO, TG>(
        &mut self,
        s: TS,
        p: TP,
        o: TO,
        g: GraphName<TG>,
    ) -> MdResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
        TG: Term,
    {
        Ok(self.insert((
            [s.into_term(), p.into_term(), o.into_term()],
            g.map(Term::into_term),
        )))
    }

    fn remove<TS, TP, TO, TG>(
        &mut self,
        s: TS,
        p: TP,
        o: TO,
        g: GraphName<TG>,
    ) -> MdResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
        TG: Term,
    {
        Ok(self.remove(&(
            [s.into_term(), p.into_term(), o.into_term()],
            g.map(Term::into_term),
        )))
    }
}

impl<T> CollectibleDataset for BTreeSet<Gspo<T>>
where
    T: Term + FromTerm + Ord,
{
    type CollectError = Infallible;

    fn from_quad_source<TS: QuadSource>(
        mut quads: TS,
    ) -> StreamResult<Self, TS::Error, Self::CollectError> {
        let mut s = BTreeSet::new();
        quads
            .for_each_quad(|q| {
                s.insert((
                    q.g().map(Term::into_term),
                    [q.s().into_term(), q.p().into_term(), q.o().into_term()],
                ));
            })
            .map_err(SourceError)?;
        Ok(s)
    }
}

impl<T> MutableDataset for BTreeSet<Gspo<T>>
where
    T: Term + FromTerm + Ord,
{
    type MutationError = Infallible;

    fn insert<TS, TP, TO, TG>(
        &mut self,
        s: TS,
        p: TP,
        o: TO,
        g: GraphName<TG>,
    ) -> MdResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
        TG: Term,
    {
        Ok(self.insert((
            g.map(Term::into_term),
            [s.into_term(), p.into_term(), o.into_term()],
        )))
    }

    fn remove<TS, TP, TO, TG>(
        &mut self,
        s: TS,
        p: TP,
        o: TO,
        g: GraphName<TG>,
    ) -> MdResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
        TG: Term,
    {
        Ok(self.remove(&(
            g.map(Term::into_term),
            [s.into_term(), p.into_term(), o.into_term()],
        )))
    }
}

impl<T: Quad> SetDataset for BTreeSet<T> {}

#[cfg(test)]
mod test {
    use super::*;
    // NB: implementation of Dataset by &D and &mut D are not tested,
    // as the code is trivial to review.

    // NB: implementation of Dataset by [Q] is tested indirectly,
    // as the implementation of Dataset by Vec<Q> relies on it.

    type VecAsDataset = Vec<Spog<SimpleTerm<'static>>>;
    crate::test_dataset_impl!(vec, VecAsDataset, false);

    // the following is only to test the test macro with is_gen=false
    #[cfg(feature = "all_tests")]
    crate::test_immutable_dataset_impl!(vec_strict, VecAsDataset, false, false);

    #[cfg(feature = "all_tests")]
    type HashSetAsDataset = HashSet<Spog<SimpleTerm<'static>>>;
    #[cfg(feature = "all_tests")]
    crate::test_dataset_impl!(hashset, HashSetAsDataset);

    #[cfg(feature = "all_tests")]
    type HashSetAsDataset2 = HashSet<Gspo<SimpleTerm<'static>>>;
    #[cfg(feature = "all_tests")]
    crate::test_dataset_impl!(hashset2, HashSetAsDataset2);

    #[cfg(feature = "all_tests")]
    type BTreeSetAsDataset = BTreeSet<Spog<SimpleTerm<'static>>>;
    #[cfg(feature = "all_tests")]
    crate::test_dataset_impl!(btreeset, BTreeSetAsDataset);

    #[cfg(feature = "all_tests")]
    type BTreeSetAsDataset2 = BTreeSet<Gspo<SimpleTerm<'static>>>;
    #[cfg(feature = "all_tests")]
    crate::test_dataset_impl!(btreeset2, BTreeSetAsDataset2);
}
