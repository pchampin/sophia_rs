// this module is transparently re-exported by its parent `graph`

use std::borrow::Borrow;

use ::term::*;
use ::term::matcher::TermMatcher;

pub type TermIterator<'a, T> = Box<Iterator<Item=&'a Term<T>>+'a>;
pub type PairIterator<'a, T, U=T> = Box<Iterator<Item=(&'a Term<T>, &'a Term<U>)>+'a>;
pub type TripleIterator<'a, T, U=T, V=U> = Box<Iterator<Item=(&'a Term<T>, &'a Term<U>, &'a Term<V>)>+'a>;

pub trait Triple<T: Borrow<str>> {
    fn s(&self) -> &Term<T>;
    fn p(&self) -> &Term<T>;
    fn o(&self) -> &Term<T>;
}

// NB: the semantics of this trait allows a graph to contain duplicate triples;
// see also SetGraph
pub trait Graph
{
    type Holder: Borrow<str>;

    fn iter(&self) -> TripleIterator<Self::Holder>;

    fn iter_for_s<'a, T> (&'a self, s: &'a Term<T>) -> TripleIterator<'a, Self::Holder> where
        T: Borrow<str>,
    {
        Box::new(
            self.iter().filter(move |t| t.s()==s)
        )
    }
    fn iter_for_p<'a, T> (&'a self, p: &'a Term<T>) -> TripleIterator<'a, Self::Holder> where
        T: Borrow<str>,
    {
        Box::new(
            self.iter().filter(move |t| t.p()==p)
        )
    }
    fn iter_for_o<'a, T> (&'a self, o: &'a Term<T>) -> TripleIterator<'a, Self::Holder> where
        T: Borrow<str>,
    {
        Box::new(
            self.iter().filter(move |t| t.o()==o)
        )
    }
    fn iter_for_sp<'a, T, U> (&'a self, s: &'a Term<T>, p: &'a Term<U>) -> TripleIterator<'a, Self::Holder> where
        T: Borrow<str>,
        U: Borrow<str>,
    {
        Box::new(
            self.iter_for_s(s).filter(move |t| t.p()==p)
        )
    }
    fn iter_for_so<'a, T, U> (&'a self, s: &'a Term<T>, o: &'a Term<U>) -> TripleIterator<'a, Self::Holder> where
        T: Borrow<str>,
        U: Borrow<str>,
    {
        Box::new(
            self.iter_for_s(s).filter(move |t| t.o()==o)
        )
    }
    fn iter_for_po<'a, T, U> (&'a self, p: &'a Term<T>, o: &'a Term<U>) -> TripleIterator<'a, Self::Holder> where
        T: Borrow<str>,
        U: Borrow<str>,
    {
        Box::new(
            self.iter_for_p(p).filter(move |t| t.o()==o)
        )
    }
    fn iter_for_spo<'a, T, U, V> (&'a self, s: &'a Term<T>, p: &'a Term<U>, o: &'a Term<V>) -> TripleIterator<'a, Self::Holder> where
        T: Borrow<str>,
        U: Borrow<str>,
        V: Borrow<str>,
    {
        Box::new(
            self.iter_for_sp(s,p).filter(move |t| t.o()==o)
        )
    }

    fn contains<T, U, V> (&self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> bool where
        T: Borrow<str>,
        U: Borrow<str>,
        V: Borrow<str>,
    {
        self.iter_for_spo(s, p, o).next().is_some()
    }

    fn len(&self) -> usize {
        self.iter().count()
    }

    fn iter_matching<'a, S, P, O> (&'a self, ms: &'a S, mp: &'a P, mo: &'a O) -> TripleIterator<'a, Self::Holder, Self::Holder, Self::Holder> where
        S: TermMatcher<Self::Holder>,
        P: TermMatcher<Self::Holder>,
        O: TermMatcher<Self::Holder>,
    {
        match (&ms.constant(), &mp.constant(), &mo.constant()) {
            (None,    None,    None   )    => Box::from(
                self.iter().filter(move |t| ms.try(t.s()) && mp.try(t.p()) && mo.try(t.o()))),
            (Some(s), None,    None   )    => Box::from(
                self.iter_for_s(s).filter(move |t| mp.try(t.p()) && mo.try(t.o()))),
            (None,    Some(p), None   )    => Box::from(
                self.iter_for_p(p).filter(move |t| ms.try(t.s()) && mo.try(t.o()))),
            (None,    None,    Some(o))    => Box::from(
                self.iter_for_o(o).filter(move |t| mp.try(t.s()) && mp.try(t.p()))),
            (Some(s), Some(p), None   )    => Box::from(
                self.iter_for_sp(s,p).filter(move |t| mo.try(t.o()))),
            (Some(s), None,    Some(o))    => Box::from(
                self.iter_for_so(s,o).filter(move |t| mp.try(t.p()))),
            (None,    Some(p), Some(o))    => Box::from(
                self.iter_for_po(p,o).filter(move |t| ms.try(t.s()))),
            (Some(s), Some(p), Some(o))    => Box::from(
                self.iter_for_spo(s,p,o))
        }
    }

    fn hint(&self) -> (usize, Option<usize>) {
        (0, None)
    }

    fn hint_for_s<T> (&self, _s: &Term<T>) -> (usize, Option<usize>) where
        T: Borrow<str>,
    {
        (0, self.hint().1)
    }

    fn hint_for_p<T> (&self, _p: &Term<T>) -> (usize, Option<usize>) where
        T: Borrow<str>,
    {
        (0, self.hint().1)
    }

    fn hint_for_o<T> (&self, _o: &Term<T>) -> (usize, Option<usize>) where
        T: Borrow<str>,
    {
        (0, self.hint().1)
    }

    fn hint_for_sp<T, U> (&self, s: &Term<T>, _p: &Term<U>) -> (usize, Option<usize>) where
        T: Borrow<str>,
        U: Borrow<str>,
    {
        (0, self.hint_for_s(s).1)
    }

    fn hint_for_so<T, U> (&self, s: &Term<T>, _o: &Term<U>) -> (usize, Option<usize>) where
        T: Borrow<str>,
        U: Borrow<str>,
    {
        (0, self.hint_for_s(s).1)
    }

    fn hint_for_po<T, U> (&self, p: &Term<T>, _o: &Term<U>) -> (usize, Option<usize>) where
        T: Borrow<str>,
        U: Borrow<str>,
    {
        (0, self.hint_for_p(p).1)
    }

}

pub trait MutableGraph : Graph {
    fn copy<T: Borrow<str>> (&mut self, t: &Term<T>) -> Term<Self::Holder>;
    fn insert_as_is(&mut self, s: Term<Self::Holder>, p: Term<Self::Holder>, o: Term<Self::Holder>) -> bool;
    fn remove_as_is(&mut self, s: &Term<Self::Holder>, p: &Term<Self::Holder>, o: &Term<Self::Holder>) -> bool;

    fn insert<T, U, V> (&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> bool where
        T: Borrow<str>,
        U: Borrow<str>,
        V: Borrow<str>,
    {
        let s = self.copy(s);
        let p = self.copy(p);
        let o = self.copy(o);
        self.insert_as_is(s, p, o)
    }

    // TODO NB: this is impl is not very efficient;
    // consider reimplementing it,
    // possibly using a function from the helper module (TODO link)
    // if your implementation matches its assumptions
    fn remove<T, U, V> (&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> bool where
        T: Borrow<str>,
        U: Borrow<str>,
        V: Borrow<str>,
    {
        let s = self.copy(s);
        let p = self.copy(p);
        let o = self.copy(o);
        self.insert_as_is(s, p, o)
    }

    // TODO NB: this is impl is not very efficient;
    // consider reimplementing it,
    // possibly using a function from the helper module (TODO link)
    // if your implementation matches its assumptions
    fn remove_matching<S, P, O> (&mut self, ms: &S, mp: &P, mo: &O) -> usize where
        S: TermMatcher<Self::Holder>,
        P: TermMatcher<Self::Holder>,
        O: TermMatcher<Self::Holder>,
    {
        let mut to_remove = vec![];
        for (s, p, o) in self.iter_matching(ms, mp, mo) {
            to_remove.push((
                BoxTerm::from(s), BoxTerm::from(p), BoxTerm::from(o)));
        }
        let ret = to_remove.len();
        for (s, p, o) in &to_remove {
            self.remove(s, p, o);
        }
        ret
    }

    // TODO NB: this is impl is not very efficient;
    // consider reimplementing it,
    // possibly using a function from the helper module (TODO link)
    // if your implementation matches its assumptions
    fn retain<S, P, O> (&mut self, ms: S, mp: P, mo: O) where
        S: TermMatcher<Self::Holder>,
        P: TermMatcher<Self::Holder>,
        O: TermMatcher<Self::Holder>,
    {
        self.remove_matching(
            &|t: &Term<_>| !ms.try(t),
            &|t: &Term<_>| !mp.try(t),
            &|t: &Term<_>| !mo.try(t),
        );
    }
}

impl<'a, T: Borrow<str>> Triple<T> for (&'a Term<T>, &'a Term<T>, &'a Term<T>) {
    #[inline] fn s(&self) -> &Term<T> { self.0 }
    #[inline] fn p(&self) -> &Term<T> { self.1 }
    #[inline] fn o(&self) -> &Term<T> { self.2 }
}

impl<T: Borrow<str>> Triple<T> for (Term<T>, Term<T>, Term<T>) {
    #[inline] fn s(&self) -> &Term<T> { &self.0 }
    #[inline] fn p(&self) -> &Term<T> { &self.1 }
    #[inline] fn o(&self) -> &Term<T> { &self.2 }
}

/// This module provides helper functions for implementing the `Graph` and `MutableGraph` traits.
pub mod helper {
    use super::*;

    pub fn remove_with_from<'a, G, T, U, V> (g: &mut G, s: &'a Term<T>, p: &'a Term<U>, o: &'a Term<V>) -> bool where
        G: MutableGraph,
        G::Holder: From<&'a str>,
        T: Borrow<str>,
        U: Borrow<str>,
        V: Borrow<str>,
    {
        let s = Term::from(s);
        let p = Term::from(p);
        let o = Term::from(o);
        g.remove_as_is(&s, &p, &o)
    }
}



// todo explain that this trait adds the set-semantics to Graph / MutableGraph
// triples will never be yielded twice by iterators, or added twice
pub trait SetGraph: Graph {
}