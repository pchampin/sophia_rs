// this module is transparently re-exported by its parent `graph`
// It defines implementation of Graph and MutableGraph for existing types.

use std::borrow::Borrow;
use std::collections::HashSet;
use std::hash::Hash;

use super::*;
use ::term::*;
use ::triple::*;


impl<T> Graph for [(Term<T>, Term<T>, Term<T>)] where
    T: Borrow<str>,
{
    type SHolder = T;

    #[inline]
    fn iter(&self) -> TripleIterator<Self::SHolder> {
        Box::from(self.iter().map(|t| (t.s(), t.p(), t.o())))
    }

    #[inline]
    fn len(&self) -> usize {
        <[_]>::len(self)
    }

    #[inline]
    fn hint(&self) -> (usize, Option<usize>) {
        (self.len(), Some(self.len()))
    }
}

impl<T> Graph for Vec<(Term<T>, Term<T>, Term<T>)> where
    T: Borrow<str>,
{
    type SHolder = T;

    #[inline]
    fn iter(&self) -> TripleIterator<Self::SHolder> {
        Box::from(self[..].iter().map(|t| (t.s(), t.p(), t.o())))
    }

    #[inline]
    fn len(&self) -> usize {
        <Vec<_>>::len(self)
    }

    #[inline]
    fn hint(&self) -> (usize, Option<usize>) {
        (self.len(), Some(self.len()))
    }
}

impl<T> Graph for HashSet<(Term<T>, Term<T>, Term<T>)> where
    T: Borrow<str> + Eq + Hash,
{
    type SHolder = T;

    #[inline]
    fn iter(&self) -> TripleIterator<Self::SHolder> {
        Box::from(self.iter().map(|t| (t.s(), t.p(), t.o())))
    }

    #[inline]
    fn len(&self) -> usize {
        <HashSet<_>>::len(self)
    }

    #[inline]
    fn hint(&self) -> (usize, Option<usize>) {
        (self.len(), Some(self.len()))
    }
}

impl MutableGraph for HashSet<(BoxTerm, BoxTerm, BoxTerm)> where
{
    fn insert<T, U, V> (&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> bool where
        T: Borrow<str>,
        U: Borrow<str>,
        V: Borrow<str>,
    {
        let s = BoxTerm::from(s);
        let p = BoxTerm::from(p);
        let o = BoxTerm::from(o);
        HashSet::insert(self, (s, p, o))
    }
    fn remove<T, U, V> (&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> bool where
        T: Borrow<str>,
        U: Borrow<str>,
        V: Borrow<str>,
    {
        let s = BoxTerm::from(s);
        let p = BoxTerm::from(p);
        let o = BoxTerm::from(o);
        HashSet::remove(self, &(s, p, o))
    }
}

impl<T> SetGraph for HashSet<(Term<T>, Term<T>, Term<T>)> where
    T: Borrow<str> + Eq + Hash,
{}
