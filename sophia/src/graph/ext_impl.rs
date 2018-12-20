// this module is transparently re-exported by its parent `graph`
// It defines implementation of Graph and MutableGraph for existing types.

use std::borrow::Borrow;
use std::collections::HashSet;
use std::hash::Hash;

use super::*;
use ::error::Never;
use ::term::*;
use ::triple::*;


impl<T> Graph for [T] where
    T: Triple,
{
    type Holder = T::Holder;
    type Error = Never;

    #[inline]
    fn iter(&self) -> GFallibleTripleIterator<Self> {
        Box::from(self.iter().map(|t| Ok((t.s(), t.p(), t.o()))))
    }

    #[inline]
    fn hint(&self) -> (usize, Option<usize>) {
        (self.len(), Some(self.len()))
    }
}

impl<T> Graph for Vec<T> where
    T: Triple,
{
    type Holder = T::Holder;
    type Error = Never;

    #[inline]
    fn iter(&self) -> GFallibleTripleIterator<Self> {
        Box::from(self[..].iter().map(|t| Ok((t.s(), t.p(), t.o()))))
    }

    #[inline]
    fn hint(&self) -> (usize, Option<usize>) {
        (self.len(), Some(self.len()))
    }
}

impl<T> Graph for HashSet<T> where
    T: Eq + Hash + Triple,
{
    type Holder = T::Holder;
    type Error = Never;

    #[inline]
    fn iter(&self) -> GFallibleTripleIterator<Self> {
        Box::from(self.iter().map(|t| Ok((t.s(), t.p(), t.o()))))
    }

    #[inline]
    fn hint(&self) -> (usize, Option<usize>) {
        (self.len(), Some(self.len()))
    }
}


impl MutableGraph for HashSet<(BoxTerm, BoxTerm, BoxTerm)> where
{
    fn insert<T, U, V> (&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> GResult<Self, bool> where
        T: Borrow<str>,
        U: Borrow<str>,
        V: Borrow<str>,
    {
        let s = BoxTerm::from(s);
        let p = BoxTerm::from(p);
        let o = BoxTerm::from(o);
        Ok(HashSet::insert(self, (s, p, o)))
    }
    fn remove<T, U, V> (&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> GResult<Self, bool> where
        T: Borrow<str>,
        U: Borrow<str>,
        V: Borrow<str>,
    {
        let s = BoxTerm::from(s);
        let p = BoxTerm::from(p);
        let o = BoxTerm::from(o);
        Ok(HashSet::remove(self, &(s, p, o)))
    }
}

impl<T> SetGraph for HashSet<(Term<T>, Term<T>, Term<T>)> where
    T: Borrow<str> + Eq + Hash,
{}



#[cfg(test)]
mod test {
    use std::collections::HashSet;
    use resiter::oks::*;

    use ::graph::*;
    use ::ns::*;
    use ::term::BoxTerm;

    #[test]
    fn test_slice() {
        let g = [
            (rdf::type_, rdf::type_, rdf::Property),
            (rdf::Property, rdf::type_, rdfs::Class),
            (rdfs::Class, rdf::type_, rdfs::Class),
        ];
        let v: Vec<_> = <[_] as Graph>::iter(&g).oks().collect();
        assert_eq!(v.len(), 3);
    }

    #[test]
    fn test_vec() {
        let g = vec![
            (rdf::type_, rdf::type_, rdf::Property),
            (rdf::Property, rdf::type_, rdfs::Class),
            (rdfs::Class, rdf::type_, rdfs::Class),
        ];
        let v: Vec<_> = <Vec<_> as Graph>::iter(&g).oks().collect();
        assert_eq!(v.len(), 3);
    }

    #[test]
    fn test_hashset() {
        let mut g1: HashSet<(BoxTerm, BoxTerm, BoxTerm)> = HashSet::new();

        let g2 = [
            (rdf::type_, rdf::type_, rdf::Property),
            (rdf::Property, rdf::type_, rdfs::Class),
            (rdfs::Class, rdf::type_, rdfs::Class),
        ];
        let inserted = g1.insert_all(<[_] as Graph>::iter(&g2)).unwrap();
        assert_eq!(inserted, g2.len());
        let v: Vec<_> = <HashSet<_> as Graph>::iter(&g1).oks().collect();
        assert_eq!(v.len(), 3);
    }
}