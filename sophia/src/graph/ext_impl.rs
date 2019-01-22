// this module is transparently re-exported by its parent `graph`
// It defines implementation of Graph and MutableGraph for existing types.

use std::borrow::Borrow;
use std::collections::HashSet;
use std::hash::Hash;

use resiter::oks::*;

use super::*;
use ::error::*;
use ::streams::AsTripleSource;
use ::term::*;
use ::triple::*;


impl<'a, T> Graph<'a> for [T] where
    T: Triple<'a>+'a,
{
    type Triple = &'a T;
    type Error = Never;

    #[inline]
    fn iter(&'a self) -> GTripleSource<Self> {
        Box::new(
            <[T]>::iter(self).as_triple_source()
        )
    }
}



impl<'a, T> Graph<'a> for Vec<T> where
    T: Triple<'a>+'a,
{
    type Triple = &'a T;
    type Error = Never;

    #[inline]
    fn iter(&'a self) -> GTripleSource<Self> {
        Box::new(
            <[T]>::iter(self).as_triple_source()
        )
    }
}

impl MutableGraph for Vec<[BoxTerm;3]> where
{
    type MutationError = Never;

    fn insert<T, U, V> (&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> MGResult< Self, bool> where
        T: Borrow<str>,
        U: Borrow<str>,
        V: Borrow<str>,
    {
        let s = BoxTerm::from(s);
        let p = BoxTerm::from(p);
        let o = BoxTerm::from(o);
        self.push([s, p, o]);
        Ok(true)
    }
    fn remove<T, U, V> (&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> MGResult< Self, bool> where
        T: Borrow<str>,
        U: Borrow<str>,
        V: Borrow<str>,
    {
        let i = self.iter().oks().position(|t|
            s == t.s() && p == t.p() && o == t.o()
        );
        if let Some(i) = i {
            self.swap_remove(i);
            Ok(true)
        } else {
            Ok(false)
        }
    }
}



impl<'a, T> Graph<'a> for HashSet<T> where
    T: Eq + Hash + Triple<'a> + 'a,
{
    type Triple = &'a T;
    type Error = Never;

    #[inline]
    fn iter(&'a self) -> GTripleSource<Self> {
        Box::from(self.iter().as_triple_source())
    }
}

impl MutableGraph for HashSet<[BoxTerm;3]> where
{
    type MutationError = Never;

    fn insert<T, U, V> (&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> MGResult< Self, bool> where
        T: Borrow<str>,
        U: Borrow<str>,
        V: Borrow<str>,
    {
        let s = BoxTerm::from(s);
        let p = BoxTerm::from(p);
        let o = BoxTerm::from(o);
        Ok(HashSet::insert(self, [s, p, o]))
    }
    fn remove<T, U, V> (&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> MGResult< Self, bool> where
        T: Borrow<str>,
        U: Borrow<str>,
        V: Borrow<str>,
    {
        let s = BoxTerm::from(s);
        let p = BoxTerm::from(p);
        let o = BoxTerm::from(o);
        Ok(HashSet::remove(self, &[s, p, o]))
    }
}

impl<'a, T> SetGraph for HashSet<T> where
    T: Eq + Hash + Triple<'a> + 'a,
{}




#[cfg(test)]
mod test {
    use std::collections::HashSet;
    use resiter::oks::*;

    use ::graph::*;
    use ::ns::*;
    use ::streams::*;
    use ::term::BoxTerm;

    #[test]
    fn test_slice() {
        let g = [
            [rdf::type_, rdf::type_, rdf::Property],
            [rdf::Property, rdf::type_, rdfs::Class],
            [rdfs::Class, rdf::type_, rdfs::Class],
        ];
        let len = Graph::iter(&g[..]).oks().count();
        assert_eq!(len, 3);
        let len = g.iter_for_o(&rdfs::Class).oks().count();
        assert_eq!(len, 2);
    }

    #[test]
    fn test_vec() {
        // NB: Vec<[BoxTerm;3]> can not be tested with test_graph_impl!
        // because it assumes that the passed implementation satisfies SetGraph.
        let mut g: Vec<[BoxTerm;3]> = Vec::new();

        let triples = [
            [rdf::type_, rdf::type_, rdf::Property],
            [rdf::Property, rdf::type_, rdfs::Class],
            [rdfs::Class, rdf::type_, rdfs::Class],
        ];
        let inserted = triples.into_iter().as_triple_source().in_graph(&mut g).unwrap();
        assert_eq!(inserted, triples.len());
        assert_eq!(inserted, g.len());

        let len = Graph::iter(&g[..]).oks().count();
        assert_eq!(len, 3);
        let len = g.iter_for_o(&rdfs::Class).oks().count();
        assert_eq!(len, 2);

        MutableGraph::remove(&mut g, &rdfs::Class, &rdf::type_, &rdfs::Class).unwrap();
        assert_eq!(g.len(), 2);
    }

    type HashSetBoxTerm = HashSet<[BoxTerm;3]>;
    test_graph_impl!(hashset, HashSetBoxTerm);
}