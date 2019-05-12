// this module is transparently re-exported by its parent `graph`
// It defines implementation of Graph and MutableGraph for existing types.

use std::collections::HashSet;
use std::hash::{BuildHasher, Hash};

use resiter::oks::*;

use super::*;
use crate::error::*;
use crate::term::*;
use crate::triple::stream::AsTripleSource;
use crate::triple::*;

impl<'a, T> Graph<'a> for [T]
where
    T: Triple<'a> + 'a,
{
    type Triple = &'a T;
    type Error = Never;

    #[inline]
    fn triples(&'a self) -> GTripleSource<Self> {
        Box::new(<[T]>::iter(self).as_triple_source())
    }
}

impl<'a, T> Graph<'a> for Vec<T>
where
    T: Triple<'a> + 'a,
{
    type Triple = &'a T;
    type Error = Never;

    #[inline]
    fn triples(&'a self) -> GTripleSource<Self> {
        Box::new(<[T]>::iter(self).as_triple_source())
    }
}

impl MutableGraph for Vec<[BoxTerm; 3]> {
    type MutationError = Never;

    fn insert<T, U, V>(&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> MGResult<Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        let s = BoxTerm::from(s);
        let p = BoxTerm::from(p);
        let o = BoxTerm::from(o);
        self.push([s, p, o]);
        Ok(true)
    }
    fn remove<T, U, V>(&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> MGResult<Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        let i = self
            .triples()
            .oks()
            .position(|t| s == t.s() && p == t.p() && o == t.o());
        if let Some(i) = i {
            self.swap_remove(i);
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

impl<'a, T, BH> Graph<'a> for HashSet<T, BH>
where
    T: Eq + Hash + Triple<'a> + 'a,
    BH: BuildHasher,
{
    type Triple = &'a T;
    type Error = Never;

    #[inline]
    fn triples(&'a self) -> GTripleSource<Self> {
        Box::from(self.iter().as_triple_source())
    }
}

impl<BH> MutableGraph for HashSet<[BoxTerm; 3], BH>
where
    BH: BuildHasher,
{
    type MutationError = Never;

    fn insert<T, U, V>(&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> MGResult<Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        let s = BoxTerm::from(s);
        let p = BoxTerm::from(p);
        let o = BoxTerm::from(o);
        Ok(HashSet::insert(self, [s, p, o]))
    }
    fn remove<T, U, V>(&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> MGResult<Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        let s = BoxTerm::from(s);
        let p = BoxTerm::from(p);
        let o = BoxTerm::from(o);
        Ok(HashSet::remove(self, &[s, p, o]))
    }
}

impl<'a, T, S: ::std::hash::BuildHasher> SetGraph for HashSet<T, S> where
    T: Eq + Hash + Triple<'a> + 'a
{
}

#[cfg(test)]
mod test {
    use resiter::oks::*;
    use std::collections::HashSet;

    use crate::graph::*;
    use crate::ns::*;
    use crate::term::BoxTerm;

    #[test]
    fn test_slice() {
        let g = [
            [rdf::type_, rdf::type_, rdf::Property],
            [rdf::Property, rdf::type_, rdfs::Class],
            [rdfs::Class, rdf::type_, rdfs::Class],
        ];
        let len = g.triples().oks().count();
        assert_eq!(len, 3);
        let len = g.triples_with_o(&rdfs::Class).oks().count();
        assert_eq!(len, 2);
    }

    type VecAsGraph = Vec<[BoxTerm; 3]>;
    test_graph_impl!(vec, VecAsGraph, false);

    type HashSetAsGraph = HashSet<[BoxTerm; 3]>;
    test_graph_impl!(hashset, HashSetAsGraph);
}
