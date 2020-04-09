// this module is transparently re-exported by its parent `graph`
// It defines implementation of Graph and MutableGraph for existing types.

use std::collections::HashSet;
use std::convert::Infallible;
use std::hash::{BuildHasher, Hash};

use resiter::oks::*;

use super::*;
use crate::triple::stream::AsTripleSource;
use crate::triple::streaming_mode::*;
use crate::triple::*;
use sophia_term::*;

impl<T> Graph for [T]
where
    T: Triple,
{
    type Triple = ByRef<T>;
    type Error = Infallible;

    #[inline]
    fn triples(&self) -> GTripleSource<Self> {
        Box::new(
            <[T]>::iter(self)
                .map(StreamedTriple::by_ref)
                .as_triple_source(),
        )
    }
}

impl<T> Graph for Vec<T>
where
    T: Triple,
{
    type Triple = ByRef<T>;
    type Error = Infallible;

    #[inline]
    fn triples(&self) -> GTripleSource<Self> {
        Box::new(
            <[T]>::iter(self)
                .map(StreamedTriple::by_ref)
                .as_triple_source(),
        )
    }
}

impl MutableGraph for Vec<[BoxTerm; 3]> {
    type MutationError = Infallible;

    fn insert<T, U, V>(&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> MGResult<Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        let s = s.clone_into();
        let p = p.clone_into();
        let o = o.clone_into();
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

impl<T, BH> Graph for HashSet<T, BH>
where
    T: Eq + Hash + Triple,
    BH: BuildHasher,
{
    type Triple = ByRef<T>;
    type Error = Infallible;

    #[inline]
    fn triples(&self) -> GTripleSource<Self> {
        Box::from(self.iter().map(StreamedTriple::by_ref).as_triple_source())
    }
}

impl<BH> MutableGraph for HashSet<[BoxTerm; 3], BH>
where
    BH: BuildHasher,
{
    type MutationError = Infallible;

    fn insert<T, U, V>(&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> MGResult<Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        let s = s.clone_into();
        let p = p.clone_into();
        let o = o.clone_into();
        Ok(HashSet::insert(self, [s, p, o]))
    }
    fn remove<T, U, V>(&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> MGResult<Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        let s = s.clone_into();
        let p = p.clone_into();
        let o = o.clone_into();
        Ok(HashSet::remove(self, &[s, p, o]))
    }
}

impl<'a, T, S: ::std::hash::BuildHasher> SetGraph for HashSet<T, S> where T: Eq + Hash + Triple {}

#[cfg(test)]
mod test {
    use resiter::oks::*;
    use std::collections::HashSet;

    use crate::graph::*;
    use crate::ns::*;
    use sophia_term::BoxTerm;

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

    // only for the purpose of testing the test macro:
    //test_graph_impl!(vec_strict, VecAsGraph, false, VecAsGraph::new, false);
    //test_graph_impl!(hashset_strict, HashSetAsGraph, true, HashSetAsGraph::new, false);
}
