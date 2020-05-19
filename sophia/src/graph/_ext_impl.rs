// this module is transparently re-exported by its parent `graph`
// It defines implementation of Graph and MutableGraph for existing types.

use std::collections::HashSet;
use std::convert::Infallible;
use std::hash::{BuildHasher, Hash};

use resiter::oks::*;

use super::*;
use crate::triple::stream::{AsTripleSource, StreamError, StreamResult, TripleSource};
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

impl<TD> CollectibleGraph for Vec<[Term<TD>; 3]>
where
    TD: TermData + 'static,
    TD: for<'x> From<&'x str>,
{
    fn from_triple_source<TS: TripleSource>(
        triples: TS,
    ) -> StreamResult<Self, TS::Error, Infallible> {
        triples
            .map_triples(|t| [Term::copy(t.s()), Term::copy(t.p()), Term::copy(t.o())])
            .into_iter()
            .collect::<Result<Self, TS::Error>>()
            .map_err(StreamError::SourceError)
    }
}

impl<TD> MutableGraph for Vec<[Term<TD>; 3]>
where
    TD: TermData + 'static,
    TD: for<'x> From<&'x str>,
{
    type MutationError = Infallible;

    fn insert<TS, TP, TO>(&mut self, s: &TS, p: &TP, o: &TO) -> MGResult<Self, bool>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
    {
        let s = Term::copy(s);
        let p = Term::copy(p);
        let o = Term::copy(o);
        self.push([s, p, o]);
        Ok(true)
    }
    fn remove<TS, TP, TO>(&mut self, s: &TS, p: &TP, o: &TO) -> MGResult<Self, bool>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
    {
        let i = self
            .triples()
            .oks()
            .position(|t| term_eq(s, t.s()) && term_eq(p, t.p()) && term_eq(o, t.o()));
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

impl<TD, BH> CollectibleGraph for HashSet<[Term<TD>; 3], BH>
where
    TD: TermData + 'static,
    TD: for<'x> From<&'x str>,
    BH: BuildHasher + Default,
{
    fn from_triple_source<TS: TripleSource>(
        triples: TS,
    ) -> StreamResult<Self, TS::Error, Infallible> {
        triples
            .map_triples(|t| [Term::copy(t.s()), Term::copy(t.p()), Term::copy(t.o())])
            .into_iter()
            .collect::<Result<Self, TS::Error>>()
            .map_err(StreamError::SourceError)
    }
}

impl<TD, BH> MutableGraph for HashSet<[Term<TD>; 3], BH>
where
    TD: TermData + 'static,
    TD: for<'x> From<&'x str>,
    BH: BuildHasher,
{
    type MutationError = Infallible;

    fn insert<TS, TP, TO>(&mut self, s: &TS, p: &TP, o: &TO) -> MGResult<Self, bool>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
    {
        let s = Term::copy(s);
        let p = Term::copy(p);
        let o = Term::copy(o);
        Ok(HashSet::insert(self, [s, p, o]))
    }
    fn remove<TS, TP, TO>(&mut self, s: &TS, p: &TP, o: &TO) -> MGResult<Self, bool>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
    {
        let s = Term::copy(s);
        let p = Term::copy(p);
        let o = Term::copy(o);
        Ok(HashSet::remove(self, &[s, p, o]))
    }
}

impl<'a, T, S: BuildHasher> SetGraph for HashSet<T, S> where T: Eq + Hash + Triple {}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ns::*;

    static G: [[SimpleIri; 3]; 3] = [
        [rdf::type_, rdf::type_, rdf::Property],
        [rdf::Property, rdf::type_, rdfs::Class],
        [rdfs::Class, rdf::type_, rdfs::Class],
    ];

    #[test]
    fn test_slice() {
        let len = G.triples().oks().count();
        assert_eq!(len, 3);
        let len = G.triples_with_o(&rdfs::Class).oks().count();
        assert_eq!(len, 2);
    }

    #[cfg(feature = "all_tests")]
    type VecAsGraph = Vec<[sophia_term::BoxTerm; 3]>;

    #[cfg(feature = "all_tests")]
    test_graph_impl!(vec, VecAsGraph, false);

    #[cfg(feature = "all_tests")]
    #[test]
    fn test_collect_vec() {
        let g: VecAsGraph = G.triples().collect_triples().unwrap();
        assert_eq!(g.len(), 3);
        let len = g.triples_with_o(&rdfs::Class).oks().count();
        assert_eq!(len, 2);
    }

    #[cfg(feature = "all_tests")]
    type HashSetAsGraph = HashSet<[sophia_term::BoxTerm; 3]>;

    #[cfg(feature = "all_tests")]
    test_graph_impl!(hashset, HashSetAsGraph);

    #[cfg(feature = "all_tests")]
    #[test]
    fn test_collect_hashset() {
        let g: HashSetAsGraph = G.triples().collect_triples().unwrap();
        assert_eq!(g.len(), 3);
        let len = g.triples_with_o(&rdfs::Class).oks().count();
        assert_eq!(len, 2);
    }

    // only for the purpose of testing the test macro with is_set and is_gen set to false
    //test_graph_impl!(vec_strict, VecAsGraph, false, false);
    //test_graph_impl!(hashset_strict, HashSetAsGraph, false, false);
    // only for the purpose of testing test_immutable_graph_impl
    //test_immutable_graph_impl!(immutable, HashSetAsGraph);
}
