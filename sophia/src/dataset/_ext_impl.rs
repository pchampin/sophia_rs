// this module is transparently re-exported by its parent `dataset`
// It defines implementation of Graph and MutableGraph for existing types.

use std::collections::HashSet;
use std::convert::Infallible;
use std::hash::{BuildHasher, Hash};

use resiter::oks::*;

use super::*;
use crate::quad::stream::{AsQuadSource, QuadSource, StreamError, StreamResult};
use crate::quad::streaming_mode::*;
use crate::quad::*;
use sophia_term::*;

impl<Q> Dataset for [Q]
where
    Q: Quad,
{
    type Quad = ByRef<Q>;
    type Error = Infallible;

    #[inline]
    fn quads(&self) -> DQuadSource<Self> {
        Box::new(<[Q]>::iter(self).map(StreamedQuad::by_ref).as_quad_source())
    }
}

impl<Q> Dataset for Vec<Q>
where
    Q: Quad,
{
    type Quad = ByRef<Q>;
    type Error = Infallible;

    #[inline]
    fn quads(&self) -> DQuadSource<Self> {
        Box::new(<[Q]>::iter(self).map(StreamedQuad::by_ref).as_quad_source())
    }
}

impl<TD> CollectibleDataset for Vec<([Term<TD>; 3], Option<Term<TD>>)>
where
    TD: TermData + 'static,
    TD: for<'x> From<&'x str>,
{
    fn from_quad_source<QS: QuadSource>(quads: QS) -> StreamResult<Self, QS::Error, Infallible> {
        quads
            .map_quads(|q| {
                (
                    [Term::copy(q.s()), Term::copy(q.p()), Term::copy(q.o())],
                    q.g().map(Term::copy),
                )
            })
            .into_iter()
            .collect::<Result<Self, QS::Error>>()
            .map_err(StreamError::SourceError)
    }
}

impl<TD> MutableDataset for Vec<([Term<TD>; 3], Option<Term<TD>>)>
where
    TD: TermData + 'static,
    TD: for<'x> From<&'x str>,
{
    type MutationError = Infallible;

    fn insert<TS, TP, TO, TG>(
        &mut self,
        s: &TS,
        p: &TP,
        o: &TO,
        g: Option<&TG>,
    ) -> MDResult<Self, bool>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
        TG: TTerm + ?Sized,
    {
        let s = Term::copy(s);
        let p = Term::copy(p);
        let o = Term::copy(o);
        let g = g.map(|n| Term::copy(n));
        self.push(([s, p, o], g));
        Ok(true)
    }
    fn remove<TS, TP, TO, TG>(
        &mut self,
        s: &TS,
        p: &TP,
        o: &TO,
        g: Option<&TG>,
    ) -> MDResult<Self, bool>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
        TG: TTerm + ?Sized,
    {
        let item = self
            .quads()
            .oks()
            .position(|q| q.s() == s && q.p() == p && q.o() == o && same_graph_name(g, q.g()));
        if let Some(i) = item {
            self.swap_remove(i);
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

impl<Q, S: BuildHasher> Dataset for HashSet<Q, S>
where
    Q: Eq + Hash + Quad,
{
    type Quad = ByRef<Q>;
    type Error = Infallible;

    #[inline]
    fn quads(&self) -> DQuadSource<Self> {
        Box::from(self.iter().map(StreamedQuad::by_ref).as_quad_source())
    }
}

impl<TD, S> CollectibleDataset for HashSet<([Term<TD>; 3], Option<Term<TD>>), S>
where
    TD: TermData + 'static,
    TD: for<'x> From<&'x str>,
    S: BuildHasher + Default,
{
    fn from_quad_source<QS: QuadSource>(quads: QS) -> StreamResult<Self, QS::Error, Infallible> {
        quads
            .map_quads(|q| {
                (
                    [Term::copy(q.s()), Term::copy(q.p()), Term::copy(q.o())],
                    q.g().map(Term::copy),
                )
            })
            .into_iter()
            .collect::<Result<Self, QS::Error>>()
            .map_err(StreamError::SourceError)
    }
}

impl<TD, S> MutableDataset for HashSet<([Term<TD>; 3], Option<Term<TD>>), S>
where
    TD: TermData + 'static,
    TD: for<'x> From<&'x str>,
    S: BuildHasher,
{
    type MutationError = Infallible;

    fn insert<TS, TP, TO, TG>(
        &mut self,
        s: &TS,
        p: &TP,
        o: &TO,
        g: Option<&TG>,
    ) -> MDResult<Self, bool>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
        TG: TTerm + ?Sized,
    {
        let s = Term::copy(s);
        let p = Term::copy(p);
        let o = Term::copy(o);
        let g = g.map(|n| Term::copy(n));
        Ok(HashSet::insert(self, ([s, p, o], g)))
    }
    fn remove<TS, TP, TO, TG>(
        &mut self,
        s: &TS,
        p: &TP,
        o: &TO,
        g: Option<&TG>,
    ) -> MDResult<Self, bool>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
        TG: TTerm + ?Sized,
    {
        let s = Term::copy(s);
        let p = Term::copy(p);
        let o = Term::copy(o);
        let g = g.map(|n| Term::copy(n));
        Ok(HashSet::remove(self, &([s, p, o], g)))
    }
}

impl<T, S: BuildHasher> SetDataset for HashSet<T, S> where T: Eq + Hash + Quad {}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ns::*;
    use crate::quad::TupleQuad;

    static D: [TupleQuad<SimpleIri>; 3] = [
        ([rdf::type_, rdf::type_, rdf::Property], None),
        ([rdf::Property, rdf::type_, rdfs::Class], None),
        ([rdfs::Class, rdf::type_, rdfs::Class], Some(rdfs::Resource)),
    ];

    #[test]
    fn test_slice() {
        let gn = Some(rdfs::Resource);
        let len = D.quads().oks().count();
        assert_eq!(len, 3);
        let len = D.quads_with_o(&rdfs::Class).oks().count();
        assert_eq!(len, 2);
        let len = D.quads_with_g(gn.as_ref()).oks().count();
        assert_eq!(len, 1);
    }

    #[cfg(feature = "all_tests")]
    type VecAsDataset = Vec<([BoxTerm; 3], Option<BoxTerm>)>;

    #[cfg(feature = "all_tests")]
    test_dataset_impl!(vec, VecAsDataset, false);

    #[cfg(feature = "all_tests")]
    #[test]
    fn test_collect_vec() {
        let d: VecAsDataset = D.quads().collect_quads().unwrap();
        assert_eq!(d.len(), 3);
        let len = d.quads_with_o(&rdfs::Class).oks().count();
        assert_eq!(len, 2);
    }

    #[cfg(feature = "all_tests")]
    type HashSetAsDataset = HashSet<([BoxTerm; 3], Option<BoxTerm>)>;

    #[cfg(feature = "all_tests")]
    test_dataset_impl!(hashset, HashSetAsDataset);

    #[cfg(feature = "all_tests")]
    #[test]
    fn test_collect_hashset() {
        let d: HashSetAsDataset = D.quads().collect_quads().unwrap();
        assert_eq!(d.len(), 3);
        let len = d.quads_with_o(&rdfs::Class).oks().count();
        assert_eq!(len, 2);
    }

    // only for the purpose of testing the test macro with is_set and is_gen set to false
    //test_dataset_impl!(vec_strict, VecAsDataset, false, false);
    //test_dataset_impl!(hashset_strict, HashSetAsDataset, true, false);
    // only for the purpose of testing test_immutable_dataset_impl
    //test_immutable_dataset_impl!(immutable, HashSetAsDataset);
}
