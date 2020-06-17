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
use sophia_api::term::{same_graph_name, term_eq, CopiableTerm, CopyTerm, TTerm};

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

impl<T> CollectibleDataset for Vec<([T; 3], Option<T>)>
where
    T: TTerm + CopyTerm + 'static,
{
    fn from_quad_source<QS: QuadSource>(quads: QS) -> StreamResult<Self, QS::Error, Infallible> {
        quads
            .map_quads(|q| {
                (
                    [q.s().copied(), q.p().copied(), q.o().copied()],
                    q.g().map(T::copy),
                )
            })
            .into_iter()
            .collect::<Result<Self, QS::Error>>()
            .map_err(StreamError::SourceError)
    }
}

impl<T> MutableDataset for Vec<([T; 3], Option<T>)>
where
    T: TTerm + CopyTerm,
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
        let s = s.copied();
        let p = p.copied();
        let o = o.copied();
        let g = g.map(T::copy);
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
        let item = self.quads().oks().position(|q| {
            term_eq(q.s(), s) && term_eq(q.p(), p) && term_eq(q.o(), o) && same_graph_name(g, q.g())
        });
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

impl<T, S> CollectibleDataset for HashSet<([T; 3], Option<T>), S>
where
    T: TTerm + CopyTerm + Eq + Hash + 'static,
    S: BuildHasher + Default,
{
    fn from_quad_source<QS: QuadSource>(quads: QS) -> StreamResult<Self, QS::Error, Infallible> {
        quads
            .map_quads(|q| {
                (
                    [q.s().copied(), q.p().copied(), q.o().copied()],
                    q.g().map(T::copy),
                )
            })
            .into_iter()
            .collect::<Result<Self, QS::Error>>()
            .map_err(StreamError::SourceError)
    }
}

impl<T, S> MutableDataset for HashSet<([T; 3], Option<T>), S>
where
    T: TTerm + CopyTerm + Eq + Hash,
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
        let s = s.copied();
        let p = p.copied();
        let o = o.copied();
        let g = g.map(T::copy);
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
        let s = s.copied();
        let p = p.copied();
        let o = o.copied();
        let g = g.map(T::copy);
        Ok(HashSet::remove(self, &([s, p, o], g)))
    }
}

impl<T, S: BuildHasher> SetDataset for HashSet<T, S> where T: Eq + Hash + Quad {}

#[cfg(test)]
mod test {
    use super::*;
    use crate::quad::TupleQuad;
    use sophia_api::ns::*;
    use sophia_api::term::test::TestTerm;
    use sophia_api::term::SimpleIri;

    #[allow(dead_code)]
    type BoxTerm = TestTerm<Box<str>>;

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
