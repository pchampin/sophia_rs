// this module is transparently re-exported by its parent `dataset`
// It defines implementation of Graph and MutableGraph for existing types.

use std::collections::HashSet;
use std::convert::Infallible;
use std::hash::Hash;

use resiter::oks::*;

use super::*;
use crate::quad::stream::AsQuadSource;
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

impl MutableDataset for Vec<([BoxTerm; 3], Option<BoxTerm>)> {
    type MutationError = Infallible;

    fn insert<T, U, V, W>(
        &mut self,
        s: &Term<T>,
        p: &Term<U>,
        o: &Term<V>,
        g: Option<&Term<W>>,
    ) -> MDResult<Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
        W: TermData,
    {
        let s = s.clone_into();
        let p = p.clone_into();
        let o = o.clone_into();
        let g = g.map(|n| n.clone_into());
        self.push(([s, p, o], g));
        Ok(true)
    }
    fn remove<T, U, V, W>(
        &mut self,
        s: &Term<T>,
        p: &Term<U>,
        o: &Term<V>,
        g: Option<&Term<W>>,
    ) -> MDResult<Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
        W: TermData,
    {
        let item = self
            .quads()
            .oks()
            .position(|q| s == q.s() && p == q.p() && o == q.o() && same_graph_name(g, q.g()));
        if let Some(i) = item {
            self.swap_remove(i);
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

impl<Q, S: ::std::hash::BuildHasher> Dataset for HashSet<Q, S>
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

impl<S: ::std::hash::BuildHasher> MutableDataset for HashSet<([BoxTerm; 3], Option<BoxTerm>), S> {
    type MutationError = Infallible;

    fn insert<T, U, V, W>(
        &mut self,
        s: &Term<T>,
        p: &Term<U>,
        o: &Term<V>,
        g: Option<&Term<W>>,
    ) -> MDResult<Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
        W: TermData,
    {
        let s = s.clone_into();
        let p = p.clone_into();
        let o = o.clone_into();
        let g = g.map(|n| n.clone_into());
        Ok(HashSet::insert(self, ([s, p, o], g)))
    }
    fn remove<T, U, V, W>(
        &mut self,
        s: &Term<T>,
        p: &Term<U>,
        o: &Term<V>,
        g: Option<&Term<W>>,
    ) -> MDResult<Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
        W: TermData,
    {
        let s = s.clone_into();
        let p = p.clone_into();
        let o = o.clone_into();
        let g = g.map(|n| n.clone_into());
        Ok(HashSet::remove(self, &([s, p, o], g)))
    }
}

impl<T, S: ::std::hash::BuildHasher> SetDataset for HashSet<T, S> where T: Eq + Hash + Quad {}

#[cfg(test)]
mod test {
    use resiter::oks::*;
    use std::collections::HashSet;

    use crate::dataset::*;
    use crate::ns::*;
    use sophia_term::*;

    #[test]
    fn test_slice() {
        let gn = Some(StaticTerm::new_bnode("x").unwrap());
        let d = [
            ([rdf::type_, rdf::type_, rdf::Property], None),
            ([rdf::Property, rdf::type_, rdfs::Class], None),
            ([rdfs::Class, rdf::type_, rdfs::Class], gn.as_ref()),
        ];
        let len = d.quads().oks().count();
        assert_eq!(len, 3);
        let len = d.quads_with_o(&rdfs::Class).oks().count();
        assert_eq!(len, 2);
        let len = d.quads_with_g(gn.as_ref()).oks().count();
        assert_eq!(len, 1);
    }

    type VecAsDataset = Vec<([BoxTerm; 3], Option<BoxTerm>)>;
    test_dataset_impl!(vec, VecAsDataset, false);

    type HashSetAsDataset = HashSet<([BoxTerm; 3], Option<BoxTerm>)>;
    test_dataset_impl!(hashset, HashSetAsDataset);

    // only for the purpose of testing the test macro with is_set and is_gen set to false
    //test_dataset_impl!(vec_strict, VecAsDataset, false, VecAsDataset::new, false);
    //test_dataset_impl!(hashset_strict, HashSetAsDataset, true, HashSetAsDataset::new, false);
}
