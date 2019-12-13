// this module is transparently re-exported by its parent `dataset`
// It defines implementation of Graph and MutableGraph for existing types.

use std::collections::HashSet;
use std::hash::Hash;
use crate::error::*;

use resiter::oks::*;

use super::*;
use crate::quad::stream::AsQuadSource;
use crate::quad::*;
use crate::term::*;
use crate::triple::*;

impl<'a, Q> Dataset<'a> for [Q]
where
    Q: Quad<'a> + 'a,
{
    type Quad = &'a Q;
    type Error = Infallible;

    #[inline]
    fn quads(&'a self) -> DQuadSource<Self> {
        Box::new(<[Q]>::iter(self).as_quad_source())
    }
}

impl<'a, Q> Dataset<'a> for Vec<Q>
where
    Q: Quad<'a> + 'a,
{
    type Quad = &'a Q;
    type Error = Infallible;

    #[inline]
    fn quads(&'a self) -> DQuadSource<Self> {
        Box::new(<[Q]>::iter(self).as_quad_source())
    }
}

impl MutableDataset for Vec<([BoxTerm; 3], Option<BoxTerm>)> where {
    type MutationError = Infallible;

    fn insert<T, U, V, W>(
        &mut self,
        s: &Term<T>,
        p: &Term<U>,
        o: &Term<V>,
        g: Option<&Term<W>>,
    ) -> Result<bool, Self::MutationError>
    where
        T: TermData,
        U: TermData,
        V: TermData,
        W: TermData,
    {
        let s = s.into();
        let p = p.into();
        let o = o.into();
        let g = g.map(|n| n.into());
        self.push(([s, p, o], g));
        Ok(true)
    }
    fn remove<T, U, V, W>(
        &mut self,
        s: &Term<T>,
        p: &Term<U>,
        o: &Term<V>,
        g: Option<&Term<W>>,
    ) -> Result<bool, Self::MutationError>
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

impl<'a, Q, S: ::std::hash::BuildHasher> Dataset<'a> for HashSet<Q, S>
where
    Q: Eq + Hash + Quad<'a> + 'a,
{
    type Quad = &'a Q;
    type Error = Infallible;

    #[inline]
    fn quads(&'a self) -> DQuadSource<Self> {
        Box::from(self.iter().as_quad_source())
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
    ) -> Result<bool, Self::MutationError>
    where
        T: TermData,
        U: TermData,
        V: TermData,
        W: TermData,
    {
        let s = s.into();
        let p = p.into();
        let o = o.into();
        let g = g.map(|n| n.into());
        Ok(HashSet::insert(self, ([s, p, o], g)))
    }
    fn remove<T, U, V, W>(
        &mut self,
        s: &Term<T>,
        p: &Term<U>,
        o: &Term<V>,
        g: Option<&Term<W>>,
    ) -> Result<bool, Self::MutationError>
    where
        T: TermData,
        U: TermData,
        V: TermData,
        W: TermData,
    {
        let s = s.into();
        let p = p.into();
        let o = o.into();
        let g = g.map(|n| n.into());
        Ok(HashSet::remove(self, &([s, p, o], g)))
    }
}

impl<'a, T, S: ::std::hash::BuildHasher> SetDataset for HashSet<T, S> where
    T: Eq + Hash + Triple<'a> + 'a
{
}

#[cfg(test)]
mod test {
    use resiter::oks::*;
    use std::collections::HashSet;

    use crate::dataset::*;
    use crate::ns::*;
    use crate::term::*;

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
}
