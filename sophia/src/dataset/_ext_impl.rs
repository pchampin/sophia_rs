// this module is transparently re-exported by its parent `dataset`
// It defines implementation of Graph and MutableGraph for existing types.

use std::collections::HashSet;
use std::hash::Hash;

use resiter::oks::*;

use super::*;
use crate::error::*;
use crate::quad::stream::AsQuadSource;
use crate::quad::*;
use crate::term::graph_id::*;
use crate::term::*;
use crate::triple::*;

impl<'a, Q> Dataset<'a> for [Q]
where
    Q: Quad<'a> + 'a,
{
    type Quad = &'a Q;
    type Error = Never;

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
    type Error = Never;

    #[inline]
    fn quads(&'a self) -> DQuadSource<Self> {
        Box::new(<[Q]>::iter(self).as_quad_source())
    }
}

impl MutableDataset for Vec<([BoxTerm; 3], GraphId<Box<str>>)> where {
    type MutationError = Never;

    fn insert<T, U, V, W>(
        &mut self,
        s: &Term<T>,
        p: &Term<U>,
        o: &Term<V>,
        g: &GraphId<W>,
    ) -> MDResult<Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
        W: TermData,
    {
        let s = BoxTerm::from(s);
        let p = BoxTerm::from(p);
        let o = BoxTerm::from(o);
        let g = GraphId::from(g);
        self.push(([s, p, o], g));
        Ok(true)
    }
    fn remove<T, U, V, W>(
        &mut self,
        s: &Term<T>,
        p: &Term<U>,
        o: &Term<V>,
        g: &GraphId<W>,
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
            .position(|q| s == q.s() && p == q.p() && o == q.o() && g == q.g());
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
    type Error = Never;

    #[inline]
    fn quads(&'a self) -> DQuadSource<Self> {
        Box::from(self.iter().as_quad_source())
    }
}

impl<S: ::std::hash::BuildHasher> MutableDataset for HashSet<([BoxTerm; 3], GraphId<Box<str>>), S> {
    type MutationError = Never;

    fn insert<T, U, V, W>(
        &mut self,
        s: &Term<T>,
        p: &Term<U>,
        o: &Term<V>,
        g: &GraphId<W>,
    ) -> MDResult<Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
        W: TermData,
    {
        let s = BoxTerm::from(s);
        let p = BoxTerm::from(p);
        let o = BoxTerm::from(o);
        let g = GraphId::from(g);
        Ok(HashSet::insert(self, ([s, p, o], g)))
    }
    fn remove<T, U, V, W>(
        &mut self,
        s: &Term<T>,
        p: &Term<U>,
        o: &Term<V>,
        g: &GraphId<W>,
    ) -> MDResult<Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
        W: TermData,
    {
        let s = BoxTerm::from(s);
        let p = BoxTerm::from(p);
        let o = BoxTerm::from(o);
        let g = GraphId::from(g);
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
    use crate::term::{graph_id::*, *};

    #[test]
    fn test_slice() {
        let gn = StaticTerm::new_bnode("x").unwrap();
        let gn = GraphId::<&str>::from(&gn);
        let d = [
            ([rdf::type_, rdf::type_, rdf::Property], GraphId::Default),
            ([rdf::Property, rdf::type_, rdfs::Class], GraphId::Default),
            ([rdfs::Class, rdf::type_, rdfs::Class], gn.clone()),
        ];
        let len = d.quads().oks().count();
        assert_eq!(len, 3);
        let len = d.quads_with_o(&rdfs::Class).oks().count();
        assert_eq!(len, 2);
        let len = d.quads_with_g(&gn).oks().count();
        assert_eq!(len, 1);
    }

    type VecAsDataset = Vec<([BoxTerm; 3], GraphId<Box<str>>)>;
    test_dataset_impl!(vec, VecAsDataset, false);

    type HashSetAsDataset = HashSet<([BoxTerm; 3], GraphId<Box<str>>)>;
    test_dataset_impl!(hashset, HashSetAsDataset);
}
