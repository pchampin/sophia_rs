// this module is transparently re-exported by its parent `dataset::inmem`
use std::collections::HashSet;
use std::hash::Hash;

use crate::dataset::indexed::IndexedDataset;
use crate::dataset::*;
use crate::error::*;
use crate::term::factory::TermFactory;
use crate::term::index_map::TermIndexMap;
use crate::term::*;

/// A generic implementation of [`Dataset`] and [`MutableDataset`],
/// storing its terms in a [`TermIndexMap`],
/// and its triples in a [`HashSet`].
///
/// [`Dataset`]: ../trait.Dataset.html
/// [`MutableDataset`]: ../trait.MutableDataset.html
/// [`TermIndexMap`]: ../../graph/index/trait.TermIndexMap.html
/// [`HashSet`]: https://doc.rust-lang.org/std/collections/struct.HashSet.html
#[derive(Default)]
pub struct HashDataset<I>
where
    I: TermIndexMap,
    I::Index: Hash,
    <I::Factory as TermFactory>::TermData: 'static,
{
    terms: I,
    quads: HashSet<[I::Index; 4]>,
}

impl<I> HashDataset<I>
where
    I: TermIndexMap,
    I::Index: Hash,
{
    pub fn new() -> HashDataset<I> {
        HashDataset {
            terms: I::default(),
            quads: HashSet::new(),
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.quads.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.quads.is_empty()
    }
}

impl<I> IndexedDataset for HashDataset<I>
where
    I: TermIndexMap,
    I::Index: Hash,
    <I::Factory as TermFactory>::TermData: 'static,
{
    type Index = I::Index;
    type TermData = <I::Factory as TermFactory>::TermData;

    #[inline]
    fn get_index<T>(&self, t: &Term<T>) -> Option<Self::Index>
    where
        T: TermData,
    {
        self.terms.get_index(&t.into())
    }

    #[inline]
    fn get_index_for_graph_id<T>(&self, g: &GraphName<T>) -> Option<Self::Index>
    where
        T: TermData,
    {
        self.terms.get_index_for_graph_id(&g.convert_graph_name())
    }

    #[inline]
    fn get_term(&'_ self, i: Self::Index) -> Option<&Term<Self::TermData>> {
        self.terms.get_term(i)
    }

    #[inline]
    fn get_graph_id(&self, i: Self::Index) -> Option<&GraphName<Self::TermData>> {
        self.terms.get_graph_id(i)
    }

    fn insert_indexed<T, U, V, W>(
        &mut self,
        s: &Term<T>,
        p: &Term<U>,
        o: &Term<V>,
        g: &GraphName<W>,
    ) -> Option<[I::Index; 4]>
    where
        T: TermData,
        U: TermData,
        V: TermData,
        W: TermData,
    {
        let si = self.terms.make_index(&s.into());
        let pi = self.terms.make_index(&p.into());
        let oi = self.terms.make_index(&o.into());
        let gi = self.terms.make_index_for_graph_id(&g.convert_graph_name());
        let modified = self.quads.insert([si, pi, oi, gi]);
        if modified {
            Some([si, pi, oi, gi])
        } else {
            self.terms.dec_ref(si);
            self.terms.dec_ref(pi);
            self.terms.dec_ref(oi);
            self.terms.dec_ref(gi);
            None
        }
    }

    fn remove_indexed<T, U, V, W>(
        &mut self,
        s: &Term<T>,
        p: &Term<U>,
        o: &Term<V>,
        g: &GraphName<W>,
    ) -> Option<[I::Index; 4]>
    where
        T: TermData,
        U: TermData,
        V: TermData,
        W: TermData,
    {
        let si = self.get_index(s);
        let pi = self.get_index(p);
        let oi = self.get_index(o);
        let gi = self.get_index_for_graph_id(g);
        if let (Some(si), Some(pi), Some(oi), Some(gi)) = (si, pi, oi, gi) {
            let modified = self.quads.remove(&[si, pi, oi, gi]);
            if modified {
                self.terms.dec_ref(si);
                self.terms.dec_ref(pi);
                self.terms.dec_ref(oi);
                self.terms.dec_ref(gi);
                return Some([si, pi, oi, gi]);
            }
        }
        None
    }

    #[inline]
    fn shrink_to_fit(&mut self) {
        self.terms.shrink_to_fit();
        self.quads.shrink_to_fit();
    }
}

impl<'a, I> Dataset<'a> for HashDataset<I>
where
    I: TermIndexMap,
    I::Index: Hash,
    <I::Factory as TermFactory>::TermData: 'static,
{
    type Quad = (
        [&'a Term<<Self as IndexedDataset>::TermData>; 3],
        &'a GraphName<<Self as IndexedDataset>::TermData>,
    );
    type Error = Never;

    fn quads(&'a self) -> DQuadSource<'a, Self> {
        Box::from(self.quads.iter().map(move |[si, pi, oi, gi]| {
            Ok((
                [
                    self.terms.get_term(*si).unwrap(),
                    self.terms.get_term(*pi).unwrap(),
                    self.terms.get_term(*oi).unwrap(),
                ],
                self.get_graph_id(*gi).unwrap(),
            ))
        }))
    }
}

impl<I> MutableDataset for HashDataset<I>
where
    I: TermIndexMap,
    I::Index: Hash,
    <I::Factory as TermFactory>::TermData: 'static,
{
    impl_mutable_dataset_for_indexed_dataset!();
}

impl<I> SetDataset for HashDataset<I>
where
    I: TermIndexMap,
    I::Index: Hash,
{
}

#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially in ./inmem.rs).
}
