// this module is transparently re-exported by its parent `dataset::inmem`
use std::collections::HashSet;
use std::convert::Infallible;
use std::hash::Hash;

use crate::dataset::indexed::IndexedDataset;
use crate::dataset::*;
use sophia_api::quad::stream::QuadSource;
use sophia_api::quad::streaming_mode::{ByTermRefs, StreamedQuad};
use sophia_api::term::TTerm;
use sophia_api::triple::stream::StreamResult;
use sophia_term::factory::TermFactory;
use sophia_term::index_map::TermIndexMap;
use sophia_term::*;

/// A generic implementation of [`Dataset`] and [`MutableDataset`],
/// storing its terms in a [`TermIndexMap`],
/// and its triples in a [`HashSet`].
///
/// [`Dataset`]: ../trait.Dataset.html
/// [`MutableDataset`]: ../trait.MutableDataset.html
/// [`TermIndexMap`]: ../../term/index_map/trait.TermIndexMap.html
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
    fn with_capacity(capacity: usize) -> Self {
        HashDataset {
            terms: I::default(),
            quads: HashSet::with_capacity(capacity),
        }
    }

    #[inline]
    fn shrink_to_fit(&mut self) {
        self.terms.shrink_to_fit();
        self.quads.shrink_to_fit();
    }

    #[inline]
    fn get_index<T>(&self, t: &T) -> Option<Self::Index>
    where
        T: TTerm + ?Sized,
    {
        self.terms.get_index(&t.into())
    }

    #[inline]
    fn get_index_for_graph_name<T>(&self, g: Option<&T>) -> Option<Self::Index>
    where
        T: TTerm + ?Sized,
    {
        self.terms
            .get_index_for_graph_name(g.map(|g| g.into()).as_ref())
    }

    #[inline]
    fn get_term(&'_ self, i: Self::Index) -> Option<&Term<Self::TermData>> {
        self.terms.get_term(i)
    }

    #[inline]
    fn get_graph_name(&self, i: Self::Index) -> Option<Option<&Term<Self::TermData>>> {
        self.terms.get_graph_name(i)
    }

    fn insert_indexed<TS, TP, TO, TG>(
        &mut self,
        s: &TS,
        p: &TP,
        o: &TO,
        g: Option<&TG>,
    ) -> Option<[Self::Index; 4]>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
        TG: TTerm + ?Sized,
    {
        let si = self.terms.make_index(&s.into());
        let pi = self.terms.make_index(&p.into());
        let oi = self.terms.make_index(&o.into());
        let gi = self
            .terms
            .make_index_for_graph_name(g.map(|g| g.into()).as_ref());
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

    fn remove_indexed<TS, TP, TO, TG>(
        &mut self,
        s: &TS,
        p: &TP,
        o: &TO,
        g: Option<&TG>,
    ) -> Option<[Self::Index; 4]>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
        TG: TTerm + ?Sized,
    {
        let si = self.get_index(s);
        let pi = self.get_index(p);
        let oi = self.get_index(o);
        let gi = self.get_index_for_graph_name(g);
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
}

impl<I> Dataset for HashDataset<I>
where
    I: TermIndexMap,
    I::Index: Hash,
    <I::Factory as TermFactory>::TermData: 'static,
{
    #[allow(clippy::type_complexity)]
    type Quad = ByTermRefs<Term<<Self as IndexedDataset>::TermData>>;
    type Error = Infallible;

    fn quads(&self) -> DQuadSource<Self> {
        Box::from(self.quads.iter().map(move |[si, pi, oi, gi]| {
            Ok(StreamedQuad::by_term_refs(
                self.terms.get_term(*si).unwrap(),
                self.terms.get_term(*pi).unwrap(),
                self.terms.get_term(*oi).unwrap(),
                self.get_graph_name(*gi).unwrap(),
            ))
        }))
    }
}

impl<I> CollectibleDataset for HashDataset<I>
where
    I: TermIndexMap,
    I::Index: Hash,
    <I::Factory as TermFactory>::TermData: 'static,
{
    fn from_quad_source<QS: QuadSource>(quads: QS) -> StreamResult<Self, QS::Error, Infallible> {
        let (tmin, tmax) = quads.size_hint_quads();
        let cap = tmax.unwrap_or(tmin);
        let mut hash_dataset = HashDataset {
            terms: I::default(),
            quads: if cap > 0 || tmax == Some(0) {
                HashSet::with_capacity(cap)
            } else {
                HashSet::default()
            },
        };
        hash_dataset.insert_all(quads).map(|_| hash_dataset)
    }
}

impl<I> MutableDataset for HashDataset<I>
where
    I: TermIndexMap,
    I::Index: Hash,
    <I::Factory as TermFactory>::TermData: 'static,
{
    crate::impl_mutable_dataset_for_indexed_dataset!();
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
