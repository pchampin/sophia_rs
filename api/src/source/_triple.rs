use std::error::Error;

use super::*;
use crate::graph::{CollectibleGraph, Graph, MutableGraph};
use crate::triple::Triple;

/// A triple source is a [`Source`] producing [triples](Triple).
///
/// This trait extends the [`Source`] trait with triple-specific methods.
///
/// It does not need to be explicitly implemented:
/// any [`Source`] implementation producing [triples](Triple)
/// will automatically implement [`TripleSource`].
pub trait TripleSource: Sized + for<'x> Source<Item<'x>: Triple> {
    /// Call f for some triple(s) (possibly zero) from this source, if any.
    ///
    /// Return `Ok(false)` if there are no more triples in this source.
    ///
    /// Return an error if either the source or `f` errs.
    #[inline]
    fn try_for_some_triple<E, F>(&mut self, mut f: F) -> StreamResult<bool, Self::Error, E>
    where
        E: Error + Send + Sync + 'static,
        F: FnMut(Self::Item<'_>) -> Result<(), E>,
    {
        self.try_for_some_item(|i| f(i))
    }

    /// Call f for all triples from this source.
    ///
    /// Return an error if either the source or `f` errs.
    #[inline]
    fn try_for_each_triple<F, E>(&mut self, mut f: F) -> StreamResult<(), Self::Error, E>
    where
        F: FnMut(Self::Item<'_>) -> Result<(), E>,
        E: Error + Send + Sync + 'static,
    {
        self.try_for_each_item(|i| f(i))
    }

    /// Call f for some triple(s) (possibly zero) from this source, if any.
    ///
    /// Return false if there are no more triples in this source.
    ///
    /// Return an error if either the source errs.
    #[inline]
    fn for_some_triple<F>(&mut self, mut f: F) -> Result<bool, Self::Error>
    where
        F: FnMut(Self::Item<'_>),
    {
        self.for_some_item(|i| f(i))
    }

    /// Call f for all triples from this source.
    ///
    /// Return an error if either the source errs.
    #[inline]
    fn for_each_triple<F>(&mut self, mut f: F) -> Result<(), Self::Error>
    where
        F: FnMut(Self::Item<'_>),
    {
        self.for_each_item(|i| f(i))
    }

    /// Returns a source which uses `predicate` to determine if an triple should be yielded.
    #[inline]
    fn filter_triples<'f, F>(
        self,
        mut predicate: F,
    ) -> filter::FilterTripleSource<Self, impl FnMut(&Self::Item<'_>) -> bool + 'f>
    where
        Self: Sized,
        F: FnMut(&Self::Item<'_>) -> bool + 'f,
    {
        filter::FilterTripleSource(self.filter_items(move |i| predicate(i)))
    }

    /// Returns a source that both filters and maps.
    ///
    /// See also [`TripleSource::filter_triples`] and [`TripleSource::map_triples`].
    #[inline]
    fn filter_map_triples<'f, F, T>(
        self,
        mut filter_map: F,
    ) -> filter_map::FilterMapSource<Self, impl FnMut(Self::Item<'_>) -> Option<T> + 'f>
    where
        Self: Sized,
        F: FnMut(Self::Item<'_>) -> Option<T> + 'f,
    {
        self.filter_map_items(move |i| filter_map(i))
    }

    /// Returns a source which yield the result of `map` for each triple.
    ///
    /// See also [`TripleSource::to_quads`].
    ///
    /// NB: due to [some limitations in GATs (Generic) Associated Types](https://blog.rust-lang.org/2022/10/28/gats-stabilization.html),
    /// the `map` function is currently restricted in what it can return.
    /// In particular, passing functions as trivial as `|t| t` or `|t| t.to_spo()`
    /// currently do not compile on all implementations of [`TripleSource`].
    /// Furthermore, some functions returning a [`Triple`] are accepted,
    /// but fail to make the resulting [`map::MapSource`] recognized as a [`TripleSource`].
    ///
    /// As a rule of thumb,
    /// whenever `map` returns something satisfying the `'static` lifetime,
    /// things should work as expected.
    #[inline]
    fn map_triples<'m, F, T>(
        self,
        mut map: F,
    ) -> map::MapSource<Self, impl FnMut(Self::Item<'_>) -> T + 'm>
    where
        Self: Sized,
        F: FnMut(Self::Item<'_>) -> T + 'm,
    {
        self.map_items(move |i| map(i))
    }

    /// Returns the bounds on the remaining length of the source.
    ///
    /// This method has the same contract as [`Iterator::size_hint`].
    fn size_hint_triples(&self) -> (usize, Option<usize>) {
        self.size_hint_items()
    }

    /// Convert of triples in this source to quads (belonging to the default graph).
    #[inline]
    fn to_quads(self) -> convert::ToQuads<Self>
    where
        Self: Sized,
    {
        convert::ToQuads(self)
    }

    /// Collect these triples into a new graph.
    #[inline]
    fn collect_triples<G>(self) -> StreamResult<G, Self::Error, <G as Graph>::Error>
    where
        Self: Sized,
        G: CollectibleGraph,
    {
        G::from_triple_source(self)
    }

    /// Insert all triples from this source into the given [MutableGraph].
    ///
    /// Stop on the first error (in the source or in the graph).
    #[inline]
    fn add_to_graph<G: MutableGraph>(
        self,
        graph: &mut G,
    ) -> StreamResult<usize, Self::Error, <G as MutableGraph>::MutationError>
    where
        Self: Sized,
    {
        graph.insert_all(self)
    }
}

impl<T: Sized + for<'x> Source<Item<'x>: Triple>> TripleSource for T {}

#[cfg(test)]
mod check_triple_source {
    use super::*;
    use crate::term::{SimpleTerm, Term};
    use sophia_iri::IriRef;
    use std::convert::Infallible;
    use std::fmt::Write;

    #[allow(dead_code)] // only checks that this compiles
    pub fn check_for_each<TS>(ts: TS)
    where
        TS: TripleSource,
    {
        ts.filter_triples(|t| t.s().is_iri())
            .for_each_triple(|t| println!("{:?}", t.s()))
            .unwrap();
    }

    #[allow(dead_code)] // only checks that this compiles
    fn check_triple_source_impl_generic_graph<G: Graph>(g: &G) {
        g.triples()
            .filter_triples(|t| t.s().is_iri())
            .for_each_triple(|t| println!("{:?}", t.s()))
            .unwrap();
    }

    #[allow(dead_code)] // only checks that this compiles
    fn check_triple_source_impl_concrete_graph(g: &[[SimpleTerm; 3]]) {
        g.triples()
            .filter_triples(|t| t.s().is_iri())
            .for_each_triple(|t| println!("{:?}", t.s()))
            .unwrap();
    }

    // checking that TripleSource can be implemented
    struct DummyParser<'a> {
        tokens: &'a [usize],
        pos: usize,
        buffers: [String; 3],
    }

    impl Source for DummyParser<'_> {
        type Item<'x> = [SimpleTerm<'x>; 3];
        type Error = Infallible;

        fn try_for_some_item<E2, F>(&mut self, mut f: F) -> StreamResult<bool, Self::Error, E2>
        where
            E2: Error,
            F: FnMut(Self::Item<'_>) -> Result<(), E2>,
        {
            if self.tokens.len() - self.pos < 3 {
                Ok(false)
            } else {
                for i in 0..3 {
                    write!(&mut self.buffers[i], "b{}", self.tokens[self.pos + i]).unwrap();
                }
                let t = [
                    IriRef::new_unchecked(&self.buffers[0][..]).into_term(),
                    IriRef::new_unchecked(&self.buffers[1][..]).into_term(),
                    IriRef::new_unchecked(&self.buffers[2][..]).into_term(),
                ];
                f(t).map_err(SinkError).map(|_| true)
            }
        }
    }

    #[allow(dead_code)] // only checks that this compiles
    fn check_triple_source_impl_by_iterator(v: Vec<Result<[SimpleTerm; 3], std::io::Error>>) {
        v.into_iter()
            .for_each_triple(|t| println!("{:?}", t.s()))
            .unwrap();
    }
}
