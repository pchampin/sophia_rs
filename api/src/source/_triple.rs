use super::*;
use crate::graph::{CollectibleGraph, Graph, MutableGraph};
use crate::triple::Triple;

/// A triple source produces [triples](Triple), and may also fail in the process.
///
/// see [module documentation](super) for the rationale of his trait.
///
/// # Common implementors
///
/// Any iterator yielding [results](std::result::Result) of [`Triple`]
/// implements the [`TripleSource`] trait.
///
/// Any iterator of [`Triple`] can also be converted to an [`Infallible`] [`TripleSource`]
/// thanks to the [`IntoTripleSource`] extension trait.
pub trait TripleSource {
    /// The type of triples this source yields.
    type Triple<'x>: Triple;
    /// The type of errors produced by this source.
    type Error: Error + 'static;

    /// Call f for some triple(s) (possibly zero) from this source, if any.
    ///
    /// Return `Ok(false)` if there are no more triples in this source.
    ///
    /// Return an error if either the source or `f` errs.
    fn try_for_some_triple<E, F>(&mut self, f: F) -> StreamResult<bool, Self::Error, E>
    where
        E: Error,
        F: FnMut(Self::Triple<'_>) -> Result<(), E>;

    /// Call f for all triples from this source.
    ///
    /// Return an error if either the source or `f` errs.
    #[inline]
    fn try_for_each_triple<F, E>(&mut self, mut f: F) -> StreamResult<(), Self::Error, E>
    where
        F: FnMut(Self::Triple<'_>) -> Result<(), E>,
        E: Error,
    {
        while self.try_for_some_triple(&mut f)? {}
        Ok(())
    }

    /// Call f for some triple(s) (possibly zero) from this source, if any.
    ///
    /// Return false if there are no more triples in this source.
    ///
    /// Return an error if either the source errs.
    #[inline]
    fn for_some_triple<F>(&mut self, f: &mut F) -> Result<bool, Self::Error>
    where
        F: FnMut(Self::Triple<'_>),
    {
        self.try_for_some_triple(|t| -> Result<(), Self::Error> {
            f(t);
            Ok(())
        })
        .map_err(StreamError::inner_into)
    }

    /// Call f for all triples from this source.
    ///
    /// Return an error if either the source errs.
    #[inline]
    fn for_each_triple<F>(&mut self, f: F) -> Result<(), Self::Error>
    where
        F: FnMut(Self::Triple<'_>),
    {
        let mut f = f;
        while self.for_some_triple(&mut f)? {}
        Ok(())
    }

    /// Returns a source which uses `predicate` to determine if an triple should be yielded.
    #[inline]
    fn filter_triples<F>(self, predicate: F) -> filter::FilterTripleSource<Self, F>
    where
        Self: Sized,
        F: FnMut(&Self::Triple<'_>) -> bool,
    {
        filter::FilterTripleSource {
            source: self,
            predicate,
        }
    }

    /// Returns a source that both filters and maps.
    ///
    /// See also [`TripleSource::filter_triples`] and [`TripleSource::map_triples`].
    #[inline]
    fn filter_map_triples<F, T>(self, filter_map: F) -> filter_map::FilterMapTripleSource<Self, F>
    where
        Self: Sized,
        F: FnMut(Self::Triple<'_>) -> Option<T>,
    {
        filter_map::FilterMapTripleSource {
            source: self,
            filter_map,
        }
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
    /// but fail to make the resulting [`map::MapTripleSource`] recognized as a [`TripleSource`].
    ///
    /// As a rule of thumb,
    /// whenever `map` returns something satisfying the `'static` lifetime,
    /// things should work as expected.
    #[inline]
    fn map_triples<F, T>(self, map: F) -> map::MapTripleSource<Self, F>
    where
        Self: Sized,
        F: FnMut(Self::Triple<'_>) -> T,
    {
        map::MapTripleSource { source: self, map }
    }

    // Convert of triples in this source to quads (belonging to the default graph).
    #[inline]
    fn to_quads(self) -> convert::ToQuads<Self>
    where
        Self: Sized,
    {
        convert::ToQuads(self)
    }

    /// Returns the bounds on the remaining length of the source.
    ///
    /// This method has the same contract as [`Iterator::size_hint`].
    fn size_hint_triples(&self) -> (usize, Option<usize>) {
        (0, None)
    }

    /// Collect these triples into a new graph.
    #[inline]
    fn collect_triples<G>(self) -> StreamResult<G, Self::Error, <G as Graph>::Error>
    where
        Self: Sized,
        for<'x> Self::Triple<'x>: Triple,
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
        for<'x> Self::Triple<'x>: Triple,
    {
        graph.insert_all(self)
    }
}

impl<'a, I, T, E> TripleSource for I
where
    I: Iterator<Item = Result<T, E>> + 'a,
    T: Triple,
    E: Error + 'static,
{
    type Triple<'x> = T;
    type Error = E;

    fn try_for_some_triple<E2, F>(&mut self, mut f: F) -> StreamResult<bool, Self::Error, E2>
    where
        E2: Error,
        F: FnMut(Self::Triple<'_>) -> Result<(), E2>,
    {
        match self.next() {
            Some(Err(e)) => Err(SourceError(e)),
            Some(Ok(t)) => {
                f(t).map_err(SinkError)?;
                Ok(true)
            }
            None => Ok(false),
        }
    }

    fn size_hint_triples(&self) -> (usize, Option<usize>) {
        self.size_hint()
    }
}

/// An extension trait for iterators,
/// converting them to an [`Infallible`] [`TripleSource`].
pub trait IntoTripleSource: Iterator + Sized {
    /// Convert this iterator into an [`Infallible`] [`TripleSource`].
    #[allow(clippy::type_complexity)]
    fn into_triple_source(
        self,
    ) -> std::iter::Map<
        Self,
        fn(<Self as Iterator>::Item) -> Result<<Self as Iterator>::Item, Infallible>,
    > {
        self.map(Ok::<_, Infallible>)
    }
}

impl<I> IntoTripleSource for I
where
    I: Iterator,
    I::Item: Triple,
{
}

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

    impl<'a> TripleSource for DummyParser<'a> {
        type Triple<'x> = [SimpleTerm<'x>; 3];
        type Error = Infallible;

        fn try_for_some_triple<E2, F>(&mut self, mut f: F) -> StreamResult<bool, Self::Error, E2>
        where
            E2: Error,
            F: FnMut(Self::Triple<'_>) -> Result<(), E2>,
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
