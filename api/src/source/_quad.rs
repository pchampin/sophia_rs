use super::*;
use crate::dataset::{CollectibleDataset, Dataset, MutableDataset};
use crate::quad::Quad;

/// A quad source produces [quads](Quad), and may also fail in the process.
///
/// see [module documentation](super) for the rationale of his trait.
///
/// # Common implementors
///
/// Any iterator yielding [results](std::result::Result) of [`Quad`]
/// implements the [`QuadSource`] trait.
///
/// Any iterator of [`Quad`] can also be converted to an [`Infallible`] [`QuadSource`]
/// thanks to the [`IntoQuadSource`] extension trait.
pub trait QuadSource {
    /// The type of quads this source yields.
    type Quad<'x>: Quad;
    /// The type of errors produced by this source.
    type Error: Error + 'static;

    /// Call f for some quad(s) (possibly zero) from this source, if any.
    ///
    /// Return `Ok(false)` if there are no more quads in this source.
    ///
    /// Return an error if either the source or `f` errs.
    fn try_for_some_quad<E, F>(&mut self, f: F) -> StreamResult<bool, Self::Error, E>
    where
        E: Error,
        F: FnMut(Self::Quad<'_>) -> Result<(), E>;

    /// Call f for all quads from this source.
    ///
    /// Return an error if either the source or `f` errs.
    #[inline]
    fn try_for_each_quad<F, E>(&mut self, mut f: F) -> StreamResult<(), Self::Error, E>
    where
        F: FnMut(Self::Quad<'_>) -> Result<(), E>,
        E: Error,
    {
        while self.try_for_some_quad(&mut f)? {}
        Ok(())
    }

    /// Call f for some quad(s) (possibly zero) from this source, if any.
    ///
    /// Return false if there are no more quads in this source.
    ///
    /// Return an error if either the source errs.
    #[inline]
    fn for_some_quad<F>(&mut self, f: &mut F) -> Result<bool, Self::Error>
    where
        F: FnMut(Self::Quad<'_>),
    {
        self.try_for_some_quad(|t| -> Result<(), Self::Error> {
            f(t);
            Ok(())
        })
        .map_err(StreamError::inner_into)
    }

    /// Call f for all quads from this source.
    ///
    /// Return an error if either the source errs.
    #[inline]
    fn for_each_quad<F>(&mut self, f: F) -> Result<(), Self::Error>
    where
        F: FnMut(Self::Quad<'_>),
    {
        let mut f = f;
        while self.for_some_quad(&mut f)? {}
        Ok(())
    }

    /// Returns a source which uses `predicate` to determine if an quad should be yielded.
    #[inline]
    fn filter_quads<F>(self, predicate: F) -> filter::FilterQuadSource<Self, F>
    where
        Self: Sized,
        F: FnMut(&Self::Quad<'_>) -> bool,
    {
        filter::FilterQuadSource {
            source: self,
            predicate,
        }
    }

    /// Returns a source that both filters and maps.
    ///
    /// See also [`QuadSource::filter_quads`] and [`QuadSource::map_quads`].
    #[inline]
    fn filter_map_quads<F, T>(self, filter_map: F) -> filter_map::FilterMapQuadSource<Self, F>
    where
        Self: Sized,
        F: FnMut(Self::Quad<'_>) -> Option<T>,
    {
        filter_map::FilterMapQuadSource {
            source: self,
            filter_map,
        }
    }

    /// Returns a source which yield the result of `map` for each quad.
    ///
    /// See also [`QuadSource::to_triples`].
    ///
    /// NB: due to [some limitations in GATs (Generic) Associated Types](https://blog.rust-lang.org/2022/10/28/gats-stabilization.html),
    /// the `map` function is currently restricted in what it can return.
    /// In particular, passing functions as trivial as `|q| q` or `|q| q.to_spog()`
    /// currently do not compile on all implementations of [`QuadSource`].
    /// Furthermore, some functions returning a [`Quad`] are accepted,
    /// but fail to make the resulting [`map::MapQuadSource`] recognized as a [`QuadSource`].
    ///
    /// As a rule of thumb,
    /// whenever `map` returns something satisfying the `'static` lifetime,
    /// things should work as expected.
    #[inline]
    fn map_quads<F, T>(self, map: F) -> map::MapQuadSource<Self, F>
    where
        Self: Sized,
        F: FnMut(Self::Quad<'_>) -> T,
    {
        map::MapQuadSource { source: self, map }
    }

    // Convert of quads in this source to triples (stripping the graph name).
    fn to_triples(self) -> convert::ToTriples<Self>
    where
        Self: Sized,
    {
        convert::ToTriples(self)
    }

    /// Returns the bounds on the remaining length of the source.
    ///
    /// This method has the same contract as [`Iterator::size_hint`].
    fn size_hint_quads(&self) -> (usize, Option<usize>) {
        (0, None)
    }

    /// Collect these quads into a new dataset.
    #[inline]
    fn collect_quads<D>(self) -> StreamResult<D, Self::Error, <D as Dataset>::Error>
    where
        Self: Sized,
        for<'x> Self::Quad<'x>: Quad,
        D: CollectibleDataset,
    {
        D::from_quad_source(self)
    }

    /// Insert all quads from this source into the given [MutableDataset].
    ///
    /// Stop on the first error (in the source or in the dataset).
    #[inline]
    fn add_to_dataset<D: MutableDataset>(
        self,
        dataset: &mut D,
    ) -> StreamResult<usize, Self::Error, <D as MutableDataset>::MutationError>
    where
        Self: Sized,
        for<'x> Self::Quad<'x>: Quad,
    {
        dataset.insert_all(self)
    }
}

impl<'a, I, T, E> QuadSource for I
where
    I: Iterator<Item = Result<T, E>> + 'a,
    T: Quad,
    E: Error + 'static,
{
    type Quad<'x> = T;
    type Error = E;

    fn try_for_some_quad<E2, F>(&mut self, mut f: F) -> StreamResult<bool, Self::Error, E2>
    where
        E2: Error,
        F: FnMut(Self::Quad<'_>) -> Result<(), E2>,
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

    fn size_hint_quads(&self) -> (usize, Option<usize>) {
        self.size_hint()
    }
}

/// An extension trait for iterators,
/// converting them to an [`Infallible`] [`QuadSource`].
pub trait IntoQuadSource: Iterator + Sized {
    /// Convert this iterator into an [`Infallible`] [`QuadSource`].
    #[allow(clippy::type_complexity)]
    fn into_quad_source(
        self,
    ) -> std::iter::Map<
        Self,
        fn(<Self as Iterator>::Item) -> Result<<Self as Iterator>::Item, Infallible>,
    > {
        self.map(Ok::<_, Infallible>)
    }
}

impl<I> IntoQuadSource for I
where
    I: Iterator,
    I::Item: Quad,
{
}

#[cfg(test)]
mod check_quad_source {
    use super::*;
    use crate::quad::Spog;
    use crate::term::{SimpleTerm, Term};
    use sophia_iri::IriRef;
    use std::convert::Infallible;
    use std::fmt::Write;

    #[allow(dead_code)] // only checks that this compiles
    pub fn check_for_each<TS>(ts: TS)
    where
        TS: QuadSource,
    {
        ts.filter_quads(|t| t.s().is_iri())
            .for_each_quad(|t| println!("{:?}", t.s()))
            .unwrap();
    }

    #[allow(dead_code)] // only checks that this compiles
    fn check_quad_source_impl_generic_dataset<D: Dataset>(d: &D) {
        d.quads()
            .filter_quads(|t| t.s().is_iri())
            .for_each_quad(|t| println!("{:?}", t.s()))
            .unwrap();
    }

    #[allow(dead_code)] // only checks that this compiles
    fn check_quad_source_impl_concrete_dataset(d: &[Spog<SimpleTerm>]) {
        d.quads()
            .filter_quads(|t| t.s().is_iri())
            .for_each_quad(|t| println!("{:?}", t.s()))
            .unwrap();
    }

    // checking that QuadSource can be implemented
    struct DummyParser<'a> {
        tokens: &'a [usize],
        pos: usize,
        buffers: [String; 3],
    }

    impl<'a> QuadSource for DummyParser<'a> {
        type Quad<'x> = Spog<SimpleTerm<'x>>;
        type Error = Infallible;

        fn try_for_some_quad<E2, F>(&mut self, mut f: F) -> StreamResult<bool, Self::Error, E2>
        where
            E2: Error,
            F: FnMut(Self::Quad<'_>) -> Result<(), E2>,
        {
            if self.tokens.len() - self.pos < 3 {
                Ok(false)
            } else {
                for i in 0..3 {
                    write!(&mut self.buffers[i], "b{}", self.tokens[self.pos + i]).unwrap();
                }
                let q = (
                    [
                        IriRef::new_unchecked(&self.buffers[0][..]).into_term(),
                        IriRef::new_unchecked(&self.buffers[1][..]).into_term(),
                        IriRef::new_unchecked(&self.buffers[2][..]).into_term(),
                    ],
                    None,
                );
                f(q).map_err(SinkError).map(|_| true)
            }
        }
    }

    #[allow(dead_code)] // only checks that this compiles
    fn check_quad_source_impl_by_iterator(v: Vec<Result<Spog<SimpleTerm>, std::io::Error>>) {
        v.into_iter()
            .for_each_quad(|q| println!("{:?}", q.s()))
            .unwrap();
    }

    /* Currently does not work...
    #[allow(dead_code)]
    fn check_quad_source_map<S: QuadSource>(mut qs: S) {
        qs.for_each_quad(|q| println!("{:?}", q.s())).unwrap();
        qs.map_quads(|q| q)
            .for_each_quad(|q| println!("{:?}", q.s()))
            .unwrap();
    }
    */
}
