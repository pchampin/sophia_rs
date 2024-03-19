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
pub trait QuadSource: Source + IsQuadSource {
    /// Call f for some quad(s) (possibly zero) from this source, if any.
    ///
    /// Return `Ok(false)` if there are no more quads in this source.
    ///
    /// Return an error if either the source or `f` errs.
    #[inline]
    fn try_for_some_quad<E, F>(&mut self, mut f: F) -> StreamResult<bool, Self::Error, E>
    where
        E: Error,
        F: FnMut(Self::Quad<'_>) -> Result<(), E>
    {
        self.try_for_some_item(|i| f(Self::i2q(i)))
    }

    /// Call f for all quads from this source.
    ///
    /// Return an error if either the source or `f` errs.
    #[inline]
    fn try_for_each_quad<F, E>(&mut self, mut f: F) -> StreamResult<(), Self::Error, E>
    where
        F: FnMut(Self::Quad<'_>) -> Result<(), E>,
        E: Error,
    {
        self.try_for_each_item(|i| f(Self::i2q(i)))
    }

    /// Call f for some quad(s) (possibly zero) from this source, if any.
    ///
    /// Return false if there are no more quads in this source.
    ///
    /// Return an error if either the source errs.
    #[inline]
    fn for_some_quad<F>(&mut self, mut f: F) -> Result<bool, Self::Error>
    where
        F: FnMut(Self::Quad<'_>),
    {
        self.for_some_item(&mut |i| f(Self::i2q(i)))
    }

    /// Call f for all quads from this source.
    ///
    /// Return an error if either the source errs.
    #[inline]
    fn for_each_quad<F>(&mut self, mut f: F) -> Result<(), Self::Error>
    where
        F: FnMut(Self::Quad<'_>),
    {
        self.for_each_item(|i| f(Self::i2q(i)))
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
    fn filter_map_quads<'f, F, T>(self, mut filter_map: F) -> filter_map::FilterMapSource<Self, impl FnMut(Self::Item<'_>) -> Option<T> + 'f>
    where
        Self: Sized,
        F: FnMut(Self::Quad<'_>) -> Option<T> + 'f,
    {
        self.filter_map_items(move |i| filter_map(Self::i2q(i)))
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
    fn map_quads<'f, F, T>(self, mut map: F) -> map::MapSource<Self, impl FnMut(Self::Item<'_>) -> T + 'f>
    where
        Self: Sized,
        F: FnMut(Self::Quad<'_>) -> T + 'f,
    {
        self.map_items(move |i| map(Self::i2q(i)))
    }

    /// Convert of quads in this source to triples (stripping the graph name).
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

/// Ensures that QuadSource acts as an type alias for any Source satisfying the conditions.
impl<T> QuadSource for T where
    T: Source + IsQuadSource,
{}

/// Type alias to denote the type of quads yielded by a [`QuadSource`].
///
/// **Why not using `TS::Item<'a>` instead?**
/// [`QuadSource::Item`] being a generic associated type (GAT),
/// the compiler will not always "know" that `TS::Item<'a>` implements the [`Quad`] trait.
/// This type alias, on the other hand, will always be recognized as a [`Quad`] implementation.
pub type QSQuad<'a, TS> = <TS as IsQuadSource>::Quad<'a>;

mod sealed {
    use super::*;

    pub trait IsQuadSource: Source {
        type Quad<'x>: Quad;
        fn i2q(i: Self::Item<'_>) -> Self::Quad<'_>;
        fn ri2q<'a, 'b>(i: &'a Self::Item<'b>) -> &'a Self::Quad<'b>;
    }

    impl<TS> IsQuadSource for TS
    where
        TS: Source,
        for <'x> TS::Item<'x>: Quad,
    {
        type Quad<'x> = Self::Item<'x>;

        fn i2q(i: Self::Item<'_>) -> Self::Quad<'_> {
            i
        }

        fn ri2q<'a, 'b>(i: &'a Self::Item<'b>) -> &'a Self::Quad<'b> {
            i
        }
    }
}
use sealed::IsQuadSource;


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

    impl<'a> Source for DummyParser<'a> {
        type Item<'x> = Spog<SimpleTerm<'x>>;
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
