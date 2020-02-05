//! A `QuadSource` produces quads, and may also fail in the process.
//!
//! If provides an API similar to (a subset of) the [`Iterator`] API,
//! with methods such as [`for_each_quad`] and [`try_for_each_quad`].
//!
//! # Rationale (or Why not simply use `Iterator`?)
//!
//! See the documentation of module [`triple::stream`].
//!
//! [`QuadSource`]: trait.QuadSource.html
//! [`Iterator`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html
//! [`for_each_quad`]: ./trait.TripleSource.html#method.for_each_quad
//! [`try_for_each_quad`]: ./trait.TripleSource.html#method.try_for_each_quad

use std::convert::Infallible;
use std::error::Error;
use std::iter::Map;

use crate::dataset::*;
use crate::quad::streaming_mode::*;
use crate::quad::*;
use crate::triple::stream::{
    FilterMapSource, FilterSource, MapSource, SinkError, SourceError, StreamError, StreamResult,
};

/// A quad source produces [quads], and may also fail in the process.
///
/// Any iterator yielding [quads] wrapped in `Result`
/// implements the `QuadSource` trait.
///
/// [quads]: ../trait.Quad.html
pub trait QuadSource {
    /// The type of errors produced by this source.
    type Error: 'static + Error;

    /// Determine the type of [`Quad`](../quad/trait.Quad.html)s
    /// that this quad source yields.
    /// (see [`streaming_mode`](../quad/streaming_mode/index.html)
    type Quad: QuadStreamingMode;

    /// Call f for at least one quad from this quad source, if any.
    ///
    /// Return false if there are no more quads in this source.
    fn try_for_some_quad<F, E>(&mut self, f: &mut F) -> StreamResult<bool, Self::Error, E>
    where
        F: FnMut(StreamedQuad<Self::Quad>) -> Result<(), E>,
        E: Error;

    /// Call f for all quads from this quad source.
    #[inline]
    fn try_for_each_quad<F, E>(&mut self, f: F) -> StreamResult<(), Self::Error, E>
    where
        F: FnMut(StreamedQuad<Self::Quad>) -> Result<(), E>,
        E: Error,
    {
        let mut f = f;
        while self.try_for_some_quad(&mut f)? {}
        Ok(())
    }
    /// Call f for at least one quad from this quad source, if any.
    ///
    /// Return false if there are no more quads in this source.
    #[inline]
    fn for_some_quad<F>(&mut self, f: &mut F) -> Result<bool, Self::Error>
    where
        F: FnMut(StreamedQuad<Self::Quad>) -> (),
    {
        self.try_for_some_quad(&mut |t| -> Result<(), Self::Error> {
            f(t);
            Ok(())
        })
        .map_err(StreamError::inner_into)
    }
    /// Call f for all quads from this quad source.
    #[inline]
    fn for_each_quad<F>(&mut self, f: &mut F) -> Result<(), Self::Error>
    where
        F: FnMut(StreamedQuad<Self::Quad>) -> (),
    {
        let mut f = f;
        while self.for_some_quad(&mut f)? {}
        Ok(())
    }
    /// Insert all quads from this source into the given [dataset](../../dataset/trait.MutableDataset.html).
    ///
    /// Stop on the first error (in the source or in the dataset).
    #[inline]
    fn in_dataset<D: MutableDataset>(
        &mut self,
        dataset: &mut D,
    ) -> StreamResult<usize, Self::Error, <D as MutableDataset>::MutationError>
    where
        Self: Sized,
    {
        dataset.insert_all(self)
    }
    /// Creates a quad source which uses a closure to determine if a quad should be yielded.
    #[inline]
    fn filter_quads<F>(self, filter: F) -> FilterSource<Self, F>
    where
        Self: Sized,
        F: FnMut(&StreamedQuad<Self::Quad>) -> bool,
    {
        FilterSource {
            source: self,
            filter,
        }
    }
    /// Creates a quad source that both filters and maps.
    #[inline]
    fn filter_map_quads<F>(self, filter_map: F) -> FilterMapSource<Self, F>
    where
        Self: Sized,
        F: FnMut(StreamedQuad<Self::Quad>) -> bool,
    {
        FilterMapSource {
            source: self,
            filter_map,
        }
    }
    /// Takes a closure and creates quad source which yield the result of that closure for each quad.
    #[inline]
    fn map_quads<F>(self, map: F) -> MapSource<Self, F>
    where
        Self: Sized,
        F: FnMut(StreamedQuad<Self::Quad>) -> bool,
    {
        MapSource { source: self, map }
    }
}

impl<S, F> QuadSource for FilterSource<S, F>
where
    S: QuadSource,
    F: FnMut(&StreamedQuad<S::Quad>) -> bool,
{
    type Error = S::Error;
    type Quad = S::Quad;
    fn try_for_some_quad<G, E>(&mut self, f: &mut G) -> StreamResult<bool, Self::Error, E>
    where
        G: FnMut(StreamedQuad<Self::Quad>) -> Result<(), E>,
        E: Error,
    {
        let filter = &mut self.filter;
        self.source.try_for_some_quad(&mut |t| {
            if (filter)(&t) {
                f(t)
            } else {
                Ok(())
            }
        })
    }
}

impl<S, F, U> QuadSource for MapSource<S, F>
where
    S: QuadSource,
    F: FnMut(StreamedQuad<S::Quad>) -> U,
    U: Quad,
{
    type Error = S::Error;
    type Quad = ByValue<U>;
    fn try_for_some_quad<G, E>(&mut self, f: &mut G) -> StreamResult<bool, Self::Error, E>
    where
        G: FnMut(StreamedQuad<Self::Quad>) -> Result<(), E>,
        E: Error,
    {
        let map = &mut self.map;
        self.source
            .try_for_some_quad(&mut |t| f(StreamedQuad::by_value((map)(t))))
    }
}

// TODO impl TripleSource for MapSource where U: Triple

impl<S, F, U> QuadSource for FilterMapSource<S, F>
where
    S: QuadSource,
    F: FnMut(StreamedQuad<S::Quad>) -> Option<U>,
    U: Quad,
{
    type Error = S::Error;
    type Quad = ByValue<U>;
    fn try_for_some_quad<G, E>(&mut self, f: &mut G) -> StreamResult<bool, Self::Error, E>
    where
        G: FnMut(StreamedQuad<Self::Quad>) -> Result<(), E>,
        E: Error,
    {
        let filter_map = &mut self.filter_map;
        self.source.try_for_some_quad(&mut |t| {
            if let Some(u) = (filter_map)(t) {
                f(StreamedQuad::by_value(u))
            } else {
                Ok(())
            }
        })
    }
}

// TODO impl TripleSource for FilterMapSource where U: Triple

impl<I, T, E> QuadSource for I
where
    I: Iterator<Item = Result<T, E>>,
    T: Quad,
    E: 'static + Error,
{
    type Error = E;
    type Quad = ByValue<T>;

    fn try_for_some_quad<F, EF>(&mut self, f: &mut F) -> StreamResult<bool, E, EF>
    where
        F: FnMut(StreamedQuad<Self::Quad>) -> Result<(), EF>,
        EF: Error,
    {
        match self.next() {
            Some(Ok(quad)) => f(StreamedQuad::by_value(quad))
                .map_err(SinkError)
                .and(Ok(true)),
            Some(Err(err)) => Err(SourceError(err)),
            None => Ok(false),
        }
    }
}

pub type AsInfallibleSource<I, T> = Map<I, fn(T) -> Result<T, Infallible>>;

/// A utility extension trait for converting any iterator of [`Quad`]s
/// into [`QuadSource`], by wrapping its items in `Ok` results.
///
/// [`QuadSource`]: trait.QuadSource.html
/// [`Quad`]: ../trait.Quad.html
pub trait AsQuadSource<T>: Sized {
    /// Map all items of this iterator into an Ok result.
    fn as_quad_source(self) -> AsInfallibleSource<Self, T>;
}

impl<T, I> AsQuadSource<T> for I
where
    I: Iterator<Item = T> + Sized,
    T: Quad,
{
    fn as_quad_source(self) -> AsInfallibleSource<Self, T> {
        self.map(Ok)
    }
}

/// Soon to be deprecated.
pub trait QuadSink {
    /// The type of the result produced by this quad sink.
    ///
    /// See [`finish`](#tymethod.finish).
    type Outcome;

    /// The type of error raised by this quad sink.
    type Error: 'static + Error;

    /// Feed one quad in this sink.
    fn feed<T: Quad>(&mut self, t: &T) -> Result<(), Self::Error>;

    /// Produce the result once all quads were fed.
    ///
    /// NB: the behaviour of a quad sink after `finish` is called is unspecified by this trait.
    fn finish(&mut self) -> Result<Self::Outcome, Self::Error>;
}

#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially the parser/serializer modules).
}
