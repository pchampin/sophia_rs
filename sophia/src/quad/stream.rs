//! `QuadSource` and `QuadSink`,
//! are pervasive traits for streaming quads from one object to another.
//!
//! See [`QuadSource`]'s and [`QuadSink`]'s documentation for more detail.
//!
//! [`QuadSource`]: trait.QuadSource.html
//! [`QuadSink`]: trait.QuadSink.html
//! [`Quad`]: ../trait.Quad.html
//! [`Iterator`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html

use std::iter::Map;

use crate::dataset::*;
use crate::error::*;
use crate::quad::*;

use std::result::Result; // override ::error::Result

/// A quad source produces [quads], and may also fail in the process.
///
/// A quad source is castable, with the `as_iter` method,
/// to an iterator yielding [quads] wrapped in [results],
/// and any such iterator implements the `QuadSource` trait.
/// It also has additional methods dedicated to interacting with [`QuadSink`]s.
///
/// [quads]: ../trait.Quad.html
/// [results]: ../../error/type.Result.html
/// [`QuadSink`]: trait.QuadSink.html
///
pub trait QuadSource<'a> {
    /// The type of quads produced by this source.
    type Quad: Quad<'a>;

    /// The type of errors produced by this source.
    type Error: CoercibleWith<Error> + CoercibleWith<Never>;

    /// The type of iterator this quad source casts to.
    type Iter: Iterator<Item = Result<Self::Quad, Self::Error>>;

    /// Cast to iterator.
    fn as_iter(&mut self) -> &mut Self::Iter;

    /// Feed all quads from this source into the given [sink](trait.QuadSink.html).
    ///
    /// Stop on the first error (in the source or the sink).
    fn in_sink<TS: QuadSink>(
        &mut self,
        sink: &mut TS,
    ) -> CoercedResult<TS::Outcome, Self::Error, TS::Error>
    where
        Self::Error: CoercibleWith<TS::Error>,
    {
        for tr in self.as_iter() {
            let t = tr?;
            sink.feed(&t)?;
        }
        Ok(sink.finish()?)
    }

    /// Insert all quads from this source into the given [dataset](../../dataset/trait.MutableDataset.html).
    ///
    /// Stop on the first error (in the source or in the dataset).
    fn in_dataset<D: MutableDataset>(
        &mut self,
        dataset: &mut D,
    ) -> CoercedResult<usize, Self::Error, <D as MutableDataset>::MutationError>
    where
        Self::Error: CoercibleWith<<D as MutableDataset>::MutationError>,
    {
        self.in_sink(&mut dataset.inserter())
    }
}

impl<'a, I, T, E> QuadSource<'a> for I
where
    I: Iterator<Item = Result<T, E>> + 'a,
    T: Quad<'a>,
    E: CoercibleWith<Error> + CoercibleWith<Never>,
{
    type Quad = T;
    type Error = E;
    type Iter = Self;

    fn as_iter(&mut self) -> &mut Self::Iter {
        self
    }
}

/// A utility extension trait for converting any iterator of [`Quad`]s
/// into [`QuadSource`], by wrapping its items in `Ok` results.
///
/// [`QuadSource`]: trait.QuadSource.html
/// [`Quad`]: ../trait.Quad.html
pub trait AsQuadSource<T>: Sized {
    /// Map all items of this iterator into an Ok result.
    fn as_quad_source(self) -> Map<Self, fn(T) -> OkResult<T>>;
}

impl<'a, T, I> AsQuadSource<T> for I
where
    I: Iterator<Item = T> + 'a + Sized,
    T: Quad<'a>,
{
    fn as_quad_source(self) -> Map<Self, fn(T) -> OkResult<T>> {
        self.map(Ok)
    }
}

/// A quad sink consumes [quads](../trait.Quad.html),
/// produces a result, and may also fail in the process.
///
/// Typical quad sinks are [serializer]
/// or graphs' [inserters] and [removers].
///
/// See also [`QuadSource`].
///
/// [serializer]: ../../serializer/index.html
/// [inserters]: ../../graph/trait.MutableGraph.html#method.inserter
/// [removers]: ../../graph/trait.MutableGraph.html#method.remover
/// [`QuadSource`]: trait.QuadSource.html
///
pub trait QuadSink {
    /// The type of the result produced by this quad sink.
    ///
    /// See [`finish`](#tymethod.finish).
    type Outcome;

    /// The type of error raised by this quad sink.
    type Error: CoercibleWith<Error> + CoercibleWith<Never>;

    /// Feed one quad in this sink.
    fn feed<'a, T: Quad<'a>>(&mut self, t: &T) -> Result<(), Self::Error>;

    /// Produce the result once all quads were fed.
    ///
    /// NB: the behaviour of a quad sink after `finish` is called is unspecified by this trait.
    fn finish(&mut self) -> Result<Self::Outcome, Self::Error>;
}

/// [`()`](https://doc.rust-lang.org/std/primitive.unit.html) acts as a "black hole",
/// consuming all quads without erring, and producing no result.
///
/// Useful for benchmarking quad sources.
impl QuadSink for () {
    type Outcome = ();
    type Error = Never;

    fn feed<'a, T: Quad<'a>>(&mut self, _: &T) -> OkResult<()> {
        Ok(())
    }
    fn finish(&mut self) -> OkResult<Self::Outcome> {
        Ok(())
    }
}

#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially the parser/serializer modules).
}
