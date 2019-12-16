//! `QuadSource` and `QuadSink`,
//! are pervasive traits for streaming quads from one object to another.
//!
//! See [`QuadSource`]'s and [`QuadSink`]'s documentation for more detail.
//!
//! # Rationale (or Why not simply use `Iterator`?)
//!
//! See the documentation of module [`triple::stream`].
//!
//! [`QuadSource`]: trait.QuadSource.html
//! [`QuadSink`]: trait.QuadSink.html
//! [`Quad`]: ../trait.Quad.html
//! [`Iterator`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html
//! [`triple::stream`]: ../../triple/stream/index.html

use std::convert::Infallible;
use std::error::Error;
use std::iter::Map;

use crate::dataset::*;
use crate::error::Error as SophiaError;
use crate::quad::*;
use crate::triple::stream::*;

use std::result::Result; // override ::error::Result

/// A quad source produces [quads], and may also fail in the process.
///
/// It provides additional methods dedicated to interacting with [`QuadSink`]s.
/// Any iterator yielding [quads] wrapped in [results]
/// implements the `QuadSource` trait.
///
/// [quads]: ../trait.Quad.html
/// [results]: ../../error/type.Result.html
/// [`QuadSink`]: trait.QuadSink.html
///
pub trait QuadSource {
    /// The type of errors produced by this source.
    type Error: 'static + Error + Into<SophiaError>;

    /// Feed all quads from this source into the given [sink](trait.QuadSink.html).
    ///
    /// Stop on the first error (in the source or the sink).
    fn in_sink<TS: QuadSink>(
        &mut self,
        sink: &mut TS,
    ) -> Result<TS::Outcome, StreamError<Self::Error, TS::Error>>;

    /// Insert all quads from this source into the given [dataset](../../dataset/trait.MutableDataset.html).
    ///
    /// Stop on the first error (in the source or in the dataset).
    fn in_dataset<D: MutableDataset>(
        &mut self,
        dataset: &mut D,
    ) -> Result<usize, StreamError<Self::Error, <D as MutableDataset>::MutationError>> {
        self.in_sink(&mut dataset.inserter())
    }
}

impl<'a, I, T, E> QuadSource for I
where
    I: Iterator<Item = Result<T, E>>,
    T: Quad<'a>,
    E: 'static + Error + Into<SophiaError>,
{
    type Error = E;

    fn in_sink<TS: QuadSink>(
        &mut self,
        sink: &mut TS,
    ) -> Result<TS::Outcome, StreamError<Self::Error, TS::Error>> {
        for tr in self {
            let t = tr.map_source_err()?;
            sink.feed(&t).map_sink_err()?;
        }
        Ok(sink.finish().map_sink_err()?)
    }
}

/// A utility extension trait for converting any iterator of [`Quad`]s
/// into [`QuadSource`], by wrapping its items in `Ok` results.
///
/// [`QuadSource`]: trait.QuadSource.html
/// [`Quad`]: ../trait.Quad.html
pub trait AsQuadSource<T>: Sized {
    /// Map all items of this iterator into an Ok result.
    fn as_quad_source(self) -> Map<Self, fn(T) -> Result<T, Infallible>>;
}

impl<'a, T, I> AsQuadSource<T> for I
where
    I: Iterator<Item = T> + 'a + Sized,
    T: Quad<'a>,
{
    fn as_quad_source(self) -> Map<Self, fn(T) -> Result<T, Infallible>> {
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
    type Error: 'static + Error + Into<SophiaError>;

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
    type Error = Infallible;

    fn feed<'a, T: Quad<'a>>(&mut self, _: &T) -> Result<(), Self::Error> {
        Ok(())
    }
    fn finish(&mut self) -> Result<Self::Outcome, Self::Error> {
        Ok(())
    }
}

#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially the parser/serializer modules).
}
