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
use crate::error::*;
use anyhow;

use crate::quad::*;

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
    type Error: SafeError;

    /// The type of iterator this quad source casts to.
    type Iter: Iterator<Item = Result<Self::Quad, Self::Error>>;

    /// Cast to iterator.
    fn as_iter(&mut self) -> &mut Self::Iter;
}

impl<'a, I, T, E> QuadSource<'a> for I
where
    I: Iterator<Item = Result<T, E>> + 'a,
    T: Quad<'a>,
    E: SafeError,
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
pub trait AsQuadSource<Q>: Sized {
    /// Map all items of this iterator into an Ok result.
    fn as_quad_source(self) -> Map<Self, fn(Q) -> Result<Q, Infallible>>;
}

impl<'a, Q, I> AsQuadSource<Q> for I
where
    I: Iterator<Item = Q> + 'a + Sized,
    Q: Quad<'a>,
{
    fn as_quad_source(self) -> Map<Self, fn(Q) -> Result<Q, Infallible>> {
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
    type Error: SafeError;

    /// Feed one quad in this sink.
    fn feed<'a, T: Quad<'a>>(&mut self, t: &T) -> Result<(), Self::Error>;

    /// Feeds all quads of the `QuadSource` into the `QuadSink`.
    fn feed_all<'a, QS>(&mut self, qs: QS) -> Result<(), anyhow::Error> 
    where
        QS: QuadSource<'a>,
    {
        let mut qs = qs;
        for t in qs.as_iter() {
            let t = t?;
            self.feed(&t)?;
        }
        Ok(())
    }

    /// Produce the result once all quads were fed.
    ///
    /// NB: the behaviour of a quad sink after `finish` is called is unspecified by this trait.
    fn finish(&mut self) -> Result<Self::Outcome, Self::Error>;

    fn feed_all_and_finish<'a, QS>(&mut self, qs: QS) -> Result<Self::Outcome, anyhow::Error> 
    where
        QS: QuadSource<'a>,
    {
        self.feed_all(qs)?;
        Ok(self.finish()?)
    }
}

/// [`()`](https://doc.rust-lang.org/std/primitive.unit.html) acts as a "black hole",
/// consuming all quads without erring, and producing no result.
///
/// Useful for benchmarking quad sources.
impl QuadSink for () {
    type Outcome = ();
    type Error = Infallible;

    fn feed<'a, T: Quad<'a>>(&mut self, _: &T) -> Result<(), Infallible> {
        Ok(())
    }
    fn finish(&mut self) -> Result<Self::Outcome, Infallible> {
        Ok(())
    }
}

#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially the parser/serializer modules).
}
