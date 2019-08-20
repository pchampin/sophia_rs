//! `TripleSource` and `TripleSink`,
//! are pervasive traits for streaming triples from one object to another.
//!
//! See [`TripleSource`]'s and [`TripleSink`]'s documentation for more detail.
//!
//! [`TripleSource`]: trait.TripleSource.html
//! [`TripleSink`]: trait.TripleSink.html
//! [`Triple`]: ../trait.Triple.html
//! [`Iterator`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html

use std::iter::Map;

use crate::error::*;
use crate::graph::*;
use crate::triple::*;

use std::result::Result; // override ::error::Result

/// A triple source produces [triples], and may also fail in the process.
///
/// It provides methods dedicated to interacting with [`TripleSink`]s.
/// Any iterator yielding  [triples] wrapped in [results]
/// implements the `TripleSource` trait.
///
/// [triples]: ../trait.Triple.html
/// [results]: ../../error/type.Result.html
/// [`TripleSink`]: trait.TripleSink.html
///
pub trait TripleSource {
    /// The type of errors produced by this source.
    type Error: CoercibleWith<Error> + CoercibleWith<Never>;

    /// Feed all triples from this source into the given [sink](trait.TripleSink.html).
    ///
    /// Stop on the first error (in the source or the sink).
    fn in_sink<TS: TripleSink>(
        &mut self,
        sink: &mut TS,
    ) -> CoercedResult<TS::Outcome, Self::Error, TS::Error>
    where
        Self::Error: CoercibleWith<TS::Error>;

    /// Insert all triples from this source into the given [graph](../../graph/trait.MutableGraph.html).
    ///
    /// Stop on the first error (in the source or in the graph).
    fn in_graph<G: MutableGraph>(
        &mut self,
        graph: &mut G,
    ) -> CoercedResult<usize, Self::Error, <G as MutableGraph>::MutationError>
    where
        Self::Error: CoercibleWith<<G as MutableGraph>::MutationError>,
    {
        self.in_sink(&mut graph.inserter())
    }
}

impl<'a, I, T, E> TripleSource for I
where
    I: Iterator<Item = Result<T, E>>,
    T: Triple<'a>,
    E: CoercibleWith<Error> + CoercibleWith<Never>,
{
    type Error = E;

    fn in_sink<TS: TripleSink>(
        &mut self,
        sink: &mut TS,
    ) -> CoercedResult<TS::Outcome, Self::Error, TS::Error>
    where
        Self::Error: CoercibleWith<TS::Error>,
    {
        for tr in self {
            let t = tr?;
            sink.feed(&t)?;
        }
        Ok(sink.finish()?)
    }
}

/// A utility extension trait for converting any iterator of [`Triple`]s
/// into [`TripleSource`], by wrapping its items in `Ok` results.
///
/// [`TripleSource`]: trait.TripleSource.html
/// [`Triple`]: ../trait.Triple.html
pub trait AsTripleSource<T>: Sized {
    /// Map all items of this iterator into an Ok result.
    fn as_triple_source(self) -> Map<Self, fn(T) -> OkResult<T>>;
}

impl<'a, T, I> AsTripleSource<T> for I
where
    I: Iterator<Item = T> + 'a + Sized,
    T: Triple<'a>,
{
    fn as_triple_source(self) -> Map<Self, fn(T) -> OkResult<T>> {
        self.map(Ok)
    }
}

/// A triple sink consumes [triples](../trait.Triple.html),
/// produces a result, and may also fail in the process.
///
/// Typical triple sinks are [serializer]
/// or graphs' [inserters] and [removers].
///
/// See also [`TripleSource`].
///
/// [serializer]: ../../serializer/index.html
/// [inserters]: ../../graph/trait.MutableGraph.html#method.inserter
/// [removers]: ../../graph/trait.MutableGraph.html#method.remover
/// [`TripleSource`]: trait.TripleSource.html
///
pub trait TripleSink {
    /// The type of the result produced by this triple sink.
    ///
    /// See [`finish`](#tymethod.finish).
    type Outcome;

    /// The type of error raised by this triple sink.
    type Error: CoercibleWith<Error> + CoercibleWith<Never>;

    /// Feed one triple in this sink.
    fn feed<'a, T: Triple<'a>>(&mut self, t: &T) -> Result<(), Self::Error>;

    /// Produce the result once all triples were fed.
    ///
    /// NB: the behaviour of a triple sink after `finish` is called is unspecified by this trait.
    fn finish(&mut self) -> Result<Self::Outcome, Self::Error>;
}

/// [`()`](https://doc.rust-lang.org/std/primitive.unit.html) acts as a "black hole",
/// consuming all triples without erring, and producing no result.
///
/// Useful for benchmarking triple sources.
impl TripleSink for () {
    type Outcome = ();
    type Error = Never;

    fn feed<'a, T: Triple<'a>>(&mut self, _: &T) -> OkResult<()> {
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
