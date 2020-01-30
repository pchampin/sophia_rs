//! `TripleSource` and `TripleSink`,
//! are pervasive traits for streaming triples from one object to another.
//!
//! See [`TripleSource`]'s and [`TripleSink`]'s documentation for more detail.
//!
//! # Rationale (or Why not simply use `Iterator`?)
//!
//! [`TripleSource`]s are conceptually very similar to iterators,
//! so why introduce a new trait?
//! The answer is that Rust iterators are limited,
//! when it comes to yielding *references*,
//! and [`TripleSource`] is designed to overcome this limitation.
//!
//! More precisely, when `Iterator::Item` is a reference type
//! (or contains some reference, such as `[&RcTerm;3]` for example),
//! its lifetime must be known in advance,
//! and will typically be the lifetime of the iterator itself
//! (what we shall call *long-lived* references).
//! But triple sources (parsers in particular)
//! may need to yield *short-lived* references,
//! *i.e.* references that will be valid during the time need to process them,
//! but may be outlived by the triple source itself.
//!
//! [`TripleSource`]: trait.TripleSource.html
//! [`TripleSink`]: trait.TripleSink.html
//! [`Triple`]: ../trait.Triple.html
//! [`Iterator`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html

mod _error;
pub use self::_error::*;

use std::convert::Infallible;
use std::error::Error;
use std::iter::Map;

use crate::graph::*;
use crate::triple::streaming_mode::*;
use crate::triple::*;

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
    type Error: 'static + Error;

    /// Determine the type of [`Triple`](../triple/trait.Triple.html)s
    /// that this triple source yields.
    /// (see [`streaming_mode`](../triple/streaming_mode/index.html)
    type Triple: TripleStreamingMode;

    /// Call f for at least one triple from this triple source, if any.
    ///
    /// Return false if there are no more triples in this source.
    fn try_for_some_triple<F, E>(&mut self, f: &mut F) -> StreamResult<bool, Self::Error, E>
    where
        F: FnMut(StreamedTriple<Self::Triple>) -> Result<(), E>,
        E: Error;

    /// Call f for all triples from this triple source.
    #[inline]
    fn try_for_each_triple<F, E>(&mut self, f: F) -> StreamResult<(), Self::Error, E>
    where
        F: FnMut(StreamedTriple<Self::Triple>) -> Result<(), E>,
        E: Error,
    {
        let mut f = f;
        while self.try_for_some_triple(&mut f)? {}
        Ok(())
    }
    /// Call f for at least one triple from this triple source.
    ///
    /// Return false if there are no more triples in this source.
    #[inline]
    fn for_some_triple<F>(&mut self, f: &mut F) -> Result<bool, Self::Error>
    where
        F: FnMut(StreamedTriple<Self::Triple>) -> (),
    {
        self.try_for_some_triple(&mut |t| -> Result<(), Self::Error> {
            f(t);
            Ok(())
        })
        .map_err(StreamError::inner_into)
    }
    /// Call f for all triples from this triple source.
    #[inline]
    fn for_each_triple<F>(&mut self, f: &mut F) -> Result<(), Self::Error>
    where
        F: FnMut(StreamedTriple<Self::Triple>) -> (),
    {
        let mut f = f;
        while self.for_some_triple(&mut f)? {}
        Ok(())
    }
    /// Feed all triples from this source into the given [sink](trait.TripleSink.html).
    ///
    /// Stop on the first error (in the source or the sink).
    fn in_sink<TS: TripleSink>(
        &mut self,
        sink: &mut TS,
    ) -> StreamResult<TS::Outcome, Self::Error, TS::Error> {
        self.try_for_each_triple(|t| sink.feed(&t))
            .and(sink.finish().map_err(SinkError))
    }
    /// Insert all triples from this source into the given [graph](../../graph/trait.MutableGraph.html).
    ///
    /// Stop on the first error (in the source or in the graph).
    fn in_graph<G: MutableGraph>(
        &mut self,
        graph: &mut G,
    ) -> StreamResult<usize, Self::Error, <G as MutableGraph>::MutationError>
    where
        Self: Sized,
    {
        graph.insert_all(self)
    }
}

impl<I, T, E> TripleSource for I
where
    I: Iterator<Item = Result<T, E>>,
    T: Triple,
    E: 'static + Error,
{
    type Error = E;
    type Triple = ByValue<T>;

    fn try_for_some_triple<F, EF>(&mut self, f: &mut F) -> StreamResult<bool, E, EF>
    where
        F: FnMut(StreamedTriple<Self::Triple>) -> Result<(), EF>,
        EF: Error,
    {
        match self.next() {
            Some(Ok(triple)) => f(StreamedTriple::by_value(triple))
                .map_err(SinkError)
                .and(Ok(true)),
            Some(Err(err)) => Err(SourceError(err)),
            None => Ok(false),
        }
    }
}

pub type AsInfallibleSource<I, T> = Map<I, fn(T) -> Result<T, Infallible>>;

/// A utility extension trait for converting any iterator of [`Triple`]s
/// into [`TripleSource`], by wrapping its items in `Ok` results.
///
/// [`TripleSource`]: trait.TripleSource.html
/// [`Triple`]: ../trait.Triple.html
pub trait AsTripleSource<T>: Sized {
    /// Map all items of this iterator into an Ok result.
    fn as_triple_source(self) -> AsInfallibleSource<Self, T>;
}

impl<T, I> AsTripleSource<T> for I
where
    I: Iterator<Item = T> + Sized,
    T: Triple,
{
    fn as_triple_source(self) -> AsInfallibleSource<Self, T> {
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
    type Error: 'static + Error;

    /// Feed one triple in this sink.
    fn feed<T: Triple>(&mut self, t: &T) -> Result<(), Self::Error>;

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
    type Error = Infallible;

    fn feed<T: Triple>(&mut self, _: &T) -> Result<(), Self::Error> {
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
