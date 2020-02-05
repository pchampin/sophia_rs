//! A `TripleSource` produces triples, and may also fail in the process.
//!
//! If provies an API similar to (a subset of) the `Iterator` API,
//! with methods such as [`for_each_triple`] and [`try_for_each_triple`].
//!
//! # Rationale (or Why not simply use `Iterator`?)
//!
//! The [`Iterator`] trait is designed in such a way that items must live at least as long as the iterator itself.
//! This assumption may be too strong in some situations.
//!
//! For example,
//! consider a parser using 3 buffers to store the subject, predicate,
//! and object of the triples it parses.
//! Each time it extracts a triple from the data,
//! it yields it (as 3 references to its internal buffers)
//! to the closure passed to `for_each_triple`.
//! Then, it **reuses** the buffers to store the data for the next triple,
//! and yields the new triple, as 3 references to the *same* buffers.
//!
//! Such a parser can not implement [`Iterator`],
//! because, once yielded by the iterator's `next` method,
//! an item is free to live during further iterations.
//! In particular, it can be stored in a vector,
//! and still be alive when the `next` method is called again
//! (consider for example the [`Iterator::collect`] method).
//!
//! Because many parsers (as well as other triple sources)
//! will be implemented in a manner similar to that described above,
//! we have to provide a trait with *weaker assumptions*
//! on the lifetime of the yielded triples.
//!
//! The alternative would be to wrap such parsers with a layer that would *copy*
//! the data from internal buffers to fresh buffers for each triples,
//! but we do not want to impose that cost on all implementations
//! — especially when many consumers will be happy with short-lived references.
//!
//! [`TripleSource`]: trait.TripleSource.html
//! [`for_each_triple`]: ./trait.TripleSource.html#method.for_each_triple
//! [`try_for_each_triple`]: ./trait.TripleSource.html#method.try_for_each_triple
//! [`Iterator`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html
//! [`Iterator::collect`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.collect

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
/// Any iterator yielding [triples] wrapped in `Result`
/// implements the `TripleSource` trait.
///
/// [triples]: ../trait.Triple.html
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
    /// Call f for at least one triple from this triple source, if any.
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
    /// Insert all triples from this source into the given [graph](../../graph/trait.MutableGraph.html).
    ///
    /// Stop on the first error (in the source or in the graph).
    #[inline]
    fn in_graph<G: MutableGraph>(
        &mut self,
        graph: &mut G,
    ) -> StreamResult<usize, Self::Error, <G as MutableGraph>::MutationError>
    where
        Self: Sized,
    {
        graph.insert_all(self)
    }
    /// Creates a triple source which uses a closure to determine if a triple should be yielded.
    #[inline]
    fn filter_triples<F>(self, filter: F) -> FilterSource<Self, F>
    where
        Self: Sized,
        F: FnMut(&StreamedTriple<Self::Triple>) -> bool,
    {
        FilterSource {
            source: self,
            filter,
        }
    }
    /// Creates a triple source that both filters and maps.
    #[inline]
    fn filter_map_triples<F>(self, filter_map: F) -> FilterMapSource<Self, F>
    where
        Self: Sized,
        F: FnMut(StreamedTriple<Self::Triple>) -> bool,
    {
        FilterMapSource {
            source: self,
            filter_map,
        }
    }
    /// Takes a closure and creates triple source which yield the result of that closure for each triple.
    #[inline]
    fn map_triples<F>(self, map: F) -> MapSource<Self, F>
    where
        Self: Sized,
        F: FnMut(StreamedTriple<Self::Triple>) -> bool,
    {
        MapSource { source: self, map }
    }
}

pub struct FilterSource<S, F> {
    source: S,
    filter: F,
}

impl<S, F> TripleSource for FilterSource<S, F>
where
    S: TripleSource,
    F: FnMut(&StreamedTriple<S::Triple>) -> bool,
{
    type Error = S::Error;
    type Triple = S::Triple;
    fn try_for_some_triple<G, E>(&mut self, f: &mut G) -> StreamResult<bool, Self::Error, E>
    where
        G: FnMut(StreamedTriple<Self::Triple>) -> Result<(), E>,
        E: Error,
    {
        let filter = &mut self.filter;
        self.source.try_for_some_triple(&mut |t| {
            if (filter)(&t) {
                f(t)
            } else {
                Ok(())
            }
        })
    }
}

pub struct MapSource<S, F> {
    source: S,
    map: F,
}

impl<S, F, U> TripleSource for MapSource<S, F>
where
    S: TripleSource,
    F: FnMut(StreamedTriple<S::Triple>) -> U,
    U: Triple,
{
    type Error = S::Error;
    type Triple = ByValue<U>;
    fn try_for_some_triple<G, E>(&mut self, f: &mut G) -> StreamResult<bool, Self::Error, E>
    where
        G: FnMut(StreamedTriple<Self::Triple>) -> Result<(), E>,
        E: Error,
    {
        let map = &mut self.map;
        self.source
            .try_for_some_triple(&mut |t| f(StreamedTriple::by_value((map)(t))))
    }
}

// TODO impl QuadSource for MapSource where U: Quad

pub struct FilterMapSource<S, F> {
    source: S,
    filter_map: F,
}

impl<S, F, U> TripleSource for FilterMapSource<S, F>
where
    S: TripleSource,
    F: FnMut(StreamedTriple<S::Triple>) -> Option<U>,
    U: Triple,
{
    type Error = S::Error;
    type Triple = ByValue<U>;
    fn try_for_some_triple<G, E>(&mut self, f: &mut G) -> StreamResult<bool, Self::Error, E>
    where
        G: FnMut(StreamedTriple<Self::Triple>) -> Result<(), E>,
        E: Error,
    {
        let filter_map = &mut self.filter_map;
        self.source.try_for_some_triple(&mut |t| {
            if let Some(u) = (filter_map)(t) {
                f(StreamedTriple::by_value(u))
            } else {
                Ok(())
            }
        })
    }
}

// TODO impl QuadSource for FilterMapSource where U: Quad

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

/// Soon to be deprecated.
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

#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially the parser/serializer modules).
}
