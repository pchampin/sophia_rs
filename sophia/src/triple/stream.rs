//! A triple source produces triples, and may also fail in the process.
//!
//! The trait [`TripleSource`]
//! provides an API similar to (a subset of) the [`Iterator`] API,
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
//! [`Iterator`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html
//! [`for_each_triple`]: ./trait.TripleSource.html#method.for_each_triple
//! [`try_for_each_triple`]: ./trait.TripleSource.html#method.try_for_each_triple
//! [`Iterator::collect`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.collect

use std::error::Error;

use crate::graph::*;
use crate::triple::streaming_mode::*;
use crate::triple::*;

mod _error;
pub use self::_error::*;
mod _filter;
pub use self::_filter::*;
mod _filter_map;
pub use self::_filter_map::*;
mod _iterator;
pub use self::_iterator::*;
mod _map;
pub use self::_map::*;

/// Type alias for referencing the `TermData` used in a `TripleSource`.
pub type TSData<S> =
    <<<S as TripleSource>::Triple as TripleStreamingMode>::UnsafeTriple as UnsafeTriple>::TermData;

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
    fn for_each_triple<F>(&mut self, f: F) -> Result<(), Self::Error>
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
        self,
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
    fn filter_map_triples<F, T>(self, filter_map: F) -> FilterMapSource<Self, F>
    where
        Self: Sized,
        F: FnMut(StreamedTriple<Self::Triple>) -> Option<T>,
    {
        FilterMapSource {
            source: self,
            filter_map,
        }
    }
    /// Takes a closure and creates triple source which yield the result of that closure for each triple.
    #[inline]
    fn map_triples<F, T>(self, map: F) -> MapSource<Self, F>
    where
        Self: Sized,
        F: FnMut(StreamedTriple<Self::Triple>) -> T,
    {
        MapSource { source: self, map }
    }

    /// Returns the bounds on the remaining length of the triple source.
    ///
    /// This method has the same contract as [`Iterator::size_hint`].
    ///
    /// [`Iterator::size_hint`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.size_hint
    fn size_hint_triples(&self) -> (usize, Option<usize>) {
        (0, None)
    }
}

#[cfg(test)]
mod test;
