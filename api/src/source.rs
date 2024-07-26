//! A [`Source`] yields items, and may also fail in the process.
//! This module also provides two specialized kinds of source:
//! [`TripleSource`] and [`QuadSource`].
//!
//! The [`Source`] trait provides an API similar to (a subset of) the [`Iterator`] API,
//! with methods such as [`for_each_item`] and [`try_for_each_item`].
//! [`TripleSource`] and [`QuadSource`] provides specialized alternative
//! (e.g. [`for_each_triple`] and [`for_each_quad`], respectively).
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
//! In particular, it can be stored in a collection,
//! and still be alive when the `next` method is called again
//! (consider for example the [`Iterator::collect`] method).
//!
//! Because many parsers (as well as other triple/quad sources)
//! will be implemented in a manner similar to that described above,
//! we have to provide a trait with *weaker assumptions*
//! on the lifetime of the yielded triples.
//!
//! The alternative would be to wrap such parsers with a layer that would *copy*
//! the data from internal buffers to fresh buffers for each triples,
//! but we do not want to impose that cost on all implementations
//! — especially when many consumers will be happy with short-lived references.
//!
//! # About the design of [`TripleSource`] and [`QuadSource`]
//!
//! The design of [`TripleSource`] (resp. [`QuadSource`]) may seem overcomplicated,
//! but it aims to have the following properties.
//!
//! * When a concrete type implements [`Source`] and its items implement
//!   [`Triple`](crate::triple::Triple), then it is automatically recognized as a
//!   [`TripleSource`].
//! * When a [`TripleSource`] is required, it can be used as a simple trait bound,
//!   without requiring the user to add a higher ranked trait bound (HRTB) like
//!   `for <'x> S:Item<'x>: Triple`.
//!
//! [`for_each_item`]: Source::for_each_item
//! [`try_for_each_item`]: Source::try_for_each_item
//! [`for_each_triple`]: TripleSource::for_each_triple
//! [`for_each_quad`]: QuadSource::for_each_quad
//! [higher-rank trait bounds]: https://doc.rust-lang.org/nomicon/hrtb.html



pub mod convert;
pub mod filter;
pub mod filter_map;
pub mod map;

mod _quad;
pub use _quad::*;
mod _stream_error;
pub use _stream_error::*;
mod _triple;
pub use _triple::*;

use std::convert::Infallible;

/// A source produces [items](Source::Item), and may also fail in the process.
///
/// See [module documentation](super) for the rationale of his trait.
///
/// # Common implementors
///
/// Any iterator yielding [results](std::result::Result)
/// implements the [`Source`] trait.
///
/// Any iterator can also be converted to an [`Infallible`] [`Source`]
/// thanks to the [`IntoSource`] extension trait.
pub trait Source {
    /// The type of items this source yields.
    type Item<'x>;
    /// The type of errors produced by this source.
    type Error: Error + 'static;

    /// Call f for some item(s) (possibly zero) from this source, if any.
    ///
    /// Return `Ok(false)` if there are no more items in this source.
    ///
    /// Return an error if either the source or `f` errs.
    fn try_for_some_item<E, F>(&mut self, f: F) -> StreamResult<bool, Self::Error, E>
    where
        E: Error,
        F: FnMut(Self::Item<'_>) -> Result<(), E>;

    /// Call f for all items from this source.
    ///
    /// Return an error if either the source or `f` errs.
    #[inline]
    fn try_for_each_item<F, E>(&mut self, mut f: F) -> StreamResult<(), Self::Error, E>
    where
        F: FnMut(Self::Item<'_>) -> Result<(), E>,
        E: Error,
    {
        while self.try_for_some_item(&mut f)? {}
        Ok(())
    }

    /// Call f for some item(s) (possibly zero) from this source, if any.
    ///
    /// Return false if there are no more items in this source.
    ///
    /// Return an error if either the source errs.
    #[inline]
    fn for_some_item<F>(&mut self, mut f: F) -> Result<bool, Self::Error>
    where
        F: FnMut(Self::Item<'_>),
    {
        self.try_for_some_item(|t| -> Result<(), Self::Error> {
            f(t);
            Ok(())
        })
        .map_err(StreamError::inner_into)
    }

    /// Call f for all items from this source.
    ///
    /// Return an error if either the source errs.
    #[inline]
    fn for_each_item<F>(&mut self, f: F) -> Result<(), Self::Error>
    where
        F: FnMut(Self::Item<'_>),
    {
        let mut f = f;
        while self.for_some_item(&mut f)? {}
        Ok(())
    }

    /// Returns a source which uses `predicate` to determine if an item should be yielded.
    #[inline]
    fn filter_items<F>(self, predicate: F) -> filter::FilterSource<Self, F>
    where
        Self: Sized,
        F: FnMut(&Self::Item<'_>) -> bool,
    {
        filter::FilterSource {
            source: self,
            predicate,
        }
    }

    /// Returns a source that both filters and maps.
    ///
    /// See also [`TripleSource::filter_triples`] and [`TripleSource::map_triples`].
    #[inline]
    fn filter_map_items<F, T>(self, filter_map: F) -> filter_map::FilterMapSource<Self, F>
    where
        Self: Sized,
        F: FnMut(Self::Item<'_>) -> Option<T>,
    {
        filter_map::FilterMapSource {
            source: self,
            filter_map,
        }
    }

    /// Returns a source which yield the result of `map` for each Item.
    ///
    /// NB: due to [some limitations in GATs (Generic) Associated Types](https://blog.rust-lang.org/2022/10/28/gats-stabilization.html),
    /// the `map` function is currently restricted in what it can return.
    /// In particular, passing functions as trivial as `|t| t`
    /// currently does not compile on all implementations of [`Source`].
    ///
    /// As a rule of thumb,
    /// whenever `map` returns something satisfying the `'static` lifetime,
    /// things should work as expected.
    #[inline]
    fn map_items<F, T>(self, map: F) -> map::MapSource<Self, F>
    where
        Self: Sized,
        F: FnMut(Self::Item<'_>) -> T,
    {
        map::MapSource { source: self, map }
    }

    /// Returns the bounds on the remaining length of the source.
    ///
    /// This method has the same contract as [`Iterator::size_hint`].
    fn size_hint_items(&self) -> (usize, Option<usize>) {
        (0, None)
    }
}

impl<'a, I, T, E> Source for I
where
    I: Iterator<Item = Result<T, E>> + 'a,
    E: Error + 'static,
{
    type Item<'x> = T;
    type Error = E;

    fn try_for_some_item<E2, F>(&mut self, mut f: F) -> StreamResult<bool, Self::Error, E2>
    where
        E2: Error,
        F: FnMut(Self::Item<'_>) -> Result<(), E2>,
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

    fn size_hint_items(&self) -> (usize, Option<usize>) {
        self.size_hint()
    }
}

/// An extension trait for iterators,
/// converting them to an [`Infallible`] [`Source`].
pub trait IntoSource: Iterator + Sized {
    /// Convert this iterator into an [`Infallible`] [`Source`].
    #[allow(clippy::type_complexity)]
    fn into_source(
        self,
    ) -> std::iter::Map<
        Self,
        fn(<Self as Iterator>::Item) -> Result<<Self as Iterator>::Item, Infallible>,
    > {
        self.map(Ok::<_, Infallible>)
    }
}

impl<I> IntoSource for I where I: Iterator {}
