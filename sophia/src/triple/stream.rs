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
use crate::triple::*;
use anyhow;

/// A triple source produces [triples], and may also fail in the process.
///
/// A triple source is castable, with the `as_iter` method,
/// to an iterator yielding [triples] wrapped in [results],
/// and any such iterator implements the `TripleSource` trait.
/// It also has additional methods dedicated to interacting with [`TripleSink`]s.
///
/// [triples]: ../trait.Triple.html
/// [results]: ../../error/type.Result.html
/// [`TripleSink`]: trait.TripleSink.html
///
pub trait TripleSource<'a> {
    /// The type of triples produced by this source.
    type Triple: Triple<'a>;

    /// The type of errors produced by this source.
    type Error: SafeError;

    /// The type of iterator this triple source casts to.
    type Iter: Iterator<Item = Result<Self::Triple, Self::Error>>;

    /// Cast to iterator.
    fn as_iter(&mut self) -> &mut Self::Iter;
}

impl<'a, I, T, E> TripleSource<'a> for I
where
    I: Iterator<Item = Result<T, E>> + 'a,
    T: Triple<'a>,
    E: SafeError,
{
    type Triple = T;
    type Error = E;
    type Iter = Self;

    fn as_iter(&mut self) -> &mut Self::Iter {
        self
    }
}

/// A utility extension trait for converting any iterator of [`Triple`]s
/// into [`TripleSource`], by wrapping its items in `Ok` results.
///
/// [`TripleSource`]: trait.TripleSource.html
/// [`Triple`]: ../trait.Triple.html
pub trait AsTripleSource<T>: Sized {
    /// Map all items of this iterator into an Ok result.
    fn as_triple_source(self) -> Map<Self, fn(T) -> Result<T, Infallible>>;
}

impl<'a, T, I> AsTripleSource<T> for I
where
    I: Iterator<Item = T> + 'a + Sized,
    T: Triple<'a>,
{
    fn as_triple_source(self) -> Map<Self, fn(T) -> Result<T, Infallible>> {
       self.map(Ok)
    }
}

/// A triple sink consumes [triples](../trait.Triple.html),
/// produces a result, and may also fail in the process.
///
/// Typical triple sinks are [serializers].
///
/// See also [`TripleSource`].
///
/// [serializers]: ../../serializer/index.html
/// [`TripleSource`]: trait.TripleSource.html
///
pub trait TripleSink {
    /// The type of the result produced by this triple sink.
    ///
    /// See [`finish`](#tymethod.finish).
    type Outcome;

    /// The type of error raised by this triple sink.
    type Error: SafeError;

    /// Feed one triple in this sink.
    fn feed<'a, T: Triple<'a>>(&mut self, t: &T) -> Result<(), Self::Error>;

    fn feed_all<'a, TS>(&mut self, ts: TS) -> Result<(), anyhow::Error> 
    where
        TS: TripleSource<'a>,
    {
        let mut ts = ts;
        for t in ts.as_iter() {
            let t = t?;
            self.feed(&t)?;
        }
        Ok(())
    }

    /// Produce the result once all triples were fed.
    ///
    /// NB: the behaviour of a triple sink after `finish` is called is unspecified by this trait.
    fn finish(&mut self) -> Result<Self::Outcome, Self::Error>;

    fn feed_all_and_finish<'a, TS>(&mut self, ts: TS) -> Result<Self::Outcome, anyhow::Error> 
    where
        TS: TripleSource<'a>,
    {
        self.feed_all(ts)?;
        Ok(self.finish()?)
    }
}

#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially the parser/serializer modules).
}
