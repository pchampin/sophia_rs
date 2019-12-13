//! Serializers for standard RDF syntaxes,
//! and tools for building new serializers.
//!
//! # Uniform interface
//!
//! Each serializer module defines a `Config` type, that
//! - implements [`Default`],
//! - has a `writer` method taking any `io::Write`
//!   and returning a [`TripleWriter`] or a [`QuadWriter`],
//! - has a `stringifier` method returning a [`StringSerializer`] or a [`QuadStringifier`].
//!
//! Each serializer module also provides two functions `writer` and a `stringifier`,
//! calling the corresponding methods from the default `Config`.
//!
//! [`TripleWriter`] and [`TripleStringifier`] are specializations of [`TripleSink`].
//! [`QuadWriter`] and [`QuadStringifier`] are specializations of [`QuadSink`].
//!
//! [`Default`]: https://doc.rust-lang.org/std/default/trait.Default.html
//! [`TripleWriter`]: trait.TripleWriter.html
//! [`TripleStringifier`]: trait.TripleStringifier.html
//! [`QuadWriter`]: trait.QuadWriter.html
//! [`QuadStringifier`]: trait.QuadStringifier.html
//! [`TripleSink`]: ../triple/stream/trait.TripleSink.html
//! [`QuadSink`]: ../quad/stream/trait.QuadSink.html

use std::io;
use crate::dataset::*;
use crate::graph::*;
use crate::quad::{stream::*, *};
use crate::triple::{stream::*, *};
use anyhow;

#[macro_use]
pub mod common;
pub mod nq;
pub mod nt;

mod _error;
pub use self::_error::*;

/// An extension of the `TripleSink` trait,
/// dedicated to serialization to IO streams.
///
/// See also [`TripleSink`].
///
/// [`TripleSink`]: ../triple/stream/trait.TripleSink.html
///
pub trait TripleWriter<W: io::Write>: TripleSink<Outcome = ()> + Sized {
    type Config;

    fn new(write: W, config: Self::Config) -> Self;

    /// Serialize the triples from the given source.
    fn write<'a, TS, T>(&mut self, source: TS) -> Result<(), anyhow::Error>
    where
        TS: TripleSource<'a>,
    {
        self.feed_all_and_finish(source)
    }

    /// Serialize the given graph.
    fn write_graph<'a, G>(&mut self, graph: &'a mut G) -> Result<(), anyhow::Error>
    where
        G: Graph<'a>,
    {
        self.feed_all_and_finish(graph.triples())
    }

    /// Serialize the given triple.
    fn write_triple<'a, T>(&mut self, t: &T) -> Result<(), Self::Error>
    where
        T: Triple<'a>,
    {
        self.feed(t)?;
        Ok(self.finish()?)
    }
}

/// An extension of the `TripleSink` trait,
/// dedicated to serialization to strings,
/// with methods more explicitly named.
///
/// See also [`TripleSink`].
///
/// [`TripleSink`]: ../triple/stream/trait.TripleSink.html
///
pub trait TripleStringifier: TripleSink<Outcome = String> + Sized {
    type Config;

    fn new(config: Self::Config) -> Self;

    /// Stringify the triples from the given source.
    fn stringify<'a, TS, T>(
        &mut self,
        source: TS,
    ) -> Result<String, anyhow::Error>
    where
        TS: TripleSource<'a>,
    {
        self.feed_all_and_finish(source)
    }

    /// Stringify the given graph.
    fn stringify_graph<'a, G>(
        &mut self,
        graph: &'a mut G,
    ) -> Result<String, anyhow::Error>
    where
        G: Graph<'a>,
    {
        self.feed_all_and_finish(graph.triples())
    }

    /// Stringify the given triple.
    fn stringify_triple<'a, T>(&mut self, t: &T) -> Result<String, Self::Error>
    where
        T: Triple<'a>,
    {
        self.feed(t)?;
        Ok(self.finish()?)
    }
}

/// An extension of the `QuadSink` trait,
/// dedicated to serialization to IO streams.
///
/// See also [`QuadSink`].
///
/// [`QuadSink`]: ../quad/stream/trait.QuadSink.html
///
pub trait QuadWriter<W: io::Write>: QuadSink<Outcome = ()> + Sized {
    type Config;

    fn new(write: W, config: Self::Config) -> Self;

    /// Serialize the triples from the given source.
    fn write<'a, QS, T>(&mut self, source: QS) -> Result<(), anyhow::Error>
    where
        QS: QuadSource<'a>,
    {
        Ok(self.feed_all_and_finish(source)?)
    }

    /// Serialize the given dataset.
    fn write_dataset<'a, D>(
        &mut self,
        dataset: &'a mut D,
    ) -> Result<(), anyhow::Error>
    where
        D: Dataset<'a>,
    {
        Ok(self.feed_all_and_finish(dataset.quads())?)
    }

    /// Serialize the given triple.
    fn write_quad<'a, Q>(&mut self, q: &Q) -> Result<(), Self::Error>
    where
        Q: Quad<'a>,
    {
        self.feed(q)?;
        self.finish()
    }
}

/// An extension of the `QuadSink` trait,
/// dedicated to serialization to strings,
/// with methods more explicitly named.
///
/// See also [`QuadSink`].
///
/// [`QuadSink`]: ../quad/stream/trait.QuadSink.html
///
pub trait QuadStringifier: QuadSink<Outcome = String> + Sized {
    type Config;

    fn new(config: Self::Config) -> Self;

    /// Stringify the triples from the given source.
    fn stringify<'a, QS, T>(
        &mut self,
        source: QS,
    ) -> Result<String, anyhow::Error>
    where
        QS: QuadSource<'a>,
    {
        Ok(self.feed_all_and_finish(source)?)
    }

    /// Stringify the given dataset.
    fn stringify_dataset<'a, D>(
        &mut self,
        dataset: &'a mut D,
    ) -> Result<String, anyhow::Error>
    where
        D: Dataset<'a>,
    {
        Ok(self.feed_all_and_finish(dataset.quads())?)
    }

    /// Stringify the given triple.
    fn stringify_quad<'a, Q>(&mut self, q: &Q) -> Result<String, Self::Error>
    where
        Q: Quad<'a>,
    {
        self.feed(q)?;
        Ok(self.finish()?)
    }
}

#[cfg(test)]
mod test {
    // Nothing really worth testing here
}
