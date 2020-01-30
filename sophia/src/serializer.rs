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

use std::convert::Infallible;
use std::io;

use crate::dataset::*;
use crate::graph::*;
use crate::quad::{stream::*, *};
use crate::triple::{stream::*, *};

use std::result::Result; // override ::error::Result

#[macro_use]
pub mod common;
pub mod nq;
pub mod nt;

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
    fn write<TS, T>(&mut self, mut source: TS) -> StreamResult<(), TS::Error, Self::Error>
    where
        TS: TripleSource,
    {
        source.in_sink(self)
    }

    /// Serialize the given graph.
    fn write_graph<G>(&mut self, graph: &mut G) -> StreamResult<(), G::Error, Self::Error>
    where
        G: Graph,
    {
        graph.triples().in_sink(self)
    }

    /// Serialize the given triple.
    fn write_triple<T>(&mut self, t: &T) -> StreamResult<(), Infallible, Self::Error>
    where
        T: Triple,
    {
        let mut source = vec![[t.s(), t.p(), t.o()]].into_iter().as_triple_source();
        source.in_sink(self)
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
    fn stringify<TS, T>(&mut self, mut source: TS) -> StreamResult<String, TS::Error, Self::Error>
    where
        TS: TripleSource,
    {
        source.in_sink(self)
    }

    /// Stringify the given graph.
    fn stringify_graph<G>(&mut self, graph: &mut G) -> StreamResult<String, G::Error, Self::Error>
    where
        G: Graph,
    {
        graph.triples().in_sink(self)
    }

    /// Stringify the given triple.
    fn stringify_triple<T>(&mut self, t: &T) -> StreamResult<String, Infallible, Self::Error>
    where
        T: Triple,
    {
        let mut source = vec![[t.s(), t.p(), t.o()]].into_iter().as_triple_source();
        source.in_sink(self)
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
    fn write<QS, T>(&mut self, mut source: QS) -> StreamResult<(), QS::Error, Self::Error>
    where
        QS: QuadSource,
    {
        source.in_sink(self)
    }

    /// Serialize the given dataset.
    fn write_dataset<D>(&mut self, dataset: &mut D) -> StreamResult<(), D::Error, Self::Error>
    where
        D: Dataset,
    {
        dataset.quads().in_sink(self)
    }

    /// Serialize the given triple.
    fn write_quad<Q>(&mut self, q: &Q) -> StreamResult<(), Infallible, Self::Error>
    where
        Q: Quad,
    {
        let mut source = vec![([q.s(), q.p(), q.o()], q.g())]
            .into_iter()
            .as_quad_source();
        source.in_sink(self)
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
    fn stringify<QS, T>(&mut self, mut source: QS) -> StreamResult<String, QS::Error, Self::Error>
    where
        QS: QuadSource,
    {
        source.in_sink(self)
    }

    /// Stringify the given dataset.
    fn stringify_dataset<D>(
        &mut self,
        dataset: &mut D,
    ) -> StreamResult<String, D::Error, Self::Error>
    where
        D: Dataset,
    {
        dataset.quads().in_sink(self)
    }

    /// Stringify the given triple.
    fn stringify_quad<Q>(&mut self, q: &Q) -> StreamResult<String, Infallible, Self::Error>
    where
        Q: Quad,
    {
        let mut source = vec![([q.s(), q.p(), q.o()], q.g())]
            .into_iter()
            .as_quad_source();
        source.in_sink(self)
    }
}

#[cfg(test)]
mod test {
    // Nothing really worth testing here
}
